// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.
//
// Acknowledgement
//
// This implementation is inspired by several key papers:
// [1] Alex Ramirez, Oliverio J. Santana, Josep L. Larriba-Pey, and Mateo Valero. "[Fetching instruction streams.]
// (https://doi.org/10.1109/MICRO.2002.1176264)" 35th Annual IEEE/ACM International Symposium on Microarchitecture
// (MICRO). 2002.
// [2] Yasuo Ishii, Jaekyu Lee, Krishnendra Nathella, and Dam Sunwoo. "[Rebasing instruction prefetching: An industry
// perspective.](https://doi.org/10.1109/LCA.2020.3035068)" IEEE Computer Architecture Letters 19.2: 147-150. 2020.
// [3] Yasuo Ishii, Jaekyu Lee, Krishnendra Nathella, and Dam Sunwoo. "[Re-establishing fetch-directed instruction
// prefetching: An industry perspective.](https://doi.org/10.1109/ISPASS51385.2021.00034)" 2021 IEEE International
// Symposium on Performance Analysis of Systems and Software (ISPASS). 2021.

package xiangshan.frontend
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import ftq.Ftq
import ftq.FtqEntry
import ftq.FtqPtr
import org.chipsalliance.cde.config.Parameters
import utility.ClockGate
import utility.DelayN
import utility.DFTResetSignals
import utility.HasPerfEvents
import utility.HPerfMonitor
import utility.ModuleNode
import utility.PerfEvent
import utility.ResetGen
import utility.ResetGenNode
import utility.XSError
import utility.mbist.MbistInterface
import utility.mbist.MbistPipeline
import utility.sram.SramBroadcastBundle
import utility.sram.SramHelper
import xiangshan.CustomCSRCtrlIO
import xiangshan.DebugOptionsKey
import xiangshan.FrontendToCtrlIO
import xiangshan.HasXSParameter
import xiangshan.L1CacheErrorInfo
import xiangshan.SfenceBundle
import xiangshan.SoftIfetchPrefetchBundle
import xiangshan.TlbCsrBundle
import xiangshan.XSBundle
import xiangshan.backend.fu.NewCSR.PFEvent
import xiangshan.backend.fu.PMP
import xiangshan.backend.fu.PMPChecker
import xiangshan.backend.fu.PMPReqBundle
import xiangshan.cache.mmu.PTWFilter
import xiangshan.cache.mmu.PTWRepeaterNB
import xiangshan.cache.mmu.TLB
import xiangshan.cache.mmu.TlbPtwIO
import xiangshan.cache.mmu.VectorTlbPtwIO
import xiangshan.frontend.bpu.DummyBpu
import xiangshan.frontend.icache.ICache
import xiangshan.frontend.ifu.Ifu
import xiangshan.frontend.instruncache.InstrUncache

class Frontend()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  val inner:       FrontendInlined = LazyModule(new FrontendInlined)
  lazy val module: FrontendImp     = new FrontendImp(this)
}

class FrontendImp(wrapper: Frontend)(implicit p: Parameters) extends LazyModuleImp(wrapper) {
  val io: FrontendInlinedImpIO = IO(wrapper.inner.module.io.cloneType)
  io <> wrapper.inner.module.io

  // FIXME: in new style guide, this should be `perf_io` instead, but now we keep this for compatibility
  val io_perf: Vec[PerfEvent] = IO(wrapper.inner.module.io_perf.cloneType) // scalastyle:ignore
  io_perf <> wrapper.inner.module.io_perf

  if (p(DebugOptionsKey).ResetGen) {
    ResetGen(ResetGenNode(Seq(ModuleNode(wrapper.inner.module))), reset, sim = false, io.dft_reset)
  }
}

class FrontendInlined()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = true

  val instrUncache: InstrUncache = LazyModule(new InstrUncache())
  val icache:       ICache       = LazyModule(new ICache())

  lazy val module: FrontendInlinedImp = new FrontendInlinedImp(this)
}

class FrontendInlinedImpIO(implicit p: Parameters) extends XSBundle {
  // TODO: re-arrange frontend IOs to make them more sensible
  class FrontendPerfInfo(implicit p: Parameters) extends XSBundle {
    val ibufFull: Bool        = Bool()
    val bpuInfo:  BpuPerfInfo = new BpuPerfInfo
  }

  class DebugTopDown(implicit p: Parameters) extends XSBundle {
    val robHeadVaddr: Valid[PrunedAddr] = Valid(PrunedAddr(VAddrBits))
  }

  val hartId: UInt = Input(UInt(hartIdLen.W))
  // FIXME: in new style guide, this should be `resetVector` instead, but now we keep this for compatibility
  val reset_vector: PrunedAddr       = Input(PrunedAddr(PAddrBits)) // scalastyle:ignore
  val fencei:       Bool             = Input(Bool())
  val ptw:          TlbPtwIO         = new TlbPtwIO()
  val backend:      FrontendToCtrlIO = new FrontendToCtrlIO
  val softPrefetch: Vec[Valid[SoftIfetchPrefetchBundle]] =
    Vec(backendParams.LduCnt, Flipped(Valid(new SoftIfetchPrefetchBundle)))
  val sfence:          SfenceBundle            = Input(new SfenceBundle)
  val tlbCsr:          TlbCsrBundle            = Input(new TlbCsrBundle)
  val csrCtrl:         CustomCSRCtrlIO         = Input(new CustomCSRCtrlIO)
  val error:           Valid[L1CacheErrorInfo] = Valid(new L1CacheErrorInfo)
  val frontendInfo:    FrontendPerfInfo        = Output(new FrontendPerfInfo)
  val resetInFrontend: Bool                    = Output(Bool())
  val debugTopDown:    DebugTopDown            = Flipped(new DebugTopDown)

  // dft
  val dft: Option[SramBroadcastBundle] = Option.when(hasDFT)(Input(new SramBroadcastBundle))
  // FIXME: in new style guide, this should be `dftReset` instead, but now we keep this for compatibility
  val dft_reset: Option[DFTResetSignals] = Option.when(hasMbist)(Input(new DFTResetSignals)) // scalastyle:ignore
}

class FrontendInlinedImp(outer: FrontendInlined) extends LazyModuleImp(outer)
    with HasXSParameter
    with HasPerfEvents {

  val io: FrontendInlinedImpIO = IO(new FrontendInlinedImpIO)

  // decoupled-frontend modules
  private val instrUncache = outer.instrUncache.module
  private val icache       = outer.icache.module
  private val bpu          = Module(new DummyBpu)
  private val ifu          = Module(new Ifu)
  private val ibuffer      = Module(new IBuffer)
  private val ftq          = Module(new Ftq)
  ftq.io.reset_vector := io.reset_vector

  private val needFlush            = RegNext(io.backend.toFtq.redirect.valid)
  private val flushControlRedirect = RegNext(io.backend.toFtq.redirect.bits.debugIsCtrl)
  private val flushMemVioRedirect  = RegNext(io.backend.toFtq.redirect.bits.debugIsMemVio)
  private val flushControlBTBMiss  = Wire(Bool())
  private val flushTAGEMiss        = Wire(Bool())
  private val flushSCMiss          = Wire(Bool())
  private val flushITTAGEMiss      = Wire(Bool())
  private val flushRASMiss         = Wire(Bool())

  private val tlbCsr  = DelayN(io.tlbCsr, 2)
  private val csrCtrl = DelayN(io.csrCtrl, 2)
  private val sfence  = DelayN(io.sfence, 2)

  // trigger
  ifu.io.frontendTrigger := csrCtrl.frontend_trigger

  // RVCDecoder fsIsOff
  ifu.io.csrFsIsOff := csrCtrl.fsIsOff

  // bpu ctrl
  bpu.io.ctrl        := csrCtrl.bp_ctrl
  bpu.io.resetVector := io.reset_vector

  // pmp
  def PortNumber: Int = coreParams.icacheParameters.PortNumber
  private val pmp        = Module(new PMP())
  private val pmpChecker = Seq.fill(coreParams.ipmpPortNum)(Module(new PMPChecker(3, sameCycle = true)))
  pmp.io.distribute_csr := csrCtrl.distribute_csr
  private val pmpReqVec = Wire(Vec(coreParams.ipmpPortNum, Valid(new PMPReqBundle())))
  (0 until 2 * PortNumber).foreach(i => pmpReqVec(i) <> icache.io.pmp(i).req)
  pmpReqVec.last <> ifu.io.pmp.req

  for (i <- pmpChecker.indices) {
    if (HasBitmapCheck) {
      pmpChecker(i).io.apply(tlbCsr.mbmc.CMODE.asBool, tlbCsr.priv.imode, pmp.io.pmp, pmp.io.pma, pmpReqVec(i))
    } else {
      pmpChecker(i).io.apply(tlbCsr.priv.imode, pmp.io.pmp, pmp.io.pma, pmpReqVec(i))
    }
  }
  (0 until 2 * PortNumber).foreach(i => icache.io.pmp(i).resp <> pmpChecker(i).io.resp)
  ifu.io.pmp.resp <> pmpChecker.last.io.resp

  private val itlb =
    Module(new TLB(coreParams.itlbPortNum, nRespDups = 1, Seq.fill(PortNumber)(false) ++ Seq(true), itlbParams))
  itlb.io.requestor.take(PortNumber) zip icache.io.itlb foreach { case (a, b) => a <> b }
  itlb.io.requestor.last <> ifu.io.itlb // mmio may need re-tlb, blocked
  itlb.io.hartId := io.hartId
  itlb.io.base_connect(sfence, tlbCsr)
  itlb.io.flushPipe.foreach(_ := icache.io.itlbFlushPipe)
  itlb.io.redirect := DontCare // itlb has flushPipe, don't need redirect signal

  private val itlbPtw = Wire(new VectorTlbPtwIO(coreParams.itlbPortNum))
  itlbPtw.connect(itlb.io.ptw)
  private val itlbRepeater1 = PTWFilter(itlbParams.fenceDelay, itlbPtw, sfence, tlbCsr, l2tlbParams.ifilterSize)
  private val itlbRepeater2 =
    PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, itlbRepeater1.io.ptw, io.ptw, sfence, tlbCsr)

  // ICache-MemBlock
  icache.io.softPrefetchReq <> io.softPrefetch

  // wfi (backend-icache, backend-instrUncache)
  // DelayN for better timing, FIXME: maybe 1 cycle is not enough, to be evaluated
  private val wfiReq = DelayN(io.backend.wfi.wfiReq, 1)
  icache.io.wfi.wfiReq       := wfiReq
  instrUncache.io.wfi.wfiReq := wfiReq
  // return safe only when both icache & instrUncache are safe, also only when has wfiReq (like, safe := wfiReq.fire)
  io.backend.wfi.wfiSafe := DelayN(wfiReq && icache.io.wfi.wfiSafe && instrUncache.io.wfi.wfiSafe, 1)

  // IFU-Ftq
  ifu.io.fromFtq <> ftq.io.toIfu
  ftq.io.toIfu.req.ready := ifu.io.fromFtq.req.ready && icache.io.fromFtq.fetchReq.ready

  ftq.io.fromIfu <> ifu.io.toFtq
  bpu.io.fromFtq <> ftq.io.toBpu
  ftq.io.fromBpu <> bpu.io.toFtq

  // ICache-Ftq
  icache.io.fromFtq <> ftq.io.toICache
  // override fetchReq.ready to sync with Ifu
  ftq.io.toICache.fetchReq.ready := ifu.io.fromFtq.req.ready && icache.io.fromFtq.fetchReq.ready
  icache.io.flush                := DontCare

  // Ifu-ICache
  ifu.io.fromICache <> icache.io.toIfu
  ifu.io.toICache <> icache.io.fromIfu

  // ICache-Backend
  icache.io.csrPfEnable := RegNext(csrCtrl.pf_ctrl.l1I_pf_enable)
  icache.io.fencei      := RegNext(io.fencei)

  // IFU-Ibuffer
  ifu.io.toIBuffer <> ibuffer.io.in

  ftq.io.fromBackend <> io.backend.toFtq
  io.backend.fromFtq := ftq.io.toBackend
  io.backend.fromIfu := ifu.io.toBackend
  io.frontendInfo.bpuInfo <> ftq.io.bpuInfo

  private val checkPcMem = Reg(Vec(FtqSize, new FtqEntry))
  when(ftq.io.toBackend.pc_mem_wen) {
    checkPcMem(ftq.io.toBackend.pc_mem_waddr) := ftq.io.toBackend.pc_mem_wdata
  }

  private val checkTargetPtr = Wire(Vec(DecodeWidth, new FtqPtr))
  private val checkTarget    = Wire(Vec(DecodeWidth, PrunedAddr(VAddrBits)))

  for (i <- 0 until DecodeWidth) {
    checkTargetPtr(i) := ibuffer.io.out(i).bits.ftqPtr
    checkTarget(i) := Mux(
      ftq.io.toBackend.newest_entry_ptr.value === checkTargetPtr(i).value,
      PrunedAddrInit(ftq.io.toBackend.newest_entry_target),
      checkPcMem((checkTargetPtr(i) + 1.U).value).startAddr
    )
  }

  // commented out for this br could be the last instruction in the fetch block
  def checkNotTakenConsecutive(): Unit = {
    val prevNotTakenValid  = RegInit(0.B)
    val prevNotTakenFtqPtr = Reg(new FtqPtr)
    for (i <- 0 until DecodeWidth - 1) {
      // for instr that is not the last, if a not-taken br, the next instr should have the same ftqPtr
      // for instr that is the last, record and check next request
      when(ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr) {
        when(ibuffer.io.out(i + 1).fire) {
          // not last br, check now
        }.otherwise {
          // last br, record its info
          prevNotTakenValid  := true.B
          prevNotTakenFtqPtr := checkTargetPtr(i)
        }
      }
      XSError(
        ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr &&
          ibuffer.io.out(i + 1).fire &&
          checkTargetPtr(i).value =/= checkTargetPtr(i + 1).value,
        "not-taken br should have same ftqPtr\n"
      )
    }
    when(ibuffer.io.out(DecodeWidth - 1).fire && ibuffer.io.out(DecodeWidth - 1).bits.pd.isBr) {
      // last instr is a br, record its info
      prevNotTakenValid  := true.B
      prevNotTakenFtqPtr := checkTargetPtr(DecodeWidth - 1)
    }
    when(prevNotTakenValid && ibuffer.io.out(0).fire) {
      prevNotTakenValid := false.B
    }
    XSError(
      prevNotTakenValid && ibuffer.io.out(0).fire &&
        prevNotTakenFtqPtr.value =/= checkTargetPtr(0).value,
      "not-taken br should have same ftqPtr\n"
    )

    when(needFlush) {
      prevNotTakenValid := false.B
    }
  }

  def checkTakenNotConsecutive(): Unit = {
    val prevTakenValid  = RegInit(0.B)
    val prevTakenFtqPtr = Reg(new FtqPtr)
    for (i <- 0 until DecodeWidth - 1) {
      // for instr that is not the last, if a taken br, the next instr should not have the same ftqPtr
      // for instr that is the last, record and check next request
      when(ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && ibuffer.io.out(i).bits.pred_taken) {
        when(ibuffer.io.out(i + 1).fire) {
          // not last br, check now
        }.otherwise {
          // last br, record its info
          prevTakenValid  := true.B
          prevTakenFtqPtr := checkTargetPtr(i)
        }
      }
      XSError(
        ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && ibuffer.io.out(i).bits.pred_taken &&
          ibuffer.io.out(i + 1).fire &&
          (checkTargetPtr(i) + 1.U).value =/= checkTargetPtr(i + 1).value,
        "taken br should have consecutive ftqPtr\n"
      )
    }
    when(ibuffer.io.out(DecodeWidth - 1).fire && ibuffer.io.out(DecodeWidth - 1).bits.pd.isBr && ibuffer.io.out(
      DecodeWidth - 1
    ).bits.pred_taken) {
      // last instr is a br, record its info
      prevTakenValid  := true.B
      prevTakenFtqPtr := checkTargetPtr(DecodeWidth - 1)
    }
    when(prevTakenValid && ibuffer.io.out(0).fire) {
      prevTakenValid := false.B
    }
    XSError(
      prevTakenValid && ibuffer.io.out(0).fire &&
        (prevTakenFtqPtr + 1.U).value =/= checkTargetPtr(0).value,
      "taken br should have consecutive ftqPtr\n"
    )
    when(needFlush) {
      prevTakenValid := false.B
    }
  }

  def checkNotTakenPC(): Unit = {
    val prevNotTakenPC    = Reg(PrunedAddr(VAddrBits))
    val prevIsRVC         = Reg(Bool())
    val prevNotTakenValid = RegInit(0.B)

    for (i <- 0 until DecodeWidth - 1) {
      when(ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && !ibuffer.io.out(i).bits.pred_taken) {
        when(ibuffer.io.out(i + 1).fire) {}.otherwise {
          prevNotTakenValid := true.B
          prevIsRVC         := ibuffer.io.out(i).bits.pd.isRVC
          prevNotTakenPC    := ibuffer.io.out(i).bits.pc
        }
      }
      XSError(
        ibuffer.io.out(i).fire && ibuffer.io.out(i).bits.pd.isBr && !ibuffer.io.out(i).bits.pred_taken &&
          ibuffer.io.out(i + 1).fire &&
          ibuffer.io.out(i).bits.pc + Mux(ibuffer.io.out(i).bits.pd.isRVC, 2.U, 4.U) =/= ibuffer.io.out(
            i + 1
          ).bits.pc,
        "not-taken br should have consecutive pc\n"
      )
    }
    when(ibuffer.io.out(DecodeWidth - 1).fire && ibuffer.io.out(DecodeWidth - 1).bits.pd.isBr && !ibuffer.io.out(
      DecodeWidth - 1
    ).bits.pred_taken) {
      prevNotTakenValid := true.B
      prevIsRVC         := ibuffer.io.out(DecodeWidth - 1).bits.pd.isRVC
      prevNotTakenPC    := ibuffer.io.out(DecodeWidth - 1).bits.pc
    }
    when(prevNotTakenValid && ibuffer.io.out(0).fire) {
      prevNotTakenValid := false.B
    }
    XSError(
      prevNotTakenValid && ibuffer.io.out(0).fire &&
        prevNotTakenPC + Mux(prevIsRVC, 2.U, 4.U) =/= PrunedAddrInit(ibuffer.io.out(0).bits.pc),
      "not-taken br should have same pc\n"
    )
    when(needFlush) {
      prevNotTakenValid := false.B
    }
  }

  def checkTakenPC(): Unit = {
    val prevTakenFtqPtr = Reg(new FtqPtr)
    val prevTakenValid  = RegInit(0.B)
    val prevTakenTarget = Wire(PrunedAddr(VAddrBits))
    prevTakenTarget := checkPcMem((prevTakenFtqPtr + 1.U).value).startAddr

    for (i <- 0 until DecodeWidth - 1) {
      when(ibuffer.io.out(i).fire && !ibuffer.io.out(i).bits.pd.notCFI && ibuffer.io.out(i).bits.pred_taken) {
        when(ibuffer.io.out(i + 1).fire) {}.otherwise {
          prevTakenValid  := true.B
          prevTakenFtqPtr := checkTargetPtr(i)
        }
      }
      XSError(
        ibuffer.io.out(i).fire && !ibuffer.io.out(i).bits.pd.notCFI && ibuffer.io.out(i).bits.pred_taken &&
          ibuffer.io.out(i + 1).fire &&
          checkTarget(i) =/= PrunedAddrInit(ibuffer.io.out(i + 1).bits.pc),
        "taken instr should follow target pc\n"
      )
    }
    when(ibuffer.io.out(DecodeWidth - 1).fire && !ibuffer.io.out(DecodeWidth - 1).bits.pd.notCFI && ibuffer.io.out(
      DecodeWidth - 1
    ).bits.pred_taken) {
      prevTakenValid  := true.B
      prevTakenFtqPtr := checkTargetPtr(DecodeWidth - 1)
    }
    when(prevTakenValid && ibuffer.io.out(0).fire) {
      prevTakenValid := false.B
    }
    XSError(
      prevTakenValid && ibuffer.io.out(0).fire &&
        prevTakenTarget =/= PrunedAddrInit(ibuffer.io.out(0).bits.pc),
      "taken instr should follow target pc\n"
    )
    when(needFlush) {
      prevTakenValid := false.B
    }
  }

  /* *** checks *** */
  // checkNotTakenConsecutive()
  checkTakenNotConsecutive()
  checkTakenPC()
  checkNotTakenPC()

  ifu.io.robCommits <> io.backend.toFtq.rob_commits

  ibuffer.io.flush                := needFlush
  ibuffer.io.ControlRedirect      := flushControlRedirect
  ibuffer.io.MemVioRedirect       := flushMemVioRedirect
  ibuffer.io.ControlBTBMissBubble := flushControlBTBMiss
  ibuffer.io.TAGEMissBubble       := flushTAGEMiss
  ibuffer.io.SCMissBubble         := flushSCMiss
  ibuffer.io.ITTAGEMissBubble     := flushITTAGEMiss
  ibuffer.io.RASMissBubble        := flushRASMiss
  ibuffer.io.decodeCanAccept      := io.backend.canAccept

  flushControlBTBMiss := ftq.io.ControlBTBMissBubble
  flushTAGEMiss       := ftq.io.TAGEMissBubble
  flushSCMiss         := ftq.io.SCMissBubble
  flushITTAGEMiss     := ftq.io.ITTAGEMissBubble
  flushRASMiss        := ftq.io.RASMissBubble

  io.backend.cfVec <> ibuffer.io.out
  io.backend.stallReason <> ibuffer.io.stallReason

  instrUncache.io.fromIfu <> ifu.io.toUncache
  ifu.io.fromUncache <> instrUncache.io.toIfu
  instrUncache.io.flush := false.B

  io.error <> RegNext(RegNext(icache.io.error))

  icache.io.hartId := io.hartId

  itlbRepeater1.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr.map(_.toUInt)

  io.frontendInfo.ibufFull := RegNext(ibuffer.io.full)
  io.resetInFrontend       := reset.asBool

  /* *** perfEvents *** */
  private val pfEvent = Module(new PFEvent)
  pfEvent.io.distribute_csr := io.csrCtrl.distribute_csr
  private val csrPerfEvents = pfEvent.io.hpmevent.take(8)

  private val perfFromUnits = Seq(ifu, ibuffer, icache, ftq).flatMap(_.getPerfEvents)
  private val perfFromIO    = Seq()
  private val perfBlock     = Seq()
  // let index = 0 be no event
  private val allPerfEvents = Seq(("noEvent", 0.U)) ++ perfFromUnits ++ perfFromIO ++ perfBlock

  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("Frontend perfEvents Set", name, inc, i)
    }
  }

  private val allPerfInc = allPerfEvents.map(_._2.asTypeOf(new PerfEvent))
  override val perfEvents: Seq[(String, UInt)] = HPerfMonitor(csrPerfEvents, allPerfInc).getPerfEvents
  generatePerfEvent()

  /* *** DFT (Design for Testability) *** */
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(Int.MaxValue, "MbistPipeFrontend", hasMbist)
  private val mbistIntf = if (hasMbist) {
    val params = mbistPl.get.nodeParams
    val intf = Some(Module(new MbistInterface(
      params = Seq(params),
      ids = Seq(mbistPl.get.childrenIds),
      name = s"MbistIntfFrontend",
      pipelineNum = 1
    )))
    intf.get.toPipeline.head <> mbistPl.get.mbist
    mbistPl.get.registerCSV(intf.get.info, "MbistFrontend")
    intf.get.mbist := DontCare
    dontTouch(intf.get.mbist)
    // TODO: add mbist controller connections here
    intf
  } else {
    None
  }
  private val sigFromSrams = if (hasDFT) Some(SramHelper.genBroadCastBundleTop()) else None
  private val cg           = ClockGate.genTeSrc
  dontTouch(cg)

  if (hasMbist) {
    cg.cgen := io.dft.get.cgen
  } else {
    cg.cgen := false.B
  }

  sigFromSrams.foreach(_ := DontCare)
  sigFromSrams.zip(io.dft).foreach {
    case (sig, dft) =>
      if (hasMbist) {
        sig.ram_hold     := dft.ram_hold
        sig.ram_bypass   := dft.ram_bypass
        sig.ram_bp_clken := dft.ram_bp_clken
        sig.ram_aux_clk  := dft.ram_aux_clk
        sig.ram_aux_ckbp := dft.ram_aux_ckbp
        sig.ram_mcp_hold := dft.ram_mcp_hold
        sig.cgen         := dft.cgen
      }
      if (hasSramCtl) {
        sig.ram_ctl := dft.ram_ctl
      }
  }
}
