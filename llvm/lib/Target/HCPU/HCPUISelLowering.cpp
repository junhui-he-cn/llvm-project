//===-- HCPUISelLowering.cpp - HCPU DAG Lowering Implementation -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that HCPU uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//
#include "HCPUISelLowering.h"

#include "HCPUMachineFunction.h"
#include "HCPUSubtarget.h"
#include "HCPUTargetMachine.h"
#include "HCPUTargetObjectFile.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "hcpu-lower"

const char *HCPUTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch (Opcode) {
  case HCPUISD::JmpLink:
    return "HCPUISD::JmpLink";
  case HCPUISD::TailCall:
    return "HCPUISD::TailCall";
  case HCPUISD::Hi:
    return "HCPUISD::Hi";
  case HCPUISD::Lo:
    return "HCPUISD::Lo";
  case HCPUISD::GPRel:
    return "HCPUISD::GPRel";
  case HCPUISD::Ret:
    return "HCPUISD::Ret";
  case HCPUISD::EH_RETURN:
    return "HCPUISD::EH_RETURN";
  case HCPUISD::DivRem:
    return "HCPUISD::DivRem";
  case HCPUISD::DivRemU:
    return "HCPUISD::DivRemU";
  case HCPUISD::Wrapper:
    return "HCPUISD::Wrapper";
  default:
    return NULL;
  }
}

HCPUTargetLowering::HCPUTargetLowering(const HCPUTargetMachine &TM,
                                       const HCPUSubtarget &STI)
    : TargetLowering(TM), Subtarget(STI), ABI(TM.getABI()) {
  // HCPU Custom Operations

  // Operations not directly supported by HCPU.

  //- Set .align 2
  // It will emit .align 2 later
  setMinFunctionAlignment(Align(2));

  setOperationAction(ISD::SDIV, MVT::i32, Expand);
  setOperationAction(ISD::SREM, MVT::i32, Expand);
  setOperationAction(ISD::UDIV, MVT::i32, Expand);
  setOperationAction(ISD::UREM, MVT::i32, Expand);

    // HCPU doesn't have sext_inreg, replace them with shl/sra.
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1 , Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8 , Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i16 , Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i32 , Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::Other , Expand);

  setOperationAction(ISD::GlobalAddress, MVT::i32, Custom);

  // HCPU does not have i1 type, so use i32 for
  // setcc operations results (slt, sgt, ...).
  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrNegativeOneBooleanContent);

  // Load extented operations for i1 types must be promoted
  for (MVT VT : MVT::integer_valuetypes()) {
    setLoadExtAction(ISD::EXTLOAD,  VT, MVT::i1,  Promote);
    setLoadExtAction(ISD::ZEXTLOAD, VT, MVT::i1,  Promote);
    setLoadExtAction(ISD::SEXTLOAD, VT, MVT::i1,  Promote);
  }
    // Handle i64 shl
  setOperationAction(ISD::SHL_PARTS,          MVT::i32,   Expand);
  setOperationAction(ISD::SRA_PARTS,          MVT::i32,   Expand);
  setOperationAction(ISD::SRL_PARTS,          MVT::i32,   Expand);

  setTargetDAGCombine(ISD::SDIVREM);
  setTargetDAGCombine(ISD::UDIVREM);
}

const HCPUTargetLowering *
HCPUTargetLowering::create(const HCPUTargetMachine &TM,
                           const HCPUSubtarget &STI) {
  return createHCPUSETargetLowering(TM, STI);
}

//===----------------------------------------------------------------------===//
//  Lower helper functions
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Misc Lower Operation implementation
//===----------------------------------------------------------------------===//

#include "HCPUGenCallingConv.inc"

SDValue HCPUTargetLowering::getGlobalReg(SelectionDAG &DAG, EVT Ty) const {
  HCPUFunctionInfo *FI = DAG.getMachineFunction().getInfo<HCPUFunctionInfo>();
  return DAG.getRegister(FI->getGlobalBaseReg(), Ty);
}

//@getTargetNode(GlobalAddressSDNode
SDValue HCPUTargetLowering::getTargetNode(GlobalAddressSDNode *N, EVT Ty,
                                          SelectionDAG &DAG,
                                          unsigned Flag) const {
  return DAG.getTargetGlobalAddress(N->getGlobal(), SDLoc(N), Ty, 0, Flag);
}

//@getTargetNode(ExternalSymbolSDNode
SDValue HCPUTargetLowering::getTargetNode(ExternalSymbolSDNode *N, EVT Ty,
                                          SelectionDAG &DAG,
                                          unsigned Flag) const {
  return DAG.getTargetExternalSymbol(N->getSymbol(), Ty, Flag);
}

//===----------------------------------------------------------------------===//
//@            Formal Arguments Calling Convention Implementation
//===----------------------------------------------------------------------===//

/// LowerFormalArguments - transform physical registers into virtual registers
/// and generate load operations for arguments places on the stack.
SDValue HCPUTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  return Chain;
}

SDValue
HCPUTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                                bool IsVarArg,
                                const SmallVectorImpl<ISD::OutputArg> &Outs,
                                const SmallVectorImpl<SDValue> &OutVals,
                                const SDLoc &DL, SelectionDAG &DAG) const {
  // CCValAssign - represent the assignment of
  // the return value to a location
  SmallVector<CCValAssign, 16> RVLocs;
  MachineFunction &MF = DAG.getMachineFunction();

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, IsVarArg, MF, RVLocs, *DAG.getContext());
  HCPUCC HCPUCCInfo(CallConv, ABI.IsO32(), CCInfo);

  // Analyze return values.
  HCPUCCInfo.analyzeReturn(Outs, Subtarget.abiUsesSoftFloat(),
                           MF.getFunction().getReturnType());

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);

  // Copy the result values into the output registers.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    SDValue Val = OutVals[i];
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    if (RVLocs[i].getValVT() != RVLocs[i].getLocVT())
      Val = DAG.getNode(ISD::BITCAST, DL, RVLocs[i].getLocVT(), Val);

    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Val, Flag);

    // Guarantee that all emitted copies are stuck together with flags.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  // The HCPU ABIs for returning structs by value requires that we copy
  // the sret argument into $v0 for the return. We saved the argument into
  // a virtual register in the entry block, so now we copy the value out
  // and into $v0.
  if (MF.getFunction().hasStructRetAttr()) {
    HCPUFunctionInfo *HCPUFI = MF.getInfo<HCPUFunctionInfo>();
    unsigned Reg = HCPUFI->getSRetReturnReg();

    if (!Reg)
      llvm_unreachable("sret virtual register not created in the entry block");
    SDValue Val =
        DAG.getCopyFromReg(Chain, DL, Reg, getPointerTy(DAG.getDataLayout()));
    unsigned V0 = HCPU::V0;

    Chain = DAG.getCopyToReg(Chain, DL, V0, Val, Flag);
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(V0, getPointerTy(DAG.getDataLayout())));
  }

  RetOps[0] = Chain; // Update chain.

  // Add the flag if we have it.
  if (Flag.getNode())
    RetOps.push_back(Flag);

  // Return on HCPU is always a "ret $lr"
  return DAG.getNode(HCPUISD::Ret, DL, MVT::Other, RetOps);
}

HCPUTargetLowering::HCPUCC::HCPUCC(
    CallingConv::ID CC, bool IsO32_, CCState &Info,
    HCPUCC::SpecialCallingConvType SpecialCallingConv_)
    : CCInfo(Info), CallConv(CC), IsO32(IsO32_) {
  // Pre-allocate reserved argument area.
  CCInfo.AllocateStack(reservedArgArea(), Align(1));
}

template <typename Ty>
void HCPUTargetLowering::HCPUCC::analyzeReturn(
    const SmallVectorImpl<Ty> &RetVals, bool IsSoftFloat,
    const SDNode *CallNode, const Type *RetTy) const {
  CCAssignFn *Fn;

  Fn = RetCC_HCPU;

  for (unsigned I = 0, E = RetVals.size(); I < E; ++I) {
    MVT VT = RetVals[I].VT;
    ISD::ArgFlagsTy Flags = RetVals[I].Flags;
    MVT RegVT = this->getRegVT(VT, IsSoftFloat);

    if (Fn(I, VT, RegVT, CCValAssign::Full, Flags, this->CCInfo)) {
#ifndef NDEBUG
      dbgs() << "Call result #" << I << " has unhandled type "
             << EVT(VT).getEVTString() << '\n';
#endif
      llvm_unreachable(nullptr);
    }
  }
}

void HCPUTargetLowering::HCPUCC::analyzeCallResult(
    const SmallVectorImpl<ISD::InputArg> &Ins, bool IsSoftFloat,
    const SDNode *CallNode, const Type *RetTy) const {
  analyzeReturn(Ins, IsSoftFloat, CallNode, RetTy);
}

void HCPUTargetLowering::HCPUCC::analyzeReturn(
    const SmallVectorImpl<ISD::OutputArg> &Outs, bool IsSoftFloat,
    const Type *RetTy) const {
  analyzeReturn(Outs, IsSoftFloat, nullptr, RetTy);
}
unsigned HCPUTargetLowering::HCPUCC::reservedArgArea() const {
  return (IsO32 && (CallConv != CallingConv::Fast)) ? 8 : 0;
}
MVT HCPUTargetLowering::HCPUCC::getRegVT(MVT VT, bool IsSoftFloat) const {
  if (IsSoftFloat || IsO32)
    return VT;

  return VT;
}

static SDValue performDivRemCombine(SDNode *N, SelectionDAG& DAG,
                                    TargetLowering::DAGCombinerInfo &DCI,
                                    const HCPUSubtarget &Subtarget) {
  if (DCI.isBeforeLegalizeOps())
    return SDValue();

  EVT Ty = N->getValueType(0);
  unsigned LO = HCPU::LO;
  unsigned HI = HCPU::HI;
  unsigned Opc = N->getOpcode() == ISD::SDIVREM ? HCPUISD::DivRem :
                                                  HCPUISD::DivRemU;
  SDLoc DL(N);

  SDValue DivRem = DAG.getNode(Opc, DL, MVT::Glue,
                               N->getOperand(0), N->getOperand(1));
  SDValue InChain = DAG.getEntryNode();
  SDValue InGlue = DivRem;

  // insert MFLO
  if (N->hasAnyUseOfValue(0)) {
    SDValue CopyFromLo = DAG.getCopyFromReg(InChain, DL, LO, Ty,
                                            InGlue);
    DAG.ReplaceAllUsesOfValueWith(SDValue(N, 0), CopyFromLo);
    InChain = CopyFromLo.getValue(1);
    InGlue = CopyFromLo.getValue(2);
  }

  // insert MFHI
  if (N->hasAnyUseOfValue(1)) {
    SDValue CopyFromHi = DAG.getCopyFromReg(InChain, DL,
                                            HI, Ty, InGlue);
    DAG.ReplaceAllUsesOfValueWith(SDValue(N, 1), CopyFromHi);
  }

  return SDValue();
}

SDValue HCPUTargetLowering::PerformDAGCombine(SDNode *N, DAGCombinerInfo &DCI)
  const {
  SelectionDAG &DAG = DCI.DAG;
  unsigned Opc = N->getOpcode();

  switch (Opc) {
  default: break;
  case ISD::SDIVREM:
  case ISD::UDIVREM:
    return performDivRemCombine(N, DAG, DCI, Subtarget);
  }

  return SDValue();
}

SDValue HCPUTargetLowering::
LowerOperation(SDValue Op, SelectionDAG &DAG) const
{
  switch (Op.getOpcode())
  {
  case ISD::GlobalAddress:      return LowerGlobalAddress(Op, DAG);
  }
  return SDValue();
}


SDValue HCPUTargetLowering::LowerGlobalAddress(SDValue Op,
                                               SelectionDAG &DAG) const {
  //@lowerGlobalAddress }
  SDLoc DL(Op);
  const HCPUTargetObjectFile *TLOF =
        static_cast<const HCPUTargetObjectFile *>(
            getTargetMachine().getObjFileLowering());
  //@lga 1 {
  EVT Ty = Op.getValueType();
  GlobalAddressSDNode *N = cast<GlobalAddressSDNode>(Op);
  const GlobalValue *GV = N->getGlobal();
  //@lga 1 }

  if (!isPositionIndependent()) {
    //@ %gp_rel relocation
    const GlobalObject *GO = GV->getAliaseeObject();
    if (GO && TLOF->IsGlobalInSmallSection(GO, getTargetMachine())) {
      SDValue GA = DAG.getTargetGlobalAddress(GV, DL, MVT::i32, 0,
                                              HCPUII::MO_GPREL);
      SDValue GPRelNode = DAG.getNode(HCPUISD::GPRel, DL,
                                      DAG.getVTList(MVT::i32), GA);
      SDValue GPReg = DAG.getRegister(HCPU::GP, MVT::i32);
      return DAG.getNode(ISD::ADD, DL, MVT::i32, GPReg, GPRelNode);
    }

    //@ %hi/%lo relocation
    return getAddrNonPIC(N, Ty, DAG);
  }

  if (GV->hasInternalLinkage() || (GV->hasLocalLinkage() && !isa<Function>(GV)))
    return getAddrLocal(N, Ty, DAG);

  //@large section
  const GlobalObject *GO = GV->getAliaseeObject();
  if (GO && !TLOF->IsGlobalInSmallSection(GO, getTargetMachine()))
    return getAddrGlobalLargeGOT(
        N, Ty, DAG, HCPUII::MO_GOT_HI16, HCPUII::MO_GOT_LO16, 
        DAG.getEntryNode(), 
        MachinePointerInfo::getGOT(DAG.getMachineFunction()));
  return getAddrGlobal(
      N, Ty, DAG, HCPUII::MO_GOT, DAG.getEntryNode(), 
      MachinePointerInfo::getGOT(DAG.getMachineFunction()));
}

bool
HCPUTargetLowering::isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const {
  // The HCPU target isn't yet aware of offsets.
  return false;
}

EVT HCPUTargetLowering::getSetCCResultType(const DataLayout &, LLVMContext &,
                                           EVT VT) const {
  if (!VT.isVector())
    return MVT::i32;
  return VT.changeVectorElementTypeToInteger();
}