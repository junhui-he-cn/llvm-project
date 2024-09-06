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
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
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
    : TargetLowering(TM), Subtarget(STI), ABI(TM.getABI()) {}

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
  return DAG.getNode(HCPUISD::Ret, DL, MVT::Other, Chain,
                     DAG.getRegister(HCPU::LR, MVT::i32));
}
