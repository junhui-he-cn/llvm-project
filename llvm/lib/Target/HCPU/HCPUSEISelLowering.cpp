//===-- HCPUSEISelLowering.cpp - HCPUSE DAG Lowering Interface --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Subclass of HCPUTargetLowering specialized for HCPU32.
//
//===----------------------------------------------------------------------===//

#include "HCPUSEISelLowering.h"
#include "HCPUISelLowering.h"
#include "HCPUMachineFunction.h"

#include "HCPURegisterInfo.h"
#include "HCPUTargetMachine.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "HCPU-isel"

static cl::opt<bool> EnableHCPUTailCalls("enable-hcpu-tail-calls", cl::Hidden,
                                         cl::desc("HCPU: Enable tail calls."),
                                         cl::init(false));

HCPUSETargetLowering::HCPUSETargetLowering(const HCPUTargetMachine &TM,
                                           const HCPUSubtarget &STI)
    : HCPUTargetLowering(TM, STI) {
  addRegisterClass(MVT::i32, &HCPU::CPURegsRegClass);

  // must, computeRegisterProperties - Once all of the register classes are
  //  added, this allows us to compute derived properties we expose.
  computeRegisterProperties(Subtarget.getRegisterInfo());
}

SDValue HCPUSETargetLowering::LowerOperation(SDValue Op,
                                             SelectionDAG &DAG) const {
  return HCPUTargetLowering::LowerOperation(Op, DAG);
}

const HCPUTargetLowering *
llvm::createHCPUSETargetLowering(const HCPUTargetMachine &TM,
                           const HCPUSubtarget &STI) {
  return new HCPUSETargetLowering(TM, STI);
}
