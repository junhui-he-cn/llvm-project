//===-- HCPUISEISelLowering.h - HCPUISE DAG Lowering Interface ----*- C++
//-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Subclass of HCPUITargetLowering specialized for HCPU32/64.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_HCPUSEISELLOWERING_H
#define LLVM_LIB_TARGET_HCPU_HCPUSEISELLOWERING_H

#include "HCPUISelLowering.h"
#include "HCPURegisterInfo.h"

namespace llvm {
class HCPUSETargetLowering : public HCPUTargetLowering {
public:
  explicit HCPUSETargetLowering(const HCPUTargetMachine &TM,
                                const HCPUSubtarget &STI);

  SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;

  bool isEligibleForTailCallOptimization(const HCPUCC &HCPUCCInfo,
                                     unsigned NextStackOffset,
                                     const HCPUFunctionInfo& FI) const override;
};
} // namespace llvm

#endif