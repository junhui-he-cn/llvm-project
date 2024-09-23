//===-- HCPUFrameLowering.h - Define frame lowering for HCPU ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_LIB_TARGET_HCPU_HCPUFRAMELOWERING_H
#define LLVM_LIB_TARGET_HCPU_HCPUFRAMELOWERING_H

#include "HCPU.h"
#include "llvm/CodeGen/TargetFrameLowering.h"

namespace llvm {
class HCPUSubtarget;

class HCPUFrameLowering : public TargetFrameLowering {
protected:
  const HCPUSubtarget &STI;

public:
  explicit HCPUFrameLowering(const HCPUSubtarget &sti, unsigned Alignment)
      : TargetFrameLowering(StackGrowsDown, Align(Alignment), 0,
                            Align(Alignment)),
        STI(sti) {}

  static const HCPUFrameLowering *create(const HCPUSubtarget &ST);

  bool hasFP(const MachineFunction &MF) const override;

  MachineBasicBlock::iterator
  eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator I) const override;
};

const HCPUFrameLowering *createHCPUSEFrameLowering(const HCPUSubtarget &ST);
} // namespace llvm

#endif