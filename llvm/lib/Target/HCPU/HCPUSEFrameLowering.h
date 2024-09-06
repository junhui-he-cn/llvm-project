//===-- HCPUSEFrameLowering.h - HCPU32/64 frame lowering --------*- C++ -*-===//
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

#ifndef LLVM_LIB_TARGET_HCPU_HCPUSEFRAMELOWERING_H
#define LLVM_LIB_TARGET_HCPU_HCPUSEFRAMELOWERING_H

#include "HCPUFrameLowering.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {
class HCPUSEFrameLowering : public HCPUFrameLowering {
public:
  explicit HCPUSEFrameLowering(const HCPUSubtarget &STI);

  void emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
};
}


#endif