//===-- HCPUInstrInfo.cpp - HCPU Instruction Information ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the HCPU implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "HCPUInstrInfo.h"

#include "HCPUMachineFunction.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_INSTRINFO_CTOR_DTOR
#include "HCPUGenInstrInfo.inc"

void HCPUInstrInfo::anchor() {}

HCPUInstrInfo::HCPUInstrInfo(const HCPUSubtarget &STI)
    : Subtarget(STI),
      HCPUGenInstrInfo(HCPU::ADJCALLSTACKDOWN, HCPU::ADJCALLSTACKUP) {}

const HCPUInstrInfo *HCPUInstrInfo::create(HCPUSubtarget &STI) {
  return createHCPUSEInstrInfo(STI);
}

unsigned HCPUInstrInfo::GetInstSizeInBytes(const MachineInstr &MI) const {
  switch (MI.getOpcode()) {
  default:
    return MI.getDesc().getSize();
  case TargetOpcode::INLINEASM: { // Inline Asm: Variable size.
    const MachineFunction *MF = MI.getParent()->getParent();
    const char *AsmStr = MI.getOperand(0).getSymbolName();
    return getInlineAsmLength(AsmStr, *MF->getTarget().getMCAsmInfo());
  }
  }
}

MachineMemOperand *
HCPUInstrInfo::GetMemOperand(MachineBasicBlock &MBB, int FI,
                             MachineMemOperand::Flags Flags) const {

  MachineFunction &MF = *MBB.getParent();
  MachineFrameInfo &MFI = MF.getFrameInfo();

  return MF.getMachineMemOperand(MachinePointerInfo::getFixedStack(MF, FI),
                                 Flags, MFI.getObjectSize(FI),
                                 MFI.getObjectAlign(FI));
}