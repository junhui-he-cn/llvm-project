//===-- HCPUSEInstrInfo.cpp - HCPU32/64 Instruction Information -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the HCPU32/64 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "HCPUSEInstrInfo.h"

#include "HCPUMachineFunction.h"
#include "HCPUTargetMachine.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

HCPUSEInstrInfo::HCPUSEInstrInfo(const HCPUSubtarget &STI)
    : HCPUInstrInfo(STI),
      RI(STI) {}

const HCPURegisterInfo &HCPUSEInstrInfo::getRegisterInfo() const {
  return RI;
}

const HCPUInstrInfo *llvm::createHCPUSEInstrInfo(const HCPUSubtarget &STI) {
  return new HCPUSEInstrInfo(STI);
}

/// Expand Pseudo instructions into real backend instructions
bool HCPUSEInstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
//@expandPostRAPseudo-body
  MachineBasicBlock &MBB = *MI.getParent();

  switch (MI.getDesc().getOpcode()) {
  default:
    return false;
  case HCPU::RetLR:
    expandRetLR(MBB, MI);
    break;
  }

  MBB.erase(MI);
  return true;
}
void HCPUSEInstrInfo::expandRetLR(MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator I) const {
  BuildMI(MBB, I, I->getDebugLoc(), get(HCPU::RET)).addReg(HCPU::LR);
}
