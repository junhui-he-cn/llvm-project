//===-- HCPUMachineFunctionInfo.cpp - Private data used for HCPU ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "HCPUMachineFunction.h"

#include "HCPUInstrInfo.h"
#include "HCPUSubtarget.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/PseudoSourceValueManager.h"
#include "llvm/IR/Function.h"

using namespace llvm;

bool FixGlobalBaseReg;

HCPUFunctionInfo::~HCPUFunctionInfo() {}

bool HCPUFunctionInfo::globalBaseRegFixed() const { return FixGlobalBaseReg; }

bool HCPUFunctionInfo::globalBaseRegSet() const { return GlobalBaseReg; }

unsigned HCPUFunctionInfo::getGlobalBaseReg() {
  return GlobalBaseReg = HCPU::GP;
}

void HCPUFunctionInfo::createEhDataRegsFI(MachineFunction &MF) {
  const TargetRegisterInfo &TRI = *MF.getSubtarget().getRegisterInfo();
  for (int I = 0; I < 2; ++I) {
    const TargetRegisterClass &RC = HCPU::CPURegsRegClass;

    EhDataRegFI[I] = MF.getFrameInfo().CreateStackObject(
        TRI.getSpillSize(RC), TRI.getSpillAlign(RC), false);
  }
}

void HCPUFunctionInfo::anchor() {};

MachinePointerInfo HCPUFunctionInfo::callPtrInfo(MachineFunction &MF,
                                                 const char *ES) {
  return MachinePointerInfo(MF.getPSVManager().getExternalSymbolCallEntry(ES));
}

MachinePointerInfo HCPUFunctionInfo::callPtrInfo(MachineFunction &MF,
                                                 const GlobalValue *GV) {
  return MachinePointerInfo(MF.getPSVManager().getGlobalValueCallEntry(GV));
}