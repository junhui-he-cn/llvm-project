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
#include "llvm/IR/Function.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"


using namespace llvm;

bool FixGlobalBaseReg;

HCPUFunctionInfo::~HCPUFunctionInfo() {}

void HCPUFunctionInfo::anchor() {};