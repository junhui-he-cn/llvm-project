//===-- HCPUTargetInfo.cpp - HCPU Target Implementation -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "../HCPU.h"
#include "HCPUTargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

Target &llvm::getTheHCPUTarget() {
  static Target TheHCPUTarget;
  return TheHCPUTarget;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeHCPUTargetInfo() {
  RegisterTarget<Triple::hcpu, true> X(getTheHCPUTarget(), "hcpu", "HCPU", "HCPU");
}