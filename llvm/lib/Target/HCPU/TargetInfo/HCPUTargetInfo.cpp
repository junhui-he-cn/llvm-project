//===-- HCPUTargetInfo.cpp - HCPU Target Implementation -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "../HCPU.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

Target &getTheHCPUTarget() {
  static Target TheHCPUTarget;
  return TheHCPUTarget;
}

extern "C" void LLVMInitializeHCPUTargetInfo() {
  RegisterTarget<Triple::hcpu, true> X(getTheHCPUTarget(), "hcpu", "HCPU", "HCPU");
}