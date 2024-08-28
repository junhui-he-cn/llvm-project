//===-- HCPU.h - Top-level interface for HCPU representation ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in
// the LLVM HCPU back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_HCPU_H
#define LLVM_LIB_TARGET_HCPU_HCPU_H

#include "MCTargetDesc/HCPUMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
  class HCPUTargetMachine;
  class FunctionPass;
} // end namespace llvm;

#endif