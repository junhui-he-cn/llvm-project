//===-- HCPUMachineFunctionInfo.h - Private data used for HCPU ----*- C++ -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the HCPU specific subclass of MachineFunctionInfo.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_HCPUMACHINEFUNCTION_H
#define LLVM_LIB_TARGET_HCPU_HCPUMACHINEFUNCTION_H

#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/PseudoSourceValue.h"
#include "llvm/Target/TargetMachine.h"
#include <map>

namespace llvm {
class HCPUFunctionInfo : public MachineFunctionInfo {
public:
  HCPUFunctionInfo(MachineFunction &MF)
      : MF(MF), VarArgsFrameIndex(0), MaxCallFrameSize(0) {}

  ~HCPUFunctionInfo();

  int getVarArgsFrameInfo() const { return VarArgsFrameIndex; }
  void setVarArgsFrameIndex(int Index) { VarArgsFrameIndex = Index; }

private:
  virtual void anchor();

  MachineFunction &MF;

  /// VarArgsFrameIndex - FrameIndex for start of varargs area.
  int VarArgsFrameIndex;

  unsigned MaxCallFrameSize;
};
} // namespace llvm

#endif