//===---- HCPUABIInfo.cpp - Information about HCPU ABI's ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "HCPUABIInfo.h"
#include "HCPUMCTargetDesc.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCRegister.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

static cl::opt<bool> EnableHCPUS32Calls(
    "hcpu-s32-calls", cl::Hidden,
    cl::desc("HCPU S32 call: use stack only to pass argumetns."),
    cl::init(false));

namespace {
static const MCPhysReg O32IntRegs[4] = {HCPU::A0, HCPU::A1};
static const MCPhysReg S32IntRegs[1] = {0};
}

const ArrayRef<MCPhysReg> HCPUABIInfo::GetByValArgRegs() const {
  if (IsO32())
    return ArrayRef(O32IntRegs);
  if (IsS32())
    return ArrayRef(S32IntRegs);
  llvm_unreachable("Unhandled ABI");
}

const ArrayRef<MCPhysReg> HCPUABIInfo::GetVarArgsRegs() const {
  if (IsO32())
    return ArrayRef(O32IntRegs);
  if (IsS32())
    return ArrayRef(S32IntRegs);
  llvm_unreachable("Unhandled ABI");
}

unsigned HCPUABIInfo::GetCalleeAllocdArgSizeInBytes(CallingConv::ID CC) const {
  if (IsO32())
    return CC != 0;
  if (IsS32())
    return 0;
  llvm_unreachable("Unhandled ABI");
}

HCPUABIInfo HCPUABIInfo::computeTargetABI() {
  HCPUABIInfo abi(ABI::Unknown);

  if (EnableHCPUS32Calls)
    abi = ABI::S32;
  else
    abi = ABI::O32;

  assert(abi.ThisABI != ABI::Unknown);

  return abi;
}

unsigned HCPUABIInfo::GetStackPtr() const{
  return HCPU::SP;
}

unsigned HCPUABIInfo::GetFramePtr() const {
  return HCPU::FP;
}

unsigned HCPUABIInfo::GetNullPtr() const {
  return HCPU::ZERO;
}

unsigned HCPUABIInfo::GetEhDataReg(unsigned I) const {
  static const unsigned EhDataReg[] {
    HCPU::A0, HCPU::A1
  };

  return EhDataReg[I];
}

int HCPUABIInfo::EhDataRegSize() const {
  if (ThisABI == ABI::S32)
    return 0;
  else
    return 2;
}