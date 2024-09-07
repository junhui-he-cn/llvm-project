//===-- HCPUSubtarget.cpp - HCPU Subtarget Information --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the HCPU specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "HCPUSubtarget.h"

#include "HCPU.h"
#include "HCPUMachineFunction.h"
#include "HCPURegisterInfo.h"

#include "HCPUSEISelLowering.h"
#include "HCPUTargetMachine.h"
#include "MCTargetDesc/HCPUABIInfo.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define DEBUG_TYPE "hcpu-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "HCPUGenSubtargetInfo.inc"

extern bool FixGlobalBaseReg;

void HCPUSubtarget::anchor() {}

HCPUSubtarget::HCPUSubtarget(const Triple &TT, StringRef CPU, StringRef FS,
                             bool little, const HCPUTargetMachine &_TM)
    : HCPUGenSubtargetInfo(TT, CPU, CPU, FS), IsLittle(little), TM(_TM),
      TargetTriple(TT), TSInfo(),
      InstrInfo(
          HCPUInstrInfo::create(initializeSubtargetDependencies(CPU, FS, TM))),
      FrameLowering(HCPUFrameLowering::create(*this)),
      TLInfo(HCPUSETargetLowering::create(TM, *this)) {}

bool HCPUSubtarget::isPositionIndependent() const {
  return TM.isPositionIndependent();
}

HCPUSubtarget &
HCPUSubtarget::initializeSubtargetDependencies(StringRef CPU, StringRef FS,
                                               const TargetMachine &TM) {
  if (TargetTriple.getArch() == Triple::hcpu) {
    if (CPU.empty() || CPU == "generic") {
      CPU = "hcpu32II";
    } else if (CPU == "help") {
      CPU = "";
      return *this;
    } else if (CPU != "hcpu32I" && CPU != "hcpu32II") {
      CPU = "hcpu32II";
    }
  } else {
    errs() << "!!!Error, TargetTriple.getArch() = " << TargetTriple.getArch()
           << "CPU = " << CPU << "\n";
    exit(0);
  }

  if (CPU == "hcpu32I")
    HCPUArchVersion = HCPU32I;
  else if (CPU == "hcpu32II")
    HCPUArchVersion = HCPU32II;

  if (isHCPU32I()) {
    HasCmp = true;
    HasSlt = false;
  } else if (isHCPU32II()) {
    HasCmp = false;
    HasSlt = true;
  } else {
    errs() << "-mcpu must be empty(default:cpu032II), cpu032I or cpu032II"
           << "\n";
  }

  ParseSubtargetFeatures(CPU, CPU, FS);
  InstrItins = getInstrItineraryForCPU(CPU);

  return *this;
}

bool HCPUSubtarget::abiUsesSoftFloat() const {
  // return TM->Options.UseSoftFloat;
  return true;
}

const HCPUABIInfo &HCPUSubtarget::getABI() const { return TM.getABI(); }