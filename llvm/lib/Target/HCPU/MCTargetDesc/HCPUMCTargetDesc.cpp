//===-- HCPUMCTargetDesc.cpp - HCPU Target Descriptions -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides HCPU specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "HCPUMCTargetDesc.h"
#include "HCPUMCAsmInfo.h"
#include "InstPrinter/HCPUInstPrinter.h"
#include "TargetInfo/HCPUTargetInfo.h"
#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCInstrAnalysis.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MachineLocation.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

#define GET_INSTRINFO_MC_DESC
#include "HCPUGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "HCPUGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "HCPUGenRegisterInfo.inc"

//@1 {
/// Select the HCPU Architecture Feature for the given triple and cpu name.
/// The function will be called at command 'llvm-objdump -d' for HCPU elf input.
static std::string selectHCPUArchFeature(const Triple &TT, StringRef CPU) {
  std::string HCPUArchFeature;
  if (CPU.empty() || CPU == "generic") {
    if (TT.getArch() == Triple::hcpu) {
      if (CPU.empty() || CPU == "hcpu32II") {
        HCPUArchFeature = "+hcpu32II";
      } else {
        if (CPU == "hcpu32I") {
          HCPUArchFeature = "+hcpu32I";
        }
      }
    }
  }
  return HCPUArchFeature;
}
//@1 }

static MCInstrInfo *createHCPUMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitHCPUMCInstrInfo(X); // defined in HCPUGenInstrInfo.inc
  return X;
}

static MCRegisterInfo *createHCPUMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitHCPUMCRegisterInfo(X, HCPU::SW); // defined in HCPUGenRegisterInfo.inc
  return X;
}

static MCSubtargetInfo *createHCPUMCSubtargetInfo(const Triple &TT,
                                                  StringRef CPU, StringRef FS) {
  std::string ArchFS = selectHCPUArchFeature(TT, CPU);
  if (!FS.empty()) {
    if (!ArchFS.empty())
      ArchFS = ArchFS + "," + FS.str();
    else
      ArchFS = FS.str();
  }
  return createHCPUMCSubtargetInfoImpl(TT, CPU, /*TuneCPU*/ CPU, ArchFS);
  // createHCPUMCSubtargetInfoImpl defined in HCPUGenSubtargetInfo.inc
}

static MCAsmInfo *createHCPUMCAsmInfo(const MCRegisterInfo &MRI,
                                      const Triple &TT,
                                      const MCTargetOptions &Options) {
  MCAsmInfo *MAI = new HCPUMCAsmInfo(TT);

  unsigned SP = MRI.getDwarfRegNum(HCPU::SP, true);
  MCCFIInstruction Inst = MCCFIInstruction::createDefCfaRegister(nullptr, SP);
  MAI->addInitialFrameState(Inst);

  return MAI;
}

static MCInstPrinter *createHCPUMCInstPrinter(const Triple &T,
                                              unsigned SyntaxVariant,
                                              const MCAsmInfo &MAI,
                                              const MCInstrInfo &MII,
                                              const MCRegisterInfo &MRI) {
  return new HCPUInstPrinter(MAI, MII, MRI);
}

namespace {

class HCPUMCInstrAnalysis : public MCInstrAnalysis {
public:
  HCPUMCInstrAnalysis(const MCInstrInfo *Info) : MCInstrAnalysis(Info) {}
};
} // namespace

static MCInstrAnalysis *createHCPUMCInstrAnalysis(const MCInstrInfo *Info) {
  return new HCPUMCInstrAnalysis(Info);
}

//@2 {
extern "C" void LLVMInitializeHCPUTargetMC() {

  // Register the MC asm info.
  // RegisterMCAsmInfoFn X(getTheHCPUTarget(), createHCPUMCAsmInfo);

  TargetRegistry::RegisterMCAsmInfo(getTheHCPUTarget(), createHCPUMCAsmInfo);

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(getTheHCPUTarget(),
                                      createHCPUMCInstrInfo);

  // Register the MC register info.
  TargetRegistry::RegisterMCRegInfo(getTheHCPUTarget(),
                                    createHCPUMCRegisterInfo);

  // Register the MC subtarget info.
  TargetRegistry::RegisterMCSubtargetInfo(getTheHCPUTarget(),
                                          createHCPUMCSubtargetInfo);
  // Register the MC instruction analyzer.
  TargetRegistry::RegisterMCInstrAnalysis(getTheHCPUTarget(),
                                          createHCPUMCInstrAnalysis);
  // Register the MCInstPrinter.
  TargetRegistry::RegisterMCInstPrinter(getTheHCPUTarget(),
                                        createHCPUMCInstPrinter);
}
//@2 }
