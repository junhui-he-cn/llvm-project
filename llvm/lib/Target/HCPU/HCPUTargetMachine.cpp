//===-- HCPUTargetMachine.cpp - Define TargetMachine for HCPU -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implements the info about HCPU target spec.
//
//===----------------------------------------------------------------------===//

#include "HCPUTargetMachine.h"
#include "HCPU.h"

#include "HCPUInstrInfo.h"
#include "HCPUMachineFunction.h"
#include "HCPUSubtarget.h"
#include "HCPUTargetObjectFile.h"
#include "MCTargetDesc/HCPUABIInfo.h"
#include "HCPUSEISelDAGToDAG.h"
#include "TargetInfo/HCPUTargetInfo.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <memory>

using namespace llvm;

#define DEBUG_TYPE "hcpu"

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeHCPUTarget() {
  RegisterTargetMachine<HCPUelTargetMachine> X(getTheHCPUTarget());

  PassRegistry *PR = PassRegistry::getPassRegistry();
  initializeHCPUDAGToDAGISelLegacyPass(*PR);
}

static std::string computeDataLayout(const Triple &TT, StringRef CPU,
                                     const TargetOptions &Options) {
  std::string Ret = "";
  Ret += "e";

  // Ret += "E";

  Ret += "-m:m";

  Ret += "-p:32:32";

  Ret += "-i8:8:32-i16:16:32-i64:64";

  Ret += "-n32-S64";

  return Ret;
}

static Reloc::Model getEffectiveRelocModel(bool JIT,
                                           std::optional<Reloc::Model> RM) {
  if (!RM.has_value() || JIT)
    return Reloc::Static;
  return *RM;
}

// DataLayout --> Big-endian, 32-bit pointer/ABI/alignment
// The stack is always 8 byte aligned
// On function prologue, the stack is created by decrementing
// its pointer. Once decremented, all references are done with positive
// offset from the stack/frame pointer, using StackGrowsUp enables
// an easier handling.
// Using CodeModel::Large enables different CALL behavior.
HCPUTargetMachine::HCPUTargetMachine(const Target &T, const Triple &TT,
                                     StringRef CPU, StringRef FS,
                                     const TargetOptions &Options,
                                     std::optional<Reloc::Model> RM,
                                     std::optional<CodeModel::Model> CM,
                                     CodeGenOptLevel OL, bool JIT,
                                     bool isLittle)
    : LLVMTargetMachine(T, computeDataLayout(TT, CPU, Options), TT, CPU, FS,
                        Options, getEffectiveRelocModel(JIT, RM),
                        getEffectiveCodeModel(CM, CodeModel::Small), OL),
      isLittle(isLittle), TLOF(std::make_unique<HCPUTargetObjectFile>()),
      ABI(HCPUABIInfo::computeTargetABI()),
      DefaultSubtarget(TT, CPU, FS, isLittle, *this) {
  // initAsmInfo will display features by llc -march=HCPU -mcpu=help on 3.7
  // but not on 3.6
  initAsmInfo();
}

HCPUTargetMachine::~HCPUTargetMachine() {}

void HCPUelTargetMachine::anchor() {}

HCPUelTargetMachine::HCPUelTargetMachine(const Target &T, const Triple &TT,
                                         StringRef CPU, StringRef FS,
                                         const TargetOptions &Options,
                                         std::optional<Reloc::Model> RM,
                                         std::optional<CodeModel::Model> CM,
                                         CodeGenOptLevel OL, bool JIT)
    : HCPUTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL, JIT, true) {}

const HCPUSubtarget *
HCPUTargetMachine::getSubtargetImpl(const Function &F) const {
  std::string CPU = TargetCPU;
  std::string FS = TargetFS;

  auto &I = SubtargetMap[CPU + FS];
  if (!I) {
    // This needs to be done before we create a new subtarget since any
    // creation will depend on the TM and the code generation flags on the
    // function that reside in TargetOptions.
    resetTargetOptions(F);
    I = std::make_unique<HCPUSubtarget>(TargetTriple, CPU, FS, isLittle, *this);
  }
  return I.get();
}

namespace {
//@HCPUPassConfig
/// HCPU Code Generator Pass Configuration Options.
class HCPUPassConfig : public TargetPassConfig {
public:
  HCPUPassConfig(HCPUTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  HCPUTargetMachine &getHCPUTargetMachine() const {
    return getTM<HCPUTargetMachine>();
  }

  const HCPUSubtarget &getHCPUSubtarget() const {
    return *getHCPUTargetMachine().getSubtargetImpl();
  }

  bool addInstSelector() override;
};
} // namespace

TargetPassConfig *HCPUTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new HCPUPassConfig(*this, PM);
}

// Install an instruction selector pass using
// the ISelDag to gen HCPU code.
bool HCPUPassConfig::addInstSelector() {
  addPass(createHCPUSEISelDag(getHCPUTargetMachine(), getOptLevel()));
  return false;
}

MachineFunctionInfo *HCPUTargetMachine::createMachineFunctionInfo(
    BumpPtrAllocator &Allocator, const Function &F,
    const TargetSubtargetInfo *STI) const {
  return HCPUFunctionInfo::create<HCPUFunctionInfo>(Allocator, F, STI);
}
