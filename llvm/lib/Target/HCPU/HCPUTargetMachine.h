//===-- HCPUTargetMachine.h - Define TargetMachine for HCPU -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the HCPU specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_HCPUTARGETMACHINE_H
#define LLVM_LIB_TARGET_HCPU_HCPUTARGETMACHINE_H

#include "HCPU.h"
#include "HCPUSubtarget.h"
#include "MCTargetDesc/HCPUABIInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/Target/TargetMachine.h"
#include <memory>

namespace llvm {
class formatted_raw_ostream;
class HCPURegisterInfo;
class HCPUSubtarget;

class HCPUTargetMachine : public LLVMTargetMachine {
  bool isLittle;
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  HCPUABIInfo ABI;
  HCPUSubtarget DefaultSubtarget;

  mutable StringMap<std::unique_ptr<HCPUSubtarget>> SubtargetMap;

public:
  HCPUTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                    StringRef FS, const TargetOptions &Options,
                    std::optional<Reloc::Model> RM,
                    std::optional<CodeModel::Model> CM, CodeGenOptLevel OL,
                    bool JIT, bool isLittle);

  ~HCPUTargetMachine() override;

  const HCPUSubtarget *getSubtargetImpl() const { return &DefaultSubtarget; }

  const HCPUSubtarget *getSubtargetImpl(const Function &F) const override;

  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;

  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }

  bool isLittleEndian() const { return isLittle; }

  const HCPUABIInfo &getABI() const { return ABI; }

  MachineFunctionInfo *
  createMachineFunctionInfo(BumpPtrAllocator &Allocator, const Function &F,
                            const TargetSubtargetInfo *STI) const override;

};

/// HCPUelTargetMachine - HCPU32 little endian target machine.
///
class HCPUelTargetMachine : public HCPUTargetMachine {
  virtual void anchor();

public:
  HCPUelTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                      StringRef FS, const TargetOptions &Options,
                      std::optional<Reloc::Model> RM,
                      std::optional<CodeModel::Model> CM, CodeGenOptLevel OL,
                      bool JIT);
};

} // namespace llvm

#endif
