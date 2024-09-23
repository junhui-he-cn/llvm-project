//===-- HCPUAsmPrinter.h - HCPU LLVM Assembly Printer ----------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// HCPU Assembly printer class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_HCPUASMPRINTER_H
#define LLVM_LIB_TARGET_HCPU_HCPUASMPRINTER_H

#include "HCPUMachineFunction.h"
#include "HCPUMCInstLower.h"
#include "HCPUSubtarget.h"
#include "HCPUTargetMachine.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class MCStreamer;
class MachineInstr;
class MachineBasicBlock;
class Module;
class raw_ostream;

class LLVM_LIBRARY_VISIBILITY HCPUAsmPrinter : public AsmPrinter {

  void EmitInstrWithMacroNoAT(const MachineInstr *MI);

private:

  // lowerOperand - Convert a MachineOperand into the equivalent MCOperand.
  bool lowerOperand(const MachineOperand &MO, MCOperand &MCOp);

public:

  const HCPUSubtarget *Subtarget;
  const HCPUFunctionInfo *HCPUFI;
  HCPUMCInstLower MCInstLowering;

  explicit HCPUAsmPrinter(TargetMachine &TM,
                          std::unique_ptr<MCStreamer> Streamer)
    : AsmPrinter(TM, std::move(Streamer)), 
      MCInstLowering(*this) {
    Subtarget = static_cast<HCPUTargetMachine &>(TM).getSubtargetImpl();
  }

  StringRef getPassName() const override {
    return "HCPU Assembly Printer";
  }

  virtual bool runOnMachineFunction(MachineFunction &MF) override;

//- emitInstruction() must exists or will have run time error.
  void emitInstruction(const MachineInstr *MI) override;
  void printSavedRegsBitmask(raw_ostream &O);
  void printHex32(unsigned int Value, raw_ostream &O);
  void emitFrameDirective();
  const char *getCurrentABIString() const;
  void emitFunctionEntryLabel() override;
  void emitFunctionBodyStart() override;
  void emitFunctionBodyEnd() override;
  void emitStartOfAsmFile(Module &M) override;
  void PrintDebugValueComment(const MachineInstr *MI, raw_ostream &OS);
  bool isLongBranchPseudo(int Opcode) const;

    // tblgen'erated function.
  bool lowerPseudoInstExpansion(const MachineInstr *MI, MCInst &Inst);
};
}

#endif