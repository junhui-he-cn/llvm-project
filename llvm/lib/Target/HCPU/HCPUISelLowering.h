//===-- HCPUISelLowering.h - HCPU DAG Lowering Interface --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that HCPU uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_HCPUISELLOWERING_H
#define LLVM_LIB_TARGET_HCPU_HCPUISELLOWERING_H

#include "HCPU.h"
#include "MCTargetDesc/HCPUABIInfo.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetCallingConv.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/IR/Function.h"
#include <deque>

namespace llvm {
namespace HCPUISD {
enum NodeType {
  // Start the numbering from where ISD NodeType finishes.
  FIRST_NUMBER = ISD::BUILTIN_OP_END,
  // Jump and link (call)
  JmpLink,
  TailCall,
  // Get the Higher 16 bits from a 32-bit immediate
  // No relation with Cpu0 Hi register
  Hi,
  // Get the Lower 16 bits from a 32-bit immediate
  // No relation with Cpu0 Lo register
  Lo,
  // Handle gp_rel (small data/bss sections) relocation.
  GPRel,
  ThreadPointer,
  Ret,
  EH_RETURN,
  DivRem,
  DivRemU,
  Wrapper,
  DynAlloc,
  Sync
};
}

class HCPUFunctionInfo;
class HCPUSubtarget;

class HCPUTargetLowering : public TargetLowering {
public:
  explicit HCPUTargetLowering(const HCPUTargetMachine &TM,
                              const HCPUSubtarget &STI);

  static const HCPUTargetLowering *create(const HCPUTargetMachine &TM,
                                          const HCPUSubtarget &STI);

  const char *getTargetNodeName(unsigned Opcode) const override;

protected:
  /// ByValArgInfo - Byval argument information.
  struct ByValArgInfo {
    unsigned FirstIdx; // Index of the first register used.
    unsigned NumRegs;  // Number of registers used for this argument.
    unsigned Address;  // Offset of the stack area used to pass this argument.

    ByValArgInfo() : FirstIdx(0), NumRegs(0), Address(0) {}
  };

  const HCPUSubtarget &Subtarget;
  const HCPUABIInfo &ABI;

private:
  SDValue LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const;

  //- must be exist even without function all
  SDValue LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv,
                               bool isVarArg,
                               const SmallVectorImpl<ISD::InputArg> &Ins,
                               const SDLoc &dl, SelectionDAG &DAG,
                               SmallVectorImpl<SDValue> &InVals) const override;

  SDValue LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
                      const SmallVectorImpl<ISD::OutputArg> &Outs,
                      const SmallVectorImpl<SDValue> &OutVals, const SDLoc &dl,
                      SelectionDAG &DAG) const override;
};
const HCPUTargetLowering *
createHCPUSETargetLowering(const HCPUTargetMachine &TM,
                           const HCPUSubtarget &STI);
} // namespace llvm

#endif