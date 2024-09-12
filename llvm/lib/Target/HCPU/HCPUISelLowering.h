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
  // No relation with HCPU Hi register
  Hi,
  // Get the Lower 16 bits from a 32-bit immediate
  // No relation with HCPU Lo register
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

  /// HCPUCC - This class provides methods used to analyze formal and call
  /// arguments and inquire about calling convention information.
  class HCPUCC {
  public:
    enum SpecialCallingConvType { NoSpecialCallingConv };

    HCPUCC(CallingConv::ID CallConv, bool IsO32, CCState &Info,
           SpecialCallingConvType SpecialCallingConv = NoSpecialCallingConv);

    void analyzeCallResult(const SmallVectorImpl<ISD::InputArg> &Ins,
                           bool IsSoftFloat, const SDNode *CallNode,
                           const Type *RetTy) const;

    void analyzeReturn(const SmallVectorImpl<ISD::OutputArg> &Outs,
                       bool IsSoftFloat, const Type *RetTy) const;

    const CCState &getCCInfo() const { return CCInfo; }

    /// hasByValArg - Returns true if function has byval arguments.
    bool hasByValArg() const { return !ByValArgs.empty(); }

    /// reservedArgArea - The size of the area the caller reserves for
    /// register arguments. This is 16-byte if ABI is O32.
    unsigned reservedArgArea() const;

    typedef SmallVectorImpl<ByValArgInfo>::const_iterator byval_iterator;
    byval_iterator byval_begin() const { return ByValArgs.begin(); }
    byval_iterator byval_end() const { return ByValArgs.end(); }

  private:
    /// Return the type of the register which is used to pass an argument or
    /// return a value. This function returns f64 if the argument is an i64
    /// value which has been generated as a result of softening an f128 value.
    /// Otherwise, it just returns VT.
    MVT getRegVT(MVT VT, bool IsSoftFloat) const;

    template <typename Ty>
    void analyzeReturn(const SmallVectorImpl<Ty> &RetVals, bool IsSoftFloat,
                       const SDNode *CallNode, const Type *RetTy) const;

    CCState &CCInfo;
    CallingConv::ID CallConv;
    bool IsO32;
    SmallVector<ByValArgInfo, 2> ByValArgs;
  };

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