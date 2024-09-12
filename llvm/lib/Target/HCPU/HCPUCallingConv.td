//===-- HCPUCallingConv.td - Calling Conventions for HCPU --*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This describes the calling conventions for HCPU architecture.
//===----------------------------------------------------------------------===//

/// CCIfSubtarget - Match if the current subtarget has a feature F.

class CCIfSubtarget<string F, CCAction A> :
  CCIf<!strconcat("State.getTarget().getSubtarget<HCPUSubtarget>().", F), A>;

def CSR_O32 : CalleeSavedRegs<(add LR, FP, (sequence "S%u", 1, 0))>;

def RetCC_HCPUEABI : CallingConv<[
  // i32 are returned in registers V0, V1, A0, A1
  CCIfType<[i32], CCAssignToReg<[V0, V1, A0, A1]>>
]>;

def RetCC_HCPU : CallingConv<[
  CCDelegateTo<RetCC_HCPUEABI>
]>; 