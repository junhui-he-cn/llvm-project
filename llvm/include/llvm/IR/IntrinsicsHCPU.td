//===- IntrinsicsCpu0.td - Defines HCPU intrinsics ---------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines all of the HCPU-specific intrinsics.
//
//===----------------------------------------------------------------------===//

// __builtin_hcpu_gcd defined in
def int_hcpu_gcd : ClangBuiltin<"__builtin_hcpu_gcd">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty, llvm_i32_ty],
  [Commutative, IntrNoMem]>;
