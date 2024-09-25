add_llvm_component_library(LLVMHCPUDisassembler
  HCPUDisassembler.cpp

  LINK_COMPONENTS
  MCDisassembler
  HCPUInfo
  Support

  ADD_TO_COMPONENT
  HCPU
  )