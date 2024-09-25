add_llvm_component_library(LLVMHCPUAsmParser
  HCPUAsmParser.cpp

  LINK_COMPONENTS
  MC
  MCParser
  HCPUDesc
  HCPUInfo
  Support

  ADD_TO_COMPONENT
  HCPU
)