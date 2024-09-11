add_llvm_component_library(LLVMHCPUAsmPrinter
  HCPUInstPrinter.cpp

  LINK_COMPONENTS
  Support

  ADD_TO_COMPONENT
  HCPU
)
