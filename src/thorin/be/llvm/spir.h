#ifndef THORIN_BE_LLVM_SPIR_H
#define THORIN_BE_LLVM_SPIR_H

#include "thorin/be/llvm/llvm.h"

namespace thorin {

class SPIRCodeGen : public CodeGen {
public:
    SPIRCodeGen(World& world);

    virtual llvm::Function* emit_function_decl(std::string&, Lambda*);
};

}

#endif