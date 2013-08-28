/**
 * Seashell's LLVM and Clang interface.
 * Copyright (C) 2013 The Seashell Maintainers.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <clang/Driver/Driver.h>
#include <clang/Driver/Compilation.h>
#include <clang/Driver/Options.h>
#include <clang/Driver/Tool.h>
#include <clang/Basic/DiagnosticOptions.h>
#include <clang/Frontend/Utils.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <clang/Frontend/FrontendDiagnostic.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/CompilerInvocation.h>
#include <clang/FrontendTool/Utils.h>
#include <clang/CodeGen/CodeGenAction.h>
#include <llvm/Support/Host.h>
#include <llvm/Option/ArgList.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/TargetSelect.h>

#include <vector>
#include <string>
#include <iostream>

using namespace clang;
using namespace llvm::opt;

/** Seashell's compiler data structure.
 * Opaque to Racket - make sure to pass a cleanup function
 * to the FFI so garbage collection works properly.
 */
struct seashell_compiler {
  std::vector<std::string> inputs;
  std::string output;
};

/**
 * make_seashell_compiler (void)
 * Creates a new instance of the Seashell compiler.
 *
 * Returns:
 *  A new instance.
 *
 * Notes:
 *  It might be worthwhile to assign free_seashell_compiler as
 *  the cleanup function for garbage collection in the Racket FFI.
 */
extern "C" struct seashell_compiler* seashell_compiler_make (void) {
  return new seashell_compiler;
}

/**
 * free_seashell_compiler (struct seashell_compiler* compiler)
 * Deletes an instance of the Seashell compiler.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 */
extern "C" void seashell_compiler_free (struct seashell_compiler* compiler) {
  delete compiler;
}

/**
 * add_file_to_compiler (struct seashell_compiler* compiler, const char* file)
 * Adds a file to be compiled.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  file - File to add.
 */
extern "C" void seashell_compiler_add_file (struct seashell_compiler* compiler, const char* file) {
  compiler->inputs.push_back(file);
}

/**
 * set_compiler_output (struct seashell_compiler* compiler, const char* file)
 * Sets the compiler's output file.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  file - File to add.
 */
void seashell_compiler_set_output (struct seashell_compiler* compiler, const char* file) {
  compiler->output = file;
}

/**
 * seashell_compiler_run (struct seashell_compiler* compiler)
 * Runs the Seashell compiler instance.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  file - File to add.
 *
 * Returns
 *  0 if everything went OK, nonzero otherwise.
 */
int seashell_compiler_run (struct seashell_compiler* compiler) {
}

int compile_one_file(const char* fileName, const char* binaryDestination) {
    SmallVector<const char *, 16> args;
    args.push_back("-I/usr/include");
    args.push_back("-Wall");
    args.push_back(fileName);
    args.push_back("-o");
    args.push_back(binaryDestination);

    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmPrinters();
    llvm::InitializeAllAsmParsers();

    clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> diag_opts(new clang::DiagnosticOptions());
    clang::TextDiagnosticPrinter * diag_client = new clang::TextDiagnosticPrinter(llvm::errs(), &*diag_opts);

    clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> diag_ID(new clang::DiagnosticIDs());
    clang::DiagnosticsEngine Diags(diag_ID, &*diag_opts, diag_client);

    llvm::OwningPtr<clang::CompilerInvocation> CI(new clang::CompilerInvocation);

    bool Success = clang::CompilerInvocation::CreateFromArgs(*CI, &args[0], &args[0] + args.size(), Diags);
    if(!Success) {
      std::cerr << "CreateFromArgs() failed\n";
      return 1;
    }

    clang::CompilerInstance Clang;
    Clang.setInvocation(CI.take());
    Clang.createDiagnostics();

    if(!Clang.hasDiagnostics()) {
      std::cerr << "createDiagnostics() failed\n";
      return 1;
    }

    llvm::OwningPtr<clang::CodeGenAction> Act(new clang::EmitAssemblyAction());
    Success = Clang.ExecuteAction(*Act);
    if(!Success) {
      std::cerr << "Could not execute EmitAssemblyAction.\n";
      return 1;
    }

    llvm::Module * mod = Act->takeModule();

    mod->print(llvm::errs(), NULL);

    return !Success;
}

int derp() {
  if(compile_one_file("foo.c", "foo.out")) {
    std::cerr << "not successful\n";
  } else {
    std::cerr << "successful\n";
  }
}

