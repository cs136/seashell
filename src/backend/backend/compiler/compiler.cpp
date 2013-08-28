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
#include <clang/Basic/DiagnosticOptions.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/Program.h>
#include <llvm/Support/raw_ostream.h>
#include <vector>

/** Seashell's compiler data structure.
 * Opaque to Racket - make sure to pass a cleanup function
 * to the FFI so garbage collection works properly.
 */
struct seashell_compiler {
  vector<string> inputs;
  string output;
}

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
extern "C" void seashell_compiler_add_file (struct seashell_compiler* compiler, cont char* file) {
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
    // "clang -Wall foo.c"
    //llvm::sys::Path clangExecutable = llvm::sys::Program::FindProgramByName("clang");
    const char* clangPath = "/scratch/m4burns/llvm/bin/clang"; //clangExecutable.c_str();
    std::vector<const char*> args;
    args.push_back(clangPath);
    args.push_back("-Wall");
    args.push_back(fileName);

    // temporary. Eventually want to use something custom instead of TextDiagnosticPrinter
    clang::DiagnosticOptions * diag_opts = new clang::DiagnosticOptions();
    clang::TextDiagnosticPrinter *diag_client = new clang::TextDiagnosticPrinter(llvm::errs(), diag_opts);
    //

    clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> diag_ID(new clang::DiagnosticIDs());
    clang::DiagnosticsEngine *Diags = new clang::DiagnosticsEngine(diag_ID, diag_opts, diag_client);

    // now that the diagnostics engine is constructed, compile.

    // TODO: possibly investigate
    // from clang/Frontend/Utils.h:
    // http://clang.llvm.org/doxygen/CreateInvocationFromCommandLine_8cpp_source.html
    //clang::CompilerInvocation *CI = clang::createInvocationFromCommandLine(args, Diags);
    clang::driver::Driver TheDriver (clangPath, llvm::sys::getDefaultTargetTriple(), binaryDestination, *Diags);

    clang::OwningPtr<clang::driver::Compilation> C(TheDriver.BuildCompilation(args));
    int Res = 0;
    llvm::SmallVector<std::pair<int, const clang::driver::Command *>, 32 > Commands;

    if (C) {
        Res = TheDriver.ExecuteCompilation(*C, Commands);
    }

    if (Res < 0) {
        for(int i = 0; i < Commands.size(); i++) {
          TheDriver.generateCompilationDiagnostics(*C, Commands[i].second);
        }
    }
    return Res;
}

