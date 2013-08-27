/* Seashell (C) 2012 Jenny Wong, Marc Burns. */

#include "compiler.h"

#include <clang/Driver/Driver.h>
#include <clang/Driver/Compilation.h>
#include <clang/Basic/DiagnosticOptions.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/Program.h>
#include <llvm/Support/raw_ostream.h>
#include <vector>

//int compile_one_file(const char* fileName, const char* binaryDestination="/tmp/a.out") {
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

int main() {
    const char* fname = "test.c";
    const char* dest = "/tmp/a.out";
    compile_one_file(fname, dest);
    return 0;
}
