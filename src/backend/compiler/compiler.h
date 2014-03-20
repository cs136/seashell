/**
 * Seashell's LLVM and Clang interface.
 * Copyright (C) 2013-2014 The Seashell Maintainers.
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

#ifndef __COMPILER_H__
#define __COMPILER_H__

#include <string>
#include <vector>

#include <clang/Basic/Version.h>
#include <clang/Basic/Diagnostic.h>
#include <clang/Basic/DiagnosticOptions.h>
#include <clang/Basic/FileManager.h>
#include <clang/Basic/LLVM.h>
#include <clang/Basic/SourceManager.h>
#include <clang/CodeGen/CodeGenAction.h>
#include <clang/Driver/Compilation.h>
#include <clang/Driver/Driver.h>
#include <clang/Driver/Options.h>
#include <clang/Driver/Tool.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/CompilerInvocation.h>
#include <clang/Frontend/FrontendDiagnostic.h>
#include <clang/Frontend/TextDiagnostic.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <clang/Frontend/Utils.h>
#include <clang/FrontendTool/Utils.h>
#include <clang/Lex/Lexer.h>
#include <llvm/ADT/IntrusiveRefCntPtr.h>
#include <llvm/ADT/OwningPtr.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/CodeGen/CommandFlags.h>
#include <llvm/CodeGen/LinkAllAsmWriterComponents.h>
#include <llvm/CodeGen/LinkAllCodegenComponents.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/MC/SubtargetFeature.h>
#include <llvm/Option/ArgList.h>
#include <llvm/Pass.h>
#include <llvm/PassManager.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/PluginLoader.h>
#include <llvm/Support/PrettyStackTrace.h>
#include <llvm/Support/Signals.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/ToolOutputFile.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/Target/TargetLibraryInfo.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Linker.h>
#include <llvm/DebugInfo.h>
#include <llvm/DIBuilder.h>
#include <llvm/Transforms/Utils/Cloning.h>
/** Data structure for compiler diagnostic messages.
 * Opaque to Racket - C accessor functions described below.
 */
struct seashell_diag {
  public:
    seashell_diag(bool e, std::string f, std::string m, int l = 0, int c = 0)
      : error(e), file(f), mesg(m), line(l), col(c), loc_known(true) { }
  public:
    /** Diagnostic location information: */

    /** Is error? */
    bool error;

    /** File, message */
    std::string file, mesg;

    /** Line and column. */
    int line, col;

    /** Location known? */
    bool loc_known;
};

/** Seashell's compiler data structure.
 * Opaque to Racket - make sure to pass a cleanup function
 * to the FFI so garbage collection works properly.
 */
struct seashell_compiler {
  /** Control flags to the compiler.*/
  std::vector<std::string> compiler_flags;
  std::vector<std::string> source_paths;

  /** Module compilation messages. */
  std::vector<std::vector<seashell_diag>> module_messages;

  /** Linking messages. */
  std::string linker_messages;

  /** Outputs. */
  llvm::LLVMContext context;
  llvm::Module module;
  std::vector<char> output_object;

  /** Default constructor. */
  seashell_compiler() :
    module("seashell-compiler-output", context) {}
};

extern "C" const char* seashell_clang_version();
extern "C" struct seashell_compiler* seashell_compiler_make (void);
extern "C" void seashell_compiler_free (struct seashell_compiler* compiler);
extern "C" void seashell_compiler_add_file (struct seashell_compiler* compiler, const char* file);
extern "C" void seashell_compiler_clear_files (struct seashell_compiler* compiler);
extern "C" void seashell_compiler_add_compile_flag (struct seashell_compiler* compiler, const char* flag);
extern "C" void seashell_compiler_clear_compile_flags (struct seashell_compiler* compiler);
extern "C" const char * seashell_compiler_get_linker_messages(struct seashell_compiler* compiler);
extern "C" int seashell_compiler_get_diagnostic_count (struct seashell_compiler* compiler, int n);
extern "C" int seashell_compiler_get_diagnostic_line (struct seashell_compiler* compiler, int n, int k);
extern "C" int seashell_compiler_get_diagnostic_column (struct seashell_compiler* compiler, int n, int k);
extern "C" bool seashell_compiler_get_diagnostic_error (struct seashell_compiler* compiler, int n, int k);
extern "C" const char * seashell_compiler_get_diagnostic_file (struct seashell_compiler* compiler, int n, int k);
extern "C" const char * seashell_compiler_get_diagnostic_message (struct seashell_compiler* compiler, int n, int k);
extern "C" int seashell_compiler_run (struct seashell_compiler* compiler);
extern "C" void * seashell_compiler_get_object (struct seashell_compiler* compiler, int * length);
extern "C" const char* seashell_compiler_object_arch (struct seashell_compiler* compiler);
extern "C" const char* seashell_compiler_object_os (struct seashell_compiler* compiler);
#endif
