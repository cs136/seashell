/**
 * Seashell's LLVM and Clang interface.
 * Copyright (C) 2013-2015 The Seashell Maintainers.
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
#include "compiler.h"
#include <string>
#include <vector>
#include <set>
#include <tuple>
#include <memory>
#include <sstream>
#include <iostream>
#include <algorithm>

#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef __EMSCRIPTEN__
#include <seashell-config.h>
#endif

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
#include <clang/StaticAnalyzer/Frontend/FrontendActions.h>
#include <clang/Lex/Lexer.h>
#include <clang/Lex/Preprocessor.h>
#include <llvm/ADT/IntrusiveRefCntPtr.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/Triple.h>
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
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Bitcode/ReaderWriter.h>

#if CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR >= 6
#include <llvm/IR/DiagnosticPrinter.h>
#include <llvm/Target/TargetSubtargetInfo.h>
#include <llvm/Linker/Linker.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/DIBuilder.h>
#elif CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR == 5
#include <llvm/Linker/Linker.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/DIBuilder.h>
#elif CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR == 4
#include <llvm/Linker.h>
#include <llvm/DebugInfo.h>
#include <llvm/DIBuilder.h>
#else
#error "Unsupported version of clang."
#endif

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
bool operator <(const seashell_diag& d1, const seashell_diag& d2) {
  return std::tie(d1.error, d1.file, d1.mesg, d1.line, d1.col, d1.loc_known) <
         std::tie(d2.error, d2.file, d2.mesg, d2.line, d2.col, d2.loc_known);
}

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

  seashell_compiler();
};

struct seashell_preprocessor {
  /** Flags to the compiler */
  std::vector<std::string> compiler_flags;

  /** Preprocessor messages */
  std::vector<seashell_diag> messages;

  /** Main file to begin preprocessing from */
  std::string main_file;

  /** Produced set of sources */
  std::set<std::string> sources;
};

seashell_compiler::seashell_compiler() :
  context(),
  module("seashell-compiler-output", context) {
}

#ifdef __EMSCRIPTEN__
extern "C" __attribute__((weak)) void llvm_assume(bool c) {
  return;
}
#endif

/**
 * seashell_llvm_setup (void)
 * Performs necessary LLVM setup.
 */
static void seashell_llvm_setup() {
#ifndef __EMSCRIPTEN__
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();
    InitializeNativeTargetDisassembler();

    PassRegistry *Registry = PassRegistry::getPassRegistry();
    initializeCore(*Registry);
    initializeCodeGen(*Registry);
    initializeLoopStrengthReducePass(*Registry);
    initializeLowerIntrinsicsPass(*Registry);
    initializeUnreachableBlockElimPass(*Registry);
#else
    // Compiler bug in emcc (as of 7 April/15)
    llvm_assume(true);
#endif
}

/**
 * seashell_llvm_cleanup (void)
 * Performs necessary LLVM cleanup.
 */
static void seashell_llvm_cleanup() {
  llvm_shutdown();
}

/** Helper class for making sure LLVM
 *  gets started up and cleaned up properly.
 */
class LLVMSetup {
  public:
    LLVMSetup() {seashell_llvm_setup();}
    ~LLVMSetup() {seashell_llvm_cleanup();}
} LLVMSetupObject;

/**
 * seashell_clang_version (void)
 * Gets the Clang version string.
 */
#ifdef __EMSCRIPTEN__
std::string seashell_clang_version() {
#else
extern "C" const char* seashell_clang_version() {
#endif
  return CLANG_VERSION_STRING;
}

/**
 * seashell_compiler_make (void)
 * Creates a new instance of the Seashell compiler.
 *
 * Returns:
 *  A new instance.
 *
 * Notes:
 *  It might be worthwhile to assign seashell_compiler_free as
 *  the cleanup function for garbage collection in the Racket FFI.
 */
extern "C" struct seashell_compiler* seashell_compiler_make (void) {
  struct seashell_compiler* r = new seashell_compiler;
#if defined(__EMSCRIPTEN__) && !defined(NDEBUG)
  printf("[compiler] Allocating new compiler object at %p\n", r);
#endif
  return r;
}

/**
 * seashell_compiler_free (struct seashell_compiler* compiler)
 * Deletes an instance of the Seashell compiler.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 */
extern "C" void seashell_compiler_free (struct seashell_compiler* compiler) {
#if defined(__EMSCRIPTEN__) && !defined(NDEBUG)
  printf("[compiler] De-Allocating new compiler object at %p\n", compiler);
#endif
  delete compiler;
}

/**
 * seashell_compiler_add_file (struct seashell_compiler* compiler, const char* file)
 * Adds a file to be compiled.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  file - Pathname of file to add.
 */
#ifndef __EMSCRIPTEN__
extern "C" void seashell_compiler_add_file (struct seashell_compiler* compiler, const char* file) {
#else
void seashell_compiler_add_file (struct seashell_compiler* compiler, std::string file) {
#endif
  compiler->source_paths.push_back(file);
}

/**
 * seashell_compiler_clear_files (struct seashell_compiler* compiler)
 * Clears the compiler's input file list.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 */
extern "C" void seashell_compiler_clear_files (struct seashell_compiler* compiler) {
  compiler->source_paths.clear();
}

/**
 * seashell_compiler_add_compile_flag (struct seashell_compiler* compiler, const char* flag)
 * Adds a compilation flag to the compiler.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  flag - Compilation flag to add.
 */
#ifndef __EMSCRIPTEN__
extern "C" void seashell_compiler_add_compile_flag (struct seashell_compiler* compiler, const char* flag) {
#else
void seashell_compiler_add_compile_flag (struct seashell_compiler* compiler, std::string flag) {
#endif
  compiler->compiler_flags.push_back(flag);
}

/**
 * seashell_compiler_clear_compile_flags (struct seashell_compiler* compiler)
 * Clears the compiler's compilation flag list.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 */
extern "C" void seashell_compiler_clear_compile_flags (struct seashell_compiler* compiler) {
  compiler->compiler_flags.clear();
}

/**
 * seashell_compiler_get_linker_messages (struct seashell_compiler* compiler)
 * Gets any errors or warnings related to the intermediate linking stage, as a string.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *
 * Notes:
 *  The string returned is only valid while the compiler exists and until the next call
 *  of seashell_compiler_run.
 */
#ifndef __EMSCRIPTEN__
extern "C" const char * seashell_compiler_get_linker_messages(struct seashell_compiler* compiler) {
#else
std::string seashell_compiler_get_linker_messages(struct seashell_compiler* compiler) {
#endif
  return compiler->linker_messages.c_str();
}

/**
 * seashell_compiler_get_diagnostic_count (struct seashell_compiler* compiler, int n)
 * Gets the number of compilation diagnostic messages available for the nth file.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  n - Index into currently added files list.
 */
extern "C" int seashell_compiler_get_diagnostic_count (struct seashell_compiler* compiler, int n) {
  if (compiler->module_messages.size() <= n) {
    return 0;
  } else {
    return compiler->module_messages.at(n).size();
  }
}

/**
 * seashell_compiler_get_diagnostic_line (struct seashell_compiler* compiler, int n, int k)
 * Gets the line number of the kth available diagnostic message for the nth file.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  n - Index into currently added files list.
 *  k - Index into file diagnostics list.
 */
extern "C" int seashell_compiler_get_diagnostic_line (struct seashell_compiler* compiler, int n, int k) {
  if (compiler->module_messages.size() <= n) {
    return 0;
  } else {
    if(compiler->module_messages.at(n).size() <= k) {
      return 0;
    } else {
      return compiler->module_messages.at(n).at(k).line;
    }
  }
}

/**
 * seashell_compiler_get_diagnostic_column (struct seashell_compiler* compiler, int n, int k)
 * Gets the column number of the kth available diagnostic message for the nth file.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  n - Index into currently added files list.
 *  k - Index into file diagnostics list.
 */
extern "C" int seashell_compiler_get_diagnostic_column (struct seashell_compiler* compiler, int n, int k) {
  if (compiler->module_messages.size() <= n) {
    return 0;
  } else {
    if(compiler->module_messages.at(n).size() <= k) {
      return 0;
    } else {
      return compiler->module_messages.at(n).at(k).col;
    }
  }
}

/**
 * seashell_compiler_get_diagnostic_error (struct seashell_compiler* compiler, int n, int k)
 * Gets if the kth available diagnostic message for the nth file is an error.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  n - Index into currently added files list.
 *  k - Index into file diagnostics list.
 */
extern "C" bool seashell_compiler_get_diagnostic_error (struct seashell_compiler* compiler, int n, int k) {
  if (compiler->module_messages.size() <= n) {
    return 0;
  } else {
    if(compiler->module_messages.at(n).size() <= k) {
      return 0;
    } else {
      return compiler->module_messages.at(n).at(k).error;
    }
  }
}

/**
 * seashell_compiler_get_diagnostic_file (struct seashell_compiler* compiler, int n, int k)
 * Gets the file name for the kth available diagnostic message for the nth file.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  n - Index into currently added files list.
 *  k - Index into file diagnostics list.
 */
#ifndef __EMSCRIPTEN__
extern "C" const char * seashell_compiler_get_diagnostic_file (struct seashell_compiler* compiler, int n, int k) {
#else
std::string seashell_compiler_get_diagnostic_file(struct seashell_compiler* compiler, int n, int k) {
#endif
  if (compiler->module_messages.size() <= n) {
    return NULL;
  } else {
    if(compiler->module_messages.at(n).size() <= k) {
      return NULL;
    } else {
      return compiler->module_messages.at(n).at(k).file.c_str();
    }
  }
}

/**
 * seashell_compiler_get_diagnostic_message (struct seashell_compiler* compiler, int n, int k)
 * Gets the message for the kth available diagnostic message for the nth file.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  n - Index into currently added files list.
 *  k - Index into file diagnostics list.
 *
 * Note:
 *  The string returned is only valid while the compiler instance exists and until the next call
 *  of seashell_compiler_run.
 */
#ifndef __EMSCRIPTEN__
extern "C" const char * seashell_compiler_get_diagnostic_message (struct seashell_compiler* compiler, int n, int k) {
#else
std::string seashell_compiler_get_diagnostic_message(struct seashell_compiler* compiler, int n, int k) {
#endif
  if (compiler->module_messages.size() <= n) {
    return NULL;
  } else {
    if(compiler->module_messages.at(n).size() <= k) {
      return NULL;
    } else {
      return compiler->module_messages.at(n).at(k).mesg.c_str();
    }
  }
}

static int compile_module (seashell_compiler* compiler,
    llvm::Module* module, const char* src_path);

static int final_link_step (seashell_compiler* compiler);

/**
 * seashell_compiler_run (struct seashell_compiler* compiler)
 * Runs the Seashell compiler instance.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *
 * Returns
 *  0 if everything went OK, nonzero otherwise.
 *
 * Notes:
 *  May output some additional error information to stderr.
 *  seashell_llvm_setup must be called before this function.
 */
extern "C" int seashell_compiler_run (struct seashell_compiler* compiler) {
    std::string errors;

    compiler->module_messages.clear();

    for (std::vector<std::string>::iterator path = compiler->source_paths.begin();
          path != compiler->source_paths.end();
          ++path)
    {
      compiler->module_messages.push_back(std::vector<seashell_diag>());
      if (compile_module(compiler, &compiler->module, path->c_str())) {
        return 1;
      }
    }

    final_link_step(compiler);

    compiler->linker_messages = "";
    return 0;
}

/**
 * seashell_compiler_get_object (struct seashell_compiler* compiler)
 * Returns a pointer to the resulting object, if any.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  length - Output argument that will contain the length of the executable.
 *
 * Returns
 *  A pointer to the resulting executable or NULL.
 */
#ifndef __EMSCRIPTEN__
extern "C" const char * seashell_compiler_get_object (struct seashell_compiler* compiler, int * length) {
  if (compiler->output_object.size() > 0) {
    *length = compiler->output_object.size();
    return (const char*)&compiler->output_object.at(0);
  } else {
    *length = 0;
    return NULL;
  }
#else
std::string seashell_compiler_get_object(struct seashell_compiler* compiler) {
  return std::string(compiler->output_object.begin(), compiler->output_object.end());
#endif
}

/**
 * seashell_compiler_object_arch (struct seashell_compiler* compiler)
 * Returns a string representing the resulting object's architecture, if any.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 * Returns:
 *  Architecture, or NULL.
 */
#ifndef __EMSCRIPTEN__
extern "C" const char* seashell_compiler_object_arch (struct seashell_compiler* compiler) {
#else
std::string seashell_compiler_object_arch(struct seashell_compiler* compiler) {
#endif
  /** Grab the triple, get the right code generator. */
  llvm::Triple TheTriple = llvm::Triple(compiler->module.getTargetTriple());
  if (TheTriple.getArch() == llvm::Triple::UnknownArch)
    return NULL;
  return llvm::Triple::getArchTypeName(TheTriple.getArch());
}

/**
 * seashell_compiler_object_os (struct seashell_compiler* compiler)
 * Returns a string representing the target's operating system.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 * Returns:
 *  OS name, or NULL.
 */
#ifndef __EMSCRIPTEN__
extern "C" const char* seashell_compiler_object_os (struct seashell_compiler* compiler) {
#else
std::string seashell_compiler_object_os (struct seashell_compiler* compiler) {
#endif
  /** Grab the triple, get the right code generator. */
  llvm::Triple TheTriple = llvm::Triple(compiler->module.getTargetTriple());
  if (TheTriple.getOS() == llvm::Triple::UnknownOS)
    return NULL;
  return llvm::Triple::getOSTypeName(TheTriple.getOS());
}

static void printDiagnosticOptions(raw_ostream &OS,
                                   clang::DiagnosticsEngine::Level Level,
                                   const clang::Diagnostic &Info,
                                   const clang::DiagnosticOptions &DiagOpts) {
  bool Started = false;
  if (DiagOpts.ShowOptionNames) {
    if (Info.getID() == clang::diag::fatal_too_many_errors) {
      OS << " [-ferror-limit=]";
      return;
    }

    if (Level == clang::DiagnosticsEngine::Error &&
        clang::DiagnosticIDs::isBuiltinWarningOrExtension(Info.getID()) &&
        !clang::DiagnosticIDs::isDefaultMappingAsError(Info.getID())) {
      OS << " [-Werror";
      Started = true;
    }

    StringRef Opt = clang::DiagnosticIDs::getWarningOptionForDiag(Info.getID());
    if (!Opt.empty()) {
      OS << (Started ? "," : " [") << "-W" << Opt;
      Started = true;
    }
  }

  if (DiagOpts.ShowCategories) {
    unsigned DiagCategory =
      clang::DiagnosticIDs::getCategoryNumberForDiag(Info.getID());
    if (DiagCategory) {
      OS << (Started ? "," : " [");
      Started = true;
      if (DiagOpts.ShowCategories == 1)
        OS << DiagCategory;
      else {
        assert(DiagOpts.ShowCategories == 2 && "Invalid ShowCategories value");
        OS << clang::DiagnosticIDs::getCategoryNameFromID(DiagCategory);
      }
    }
  }
  if (Started)
    OS << ']';
}

class SeashellDiagnosticClient : public clang::DiagnosticConsumer {
  IntrusiveRefCntPtr<clang::DiagnosticOptions> DiagOpts;

public:
  std::set<seashell_diag> messages;

  SeashellDiagnosticClient(clang::DiagnosticOptions * diags) : DiagOpts(diags) { }
  virtual ~SeashellDiagnosticClient() { }

  void BeginSourceFile(const clang::LangOptions &LO, const clang::Preprocessor *PP) { }
  void EndSourceFile() { }
  void HandleDiagnostic(clang::DiagnosticsEngine::Level Level, const clang::Diagnostic & Info) {
    llvm::SmallString<100> OutStr;
    Info.FormatDiagnostic(OutStr);
    llvm::raw_svector_ostream DiagMessageStream(OutStr);
    printDiagnosticOptions(DiagMessageStream, Level, Info, *DiagOpts);

    const clang::SourceManager* SM = Info.hasSourceManager() ? &Info.getSourceManager() : nullptr;
    const clang::SourceLocation Loc = Info.getLocation();
    bool error = (Level == clang::DiagnosticsEngine::Error) || (Level == clang::DiagnosticsEngine::Fatal);
#ifndef _NDEBUG
    fprintf(stderr, "Got diagnostic %s\n", OutStr.c_str());
#endif    
    if (SM) {
      clang::PresumedLoc PLoc = SM->getPresumedLoc(Loc);

      if (PLoc.isInvalid()) {
        clang::FileID FID = SM->getFileID(Loc);
        if( !FID.isInvalid()) {
          const clang::FileEntry * FE = SM->getFileEntryForID(FID);
          if(FE && FE->getName()) {
            messages.insert(seashell_diag(error, FE->getName(), OutStr.c_str()));
            return;
          } else {
            messages.insert(seashell_diag(error, "?", OutStr.c_str()));
            return;
          }
        } else {
          messages.insert(seashell_diag(error, "?", OutStr.c_str()));
          return;
        }
      } else {
        messages.insert(seashell_diag(error, PLoc.getFilename(), OutStr.c_str(),
                                      PLoc.getLine(), PLoc.getColumn()));
        return;
      }
    } else {
      messages.insert(seashell_diag(error, "?", OutStr.c_str()));
    }
  }
};

/**
 * final_link_step(struct seashell_compiler* compiler)
 *
 * Executes the final link step on the compiler state,
 * producing an ELF file that will be stored in memory.
 *
 * Arguments:
 *  compiler - Compiler instance.
 *
 * Returns:
 *  1 on error, 0 otherwise.
 */
static int final_link_step (struct seashell_compiler* compiler)
{
  Module* mod = &compiler->module;
  /** Compile to Object code if running natively. */
  std::string Error;


  /* Compile LLVM IR to architecture-specific assembly code. */
  SMDiagnostic Err;
  Triple TheTriple;

  /** Grab the triple, get the right code generator. */
  TheTriple = Triple(mod->getTargetTriple());
  if (TheTriple.getTriple().empty())
    TheTriple.setTriple(sys::getDefaultTargetTriple());

#ifndef __EMSCRIPTEN__
  const Target *TheTarget = TargetRegistry::lookupTarget(TheTriple.getTriple(), Error);
  if (!TheTarget) {
    compiler->linker_messages = "libseashell-clang: couldn't look up target: " + TheTriple.getTriple() + ".";
    return 1;
  }


  /** Code Generation Options - we generate code with standard options. */
  TargetOptions Options;

  /** Grab a copy of the target. */
  std::unique_ptr<TargetMachine>
    target(TheTarget->createTargetMachine(TheTriple.getTriple(),
                                          "generic", "", Options));
  if (!target.get()) {
    compiler->linker_messages = "libseashell-clang: couldn't get machine for target: " + TheTriple.getTriple() + ".";
    return 1;
  }
  TargetMachine &Target = *target.get();

  /** Set up the code generator. */
  PassManager PM;

  TargetLibraryInfo *TLI = new TargetLibraryInfo(TheTriple);
  PM.add(TLI);
  Target.addAnalysisPasses(PM);

  /** Drive the code generator. */
  std::string result;
  llvm::raw_string_ostream raw(result);
  llvm::formatted_raw_ostream adapt(raw);

#if CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR >= 6
  if (const DataLayout *TD = Target.getSubtargetImpl()->getDataLayout())
    mod->setDataLayout(TD);
  PM.add(new DataLayoutPass());
#elif CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR == 5
  if (const DataLayout *TD = Target.getDataLayout())
    mod->setDataLayout(TD);
  PM.add(new DataLayoutPass(mod));
#elif CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR == 4
  if (const DataLayout *TD = Target.getDataLayout())
    PM.add(new DataLayout(*TD));
  else
    PM.add(new DataLayout(mod));
#else
#error "Unsupported version of clang."
#endif

  if (Target.addPassesToEmitFile(PM, adapt, llvm::TargetMachine::CGFT_ObjectFile)) {
    compiler->linker_messages = "libseashell-clang: couldn't emit object code for target: " + TheTriple.getTriple() + ".";
    return 1;
  }

  PM.run(*mod);
  adapt.flush();
#else
  std::string result;
  llvm::raw_string_ostream raw(result);
  llvm::WriteBitcodeToFile(mod, raw);
#endif

  /** Final link step needs to happen with an invocation to cc.
   *  We'll do this in Racket.  Pass back the completed object file
   *  in memory.
   */
  raw.flush();
  compiler->output_object = std::vector<char>(raw.str().begin(), raw.str().end());

  return 0;
}

/**
 * CreateAndPopulateDiagOpts(...)
 * Creates a populated DiagnosticOpts object.
 *
 * see clang/tools/driver/driver.cpp
 */
clang::DiagnosticOptions * CreateAndPopulateDiagOpts(const char *const *start, const char *const *end) {
  auto *DiagOpts = new clang::DiagnosticOptions;
  std::unique_ptr<llvm::opt::OptTable> Opts(clang::driver::createDriverOptTable());
  unsigned MissingArgIndex, MissingArgCount;
  std::unique_ptr<llvm::opt::InputArgList> Args(Opts->ParseArgs(
        start, end, MissingArgIndex, MissingArgCount));
  // We ignore MissingArgCount and the return value of ParseDiagnosticArgs.
  // Any errors that would be diagnosed here will also be diagnosed later,
  // when the DiagnosticsEngine actually exists.
  (void) clang::ParseDiagnosticArgs(*DiagOpts, *Args);
  return DiagOpts; 
}

/**
 * compile_module(
 *  seashell_compiler* compiler,
 *  llvm::Module* target,
 *  const char* src_path)
 *
 * Compiles a given source file, links it into a module.
 *
 * Arguments:
 *  module - Module to link into.
 *  src_path - File to compile.
 *
 * Returns:
 *  1 on error, 0 otherwise.
 */
static int compile_module (seashell_compiler* compiler,
    llvm::Module* module, const char* src_path)
{
#ifndef NDEBUG
  fprintf(stderr, "[compiler] Compiling file: %s\n", src_path);
#endif
    std::string Error;
    bool Success;
    std::vector<const char*> args;
    size_t index;

    /** Look up the right compilation message handle. */
    for (index = 0; index < compiler->module_messages.size(); index++) {
      if (compiler->source_paths[index] == src_path)
        break;
    }
    if (index >= compiler->module_messages.size())
      return 1;
    std::vector<seashell_diag>& compile_messages = compiler->module_messages[index];

    #define PUSH_DIAGNOSTIC(x) compile_messages.push_back(seashell_diag(true, src_path, (x)))
    /** Set up compilation arguments. */
    for(std::vector<std::string>::iterator p = compiler->compiler_flags.begin();
          p != compiler->compiler_flags.end();
          ++p)
    {
      args.push_back(p->c_str());
    }
    args.push_back(src_path);
    
    /** Parse Diagnostic Arguments */
    clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> diag_opts(CreateAndPopulateDiagOpts(&args[0], &args[0] + args.size()));

    /* Invoke clang to compile file to LLVM IR. */
    SeashellDiagnosticClient diag_client(&*diag_opts);

    clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> diag_ID(new clang::DiagnosticIDs());
    clang::DiagnosticsEngine CI_Diags(diag_ID, &*diag_opts, &diag_client, false);
    clang::FileManager CI_FM((clang::FileSystemOptions()));
    clang::SourceManager CI_SM(CI_Diags, CI_FM);

    clang::IntrusiveRefCntPtr<clang::CompilerInvocation> CI(new clang::CompilerInvocation);

    Success = clang::CompilerInvocation::CreateFromArgs(*CI, &args[0], &args[0] + args.size(), CI_Diags);
    if (!Success) {
      PUSH_DIAGNOSTIC("libseashell-clang: clang::CompilerInvocation::CreateFromArgs() failed.");
      std::copy(diag_client.messages.begin(), diag_client.messages.end(),
                  std::back_inserter(compile_messages));
      return 1;
    }

    clang::CompilerInstance Clang;
#if CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR >= 5
    Clang.setInvocation(CI.get());
#elif CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR == 4
    Clang.setInvocation(CI.getPtr());
#endif
    Clang.createDiagnostics(&diag_client, false);
    Clang.createFileManager();
    Clang.createSourceManager(Clang.getFileManager());

    if (!Clang.hasDiagnostics()) {
      PUSH_DIAGNOSTIC("libseashell-clang: clang::CompilerInstance::createDiagnostics() failed.");
      std::copy(diag_client.messages.begin(), diag_client.messages.end(),
                  std::back_inserter(compile_messages));
      return 1;
    }

    /** Add compiler-specific headers. */
#ifndef __EMSCRIPTEN__
    if (!IS_INSTALLED() && access (BUILD_DIR "/lib/llvm/lib/clang/" CLANG_VERSION_STRING "/include/", F_OK) != -1) {
      Clang.getHeaderSearchOpts().AddPath(BUILD_DIR "/lib/llvm/lib/clang/" CLANG_VERSION_STRING "/include/", clang::frontend::System, false, true);
    } else {
      Clang.getHeaderSearchOpts().AddPath(INSTALL_PREFIX "/lib/clang/" CLANG_VERSION_STRING "/include", clang::frontend::System, false, true);
    } 
    /** NOTE: this will have to change for different platforms */
#ifdef MULTIARCH_PLATFORM
    Clang.getHeaderSearchOpts().AddPath("/usr/include/" MULTIARCH_PLATFORM, clang::frontend::System, false, true);
#endif
    /** Set up the default (generic) headers */
    Clang.getHeaderSearchOpts().AddPath("/usr/include", clang::frontend::System, false, true);
#else
    Clang.getHeaderSearchOpts().AddPath("/clang-include/", clang::frontend::System, false, true);
    Clang.getHeaderSearchOpts().AddPath("/include", clang::frontend::System, false, true);
#endif

    /** Run the static analysis pass. */
    clang::ento::AnalysisAction Analyze;
    Success = Clang.ExecuteAction(Analyze);
    if (!Success) {
      PUSH_DIAGNOSTIC("libseashell-clang: clang::CompilerInstance::ExecuteAction(AnalysisAction) failed.");
      std::copy(diag_client.messages.begin(), diag_client.messages.end(),
                  std::back_inserter(compile_messages));
      return 1;
    }

    clang::EmitLLVMOnlyAction CodeGen(&compiler->context);
    Success = Clang.ExecuteAction(CodeGen);
    if (!Success) {
      PUSH_DIAGNOSTIC("libseashell-clang: clang::CompilerInstance::ExecuteAction(EmitLLVMOnlyAction) failed.");
      std::copy(diag_client.messages.begin(), diag_client.messages.end(),
                  std::back_inserter(compile_messages));
      return 1;
    }

    /** Store the diagnostics. */
    std::copy(diag_client.messages.begin(), diag_client.messages.end(),
                std::back_inserter(compile_messages));

    std::unique_ptr<Module> mod(CodeGen.takeModule());
    if (!mod) {
      return 1;
    }

    /** Link the module into the one we're building.
     *  NOTE: We destroy the source as we've taken the module
     *  (and that for some awful reason, copying modules breaks horribly
     *   LLVM's DWARF emitter.  Someone really ought to file a bug) */
#if CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR >= 6
    raw_string_ostream Stream(Error);
    DiagnosticPrinterRawOStream DP(Stream);
    Success = !llvm::Linker::LinkModules(module, &*mod, [&](const DiagnosticInfo &DI) { DI.print(DP); });
    Stream.flush();
#else
    Success = !llvm::Linker::LinkModules(module, &*mod, llvm::Linker::DestroySource, &Error);
#endif
    if (!Success) {
      PUSH_DIAGNOSTIC(Error);
      return 1;
    }

    /* Success. */
    return 0;
    #undef PUSH_DIAGNOSTIC
}

/** seashell_preprocessor methods */

/**
 * seashell_preprocessor_make(void)
 * Creates a new instance of the Seashell preprocessor.
 *
 * Returns:
 *  A new instance.
 *
 * Notes:
 *  It might be worthwhile to assign seashell_preprocessor_free as
 *  the cleanup function for garbage collection in the Racket FFI.
 */
extern "C" struct seashell_preprocessor *seashell_preprocessor_make(void) {
  struct seashell_preprocessor *r = new seashell_preprocessor;
#if defined(__EMSCRIPTEN__) && !defined(NDEBUG)
  printf("[preprocessor] Allocating new preprocessor object at %p\n", r);
#endif
  return r;
}

/**
 * seashell_preprocessor_free(struct seashell_preprocessor *preprocessor)
 * Deletes an instance of the Seashell compiler.
 *
 * Arguments:
 *  preprocessor - A Seashell preprocessor instance.
 */
extern "C" void seashell_preprocessor_free(struct seashell_preprocessor *preprocessor) {
#if defined(__EMSCRIPTEN__) && !defined(NDEBUG)
  printf("[preprocessor] De-Allocating new preprocessor object at %p\n", preprocessor);
#endif
  delete preprocessor;
}

/**
 * seashell_preprocessor_set_main_file(struct seashell_preprocessor* preprocessor, const char* file)
 * Sets the main file to begin running the preprocessor on
 *
 * Arguments:
 *  preprocessor - A Seashell preprocessor instance.
 *  file - Pathname of file to add.
 */
#ifndef __EMSCRIPTEN__
extern "C" void seashell_preprocessor_set_main_file(struct seashell_preprocessor *preprocessor, const char* file) {
#else
void seashell_preprocessor_set_main_file(struct seashell_preprocessor *preprocessor, std::string file) {
#endif
  preprocessor->main_file = file;
}

/**
 * seashell_preprocessor_get_include_count(struct seashell_preprocessor* preprocessor)
 * Gets the number of include dependencies the preprocessor found
 *
 * Arguments:
 *  preprocessor - A Seashell preprocessor instance.
 */
extern "C" int seashell_preprocessor_get_include_count(struct seashell_preprocessor* preprocessor) {
  return preprocessor->sources.size();
}

static int preprocess_file(struct seashell_preprocessor *, const char *);

/**
 * seashell_preprocessor_run(struct seashell_preprocessor *preprocessor)
 * Runs the Seashell preprocessor instance.
 *
 * Arguments:
 *  preprocessor - A Seashell preprocessor instance.
 *
 * Returns
 *  0 if everything went OK, nonzero otherwise.
 *
 * Notes:
 *  May output some additional error information to stderr.
 *  seashell_llvm_setup must be called before this function.
 */
extern "C" int seashell_preprocessor_run(struct seashell_preprocessor* preprocessor) {
  return preprocess_file(preprocessor, preprocessor->main_file.c_str());
}

/** Implementation of clang::PPCallbacks used to catch all #includes */
class PPCallbacks : public clang::PPCallbacks {
  
  std::set<std::string> &_deps;
  std::list<std::string> &_wl;

public:
  PPCallbacks(std::set<std::string> &deps, std::list<std::string> &wl) : _deps(deps), _wl(wl) { }

  void InclusionDirective(clang::SourceLocation HashLoc, const clang::Token &IncludeToken,
    clang::StringRef FileName, bool isAngled, clang::CharSourceRange FilenameRange,
    const clang::FileEntry *File, clang::StringRef SearchPath, clang::StringRef RelativePath,
    const clang::Module *Imported) {

    // enforce non-standard library includes must use quotes
    if(!isAngled) {
      const char *path = realpath(File->getName(), NULL);
      if(path == NULL) {
        fprintf(stderr, "NULL path encountered.\n");
      }
      else {
        std::pair<std::set<std::string>::iterator, bool> res =_deps.insert(path);
        // if file is a new dependency, add it to worklist
        if(res.second) {
          fprintf(stderr, "Adding dependency: %s\n", path);
          _wl.push_back(path);
        }
      }
    }
  }
};

/**
 * preprocess_file(seashell_preprocessor* preprocessor, const char* src_path)
 *
 * Compiles a given source file, links it into a module.
 *
 * Arguments:
 *  preprocessor - the Seashell preprocessor object in use
 *  src_path - Main file to preprocess.
 *
 * Returns:
 *  1 on error, 0 otherwise.
 */
static int preprocess_file(struct seashell_preprocessor* preprocessor, const char* src_path)
{
#ifndef NDEBUG
  fprintf(stderr, "[preprocessor] Preprocessing file: %s\n", src_path);
#endif

  std::string Error;
  bool Success;
  std::vector<const char*> args;
  size_t index;
  std::list<std::string> worklist(1, src_path);

  std::vector<seashell_diag>& pp_messages = preprocessor->messages;
#define PUSH_DIAGNOSTIC(x) pp_messages.push_back(seashell_diag(true, src_path, (x)))

  while(worklist.size() > 0) {
    /** Set up compilation arguments. */
    for(std::vector<std::string>::iterator p = preprocessor->compiler_flags.begin();
          p != preprocessor->compiler_flags.end();
          ++p)
    {
      args.push_back(p->c_str());
    }
    args.push_back(src_path);
    
    /** Parse Diagnostic Arguments */
    clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> diag_opts(CreateAndPopulateDiagOpts(&args[0], &args[0] + args.size()));

    /* Invoke clang to compile file to LLVM IR. */
    SeashellDiagnosticClient diag_client(&*diag_opts);

    clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> diag_ID(new clang::DiagnosticIDs());
    clang::DiagnosticsEngine CI_Diags(diag_ID, &*diag_opts, &diag_client, false);
    clang::FileManager CI_FM((clang::FileSystemOptions()));
    clang::SourceManager CI_SM(CI_Diags, CI_FM);

    clang::IntrusiveRefCntPtr<clang::CompilerInvocation> CI(new clang::CompilerInvocation);


    Success = clang::CompilerInvocation::CreateFromArgs(*CI, &args[0], &args[0] + args.size(), CI_Diags);
    if (!Success) {
      PUSH_DIAGNOSTIC("libseashell-clang: clang::CompilerInvocation::CreateFromArgs() failed.");
      std::copy(diag_client.messages.begin(), diag_client.messages.end(),
                  std::back_inserter(pp_messages));
      return 1;
    }

    clang::CompilerInstance Clang;
#if CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR >= 5
    Clang.setInvocation(CI.get());
#elif CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR == 4
    Clang.setInvocation(CI.getPtr());
#endif
    Clang.createDiagnostics(&diag_client, false);
    Clang.createFileManager();
    Clang.createSourceManager(Clang.getFileManager());

    if (!Clang.hasDiagnostics()) {
      PUSH_DIAGNOSTIC("libseashell-clang: clang::CompilerInstance::createDiagnostics() failed.");
      std::copy(diag_client.messages.begin(), diag_client.messages.end(),
                  std::back_inserter(pp_messages));
      return 1;
    }

    /** Add compiler-specific headers. */
#ifndef __EMSCRIPTEN__
    if (!IS_INSTALLED() && access (BUILD_DIR "/lib/llvm/lib/clang/" CLANG_VERSION_STRING "/include/", F_OK) != -1) {
      Clang.getHeaderSearchOpts().AddPath(BUILD_DIR "/lib/llvm/lib/clang/" CLANG_VERSION_STRING "/include/", clang::frontend::System, false, true);
    } else {
      Clang.getHeaderSearchOpts().AddPath(INSTALL_PREFIX "/lib/clang/" CLANG_VERSION_STRING "/include", clang::frontend::System, false, true);
    } 
    /** NOTE: this will have to change for different platforms */
#ifdef MULTIARCH_PLATFORM
    Clang.getHeaderSearchOpts().AddPath("/usr/include/" MULTIARCH_PLATFORM, clang::frontend::System, false, true);
#endif
    /** Set up the default (generic) headers */
    Clang.getHeaderSearchOpts().AddPath("/usr/include", clang::frontend::System, false, true);
#else
    Clang.getHeaderSearchOpts().AddPath("/clang-include/", clang::frontend::System, false, true);
    Clang.getHeaderSearchOpts().AddPath("/include", clang::frontend::System, false, true);
#endif

    /** Run the static analysis pass. */
    clang::ento::AnalysisAction Analyze;
    Success = Clang.ExecuteAction(Analyze);
    if (!Success) {
      PUSH_DIAGNOSTIC("libseashell-clang: clang::CompilerInstance::ExecuteAction(AnalysisAction) failed.");
      std::copy(diag_client.messages.begin(), diag_client.messages.end(),
                  std::back_inserter(pp_messages));
      return 1;
    }

    clang::Preprocessor &pp(Clang.getPreprocessor());
    PPCallbacks *ppc = new PPCallbacks(preprocessor->sources, worklist);
    pp.addPPCallbacks(std::unique_ptr<clang::PPCallbacks>(ppc)); 

    clang::Token token;
    pp.EnterMainSourceFile();
    do {
      pp.Lex(token);
    } while(token.isNot(clang::tok::eof));
  }

  /* Success. */
  return 0;
  #undef PUSH_DIAGNOSTIC
}

#ifdef __EMSCRIPTEN__
#include "compiler-binding.h"
#endif
