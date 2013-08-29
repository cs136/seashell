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
#include <llvm/Target/TargetLibraryInfo.h>
#include <llvm/Target/TargetMachine.h>

#include <vector>
#include <string>
#include <iostream>
#include <memory>
#include <algorithm>

#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>

using namespace llvm;

/** Seashell's compiler data structure.
 * Opaque to Racket - make sure to pass a cleanup function
 * to the FFI so garbage collection works properly.
 */
struct seashell_compiler {
  std::vector<std::string> compiler_flags;
  std::vector<std::string> linker_flags;
  std::vector<std::string> source_paths;
  std::string output_path;
};

/**
 * seashell_llvm_setup (void)
 * Performs necessary LLVM setup.
 *
 * Notes:
 *  This function *must* be called *exactly once* before any calls
 *  to seashell_compiler_run.
 */
extern "C" void seashell_llvm_setup() {
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmPrinters();
    InitializeAllAsmParsers();

    PassRegistry *Registry = PassRegistry::getPassRegistry();
    initializeCore(*Registry);
    initializeCodeGen(*Registry);
    initializeLoopStrengthReducePass(*Registry);
    initializeLowerIntrinsicsPass(*Registry);
    initializeUnreachableBlockElimPass(*Registry);
}

/**
 * seashell_llvm_cleanup (void)
 * Performs necessary LLVM cleanup.
 *
 * Notes:
 *  This function *may* be called at most *once* after any calls
 *  to seashell_compiler_run.
 */
extern "C" void seashell_llvm_cleanup() {
  llvm_shutdown();
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
  return new seashell_compiler;
}

/**
 * seashell_compiler_free (struct seashell_compiler* compiler)
 * Deletes an instance of the Seashell compiler.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 */
extern "C" void seashell_compiler_free (struct seashell_compiler* compiler) {
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
extern "C" void seashell_compiler_add_file (struct seashell_compiler* compiler, const char* file) {
  compiler->source_paths.push_back(file);
}

/**
 * seashell_compiler_clear_files (struct seashell_compiler* compiler)
 * Clears the compiler's input file list.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 */
extern "C" void seashell_compiler_clear_files (struct seashell_compiler* compiler, const char* file) {
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
extern "C" void seashell_compiler_add_compile_flag (struct seashell_compiler* compiler, const char* flag) {
  compiler->compiler_flags.push_back(flag);
}

/**
 * seashell_compiler_clear_compile_flags (struct seashell_compiler* compiler)
 * Clears the compiler's compilation flag list.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 */
extern "C" void seashell_compiler_clear_compile_flags (struct seashell_compiler* compiler, const char* file) {
  compiler->compiler_flags.clear();
}

/**
 * seashell_compiler_add_link_flag (struct seashell_compiler* compiler, const char* flag)
 * Adds a linking flag to the compiler.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  flag - Linking flag to add.
 */
extern "C" void seashell_compiler_add_link_flag (struct seashell_compiler* compiler, const char* flag) {
  compiler->linker_flags.push_back(flag);
}

/**
 * seashell_compiler_clear_link_flags (struct seashell_compiler* compiler)
 * Clears the compiler's linking flag list.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 */
extern "C" void seashell_compiler_clear_link_flags (struct seashell_compiler* compiler, const char* file) {
  compiler->linker_flags.clear();
}

/**
 * seashell_compiler_set_output (struct seashell_compiler* compiler, const char* file)
 * Sets the compiler's output file.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *  file - Pathname for output file.
 */
extern "C" void seashell_compiler_set_output (struct seashell_compiler* compiler, const char* file) {
  compiler->output_path = file;
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
  struct seashell_diag {
    seashell_diag(std::string m, int l, int c)
      : mesg(m), line(l), col(c), loc_known(true) { }
    seashell_diag(std::string m)
      : mesg(m), line(0), col(0), loc_known(false) { }
    std::string mesg;
    int line, col;
    bool loc_known;
  };

  std::multimap<std::string, seashell_diag> messages;

  SeashellDiagnosticClient(clang::DiagnosticOptions * diags) : DiagOpts(diags) { }
  virtual ~SeashellDiagnosticClient() { }

  void BeginSourceFile(const clang::LangOptions &LO, const clang::Preprocessor *PP) { }
  void EndSourceFile() { }
  void HandleDiagnostic(clang::DiagnosticsEngine::Level Level, const clang::Diagnostic & Info) {
    clang::DiagnosticConsumer::HandleDiagnostic(Level, Info);
    SmallString<100> OutStr;
    Info.FormatDiagnostic(OutStr);
    llvm::raw_svector_ostream DiagMessageStream(OutStr);
    printDiagnosticOptions(DiagMessageStream, Level, Info, *DiagOpts);

    const clang::SourceManager & SM = Info.getSourceManager();
    const clang::SourceLocation & Loc = Info.getLocation();

    clang::PresumedLoc PLoc = SM.getPresumedLoc(Loc);
    if(PLoc.isInvalid()) {
      clang::FileID FID = SM.getFileID(Loc);
      if(!FID.isInvalid()) {
        const clang::FileEntry * FE = SM.getFileEntryForID(FID);
        if(FE && FE->getName()) {
          messages.insert(std::make_pair(FE->getName(), seashell_diag(OutStr.c_str())));
        } else {
          messages.insert(std::make_pair("?", seashell_diag(OutStr.c_str())));
        }
      } else {
        messages.insert(std::make_pair("?", seashell_diag(OutStr.c_str())));
      }
    } else {
      messages.insert(std::make_pair(PLoc.getFilename(),
                        seashell_diag(OutStr.c_str(), PLoc.getLine(),
                                        PLoc.getColumn())));
    }
  }

  void dump() {
    for(std::multimap<std::string, seashell_diag>::iterator p = messages.begin();
        p != messages.end();
        ++p) {
      std::cerr << p->first << ":" << p->second.line << ":" << p->second.col << ": " << p->second.mesg << "\n";
    }
  }
};

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
 *  May output additional error information to std::cerr.
 *  seashell_llvm_setup must be called before this function.
 */
extern "C" int seashell_compiler_run (struct seashell_compiler* compiler) {
    LLVMContext Context;

    std::vector<const char*> args;
    for(std::vector<std::string>::iterator p = compiler->compiler_flags.begin();
          p != compiler->compiler_flags.end();
          ++p)
    {
      args.push_back(p->c_str());
    }
    for(std::vector<std::string>::iterator p = compiler->source_paths.begin();
          p != compiler->source_paths.end();
          ++p)
    {
      args.push_back(p->c_str());
    }

    /* Codegen to LLVM IR. */
    clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> diag_opts(new clang::DiagnosticOptions());
    SeashellDiagnosticClient * diag_client = new SeashellDiagnosticClient(&*diag_opts);

    clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> diag_ID(new clang::DiagnosticIDs());
    clang::DiagnosticsEngine Diags(diag_ID, &*diag_opts, diag_client);

    OwningPtr<clang::CompilerInvocation> CI(new clang::CompilerInvocation);

    bool Success = clang::CompilerInvocation::CreateFromArgs(*CI, &args[0], &args[0] + args.size(), Diags);
    if(!Success) {
      std::cerr << "libseashell-clang: clang::CompilerInvocation::CreateFromArgs() failed\n";
      return 1;
    }

    clang::CompilerInstance Clang;
    Clang.setInvocation(CI.take());
    Clang.createDiagnostics(diag_client, false);

    if(!Clang.hasDiagnostics()) {
      std::cerr << "libseashell-clang: clang::CompilerInstance::createDiagnostics() failed\n";
      return 1;
    }

    OwningPtr<clang::CodeGenAction> Act(new clang::EmitLLVMOnlyAction());
    Success = Clang.ExecuteAction(*Act);
    if(!Success) {
      std::cerr << "libseashell-clang: clang::CompilerInstance::ExecuteAction(EmitLLVMOnlyAction) failed\n";
      return 1;
    }

    // XXX
    diag_client->dump();

    Module * mod = Act->takeModule();

    /* Compile LLVM IR to architecture-specific assembly code. */

    SMDiagnostic Err;
    Triple TheTriple;

    TheTriple = Triple(mod->getTargetTriple());

    if (TheTriple.getTriple().empty())
      TheTriple.setTriple(sys::getDefaultTargetTriple());

    std::string Error;
    const Target *TheTarget = TargetRegistry::lookupTarget(MArch, TheTriple, Error);
    if (!TheTarget) {
      std::cerr << "libseashell-clang: " << Error;
      return 1;
    }

    std::string FeaturesStr;
    if (MAttrs.size()) {
      SubtargetFeatures Features;
      for (unsigned i = 0; i != MAttrs.size(); ++i)
        Features.AddFeature(MAttrs[i]);
      FeaturesStr = Features.getString();
    }

    CodeGenOpt::Level OLvl = CodeGenOpt::Default;

    TargetOptions Options;
    Options.LessPreciseFPMADOption = EnableFPMAD;
    Options.NoFramePointerElim = DisableFPElim;
    Options.AllowFPOpFusion = FuseFPOps;
    Options.UnsafeFPMath = EnableUnsafeFPMath;
    Options.NoInfsFPMath = EnableNoInfsFPMath;
    Options.NoNaNsFPMath = EnableNoNaNsFPMath;
    Options.HonorSignDependentRoundingFPMathOption =
        EnableHonorSignDependentRoundingFPMath;
    Options.UseSoftFloat = GenerateSoftFloatCalls;
    if (FloatABIForCalls != FloatABI::Default)
      Options.FloatABIType = FloatABIForCalls;
    Options.NoZerosInBSS = DontPlaceZerosInBSS;
    Options.GuaranteedTailCallOpt = EnableGuaranteedTailCallOpt;
    Options.DisableTailCalls = DisableTailCalls;
    Options.StackAlignmentOverride = OverrideStackAlignment;
    Options.TrapFuncName = TrapFuncName;
    Options.PositionIndependentExecutable = EnablePIE;
    Options.EnableSegmentedStacks = SegmentedStacks;
    Options.UseInitArray = UseInitArray;

    OwningPtr<TargetMachine>
      target(TheTarget->createTargetMachine(TheTriple.getTriple(),
                                            MCPU, FeaturesStr, Options,
                                            RelocModel, CMModel, OLvl));
    assert(target.get() && "Could not allocate target machine!");
    TargetMachine &Target = *target.get();

    if (DisableDotLoc)
      Target.setMCUseLoc(false);

    if (DisableCFI)
      Target.setMCUseCFI(false);

    if (EnableDwarfDirectory)
      Target.setMCUseDwarfDirectory(true);

    if (GenerateSoftFloatCalls)
      FloatABIForCalls = FloatABI::Soft;

    if (TheTriple.isMacOSX() &&
        TheTriple.isMacOSXVersionLT(10, 6))
      Target.setMCUseLoc(false);

    PassManager PM;

    TargetLibraryInfo *TLI = new TargetLibraryInfo(TheTriple);
    PM.add(TLI);

    Target.addAnalysisPasses(PM);

    if (const DataLayout *TD = Target.getDataLayout())
      PM.add(new DataLayout(*TD));
    else
      PM.add(new DataLayout(mod));

    Target.setAsmVerbosityDefault(true);

    if (RelaxAll) {
      if (FileType != TargetMachine::CGFT_ObjectFile)
        std::cerr << "libseashell-clang: warning: ignoring -mc-relax-all because filetype != obj";
      else
        Target.setMCRelaxAll(true);
    }

    std::string ArchAsm;

    {
      raw_string_ostream OS(ArchAsm);
      formatted_raw_ostream FOS(OS);

      AnalysisID StartAfterID = 0;
      AnalysisID StopAfterID = 0;
      const PassRegistry *PR = PassRegistry::getPassRegistry();
      if (!StartAfter.empty()) {
        const PassInfo *PI = PR->getPassInfo(StartAfter);
        if (!PI) {
          std::cerr << "libseashell-clang: fatal: start-after pass is not registered\n";
          return 1;
        }
        StartAfterID = PI->getTypeInfo();
      }
      if (!StopAfter.empty()) {
        const PassInfo *PI = PR->getPassInfo(StopAfter);
        if (!PI) {
          std::cerr << "libseashell-clang: fatal: stop-after pass is not registered\n";
          return 1;
        }
        StopAfterID = PI->getTypeInfo();
      }

      if (Target.addPassesToEmitFile(PM, FOS, FileType, false,
                                    StartAfterID, StopAfterID)) {
        std::cerr << "libseashell-clang: fatal: target does not support generation of this"
                  << " file type\n";
        return 1;
      }

      PM.run(*mod);
    }

    /* Assemble architecture-specific assembly code and link using host's cc. */

    {
      std::vector<const char *> args;
      args.push_back("cc");
      for(std::vector<std::string>::iterator p = compiler->compiler_flags.begin();
            p != compiler->compiler_flags.end();
            ++p)
      {
        args.push_back(p->c_str());
      }
      args.push_back("-x");
      args.push_back("assembler");
      args.push_back("-o");
      args.push_back(compiler->output_path.c_str());
      args.push_back("-");
      args.push_back(NULL);

      int p[2];
      int res = pipe(p);
      if(res) {
        std::cerr << "libseashell-clang: Could not pipe()\n";
        return 1;
      }

      int pid = fork();
      if(pid < 0) {
        std::cerr << "libseashell-clang: Could not fork()\n";
        return 1;
      } else if(pid == 0) {
        close(0);
        close(p[1]);
        res = dup2(p[0], 0);
        if(res) {
          std::cerr << "libseashell-clang: Could not dup2()\n";
          exit(1);
        }
        res = execvp("cc", (char * const *)&args[0]);
        if(res) {
          char buf[256];
          char * err = strerror_r(errno, buf, 256);
          std::cerr << "libseashell-clang: Could not execute host cc: " << err << "\n";
          exit(1);
        }
      }

      close(p[0]);
      int pos = 0;
      while(pos < ArchAsm.length()) {
        res = write(p[1], ArchAsm.c_str() + pos, std::min<size_t>(1024, ArchAsm.length() - pos));
        if(res < 0) {
          if(errno != EINTR) {
            char buf[256];
            char * err = strerror_r(errno, buf, 256);
            std::cerr << "libseashell-clang: Could not write to pipe: " << err << "\n";
            return 1;
          }
        } else {
          pos += res;
        }
      }
      close(p[1]);

      int status = 0;
      while((res = waitpid(pid, &status, 0)) < 0) {
        if(errno != EINTR) {
          char buf[256];
          char * err = strerror_r(errno, buf, 256);
          std::cerr << "libseashell-clang: waitpid() failed: " << err << "\n";
          return 1;
        }
      }

      if(!WIFEXITED(status) || (WEXITSTATUS(status) != 0)) {
        return 1;
      }
    }

    return 0;
}

int main() {
  seashell_llvm_setup();
  struct seashell_compiler * comp = seashell_compiler_make();
  seashell_compiler_add_file(comp, "foo.c");
  seashell_compiler_set_output(comp, "foo");
  seashell_compiler_add_compile_flag(comp, "-Wall");
  if(seashell_compiler_run(comp)) {
    std::cerr << "not successful\n";
  } else {
    std::cerr << "successful\n";
  }
  seashell_compiler_free(comp);
  seashell_llvm_cleanup();
}

