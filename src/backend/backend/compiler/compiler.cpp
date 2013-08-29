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

#include <clang/Basic/DiagnosticOptions.h>
#include <clang/CodeGen/CodeGenAction.h>
#include <clang/Driver/Compilation.h>
#include <clang/Driver/Driver.h>
#include <clang/Driver/Options.h>
#include <clang/Driver/Tool.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/CompilerInvocation.h>
#include <clang/Frontend/FrontendDiagnostic.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <clang/Frontend/Utils.h>
#include <clang/FrontendTool/Utils.h>
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
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/PluginLoader.h>
#include <llvm/Support/PrettyStackTrace.h>
#include <llvm/Support/Signals.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/ToolOutputFile.h>
#include <llvm/Target/TargetLibraryInfo.h>
#include <llvm/Target/TargetMachine.h>

#include <vector>
#include <string>
#include <iostream>
#include <memory>

using namespace llvm;

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
    LLVMContext &Context = getGlobalContext();
    llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.

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

    // TODO
    SmallVector<const char *, 16> args;
    args.push_back("-I/usr/include");
    args.push_back("-Wall");
    args.push_back(fileName);
    args.push_back("-o");
    args.push_back(binaryDestination);

    // Codegen
    clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> diag_opts(new clang::DiagnosticOptions());
    clang::TextDiagnosticPrinter * diag_client = new clang::TextDiagnosticPrinter(llvm::errs(), &*diag_opts);

    clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> diag_ID(new clang::DiagnosticIDs());
    clang::DiagnosticsEngine Diags(diag_ID, &*diag_opts, diag_client);

    OwningPtr<clang::CompilerInvocation> CI(new clang::CompilerInvocation);

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

    OwningPtr<clang::CodeGenAction> Act(new clang::EmitAssemblyAction());
    Success = Clang.ExecuteAction(*Act);
    if(!Success) {
      std::cerr << "Could not execute EmitAssemblyAction.\n";
      return 1;
    }

    Module * mod = Act->takeModule();

    mod->print(llvm::errs(), NULL);

    // Compile

    SMDiagnostic Err;
    Triple TheTriple;

    TheTriple = Triple(mod->getTargetTriple());

    if (TheTriple.getTriple().empty())
      TheTriple.setTriple(sys::getDefaultTargetTriple());

    std::string Error;
    const Target *TheTarget = TargetRegistry::lookupTarget(MArch, TheTriple, Error);
    if (!TheTarget) {
      errs() << Error;
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

    // Disable .loc support for older OS X versions.
    if (TheTriple.isMacOSX() &&
        TheTriple.isMacOSXVersionLT(10, 6))
      Target.setMCUseLoc(false);

    Error = std::string();
    OwningPtr<tool_output_file> Out
      (new tool_output_file("-", Error, 0));

    if (!Error.empty()) {
      errs() << Error << '\n';
      return 1;
    }
    if (!Out) return 1;

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
        errs() << "warning: ignoring -mc-relax-all because filetype != obj";
      else
        Target.setMCRelaxAll(true);
    }

    {
      formatted_raw_ostream FOS(Out->os());

      AnalysisID StartAfterID = 0;
      AnalysisID StopAfterID = 0;
      const PassRegistry *PR = PassRegistry::getPassRegistry();
      if (!StartAfter.empty()) {
        const PassInfo *PI = PR->getPassInfo(StartAfter);
        if (!PI) {
          errs() << "fatal: start-after pass is not registered.\n";
          return 1;
        }
        StartAfterID = PI->getTypeInfo();
      }
      if (!StopAfter.empty()) {
        const PassInfo *PI = PR->getPassInfo(StopAfter);
        if (!PI) {
          errs() << "fatal: stop-after pass is not registered.\n";
          return 1;
        }
        StopAfterID = PI->getTypeInfo();
      }

      // Ask the target to add backend passes as necessary.
      if (Target.addPassesToEmitFile(PM, FOS, FileType, false,
                                    StartAfterID, StopAfterID)) {
        errs() << "fatal: target does not support generation of this"
               << " file type!\n";
        return 1;
      }

      // Before executing passes, print the final values of the LLVM options.
      cl::PrintOptionValues();

      PM.run(*mod);
    }

    // Declare success.
    Out->keep();

    return 0;
}

int main() {
  if(compile_one_file("foo.c", "foo.out")) {
    std::cerr << "not successful\n";
  } else {
    std::cerr << "successful\n";
  }
}

