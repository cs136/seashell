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
#include <sstream>
#include <algorithm>

#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <poll.h>

using namespace llvm;

/** Data structure for compiler diagnostic messages.
 * Opaque to Racket - C accessor functions described below.
 */
struct seashell_diag {
  seashell_diag(std::string f, std::string m, int l, int c)
    : file(f), mesg(m), line(l), col(c), loc_known(true) { }
  seashell_diag(std::string f, std::string m)
    : file(f), mesg(m), line(0), col(0), loc_known(false) { }
  std::string file, mesg;
  int line, col;
  bool loc_known;
};

/** Seashell's compiler data structure.
 * Opaque to Racket - make sure to pass a cleanup function
 * to the FFI so garbage collection works properly.
 */
struct seashell_compiler {
  std::vector<std::string> compiler_flags;
  std::vector<std::string> linker_flags;
  std::vector<std::string> source_paths;
  std::vector<std::string> compiled_modules;
  std::vector<std::vector<seashell_diag> > module_messages;
  std::vector<char> output_executable;
  std::string linker_messages;
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
extern "C" void seashell_compiler_clear_compile_flags (struct seashell_compiler* compiler) {
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
extern "C" void seashell_compiler_clear_link_flags (struct seashell_compiler* compiler) {
  compiler->linker_flags.clear();
}

/**
 * seashell_compiler_get_linker_messages (struct seashell_compiler* compiler)
 * Gets any errors or warnings related to the linking stage, as a string.
 *
 * Arguments:
 *  compiler - A Seashell compiler instance.
 *
 * Notes:
 *  The string returned may be changed after further calls to
 *  seashell_compiler_run.
 */
extern "C" const char * seashell_compiler_get_linker_messages(struct seashell_compiler* compiler) {
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
extern "C" int seashell_compiler_get_diagnostic_count(struct seashell_compiler* compiler, int n) {
  if(compiler->module_messages.size() >= n) {
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
extern "C" int seashell_compiler_get_diagnostic_line(struct seashell_compiler* compiler, int n, int k) {
  if(compiler->module_messages.size() >= n) {
    return 0;
  } else {
    if(compiler->module_messages.at(n).size() >= k) {
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
extern "C" int seashell_compiler_get_diagnostic_column(struct seashell_compiler* compiler, int n, int k) {
  if(compiler->module_messages.size() >= n) {
    return 0;
  } else {
    if(compiler->module_messages.at(n).size() >= k) {
      return 0;
    } else {
      return compiler->module_messages.at(n).at(k).col;
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
extern "C" const char * seashell_compiler_get_diagnostic_file(struct seashell_compiler* compiler, int n, int k) {
  if(compiler->module_messages.size() >= n) {
    return 0;
  } else {
    if(compiler->module_messages.at(n).size() >= k) {
      return 0;
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
 */
extern "C" const char * seashell_compiler_get_diagnostic_message(struct seashell_compiler* compiler, int n, int k) {
  if(compiler->module_messages.size() >= n) {
    return 0;
  } else {
    if(compiler->module_messages.at(n).size() >= k) {
      return 0;
    } else {
      return compiler->module_messages.at(n).at(k).mesg.c_str();
    }
  }
}

static int link_modules(struct seashell_compiler* compiler, std::string & errors);

static int compile_module(struct seashell_compiler* compiler, const char* src_path,
                          std::string & assembly_code,
                          std::vector<seashell_diag> & compile_messages);

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
    std::string errors;

    compiler->compiled_modules.clear();
    compiler->module_messages.clear();

    for(std::vector<std::string>::iterator p = compiler->source_paths.begin();
          p != compiler->source_paths.end();
          ++p)
    {
      compiler->compiled_modules.push_back("");
      compiler->module_messages.push_back(std::vector<seashell_diag>());
      if(compile_module(compiler, p->c_str(), compiler->compiled_modules.back(),
                          compiler->module_messages.back())) {
        return 1;
      }
    }

    if(link_modules(compiler, errors)) {
      compiler->linker_messages = errors;
      return 1;
    }

    compiler->linker_messages = "";
    return 0;
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
  std::vector<seashell_diag> messages;

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
          messages.push_back(seashell_diag(FE->getName(), OutStr.c_str()));
        } else {
          messages.push_back(seashell_diag("?", OutStr.c_str()));
        }
      } else {
        messages.push_back(seashell_diag("?", OutStr.c_str()));
      }
    } else {
      messages.push_back(seashell_diag(PLoc.getFilename(), OutStr.c_str(),
                                        PLoc.getLine(), PLoc.getColumn()));
    }
  }
};

struct raii_pipe {
  raii_pipe(int p[])
    : r(p[0]), w(p[1]), done(0)
  { }
  ~raii_pipe() {
    close();
  }
  void close() {
    if(!(done & 1)) ::close(r);
    if(!(done & 2)) ::close(w);
    done = 3;
  }
  void close_r() {
    if(!(done & 1)) ::close(r);
    done |= 1;
  }
  void close_w() {
    if(!(done & 2)) ::close(w);
    done |= 2;
  }
  int r, w, done;
};

static int link_modules(struct seashell_compiler* compiler, std::string & errors)
{
    errors = "";

    /* Clear output binary buffer. */
    compiler->output_executable.clear();

    /* Open some pipes for assembly input. */
    int p[2], res;
    std::vector<std::shared_ptr<raii_pipe>> in_pipes;
    for(int i = 0; i < compiler->compiled_modules.size(); i++) {
      res = pipe(p);
      if(res) {
        std::cerr << "libseashell-clang: Could not pipe()\n";
        errors = "Internal error";
        return 1;
      }
      in_pipes.push_back(std::shared_ptr<raii_pipe>(new raii_pipe(p)));
    }

    res = pipe(p);
    if(res) {
      std::cerr << "libseashell-clang: Could not pipe()\n";
      errors = "Internal error";
      return 1;
    }
    std::shared_ptr<raii_pipe> out_pipe(new raii_pipe(p));

    res = pipe(p);
    if(res) {
      std::cerr << "libseashell-clang: Could not pipe()\n";
      errors = "Internal error";
      return 1;
    }
    std::shared_ptr<raii_pipe> mesg_pipe(new raii_pipe(p));

    /* Assemble and link architecture-specific assembly code using host's cc. */

    std::vector<std::shared_ptr<std::string> > args_data;
    std::vector<const char *> args;
    std::stringstream arg;
#define ARGPUSH do {\
  args_data.push_back(std::shared_ptr<std::string>(new std::string(arg.str())));\
  args.push_back(args_data.back().get()->c_str());\
  arg.str(""); } while(0)

    arg << "cc"; ARGPUSH;
    for(std::vector<std::string>::iterator p = compiler->linker_flags.begin();
          p != compiler->linker_flags.end();
          ++p)
    {
      arg << p->c_str(); ARGPUSH;
    }

    arg << "-pipe"; ARGPUSH;
    arg << "-x"; ARGPUSH;
    arg << "assembler"; ARGPUSH;
    arg << "-o"; ARGPUSH;
    arg << "/proc/self/fd/" << out_pipe->w; ARGPUSH;

    for(int i = 0; i < compiler->compiled_modules.size(); i++) {
      arg << "/proc/self/fd/" << in_pipes.at(i)->r; ARGPUSH;
    }

    args.push_back(NULL);

#undef ARGPUSH

    int pid = fork();
    if(pid < 0) {
      std::cerr << "libseashell-clang: Could not fork()\n";
      errors = "Internal error";
      return 1;
    } else if(pid == 0) {
      close(0);
      close(1);
      close(2);
      dup2(mesg_pipe->w, 1);
      dup2(mesg_pipe->w, 2);
      out_pipe->close_r();
      mesg_pipe->close_r();
      for(int i = 0; i < compiler->compiled_modules.size(); i++) {
        in_pipes.at(i)->close_w();
      }
      res = execvp("cc", (char * const *)&args[0]);
      if(res) {
        char buf[256];
        char * err = strerror_r(errno, buf, 256);
        std::cerr << "libseashell-clang: Could not execute host cc: " << err << "\n";
        exit(1);
      }
    }

    out_pipe->close_w();
    mesg_pipe->close_w();
    for(int i = 0; i < compiler->compiled_modules.size(); i++) {
      in_pipes.at(i)->close_r();
    }

    int nfds = compiler->compiled_modules.size() + 2;
    int * pos = new int[nfds - 2];
    struct pollfd * fds = new pollfd[nfds];
    for(int i = 0; i < compiler->compiled_modules.size(); i++) {
      pos[i] = 0;
      fds[i].events = POLLOUT;
      fds[i].revents = 0;
      fds[i].fd = in_pipes.at(i)->w;
    }
    fds[nfds - 2].events = POLLIN | POLLHUP;
    fds[nfds - 2].revents = 0;
    fds[nfds - 2].fd = out_pipe->r;
    fds[nfds - 1].events = POLLIN;
    fds[nfds - 1].revents = 0;
    fds[nfds - 1].fd = mesg_pipe->r;

    char inbfr[1024];
    int chan_closed = 0;

    while(chan_closed != 3) {
      res = poll(fds, nfds, -1);
      if(res < 0) {
        if(errno == EINTR) {
          continue;
        } else {
          char buf[256];
          char * err = strerror_r(errno, buf, 256);
          std::cerr << "libseashell-clang: poll() failed: " << err << "\n";
          errors = "Internal error";
          return 1;
        }
      }
      for(int i = 0; i < compiler->compiled_modules.size(); i++) {
        if(fds[i].revents & POLLOUT) {
          res = write(in_pipes.at(i)->w, compiler->compiled_modules.at(i).c_str() + pos[i],
                      std::min<size_t>(1024, compiler->compiled_modules.at(i).length() - pos[i]));
          if(res < 0) {
            if(errno != EINTR) {
              char buf[256];
              char * err = strerror_r(errno, buf, 256);
              std::cerr << "libseashell-clang: Could not write to pipe: " << err << "\n";
              errors = "Internal error";
              return 1;
            }
          } else {
            pos[i] += res;
            if(pos[i] == compiler->compiled_modules.at(i).length()) {
              in_pipes.at(i)->close_w();
              fds[i].fd = -1;
            }
          }
          fds[i].revents = 0;
        }
      }
      if(fds[nfds - 1].revents & POLLIN) {
        res = read(mesg_pipe->r, inbfr, 1024);
        if(res < 0) {
          if(errno != EINTR) {
            char buf[256];
            char * err = strerror_r(errno, buf, 256);
            std::cerr << "libseashell-clang: Could not read from pipe: " << err << "\n";
            errors = "Internal error";
            return 1;
          }
        } else if(res == 0) {
          fds[nfds - 1].fd = -1;
          chan_closed |= 1;
        }
        std::copy(inbfr, inbfr + res, std::back_inserter(errors));
        fds[nfds - 1].revents = 0;
      }
      if(fds[nfds - 1].revents & POLLHUP) {
        fds[nfds - 1].fd = -1;
        chan_closed |= 1;
      }
      if(fds[nfds - 2].revents & POLLIN) {
        res = read(out_pipe->r, inbfr, 1024);
        if(res < 0) {
          if(errno != EINTR) {
            char buf[256];
            char * err = strerror_r(errno, buf, 256);
            std::cerr << "libseashell-clang: Could not read from pipe: " << err << "\n";
            errors = "Internal error";
            return 1;
          }
        } else if(res == 0) {
          chan_closed |= 2;
        }
        std::copy(inbfr, inbfr + res, std::back_inserter(compiler->output_executable));
        fds[nfds - 2].revents = 0;
      }
      if(fds[nfds - 2].revents & POLLHUP) {
        chan_closed |= 2;
      }
    }

    delete[] pos;
    delete[] fds;

    for(int i = 0; i < compiler->compiled_modules.size(); i++) {
      in_pipes.at(i)->close();
    }
    out_pipe->close();
    mesg_pipe->close();

    int status = 0;
    while((res = waitpid(pid, &status, 0)) < 0) {
      if(errno != EINTR) {
        char buf[256];
        char * err = strerror_r(errno, buf, 256);
        std::cerr << "libseashell-clang: waitpid() failed: " << err << "\n";
        errors = "Internal error";
        return 1;
      }
    }

    if(!WIFEXITED(status) || (WEXITSTATUS(status) != 0)) {
      return 1;
    }

    return 0;
}

static int compile_module(struct seashell_compiler* compiler, const char* src_path,
                          std::string & assembly_code,
                          std::vector<seashell_diag> & compile_messages)
{
    LLVMContext Context;

    std::vector<const char*> args;
    for(std::vector<std::string>::iterator p = compiler->compiler_flags.begin();
          p != compiler->compiler_flags.end();
          ++p)
    {
      args.push_back(p->c_str());
    }

    args.push_back(src_path);

    /* Codegen to LLVM IR. */
    clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> diag_opts(new clang::DiagnosticOptions());
    SeashellDiagnosticClient * diag_client = new SeashellDiagnosticClient(&*diag_opts);

    clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> diag_ID(new clang::DiagnosticIDs());
    clang::DiagnosticsEngine Diags(diag_ID, &*diag_opts, diag_client);

    OwningPtr<clang::CompilerInvocation> CI(new clang::CompilerInvocation);

    bool Success = clang::CompilerInvocation::CreateFromArgs(*CI, &args[0], &args[0] + args.size(), Diags);
    if(!Success) {
      std::cerr << "libseashell-clang: clang::CompilerInvocation::CreateFromArgs() failed\n";
      std::copy(diag_client->messages.begin(), diag_client->messages.end(),
                  std::back_inserter(compile_messages));
      return 1;
    }

    clang::CompilerInstance Clang;
    Clang.setInvocation(CI.take());
    Clang.createDiagnostics(diag_client, false);

    if(!Clang.hasDiagnostics()) {
      std::cerr << "libseashell-clang: clang::CompilerInstance::createDiagnostics() failed\n";
      std::copy(diag_client->messages.begin(), diag_client->messages.end(),
                  std::back_inserter(compile_messages));
      return 1;
    }

    OwningPtr<clang::CodeGenAction> Act(new clang::EmitLLVMOnlyAction());
    Success = Clang.ExecuteAction(*Act);
    if(!Success) {
      std::cerr << "libseashell-clang: clang::CompilerInstance::ExecuteAction(EmitLLVMOnlyAction) failed\n";
      std::copy(diag_client->messages.begin(), diag_client->messages.end(),
                  std::back_inserter(compile_messages));
      return 1;
    }

    Module * mod = Act->takeModule();
    if(!mod) {
      std::cerr << "libseashell-clang: takeModule() failed\n";
      std::copy(diag_client->messages.begin(), diag_client->messages.end(),
                  std::back_inserter(compile_messages));
      return 1;
    }

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
      std::copy(diag_client->messages.begin(), diag_client->messages.end(),
                  std::back_inserter(compile_messages));
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
    if (!target.get()) {
      std::cerr << "libseashell-clang: Could not allocate target machine\n";
      std::copy(diag_client->messages.begin(), diag_client->messages.end(),
                  std::back_inserter(compile_messages));
      return 1;
    }
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
          std::copy(diag_client->messages.begin(), diag_client->messages.end(),
                      std::back_inserter(compile_messages));
          return 1;
        }
        StartAfterID = PI->getTypeInfo();
      }
      if (!StopAfter.empty()) {
        const PassInfo *PI = PR->getPassInfo(StopAfter);
        if (!PI) {
          std::cerr << "libseashell-clang: fatal: stop-after pass is not registered\n";
          std::copy(diag_client->messages.begin(), diag_client->messages.end(),
                      std::back_inserter(compile_messages));
          return 1;
        }
        StopAfterID = PI->getTypeInfo();
      }

      if (Target.addPassesToEmitFile(PM, FOS, FileType, false,
                                    StartAfterID, StopAfterID)) {
        std::cerr << "libseashell-clang: fatal: target does not support generation of this"
                  << " file type\n";
        std::copy(diag_client->messages.begin(), diag_client->messages.end(),
                    std::back_inserter(compile_messages));
        return 1;
      }

      PM.run(*mod);
    }

    /* Success. */
    assembly_code = ArchAsm;
    std::copy(diag_client->messages.begin(), diag_client->messages.end(),
                std::back_inserter(compile_messages));

    return 0;
}

