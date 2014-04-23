#lang scribble/manual
@(require "../../utils.rkt")
@title[#:tag "seashell-compiler"]{Compiler Backend Library}

Seashell's compiler support library glues Seashell's Racket sources
with the infrastructure provided by LLVM and Clang.

Internal library developers should consult the documentation contained in
@source-url-link["/src/backend/compiler/compiler.cc"]

The following functions and types are exported for public use in Seashell's Racket
sources:

@section{Types}
@itemize[@item{@cppdef["struct seashell_compiler"]
                --- Struct that represents one instance of the compiler.}]
@(define seashell_compiler* '|struct seashell_compiler*|)

@section{Creating and Deleting Compiler Instances}
@function[(|const char*| seashell_clang_version)]{
                                                  Returns the version of Clang that the library is linked
                                                  against.
                                                  }
@function[(seashell_compiler* seashell_compiler_make)]{
                                                       Creates a new instance of the Seashell compiler.
                                                       }
@function[(void seashell_compiler_free)]{
                                         Frees all resources allocated for a Seashell
                                         compiler instance.
                                         }

@section{Manipulating Compiliation Files and Flags}
@function[(void seashell_compiler_add_file
                [seashell_compiler* compiler]
                [|const char*| file])]{
                                       Adds a source file to be compiled.
                                       }

@function[(void seashell_compiler_clear_files
                [seashell_compiler* compiler])]{
                                                Clears the compiler's list of files to compile.
                                                }
@function[(void seashell_compiler_add_compile_flag
                [seashell_compiler* compiler]
                [|const char*| flag])]{
                                       Adds a compilation flag to the compiler.
                                       }
@function[(void seashell_compiler_clear_compile_flags
                [seashell_compiler* compiler])]{
                                                Clears all compilation flags set for the compiler.
                                                }

@section{Error Reporting}
@function[(|const char*| seashell_compiler_get_linker_messages
                         [seashell_compiler* compiler])]{
                                                         Gets any errors or warnings from the intermediate
                                                         linkage step.
                                                         @linebreak[]
                                                         @bold{Note:} The string returned is only valid while
                                                         the compiler instance exists and until the next call
                                                         of @cpp{"seashell_compiler_run"}.}
@function[(int seashell_compiler_get_diagnostic_count
               [seashell_compiler* compiler]
               [int n])]{
                         Gets the number of compilation diagnostic messages available
                         for the @var{n}'th file.}

@function[(int seashell_compiler_get_diagnostic_line
               [seashell_compiler* compiler]
               [int n]
               [int k])]{
                         Gets the line number of the 
                         @var{k}'th available diagnostic message
                         for the @var{n}'th file.
                         }

@function[(int seashell_compiler_get_diagnostic_column
               [seashell_compiler* compiler]
               [int n]
               [int k])]{
                         Gets the column number of the
                         @var{k}'th :









