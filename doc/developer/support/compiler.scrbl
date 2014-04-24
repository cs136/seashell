#lang scribble/manual
@(require "../../utils.rkt")
@title[#:tag "seashell-compiler-support"]{Compiler Support Library}

Seashell's compiler support library glues Seashell's Racket sources
with the infrastructure provided by LLVM and Clang.

Internal library developers should consult the documentation contained in
@source-url-link["/src/backend/compiler/compiler.cc"]

The following functions and types are exported for public use in Seashell's Racket
sources:

@section{Types}
@itemize[@item{@cppdef["struct seashell_compiler"]
                --- Struct that represents one instance of the compiler.}]

@section{Creating and Deleting Compiler Instances}
@function[(|const char*| seashell_clang_version)]{
  Returns the version of Clang that the library is linked
  against.
}
@function[(|struct seashell_compiler*| seashell_compiler_make)]{
  Creates a new instance of the Seashell compiler.
}
@function[(void seashell_compiler_free)]{
  Frees all resources allocated for a Seashell
  compiler instance.
}

@section{Manipulating Compiliation Files and Flags}
@function[(void seashell_compiler_add_file
                [|struct seashell_compiler*| compiler]
                [|const char*| file])]{
  Adds a source file to be compiled.
}

@function[(void seashell_compiler_clear_files
                [|struct seashell_compiler*| compiler])]{
  Clears the compiler's list of files to compile.
}
@function[(void seashell_compiler_add_compile_flag
                [|struct seashell_compiler*| compiler]
                [|const char*| flag])]{
  Adds a compilation flag to the compiler.
}
@function[(void seashell_compiler_clear_compile_flags
                [|struct seashell_compiler*| compiler])]{
  Clears all compilation flags set for the compiler.
}

@section{Error Reporting}
@function[(|const char*| seashell_compiler_get_linker_messages
                         [|struct seashell_compiler*| compiler])]{
  Gets any errors or warnings from the intermediate
  linkage step.
  @linebreak[]
  @bold{Note:} The string returned is only valid while
  the compiler instance exists and until the next call
  of @cpp{"seashell_compiler_run"}.
}
@function[(int seashell_compiler_get_diagnostic_count
               [|struct seashell_compiler*| compiler]
               [int n])]{
  Gets the number of compilation diagnostic messages available
  for the @var{n}'th file.
}

@function[(int seashell_compiler_get_diagnostic_line
               [|struct seashell_compiler*| compiler]
               [int n]
               [int k])]{
  Gets the line number of the 
  @var{k}'th available diagnostic message
  for the @var{n}'th file.
}

@function[(int seashell_compiler_get_diagnostic_column
               [|struct seashell_compiler*| compiler]
               [int n]
               [int k])]{
Gets the column number of the
@var{k}'th available diagnostic message
for the @var{n}'th file.
                         }

@function[(int seashell_compiler_get_diagnostic_error
               [|struct seashell_compiler*| compiler]
               [int n]
               [int k])]{
Gets if the @var{k}'th available diagnostic for the @var{n}'th
file is an error diagnostic.
}

@function[(|const char*| seashell_compiler_get_diagnostic_file
                         [|struct seashell_compiler*| compiler]
                         [int n]
                         [int k])]{
Gets file name for the @var{k}'th available
diagnostic message for the @var{n}'th file.
}

@function[(|const char*| seashell_compiler_get_diagnostic_message
                         [|struct seashell_compiler*| compiler]
                         [int n]
                         [int k])]{
Gets the @var{k}'th available diagnostic message
for the @var{n}'th file.
}

@section{Running the Compiler}
@function[(int seashell_compiler_run
               [|struct seashell_compiler*| compiler])]{
Runs the Seashell compiler.  Returns @cpp{0} on success,
nonzero otherwise.
}

@function[(void* seashell_compiler_get_object
                 [|struct seashell_compiler*| compiler]
                 [|int* [OUT]| length])]{
Returns a pointer to the resulting compiled object, if any.
The length of the result is returned through the output
argument @var{length}.
}
