#lang scribble/manual
@(require "../utils.rkt")

@title[#:tag "seashell-layout"]{Logistics}

@section{Dependencies}
This section lists all external libraries and tools that Seashell 
requires to build and run.

@subsection[#:tag "seashell-deps"]{System-Provided Dependencies}
These dependencies must be present in the build environment,
and are @bold{not} provided by Seashell.  Seashell will only
build and run on a relatively modern Linux installation.
@itemlist[@item[@link["http://racket-lang.org/" "Racket v5.3.6+"]]
           @item[@link["http://cmake.org" "CMake 2.8+"]]]

@subsection[#:tag "seashell-integrated-deps"]{Integrated Dependencies}
Seashell integrates the following external libraries into
its build process:
@itemlist[@item[@link["http://llvm.org" "LLVM"]]
           @item[@link["http://clang.llvm.org" "Clang"]]
           @item[@link["http://compiler-rt.llvm.org" "Compiler-RT"]]
           @item[@link["https://libgit2.github.com" "libgit2"]]
           @item[@link["http://libtom.org" "LibTomCrypt"]]
           @item[@link["https://bitwiseshiftleft.github.io/sjcl" "SJCL"]]
           @item[@link["https://github.com/alexei/sprintf.js" "Sprintf.JS"]]
           @item[@link["http://www.libssh2.org" "libssh2"]]]

@section{Layout}
Seashell's source files are layed out in the following way:
@local-table-of-contents[]
@; --------------------------------------------------------------
@subsection{Libraries - @source-url-link["/lib"]}
External dependencies and libraries are stored under @source-url-link["/lib"].
If possible, do not store directly source files for external libraries under
@source-url-link["/lib"].  Instead, use @link["http://git-scm.com/docs/git-submodule"
                                              "git submodules"]
instead.

Consult @secref["seashell-integrated-deps"] for a list of integrated external libraries.

                
@subsection{Backend Support Source Files - @source-url-link["/src/backend"]}
The source for support binaries and libraries are stored under @source-url-link["/src/backend"].
The following Seashell support components live here:
@itemlist[@item[@secref["seashell-compiler-support"]]]

@subsection{Backend Server - @source-url-link["/src/collects"]}
@subsection{Fronted - @source-url-link["/src/frontend"]}

            
