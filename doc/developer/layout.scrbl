#lang scribble/manual
@(require "../utils.rkt")

@title[#:tag "seashell-layout"]{General Layout and Organization}

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

Currently, Seashell integrates the following external libraries:
@itemlist[@item[@link["http://llvm.org" "LLVM"]]
           @item[@link["http://clang.llvm.org" "Clang"]]
           @item[@link["http://compiler-rt.llvm.org" "Compiler-RT"]]
           @item[@link["https://libgit2.github.com" "libgit2"]]
           @item[@link["http://libtom.org" "LibTomCrypt"]]
           @item[@link["https://bitwiseshiftleft.github.io/sjcl" "SJCL"]]
           @item[@link["https://github.com/alexei/sprintf.js" "Sprintf.JS"]]
           @item[@link["http://www.libssh2.org" "libssh2"]]]
                
@subsection{Backend Support Source Files - @source-url-link["/src/backend"]}
The source for support binaries and libraries are stored under @source-url-link["/src/backend"].
The following Seashell support components live here:

@subsection{Backend Server - @source-url-link["/src/collects"]}
@subsection{Fronted - @source-url-link["/src/frontend"]}

            
