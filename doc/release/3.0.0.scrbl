#lang scribble/manual
@(require "../utils.rkt")

@seashell-title[#:tag "build-3.0.0"]{v.3.0.0}
Summary:
@itemlist[
  @item{Seashell offline mode released.}
  @item{Now able to compile and run C code while disconnected from the websocket connection.}
  @item{Local storage used to store user files persistently offline.}
  @item{Local file storage synchronized with backend filesystem when connection is reestablished.}
  @item{Able to run tests while offline.}
  @item{seashell-cli can generate LLVM bytecode files compatible with Seashell (to be used in place of object files).}
  @item{Integrate the seashell-clang-js library (Offline compilation depends on this repo being built and installed correctly alongside Seashell).}
]
