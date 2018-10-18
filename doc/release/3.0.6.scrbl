#lang scribble/manual
@(require "../utils.rkt")

@seashell-title[#:tag "build-3.0.6"]{v.3.0.6}
Summary:
@itemlist[
  @item{Update supported browsers again.}
  @item{Remove a few hard-coded instances of cs136.}
  @item{Hide offline mode altogether.}
  @item{Allow linking against custom glibc version to fix a memory leak that wasn't being reported.}
  @item{Make going to frontend.html directly redirect to login page.}
]
