#lang scribble/manual
@(require "../utils.rkt")

@seashell-title[#:tag "build-2.5.8"]{v.2.5.8}
Summary:
@itemlist[
  @item{File snapshots history backend added (without user interface). @pr[557]}
  @item{ASAN error message parsing/cleanup. @pr[563]}
  @item{Flatten common/ folder before submitting to Marmoset. @pr[571]}
  @item{Restore missing files from whitelist project skeletons. @pr[572]}
  @item{Add ability to make read-only files with a comment in the code. @pr[573]}
  @item{Secure the storage and transfer of whitelist project skeletons. @pr[574] @pr[584]}
  @item{Some installation documentation tweaks. @pr[579] @pr[581] @pr[583]}
  @item{No longer submits .history, .project-settings files to Marmoset. @pr[585]}
]
