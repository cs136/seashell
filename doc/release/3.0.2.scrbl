#lang scribble/manual
@(require "../utils.rkt")

@seashell-title[#:tag "build-3.0.2"]{v.3.0.2}
Summary:
@itemlist[
  @item{Fix issue with garbage bytes being sent in interactive mode @pr[636].}
  @item{Repair resetting functionality @pr[635].}
  @item{Repair deadlock in offline sync code @pr[634].}
  @item{Cosmetic console layout changes @pr[633] @pr[623].}
  @item{Improved error messages @pr[632] @pr[626].}
  @item{Basic file copy functionality @pr[618].}
]
