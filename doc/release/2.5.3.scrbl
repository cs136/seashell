#lang scribble/manual
@(require "../utils.rkt")

@seashell-title[#:tag "build-2.5.3"]{v.2.5.3}
Summary:
@itemlist[
  @item{The import preprocessor directive is no longer allowed. @issue[385]}
  @item{Uploading an empty file no longer causes an error. @issue[378]}
  @item{When attempting to load Seashell with an invalid/expired login, Seashell will now redirect to a login screen. @issue[376]
  @item{An error no longer occurs when trying to normalize newlines. @issue[367]}
  @item{Fixed being able to edit binary (.o) files for a few seconds after opening. @issue[388]}
  @item{Fixed hard tabs being inserted in some cases, which would mess up formatting in MarkUs. @issue[391]}
  @item{Fixed single quotes in file names causing errors everywhere. @issue[384]}
  @item{Leading spaces are no longer trimmed from input. @issue[394]}
  @item{Ctrl-D now works for sending EOF. @issue[396]}
  @item{Expected output is now printed when a test fails. @issue[398]}
  @item{It is now easier to tell if there is a missing newline at the end of output. @issue[398]}
]
