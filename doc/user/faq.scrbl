#lang scribble/manual

@(require "../utils.rkt")

@seashell-title[#:tag "seashell-faq"]{Frequently Asked Questions}

@local-table-of-contents[]

@section{What does error code ___ mean?}

See the @secref["seashell-error-codes"] section.

@section{Why can't I delete a question/assignment in Seashell?}

The functionality of question/assignment deletion has been disabled to prevent
students from accidentally deleting their assignment skeletons on Seashell, or
accidentally deleting their work.

@section{I am trying to submit to Marmoset through Seashell, but the Marmoset
projects are not in the list.  Why?}

The Marmoset tests have not been posted yet.  Please wait for your instructors
to post the tests.

@section{How do I submit multiple files to Marmoset?}

Seashell automatically submits all the files pertaining to the question as a
.zip file.

@section{I've tried evaluating Racket expressions in the console, but it is not
accepting them.  Why?}

The console is used for input and output, it is not equivalent to a DrRacket
interactions window.  You cannot evaluate expressions in the console.

@section{An instructor has reported that a Seashell skeleton has been released,
but it's not appearing in my list of Seashell Assignments.  What's going on?}

The skeleton needs a few seconds to copy itself into Seashell.  If your list
does not update automatically, refresh your browser (Ctrl-F5 on Windows,
Command-R on Mac).

@section{When I open up Seashell I only see a blank screen. What do I do?}

There are a few troubleshooting tips for when this occurs:

Check your router is set up to accept websocket connections. There are online
websockets compatibility tests that can verify this for you. A quick Google
search will yield several results.

Certain forms of adware have been known to prevent Seashell from loading. If
you view the Javascript error console (F12 in Google Chrome) when Seashell
fails to load and you see a Javascript error from a script hosted on a strange
website (for example, best-deals-products.com is a common one), then you likely
have some form of adware on your computer. You should run a virus/malware scan
with antivirus software you trust to fix this issue.

