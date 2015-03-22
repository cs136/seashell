#lang scribble/manual

@(require "../utils.rkt")

@title[#:tag "seashell-errors"]{Seashell Common Errors}

Below are some errors that may occur in Seashell, reasons why they may be
occurring, and possible options to fix the error.

@verbatim|{
Internal server error: subprocess: fork failed
  system error: Cannot allocate memory; errno=12.
}|

This error can occur when one or more of the linux.student.cs servers is
experiencing high load and has run out of memory. If only one of the servers
is down, you may be able to get rid of this error by using Seashell's restart
button to start a new connection.

@verbatim|{
Could not submit project - marmoset_submit returned 1: (Submitting project ____ for CS136, ______ ____ for user: ______

Request failed: java.sql.SQLException: null, message from server: "Deadlock found when trying to get lock; try restarting transaction"

) ()
}|

This error seems to occur when more than one user attempts to submit to
Marmoset at the exact same time. Wait a couple of minutes then try again, and
this error should be gone.

@verbatim|{
503 Service Temporarily Unavailable
}|

Any error message containing the above string is probably caused by temporary
downtime on the www.student.cs webservers. Check if you are able to access
other websites on that domain. If they are unresponsive or loading very slowly
as well, you will have to wait until the webserver becomes responsive again.
