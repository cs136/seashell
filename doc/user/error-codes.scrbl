#lang scribble/manual

@(require "../utils.rkt")

@seashell-title[#:tag "seashell-error-codes"]{Common Error Codes}

This document outlines the meaning of several commonly-encountered error codes
in Seashell.

@local-table-of-contents[]

@section{Code 0 - Successful Completion}
 
This is NOT an error code, the one exception among all other codes you might
encounter.  If a program returns this code, then your program has not crashed
and successfully completed (ie. reached the end of main).
 
@section{Error code 1 - General Error}
 
This is a general error code that programs can return to indicate that something
went wrong, but not as an indication of what specifically went wrong.  This
error code shouldn't happen, unless you return 1 from main, or call exit(1) at
any point in your program.  If you obtain this error (and don't satisfy the
conditions above), reload Seashell and try running your code again.

@section{Error code 134 - Program Abort}
 
This code appears when an assertion fails.
 
@section{Error code 136 - Erroneous Arithmetic Operation}
 
Some error occurred when the computer attempted some form of arithmetic
operation.  This is likely caused by:
- Division by zero
 
@section{Error code 139 - Segmentation Fault}
 
Such an error is caused by some form of invalid memory access.  This could be to
access memory that the program does not have access to, or can also be caused by
running out of memory.  If you have received this error:
- Your program/function is likely recursing infinitely, and thus running out of
  memory. 
- You have attempted to dereference/access an invalid pointer address
- Dereferencing NULL
- Attempting to access memory that was not returned through malloc
- Accessing memory that has been free-d

@section{Error code 255 - Program Timed Out}

Your program took too long to run, and was terminated by Seashell.
