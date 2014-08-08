#lang scribble/manual

@(require "../utils.rkt")

@title[#:tag "seashell-testing-programs"]{Testing Your Programs}

Seashell has features to help you create automated test suites for the
programs you write.

@section{Writing Tests}
When you create a new project in Seashell, a @tt{tests} folder is created
by default. This folder is where you will need to put all of your test
files.

In the Seashell environment, a test consists of either one or two files.
If we were to create a test called @tt{my_test}, we would need to have a
file called @tt{my_test.in}, and optionally a file called
@tt{my_test.expect}.
Seashell will only be able to detect your test if it has an associated
.in file.

@subsection{Input (.in) Files}

Every test @bold{must} have a .in associated with it. The contents of
your .in file are used as input to your program when the test is run.

For example, consider a program that takes in pairs of integers until
end of file, and outputs their pairwise sums. To test this, our .in file
could look something like the following:

@tt{my_test.in}
@verbatim{
2 4
0 0
-5 5
-2 -2
}

@subsection{Expect (.expect) Files}

Creating a .expect file associated with your test is optional, but it
can be very useful. If you
choose to create a .expect file, the output of your program from
running the test will be compared against the contents of your
.expect file.

Note that your .expect file @bold{must} have the same name as your .in
file. For example, if we want to associate a .expect file with
@tt{my_test.in} as described above, we must name it @tt{my_test.expect}.

Consider our program described above, and the @tt{my_test.in} file we
created. An associated .expect file would be the following:

@tt{my_test.expect}
@verbatim{
6
0
0
-4
}

@section{Running Tests}

To run the tests you have created, click on the grey "Run With Tests"
button (located beside the normal "Run" button). A popup window will
appear listing all of the tests you have created. Select the tests you
would like to run, and press "Run".

@section{Interpreting Results}

Running your program with tests will populate the "Tests" tab in
Seashell.

For tests that have an associated .expect file, the result will be
either pass or fail. If a test fails, the difference between your
.expect file and your program's output will be displayed, including
line numbers. Differences will be highlighted in red.

For tests that do not have an associated .expect file, Seashell will
simply display the full output of your program so you can examine it
yourself.

