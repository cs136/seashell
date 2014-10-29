#lang scribble/manual

@(require "../utils.rkt")

@title[#:tag "seashell-getting-started"]{Getting Started}

Welcome to Seashell, a web-interface for completing assignments in CS 136.
Seashell is designed to work as an environment to both develop and test code
written in both the functional language Racket and the imperative language C.
You can create test cases containing test program input, keep it in a separate
folder, and Seashell will run the test cases automatically when you want to
test your programs.  You can even submit to the submission server (Marmoset)
directly from Seashell, as opposed to having Marmoset open separately and
uploading from your computer.

@local-table-of-contents[]

@section{Projects}

In CS 136, projects will be created automatically.  All needed files will be
available, and you can begin working as soon as you open the project.  It is
also possible to create new projects and delete existing ones.

Adding files:  Clicking the "Add file..." button will open a dialog where you
can select a folder for the new file (common, default, tests, <QUESTION
FOLDER>) and enter a name for the file and enter a name for the file.  There is
also an option to upload a file from your computer to the Seashell environment.

@section{Editing Files}

Any file that is edited will be autosaved every second to your personal
account, and as such there is no save button.  This editor features syntax
highlighting for Racket and C, as well as auto-indentation.

NOTE:  Binary files (.o) files will not be displayed in Seashell.  Seashell
will display a "Binary File" message across the editor window if you attempt to
open a binary file.

@section{Running Files}

Racket: Clicking run on a Racket file will run the current Racket file through
the Racket interpreter.

C: Clicking run when on a C file will build, link, and run the C file with the
main function in the directory (there can only be one C file with a main
function per question, otherwise a link error will result in Seashell).

Errors will be displayed in the console beside the editor window.

@section{Programs that Require Input}

If at any point a program requires input to continue, it can be input in the
text box beneath the console.  You can press enter to send the input to your
program.  Notice the EOF (End-Of-File) button.  If that button is clicked, your
program will immediately receive a signal which indicates that the current
input has ended.  If there is any text in the console input textbox, it WILL
NOT be sent.

If you wish to erase all text from the console, there is a "Clear" button on
the console window. 

@section{Testing I/O Programs/Modules}

Some programs that you will be asked to write will use some form of keyboard
input (read in Racket, or scanf in C).  Since having to test your code by
typing your test input into the console every time you run your program is
tedious and very inconvenient, Seashell offers an automated way to test
programs that require input.

For every question in a Seashell project, there is a tests directory where you
can create .in and .expect files.

.in files are used to hold test keyboard input, and .expect files are used to
hold the expected output given some input.

For example, let's assume we are testing a function that takes two ints, and
prints their sum:

test1.in 3 4

test1.expect 7

test2.in 0 0

test2.expect 0

Each .in file shares its name with a .expect file.  This convention must be
followed to ensure that Seashell can properly test your function(s).

To run the tests, beside the "Run" button in Seashell, there is a "Test"
button.  Click the "Test" button to test your program against all of your tests
in the tests directory.  Any information from the tests (which passed and which
failed) will be displayed in the console window.

@section{Submitting to Marmoset}

When you want to submit to Marmoset, you can click the "Submit Question"
button, and choose the Marmoset project to which you wish to submit your source
code.  Seashell can also auto-detect which Marmoset project to submit to.

@section{Settings}

The settings button (wrench) is in the upper-right corner of the screen.  There
are options to change the font size, the editor mode (for those who have
experience with vim or emacs), the width of a tab, and the editor style.

@section{Logging Out}

The Log-out button is the right-most button at the top of the screen.
