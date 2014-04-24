#lang scribble/manual

@(require "../utils.rkt")

@title[#:tag "seashell-getting-started"]{Getting Started}

No matter what you want to do with Seashell, it's best to start by writing a hello world program. 
This chapter guides you through the basics and introduces you to the Seashell development 
environment.

@section{Launching a Seashell Instance}
@nested[#:style 'boxed]{@bold{University of Waterloo only:} The following instructions only apply to the Seashell instance
deployed at the University of Waterloo.}
To launch a Seashell instance open your favorite web browser and navigate to @link["https://www.student.cs.uwaterloo.ca/seashell" "www.student.cs.uwaterloo.ca/seashell"]. 
You will be required to authenticate with your @link["http://www.watiam.uwaterloo.ca" "WatIAM"] credentials.

@section{Creating a Project}

Click on the @bold["Create"] button in the center of your screen. You will be prompted 
in a pop-up to specify the new projects' name, enter @cpp{Hello World Program} in the input field then 
click on the @bold["Create"] button in the pop-up window. The new project will be created and 
immediately opened.

@section{Creating a File}

Since the new project is empty there will be a @bold["Create"] button in the center of your screen, 
click on it. You will be prompted in a pop-up to specify the name of the file, enter @tt{hello_world.c} 
in the input field then click on the @bold["Create"] button in the pop-up window. The new file 
will be immediately created and opened.

If you were in another project and there were one or more files already created, click on the 
@bold["New File"] link in the left sidebar to create a new file.

@section{Writing C Code}

Open @tt{hello_world.c} by selecting it from the sidebar on the left side of your screen. Add the 
following include directive and definition for @cpp{main} to your file:

@verbatim[#:indent 2]{
#include <stdio.h>

int main(void) {
  printf("Hello, World!\n");
}
}

To indent a single line of code use the @bold["tab"] key. If you are indenting one or more 
lines of code then select all of the lines you want to indent and use the @bold["tab"] key.

@section{Compiling and Running a Program}

Open the file that has the definition of @cpp{main} (@tt{hello_world.c}).

@itemlist[
  @item{Compiling: select the gear icon.}
  @item{Running: select the triangle icon that points to the right.}
]

Both icons can be found above the text box and below the Seashell navigation bar. Hover over the 
icons for more information.

@section{Program Output and Compiler Errors}

The Seashell interactions window is located at the bottom of the screen, under the text box. In addition to other meta data, program output and compiler errors will be displayed there.

@section{Next Steps}

Congratulations, you have created your first program in Seashell! See the advanced guides for a 
further in depth walkthrough of the functionality that Seashell offers.
