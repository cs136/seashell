#lang scribble/manual

@title[#:version @vector-ref[(current-command-line-arguments) 0] #:tag "main" #:style 'multi-page]{Seashell: An Online Integrated Development Environment}
@author[@author+email["Edward Lee" "e45lee@uwaterloo.ca"]
        @author+email["Jacob Pollack" "jpollack@uwaterloo.ca"]
        @author+email["Kaleb Alway" "kpalway@uwaterloo.ca"]
        @author+email["Bryan Coutts" "b2coutts@uwaterloo.ca"]
        @author+email["Graham Cooper" "grcooper@uwaterloo.ca"]]

@link["https://github.com/cs136/seashell" "Seashell"]
is an online integrated development environment, primarily
used at the @(link "http://www.uwaterloo.ca/" "University of Waterloo")
as the main development environment for students taking
@link["https://www.student.cs.uwaterloo.ca/~cs136/" "CS 136"].

The University of Waterloo's main Seashell instance can be found
@link["https://www.student.cs.uwaterloo.ca/seashell" "here"].

@margin-note{This documentation is current for version @vector-ref[(current-command-line-arguments) 0], and only
             describes API calls present in API version @vector-ref[(current-command-line-arguments) 1].}

@table-of-contents[]

@; ------------------------------------------------------------------
@include-section["user/main.scrbl"]
@include-section["administrative/main.scrbl"]
@include-section["developer/main.scrbl"]
@include-section["release/main.scrbl"]

@index-section[]
