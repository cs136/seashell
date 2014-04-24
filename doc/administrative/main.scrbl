#lang scribble/manual

@title[#:tag "administrative"]{Administrator Documentation}

This section provides documentation relevant to administrators of Seashell.
Consult the @secref["user"] for general end-user documentation.

@local-table-of-contents[]

@section{Installing Seashell}
Consult @secref["seashell-download"] and @secref["seashell-build"] for installation
instructions.
@margin-note{
  @bold{University of Waterloo only:} Seashell's source repository has
  already been cloned into @tt{~cs136/seashell_src}, its build
  directory has been setup as @tt{~cs136/seashell_build}, and
  its install directory is located at @tt{~cs136/seashell}.}

@section{Configuring Seashell}
The following files are used to configure Seashell's behaviour:
@tabular[#:style 'boxed
         `((@,bold{File} @,bold{Description})
           (@,tt{@secref["seashell-main-config"]}
            "Main backend configuration file.")
           (@,tt{@secref["seashell-hosts-file"]}
            "Known Seashell host keys."))]

@subsection[#:tag "seashell-main-config"]{etc/seashell_config.rkt}
@tt{etc/seashell_config.rkt} is the main file used for configuring
the Seashell backend server.
@subsubsection{Organization}
@tt{etc/seashell_config.rkt} is organized as a @link["http://docs.racket-lang.org/reference/reader.html" "S-expression"]
list of key-value pairs.  For example:
@racketblock[
((ssl-key "/etc/seashell_keys/seashell.key")
 (ssl-cert "/etc/seashell_keys/seashell.pem")
 (host ("ubuntu1204-002.hosts.seashell.student.cs.uwaterloo.ca"
        "ubuntu1204-004.hosts.seashell.student.cs.uwaterloo.ca"
        "ubuntu1204-006.hosts.seashell.student.cs.uwaterloo.ca")))]

@subsubsection{Options}
The following configuration options are stored in @tt{etc/seashell_config.rkt}
 @tabular[#:style 'boxed
          #:sep @hspace[1]
          `((@,bold{Option} @,bold{Possible Values} @,bold{Default Value} @,bold{Description})
            (@,tt{debug} @,racket[(or/c #t #f)] @,racket[#f] "Run Seashell with debug log output.")
            (@,tt{host} @,racket[(listof string?)] @,racket[("localhost")] "Backend runner hosts.")
            (@,tt{ssl-key} @,racket[path-string?] @,racket["etc/keys/server-key.pem"] "Location of SSL private key.")
            (@,tt{ssl-cert} @,racket[path-string?] @,racket["etc/keys/server.pem"] "Location of SSL certificate."))]

@subsection[#:tag "seashell-hosts-file"]{etc/seashell_hosts}
This file holds the SSH host keys for the hosts Seashell connects to.  Consult OpenSSH documentation
for the format of this file.
