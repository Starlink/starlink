#+
#  Name:
#     starlse_begin.csh

#  Purpose:
#     Perform initialisation and define a command to run STARLSE.

#  Type of Module.
#     C shell source file.

#  Invocation:
#     source starlse_begin.csh
#     (Normally invoked as part of the "lse" command.)

#  Notes:
#     This file must be invoked using the C shell "source" command, but should
#     not be used alone as it form part of a sequence of commands executed
#     by the "lse" alias (this alias is defined in the starlse_start.csh file).

#  Authors:
#     RFWS: R.F. Warren-Smith (STARLINK)

#  History:
#     11-JUN-1989 (RFWS):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Define an environment variable to identify the communications file used
#  to pass information about the working directory of the current process
#  to the STARLSE subprocess. Name the file after the current process ID so
#  that it is unique at any one time.
      setenv STARLSE_COMFILE ~/.Starlse_$$.tmp

#  Write the current working directory name to the communications file (note
#  this file is deleted by the STARLSE process as soon as this information
#  has been read).
      pwd > $STARLSE_COMFILE

#  Search the list of current processes to see if there is one already running
#  LSE via a link in the home directory named after the current process (this
#  allows us to distinguish the job started from this process amongst several
#  possible lse jobs). Exclude the process created by this command itself (to
#  run grep to find LSE).
     set starlse_cmd  = "`ps | grep ~/.Starlse_$$.ln | grep -v grep`"

#  If a suitable process does not exist, then notify the user that a new process
#  is being started.
     if( "$starlse_cmd" == "" ) then
        echo Starting STARLSE job

#  Put a soft link into the home directory to point at the lse binary. Define
#  a command to run LSE via this link, specifying the STARLSE environment and
#  section files (the name of the link is based on the current process ID to
#  allow the resulting job to be re-identified later).
        ln -s /usr/bin/lse ~/.Starlse_$$.ln
        set starlse_cmd = '~/.Starlse_'$$'.ln -e $STARLSE_DIR/starlse.env -s $STARLSE_DIR/starlse.tpu_section'

#  If an LSE process already exists, then define a command to bring it to the
#  foreground.
     else
        set starlse_cmd = ~/.Starlse_$$.ln
        set starlse_cmd = %$starlse_cmd
     endif

#  End of source file.
