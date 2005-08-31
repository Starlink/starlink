#+
#  Name:
#     sdthelp

#  Purpose:
#     Display "man page"-like documentation for an SDT command.

#  Type of Module:
#     Shell commands (to be included in a parent shell script).

#  Description:
#     This file comprises a sequence of commands which may be invoked from
#     within a shell script (using the . command - see below). It checks
#     whether the sole command line argument is "-h" (for help). If not,
#     it takes no action. Otherwise, it displays man page style help output in
#     the form of the prologue from the start of the parent shell script, on
#     the standard output. Comment characters are stripped from the prologue,
#     which is also piped through "more".
#
#     If help information is supplied, an "exit" command is then executed to
#     supress any subsequent operations that the parent script might perform.

#  Invocation:
#     . ${SDT_DIR}/sdthelp.sh

#  Parameters:
#     None. The command line arguments ${0} and ${*} of the parent script
#     are examined by this file.

#  Examples:
#     . ${SDT_DIR}/sdthelp.sh
#        Inserting this line at the start of a shell script which contains
#        prologue information allows the "-h" flag to be specified to request
#        help information.

#  Copyright:
#     Copyright (C) 1993 Science & Engineering Research Council

#  Authors:
#     RFWS: R.F. Warren-Smith (STARLINK, RAL)
#     {enter_new_authors_here}

#  History:
#     14-JUN-1994 (RFWS):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Test if the "-h" flag was given as the sole argument.
      if test "${*}" = "-h"; then

#  If so, run awk on the parent script to extract those lines comprising
#  the prologue. Use sed to strip off comment characters and pipe the result
#  through more.
         awk '{if($1=="#-")p=0;if(p)print $0;if($0=="#+"){p=1;print""}}' ${0} \
         | sed -e 's/^#/ /' -e 's/^  //' \
         | more

#  If help information was supplied, abort the parent script.
         exit
      fi

#  End of shell commands.
