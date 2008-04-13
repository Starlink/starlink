#+
#  Name:
#     starlse_setup.csh

#  Purpose:
#     Set up STARLSE by defining the "starlse" command.

#  Type of Module.
#     C shell source file.

#  Invocation:
#     source starlse_setup.csh

#  Notes:
#     This file must be invoked using the C shell "source" command.

#  Authors:
#     RFWS: R.F. Warren-Smith (STARLINK)

#  History:
#     11-JUN-1989 (RFWS):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Define "starlse" to be an alias which (a) sets the variable starlse_key
#  to the value of the optional argument supplied, (b) sources the starlse_start
#  file (which uses this variable to define the STARLSE_ATTACH_KEY environment
#  variable, then unsets the starlse_key variable and defines an alias for the
#  "lse" command).
      alias starlse \
         'set starlse_key = "\!*";source $STARLSE_DIR/starlse_start.csh'

#  End of source file.
