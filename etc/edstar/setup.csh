#+
#  Name:
#     edstar_setup.csh

#  Purpose:
#     Set up EDSTAR by defining the "edstar" command.

#  Type of Module.
#     C shell source file.

#  Invocation:
#     source edstar.csh

#  Notes:
#     This file must be invoked using the C shell "source" command.

#  Authors:
#     RFWS: R.F. Warren-Smith (STARLINK)

#  History:
#     20-DEC-1993 (RFWS):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Define "edstar" to be an alias which (a) sets the variable
#  edstar_key to the value of the optional argument supplied, (b)
#  sources the edstar_start file (which uses this variable to define
#  the EDSTAR_ATTACH_KEY environment variable, then unsets the
#  edstar_key variable and defines an alias for the "emacs" command).
      alias edstar \
         'set edstar_key = "\!*";source $EDSTAR_DIR/start.csh'

#  End of source file.
