#+
#  Name:
#     starlse_end.csh

#  Purpose:
#     Clean up after running STARLSE.

#  Type of Module.
#     C shell source file.

#  Invocation:
#     source starlse_end.csh
#     (Normally invoked as part of the "lse" command.)

#  Notes:
#     This file must be invoked using the C shell "source" command, but should
#     not be used alone as it form part of a sequence of commands executed
#     by the "lse" alias (this alias is defined in the starlse_start.csh file).

#  Authors:
#     RFWS: R.F. Warren-Smith (STARLINK)

#  History:
#     16-JUN-1989 (RFWS):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Unset the variable used to hold the command for running STARLSE.
      unset starlse_cmd

#  Remove the home directory link used to run the lse binary.
      rm ~/.Starlse_$$.ln

#  End of source file.
