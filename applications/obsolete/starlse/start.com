#!/bin/csh -f
#+
#  Name:
#     starlse_start

#  Purpose:
#     Perform initialisation for the STARLSE system.

#  Type of Module.
#     C shell source file.

#  Invocation:
#     source starlse_start

#  Parameters:
#     None.

#  Prior Requirements:
#     The alias "starlse" should normally be defined to source this script.

#  Authors:
#     RFWS: R.F. Warren-Smith (STARLINK)

#  History:
#     10-JUN-1989 (RFWS):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

# Define an alias to run the STARLSE attach procedure.
#      echo -n alias lse \'eval '`attach`'\'
       echo -n alias lse lse

# Translate the environment variable STARLSE1_ATTACH_KEY to see if a previous
# definition of the "attach key" still exists.
      set key = "`printenv STARLSE1_ATTACH_KEY`"

# If it does, then unset the environment variable and remove the associated key
# definition.
      if ( key != "" && "$1" == "" ) then
          echo -n ';'unsetenv STARLSE1_ATTACH_KEY
#         delete/key/nolog 'key'
      endif

# If a new attach key has been specified as the first argument, then define
# the specified key to run the STARLSE attach procedure. Also define the
# environment variable which holds the name of the key.
      if ( "$1" != "" ) then
         echo -n ';'setenv STARLSE1_ATTACH_KEY "$1"
#         define/key/noecho/terminate/nolog 'key' "starlse_attach"
      endif

      echo ""
