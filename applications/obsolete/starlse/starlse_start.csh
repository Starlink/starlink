#+
#  Name:
#     starlse_start.csh

#  Purpose:
#     Allow a user to "log in" to use STARLSE.

#  Type of Module.
#     C shell source file.

#  Invocation:
#     source starlse_start.csh
#     (Normally invoked as part of the "starlse" command.)

#  Notes:
#     This file must be invoked using the C shell "source" command, but should
#     not be used alone as it forms part of a sequence of commands executed
#     by the "starlse" alias (this alias is defined in the starlse_setup.csh
#     file).

#  Authors:
#     RFWS: R.F. Warren-Smith (STARLINK)

#  History:
#     11-JUN-1989 (RFWS):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Translate the environment variable STARLSE_ATTACH_KEY to see if a previous
#  definition of the "attach key" exists.
      set starlse_oldkey = "`printenv STARLSE_ATTACH_KEY`"

#  If it does, and no new key definition has been given, then unset the
#  environment variable which identifies the attach key.
      if ( "$starlse_oldkey" != "" && "$starlse_key" == "" ) then
          unsetenv STARLSE_ATTACH_KEY
      endif

#  If a new attach key has been specified, then define the environment variable
#  which passes its name to the STARLSE subprocess.
      if ( "$starlse_key" != "" && "$starlse_oldkey" != "$starlse_key" ) then
         setenv STARLSE_ATTACH_KEY "$starlse_key"
      endif

#  Unset the variables used locally as part of the "starlse" command.
      unset starlse_oldkey
      unset starlse_key

#  Define an alias for the "lse" command. This command should (a) source the
#  starlse_begin.csh file (this performs initialisation prior to running STARLSE
#  and also defines the "starlse_cmd" variable to contain a command which either
#  starts a new job or brings an existing one to the foreground), (b) execute
#  the command just defined, (c) clean up by sourcing the starlse_end.csh file
#  (this unsets the variable containing the command and removes the link used
#  to run lse from the home directory).
      alias lse \
'source $STARLSE_DIR/starlse_begin.csh;$starlse_cmd \!*;source $STARLSE_DIR/starlse_end.csh'

#  End of source file.
