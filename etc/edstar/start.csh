#+
#  Name:
#     edstar_start.csh

#  Purpose:
#     Allow a user to "log in" to use EDSTAR.

#  Type of Module.
#     C shell source file.

#  Invocation:
#     source edstar_start.csh
#     (Normally invoked as part of the "edstar" command.)

#  Notes:
#     This file must be invoked using the C shell "source" command, but
#     should not be used alone as it forms part of a sequence of
#     commands executed by the "edstar" alias (this alias is defined in
#     the edstar_setup.csh file).

#  Authors:
#     RFWS: R.F. Warren-Smith (STARLINK)

#  History:
#     21-DEC-1993 (RFWS):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Translate the environment variable EDSTAR_ATTACH_KEY to see if a
#  previous definition of the "attach key" exists.
      set edstar_oldkey = "`printenv EDSTAR_ATTACH_KEY`"

#  If it does, and no new key definition has been given, then unset the
#  environment variable which identifies the attach key.
      if ( "$edstar_oldkey" != "" && "$edstar_key" == "" ) then
          unsetenv EDSTAR_ATTACH_KEY
      endif

#  If a new attach key has been specified, then define the environment variable
#  which passes its name to the EDSTAR subprocess.
      if ( "$edstar_key" != "" && "$edstar_oldkey" != "$edstar_key" ) then
         setenv EDSTAR_ATTACH_KEY "$edstar_key"
      endif

#  Unset the variables used locally as part of the "edstar" command.
      unset edstar_oldkey
      unset edstar_key

#  Define an alias for the "emacs" command. This command should (a)
#  source the edstar_begin.csh file (this performs initialisation prior
#  to running EDSTAR and also defines the "edstar_cmd" variable to
#  contain a command which either starts a new job or brings an
#  existing one to the foreground), (b) execute the command just
#  defined, (c) clean up by sourcing the edstar_end.csh file (this
#  unsets the variable containing the command and removes the link used
#  to run emacs from the home directory).
       alias emacs 'source $EDSTAR_DIR/edstar.csh'
       alias xemacs 'source $EDSTAR_DIR/xedstar.csh'

#  End of source file.
