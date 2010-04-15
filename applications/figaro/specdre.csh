#+
#  Name:
#     specdre.csh
#
#  Purpose:
#     Start the Specdre package from the C shell.
#
#  Type of Module:
#     C shell command list
#
#  Invocation:
#     source specdre.csh
#
#  Description:
#     This procedure used to start the Specdre package for use from the
#     C shell by defining the aliases needed to execute each application.
#     Since Specdre has been merged with Figaro, this procedure now tells
#     the user to invoke Figaro.
#
#  Authors:
#     HME: Horst Meyerdierks (UoE, Starlink)
#     ACC: Anne Charles (RAL)
#     {enter_new_authors_here}
#
#  History:
#     23-JUN-1992 (HME):
#        Original Version
#     14-JUL-1992 (HME):
#        Switch from aliases to path change.
#     16 May 1993 (hme):
#        Use $SPECDRE_DIR.
#     10 Dec 1997 (ACC):
#        Tell user to invoke Figaro in order to use Specdre commands.
#     {enter_changes_here}
#
#-
#.
echo " "
echo "        Specdre has been merged with Figaro"
echo " "
echo "  In order to use the Specdre commands, use Figaro"
echo " "
echo "        "Type \"fighelp specdre\" for help
echo " "
#
# end
#



