{+          
{  Name:
{     polpack.icl

{  Purpose:
{     Set up command names in ICL for the POLPACK package.

{  Language:
{     ADAM ICL

{  Type of Module:
{     ICL file.

{  Arguments:

{  Invocation:
{     load polpack.icl

{  Description:
{     This procedure defines the command names for each POLPACK command.

{  Copyright:
{     Copyright (C) 1998 Central Laboratory of the Research Councils
 
{  Authors:
{     DSB: D.S. Berry (STARLINK)
{     {enter_new_authors_here}

{  History:
{     29-JUN-1997 (DSB):
{        Original version, based on ircampack.icl.
{     {enter_changes_here}

{  Bugs:

{-

{  Basic command definitions.
define polexp            $POLPACK_DIR/polpack_mon
define polext            $POLPACK_DIR/polpack_mon
define polimp            $POLPACK_DIR/polpack_mon
define polcal            $POLPACK_DIR/polpack_mon
define polmap            $POLPACK_DIR/polpack_mon
define polka             $POLPACK_DIR/polpack_mon
define polvec            $POLPACK_DIR/polpack_mon
define polbin            $POLPACK_DIR/polpack_mon
define polsim            $POLPACK_DIR/polpack_mon
define polplot           $POLPACK_DIR/polpack_mon
define polimage          $POLPACK_DIR/polpack_mon
defstring polh(elp)      !$POLPACK_DIR/polhelp

{  Full command definitions.
define pol_polexp        $POLPACK_DIR/polpack_mon polexp
define pol_polext        $POLPACK_DIR/polpack_mon polext
define pol_polimp        $POLPACK_DIR/polpack_mon polimp
define pol_polcal        $POLPACK_DIR/polpack_mon polcal
define pol_polmap        $POLPACK_DIR/polpack_mon polmap
define pol_polka         $POLPACK_DIR/polpack_mon polka
define pol_polvec        $POLPACK_DIR/polpack_mon polvec
define pol_polbin        $POLPACK_DIR/polpack_mon polbin
define pol_polsim        $POLPACK_DIR/polpack_mon polsim
define pol_polplot       $POLPACK_DIR/polpack_mon polplot
define pol_polimage      $POLPACK_DIR/polpack_mon polimage
defstring pol_polh(elp)  !$POLPACK_DIR/polhelp

{ Define POLPACK's help from ICL.  Note that polhelp points to help polpack
{ because polhelp doesn't work from UNIX ICL.
defhelp  polhelp         $POLPACK_HELP
defhelp  polmap          $POLPACK_HELP
defhelp  polka           $POLPACK_HELP
defhelp  polcal          $POLPACK_HELP
defhelp  polexp          $POLPACK_HELP
defhelp  polext          $POLPACK_HELP
defhelp  polimp          $POLPACK_HELP
defhelp  polvec          $POLPACK_HELP
defhelp  polbin          $POLPACK_HELP
defhelp  polsim          $POLPACK_HELP
defhelp  polplot         $POLPACK_HELP
defhelp  polimage        $POLPACK_HELP

{
{  Announce that the POLPACK commands are available.
{
print ""
print "   POLPACK commands are now available -- (Version PKG_VERS)"
print " "
print "   Type `polhelp' for help on POLPACK commands"
print '   Type "showme sun223" to browse the hypertext documentation'
print " "
{
{ end
