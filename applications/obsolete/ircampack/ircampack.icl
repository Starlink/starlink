{+
{  Name:
{     ircampack.icl

{  Purpose:
{     Set up command names in ICL for the IRCAMPACK package.

{  Language:
{     ADAM ICL

{  Type of Module:
{     ICL file.

{  Arguments:

{  Invocation:
{     load ircampack.icl

{  Description:
{     This procedure defines the command names for each IRCAMPACK command.

{  Authors:
{     DSB: D.S. Berry (STARLINK)
{     BLY: M.J. Bly (Starlink, RAL)
{     {enter_new_authors_here}

{  History:
{     07-OCT-1996 (BLY):
{        New, based on ircampack.csh, for port of IRCAMPACK to Linux.
{     {enter_changes_here}

{  Bugs:

{-

{  define main package help

defhelp irchelp $IRCAMPACK_HELP 0
defstring ircampackhelp help ircampack
defstring irchelp help irchelp

{  Basic command definitions.

define calpol            $IRCAMPACK_DIR/ircampack_mon calpol
define irc_calpol        $IRCAMPACK_DIR/ircampack_mon calpol

define check_ndfname     $IRCAMPACK_DIR/ircampack_mon check_ndfname
define irc_check_ndfname $IRCAMPACK_DIR/ircampack_mon check_ndfname

define errclip           $IRCAMPACK_DIR/ircampack_mon errclip
define irc_errclip       $IRCAMPACK_DIR/ircampack_mon errclip

define ircamset          $IRCAMPACK_DIR/ircampack_mon ircamset
define irc_ircamset      $IRCAMPACK_DIR/ircampack_mon ircamset

define segment           $IRCAMPACK_DIR/ircampack_mon segment
define irc_segment       $IRCAMPACK_DIR/ircampack_mon segment

define tnorm             $IRCAMPACK_DIR/ircampack_mon tnorm
define irc_tnorm         $IRCAMPACK_DIR/ircampack_mon tnorm

define vecplot           $IRCAMPACK_DIR/ircampack_mon vecplot
define irc_vecplot       $IRCAMPACK_DIR/ircampack_mon vecplot

defstring awkseg         sh $IRCAMPACK_DIR/awkseg
defstring irc_awkseg     sh $IRCAMPACK_DIR/awkseg
defstring awkvec         sh $IRCAMPACK_DIR/awkvec
defstring irc_awkvec     sh $IRCAMPACK_DIR/awkvec
defstring polcal         sh $IRCAMPACK_DIR/polcal
defstring irc_polcal     sh $IRCAMPACK_DIR/polcal
defstring polmapc        sh $IRCAMPACK_DIR/polmapc
defstring irc_polmapc    sh $IRCAMPACK_DIR/polmapc
defstring polmapd        sh $IRCAMPACK_DIR/polmapd
defstring irc_polmapd    sh $IRCAMPACK_DIR/polmapd
defstring polsky         sh $IRCAMPACK_DIR/polsky
defstring irc_polsky     sh $IRCAMPACK_DIR/polsky
defstring polsmooth      sh $IRCAMPACK_DIR/polsmooth
defstring irc_polsmooth  sh $IRCAMPACK_DIR/polsmooth
defstring polzap         sh $IRCAMPACK_DIR/polzap
defstring irc_polzap     sh $IRCAMPACK_DIR/polzap

{
{  Announce that the IRCAMPACK commands are available.
{
print ""
print "   IRCAMPACK commands are now available -- (Version PKG_VERS)"
print " "
print "   Type `irchelp' for help on IRCAMPACK commands"
print " "
{
{ end
