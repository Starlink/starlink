{+
{  Name:
{     kappa.icl
{
{  Purpose:
{     Defines KAPPA commands and help for ICL usage.
{
{  Type of Module:
{     ICL procedure
{
{  Invocation:
{     load $KAPPA_DIR/kappa
{
{  Description:
{     This procedure starts KAPPA for use from UNIX ICL by defining
{     the KAPPA commands needed to execute each application or to get
{     help about KAPPA.
{
{  Authors:
{     John Lightfoot (RoE)
{     Tim Jenness    (JACH)
{     {enter_new_authors_here}
{
{  History:
{     199? (JFL):
{       Original
{     1997 June 12 (TJ):
{       Add header. Add all the new commands added since mid-1996
{  Bugs:
{     {note_any_bugs_here}
{
{-
define extinction      $SURF_DIR/surf_mon
define flatfield       $SURF_DIR/surf_mon
define rebin           $SURF_DIR/surf_mon
define reduce_switch   $SURF_DIR/surf_mon
define skydip          $SURF_DIR/surf_mon
define bolrebin        $SURF_DIR/surf_mon
define change_quality  $SURF_DIR/surf_mon
define change_pointing $SURF_DIR/surf_mon
define change_flat     $SURF_DIR/surf_mon
define extract_data    $SURF_DIR/surf_mon
define remsky          $SURF_DIR/surf_mon
define scuover         $SURF_DIR/surf_mon
define scuphot         $SURF_DIR/surf_mon
define intrebin        $SURF_DIR/surf_mon
define scucat          $SURF_DIR/surf_mon
define restore         $SURF_DIR/surf_mon

{ Help system
defhelp extinction      $SURF_HELP
defhelp flatfield       $SURF_HELP
defhelp rebin           $SURF_HELP
defhelp reduce_switch   $SURF_HELP
defhelp skydip          $SURF_HELP
defhelp bolrebin        $SURF_HELP
defhelp change_quality  $SURF_HELP
defhelp change_pointing $SURF_HELP
defhelp change_flat     $SURF_HELP
defhelp extract_data    $SURF_HELP
defhelp remsky          $SURF_HELP
defhelp scuover         $SURF_HELP
defhelp scuphot         $SURF_HELP
defhelp intrebin        $SURF_HELP
defhelp scucat          $SURF_HELP
defhelp restore         $SURF_HELP
defhelp scuhelp         $SURF_HELP
defhelp surf            $SURF_HELP 0

{ Define the full commands for SURF

define surf_extinction      $SURF_DIR/surf_mon extinction
define surf_flatfield       $SURF_DIR/surf_mon flatfield
define surf_rebin           $SURF_DIR/surf_mon rebin
define surf_reduce_switch   $SURF_DIR/surf_mon reduce_switch
define surf_skydip          $SURF_DIR/surf_mon skydip
define surf_bolrebin        $SURF_DIR/surf_mon bolrebin
define surf_change_quality  $SURF_DIR/surf_mon change_quality
define surf_change_pointing $SURF_DIR/surf_mon change_pointing
define surf_change_flat     $SURF_DIR/surf_mon change_flat
define surf_extract_data    $SURF_DIR/surf_mon extract_data
define surf_remsky          $SURF_DIR/surf_mon remsky
define surf_scuover         $SURF_DIR/surf_mon scuover
define surf_scuphot         $SURF_DIR/surf_mon scuphot
define surf_intrebin        $SURF_DIR/surf_mon intrebin
define surf_scucat          $SURF_DIR/surf_mon scucat
define surf_restore         $SURF_DIR/surf_mon restore

{ Announce the SURF commands are now available

print " "
print "   SURF - SCUBA User Reduction Facility"
print "     Commands are now available -- (Version 1.0-0)"
print " "
print "     Type `help kappa` or `scuhelp` for help on SURF commands."
print " "
