{+
{  Name:
{     surf.icl
{
{  Purpose:
{     Defines SURF commands and help for ICL usage.
{
{  Type of Module:
{     ICL procedure
{
{  Invocation:
{     load $SURF_DIR/surf
{
{  Description:
{     This procedure starts SURF for use from UNIX ICL by defining
{     the SURF commands needed to execute each application or to get
{     help about SURF.
{
{  Authors:
{     John Lightfoot (RoE)
{     Tim Jenness    (JACH)
{     Martin Bly (Starlink, RAL)
{     {enter_new_authors_here}
{
{  Authors:
{    Tim Jenness (JAC)
{
{  Copyright:
{     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
{     Research Council. All Rights Reserved.
{
{  History:
{     199? (JFL):
{       Original
{     1997 June 12 (TIMJ):
{       Add header. Add all the new commands added since mid-1996
{     22-JUN-1997 (BLY):
{        Changed KAPPA/kappa references to SURF/surf.
{     12-NOV-1997 (TIMJ):
{        Add new commands for release v1.1
{        extract_flat, scuclip, despike, despike2, scan_rlb
{     15-Jun-1998 (TIMJ):
{        Add commands for releas v1.2
{          CALCSKY, SCUBA2MEM, SCUMAKEWT
{     20-Nov-1998 (TIMJ):
{        Add REDUCE_NOISE
{     11-Jan-1999 (TIMJ):
{        Add ADD_DBM
{     08-Mar-1999 (TIMJ):
{        Add REMIP
{
{  Bugs:
{     {note_any_bugs_here}
{
{-

defstring scuhelp help surf

define add_dbm         $SURF_DIR/surf_mon
define bolrebin        $SURF_DIR/surf_mon
define calcsky         $SURF_DIR/surf_mon
define change_quality  $SURF_DIR/surf_mon
define change_pointing $SURF_DIR/surf_mon
define change_flat     $SURF_DIR/surf_mon
define despike         $SURF_DIR/surf_mon
define despike2        $SURF_DIR/surf_mon
define extinction      $SURF_DIR/surf_mon
define extract_data    $SURF_DIR/surf_mon
define extract_flat    $SURF_DIR/surf_mon
define flatfield       $SURF_DIR/surf_mon
define intrebin        $SURF_DIR/surf_mon
define rebin           $SURF_DIR/surf_mon
define reduce_noise    $SURF_DIR/surf_mon
define reduce_switch   $SURF_DIR/surf_mon
define remip           $SURF_DIR/surf_mon
define remsky          $SURF_DIR/surf_mon
define restore         $SURF_DIR/surf_mon
define scan_rlb        $SURF_DIR/surf_mon
define scuba2mem       $SURF_DIR/surf_mon
define scucat          $SURF_DIR/surf_mon
define scuclip         $SURF_DIR/surf_mon
define scuover         $SURF_DIR/surf_mon
define scuphot         $SURF_DIR/surf_mon
define scumakewt       $SURF_DIR/surf_mon
define skydip          $SURF_DIR/surf_mon

{ Help system
defhelp surf            $SURF_HELP 0
defhelp bolrebin        $SURF_HELP
defhelp calcsky         $SURF_HELP
defhelp change_flat     $SURF_HELP
defhelp change_pointing $SURF_HELP
defhelp change_quality  $SURF_HELP
defhelp despike         $SURF_HELP
defhelp despike2        $SURF_HELP
defhelp extinction      $SURF_HELP
defhelp extract_data    $SURF_HELP
defhelp extract_flat    $SURF_HELP
defhelp flatfield       $SURF_HELP
defhelp intrebin        $SURF_HELP
defhelp rebin           $SURF_HELP
defhelp reduce_noise    $SURF_HELP
defhelp reduce_switch   $SURF_HELP
defhelp remip           $SURF_HELP
defhelp remsky          $SURF_HELP
defhelp restore         $SURF_HELP
defhelp scan_rlb        $SURF_HELP
defhelp scuba2mem       $SURF_HELP
defhelp scucat          $SURF_HELP
defhelp scuclip         $SURF_HELP
defhelp scuhelp         $SURF_HELP
defhelp scumakewt       $SURF_HELP
defhelp scuover         $SURF_HELP
defhelp scuphot         $SURF_HELP
defhelp skydip          $SURF_HELP

{ Define the full commands for SURF

define surf_add_dbm         $SURF_DIR/surf_mon add_dbm
define surf_bolrebin        $SURF_DIR/surf_mon bolrebin
define surf_calcsky         $SURF_DIR/surf_mon surf_calcsky
define surf_change_flat     $SURF_DIR/surf_mon change_flat
define surf_change_pointing $SURF_DIR/surf_mon change_pointing
define surf_change_quality  $SURF_DIR/surf_mon change_quality
define surf_despike         $SURF_DIR/surf_mon despike
define surf_despike2        $SURF_DIR/surf_mon despike2
define surf_extinction      $SURF_DIR/surf_mon extinction
define surf_extract_data    $SURF_DIR/surf_mon extract_data
define surf_extract_flat    $SURF_DIR/surF_mon extract_flat
define surf_flatfield       $SURF_DIR/surf_mon flatfield
define surf_intrebin        $SURF_DIR/surf_mon intrebin
define surf_rebin           $SURF_DIR/surf_mon rebin
define surf_reduce_noise    $SURF_DIR/surf_mon reduce_noise
define surf_reduce_switch   $SURF_DIR/surf_mon reduce_switch
define surf_remip           $SURF_DIR/surf_mon remip
define surf_remsky          $SURF_DIR/surf_mon remsky
define surf_restore         $SURF_DIR/surf_mon restore
define surf_scan_rlb        $SURF_DIR/surf_mon scan_rlb
define surf_scuba2mem       $SURF_DIR/surf_mon scuba2mem
define surf_scucat          $SURF_DIR/surf_mon scucat
define surf_scuclip         $SURF_DIR/surf_mon scuclip
define surf_scumakewt       $SURF_DIR/surf_mon scumakewt
define surf_scuover         $SURF_DIR/surf_mon scuover
define surf_scuphot         $SURF_DIR/surf_mon scuphot
define surf_skydip          $SURF_DIR/surf_mon skydip

{ Announce the SURF commands are now available

print " "
print "   SURF - SCUBA User Reduction Facility"
print "     Commands are now available -- (Version PKG_VERS)"
print " "
print "     Type `help surf` or `scuhelp` for help on SURF commands."
print " "
