{+
{  Name:
{     jcmtdr.icl
{
{  Purpose:
{     Start the JCMTDR package from ICL.
{
{  Type of Module:
{     ICL command list
{
{  Invocation:
{     load $JCMTDR_DIR/jcmtdr
{
{  Description:
{     This procedure starts the JCMTDR package for use from the ICL command
{     language defining the commands needed to execute each application.
{
{     This startup file is for the Unix version of ICL. It does not work in
{     the VMS version of ICL.
{
{  Authors:
{     hme: Horst Meyerdierks (UoE, Starlink)
{     {enter_new_authors_here}
{
{  History:
{     13 May 1994 (hme):
{        Original Version for Specdre.
{     18 May 1994 (hme):
{        Adapted for JCMTDR.
{     {enter_changes_here}
{
{-
{.
{
{  Define help for package.
{
defhelp   jcmtdr $JCMTDR_HELP
{
{  Define commands for each application.
{
define  ae2rd1   $JCMTDR_DIR/jcmtdr
define  ae2rd2   $JCMTDR_DIR/jcmtdr
define  fake     $JCMTDR_DIR/jcmtdr
define  iras_tag $JCMTDR_DIR/jcmtdr
define  jcmtextc $JCMTDR_DIR/jcmtdr
define  makemap  $JCMTDR_DIR/jcmtdr
define  map2mem  $JCMTDR_DIR/jcmtdr
define  map2ts   $JCMTDR_DIR/jcmtdr
define  restore  $JCMTDR_DIR/jcmtdr
define  ts2map   $JCMTDR_DIR/jcmtdr
{
{  Define help for each application.
{
defhelp ae2rd1   $JCMTDR_HELP jcmtdr ae2rd1
defhelp ae2rd2   $JCMTDR_HELP jcmtdr ae2rd2
defhelp fake     $JCMTDR_HELP jcmtdr fake
defhelp iras_tag $JCMTDR_HELP jcmtdr iras_tag
defhelp jcmtextc $JCMTDR_HELP jcmtdr jcmtextc
defhelp makemap  $JCMTDR_HELP jcmtdr makemap
defhelp map2mem  $JCMTDR_HELP jcmtdr map2mem
defhelp map2ts   $JCMTDR_HELP jcmtdr map2ts
defhelp restore  $JCMTDR_HELP jcmtdr restore
defhelp ts2map   $JCMTDR_HELP jcmtdr ts2map
{
{ Show that the Jcmtdr commands are now available.
{
print ' '
print ' ------------- Initialised for JCMTDR ------------'
print '            Version 1.2  10 January 1995'
print ' '
print '            Type "help jcmtdr"  for help'
print ' '
{
{ end
{
