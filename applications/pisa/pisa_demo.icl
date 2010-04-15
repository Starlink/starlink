{+
{  Name:
{     pisa_demo

{  Purpose:
{     Script to demostrate PISA functions.

{  Language:
{     ICL

{  Invocation:
{     load pisa_demo; pisa_demo device

{  Authors:
{     PDRAPER: Peter Draper (STARLINK)
{     {enter_new_authors_here}

{  History:
{     7-MAY-1992 (PDRAPER):
{        Original version.
{     {enter_changes_here}

{-
{
{  Find out which graphics device we're using.
proc pisa_demo p1
   print "Using device",(p1)
{
{  Device needs a @ to get HDS variable resolved.
   if substr( p1, 1, 1 ) = '@'
      device = (p1)
   else
      device = '@' & (p1)
   end if
{
{ initialise PISA
{
   pisa
{
{ PISA_DIR for FIO.
{
   PISA_DIR=getenv('PISA_DIR')
{
{ display the image
{
   print " "
   print "......Displaying image......"
   pisagrey noaxes device=(device) in=$PISA_DIR/frame drange=[477,700] ~
   accept reset
{
   print " "
   print "......Finding objects using PISAFIND......"
   pisafind in=$PISA_DIR/frame minpix=8 method=1 thresh=24 ~
    results=pisafind.dat sizes=pisasize.dat reset accept
{
   print " "
   print "......Plotting objects using PISAPLOT......"
   pisaplot results=pisafind.dat device=(device) palnum=2 reset accept
{
   print " "
   print "......Fitting stars using PISAFIT and PISA profiling function......"
   positions=(PISA_DIR) & "/frame_stars.acc"
   pisafit in=$PISA_DIR/frame ~
    positions=(positions) ~
    minmode=n radius=9 again=f weighted=f background=492.2 ~
    device=(device) reset accept
{
   print " "
   print "......Generating model frame using PISAGEN and profile fit......"
   pisagen input=$PISA_DIR/frame positions=pisafind.dat ~
    output=model background=0 noise=g sigma=30 reset accept
{
{ display the generated model data
   print " "
   print "......Displaying model frame......"
   pisagrey model noaxes drange=[0,450] device=(device) reset accept
{
{ object classification
   print " "
   print "......Modifying PISA parameters using PISAPEAK......"
   pisapeak in=$PISA_DIR/frame finddata=pisafind.dat ~
    results=pisapeak.dat reset accept
{
{ apply a cut to the data in peakedness
   print " "
   print "......Applying cut to peakedness parameter using PISACUT......"
   pisacut input=pisapeak.dat column=2 thresh=0.85 lower=stars1 ~
    higher=gals1 reset accept
{
{ separate pisafind results
   print " "
   print "......Matching PISACUT classes with pisafind data using PISAMATCH......"
   pisamatch one=stars1 two=pisafind.dat out=stars11 reset accept
   pisamatch one=gals1 two=pisafind.dat out=gals11 reset accept
{
{ display these
   pisagrey $PISA_DIR/frame drange=[477,700] ~
    device=(device) noaxes reset accept
   print " "
   print "......Displaying stars (in red) using PISAPLOT......"
   pisaplot overlay=t clear=f results=stars11 device=(device) palnum=2 ~
    noannota thick=1.5 reset accept
{
   print " "
   print "......Displaying galaxies (in green) using PISAPLOT......"
   pisaplot overlay=t clear=f results=gals11 device=(device) palnum=3 ~
    noannota thick=1.5 reset accept
endproc
{ $Id$
