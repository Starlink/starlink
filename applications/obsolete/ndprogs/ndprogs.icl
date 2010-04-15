{+
{  Name:
{     ndprogs.icl

{  Purpose:
{     Define command set for NDPROGS package.

{  Type of Module:
{     ICL file.

{  Invocation:
{     LOAD NDPROGS

{  Description:
{     This procedure defines each of the NDPROGS command set.

{  Authors:
{     GJP: G.J. Privett (Starlink, Cardiff)
{     BLY: M.J. Bly (Starlink, RAL)
{     {enter_new_authors_here}

{  History:
{     15-NOV-1994 (BLY/GJP)
{       Original Version:  uses IRAS90 version as guide.
{     {enter_changes_here}

{-

{   Define main package help

defhelp ndprogs     $NDPROGS_HELP 0
defstring ndprogshelp help ndprogs
defstring ndp_help help ndprogs

{   Basic command definitions.

define addnd         $NDPROGS_DIR/ndprogs_mon addnd
define ndp_addnd     $NDPROGS_DIR/ndprogs_mon addnd

define arith1        $NDPROGS_DIR/ndprogs_mon arith1
define ndp_arith1    $NDPROGS_DIR/ndprogs_mon arith1

define arith2        $NDPROGS_DIR/ndprogs_mon arith2
define ndp_arith2    $NDPROGS_DIR/ndprogs_mon arith2

define axflip        $NDPROGS_DIR/ndprogs_mon axflip
define ndp_axflip    $NDPROGS_DIR/ndprogs_mon axflip

define collapse      $NDPROGS_DIR/ndprogs_mon collapse
define ndp_collapse  $NDPROGS_DIR/ndprogs_mon collapse

define degamma       $NDPROGS_DIR/ndprogs_mon degamma
define ndp_degamm    $NDPROGS_DIR/ndprogs_mon degamma

define depict        $NDPROGS_DIR/ndprogs_mon depict
define ndp_depict    $NDPROGS_DIR/ndprogs_mon depict

define dummy         $NDPROGS_DIR/ndprogs_mon dummy
define ndp_dummy     $NDPROGS_DIR/ndprogs_mon dummy

define hilite        $NDPROGS_DIR/ndprogs_mon hilite
define ndp_hilite    $NDPROGS_DIR/ndprogs_mon hilite

define logic1        $NDPROGS_DIR/ndprogs_mon logic1
define ndp_logic1    $NDPROGS_DIR/ndprogs_mon logic1

define logic2        $NDPROGS_DIR/ndprogs_mon logic2
define ndp_logic2    $NDPROGS_DIR/ndprogs_mon logic2

define looknd        $NDPROGS_DIR/ndprogs_mon look
define ndp_looknd    $NDPROGS_DIR/ndprogs_mon look

define magic         $NDPROGS_DIR/ndprogs_mon magic
define ndp_magic     $NDPROGS_DIR/ndprogs_mon magic

define mask1         $NDPROGS_DIR/ndprogs_mon mask1
define ndp_mask1     $NDPROGS_DIR/ndprogs_mon mask1

define mask2         $NDPROGS_DIR/ndprogs_mon mask2
define ndp_mask2     $NDPROGS_DIR/ndprogs_mon mask2

define moments       $NDPROGS_DIR/ndprogs_mon moments
define ndp_moments   $NDPROGS_DIR/ndprogs_mon moments

define movie         $NDPROGS_DIR/ndprogs_mon movie
define ndp_movie     $NDPROGS_DIR/ndprogs_mon movie

define peek          $NDPROGS_DIR/ndprogs_mon peek
define ndp_peek      $NDPROGS_DIR/ndprogs_mon peek

define plots         $NDPROGS_DIR/ndprogs_mon plots
define ndp_plots     $NDPROGS_DIR/ndprogs_mon plots

define setaxes       $NDPROGS_DIR/ndprogs_mon setaxes
define ndp_setaxes   $NDPROGS_DIR/ndprogs_mon setaxes

define slice3d       $NDPROGS_DIR/ndprogs_mon slice3d
define ndp_slice3d   $NDPROGS_DIR/ndprogs_mon slice3d

define spectrum      $NDPROGS_DIR/ndprogs_mon spectrum
define ndp_spectrum  $NDPROGS_DIR/ndprogs_mon spectrum

define squint        $NDPROGS_DIR/ndprogs_mon squint
define ndp_squint    $NDPROGS_DIR/ndprogs_mon squint

define smooth        $NDPROGS_DIR/ndprogs_mon smooth
define ndp_smooth    $NDPROGS_DIR/ndprogs_mon smooth

define stack         $NDPROGS_DIR/ndprogs_mon stack
define ndp_stack     $NDPROGS_DIR/ndprogs_mon stack

define stats         $NDPROGS_DIR/ndprogs_mon stats
define ndp_stats     $NDPROGS_DIR/ndprogs_mon stats

define stretch       $NDPROGS_DIR/ndprogs_mon stretch
define ndp_stretch   $NDPROGS_DIR/ndprogs_mon stretch

define subset        $NDPROGS_DIR/ndprogs_mon subset
define ndp_subset    $NDPROGS_DIR/ndprogs_mon subset

defstring tau2fig ndp_na tau2fig
defstring ndp_tau2fig ndp_na tau2fig

define testnd        $NDPROGS_DIR/ndprogs_mon test
define ndp_testnd    $NDPROGS_DIR/ndprogs_mon test

define transform     $NDPROGS_DIR/ndprogs_mon transform
define ndp_transform $NDPROGS_DIR/ndprogs_mon transform

define transpose     $NDPROGS_DIR/ndprogs_mon transpose
define ndp_transpose $NDPROGS_DIR/ndprogs_mon transpose

define typecon       $NDPROGS_DIR/ndprogs_mon typecon
define ndp_typecon   $NDPROGS_DIR/ndprogs_mon typecon

define unmagic       $NDPROGS_DIR/ndprogs_mon unmagic
define ndp_unmagic   $NDPROGS_DIR/ndprogs_mon unmagic

{   ndp_na procedure

hidden proc ndp_na task
   print
   print " "(task) "is not available under Unix."
   print
endproc

{   Help definitions.

defhelp addnd     $NDPROGS_HELP
defhelp arith1    $NDPROGS_HELP
defhelp arith2    $NDPROGS_HELP
defhelp axflip    $NDPROGS_HELP
defhelp collapse  $NDPROGS_HELP
defhelp degamma   $NDPROGS_HELP
defhelp depict    $NDPROGS_HELP
defhelp dummy     $NDPROGS_HELP
defhelp hilite    $NDPROGS_HELP
defhelp logic1    $NDPROGS_HELP
defhelp logic2    $NDPROGS_HELP
defhelp look      $NDPROGS_HELP
defhelp magic     $NDPROGS_HELP
defhelp mask1     $NDPROGS_HELP
defhelp mask2     $NDPROGS_HELP
defhelp moments   $NDPROGS_HELP
defhelp movie     $NDPROGS_HELP
defhelp peek      $NDPROGS_HELP
defhelp plots     $NDPROGS_HELP
defhelp setaxes   $NDPROGS_HELP
defhelp slice3d   $NDPROGS_HELP
defhelp smooth    $NDPROGS_HELP
defhelp spectrum  $NDPROGS_HELP
defhelp squint    $NDPROGS_HELP
defhelp stack     $NDPROGS_HELP
defhelp stats     $NDPROGS_HELP
defhelp stretch   $NDPROGS_HELP
defhelp subset    $NDPROGS_HELP
defhelp tau2fig   $NDPROGS_HELP
defhelp test      $NDPROGS_HELP
defhelp transform $NDPROGS_HELP
defhelp transpose $NDPROGS_HELP
defhelp typecon   $NDPROGS_HELP
defhelp unmagic   $NDPROGS_HELP

{   Announce the NDPROGS commands are available.

print ""
print "   NDPROGS commands are now available -- (Version PKG_VERS)"
print " "
print "   Type `help ndprogs' or `ndprogshelp' for help on NDPROGS commands"
print " "

