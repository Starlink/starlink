{
{  CCDPACK ICL monolith command definitions file.
{
{

define    calcor      $CCDPACK_DIR/ccdpack_red
define    ccdclear    $CCDPACK_DIR/ccdpack_res
define    ccdedit     $CCDPACK_DIR/ccdpack_reg
define    ccdgenerate $CCDPACK_DIR/ccdpack_res
define    ccdndfac    $CCDPACK_DIR/ccdpack_res
define    ccdnote     $CCDPACK_DIR/ccdpack_res
define    ccdsetup    $CCDPACK_DIR/ccdpack_res
define    ccdshow     $CCDPACK_DIR/ccdpack_res
define    debias      $CCDPACK_DIR/ccdpack_red
define    findcent    $CCDPACK_DIR/ccdpack_reg
define    findobj     $CCDPACK_DIR/ccdpack_reg
define    findoff     $CCDPACK_DIR/ccdpack_reg
define    flatcor     $CCDPACK_DIR/ccdpack_red
define    idicurs     $CCDPACK_DIR/ccdpack_reg
define    import      $CCDPACK_DIR/ccdpack_res
define    makebias    $CCDPACK_DIR/ccdpack_red
define    makecal     $CCDPACK_DIR/ccdpack_red
define    makeflat    $CCDPACK_DIR/ccdpack_red
define    makemos     $CCDPACK_DIR/ccdpack_reg
define    pairndf     $CCDPACK_DIR/ccdpack_reg
define    plotlist    $CCDPACK_DIR/ccdpack_reg
define    present     $CCDPACK_DIR/ccdpack_res
define    register    $CCDPACK_DIR/ccdpack_reg
define    schedule    $CCDPACK_DIR/ccdpack_res
define    tranlist    $CCDPACK_DIR/ccdpack_reg
define    tranndf     $CCDPACK_DIR/ccdpack_reg
define    reduce      $CCDPACK_DIR/ccdpack_scr
define    ccdalign    $CCDPACK_DIR/ccdpack_scr
define    xreduce     $CCDPACK_DIR/ccdpack_scr runxreduce
defproc   ccdexercise $CCDPACK_DIR/ccdexercise
defproc   ccdfork     $CCDPACK_DIR/ccdfork
defstring ccdwww      !$CCDPACK_DIR/ccdwww

{  NAG versions of routines  (may not exist).
define    nagmakemos  $CCDPACK_DIR/ccdpack_nag

{
{  Define help.
{

defhelp   calcor        $CCDPACK_HELP
defhelp   ccdclear      $CCDPACK_HELP
defhelp   ccdedit       $CCDPACK_HELP
defhelp   ccdfork       $CCDPACK_HELP
defhelp   ccdndfac      $CCDPACK_HELP
defhelp   ccdnote       $CCDPACK_HELP
defhelp   ccdpack       $CCDPACK_HELP 0
defhelp   ccdsetup      $CCDPACK_HELP
defhelp   ccdshow       $CCDPACK_HELP
defhelp   debias        $CCDPACK_HELP
defhelp   findcent      $CCDPACK_HELP
defhelp   findobj       $CCDPACK_HELP
defhelp   findoff       $CCDPACK_HELP
defhelp   flatcor       $CCDPACK_HELP
defhelp   idicurs       $CCDPACK_HELP
defhelp   import        $CCDPACK_HELP
defhelp   makebias      $CCDPACK_HELP
defhelp   makecal       $CCDPACK_HELP
defhelp   makeflat      $CCDPACK_HELP
defhelp   makemos       $CCDPACK_HELP
defhelp   pairndf       $CCDPACK_HELP
defhelp   plotlist      $CCDPACK_HELP
defhelp   present       $CCDPACK_HELP
defhelp   reduce        $CCDPACK_HELP
defhelp   register      $CCDPACK_HELP
defhelp   schedule      $CCDPACK_HELP
defhelp   tranlist      $CCDPACK_HELP
defhelp   tranndf       $CCDPACK_HELP
defhelp   xreduce       $CCDPACK_HELP
defstring ccdhelp       help ccdpack

{ Setup conversion of header information for when using foreign data
{ access.
hidden proc ccdpack_xtn_set
   ndf_xtn = getenv("NDF_XTN")
   xtn_index = index( ndf_xtn, "CCDPACK" )
   if xtn_index=0
      if ndf_xtn = ""
         setenv NDF_XTN "CCDPACK"
      else
         setenv NDF_XTN (ndf_xtn)",CCDPACK"
      end if
   end if
   setenv NDF_IMP_CCDPACK "$CCDPACK_DIR/ccdimp ^ndf"
   setenv NDF_EXP_CCDPACK "$CCDPACK_DIR/ccdexp ^ndf"
end proc
ccdpack_xtn_set

{
{ Print welcome message
{
print " "
print "   CCDPACK commands are now available - (version PKG_VERS)"
print " "
print "   For help use the commands help ccdpack or ccdwww"
print " "

{   For IRAF data we really do need to keep hold of the bad pixels
{   so make sure of this, unless the NDF_TO_IRAF_PARS variable
{   is already set. In this case we assume that the user knows what
{   they are doing.
hidden proc ccdpack_iraf_pars_set
   pars = getenv("NDF_TO_IRAF_PARS")
   if pars = ""
      setenv NDF_TO_IRAF_PARS "FILLBAD=!"
   end if
end proc
ccdpack_iraf_pars_set
