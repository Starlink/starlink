{+
{  Name:
{     catpac.icl
{
{  Purpose:
{     Start the Catpac package from Unix ICL.
{
{  Type of Module:
{     ICL command list
{
{  Invocation:
{     load $CATPAC_DIR/catpac
{
{  Description:
{     This procedure starts the CATPAC package for use from ICL by
{     defining the actions needed to execute each application.
{
{     This is the Unix version.
{
{  Authors:
{     arw: Alan Wood (RAL)
{     {enter_new_authors_here}
{
{  History:
{     10 Oct 1994 (arw):
{        Original Version
{     {enter_changes_here}
{
{-
{.
{
{  Define help for package.
{
defhelp    cathelp $CATPAC_HELP
{
{  Announce name now, version etc. below.
{
print ' '
print ' ----------- Initialising for  CATPAC ------------'
{

define addp(aram)           $CATPAC_DIR/catpac_pm
define asciic(at)           $CATPAC_DIR/catpac_pm
define asciit(o)            $CATPAC_DIR/catpac_pm
{define cath(elp)            $CATPAC_DIR/catpac_pm
define catj(oin)            $CATPAC_DIR/catpac_pm
define catren(ame)          $CATPAC_DIR/catpac_pm
define catrep(ort)          $CATPAC_DIR/catpac_pm
define catse(arch)          $CATPAC_DIR/catpac_pm
define catso(rt)            $CATPAC_DIR/catpac_pm
define cop(ycat)            $CATPAC_DIR/catpac_pm
define delc(at)             $CATPAC_DIR/catpac_pm
define delf(ield)           $CATPAC_DIR/catpac_pm
define delp(aram)           $CATPAC_DIR/catpac_pm
define ent(ries)            $CATPAC_DIR/catpac_pm
define fie(ldinfo)          $CATPAC_DIR/catpac_pm
define fk425                $CATPAC_DIR/catpac_pm
define fk45z                $CATPAC_DIR/catpac_pm
define fk524                $CATPAC_DIR/catpac_pm
define fk54z                $CATPAC_DIR/catpac_pm
define glo(bals)            $CATPAC_DIR/catpac_pm
define parami(nfo)          $CATPAC_DIR/catpac_pm
define params               $CATPAC_DIR/catpac_pm
define pro(perm)            $CATPAC_DIR/catpac_pm
define sam(ple)             $CATPAC_DIR/catpac_pm
define upf(ield)            $CATPAC_DIR/catpac_pm
define upp(aram)            $CATPAC_DIR/catpac_pm


{ Define CATPAC's help from ICL
defhelp  addparam          $CATPAC_HELP
defhelp  asciitocat        $CATPAC_HELP
defhelp  catrename         $CATPAC_HELP
defhelp  catreport         $CATPAC_HELP
defhelp  catsearch         $CATPAC_HELP
defhelp  catsort           $CATPAC_HELP
defhelp  copycat           $CATPAC_HELP
defhelp  delcat            $CATPAC_HELP
defhelp  delparam          $CATPAC_HELP
defhelp  entries           $CATPAC_HELP
defhelp  fieldinfo         $CATPAC_HELP
defhelp  fields            $CATPAC_HELP
defhelp  fk425             $CATPAC_HELP
defhelp  fk524             $CATPAC_HELP
defhelp  fk45z             $CATPAC_HELP
defhelp  fk54z             $CATPAC_HELP
defhelp  globals           $CATPAC_HELP
defhelp  join              $CATPAC_HELP
defhelp  paraminfo         $CATPAC_HELP
defhelp  params            $CATPAC_HELP
defhelp  properm           $CATPAC_HELP
defhelp  sample            $CATPAC_HELP
defhelp  upfield           $CATPAC_HELP
defhelp  upparam           $CATPAC_HELP

{ Now repeat for the full names

define cat_addp(aram)           $CATPAC_DIR/catpac_pm
define cat_asciic(at)           $CATPAC_DIR/catpac_pm
define cat_asciit(o)            $CATPAC_DIR/catpac_pm
define cat_cath(elp)            $CATPAC_DIR/catpac_pm
define cat_catj(oin)            $CATPAC_DIR/catpac_pm
define cat_catren(ame)          $CATPAC_DIR/catpac_pm
define cat_catrep(ort)          $CATPAC_DIR/catpac_pm
define cat_catse(arch)          $CATPAC_DIR/catpac_pm
define cat_catso(rt)            $CATPAC_DIR/catpac_pm
define cat_cop(ycat)            $CATPAC_DIR/catpac_pm
define cat_delc(at)             $CATPAC_DIR/catpac_pm
define cat_delf(ield)           $CATPAC_DIR/catpac_pm
define cat_delp(aram)           $CATPAC_DIR/catpac_pm
define cat_ent(ries)            $CATPAC_DIR/catpac_pm
define cat_fie(ldinfo)          $CATPAC_DIR/catpac_pm
define cat_fk425                $CATPAC_DIR/catpac_pm
define cat_fk45z                $CATPAC_DIR/catpac_pm
define cat_fk524                $CATPAC_DIR/catpac_pm
define cat_fk54z                $CATPAC_DIR/catpac_pm
define cat_glo(bals)            $CATPAC_DIR/catpac_pm
define cat_parami(nfo)          $CATPAC_DIR/catpac_pm
define cat_params               $CATPAC_DIR/catpac_pm
define cat_pro(perm)            $CATPAC_DIR/catpac_pm
define cat_sam(ple)             $CATPAC_DIR/catpac_pm
define cat_upf(ield)            $CATPAC_DIR/catpac_pm
define cat_upp(aram)            $CATPAC_DIR/catpac_pm


{ Define CATPAC's help full names
defhelp  cat_addparam          $CATPAC_HELP addparam
defhelp  cat_asciitocat        $CATPAC_HELP asciito
defhelp  cat_catrename         $CATPAC_HELP catrename
defhelp  cat_catreport         $CATPAC_HELP catreport
defhelp  cat_catsearch         $CATPAC_HELP catsearch
defhelp  cat_catsort           $CATPAC_HELP catsort
defhelp  cat_copycat           $CATPAC_HELP copycat
defhelp  cat_delcat            $CATPAC_HELP delcat
defhelp  cat_delparam          $CATPAC_HELP delparam
defhelp  cat_entries           $CATPAC_HELP entries
defhelp  cat_fieldinfo         $CATPAC_HELP fieldinfo
defhelp  cat_fields            $CATPAC_HELP fields
defhelp  cat_fk425             $CATPAC_HELP fk425
defhelp  cat_fk524             $CATPAC_HELP fk524
defhelp  cat_fk45z             $CATPAC_HELP fk45z
defhelp  cat_fk54z             $CATPAC_HELP fk54z
defhelp  cat_globals           $CATPAC_HELP globals
defhelp  cat_catjoin           $CATPAC_HELP join
defhelp  cat_paraminfo         $CATPAC_HELP paraminfo
defhelp  cat_params            $CATPAC_HELP params
defhelp  cat_properm           $CATPAC_HELP properm
defhelp  cat_sample            $CATPAC_HELP sample
defhelp  cat_upfield           $CATPAC_HELP upfield
defhelp  cat_upparam           $CATPAC_HELP upparam

{ print catpac initialisation message }

print " "
print " --    Initialised for CATPAC    -- "
print " --   Version 1.1, 1994 January   -- "
print " "
print "     Type help cathelp for help"
print " "
{
{ end
