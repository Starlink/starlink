{
{             PISA UNIX ICL startup file
{
{  Define commands to run the PISA software
{
define pisafind      $PISA_DIR/pisa_mon
define pisaplot      $PISA_DIR/pisa_mon
define pisafit       $PISA_DIR/pisa_mon
define pisagen       $PISA_DIR/pisa_mon
define pisapeak      $PISA_DIR/pisa_mon
define pisaknn       $PISA_DIR/pisa_mon
define pisamatch     $PISA_DIR/pisa_mon
define pisacut       $PISA_DIR/pisa_mon
define pisagrey      $PISA_DIR/pisa_mon
define addnoise      $PISA_DIR/pisa_mon
defstring pisa_demo  load $PISA_DIR/pisa_demo
define pisa2ard      $PISA_DIR/pisa_mon
define pisa2cat      $PISA_DIR/pisa_mon
defproc pisawww      $PISA_DIR/pisa_proc.icl

defhelp pisa          $PISA_HELP 0
defstring pisahelp    help pisa
defhelp pisafind      $PISA_HELP
defhelp pisaplot      $PISA_HELP
defhelp pisafit       $PISA_HELP
defhelp pisagen       $PISA_HELP
defhelp pisapeak      $PISA_HELP
defhelp pisaknn       $PISA_HELP
defhelp pisamatch     $PISA_HELP
defhelp pisacut       $PISA_HELP
defhelp pisagrey      $PISA_HELP
defhelp addnoise      $PISA_HELP
defhelp pisa2ard      $PISA_HELP

{
{ Print welcome message
{
print " "
print "   PISA commands are now available - (Version PKG_VERS)"
print " "
print "   For help use the commands help pisa or pisawww"
print " "
{
{  Switch off parameter checking
{
set nocheckpars
{
{ $Id$
