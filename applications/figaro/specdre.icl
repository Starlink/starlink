{+
{  Name:
{     specdre.icl
{
{  Purpose:
{     Start the Specdre package from ICL.
{
{  Type of Module:
{     ICL command list
{
{  Invocation:
{     load $SPECDRE_DIR/specdre
{
{  Description:
{     This procedure used to start the Specdre package for use from the
{     ICL command language defining the commands needed to execute each
{     application.
{     Since Specdre has been merged with Figaro, this procedure now tells
{     the user to invoke Figaro.
{
{  Authors:
{     hme: Horst Meyerdierks (UoE, Starlink)
{     ACC: Anne Charles (RAL)
{     {enter_new_authors_here}
{
{  History:
{     13 May 1994 (hme):
{        Original Version.
{     10 Dec 1997 (ACC):
{        Tell user to invoke Figaro in order to use Specdre commands.
{     {enter_changes_here}
{
{-
{.
print ' '
print '        Specdre has been merged with Figaro'
print ' '
print '  In order to use the Specdre commands, use Figaro'
print ' '
print '        Type "fighelp specdre" for help'
print ' '
{
{ end
{
