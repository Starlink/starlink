proc pisawww browser
{+
{  Name:
{     pisawwww
{
{  Purpose:
{     Starts a hypertext browser to read the PISA document.
{
{   Invocation:
{     pisawww [browser]
{
{   Type of module:
{     ICL procedure.
{
{  Arguments:
{     browser = string
{        Command to start browser. This is optional and defaults to "Mosaic".
{
{  History:
{     7-JUN-1995 (PDRAPER):
{        Original version.
{     {enter_changes_here}
{ -


{  Check for presence of argument. This is the browser name
{  defaults to Mosaic.

if undefined(browser)
   sh $PISA_DIR/pisawww
else
   sh $PISA_DIR/pisawww (browser)
endif

{  Start the browser with the required document.

endproc
{ Z%%M%   %I%   %E% %U%   %D% %T%
