{+
{  Name:
{     CCDBATCH

{  Purpose:
{     Save an ADAM execution environment and write intialising batch
{     commands.

{  Language:
{     DCL

{  Type of Module:
{     ICL command precedure

{  Invocation:
{     CCDBATCH

{  Arguments:
{     STATUS = INTEGER (Given and Returned)
{        The global status.

{  Description:
{     This routine just activates the command procedures, CCDSAVE and
{     CCDBATFIL. CCDSAVE saves the current ADAM execution environment
{     by taking a copy of the SDF files in the ADAM_USER directory. It
{     stores them in a directory which it creates as a subdirectory of
{     SYS$SCRATCH. CCDBATFIL writes the name of the
{     ADAM_SAVED directory as created by CCDSAVE, together with other
{     routine initialisation commands, into a file CCDPACKB.COM, which
{     the user can then enter his batch commands.


{  Usage:
{     CCDBATCH

{  Authors:
{     PDRAPER: Peter Draper (STARLINK)
{     {enter_new_authors_here}

{  History:
{     24-SEP-1991 (PDRAPER):
{        Original version.
{     {enter_changes_here}

{  Bugs:
{     {note_any_bugs_here}
{

{-
{
{ Save the current environment.
{
$@CCDPACK_DIR:CCDSAVE
{
{ Write initialising log file.
{
$@CCDPACK_DIR:CCDBATFIL
{
{ End of procedure
{ $Id$
