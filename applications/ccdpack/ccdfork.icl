set nocheckpars
hidden proc ccdfork user_script output_script directory

{+
{   Name:
{      ccdfork

{   Purpose:
{      Creates a script for executing CCDPACK commands in a background
{      process.


{   Language:
{      ICL

{   Type of Module:
{      ICL/C-shell script

{   Usage:
{      ccdfork user_script [output_script] [directory]

{   Description:
{      This command saves the current ADAM execution environment and
{      writes a C-shell script which will execute a CCDPACK command file
{      written for running in the background. The output script is suitable
{      for execution as a (nice'd) background process, restoring the current
{      environment (thus isolating it from the interactive or any other
{      background processes) and re-initialising CCDPACK.

{   ADAM Parameters:
{      user_script = filename (read)
{         The name of the script file which contains the
{         CCDPACK commands which are to be run in the background.
{      output_script = filename (write)
{         The name of the output script which will re-establish
{         the current ADAM context and execute your command
{         file. [ccdpack_fork]
{      directory = directory (write)
{         The name of a directory in which to store the current
{         ADAM context. If no value is given then a sub-directory
{         of the current ADAM_USER parent is created.
{         [adam_unique_string]

{   Examples:
{      ccdfork ccdred
{      ! nice ccdpack_fork &
{         In this example ccdfork saves the current ADAM parameter
{         files and writes a script file named ccdpack_fork which
{         will enable the ccdred script file to execute in the
{         background. The output script ccdpack_fork is then run from
{         the C-shell and nice'd into the background.
{
{      ccdfork ccdred batch1
{      ! nice batch1 &
{         As above except that the output script is now called
{         batch1.
{
{      ccdfork ccdred batch2 /scratch/user/batch2
{         As above except the output script is now called batch2
{         and the ADAM parameter files are written to the directory
{         /scratch/user/batch2.

{   Notes:
{      ICL stub for passing the work onto the C-shell script of the
{      same name.

{   Authors:
{      PDRAPER: Peter Draper (STARLINK)
{      {enter_new_authors_here}

{   History:
{      12-SEP-1995 (PDRAPER):
{         Original Version.
{      {enter_further_changes_here}
{-

{  Check the input parameters.
   CCDdir=getenv("CCDPACK_DIR")
   if CCDdir <> ""
      if undefined(user_script)
         print "You must supply the name of a command script"
      else
         if undefined(output_script)
            sh (CCDdir)/ccdfork (user_script)
         else
            if undefined(directory)
               sh (CCDdir)/ccdfork (user_script) (output_script)
            else
               sh (CCDdir)/ccdfork (user_script) (output_script) (directory)
            endif
         endif
      endif
   else
      print "CCDPACK not installed correctly."
      print "Missing CCDPACK_DIR environment variable."
   endif
end proc
{ $Id$
