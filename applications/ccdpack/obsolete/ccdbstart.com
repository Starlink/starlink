$!vflag = f$verify(0)
$!+
$!  Name:
$!     CCDBSTART.COM
$!
$!  Type of Module:
$!     DCL command procedure.
$!
$!  Purpose:
$!     Performs initialisation for CCDPACK batch procedure.
$!
$!  Description:
$!
$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     20-MAY-1991 (PDRAPER):
$!        Original Version.
$!     {enter_further_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$!
$!
$! Write out the day and time.
$write sys$output "''f$time()'"
$!
$! Set up batch job ADAM_USER directory."
$!
$define/job ADAM_USER 'P1'
$!
$! Try to create the directory anyway
$!
$create/directory/nolog ADAM_USER
$!
$! Perform ADAMSTART
$!
$ADAMSTART
$!
$! Set up CCDPACK symbols
$!
$CCDPACK
$!
$set default 'P2'
$!
$!
$! if( vflag ) then set verify
$! $Id$
