$!+
$!  Name:
$!     MAKE_PACKAGE_BATCH

$!  Purpose:
$!     To relink the whole of the CCDPACK package from batch.

$!  Language:
$!     DCL

$!
$!  Notes:
$!     This command procedure is suitable for direct submission
$!     to a batch queue.

$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}

$!  History:
$!     11-DEC-1991 (PDRAPER):
$!        Original version.
$!     {enter_changes_here}
$!-
$!
$set default ccdpack_dir
$@ccdpack_dev
$@make_package
$exit
$! $Id$
