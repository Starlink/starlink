$!+
$!  Name:
$!     MAKE_MONOLITH
$!
$!  Purpose:
$!     To compile and link the CCDPACK monolith
$!
$!  Language:
$!     DCL
$!
$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     28-MAY-1991 (PDRAPER):
$!        Original version.
$!     {enter_changes_here}
$!-
$fort ccdpack
$mlink ccdpack,-
ccdpack_libdir:ccdpack_tasks/lib,-
ccdpack_libdir:ccdpack/opt,-
ccdpack_libdir:ccdpack/lib,-     ! Included again to resolve later references
ccdpack_libdir:irg_link/opt,-
psx_dir:psx_link_adam/opt,-
nag_lib/lib,-
ccdpack_libdir:ccdpack/lib,-     ! Included again to resolve later references
trn_link/opt,-
prm_link/opt
$exit
$! $Id$
