      SUBROUTINE MRDATA( NAXIS1, NAXIS2, D_VM, Q_VM, STATUS )
*+
*  Name:
*     SUBROUTINE MRDATA

*  Purpose:
*     Return general image details.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MRDATA( NAXIS1, NAXIS2, D_VM, Q_VM, STATUS )

*  Arguments:
*     NAXIS1 = INTEGER (Returned)
*        Number of Samples (X-width of image).
*     NAXIS2 = INTEGER (Returned)
*        Number of Lines (Y-height of image).
*     D_VM = INTEGER (Returned)
*        Pointer to memory allocated to hold image data.
*     Q_VM = INTEGER (Returned)
*        Pointer to memory allocated to hold image quality information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Check that the necessary image has been accessed (via DASSOC), and
*     then simply return addresses from CMFILE.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     22-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-DEC-94 (MJC)
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Returned:
      INTEGER NAXIS1     ! axis1 size
      INTEGER NAXIS2     ! axis2 size
      INTEGER D_VM       ! DATA VM
      INTEGER Q_VM       ! QUAL VM

*  Status:
      INTEGER STATUS     ! Global status.

*  Global Variables:
      INCLUDE 'CMFILE'
      INCLUDE 'CMDATA'

*.

      IF ( NODATA .OR. NOIMA ) THEN
         CALL ERROUT('Error: image not available\\', STATUS)

      ELSE
         NAXIS1 = NS
         NAXIS2 = NL
         D_VM = DATA_VM
         Q_VM = QUAL_VM
      END IF

      END
