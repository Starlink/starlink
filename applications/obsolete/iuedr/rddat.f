      SUBROUTINE RDDAT( FD, NAXIS1, NAXIS2, DATA, STATUS )
*+
*  Name:
*     SUBROUTINE RDDAT

*  Purpose:
*     Read the DATA array from disk to the supplied program array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDDAT( FD, NAXIS1, NAXIS2, DATA, STATUS )

*  Arguments:
*     FD = INTEGER (Given)
*        File I/O unit for source file.
*     NAXIS1 = INTEGER (Given)
*        Size of image in X.
*     NAXIS2 = INTEGER (Given)
*        Size of image in Y.
*     DATA = INTEGER*2( NAXIS1, NAXIS2 ) (Returned)
*        Storage space for the image data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     14-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Implicit:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMDATA'

*  Arguments Given:
      INTEGER FD                         ! File descriptor.
      INTEGER NAXIS1                     ! Size of axis1.
      INTEGER NAXIS2                     ! Size of axis2.

*  Arguments Returned:
      INTEGER*2 DATA( NAXIS1, NAXIS2 )   ! DATA array.

*  Status:
      INTEGER STATUS                     ! Global status.

*  Local variables:
      INTEGER IS                         ! Sample index.
      INTEGER IL                         ! Line index.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start of image.
      DO IL = 1, LMIN - 1
         DO IS = 1, NAXIS1
            DATA( IS, IL ) = DBLANK
         END DO
      END DO

*  Lines in image subset.
      DO IL = LMIN, LMAX
         DO IS = 1, NAXIS1
            DATA( IS, IL ) = DBLANK
         END DO

         IF ( SMIN( IL ) .LE. SMAX( IL ) ) THEN
            READ ( FD, IOSTAT = STATUS )
     :             ( DATA( IS, IL ), IS = SMIN( IL ), SMAX( IL ) )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: reading data array\\', STATUS )
               GO TO 999
            END IF
         END IF
      END DO

*  End of image.
      DO IL = LMAX + 1, NAXIS2
         DO IS = 1, NAXIS1
            DATA( IS, IL ) = DBLANK
         END DO
      END DO

 999  CONTINUE

      END
