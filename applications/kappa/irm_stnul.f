      SUBROUTINE IRM_STNUL( NY, SET, YDAT, STATUS )
*+
*  Name:
*     IRM_STNUL

*  Purpose:
*     Set bad pixels having ADAM magic value to NCAR null value, or vice
*     versa.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_STNUL( NX, SET, YDAT, STATUS )

*  Description:
*     This routine convert the value of bad pixels from ADAM magic value
*     to NCAR null value, or vice versa. 

*  Arguments:
*     NY = INTEGER (Given)
*        Number of pixels in input array
*     SET = LOGICAL (Given)
*        If it is true, the convert is from ADAM magic value to NCAR
*        null value. Otherwise, from NCAR null value to ADAM magic
*        value.
*     YDAT( NY ) = REAL (Given and Return)
*        As given, it contains input data whose bad pixel value to be
*        converted. On return, it contains output data whose bad pixel
*        value having been converted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     22-JAN-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions

*  Arguments Given:
      INTEGER NY
      LOGICAL SET

*  Arguments Given and Returned:
      REAL YDAT( NY )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL NULL1                 ! Null value of NCAR (AUTOGRAPH)
      INTEGER I                  ! Do loop index
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the NCAR null value.
      CALL AGGETF( 'NULL/1.', NULL1 )

*  If convert is from ADAM magic value to NCAR null value,
      IF( SET ) THEN
         DO  I = 1, NY
            IF( YDAT( I ) .EQ. VAL__BADR ) YDAT( I ) = NULL1
         END DO

*  Otherwise convert is from NCAR null value to ADAM magic value
      ELSE
         DO I = 1, NY
            IF( YDAT( I ) .EQ. NULL1 ) YDAT( I ) = VAL__BADR
         END DO
      END IF

      END
