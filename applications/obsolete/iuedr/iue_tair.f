      SUBROUTINE IUE_TAIR( NPOINT, X )
*+
*  Name:
*     SUBROUTINE IUE_TAIR

*  Purpose:
*     Correct Vacuum wavelengths to Air.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IUE_TAIR( NPOINT, X )

*  Arguments:
*     NPOINT = INTEGER (Given)
*        Number of wavelengths to correct.
*     X = REAL*8 ( NPOINT ) (Given and Returned)
*        The wavelengths to correct.

*  Method:
*     The wavelengths are converted from vacuum to air for values above
*     2000A.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     24-JUN-81 (JRG):
*       IUEDR Vn. 1.0
*     04-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER NPOINT     ! number of wavelengths

*  Arguments Given and Returned:
      REAL*8 X( NPOINT ) ! Vacuum Wavelengths

*  Local Variables:
      INTEGER I          ! loop index

*  Internal References:
      REAL*8 CSCALE
      REAL*8 W

      CSCALE( W ) = 1.0 - 1.0 * 2.871E-4 * ( 1.0 + 5.67E5 / W ** 2 )

*.

      DO I = 1, NPOINT
         IF ( X( I ) .GT. 2000.0 ) THEN
            X( I ) = X( I ) * CSCALE( X( I ) )
         END IF
      END DO

      END
