*+  RED4_ZERO_TO_ONE - Replace zero or negative values in a real array with 1.0
      SUBROUTINE RED4_ZERO_TO_ONE( DIM, INPUT, OUTPUT, STATUS )
*    Description :
*     This routine replaces any zero or negative value in a real array
*     with 1.0. It is used for fudging the variance array given to
*     Bob Carswell's optimal extraction routine. The output and input
*      arrays may be the same.
*    Invocation :
*     CALL RED4_ZERO_TO_ONE( DIM, INPUT, OUTPUT, STATUS )
*    Parameters :
*     DIM          = INTEGER( READ )
*         The dimension of the arrays.
*     INPUT( DIM ) = REAL( READ )
*         The input array.
*     OUTPUT( DIM ) = REAL( WRITE )
*         The output array.
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     24-Feb-1991: Original version.                       (SMB)
*     23-Feb-1993: Conform to error strategy               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
*    Global variables:
*    Import :
      INTEGER
     :  DIM                   ! The dimension of the arrays.
      REAL
     :  INPUT( DIM )          ! The input array
*    Export:
      REAL
     :  OUTPUT( DIM )         ! The output array.
*    Status :
      INTEGER STATUS
*    External references:
*    Local Constants :
*    Local variables :
      INTEGER
     :  I                     ! Loop counter
*    Local data :
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Scan through the array, replacing any zero or negative values
*   with 1.0.
      DO I = 1, DIM

         IF ( INPUT(I) .LE. 0.0 ) THEN

            OUTPUT(I) = 1.0
         ELSE

            OUTPUT(I) = INPUT(I)
         END IF
      END DO

      END
