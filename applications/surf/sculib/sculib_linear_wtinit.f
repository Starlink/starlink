*+
*     Name:
*      SCULIB_LINEAR_WTINIT
*
*     Purpose:
*      Set up weighting function for linear rebinning
*
*     Language:
*        Starlink FORTRAN 77
 
      SUBROUTINE SCULIB_LINEAR_WTINIT (WTFN, STATUS)

*    Description:
*     This is a FORTRAN version of the C transputer code. 
*     Here is the C description:
*     Initialise the array containing the weighting function for 
*     linear interpolation.
*     The weighting function is a cone, going from 1.0 at the centre to 
*     0.0 at the edge. The look-up table represents a radial slice through the
*     cone, tabulated to enable a look-up in terms of the square of the
*     radius.

*    Authors:
*     Tim Jenness (timj@jach.hawaii.edu) - FORTRAN
*     B.D.Kelly (ROE)                    - Transputer

 
*    Type definitions
      IMPLICIT NONE
 
*    External constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'REDS_SYS'
 
*    Import:
      INTEGER STATUS
 
*    Import-Export:
      REAL WTFN(SCUIP__RES1 * SCUIP__RES1)
 
*    Local variables:
 
      INTEGER I                          ! Counter
 
*    Local Data:
 
      IF (STATUS .NE. SAI__OK) RETURN
 
      DO I = 1, SCUIP__RES1 * SCUIP__RES1
 
         WTFN(I) = 1.0 - SQRT(REAL(I-1)) / SCUIP__RES1
 
      END DO
 
      END

