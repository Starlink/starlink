      SUBROUTINE SCULIB_BESSEL_REGRID_1 (WEIGHT, X, Y, NPIX, PIXSPACE, 
     :   NI, NJ, ICEN, JCEN, AV_WEIGHT, SCRATCH, STATUS)
*+
*  Name:
*     SCULIB_BESSEL_REGRID_1

*  Purpose:

*  Language:
*     Starlink Fortran 77

*  Invocation:

*  Description:
*
*  Arguments:
*     WEIGHT = REAL (Given)
*        The weight of the input dataset.
*     X( NPIX ) = DOUBLE PRECISION (Given)
*        The x coordinates of the input pixels
*     Y( NPIX ) = DOUBLE PRECISION (Given)
*        The y coordinates of the input pixels
*     NPIX = INTEGER (Given)
*        the number of input pixels
*     PIXSPACE = DOUBLE PRECISION (Given)
*        the pixel spacing of the output pixels
*     NI = INTEGER (Given)
*        The number of output pixels in the x direction
*     NJ = INTEGER (Given)
*        The number of output pixels in the y direction
*     ICEN = INTEGER (Given)
*        the x index of the centre of the output array
*     JCEN = INTEGER (Given)
*        the y index of the centre of the output array
*     AV_WEIGHT (NI, NJ) = REAL (Given and Returned)
*        Given as a workspace array
*     SCRATCH (NI, NJ) = INTEGER (Returned)
*        Temporary workspace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Authors:
*     John Lightfoot (jfl@roe.ac.uk)

*  History:
*    17-AUG-1995: original version, adapted from JCMT_BESSEL_REGRID_1

*  Bugs:

*-
      
*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants

*  Arguments Given:
      REAL WEIGHT
      INTEGER NPIX
      DOUBLE PRECISION X(NPIX)
      DOUBLE PRECISION Y(NPIX)
      DOUBLE PRECISION PIXSPACE
      INTEGER ICEN
      INTEGER JCEN
      INTEGER NI
      INTEGER NJ

*  Arguments Returned:
      REAL AV_WEIGHT (NI, NJ)
      INTEGER SCRATCH (NI, NJ)

*  Status:
      INTEGER STATUS                             ! Global status

*  External Functions:

*  Local Constants:

*  Local Variables:
      INTEGER I, J                               ! loop counters
      INTEGER IOUT, JOUT                         ! loop counters
      INTEGER PIX                                ! current input pixel number
      DOUBLE PRECISION XINC                      ! the x-axis pixel increment
      DOUBLE PRECISION YINC                      ! the y-axis pixel increment
      
*   local data
*.

      IF (STATUS .NE. SAI__OK) RETURN

*  set x and y axis pixel increments, x increases to left hence -ve.

      XINC = -PIXSPACE
      YINC = PIXSPACE

*  initialise the scratch array

      DO JOUT = 1, NJ
         DO IOUT = 1, NI
            SCRATCH (IOUT,JOUT) = 1
         END DO
      END DO

*  go through the input pixels of this dataset, set all elements of
*  the scratch array to xero that lie within one pixel space of an input 
*  point. 

      DO PIX = 1, NPIX
         IOUT = NINT (X(PIX)/XINC) + ICEN
         JOUT = NINT (Y(PIX)/YINC) + JCEN
         DO J = -1, 1
            IF ((JOUT+J .GE. 1) .AND. (JOUT+J .LE. NJ)) THEN
               DO I = -1, 1
                  IF ((IOUT+I .GE. 1) .AND. (IOUT+I .LE. NI)) THEN
                     SCRATCH (IOUT+I, JOUT+J) = 0
                  END IF
               END DO
            END IF
         END DO
      END DO
 
*  go through the scratch array and, where it's pixels are not 0, increment
*  the corresponding `average weight' pixel by the weight of the input
*  dataset

      DO JOUT = 1, NJ
         DO IOUT = 1, NI
            IF (SCRATCH (IOUT,JOUT) .NE. 1) THEN
               AV_WEIGHT (IOUT,JOUT) = AV_WEIGHT (IOUT,JOUT) + WEIGHT
            END IF
         END DO
      END DO

      END
