      SUBROUTINE ESP1_ARD_DRIVE2(ELEMS,ARRAY,MASK,STATUS)

*+
*  Name:
*     ESP1_ARD_DRIVE2

*  Purpose:
*     Sets to bad all the pixels in the image that were selected by the
*     ARD file contents.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ESP1_ARD_DRIVE2(ELEMS,ARRAY,MASK,STATUS)

*  Description:
*     Looks through the mask array. Whenever a non-zero value is found
*     the associated pixel in the output image is set to VAL__BADR.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-JUN-1994 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-
*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'PRM_PAR'              ! PRIMDAT constants
      INCLUDE 'SAE_PAR'              ! Standard SAE parameters

*  Status:
      INTEGER STATUS                 ! Global status report

*  Arguments Given:
      INTEGER ELEMS                  ! Size of the image
      INTEGER MASK(ELEMS)            ! The masking image

*  Arguments Given and Returned:
      REAL ARRAY(ELEMS)              ! The output image

*  Local variables:
       INTEGER I                      ! Loop variable
*.

*   Check the global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Look to see what is in the mask for each pixel. If it is greater than
*   zero then replace the value in the output image with the bad value.
      DO 10 I=1,ELEMS
         IF (MASK(I).GT.0) ARRAY(I)=VAL__BADR
 10   CONTINUE

 9999 CONTINUE

      END
