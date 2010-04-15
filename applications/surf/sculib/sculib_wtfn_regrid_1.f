      SUBROUTINE SCULIB_WTFN_REGRID_1 (WEIGHT,
     :     IN_DATA, X, Y, NPIX, PIXSPACE, NI, NJ, ICEN, JCEN,
     :     WEIGHTSIZE, SCLSZ, AV_WEIGHT, SCRATCH, STATUS)
*+
*  Name:
*     SCULIB_WTFN_REGRID_1

*  Purpose:
*     Calculate the coverage of the data and total weight per cell

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_WTFN_REGRID_1 (WEIGHT, IN_DATA, X, Y, NPIX,
*    :     PIXSPACE, NI, NJ, ICEN, JCEN, WEIGHTSIZE, SCLSZ,
*    :     AV_WEIGHT, SCRATCH, STATUS)

*  Description:
*     This routine performs two tasks:
*     -[1] Determine which output pixels contain data (effectively
*        detecting the edge of the useful area of the output image.
*     -[2] For each output pixel containing data the weight of the
*        map is added to the total weight. This TOTAL_WEIGHT is then
*        used to modify the behaviour when two images overlap (the
*        TOTAL_WEIGHT will be 2.0 (say) in the overlap region and
*        1 elsewhere.

*  Arguments:
*     WEIGHT = REAL (Given)
*        The weight of the input dataset.
*     IN_DATA (NPIX)                   = REAL (Given)
*        The input data values. (Used to define quality)
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
*     WEIGHTSIZE                       = INTEGER (Given)
*        radius of weight function in scale units (SCUIP__FILTRAD for BESSEL,
*        1 for LINEAR)
*     SCLSZ                            = REAL
*        1 scale length in the same units as PIXSPACE
*     AV_WEIGHT (NI, NJ) = REAL (Given and Returned)
*        Given as a workspace array
*     SCRATCH (NI, NJ) = INTEGER (Returned)
*        Temporary workspace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     John Lightfoot (jfl@roe.ac.uk)
*     Tim Jenness (JAC)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.8  2005/03/23 03:48:21  timj
*     No longer use wavelength + diameter for determining resolution element. Use
*     scale+weightsize throughout
*
*     Revision 1.7  1999/08/19 03:37:32  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.6  1999/08/06 02:24:52  timj
*     Tweak headers for use with PROLAT.
*
*     17-AUG-1995: original version, adapted from JCMT_BESSEL_REGRID_1

*  Bugs:

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! Bad values

*  Arguments Given:
      REAL WEIGHT
      INTEGER NPIX
      REAL IN_DATA(NPIX)
      DOUBLE PRECISION X(NPIX)
      DOUBLE PRECISION Y(NPIX)
      DOUBLE PRECISION PIXSPACE
      INTEGER ICEN
      INTEGER JCEN
      INTEGER NI
      INTEGER NJ
      REAL    SCLSZ
      INTEGER WEIGHTSIZE

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
      REAL DISTSQ                                ! Pixel distance squared
      REAL RDIST                                 ! Size in pixels of res el
      REAL RDIST_SQ                              ! RDIST squared
      INTEGER PIX_RANGE                          ! Pixel range

*   local data
*.

      IF (STATUS .NE. SAI__OK) RETURN

*  set x and y axis pixel increments, x increases to left hence -ve.

      XINC = -PIXSPACE
      YINC = PIXSPACE

*  find the size of the resolution element in pixels
      RDIST = REAL(WEIGHTSIZE) * SCLSZ / REAL(PIXSPACE)
      PIX_RANGE = INT (RDIST) + 1
      RDIST_SQ = RDIST * RDIST

*  initialise the scratch array

      DO JOUT = 1, NJ
         DO IOUT = 1, NI
            SCRATCH (IOUT,JOUT) = 1
         END DO
      END DO

*  go through the input pixels of this dataset, set all elements of
*  the scratch array to zero that lie within one res element of an input
*  point.

      DO PIX = 1, NPIX
         IF (IN_DATA(PIX) .NE. VAL__BADR) THEN
            IOUT = NINT (X(PIX)/XINC) + ICEN
            JOUT = NINT (Y(PIX)/YINC) + JCEN
            DO J = -PIX_RANGE, PIX_RANGE
               IF ((JOUT+J .GE. 1) .AND. (JOUT+J .LE. NJ)) THEN
                  DO I = -PIX_RANGE, PIX_RANGE
                     IF ((IOUT+I .GE. 1) .AND. (IOUT+I .LE. NI)) THEN
*                  Is this closer than  RDIST
                        DISTSQ = I**2 + J**2
                        IF (DISTSQ .LE. RDIST_SQ) THEN
                           SCRATCH (IOUT+I, JOUT+J) = 0
                        END IF
                     END IF
                  END DO
               END IF
            END DO
         END IF
      END DO

*  go through the scratch array and, where it's pixels are  0, increment
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
