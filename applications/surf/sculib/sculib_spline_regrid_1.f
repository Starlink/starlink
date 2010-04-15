      SUBROUTINE SCULIB_SPLINE_REGRID_1 (EFF_RADIUS,
     :     IN_DATA, X, Y, NPIX,
     :     PIXSPACE, NI, NJ, ICEN, JCEN, QUALITY, STATUS)
*+
*  Name:
*     SCULIB_SPLINE_REGRID_1

*  Purpose:
*     Calculate the areas of the output map that contain good data points

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_SPLINE_REGRID_1 (EFF_RADIUS,
*    :     IN_DATA, X, Y, NPIX,
*    :     PIXSPACE, NI, NJ, ICEN, JCEN, QUALITY, STATUS)

*  Description:
*     Calculate the areas of the output map that will be affected by
*     points in the input data array. This is used to generate a mask
*     so that the rebinned output map only contains data points close
*     to input data. Similar to SCULIB_WTFN_REGRID_1.

*  Arguments:
*     EFF_RADIUS = REAL (Given)
*        Radius of effective footprint of influence due to each
*        input data point. The resulting spline will have no good
*        data points at a distance greater than this from the
*        the original data. Units are the same as for PIXSPACE.
*     IN_DATA (NPIX)                   = REAL (Given)
*        The input data values. (Used to define quality)
*     X( NPIX ) = REAL (Given)
*        The x coordinates of the input pixels
*     Y( NPIX ) = REAL (Given)
*        The y coordinates of the input pixels
*     NPIX = INTEGER (Given)
*        the number of input pixels
*     PIXSPACE = REAL (Given)
*        the pixel spacing of the output pixels
*     NI = INTEGER (Given)
*        The number of output pixels in the x direction
*     NJ = INTEGER (Given)
*        The number of output pixels in the y direction
*     ICEN = INTEGER (Given)
*        the x index of the centre of the output array
*     JCEN = INTEGER (Given)
*        the y index of the centre of the output array
*     SCRATCH (NI, NJ) = UBYTE (Returned)
*        Quality mask
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     John Lightfoot (jfl@roe.ac.uk)
*     Tim Jenness (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*    17-AUG-1995: original version, adapted from JCMT_BESSEL_REGRID_1
*    1997 Apr 5 (timj)
*       adapted from sculib_wtfn_regrid_1.f

*  Bugs:

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! Bad values

*  Arguments Given:
      REAL EFF_RADIUS
      INTEGER NPIX
      REAL IN_DATA(NPIX)
      REAL X(NPIX)
      REAL Y(NPIX)
      REAL PIXSPACE
      INTEGER ICEN
      INTEGER JCEN
      INTEGER NI
      INTEGER NJ
      REAL WAVELENGTH

*  Arguments Returned:
      BYTE QUALITY (NI, NJ)

*  Status:
      INTEGER STATUS                             ! Global status

*  External Functions:

*  Local Constants:

*  Local Variables:
      INTEGER I, J                               ! loop counters
      INTEGER IOUT, JOUT                         ! loop counters
      INTEGER PIX                                ! current input pixel number
      REAL  XINC                      ! the x-axis pixel increment
      REAL YINC                      ! the y-axis pixel increment
      REAL DISTSQ                                ! Pixel distance squared
      REAL RDIST                                 ! Size in pixels of res el
      REAL RDIST_SQ                              ! RDIST squared
      INTEGER PIX_RANGE                          ! Pixel range
      REAL RES_ELEMENT                           ! Resolution element

*   local data
*.

      IF (STATUS .NE. SAI__OK) RETURN

*  set x and y axis pixel increments, x increases to left hence -ve.

      XINC = -PIXSPACE
      YINC = PIXSPACE

*  find the size of the resolution element in pixels
      RDIST = EFF_RADIUS / PIXSPACE
      PIX_RANGE = INT (RDIST) + 1
      RDIST_SQ = RDIST * RDIST

*  initialise the scratch array

      DO JOUT = 1, NJ
         DO IOUT = 1, NI
            QUALITY (IOUT,JOUT) = 1
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
                           QUALITY (IOUT+I, JOUT+J) = 0
                        END IF
                     END IF
                  END DO
               END IF
            END DO
         END IF
      END DO

      END
