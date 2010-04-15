      SUBROUTINE SCULIB_UNPACK (NBS_DIM1, NBS_DIM2, NBS_DIM3,
     :   RESNBS, ISTART, NPIX, POINTER, SUB, RES_DIM1, RES_DIM2,
     :   RES_DS, RES_VS, RES_WS, RES_QS)
*+
*  Name:
*     SCULIB_UNPACK

*  Purpose:
*     routine to unpack compressed resampled map

*  Description:
*     This routine unpacks a compressed SCUBA resampled data array into
*     a 2-d image. It does this by cycling up the j slices of the image
*     and -
*
*        if the packed data has pixels for that slice
*          - fill the image slice data (data sum, variance sum and weight sum)
*             with zeroes and bad quality before the packed slice starts.
*          - copy the data and quality of the packed slice into the appropriate
*             part of the image slice
*          - fill the image slice with zeroes and bad quality after the packed
*             slice ends.
*
*        else
*          - fill the image slice with zeroes and bad quality
*
*        endif

*  Invocation:
*     CALL SCULIB_UNPACK (NBS_DIM1, NBS_DIM2, NBS_DIM3,
*    :   RESNBS, ISTART, NPIX, POINTER, SUB, RES_DIM1, RES_DIM2,
*    :   RES_DS, RES_VS, RES_WS, RES_QS)

*  Arguments:
*     NBS_DIM1                            = INTEGER (Given)
*           first dimension of the RESNBS array
*     NBS_DIM2                            = INTEGER (Given)
*           second dimension of the RESNBS array
*     NBS_DIM3                            = INTEGER (Given)
*           third dimension of the RESNBS array
*     RESNBS (NBS_DIM1, NBS_DIM2, NBS_DIM3)) = REAL (Given)
*           the noticeboard `resampled data'
*     ISTART (RES_DIM2)                   = INTEGER (Given)
*           the I index of the start of each packed slice
*     NPIX (RES_DIM2)                     = INTEGER (Given)
*           the number of pixels in each packed slice
*     POINTER (RES_DIM2)                  = INTEGER (Given)
*           the pointer to the start of each packed slice
*     SUB                                 = INTEGER (Given)
*           the number of the sub-instrument map to be unpacked
*     RES_DIM1                            = INTEGER (Given)
*           first dimension of resampled image array
*     RES_DIM2                            = INTEGER (Given)
*           second dimension of resampled image array
*     RES_DS (RES_DIM1, RES_DIM2)         = REAL (Returned)
*           the unpacked data sum
*     RES_VS (RES_DIM1, RES_DIM2)         = REAL (Returned)
*           the unpacked variance sum
*     RES_WS (RES_DIM1, RES_DIM2)         = REAL (Returned)
*           the unpacked weight sum
*     RES_QS (RES_DIM1, RES_DIM2)         = INTEGER (Returned)
*           the unpacked quality


*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1993-1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     22 Jan 1993 (JFL):
*       Original Version

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER NBS_DIM1, NBS_DIM2, NBS_DIM3
      REAL RESNBS (NBS_DIM1, NBS_DIM2, NBS_DIM3)
      INTEGER RES_DIM1, RES_DIM2
      INTEGER ISTART (RES_DIM2)
      INTEGER NPIX (RES_DIM2)
      INTEGER POINTER (RES_DIM2)
      INTEGER SUB

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL RES_DS (RES_DIM1, RES_DIM2)
      REAL RES_VS (RES_DIM1, RES_DIM2)
      REAL RES_WS (RES_DIM1, RES_DIM2)
      INTEGER RES_QS (RES_DIM1, RES_DIM2)

*  Status:

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I                            ! DO loop index
      INTEGER J                            !      "

*  Internal References:

*  Local data:

*.

      DO J = 1, RES_DIM2

         IF (NPIX(J) .GT. 0) THEN

*  there are some pixels in this slice
*  ..first fill the part of the array before the slice starts

            IF (ISTART(J) .GT. 1) THEN
               DO I = 1, ISTART(J) - 1
                  RES_DS (I,J) = 0.0
                  RES_VS (I,J) = 0.0
                  RES_WS (I,J) = 0.0
                  RES_QS (I,J) = 1
               END DO
            END IF

*  ..copy over the slice

            DO I = ISTART(J), ISTART(J) + NPIX(J) - 1
               RES_DS (I,J) = RESNBS (1, SUB, POINTER(J)+I-ISTART(J))
               RES_VS (I,J) = RESNBS (2, SUB, POINTER(J)+I-ISTART(J))
               RES_WS (I,J) = RESNBS (3, SUB, POINTER(J)+I-ISTART(J))
               RES_QS (I,J) =
     :           NINT (RESNBS (4, SUB, POINTER(J)+I-ISTART(J)))
            END DO

*  ..and fill in bit after slice ends

            IF (ISTART(J) + NPIX(J) .LE. RES_DIM1) THEN
               DO I = ISTART(J) + NPIX(J), RES_DIM1
                  RES_DS (I,J) = 0.0
                  RES_VS (I,J) = 0.0
                  RES_WS (I,J) = 0.0
                  RES_QS (I,J) = 1
               END DO
            END IF

         ELSE

*  ..no pixels in slice

            DO I = 1, RES_DIM1
               RES_DS (I,J) = 0.0
               RES_VS (I,J) = 0.0
               RES_WS (I,J) = 0.0
               RES_QS (I,J) = 1
            END DO

         END IF

      END DO

      END
