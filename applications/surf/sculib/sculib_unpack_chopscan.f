      SUBROUTINE SCULIB_UNPACK_CHOPSCAN (RESNBS, N_POINTS, ISTART,
     :   NPIX, POINTER, RESDATA_450, RESWT_450, RESDATA_850, RESWT_850,
     :   RESDIM1, RESDIM2, BAD)
*+
*  Name:
*     SCULIB_UNPACK_CHOPSCAN

*  Purpose:
*     routine to unpack compressed chop-scan data

*  Description:
*     Used by the on-line system to unpack notice-board entries
*     and retrieve demodulated data.

*  Invocation:
*     CALL SCULIB_UNPACK_CHOPSCAN (RESNBS, N_POINTS, ISTART,
*    :   NPIX, POINTER, RESDATA_450, RESWT_450, RESDATA_850, RESWT_850,
*    :   RESDIM1, RESDIM2, BAD)

*  Arguments:
*     RESNBS (4, N_POINTS)                = REAL (Given)
*           the `resampled data' noticeboard
*     N_POINTS                            = INTEGER (Given)
*           the 2nd dimension of the RESNBS array
*     ISTART (RESDIM2)                    = INTEGER (Given)
*           the I index of the start of the packed slice at index J
*     NPIX (RESDIM2)                      = INTEGER (Given)
*           the number of pixels in the packed slice for row J
*     POINTER (RESDIM2)                   = INTEGER (Given)
*           the pointer to the start of the slice in the packed array
*     RESDATA_450 (RESDIM1, RESDIM2)      = REAL (Returned)
*           the unpacked 450 data
*     RESWT_450 (RESDIM1, RESDIM2)        = REAL (Returned)
*           450 weights
*     RESDATA_850 (RESDIM1, RESDIM2)      = REAL (Returned)
*           the unpacked 850 image
*     RESWT_850 (RESDIM1, RESDIM2)        = REAL (Returned)
*           850 weights
*     RESDIM1                             = INTEGER (Given)
*           first dimension of resampled image array
*     RESDIM2                             = INTEGER (Given)
*           second dimension
*     BAD                                 = REAL (Given)
*           value signalling bad pixel


*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Bugs:


*  History:
*     $Id$
*     $Log$
*     Revision 1.4  1999/08/19 03:37:31  timj
*     Header tweaks to ease production of SSN72 documentation.
*

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N_POINTS
      REAL RESNBS (4, N_POINTS)
      INTEGER RESDIM1
      INTEGER RESDIM2
      INTEGER ISTART (RESDIM2)
      INTEGER NPIX (RESDIM2)
      INTEGER POINTER (RESDIM2)
      REAL BAD

*  Arguments Given & Returned:
      REAL RESDATA_450 (RESDIM1, RESDIM2)
      REAL RESWT_450 (RESDIM1, RESDIM2)
      REAL RESDATA_850 (RESDIM1, RESDIM2)
      REAL RESWT_850 (RESDIM1, RESDIM2)

*  Arguments Returned:

*  Status:

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I, J

*  Internal References:

*  Local data:

*.

*  set the output arrays to bad values

      DO J = 1, RESDIM2
         DO I = 1, RESDIM1
            RESDATA_450 (I, J) = 0.0
            RESWT_450 (I, J) = 0.0
            RESDATA_850 (I, J) = 0.0
            RESWT_850 (I, J) = 0.0
         END DO
      END DO

*  cycle through J dimension of resampled array

      DO J =  1, RESDIM2

*  if there are any pixels for this slice..

         IF (NPIX(J) .GT. 0) THEN

*  cycle along the slice and get them in

            DO I = ISTART(J), ISTART(J) + NPIX(J) - 1

               IF (RESNBS(1, POINTER(J)+I-ISTART(J)) .NE. BAD) THEN
                  RESDATA_450 (I, J) = RESNBS (1,POINTER(J)+I-ISTART(J))
               ELSE
                  RESDATA_450 (I, J) = 0.0
               END IF

               IF (RESNBS(2, POINTER(J)+I-ISTART(J)) .NE. BAD) THEN
                  RESWT_450 (I, J) = RESNBS (2, POINTER(J)+I-ISTART(J))
               ELSE
                  RESWT_450 (I, J) = 0.0
               END IF

               IF (RESNBS(3, POINTER(J)+I-ISTART(J)) .NE. BAD) THEN
                  RESDATA_850 (I, J) = RESNBS (3,POINTER(J)+I-ISTART(J))
               ELSE
                  RESDATA_850 (I, J) = 0.0
               END IF

               IF (RESNBS(4, POINTER(J)+I-ISTART(J)) .NE. BAD) THEN
                  RESWT_850 (I, J) = RESNBS (4, POINTER(J)+I-ISTART(J))
               ELSE
                  RESWT_850 (I, J) = 0.0
               END IF

            END DO

         END IF

      END DO

      END
