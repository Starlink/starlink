      SUBROUTINE SCULIB_COADD_REMOVE (N, IN_DATA, IN_VARIANCE,
     :  IN_QUALITY, INCOADD_DATA, INCOADD_VAR, INCOADD_QUAL,
     :  INCOADD_NUMBER, OUTCOADD_DATA, OUTCOADD_VAR, OUTCOADD_QUAL,
     :  OUTCOADD_NUMBER, VARIANCE)
*+
*  Name:
*     SCULIB_COADD_REMOVE

*  Purpose:
*     remove exposure from coadded result

*  Description:
*     This routine removes an exposure from the input coadd and puts the
*     result in the output coadd arrays. The input and output
*     arrays can be the same. The coadd result is the average of exposures
*     remaining in the coadd. The coadd variance is calculated from
*     the spread of the exposures about the mean if more than one exposure
*     remains, 0 otherwise. Exposure pixels with bad quality are ignored.

*  Invocation:
*     CALL SCULIB_COADD_REMOVE (N, IN_DATA, IN_VARIANCE, IN_QUALITY,
*    :  INCOADD_DATA, INCOADD_VAR, INCOADD_QUAL, INCOADD_NUMBER,
*    :  OUTCOADD_DATA, OUTCOADD_VAR, OUTCOADD_QUAL, OUTCOADD_NUMBER,
*    :  VARIANCE)

*  Arguments:
*     N                       = INTEGER (Given)
*           Number of elements in arrays.
*     IN_DATA (N)             = REAL (Given)
*           data to be added to coadd
*     IN_VARIANCE (N)         = REAL (Given)
*           variance on input data
*     IN_QUALITY (N)          = INTEGER (Given)
*           quality on input data
*     INCOADD_DATA (N)        = REAL (Given)
*           Input coadd.
*     INCOADD_VAR (N)         = REAL (Given)
*           Input coadd variance.
*     INCOADD_QUAL (N)        = INTEGER (Given)
*           Input coadd quality
*     INCOADD_NUMBER (N)      = INTEGER (Given)
*           Number of exposures coadded in input coadd.
*     OUTCOADD_DATA (N)       = REAL (Returned)
*           Output coadd.
*     OUTCOADD_VAR (N)        = REAL (Returned)
*           Output coadd variance.
*     OUTCOADD_QUAL (N)       = INTEGER (returned)
*           Output coadd quality.
*     OUTCOADD_NUMBER (N)     = INTEGER (Returned)
*           Number of exposures coadded in output coadd.
*     VARIANCE                = LOGICAL (Given)
*           T if input data has variance associated with it

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     5-JUL-1994: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:

*  Arguments Given:
      INTEGER N
      REAL IN_DATA (N)
      REAL IN_VARIANCE (N)
      INTEGER IN_QUALITY (N)
      REAL INCOADD_DATA (N)
      REAL INCOADD_VAR (N)
      INTEGER INCOADD_QUAL (N)
      INTEGER INCOADD_NUMBER (N)
      LOGICAL VARIANCE

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL OUTCOADD_DATA (N)
      REAL OUTCOADD_VAR (N)
      INTEGER OUTCOADD_QUAL (N)
      INTEGER OUTCOADD_NUMBER (N)

*  Status:

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I                   ! DO loop
      REAL SUM
      REAL SUMSQ

*  Internal References:

*  Local data:

*.

      DO I = 1, N

         IF (IN_QUALITY(I) .EQ. 0) THEN

*  good quality input point

            IF (INCOADD_NUMBER(I) .LE. 1) THEN

*  ..only one number in coadd so reset coadd to 0 and bad quality

               OUTCOADD_DATA(I) = 0.0
               OUTCOADD_QUAL(I) = 1
               OUTCOADD_NUMBER(I) = 0
               OUTCOADD_VAR(I) = 0.0
            ELSE

*  ..recover the sum of the data points and the sum of them squared

               SUM = INCOADD_DATA (I) * INCOADD_NUMBER (I)
               SUMSQ = INCOADD_NUMBER (I) * INCOADD_DATA(I)**2 +
     :            INCOADD_NUMBER(I) * (INCOADD_NUMBER(I)-1) *
     :            INCOADD_VAR (I)

*  ..subtract the exposure from the coadd

               SUM = SUM - IN_DATA (I)
               SUMSQ = SUMSQ - IN_DATA(I)**2
               OUTCOADD_NUMBER (I) = INCOADD_NUMBER (I) - 1
               OUTCOADD_QUAL (I) = 0

*  ..calculate the new average and variance

               OUTCOADD_DATA (I) = SUM / OUTCOADD_NUMBER (I)
               IF (OUTCOADD_NUMBER(I) .GT. 1) THEN
                  OUTCOADD_VAR (I) =
     :              (SUMSQ - OUTCOADD_NUMBER(I)*OUTCOADD_DATA(I)**2) /
     :              (OUTCOADD_NUMBER(I) * (OUTCOADD_NUMBER(I)-1))
               ELSE
                  OUTCOADD_VAR (I) = 0.0
               END IF
            END IF

         END IF
      END DO

      END
