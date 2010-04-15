      SUBROUTINE SCULIB_FIT_PLANE (N, X, Y, Z, QUALITY, MX, MY, C,
     :  STATUS)
*+
*  Name:
*     SCULIB_FIT_PLANE

*  Purpose:
*     fits a plane to some x, y, z data

*  Description:
*     This routine does a least squares fit of data to a plane. The plane
*     is described by an equation of form:-
*
*          z = MX * x  +  MY * y  +  C
*

*  Invocation:
*     CALL SCULIB_FIT_PLANE (N, X, Y, Z, QUALITY, MX, MY, C, STATUS)

*  Arguments:
*     N                      = INTEGER (Given)
*           the number of data points
*     X (N)                  = REAL (Given)
*           the x coords of the measured points
*     Y (N)                  = REAL (Given)
*           the y coords of the measured points
*     Z (N)                  = REAL (Given)
*           the measured values
*     QUALITY (N)            = INTEGER (Given)
*           the quality on the data
*     MX                     = REAL (Returned)
*           the slope of the fitted plane in the x-axis
*     MY                     = REAL (Returned)
*           the slope of the fitted plane in the y-axis
*     C                      = REAL (Returned)
*           the constant of the fitted plane
*     STATUS                 = INTEGER (Given and returned)
*           global status

*  Method:
*     If status is good on entry the routine will loop through the
*     input data calculating the sums required and the number of valid
*     data points (i.e. those with good quality flags).
*
*     If there are less than 3 valid data then
*        an error message will be output and bad status returned
*     else
*        the plane coefficients will be calculated from the following
*        formulae:-
*
*        MX = nv.sum(y^2).sum(xz) - sum(y^2).sum(x).sum(z) -
*               sum(y).sum(y).sum(xz) - nv.sum(xy).sum(yz) +
*               sum(xy).sum(y).sum(z) + sum(x).sum(y).sum(yz)
*            .-------------------------------------------------
*             sum(xy).sum(x).sum(y) - nv.sum(xy).sum(xy) +
*               sum(x).sum(y).sum(xy) - sum(x).sum(x).sum(y^2) +
*               nv.sum(x^2).sum(y^2) + sum(x^2).sum(y).sum(y)
*
*        MY = nv.sum(yz) - sum(y).sum(z) + MX.[sum(x).sum(y) - nv.sum(xy)]
*            .------------------------------------------------------------
*                         nv.sum(y^2) - sum(y).sum(y)
*
*         C = sum(z) - MX.sum(x) - MY.sum(y)
*            .------------------------------
*                       nv
*
*        where nv is the number of valid data points. If the denominator of the
*        expression for MX is 0 then an error message will be output and bad
*        status returned.
*     end if

*  Bugs:

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Deficiencies:

*  History:
*     $Id$
*     10-AUG-1994: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N
      REAL X(N), Y(N), Z(N)
      INTEGER QUALITY (N)

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL MX, MY, C

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I                             ! DO loop variable
      INTEGER NDATA                         ! number of valid data points
      REAL SUMX, SUMY, SUMZ                 ! sums of valid x, y, z
      REAL SUMXY, SUMXZ, SUMYZ              ! sums of valid xy, xz, yz
      REAL SUMX2, SUMY2                     ! sum of valid x^2, y^2
      REAL DENOM                            ! denominator in calculation of MX

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      MX = 0.0
      MY = 0.0
      C = 0.0

*  go through valid data and calculate sums required

      NDATA = 0
      SUMX = 0.0
      SUMY = 0.0
      SUMZ = 0.0
      SUMXY = 0.0
      SUMXZ = 0.0
      SUMYZ = 0.0
      SUMX2 = 0.0
      SUMY2 = 0.0

      IF (N .GT. 0) THEN
         DO I = 1, N
            IF (QUALITY(I) .EQ. 0) THEN
               SUMX = SUMX + X(I)
               SUMY = SUMY + Y(I)
               SUMZ = SUMZ + Z(I)
               SUMXY = SUMXY + X(I) * Y(I)
               SUMXZ = SUMXZ + X(I) * Z(I)
               SUMYZ = SUMYZ + Y(I) * Z(I)
               SUMX2 = SUMX2 + X(I) * X(I)
               SUMY2 = SUMY2 + Y(I) * Y(I)
               NDATA = NDATA + 1
            END IF
         END DO
      END IF

*  check enough data to fit plane

      IF (NDATA .LT. 3) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_FIT_PLANE: insufficient valid '//
     :     'data to fit plane', STATUS)
      ELSE

         DENOM = SUMXY * SUMX * SUMY - REAL(NDATA) * SUMXY * SUMXY +
     :     SUMX *SUMY * SUMXY - SUMX * SUMX * SUMY2 +
     :     REAL(NDATA) * SUMX2 * SUMY2 + SUMX2 * SUMY * SUMY

         IF (DENOM .EQ. 0.0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_FIT_PLANE: zero denominator '//
     :        'in calculation of MX', STATUS)
         ELSE

            MX = (REAL(NDATA) * SUMY2 * SUMXZ - SUMY2 * SUMX * SUMZ -
     :        SUMY * SUMY * SUMXZ - REAL(NDATA) * SUMXY * SUMYZ +
     :        SUMXY * SUMY * SUMZ + SUMX * SUMY * SUMYZ) / DENOM

            MY = (REAL(NDATA) * SUMYZ - SUMY * SUMZ +
     :        MX * (SUMX * SUMY - REAL(NDATA) * SUMXY)) /
     :        (REAL(NDATA) * SUMY2 - SUMY * SUMY)

            C = (SUMZ - MY * SUMY - MX * SUMX) / REAL (NDATA)

         END IF

      END IF

      END
