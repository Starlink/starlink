      SUBROUTINE SCULIB_FIT_2D_PARABOLA (N, DATA, VARIANCE, QUALITY,
     :  X, Y, A0, A1, X0, Y0, Z_PEAK, Z_PEAK_VAR, BADBIT, STATUS)
*+
*  Name:
*     SCULIB_FIT_2D_PARABOLA

*  Purpose:
*     fit 2d parabola to data

*  Description:
*     This routine performs a least-squares fit of a 2d parabola to a set
*     of data. The form of the parabola is:-
*
*     z = A0 + A1 * (x - X0)**2 + A1 * (y - Y0)**2
*
*     The apex of the parabola is at X0,Y0 and its value there is A0.
*
*        If status is good on entry the routine will work through the data
*     calculating the various sums required for the least-squares method.
*     Data with bad quality or zero variance are ignored. If no `good' data
*     are found then an error will be reported and the routine will return
*     with bad status.
*
*        A matrix equation of the form M * B = Z is constructed, where:-
*
*           M =
*
*     S[1/var]         S[x/var]          S[y/var]         S[(x^2+y^2)/var]
*
*     S[x/var]         S[x^2/var]        S[xy/var]        S[(x^3+xy^2)/var]
*
*     S[y/var]         S[xy/var]         S[y^2/var]       S[(x^2y+y^3)/var]
*
*     S[(x^2+y^2)/var] S[(x^3+xy^2)/var] S[(x^2y+y^3)/var] S[(x^2+y^2)^2/var]
*
*
*           B = A0 + A1 * X0^2 + A1 * Y0^2
*
*                      - 2 * A1 * X0
*
*                      - 2 * A1 * Y0
*
*                          A1
*
*
*           Z = S[z/var]
*
*               S[xz/var]
*
*               S[yz/var]
*
*               S[(x^2+y^2)z/var]
*
*     where x, y, z and var are the x,y coords, value and variance of the
*     measured points and S[...] denotes the sum of the expression over all
*     valid points.
*
*        SCULIB_INVERT_MATRIX is called to invert the matrix and
*     SCULIB_FIT_MULT to multiply Z by the inverse. This yields the fitted
*     value for B from which the other fit parameters are derived.

*  Invocation:
*     CALL SCULIB_FIT_2D_PARABOLA (N, DATA, VARIANCE, QUALITY,
*    :  X, Y, A0, A1, X0, Y0, Z_PEAK, Z_PEAK_VAR, BADBIT, STATUS)

*  Arguments:
*     N                      = INTEGER (Given)
*           number of data points
*     DATA (N)               = REAL (Given)
*           the data
*     VARIANCE (N)           = REAL (Given)
*           variance on the data
*     QUALITY (N)            = BYTE (Given)
*           the quality on the data
*     X (N)                  = REAL (Given)
*           the x offsets of the data
*     Y (N)                  = REAL (Given)
*           the y offsets of the data
*     A0                     = REAL (Returned)
*           fit result
*     A1                     = REAL (Returned)
*           fit result
*     X0                     = REAL (Returned)
*           x coord of parabola apex
*     Y0                     = REAL (Returned)
*           y coord of parabola apex
*     Z_PEAK                 = REAL (Returned)
*           the value of the parabola apex
*     Z_PEAK_VAR             = REAL (Returned)
*           the variance on Z_PEAK
*     BADBIT                 = BYTE (Given)
*           bad bit mask
*     STATUS                 = INTEGER (Given and returned)
*           global status


*  Notes:
*     If the variances are not a true reflection of the errors
*     on the data then very strange numbers can result.

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     22-NOV-1995: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      BYTE BADBIT
      INTEGER N
      REAL DATA (N)
      REAL VARIANCE (N)
      BYTE QUALITY (N)
      REAL X (N)
      REAL Y (N)

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL A0
      REAL A1
      REAL X0
      REAL Y0
      REAL Z_PEAK
      REAL Z_PEAK_VAR

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION B (4)         ! solution of matrix equation
      DOUBLE PRECISION DET           ! the determinant of the matrix
      INTEGER          I             ! DO loop variable
      INTEGER          IK (4)        ! scratch space for SCULIB_INVERT_MATRIX
      INTEGER          JK (4)        ! scratch space for SCULIB_INVERT_MATRIX
      DOUBLE PRECISION MATRIX (4,4)  ! the matrix
      INTEGER          N_GOOD        ! the number of good data points
      DOUBLE PRECISION RHS (4)       ! right-hand-side of matrix equation
      REAL             SUM_INVAR     ! sum of 1 / variance
      REAL             SUM_X         ! sum of x / variance
      REAL             SUM_XY        ! sum of xy / variance
      REAL             SUM_XZ        ! sum of x * data / variance
      REAL             SUM_X2        ! sum of x^2 / variance
      REAL             SUM_X2YY3     ! sum of x^2y + y^3 / variance
      REAL             SUM_X2Y2      ! sum of x^2 + y^2 / variance
      REAL             SUM_X2Y2Z     ! sum of (x^2 + y^2) * data /variance
      REAL             SUM_X3XY2     ! sum of x^3 + xy^2 / variance
      REAL             SUM_X4Y4      ! sum of (x^2 + y^2)^2 / variance
      REAL             SUM_Y         ! sum of y / variance
      REAL             SUM_YZ        ! sum of y * data / variance
      REAL             SUM_Y2        ! sum of y^2 / variance
      REAL             SUM_Z         ! sum of data / variance

*  Internal References:

*  Local data:

*  Local functions:
      INCLUDE 'NDF_FUNC'

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  calculate the sums needed

      N_GOOD = 0

      IF (N .GT. 0) THEN
         SUM_INVAR = 0.0
         SUM_X = 0.0
         SUM_Y = 0.0
         SUM_X2Y2 = 0.0
         SUM_X2 = 0.0
         SUM_Y2 = 0.0
         SUM_XY = 0.0
         SUM_X3XY2 = 0.0
         SUM_X2YY3 = 0.0
         SUM_X4Y4 = 0.0
         SUM_Z = 0.0
         SUM_XZ = 0.0
         SUM_YZ = 0.0
         SUM_X2Y2Z = 0.0

         DO I = 1, N
            IF ((NDF_QMASK(QUALITY(I),BADBIT))    .AND.
     :          (VARIANCE(I) .GT. 0.0)) THEN
               N_GOOD = N_GOOD + 1

*	print *, data(i), variance(i), x(i), y(i), i
               SUM_INVAR = SUM_INVAR + 1.0 / VARIANCE(I)
               SUM_X = SUM_X + X(I) / VARIANCE(I)
               SUM_Y = SUM_Y + Y(I) / VARIANCE(I)
               SUM_X2Y2 = SUM_X2Y2 + (X(I)*X(I) + Y(I)*Y(I)) /
     :           VARIANCE(I)
               SUM_X2 = SUM_X2 + X(I)*X(I) / VARIANCE(I)
               SUM_Y2 = SUM_Y2 + Y(I)*Y(I) / VARIANCE(I)
               SUM_XY = SUM_XY + X(I)*Y(I) / VARIANCE(I)
               SUM_X3XY2 = SUM_X3XY2 +
     :           (X(I)*X(I)*X(I) + X(I)*Y(I)*Y(I)) / VARIANCE(I)
               SUM_X2YY3 = SUM_X2YY3 +
     :           (X(I)*X(I)*Y(I) + Y(I)*Y(I)*Y(I)) / VARIANCE(I)
               SUM_X4Y4 = SUM_X4Y4 + (X(I)*X(I) + Y(I)*Y(I))**2 /
     :           VARIANCE(I)

               SUM_Z = SUM_Z + DATA(I) / VARIANCE(I)
               SUM_XZ = SUM_XZ + X(I)*DATA(I) / VARIANCE(I)
               SUM_YZ = SUM_YZ + Y(I)*DATA(I) / VARIANCE(I)
               SUM_X2Y2Z = SUM_X2Y2Z + (X(I)*X(I) + Y(I)*Y(I)) *
     :           DATA(I) / VARIANCE(I)
            END IF
         END DO
      END IF

      IF (N_GOOD .EQ. 0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('N', N)
         CALL ERR_REP (' ', 'SCULIB_FIT_2D_PARABOLA: no valid data '//
     :        ' in ^N input points',STATUS)

      ELSE

*  construct matrix

         MATRIX (1,1) = DBLE (SUM_INVAR)
         MATRIX (2,1) = DBLE (SUM_X)
         MATRIX (3,1) = DBLE (SUM_Y)
         MATRIX (4,1) = DBLE (SUM_X2Y2)

         MATRIX (1,2) = DBLE (SUM_X)
         MATRIX (2,2) = DBLE (SUM_X2)
         MATRIX (3,2) = DBLE (SUM_XY)
         MATRIX (4,2) = DBLE (SUM_X3XY2)

         MATRIX (1,3) = DBLE (SUM_Y)
         MATRIX (2,3) = DBLE (SUM_XY)
         MATRIX (3,3) = DBLE (SUM_Y2)
         MATRIX (4,3) = DBLE (SUM_X2YY3)

         MATRIX (1,4) = DBLE (SUM_X2Y2)
         MATRIX (2,4) = DBLE (SUM_X3XY2)
         MATRIX (3,4) = DBLE (SUM_X2YY3)
         MATRIX (4,4) = DBLE (SUM_X4Y4)

*  invert the matrix

         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_INVERT_MATRIX (4, MATRIX, DET, IK, JK, STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP(' ','FIT_2D_PARABOLA: Error inverting '//
     :              'matrix', STATUS)
            END IF
         END IF

*  set up the RHS vector

         RHS (1) = DBLE (SUM_Z)
         RHS (2) = DBLE (SUM_XZ)
         RHS (3) = DBLE (SUM_YZ)
         RHS (4) = DBLE (SUM_X2Y2Z)

*  and multiply it by the inverted matrix

         CALL SCULIB_FIT_MULT (4, MATRIX, RHS, B, STATUS)

*  finally derive the fitted parameters from the matrix solution - NOTE
*  Z_PEAK is the same as A0

         IF (STATUS .EQ. SAI__OK) THEN
            A1 = REAL (B(4))
            IF (A1 .NE. 0.0) THEN
               X0 = REAL (-B(2)) / (2.0 * A1)
               Y0 = REAL (-B(3)) / (2.0 * A1)
               A0 = REAL (B(1)) - A1 * (X0*X0 + Y0*Y0)

               Z_PEAK = REAL(B(1)) + REAL(B(2))*X0 + REAL(B(3))*Y0 +
     :              REAL(B(4))*(X0**2 + Y0**2)
               Z_PEAK_VAR = REAL(MATRIX(1,1)) +
     :              X0**2 * REAL(MATRIX(2,2)) +
     :              Y0**2 * REAL(MATRIX(3,3)) +
     :              (X0**2+Y0**2)**2 * REAL(MATRIX(4,4)) +
     :              2.0 * X0 * REAL(MATRIX(1,2)) +
     :              2.0 * Y0 * REAL(MATRIX(1,3)) +
     :              2.0 * (X0**2+Y0**2) * REAL(MATRIX(1,4)) +
     :              2.0 * X0 * Y0 * REAL(MATRIX(2,3)) +
     :              2.0 * X0 * (X0**2+Y0**2) * REAL(MATRIX(2,4)) +
     :              2.0 * Y0 * (X0**2+Y0**2) * REAL(MATRIX(3,4))
            ELSE
               IF (STATUS .EQ. SAI__OK) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP (' ', 'SCULIB_FIT_2D_PARABOLA: failed '//
     :                 'to fit - A1 = 0', STATUS)
               END IF
            END IF
         END IF
      END IF

*      print *, 'fit ', z_peak, z_peak_var, x0, y0
      END
