

      SUBROUTINE ELF1_SOLVE(NP,XV,YV,S,C,STATUS)
*+
*  Name:
*     ELF1_SOLVE

*  Purpose:
*     To look at the variation in brightness around the ellipse
*     chosen and see what SIN and COS factors are present.
*
*     The method used is a crude matrix inversion.
*
*     This is a temporary routine used only until PDA is working properly.
*     The current (JAN 1997) version of PDA_DBOLS falls over in some
*     instances if no compiled with an FFLAG of -check nobounds
*
*     This method is slow but stable so it is necessary to cut down the
*     number of points used when it exceeds 90.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_SOLVE(NP,XV,YV,S,C,STATUS)

*  Arguments:
*     NP = INTEGER (Given)
*        The number of pixels that have an associated brightness.
*     XV(500) = DOUBLE PRECISION (Given)
*        The array containing the angle values in radians.
*     YV(500) = DOUBLE PRECISION (Given)
*        The array containing the brightness values.
*     S = DOUBLE PRECISION (Returned)
*        The SINE function amplitude.
*     C = DOUBLE PRECISION (Returned)
*        The COSINE function amplitude.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     2-May-1996
*     (Original version)
*     26-Jan-1997
*     Modified to improve speed and robustness.

*  Notes:
*     This routine is not documented and is only to be used until
*     the PDA library has been modified or recompiled so that
*     the PDA_DBOLS routine does not fall over with an array out of
*     bounds error.
*
*     This routine was required quickly for an emergency bug fix.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER NP                      ! Number of data points
      DOUBLE PRECISION XV(500)        ! Angle values
      DOUBLE PRECISION YV(500)        ! Brightness values

*  Arguments Returned:
      DOUBLE PRECISION C              ! Cosine function amplitude
      DOUBLE PRECISION S              ! Sine function amplitude

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      DOUBLE PRECISION CO
      DOUBLE PRECISION D
      DOUBLE PRECISION H(500)
      DOUBLE PRECISION K2
      DOUBLE PRECISION P(3)
      DOUBLE PRECISION TRACE
      DOUBLE PRECISION W(3,500)
      DOUBLE PRECISION X(500,3)
      DOUBLE PRECISION X1
      DOUBLE PRECISION X2
      DOUBLE PRECISION XV2(500)
      DOUBLE PRECISION Y(3,500)
      DOUBLE PRECISION YV2(500)
      DOUBLE PRECISION Z(500,500)
      DOUBLE PRECISION ZERO           ! Double zero
      DOUBLE PRECISION TEMP           ! A temporary value
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER L                       ! Loop variable
      INTEGER M                       ! Number of variables
      INTEGER N                       ! Number of points used
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Number of variables (ie sine and cosine and a constant).
      M=2

      N=NP
      DO WHILE (N.GT.90)

*      Average two points (done in sets of 4 since data is organised
*      so that sets of 4 points are all from different quadrants).
         J=1
         DO 10 I=1,N-6,8
            DO 12 K=0,3
               XV2(J)=(XV(I+K)+XV(I+K+4))/2.
               YV2(J)=(YV(I+K)+YV(I+K+4))/2.
               J=J+1
 12         CONTINUE
 10      CONTINUE

*      Modify the number of points available.
         N=J

*      Put results back into the source array.
         DO 11 I=1,N
            XV(I)=XV2(I)
            YV(I)=YV2(I)
 11      CONTINUE

      END DO

*   Set double precision zero.
      ZERO=0.D0

*   Preparing the input arrays.,
      DO 200 I = 1,N
         H(I) =   YV(I)
         X(I,1) = SIN(XV(I))
         X(I,2) = COS(XV(I))
 200  CONTINUE

*   Starting to process.
      DO 300 J = 1,M
        DO 400 I = 1,N
          W(J, I) = X(I, J)
 400    CONTINUE
 300  CONTINUE

      K2 = ZERO
      DO 500 J = 1,N
        DO 600 I = 1,N
          Z(I, J) = ZERO
          DO 700 L = 1,M
            Z(I, J) = Z(I, J) + X(I, L) * W(L, J)
 700      CONTINUE
          K2 = K2 + ABS(Z(I, J))
 600    CONTINUE
 500  CONTINUE
      K2 = 1.0D0 / K2

*   Threshold value.
      D =1.D-3

      DO 800 J = 1,N
        DO 900 I = 1,M
          Y(I, J) = K2 * W(I, J)
 900    CONTINUE
 800  CONTINUE

*   Cycle around until we get the right threshold.
      X1=D*2.
      DO WHILE (X1.GT.D)

         DO 1000 I = 1,N
           DO 1100 J = 1,N
             Z(I, J) = ZERO
             DO 1200 L = 1,M
               Z(I, J) = Z(I, J) + X(I, L) * Y(L, J)
 1200        CONTINUE
 1100      CONTINUE
 1000    CONTINUE

*      Reset the array trace.
         TRACE = ZERO
         CO = 2.0D0
         DO 1300 I = 1,N
           Z(I, I) = Z(I, I) - CO
           TRACE = TRACE + Z(I, I)
 1300    CONTINUE

         DO 1400 J = 1,N
           DO 1500 I = 1,M
             W(I, J) = ZERO
             DO 1600 L = 1,N
               W(I, J) = W(I, J) + Y(I, L) * Z(L, J)
 1600        CONTINUE
 1500      CONTINUE
 1400    CONTINUE

         DO 1700 J = 1,N
           DO 1800 I = 1,M
             Y(I, J) = -W(I, J)
 1800      CONTINUE
 1700    CONTINUE

*      Consider the residuals.
         X1 = ABS(TRACE - INT(TRACE) - 1)
         X2 = ABS(TRACE - INT(TRACE))
         IF (X2.LT.X1) THEN
            TEMP=X1
            X1=X2
            X2=TEMP
         END IF

*      Increment threshold to avoid getting stuck.
         D=D+1.D-4

      END DO

*   Generate the output parameters.
      DO 1900 I = 1,M
        P(I) = ZERO
        DO 2000 J = 1,N
          P(I) = P(I) + Y(I, J) * H(J)
 2000   CONTINUE
 1900 CONTINUE

*   Assign the parameters.
      S=P(1)
      C=P(2)

 9999 CONTINUE

      END



      SUBROUTINE ELP1_SOLVE(NP,XV,YV,S,C,STATUS)
*+
*  Name:
*     ELP1_SOLVE

*  Purpose:
*     To look at the variation in brightness around the ellipse
*     chosen and see what SIN and COS factors are present.
*
*     The method used is a crude matrix inversion.
*
*     This is a temporary routine used only until PDA is working properly.
*     The current (JAN 1997) version of PDA_DBOLS falls over in some
*     instances if no compiled with an FFLAG of -check nobounds
*
*     This method is slow but stable so it is necessary to cut down the
*     number of points used when it exceeds 90.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_SOLVE(NP,XV,YV,S,C,STATUS)

*  Arguments:
*     NP = INTEGER (Given)
*        The number of pixels that have an associated brightness.
*     XV(500) = DOUBLE PRECISION (Given)
*        The array containing the angle values in radians.
*     YV(500) = DOUBLE PRECISION (Given)
*        The array containing the brightness values.
*     S = DOUBLE PRECISION (Returned)
*        The SINE function amplitude.
*     C = DOUBLE PRECISION (Returned)
*        The COSINE function amplitude.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     2-May-1996
*     (Original version)
*     26-Jan-1997
*     Modified to improve speed and robustness.

*  Notes:
*     This routine is not documented and is only to be used until
*     the PDA library has been modified or recompiled so that
*     the PDA_DBOLS routine does not fall over with an array out of
*     bounds error.
*
*     This routine was required quickly for an emergency bug fix.

*  Bugs:
*     None known.

*-
*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER NP                      ! Number of data points
      DOUBLE PRECISION XV(500)        ! Angle values
      DOUBLE PRECISION YV(500)        ! Brightness values

*  Arguments Returned:
      DOUBLE PRECISION C              ! Cosine function amplitude
      DOUBLE PRECISION S              ! Sine function amplitude

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      DOUBLE PRECISION CO
      DOUBLE PRECISION D
      DOUBLE PRECISION H(500)
      DOUBLE PRECISION K2
      DOUBLE PRECISION P(3)
      DOUBLE PRECISION TRACE
      DOUBLE PRECISION W(3,500)
      DOUBLE PRECISION X(500,3)
      DOUBLE PRECISION X1
      DOUBLE PRECISION X2
      DOUBLE PRECISION XV2(500)
      DOUBLE PRECISION Y(3,500)
      DOUBLE PRECISION YV2(500)
      DOUBLE PRECISION Z(500,500)
      DOUBLE PRECISION ZERO           ! Double zero
      DOUBLE PRECISION TEMP           ! A temporary value
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER L                       ! Loop variable
      INTEGER M                       ! Number of variables
      INTEGER N                       ! Number of points used
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Number of variables (ie sine and cosine and a constant).
      M=2

      N=NP
      DO WHILE (N.GT.90)

*      Average two points (done in sets of 4 since data is organised
*      so that sets of 4 points are all from different quadrants).
         J=1
         DO 10 I=1,N-6,8
            DO 12 K=0,3
               XV2(J)=(XV(I+K)+XV(I+K+4))/2.
               YV2(J)=(YV(I+K)+YV(I+K+4))/2.
               J=J+1
 12         CONTINUE
 10      CONTINUE

*      Modify the number of points available.
         N=J

*      Put results back into the source array.
         DO 11 I=1,N
            XV(I)=XV2(I)
            YV(I)=YV2(I)
 11      CONTINUE

      END DO

*   Set double precision zero.
      ZERO=0.D0

*   Preparing the input arrays.,
      DO 200 I = 1,N
         H(I) =   YV(I)
         X(I,1) = SIN(XV(I))
         X(I,2) = COS(XV(I))
 200  CONTINUE

*   Starting to process.
      DO 300 J = 1,M
        DO 400 I = 1,N
          W(J, I) = X(I, J)
 400    CONTINUE
 300  CONTINUE

      K2 = ZERO
      DO 500 J = 1,N
        DO 600 I = 1,N
          Z(I, J) = ZERO
          DO 700 L = 1,M
            Z(I, J) = Z(I, J) + X(I, L) * W(L, J)
 700      CONTINUE
          K2 = K2 + ABS(Z(I, J))
 600    CONTINUE
 500  CONTINUE
      K2 = 1.0D0 / K2

*   Threshold value.
      D =1.D-3

      DO 800 J = 1,N
        DO 900 I = 1,M
          Y(I, J) = K2 * W(I, J)
 900    CONTINUE
 800  CONTINUE

*   Cycle around until we get the right threshold.
      X1=D*2.
      DO WHILE (X1.GT.D)

         DO 1000 I = 1,N
           DO 1100 J = 1,N
             Z(I, J) = ZERO
             DO 1200 L = 1,M
               Z(I, J) = Z(I, J) + X(I, L) * Y(L, J)
 1200        CONTINUE
 1100      CONTINUE
 1000    CONTINUE

*      Reset the array trace.
         TRACE = ZERO
         CO = 2.0D0
         DO 1300 I = 1,N
           Z(I, I) = Z(I, I) - CO
           TRACE = TRACE + Z(I, I)
 1300    CONTINUE

         DO 1400 J = 1,N
           DO 1500 I = 1,M
             W(I, J) = ZERO
             DO 1600 L = 1,N
               W(I, J) = W(I, J) + Y(I, L) * Z(L, J)
 1600        CONTINUE
 1500      CONTINUE
 1400    CONTINUE

         DO 1700 J = 1,N
           DO 1800 I = 1,M
             Y(I, J) = -W(I, J)
 1800      CONTINUE
 1700    CONTINUE

*      Consider the residuals.
         X1 = ABS(TRACE - INT(TRACE) - 1)
         X2 = ABS(TRACE - INT(TRACE))
         IF (X2.LT.X1) THEN
            TEMP=X1
            X1=X2
            X2=TEMP
         END IF

*      Increment threshold to avoid getting stuck.
         D=D+1.D-4

      END DO

*   Generate the output parameters.
      DO 1900 I = 1,M
        P(I) = ZERO
        DO 2000 J = 1,N
          P(I) = P(I) + Y(I, J) * H(J)
 2000   CONTINUE
 1900 CONTINUE

*   Assign the parameters.
      S=P(1)
      C=P(2)

 9999 CONTINUE

      END
