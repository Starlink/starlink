*+  TIM_POLY - subtracts a fitted polynomial from X.
      SUBROUTINE TIM_POLY(POLY,X,XN,Y,IW,W,N,NDEG,Z,A,C,SUMSQ)
*    Description :
*      This program subtracts a fitted polynomial from X.
*      X is normalized to give XN=(2*X-XMAX-XMIN)/(XMAX-XMIN)
*      and a polynomial in XN fitted, to reduce rounding errors.
*      If NDEG=0, then the mean value is subtracted.
*    Parameters :
*    Method :
*     Modified version of TRENDY.
*    Deficiencies :
*     Variable names need to be changed to be more meaningfull.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*     Phillip Andrews (pla@uk.ac.bham.sr.star)
*    History :
*     16 Sep 86: Version 4 (BHAVD::TJP)
*      1 Apr 87: Code structured, header added. (pla@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER    N                ! No.of X,Y pairs
      INTEGER    IW               ! IW=0 gives weighted fit
                                  !   =1 gives unweighted fit
      INTEGER    NDEG             ! Degree of polynomial ( max.power of X )

      REAL       X(*)             ! Array of values of independent variable
      REAL       W(*)             ! Array of weights proportional to inverse
      REAL       A(NDEG+2,NDEG+2) ! Work space.
      REAL       C(*)             ! Work space.
      LOGICAL    POLY             ! if true return fit, if not return resids

*    Import-Export :
      REAL       Y(*)             ! Array of dependent variable, becomes
                                  ! residuals after subtraction of polynomial
      REAL       Z(*)             ! Work space, before subtraction.
                                  ! First NDEG+1 elements return coefficients
                                  ! of polynomial in XN (in ascending powers)
*    Export :
      REAL       XN(*)            ! Normalized X
      REAL       SUMSQ            ! Sum of squares of weighted residuals
*    Status :
      INTEGER    STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER    I                ! Dummy variable for loops.
      INTEGER    II               !   "      "      "    "
      INTEGER    J                !   "      "      "    "
      INTEGER    NA               ! NDEG + 1
      INTEGER    NC               ! NDEG + 2
      INTEGER    M                ! (2*NDEG) + 1

      REAL       XMAX             ! Max value of X array.
      REAL       XMIN             ! Min value of X array.
      REAL       YMEAN            ! Mean value of Y array.
      REAL       YCONST           ! Constant component in Y fit.
      REAL       DENOM            ! Used in calculation of YMEAN.

*    Internal References :
*    Local data :
*-

*   Normalize X

      XMIN = 1.0E34
      XMAX = -1.0E34

      DO I = 1, N

         IF ( X(N) .LT. XMIN ) THEN
              XMIN = X(I)

           END IF

         IF ( X(I) .GT. XMAX ) THEN
              XMAX = X(I)

           END IF

        END DO

      DO I = 1, N
         XN(I) = ( 2*X(I)- XMAX - XMIN )/( XMAX - XMIN )

         IF (XN(I) .EQ. 0.0) THEN
            XN(I) = 1.0E-5

         END IF

      END DO

*   Remove mean if NDEG = 0
      IF (NDEG .EQ. 0) THEN
         YMEAN = 0.0

         IF (IW .EQ. 0) THEN
            DENOM = 0.0

            DO I = 1, N
               YMEAN = YMEAN + Y(I)*W(I)
               DENOM = DENOM + W(I)

            END DO
            YMEAN = YMEAN/DENOM                  ! Weighted mean
         ELSE

            DO I = 1, N
               YMEAN = YMEAN + Y(I)

            END DO
            YMEAN = YMEAN/N                      ! Unweighted mean

         END IF
         Z(1) = YMEAN
         SUMSQ = 0.0

         DO I = 1, N
            Y(I) = Y(I) - YMEAN

            IF (IW .EQ. 0) THEN
               SUMSQ = SUMSQ + W(I)*Y(I)**2

            ELSE
               SUMSQ = SUMSQ + Y(I)**2

            END IF
         END DO

      ELSE
*      Start of sao program: initialize variables.
         NA = NDEG + 1
         NC = NDEG + 2
         M  = (2*NA) - 1

         IF ( IW .LE. 0 ) THEN
            DO I = 1, M
               II = I - 1
               C(I) = 0.0
               DO J = 1, N
                  C(I) = C(I) +( W(J)*(XN(J)**II) )

               END DO
            END DO

            DO I = 1, NA
               DO J = 1, NA
                  A(I,J) = C(I + J - 1)
               END DO
            END DO

            DO I = 1, NA
               II = I - 1
               A(I,NC) = 0.0
               DO J = 1, N
                  A(I,NC) = A(I,NC) + W(J)*Y(J)*(XN(J)**II)
               END DO
            END DO

         ELSE
            DO I = 1, M
               II = I - 1
               C(I) = 0.0
               DO J = 1, N
                  C(I) = C(I) + XN(J)**II

               END DO
            END DO

            DO I = 1, NA
               DO J = 1, NA
                  A(I,J) = C(I + J - 1)
               END DO
            END DO

            DO I = 1, NA
               II = I - 1
               A(I,NC) = 0.0
               DO J = 1, N
                  A(I,NC) = A(I,NC) + Y(J)*XN(J)**II

               END DO
            END DO

         END IF
         CALL TIM_POLY_TRIANG(A,NA,NC)
         CALL TIM_POLY_SOLVE(A,NA,Z,NC)
         SUMSQ = 0.0
         DO I = 1, N
            YCONST = Z(1)
            IF ( (NA - 1 ) .NE. 0 ) THEN

               DO J=2,NA
                  YCONST=YCONST+Z(J)*XN(I)**(J-1)

               END DO

            END IF

            IF ( IW .EQ. 0 ) THEN
               SUMSQ=SUMSQ+W(I)*((Y(I)-YCONST)**2)

            ELSE
               SUMSQ=SUMSQ+(Y(I)-YCONST)**2

            END IF

	    IF ( .NOT. POLY ) THEN
               Y(I)=Y(I)-YCONST                         ! Return residual

            ELSE
               Y(I)=YCONST				! Return calc y val

            END IF
	 END DO
      END IF
      END
*23456.89.12.45.78.01234567890123456789012345678901234567890123456789012








*+   TIM_POLY_TRIANG - Triangularizes the matrix A, with M rows & M+N columns.
      SUBROUTINE TIM_POLY_TRIANG(A,M,NC)
*    Description :
*      A is the matrix with M rows and M + 1 columns to be triangularised.
*     <description of what the subroutine does>
*    History :
*     ????????: Original (BHVAD::TJP)
*     1 Apr 87: Structured and header added. (pla@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER    NC           !NDEG + 2
      INTEGER    M            !No. of rows in matrix

*    Import-Export :
      REAL       A(NC,NC)     !Matrix to be triangularized.
*    Export :
*    Local Constants :
*    Local variables :
      INTEGER    J            ! Dummy variable for loops.
      INTEGER    K            ! Dummy variable for loops.
      INTEGER    L            ! Dummy variable for loops.
      INTEGER    MM1          ! No. of rows - 1
      INTEGER    NUMCOLUMNS   ! No. of columns.
      INTEGER    MAXX         ! Max value of X?

      REAL       VALUE        ! Current element of matrix A.
*-

*   Initialize variables.
      MM1 = M - 1
      NUMCOLUMNS = M + 1

      DO J = 1, MM1
         MAXX  = J
         VALUE = ABS(A(J,J))
         DO K = (J + 1), M

            IF ( (VALUE - ABS( A(K,J) )) .LT. 0 ) THEN
               VALUE = ABS( A(K,J) )
               MAXX  = K

            END IF
         END DO

         DO K = J, NUMCOLUMNS
            VALUE     = A(MAXX,K)
            A(MAXX,K) = A(J,K)
            A(J,K)    = VALUE

         END DO

*         IF ( (MAXX-J) .NE. 0 ) THEN
*            DET = - DET

*         END IF

         VALUE = A(J,J)

         DO K = J, NUMCOLUMNS
            A(J,K) = A(J,K)/VALUE

         END DO

         DO K = (J + 1), M
            VALUE = A(K,J)
            DO L = J, NUMCOLUMNS
               A(K,L) = A(K,L) - (VALUE*A(J,L))

            END DO
         END DO
      END DO

      VALUE = A(M,M)

      DO K = M, NUMCOLUMNS
         A(M,K) = A(M,K)/VALUE

      END DO
      END
*23456.89.12.45.78.01234567890123456789012345678901234567890123456789012




*+   TIM_POLY_SOLVE - Converts (M,M+1) triangularized matrix to (M) matrix.
      SUBROUTINE TIM_POLY_SOLVE(A,M,X,NC)
*    Description :
*      A is the triangularised matrix with M rows and M+1 columns.
*      X is the solution matrix with M rows.
*    History :
*     1 Apr 87: Structured, tidied, header added. (pla@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER    NC           !NDEG + 2
      INTEGER    M            ! No. of rows in triangularized matrix.

      REAL       A(NC,NC)     ! Triangularized matrix.
*    Import-Export :
*    Export :
      REAL       X(NC)        ! Solution matrix.
*    Local Constants :
*    Local variables :
      INTEGER    K            ! Dummy variable for loop.
      INTEGER    I            !   "      "      "    "
      INTEGER    MP1          ! M + 1
      INTEGER    MMK          ! M - K

      REAL       SUM          ! Used in calculation.
*-
*   Initialize variables.
      MP1 = M + 1

      DO K = 1, (M - 1)
         MMK = M - K
         X(M) = A(M, MP1)/A(M,M)
         SUM    = 0.0

         DO I = MMK + 1, M
            SUM = SUM + A(MMK, I)*X(I)

         END DO
         X(MMK) = A(MMK, MP1) - SUM

      END DO
      END
