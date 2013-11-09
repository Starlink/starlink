*+  MATH_POLY - subtracts a fitted polynomial from X.
      SUBROUTINE MATH_POLY(POLY, N, X, WFIT, W, NDEG, A, C, Y, Z, XN,
     :                                                SUMSQ, XMAX, XMIN)
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
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*     Phillip Andrews (pla@uk.ac.bham.sr.star)
*    History :
*     16 Sep 86: Version 4 (BHAVD::TJP)
*      1 Apr 87: Code structured, header added. (pla@uk.ac.bham.sr.star)
*      7 May 88: Asterix88 version    (LTVAD::RDS)
*     17 June 1988   Improved the efficiency by merging two DO loops so
*                    that powers are only calculated once    (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      LOGICAL    POLY             ! if true return fit, if not return resids
      INTEGER    N                ! No.of X,Y pairs
      REAL       X(N)             ! Array of values of independent variable
      LOGICAL    WFIT             ! Is weighted fit required.
      REAL       W(*)             ! Array of weights proportional to inverse
      INTEGER    NDEG             ! Degree of polynomial ( max.power of X )
      REAL       A(NDEG+2,NDEG+2) ! Work space.
      REAL       C((2*NDEG)+1)    ! Work space.

*    Import-Export :
      REAL       Y(N)             ! Array of dependent variable, becomes
                                  ! residuals after subtraction of polynomial
      REAL       Z(NDEG+2)        ! Work space, before subtraction.
                                  ! First NDEG+1 elements return coefficients
                                  ! of polynomial in XN (in ascending powers)
*    Export :
      REAL       XN(N)            ! Normalized X
      REAL       SUMSQ            ! Sum of squares of weighted residuals
      REAL       XMAX             ! Max value of X array.
      REAL       XMIN             ! Min value of X array.
*    Local Constants :
      INTEGER MAXDATA
      PARAMETER (MAXDATA=100000)  !Maximum allowed datapoints
*    Local variables :
      INTEGER    I                ! Dummy variable for loops.
      INTEGER    II               !   "      "      "    "
      INTEGER    J                !   "      "      "    "
      INTEGER    NA               ! NDEG + 1
      INTEGER    NC               ! NDEG + 2
      INTEGER    M                ! (2*NDEG) + 1

      REAL       YMEAN            ! Mean value of Y array.
      REAL       YCONST           ! Constant component in Y fit.
      REAL       DENOM            ! Used in calculation of YMEAN.
      REAL       SUMBIT           ! Used to speed up the code

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

         IF (WFIT) THEN
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
            YMEAN = YMEAN / REAL(N)              ! Unweighted mean

         END IF
         Z(1) = YMEAN
         SUMSQ = 0.0

         DO I = 1, N
            Y(I) = Y(I) - YMEAN

            IF (WFIT) THEN
               SUMSQ = SUMSQ + (W(I) * Y(I)**2)

            ELSE
               SUMSQ = SUMSQ + Y(I)**2

            END IF
         END DO

      ELSE
*      Start of sao program: initialize variables.
         NA = NDEG + 1
         NC = NDEG + 2
         M  = (2 * NA) - 1

         IF ( WFIT ) THEN
            DO I = 1, NA
                  A(I,NC) = 0.0
            END DO

            DO I = 1, M
               II = I - 1
               C(I) = 0.0

               DO J = 1, N
                  SUMBIT = W(J) * (XN(J)**II)
                  C(I)   = C(I) + SUMBIT

*               Set the (NDEG+2)th elements
                  IF (I .LE. NA) THEN
                      A(I,NC) = A(I,NC) + Y(J) * SUMBIT

                  ENDIF
               END DO
            END DO

            DO I = 1, NA
               DO J = 1, NA
                  A(I,J) = C(I + J - 1)

               END DO
            END DO
         ELSE

            DO I = 1, NA
                  A(I,NC) = 0.0

            END DO

            DO I = 1, M
               II = I - 1
               C(I) = 0.0

               DO J = 1, N
                  SUMBIT = (XN(J)**II)
                  C(I)   = C(I) + SUMBIT

                  IF (I .LE. NA) THEN
                     A(I,NC) = A(I,NC) + (Y(J) * SUMBIT)
                  ENDIF
               END DO
            END DO

            DO I = 1, NA
               DO J = 1, NA
                  A(I,J) = C(I + J - 1)

               END DO
            END DO
         END IF

         CALL MATH_POLY_TRIANG(NA,NC,A)
         CALL MATH_POLY_SOLVE(A,NA,NC,Z)
         SUMSQ = 0.0

         DO I = 1, N
            YCONST = Z(1)
            IF ( (NA - 1 ) .NE. 0 ) THEN

               DO J = 2, NA
                  YCONST = YCONST + (Z(J) * XN(I)**(J-1))

               END DO
            END IF

            IF ( WFIT ) THEN
               SUMSQ = SUMSQ + (W(I) * ((Y(I) - YCONST)**2))

            ELSE
               SUMSQ = SUMSQ + (Y(I)-YCONST)**2

            END IF

	    IF ( .NOT. POLY ) THEN
               Y(I) = Y(I) - YCONST                     ! Return residual

            ELSE
               Y(I) = YCONST				! Return calc y val

            END IF
	 END DO
      END IF
      END








*+MATH_POLY_TRIANG - Triangularizes the matrix A, with M rows & M+N columns.
      SUBROUTINE MATH_POLY_TRIANG(M,NC,A)
*    Description :
*      A is the matrix with M rows and M + 1 columns to be triangularised.
*    History :
*     ????????: Original (BHVAD::TJP)
*     1 Apr 87: Structured and header added. (pla@uk.ac.bham.sr.star)
*     7 May 88: Argument order changed.    (LTVAD::RDS)
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




*+MATH_POLY_SOLVE - Converts (M,M+1) triangularized matrix to (M) matrix.
      SUBROUTINE MATH_POLY_SOLVE(A,M,NC,X)
*    Description :
*      A is the triangularised matrix with M rows and M+1 columns.
*      X is the solution matrix with M rows.
*    History :
*     1 Apr 87: Structured, tidied, header added. (pla@uk.ac.bham.sr.star)
*     7 Jun 88: Order of arguments changed.
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
