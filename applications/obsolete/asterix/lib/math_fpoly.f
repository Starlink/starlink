*+  MATH_FPOLY - fits polynomial from values X,Y - evaulate using MATH_EPOLY
      SUBROUTINE MATH_FPOLY( N, X, WFIT, W, NDEG, A, C, Y, Z,
     :                                               STATUS )
*    Description :
*      This program fits a fitted polynomial from X.
*      If NDEG=0, then the mean value is fitted.
*    Parameters :
*    Bugs :
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*     Phillip Andrews (pla@uk.ac.bham.sr.star)
*     David Allan ( BHVAD::DJA )
*
*    History :
*
*     16 Sep 86 : Version 4 (TJP)
*      1 Apr 87 : Code structured, header added. (PLA)
*      7 May 88 : Asterix88 version    (LTVAD::RDS)
*     17 Jun 88 : Improved the efficiency by merging two DO loops so
*                 that powers are only calculated once    (LTVAD::RDS)
*     23 Nov 89 : Taken from MATH_POLY. Only does fit, which is returned.
*                 The X values are not normalised. (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER    STATUS
*
*    Import :
*
      INTEGER    N                ! No.of X,Y pairs
      REAL       X(N)             ! Array of values of independent variable
      LOGICAL    WFIT             ! Is weighted fit required.
      REAL       W(*)             ! Array of weights proportional to inverse
      INTEGER    NDEG             ! Degree of polynomial ( max.power of X )
      REAL       A(NDEG+2,NDEG+2) ! Work space.
      REAL       C((2*NDEG)+1)    ! Work space.
*
*    Import-Export :
*
      REAL       Y(N)             ! Array of dependent variable, becomes
                                  ! residuals after subtraction of polynomial
*
*    Export :
*
      REAL       Z(NDEG+2)        ! Work space, before subtraction.
                                  ! First NDEG+1 elements return coefficients
                                  ! of polynomial in X (in ascending powers)
*
*    Local variables :
*
      INTEGER    I                ! Dummy variable for loops.
      INTEGER    II               !   "      "      "    "
      INTEGER    J                !   "      "      "    "
      INTEGER    NA               ! NDEG + 1
      INTEGER    NC               ! NDEG + 2
      INTEGER    M                ! (2*NDEG) + 1

      REAL       YMEAN            ! Mean value of Y array.
      REAL       DENOM            ! Used in calculation of YMEAN.
      REAL       SUMBIT           ! Used to speed up the code
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find mean if NDEG = 0
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

      ELSE

*       Start of sao program: initialize variables.
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
                  SUMBIT = W(J) * (X(J)**II)
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
                  SUMBIT = (X(J)**II)
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

      END IF

      END
