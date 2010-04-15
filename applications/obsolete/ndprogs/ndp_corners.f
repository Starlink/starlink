      SUBROUTINE NDP_CORNERS(DIMS,NDIM,NCORN,CORNERS)
C+
C
C   ---------------------
C   N D P _ C O R N E R S
C   ---------------------
C
C   Description
C   -----------
C   Returns the number and coordinates of all corner pixels in a notional
C   image of given dimensionality and dimensions.
C
C
C   Parameters
C   ----------
C   DIMS      (> integer array). Dimensions of notional image.
C   NDIM      (> integer). Number of dimensions, maximum 6.
C   NCORN     (< integer). Number of corner pixels.
C   CORNERS   (< integer 2-D array). Corner pixel coordinates.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   None.
C
C
C   INCLUDE statements
C   ------------------
C   None.
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C   Parameters.
C
      INTEGER DIMS(10),NDIM,NCORN,CORNERS(6,64)
C
C   Local variables.
C
      INTEGER ENDS(6,2)
      INTEGER I,J,K,L,M,N
      INTEGER ICORN
C
C   Initialize.
C
      NCORN=2**NDIM
      DO I=1,6
        ENDS(I,1)=1
        ENDS(I,2)=DIMS(I)
      END DO
C
C   Compute coordinates of all corner pixels in the image.
C
      DO N=1,2
        DO M=1,2
          DO L=1,2
            DO K=1,2
              DO J=1,2
                DO I=1,2
                  ICORN=(N-1)*32+(M-1)*16+(L-1)*8+(K-1)*4+(J-1)*2+I
                  CORNERS(6,ICORN)=ENDS(6,N)
                  CORNERS(5,ICORN)=ENDS(5,M)
                  CORNERS(4,ICORN)=ENDS(4,L)
                  CORNERS(3,ICORN)=ENDS(3,K)
                  CORNERS(2,ICORN)=ENDS(2,J)
                  CORNERS(1,ICORN)=ENDS(1,I)
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO
C
      END
