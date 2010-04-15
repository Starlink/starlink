      SUBROUTINE NDP_ADJACENT(NDIM,NADJ,ADJAC)
C+
C
C   -----------------------
C   N D P _ A D J A C E N T
C   -----------------------
C
C   Description
C   -----------
C   Returns the number and coordinates relative to (0,0,...) of all pixels
C   adjacent to any pixel (i.e. horizontal, vertical, diagonal neighbours)
C   in a notional image of given dimensionality. The central pixel is also
C   counted and its coordinates returned.
C
C
C   Parameters
C   ----------
C   NDIM   (> integer). Number of dimensions, maximum 6.
C   NADJ   (< integer). Number of adjacent pixels.
C   ADJAC  (< integer 2-D array). Coordinates of adjacent pixels.
C
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
      INTEGER NDIM,NADJ,ADJAC(6,729)
C
C   Local variables.
C
      INTEGER COORDS(6,3)
      INTEGER I,J,K,L,M,N
      INTEGER IADJ
C
C   Initialize.
C
      NADJ=3**NDIM
      DO I=1,6
        COORDS(I,1)=1
        COORDS(I,2)=2
        COORDS(I,3)=3
      END DO
C
C   Compute coordinates of all pixels in a 3x3x3... image.
C
      DO N=1,3
        DO M=1,3
          DO L=1,3
            DO K=1,3
              DO J=1,3
                DO I=1,3
                  IADJ=(N-1)*243+(M-1)*81+(L-1)*27+(K-1)*9+(J-1)*3+I
                  ADJAC(6,IADJ)=COORDS(6,N)
                  ADJAC(5,IADJ)=COORDS(5,M)
                  ADJAC(4,IADJ)=COORDS(4,L)
                  ADJAC(3,IADJ)=COORDS(3,K)
                  ADJAC(2,IADJ)=COORDS(2,J)
                  ADJAC(1,IADJ)=COORDS(1,I)
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO
C
      END
