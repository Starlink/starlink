	SUBROUTINE SMOOTH (Y,NPTS)
C+
C
C Subroutine: 
C
C  S M O O T H
C
C
C
C History: 
C  
C   May 1994 Created
C 
c author
c   Bevington, page 260.
c
c purpose
c   smooth a set of data points by averaging adjacent channels
c
c usage
c   call smooth (y, npts)
c
c description of parameters
c   y	   - array of data points
c   npts   - number of data points
c
c subroutines and function subprograms required
c   none
c
C  
C
C
C
C
C-
        IMPLICIT NONE
        INTEGER I,IMAX,NPTS
        REAL YI,YNEW
	REAL Y(*)
11	IMAX=NPTS-1
	YI=Y(1)
21	DO 24 I=1,IMAX
	YNEW=(YI+2.*Y(I)+Y(I+1))/4.
	YI=Y(I)
24	Y(I)=YNEW
25	Y(NPTS)=(YI+3.*Y(NPTS))/4.
	END

