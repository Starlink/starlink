      SUBROUTINE TPOLY(X,Y,NVERT,SIZE)
C+
C
C Subroutine: 
C
C   T P O L Y
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C X (<), Y (<), NVERT (<), SIZE (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C Interface for pgpoly
C
C
C
C-

      IMPLICIT NONE
      REAL X,Y,SIZE,ANG
      INTEGER NVERT,N
      REAL CX(20),CY(20)
      REAL X1,X2,Y1,Y2
      REAL WX1,WX2,WY1,WY2
      REAL XSCALE,YSCALE
      REAL PI
      PI=3.141592654
C
      CALL PGQVP(0,X1,X2,Y1,Y2)
      CALL PGQWIN(WX1,WX2,WY1,WY2)
      XSCALE=(WX2-WX1)/(X2-X1)
      YSCALE=(WY2-WY1)/(Y2-Y1)
C
      SIZE=SIZE/80.
      DO N=1,NVERT
       ANG=(2.*PI*REAL(N-1)/REAL(NVERT))+PI/2.
       CX(N)=X+COS(ANG)*SIZE*XSCALE
       CY(N)=Y+SIN(ANG)*SIZE*YSCALE
      ENDDO
C
      CALL PGPOLY(NVERT,CX,CY)
C
      END

       
