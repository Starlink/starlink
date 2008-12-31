      SUBROUTINE LINTERP(XS,XE,YS,YE,X,Y)
C+
C
C Subroutine: 
C
C  L I N T E R P
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C XS (<), XE (<), YS (<), YE (<), X (<) ,Y (>)
C
C History: 
C  
C   May 1994 Created
C 
C
C Linear interpolation
C
C
C-
C
      IMPLICIT NONE
C
      REAL XS,XE,YS,YE,X,Y
      REAL GR
      GR =  (YE-YS)/(XE-XS)
      Y = YS+GR*(X-XS)
      END
