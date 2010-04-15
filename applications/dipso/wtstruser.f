**==WTSTRUSER.FOR
       SUBROUTINE WTSTRUSER(X,Y,STRING,HEIGHT,JOR,JCTR)

*  Draw a character string
*  X,Y = coordinates in user (= data) coordinates
*  STRING = string to be plotted
*  HEIGHT = character height in grid window Y units
*  JOR = orientation (integer degrees anticlockwise from horizontal)
*  JCTR = justification (-1 = left, 0 = centred, +1 = right)

       IMPLICIT NONE

       REAL X, Y
       CHARACTER*(*) STRING
       REAL HEIGHT
       INTEGER JOR, JCTR

       INTEGER J, JCLIP
       REAL XGRID, YGRID
       REAL CLRECT( 4 )

       REAL SNX_AGUGX, SNX_AGUGY


*  Switch off clipping
       CALL GQCLIP(J,JCLIP,CLRECT)
       CALL GSCLIP(0)

*  Transform user (= data) coordinates to grid (= world)
       XGRID = SNX_AGUGX(X)
       YGRID = SNX_AGUGY(Y)

*  Plot the string
       CALL SNX_WRTST(XGRID,YGRID,STRING,HEIGHT,JOR,JCTR)
       CALL PLOTIT( 0, 0, 2 )

*  Restore clipping
       CALL GSCLIP(JCLIP)
       CALL SGS_FLUSH

       END
