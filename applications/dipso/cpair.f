      SUBROUTINE CPAIR(XSTART,YSTART,XEND,YEND)
      REAL XSTART, XEND, YSTART, YEND
      INTEGER HITVAL
      LOGICAL FIRSTCURSOR
      COMMON /SETCURSOR/XCURSOR, YCURSOR
      COMMON /DIVFACTOR/ DIVX,DIVY
       FIRSTCURSOR=.TRUE.
       CALL SGSCURSE(HITVAL,XEND,YEND,FIRSTCURSOR)
       FIRSTCURSOR=.FALSE.

* The following call was commented out by DSB on 13/6/2014 becuase it
* can cause the cursor to be moved to an incorrect position. XEND and
* YEND are in spectral coords, but SGS_SETCU requires graphics coords.
* Also, SGSCURSE leavs the graphics system in NCAR mode rather than SGS
* mode. These issues could be fixed, but I cannot see why this call is
* needed anyway, since the cursor will currently be in the position at
* which it was left by SGSCURSE anyway.
c       CALL SGS_SETCU(XEND,YEND)

       XSTART=XEND
       YSTART=YEND
       CALL SGSCURSE(HITVAL,XEND,YEND,FIRSTCURSOR)
      END
