*-----------------------------------------------------------------------

      SUBROUTINE SET_DISPLAY_SIZE (AX1LEN, AX2LEN, XDIM, YDIM,
     &                             AXLENX, AXLENY, XMARGIN, YMARGIN)

      IMPLICIT  NONE

*  Formal parameters

      REAL*4    AX1LEN,  AX2LEN      ! Requested sizes
      REAL*4    XDIM,    YDIM        ! Natural sizes for this map
      REAL*4    AXLENX,  AXLENY      ! Final physical sizes
      REAL*4    XMARGIN, YMARGIN     ! Total margins to be allowed

*  Other variables

      REAL*4    XSIZED, YSIZED
      REAL*4    XAXLEN, YAXLEN 

*  Go...

      XAXLEN = ABS (XDIM)
      YAXLEN = ABS (YDIM)

      IF (AX1LEN.EQ.0.0 .AND. AX2LEN.EQ.0.0) THEN

        CALL SXGDEVINFO  (XSIZED, YSIZED)

        IF (XAXLEN/(XSIZED-XMARGIN) .GT. YAXLEN/(YSIZED-YMARGIN)) THEN
          AXLENX = (XSIZED-XMARGIN)
          AXLENY = (XSIZED-XMARGIN) * YAXLEN/XAXLEN
        ELSE
          AXLENX = (YSIZED-YMARGIN) * XAXLEN/YAXLEN
          AXLENY = (YSIZED-YMARGIN)
        END IF

      ELSE IF (AX1LEN.GT.0. AND. AX2LEN.EQ.0.) THEN
        AXLENX = AX1LEN
        AXLENY = AXLENX * YAXLEN/XAXLEN

      ELSE IF (AX1LEN.EQ.0. AND. AX2LEN.GT.0.) THEN
        AXLENY = AX2LEN
        AXLENX = AXLENY * XAXLEN/YAXLEN

      ELSE
        AXLENX = AX1LEN
        AXLENY = AX2LEN

      END IF

D     type *
D     type *, '-- set_display_size --'
D     type *, '   requested (x,y)     = ', ax1len, ax2len
D     type *, '   natural sizes (x,y) = ', xaxlen, yaxlen
D     type *, '   output sizes (x,y)  = ', axlenx, axleny
D     type *

      RETURN
      END

*-----------------------------------------------------------------------
