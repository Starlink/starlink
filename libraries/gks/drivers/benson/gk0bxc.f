

*----------------------------------------------------------------------
      SUBROUTINE GK0BXC(IFID,ITXPR,NCHARS,ICHSTR,X,Y)
*
* (C) COPYRIGHT ICL & SERC
*

*----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    PART OF WORKSTATION DRIVER
*  Author:             DRJF
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     BENSON String and Character output
*
*  MAINTENANCE LOG
*  ---------------
*     11/02/86 DRJF Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier (ignored)
*     INP ITXPR    Text precision
*     INP NCHARS   Number of characters in text string
*     INP ICHSTR   Text string (ASCII value)
*     INP X,Y      Centre positions for characters in text string
*
      INTEGER IFID,ITXPR,NCHARS,ICHSTR(NCHARS)
      REAL X(NCHARS),Y(NCHARS)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER    KORDER,   KXACFR,   IXPEN,   IYPEN,    KCPEN
      PARAMETER (KORDER=5, KXACFR=7, IXPEN=9, IYPEN=10, KCPEN=11)
      INTEGER    KXACR
      PARAMETER (KXACR=1)
      INTEGER    KOTX,   KADJ
      PARAMETER (KOTX=3, KADJ=4)
      INTEGER    IPDISP,   INDISP
      PARAMETER (IPDISP=0, INDISP=1)
      INTEGER    ILSH2,   IRSH6
      PARAMETER (ILSH2=4, IRSH6=64)
      INTEGER I,ICHT,ISIGN,INT,NLEFT
      INTEGER IHFF04(6),IHFF07(8),IECHST(2)
      INTEGER INEWX,IOLDX,INEWY,IOLDY,IDISPX,IDISPY
      REAL RCHH,RCHW,RCHSIN,RCHCOS
      REAL RCENX,RCENY,RCHRX,RCHRY,RCHRL
      DATA IHFF04/255,4,0,0,0,255/
      DATA IHFF07/255,7,0,0,0,0,0,0/
      DATA IECHST/0,255/
*
*  COMMENTS
*  --------
*     Pen position is known after text output.
*
*----------------------------------------------------------------------


*
*     If CHAR precision output each character of string
*     individually. If STRING precision output string as a lump.
*
      IF (ITXPR.EQ.GCHARP) THEN
*
*       CHAR precision
*
        ICHT=NCHARS
      ELSE
*
*       STRING precision
*
        ICHT=1
      END IF
*
*     Calculate start position of first character
*
      RCHH=FLOAT(KWCHHT(KWKIX))
      RCHW=FLOAT(KWCHWD(KWKIX))
      RCENX=RCHW/2.0
      RCENY=RCHH/2.0
      RCHRX=QWCHRX(KWKIX)
      RCHRY=QWCHRY(KWKIX)
      RCHRL=SQRT(RCHRX*RCHRX+RCHRY*RCHRY)
      RCHSIN=RCHRY/RCHRL
      RCHCOS=RCHRX/RCHRL
      IOLDX=NINT(X(1)+(RCENY*RCHSIN-RCENX*RCHCOS))
      IOLDY=NINT(Y(1)-(RCENY*RCHCOS+RCENX*RCHSIN))
*
*     Move to start position of first charcter
*
      CALL GK0BLN(1,FLOAT(IOLDX),FLOAT(IOLDY))
*
*     Set current order to Output Text
*
      KWKDAT(KORDER,KWKIX)=KOTX
*
      DO 10 I=1,ICHT
*
*       Plant Output Text order plus character for CHAR precision
*       or string for STRING precision
*
        IHFF04(3)=KWKDAT(KCPEN,KWKIX)
        IF (ITXPR.EQ.GCHARP) THEN
          IHFF04(5)=ICHSTR(I)
          CALL GKIOFO(KIOPB,6,IHFF04,NLEFT)
        ELSE
*         Check that the whole string will fit into the record
          CALL GKIOFO(KIOQS,1,KDAT,NLEFT)
          IF(NLEFT.LT.NCHARS+5) CALL GKIOFO(KIOER,1,KDAT,NLEFT)
          CALL GKIOFO(KIOPB,4,IHFF04,NLEFT)
*
*         If the string contains an odd number of characters
*         terminate it with an FF
*
          IF (MOD(NCHARS,2).EQ.0) THEN
            CALL GKIOFO(KIOPB,NCHARS,ICHSTR,NLEFT)
          ELSE
            CALL GKIOFO(KIOPB,NCHARS-1,ICHSTR,NLEFT)
            IECHST(1)=ICHSTR(NCHARS)
            CALL GKIOFO(KIOPB,2,IECHST,NLEFT)
          END IF
        END IF
        IF (ITXPR.EQ.GCHARP) THEN
*
*         CHAR precision
*
          IF (I.EQ.ICHT) THEN
*
*           Calculate Adjust for last character
*
            INEWX=NINT(X(I)+(RCENX*RCHCOS+RCENY*RCHSIN))
            INEWY=NINT(Y(I)-(RCENY*RCHCOS-RCENX*RCHSIN))
          ELSE
*
*           Calculate start position of next character
*
            INEWX=NINT(X(I+1)+(RCENY*RCHSIN-RCENX*RCHCOS))
            INEWY=NINT(Y(I+1)-(RCENY*RCHCOS+RCENX*RCHSIN))
          END IF
*
*         Calculate displacement values for Adjust order
*
          IF (KWKDAT(KXACFR,KWKIX).EQ.KXACR) THEN
*
*           Xacross
*
            IDISPX=-(INEWY-IOLDY)
            IDISPY=INEWX-IOLDX
          ELSE
*
*           Yacross
*
            IDISPX=INEWX-IOLDX
            IDISPY=INEWY-IOLDY
          END IF
          IOLDX=INEWX
          IOLDY=INEWY
        ELSE
*
*         STRING precision
*
*         Calculate displacement values for Adjust order
*
          IF (KWKDAT(KXACFR,KWKIX).EQ.KXACR) THEN
*
*           Xacross
*
            IDISPX=-NINT(RCHH*NCHARS*RCHSIN)
            IDISPY=NINT(RCHH*NCHARS*RCHCOS)
          ELSE
*
*           Yacross
*
            IDISPX=NINT(RCHH*NCHARS*RCHCOS)
            IDISPY=NINT(RCHH*NCHARS*RCHSIN)
          END IF

        END IF
*
*       Set current order to Adjust
*
        KWKDAT(KORDER,KWKIX)=KADJ
*
*       Plant Character Adjust order plus displacement
*
        INT=ABS(IDISPX)
        IF (IDISPX.LT.0) THEN
*
*         X displacement, negative direction
*
          ISIGN=INDISP
        ELSE
*
*         X displacement, positive direction
*
          ISIGN=IPDISP
        END IF
*
*       Construct X part of displacement
*
        IHFF07(5)=MOD(INT,64)*ILSH2+ISIGN
        IHFF07(6)=INT/IRSH6
        INT=ABS(IDISPY)
        IF (IDISPY.LT.0) THEN
*
*         Y displacement, negative direction
*
          ISIGN=INDISP
        ELSE
*
*         X displacement, positive direction
*
          ISIGN=IPDISP
        END IF
*
*       Construct Y part of displacement
*
        IHFF07(7)=MOD(INT,64)*ILSH2+ISIGN
        IHFF07(8)=INT/IRSH6
*
*       Is there enough room for Adjust order and displacement values
*
        IF (NLEFT.LT.8) CALL GKIOFO(KIOER,1,KDAT,NLEFT)
        CALL GKIOFO(KIOPB,8,IHFF07,NLEFT)
*
*       Save pen position
*
        KWKDAT(IXPEN,KWKIX)=KWKDAT(IXPEN,KWKIX)+IDISPX
        KWKDAT(IYPEN,KWKIX)=KWKDAT(IYPEN,KWKIX)+IDISPY
   10 CONTINUE
      RETURN
*
      END
