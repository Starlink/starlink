

      SUBROUTINE GK1TLN(N,X,Y)

*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Outputs polyline to buffer
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP N   - number of points
*     INP X,Y - coordinates of points
*
      INTEGER N
      REAL X(N),Y(N)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*      IBAUD   Offset of baud rate within KWKDAT
*      ISYN,IGS,IUS  ASCII codes for SYN, GS and US
*      LIIY,LLOY,LIIX,LLOX  Previous contents of IIY,LOY,IIX,LOX. These
*              are initialised to zero to indicate no previous contents.
*      IIY,LOY,IIX,LOX      Hi Y:  Lo Y:  Hi X:  Lo X
*      I       Loop counter
*      NLEFT   Number of bytes left in buffer
*
      INTEGER IBAUD, ISYN,IGS,IUS,IX,IY,J,K,IEB,LIEB
      PARAMETER (IBAUD=1, ISYN=22,IGS=29,IUS=31)
      INTEGER LIIY,LLOY,LIIX,LLOX,IIY,LOY,IIX,LOX,I,NLEFT
      REAL PTORX,PTORY
      PARAMETER (PTORX=6.0, PTORY=6.0)
*
*---------------------------------------------------------------------
      CALL GKIOBO(KIOPB,1,IGS,NLEFT)
      LIIY = 0
      LIEB = 0
      LLOY = 0
      LIIX = 0
      LLOX = 0

      DO 10 I=1,N
*      Evaluate high and low, X and Y bytes
      IX=INT(X(I)*PTORX+0.5)
      IY=INT(Y(I)*PTORY+0.5)
      IIX = IX/128
      IIY = IY/128
      LOX = IX-IIX*128
      LOY = IY-IIY*128

*      Compute extra byte
      K=LOX
      J=LOY
      LOX=LOX/4
      LOY=LOY/4
      IEB=4*(J-LOY*4)+(K-LOX*4)

*      Add bias
      IIX=IIX+32
      IIY=IIY+32
      LOX=LOX+64
      LOY=LOY+96
      IEB=IEB+96

*      Now output the bytes omitting those we can
        IF (IIY.NE.LIIY)THEN
          CALL GKIOBO(KIOPB,1,IIY,NLEFT)
        ENDIF
*
      IF (IEB.NE.LIEB)THEN
        CALL GKIOBO(KIOPB,1,IEB,NLEFT)
        CALL GKIOBO(KIOPB,1,LOY,NLEFT)
        IF (IIX.NE.LIIX)THEN
           CALL GKIOBO(KIOPB,1,IIX,NLEFT)
        ENDIF
      ELSE
        IF (LOY.NE.LLOY)THEN
           CALL GKIOBO(KIOPB,1,LOY,NLEFT)
           IF (IIX.NE.LIIX)THEN
               CALL GKIOBO(KIOPB,1,IIX,NLEFT)
           ENDIF
        ELSE
           IF (IIX.NE.LIIX)THEN
               CALL GKIOBO(KIOPB,1,LOY,NLEFT)
               CALL GKIOBO(KIOPB,1,IIX,NLEFT)
           ENDIF
        ENDIF
        ENDIF
        CALL GKIOBO(KIOPB,1,LOX,NLEFT)
      LIIX = IIX
      LIIY = IIY
        LLOX = LOX
      LLOY = LOY
   10 CONTINUE

*   Finally, switch to normal mode (we don't have to change GSMODE since
*   it is local)
      CALL GKIOBO(KIOPB,1,IUS,NLEFT)

      END
