*---------------------------------------------------------------------



      SUBROUTINE GK0TLN(N,X,Y)

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
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwdt.cmn'
*
*  LOCALS
*  ------
*     IBAUD  Offset in KWKDAT for baud rate for this terminal
*      ISYN,IGS,IUS  ASCII codes for SYN, GS and US
*      GSMODE  Indicates whether terminal is in Graphics Mode or not
*      HIRES   Indicates whether this workstation is high resolution
*              (e.g. Tek 4014)
*      LIIY,LLOY,LIIX,LLOX  Previous contents of IIY,LOY,IIX,LOX. These
*              are initialised to KNIL to indicate no previous contents.
*      IIY,LOY,IIX,LOX      Hi Y:  Lo Y:  Hi X:  Lo X
*      JX,JY   Current X and Y, divided by 4 if high resolution device
*  MODX,MODY   Remainders when DC (1024 or 4096) is mapped to 1024 grid
*              These are used to form the "Extra byte" on a Tek 4014.
*      I       Loop counter
*      NLEFT   Number of bytes left in buffer
*      NSYNC   Number of sync characters
*      LGAP    A simple measure (overestimate) of how much movement of
*              the beam takes place
*      LOOP    A loop counter
*      IEXTRA  The extra byte calculated for high resolution
*
      INTEGER IBAUD
      PARAMETER (IBAUD=1)
      INTEGER ISYN(1),IGS(1),IUS(1)
      LOGICAL GSMODE,HIRES
      INTEGER LIIY,LLOY,LIIX,LLOX,IIY,LOY,IIX,LOX,JX,JY,MODX,MODY,
     :        I,NLEFT,NSYNC,LGAP,LOOP,IEXTRA,INTA(5),L
      DATA ISYN(1),IGS(1),IUS(1)/22,29,31/
*
*---------------------------------------------------------------------

      LIIY = KNIL
      LLOY = KNIL
      LIIX = KNIL
      LLOX = KNIL
      HIRES = KDSRX(KWKIX).EQ.4096


*   Need to find NLEFT
      CALL GKIOBO(KIOQS,1,INTA,NLEFT)
      GSMODE=.FALSE.

      DO 20 I=1,N

*      If remaining space is not enough for GS (1), data (5), syn (2)
*      and US (1), then send the buffer and start a new one, re-sending
*      the last point if this is not the first iteration.
        IF( NLEFT.LT.9 ) THEN
          IF( GSMODE ) CALL GKIOBO(KIOPB,1,IUS,NLEFT)
          GSMODE=.FALSE.
          CALL GKIOBO(KIOSN,1,IUS,NLEFT)
          CALL GKIOBO(KIOPB,1,IGS,NLEFT)
          GSMODE=.TRUE.
          IF (I.NE.1) THEN
            INTA(1) = IIY
            INTA(2) = LOY
            INTA(3) = IIX
            INTA(4) = LOX
            CALL GKIOBO(KIOPB,4,INTA,NLEFT)
          ENDIF
        ENDIF

*      If terminal is not in Graphics Mode, then switch it. It will
*      already be in GS mode, if this is not the first iteration and
*      there was enough room without emptying the buffer.
        IF( .NOT. GSMODE ) THEN
          CALL GKIOBO(KIOPB,1,IGS,NLEFT)
          GSMODE=.TRUE.
          LIIY = KNIL
          LLOY = KNIL
          LIIX = KNIL
          LLOX = KNIL
        ENDIF

*      Reduce X and Y if high resolution: store in JX and JY
*      Put the remainders in MODX and MODY
        MODX=0
        MODY=0
        JX = IFIX(X(I))
        JY = IFIX(Y(I))
        IF( HIRES ) THEN
          MODX = MOD(JX,4)
          MODY = MOD(JY,4)
          JX = JX/4
          JY = JY/4
        ENDIF

*      Evaluate high and low, X and Y bytes
        IIX = JX/32 + 32
        LOX = MOD(JX,32) + 64
        IIY = JY/32 + 32
        LOY = MOD(JY,32) + 96

*      Now output the bytes omitting those we can
        LGAP = ABS(JX)+ABS(JY)
        L = 0
        IF (IIY.NE.LIIY)THEN
          L = L+1
          INTA(L) = IIY
          LIIY = IIY
        ENDIF
        IF (LOY.NE.LLOY .OR. IIX.NE.LIIX .OR. MODX*MODY.NE.0 ) THEN
          IF( MODX*MODY.NE.0 ) THEN
            IEXTRA = MODX + 4*MODY + 96
            L = L + 1
            INTA(L) = IEXTRA
          ENDIF
          L = L + 1
          INTA(L) = LOY
          LLOY = LOY
          IF (IIX.NE.LIIX) THEN
            L = L + 1
            INTA(L) = IIX
            LIIX = IIX
          ENDIF
        ENDIF
        L = L + 1
        INTA(L) = LOX
        LLOX = LOX
        CALL GKIOBO(KIOPB,L,INTA,NLEFT)

*      If movement is large and line speed is fast, insert fillers
        IF (LGAP.GT.150 .AND. (KWKDAT(IBAUD,KWKIX).GE.4800)) THEN
          NSYNC = NINT( FLOAT(LGAP-150)*(KWKDAT(IBAUD,KWKIX)/4800) )
     :                /1500.0  +  0.5
          DO 10 LOOP=1,NSYNC
            CALL GKIOBO(KIOPB,1,ISYN,NLEFT)
   10     CONTINUE
        ENDIF
   20 CONTINUE

*   Finally, switch to normal mode (we don't have to change GSMODE since
*   it is local)
      CALL GKIOBO(KIOPB,1,IUS,NLEFT)

      END
