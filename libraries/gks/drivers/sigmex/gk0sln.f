      SUBROUTINE GK0SLN(N,X,Y)

*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Outputs polyline to buffer.
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
*     ILOX,IHIX   Low X and high X
*     ILOY,IHIY   Low Y and high Y
*     ILLOX,ILHIX,ILLOY,ILHIY  Previous copies of low/high x/y
*     I         Loop counter
*     L         Number of bytes in array used
*     INTA      Array of bytes to be passed to the buffer for the device
*               (in array, it is 1 byte per integer)
*     NLEFT     Number of bytes remaining in buffer for device
*     IFST,ILST Index of first and index of last point to be output
*               before checking for space again
*     IHIRES    Offset in KWKDAT array for resolution flag
*     IXI,IYI   Integer variables to hold IFIXed values of current
*               X(I) and Y(I) elements.
*
      INTEGER ILHIY, ILLOY, ILHIX, ILLOX, IHIY, ILOY, IHIX, ILOX,
     :        ILEB, IEB, I, INTA(7), L, IFST, ILST, NLEFT, IHIRES,
     :        IXI, IYI
      PARAMETER (IHIRES = 2)
*
*---------------------------------------------------------------------
*
      IFST = 1
   10 CONTINUE
* Switch to abbreviated graphics mode
      CALL GKIOCO(KIOPB,'JH',NLEFT)
      ILHIY = KNIL
      ILLOY = KNIL
      ILHIX = KNIL
      ILLOX = KNIL
      ILEB = KNIL
      IEB = KNIL
   20 CONTINUE
* Work out at least how many points will fit into output buffer
* (high resolution models will need more space)
      NLEFT = (NLEFT-1)/(4+KWKDAT(IHIRES,KWKIX))

* If not enough room for any points switch to normal graphics and
* send buffer.
      IF (NLEFT.LT.1) THEN
        INTA(1) = 31
        CALL GKIOBO(KIOPB,1,INTA,NLEFT)
        CALL GKIOBO(KIOSN,1,INTA,NLEFT)
        GOTO 10
      ENDIF

* Work out first and last points to loop over
      ILST = MIN ( IFST+NLEFT-1, N )

* Output points
      DO 30 I=IFST,ILST
        L = 0
        IXI = IFIX(X(I))
        IYI = IFIX(Y(I))
        IF (KWKDAT(IHIRES,KWKIX).EQ.1)
     :    IEB = (IYI/1024 )*4 + IXI/1024 + 96
        IHIX = MOD( IXI, 1024 )/32 + 32
        ILOX = MOD( IXI, 32 ) + 64
        IHIY = MOD( IYI, 1024 )/32 + 32
        ILOY = MOD( IYI, 32 ) + 96
* Filter out identical points, but leave in 2-point zero length polylines
* ie. generate dots.
        IF (I.GT.2.AND.IHIX.EQ.ILHIX.AND.ILOX.EQ.ILLOX.AND.
     :      IHIY.EQ.ILHIY.AND.ILOY.EQ.ILLOY.AND.IEB.EQ.ILEB) GOTO 30
        IF (IHIY.NE.ILHIY) THEN
* Output high Y if changed
          L = L + 1
          INTA(L) = IHIY
          ILHIY = IHIY
        ENDIF
        IF (IEB.NE.ILEB) THEN
* Output extra byte and low Y if extra byte changed
          L = L + 1
          INTA(L) = IEB
          ILEB = IEB
          L = L + 1
          INTA(L) = ILOY
          ILLOY = ILOY
          IF (IHIX.NE.ILHIX) THEN
* Output high X if high X and extra byte changed. Low Y output already
            L = L + 1
            INTA(L) = IHIX
            ILHIX = IHIX
          ENDIF
        ELSE IF (IHIX.NE.ILHIX) THEN
* Output low Y and high X if high X but not extra byte changed
          L = L + 1
          INTA(L) = ILOY
          ILLOY = ILOY
          L = L + 1
          INTA(L) = IHIX
          ILHIX = IHIX
        ELSE IF (ILOY.NE.ILLOY) THEN
* Output low Y if low Y but not high X or extra byte changed
          L = L + 1
          INTA(L) = ILOY
          ILLOY = ILOY
        ENDIF
* Always output low X
        L = L + 1
        INTA(L) = ILOX
        ILLOX = ILOX

        CALL GKIOBO(KIOPB,L,INTA,NLEFT)

   30 CONTINUE

* If there are any points left start again
      IF (ILST.LT.N) THEN
        IFST = ILST
        GOTO 20
      ELSE
        INTA(1) = 31
        CALL GKIOBO(KIOPB,1,INTA,NLEFT)
      ENDIF


      END
