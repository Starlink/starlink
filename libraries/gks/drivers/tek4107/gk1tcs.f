


*
      SUBROUTINE GK1TCS(ICODE, XDC,YDC)

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (part of) Workstation Driver
*  Author:             JRG (Tek 4010)
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To read key hit and cursor position from Tektronix 4107
*         - turn cursor on
*         - read from Tek
*         - Return key hit and cursor position
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*     19/02/85  GGT Adapted for use with Tek 4107
*
*  ARGUMENTS
*  ---------
*     OUT  ICODE   Ascii value of key hit
*     OUT XDC(1),YDC(1) Cursor position in device coordinates (real)
*
      INTEGER ICODE
      REAL XDC(1),YDC(1)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /ERR/    KERROR
*
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     MXLCIN Maximum size of array to hold bytes from Tek
*     ICRSON Array of codes for turning cursor on
*     IDUMMY Used where integer array needed, but contents do not matter
*     IN     Input buffer
*     IER    Local error indicator
*     NOUT   Number of bytes returned by I/O buffer routine
*
      INTEGER INDEV,IEBX,IEBY,J,K,ITEMP(6)
      INTEGER MXLCIN,BELL(4)
      PARAMETER (MXLCIN=10)
      INTEGER PTORX,PTORY
      PARAMETER (PTORX=6.0, PTORY=6.0)
      INTEGER ICRSON(5)
      INTEGER IDUMMY(1),IN(MXLCIN)
      INTEGER IER,NOUT
      DATA ICRSON/27,73,69,0,49/
      DATA BELL /4*7/
      DATA INDEV /0/
*     DATA INDEV /1/ - Set this for tablet input
*
*  ERRORS
*  ------
*      302   I/O error while reading
*

*
*  COMMENTS
*  --------

*-----------------------------------------------------------------------


*   Set local error indicator to be no error
      IER=0

*   Set up cursor display command
      ICRSON(4)=8*INDEV + 48

*   Flush output buffer, turn cursor on and read -- echo (from
*   operating system to terminal), purge typeahead buffer if there is on
      CALL GKIOBO(KIOPB, 5,ICRSON,NOUT)
      CALL GKIOBO(KIOSN, 1,IDUMMY, NOUT)
   5  CALL GKIOBI(KIOEP, 0,IDUMMY, MXLCIN,IN, NOUT)
      IF( KERROR.NE.0)  GOTO 9303

*   Inspect input buffer IN (number of bytes returned is NOUT)
      IF( NOUT.EQ.6 ) THEN
*      Here, ICODE contains key hit in ASCII
       ICODE=IN(1)

      ELSE IF( NOUT.EQ.0) THEN
*       Here the hit key was a control key such as CR or LF
*       so read again to get rest of the buffer.  Record key hit
*       as being CR
        ICODE = 13
        CALL GKIOBI(KIONN, 0, IDUMMY, MXLCIN-1, IN(2), NOUT)
        IF (KERROR.NE.0 .OR. NOUT.NE.5) GOTO 9303
      ELSE
        GOTO 9303
      ENDIF

*   Flush the buffer
      CALL GKIOBO(KIOSN, 1,IDUMMY, NOUT)


*   Here, ICODE contains key hit in ASCII and
*         IN(2) to IN(6) contain bytes making up X and Y
      DO 10 J=2,6
      K=IN(J)-32
      IF(K.LT.0 .OR. K.GT.31) GOTO 9303
      ITEMP(J)=K
10    CONTINUE
      IF(ITEMP(3).GT.15)ITEMP(3)=ITEMP(3)-16
      IEBX=ITEMP(3)
      IF(IEBX.GT.7)IEBX=IEBX-8
      IF(IEBX.GT.3)IEBX=IEBX-4
      XDC(1)=FLOAT(ITEMP(5)*128 + ITEMP(6)*4 + IEBX)/PTORX
      IEBY=ITEMP(3)/4
      YDC(1)=FLOAT(ITEMP(2)*128 + ITEMP(4)*4 + IEBY)/PTORY
      GOTO 9999

*   I/O error while reading
 9303 IER = 302
*   Drop to 9999

 9999 CONTINUE
*   Read again to ensure that the buffer is empty
      CALL GKIOBI(KIONP, 0,IDUMMY, MXLCIN,IN, NOUT)
      KERROR=IER
      END
