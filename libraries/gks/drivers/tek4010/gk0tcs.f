*-----------------------------------------------------------------------



      SUBROUTINE GK0TCS(ICODE, XDC,YDC)

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (part of) Workstation Driver
*  Author:             JRG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To read key hit and cursor position from Tektronix 4010
*     and the lookalikes:
*         - turn cursor on
*         - read
*         - Return key hit and cursor position
*         - turn cursor off
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
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
*     Read   /WCA/    KWKIX,KWKTYP
*     Read   /WDT/    KDSRX
*     Read   /WKD/    KWKDAT
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     ISUB,IESC,IUS,ICR   ASCII codes for SUB, ESC, US and CR
*     MXLCIN Maximum size of array to hold bytes from Tek
*     ICRSOF Array of just one code for turning cursor off
*            ( for Tek & identical lokalikes )
*     ICRSON Array of codes for turning cursor on
*            ( for Tek & identical lokalikes )
*     IPMCOF Array of just one code for turning the Pericom'
*            cursor off.
*     IDUMMY Used where integer array needed, but contents do not matter
*     IN     Input buffer
*     IER    Local error indicator
*     NOUT   Number of bytes returned by I/O buffer routine
*
      INTEGER ISUB,IESC,IUS,ICR
      PARAMETER (ISUB=26, IESC=27, IUS=31, ICR=13)
      INTEGER MXLCIN
      PARAMETER (MXLCIN=10)
      INTEGER ICRSOF(1),ICRSON(2)
      INTEGER IPMCOF(1)
      INTEGER IDUMMY(1),IN(MXLCIN)
      INTEGER IER,NOUT
      DATA ICRSOF/IUS/, ICRSON/IESC,ISUB/
      DATA IPMCOF/ICR/
*
*  ERRORS
*  ------
*      302   I/O error while reading
*
*-----------------------------------------------------------------------


*   Set local error indicator to be no error
      IER=0

*   Flush the output buffer
      CALL GKIOBO(KIOSO, 0,IDUMMY, NOUT)

*   Regardless off the terminal type, use "cursor on" sequence
*   to turn the cursor on and read -- no echo (from operating
*   system to terminal), purge typeahead buffer if there is one.
*
      CALL GKIOBI(KIONP, 2,ICRSON, MXLCIN,IN, NOUT)
      IF( KERROR.NE.0 ) GOTO 9303

*   Inspect input buffer IN (number of bytes returned is NOUT)
      IF( NOUT.EQ.5 ) THEN

*       Here, key hit was not a terminator
          ICODE=IN(1)

      ELSE IF( NOUT.EQ.0 ) THEN

*       Here, key hit was control code, so read again (no purge this
*       time) to get rest of buffer. Record the key hit as being
*       control Z.
          ICODE=ISUB
          CALL GKIOBI(KIONN, 0,IDUMMY, MXLCIN-1,IN(2), NOUT)
          IF( KERROR.NE.0 .OR. NOUT.NE.4 ) GOTO 9303

      ELSE
          GOTO 9303
      ENDIF

*   Here, ICODE contains key hit in ASCII and
*         IN(2) to IN(5) contain bytes making up X and Y
*                         Hi Byte       Lo Byte
      XDC(1) = FLOAT(  32*( IN(2)-32 ) + IN(3) -32  )
      YDC(1) = FLOAT(  32*( IN(4)-32 ) + IN(5) -32  )

*   If high resolution workstation, multiply again to get
*   device coordinates
      IF( KDSRX(KWKIX).EQ.4096 ) THEN
         XDC(1)=XDC(1)*4.0
         YDC(1)=YDC(1)*4.0
      ENDIF
      GOTO 9999

*   I/O error while reading
 9303 IER=302
*   Drop to 9999

 9999 CONTINUE

*   Depending on the terminal type, turn cursor off
*
*   Tektronix 4010
*   Tektronix 4014
*   Cifer 2634
*   Cifer T5
*   VT100 with Selanar board
      IF(   ( KWKTYP.EQ.201 )   .OR.
     :      ( KWKTYP.EQ.203 )   .OR.
     :      ( KWKTYP.EQ.800 )   .OR.
     :      ( KWKTYP.EQ.801 )   .OR.
     :      ( KWKTYP.EQ.810 ) ) THEN
         CALL GKIOBO(KIOPB,1,ICRSOF,NOUT)
*  Standard and RAL mods Pericom Monterey shared entry
      ELSEIF( KWKTYP.EQ.820.OR.KWKTYP.EQ.821 )   THEN
*        Unlike other lookalikes, the Pericoms use CR as a
*        "cursor off" sequence.
         CALL GKIOBO(KIOPB,1,IPMCOF,NOUT)
      ENDIF
      CALL GKIOBO(KIOSN,1,IDUMMY,NOUT)
      KERROR=IER
      END
