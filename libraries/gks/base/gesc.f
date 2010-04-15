C# IL>=a, OL>=0
      SUBROUTINE GESC(KESCID,LIESC,IESCDR,MLOESC,LOESC,OESCDR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  ESCAPE
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Front end for GESC.
*
*  MAINTENANCE LOG
*  ---------------
*     24/01/83  JGW   Original version stabilized
*     20/01/87  SHS   IS Conversion. Added output data record and
*                     changed error numbers.
*     23/06/88  RMK   Added code to handle escapes -1 and -2.
*     06/12/91  DLT   Added code to support Starlink escape -3.
*     06/12/91  KEVP  Made sure errors conform to GKS Standard (C95).
*     08/01/92  KEVP  Used new call layer routine GKSOTW for escape -3.
*
*  ARGUMENTS
*  ---------
*      INP   KESCID   Function identifier
*      INP   LIESC    Dimension of input data record
*      INP   IESCDR   Input data record
*      INP   MLOESC   Maximum length of output data record
*      OUT   LOESC    Number of array elements occupied in OESCDR
*      OUT   OESCDR   Output data record
*
      INTEGER KESCID, LIESC, MLOESC, LOESC
      CHARACTER*80 IESCDR(LIESC), OESCDR(MLOESC)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*
*     IER    Error indicator
*     IL     Number of integer entries in data record
*     IA     Array of unpacked integers
*     IRL    Number of real entries in data record
*     ISL    Number of character entries in data record
*     LSTR   Array of lengths of unpacked character strings
*     RA     Array of unpacked reals
*     STR    Array of unpacked character strings
*
      INTEGER IER, IL, IA(5), IRL, ISL, LSTR(1)
      REAL RA(1)
      CHARACTER*80 STR(1)
*
*  ERRORS
*  ------
*     180   specified escape function is not supported
*     181   specified escape function identifier is invalid
*     182   contents of escape data record are invalid
*     2001  output parameter size insufficient
*
*---------------------------------------------------------------------

      CALL GKPRLG(EESC,GGKOP,GSGOP)
      IF (KERROR.NE.0)  GOTO 9999
      IF (MLOESC.LE.0) KERROR = 2001
*     Check escape identifier
      IF (KESCID.LT.-3 .OR. KESCID.GT.0) KERROR = 180
      IF (KESCID.EQ.0) KERROR = 181
      IF (KERROR.NE.0)  GOTO 9999


*     Escapes -1 and -2: associate/disassociate input devices
*     The data record contains:
*     IA(1) = workstation identifier
*     IA(2) = request input class 1
*     IA(3) = device number for class 1
*     IA(4) = request input class 2
*     IA(5) = device number for class 2
*
      IF (KESCID.EQ.-1 .OR. KESCID.EQ.-2) THEN
*        Unpack data record and pass information down to workstation
         CALL GUREC(LIESC, IESCDR, 5, 1, 1, IER, IL, IA, IRL, RA,
     :              ISL, LSTR, STR)
         IF (IER.NE.0) KERROR = 182
*        Check that input class is legal
         IF (IA(2).LT.GLOCAT .OR. IA(2).GT.GSTRIN .OR.
     :       IA(4).LT.GLOCAT .OR. IA(4).GT.GSTRIN) KERROR = 182
*        Check that input device number is legal
         IF (IA(3).LT.1 .OR. IA(5).LT.1) KERROR = 182
         IF (KERROR.NE.0) GOTO 9999

         KWI1 = KESCID
         KWI2 = IA(2)
         KWI3 = IA(3)
         KWI4 = IA(4)
         KWI5 = IA(5)
*        Send escape to the specified workstation
         CALL GKSONW(IA(1), KESC, 1, KDAT, 1, QDAT, QDAT, LIESC, IESCDR)
*        If error, contents of data record must be invalid.
         IF(KERROR .NE. 0)THEN
            KERROR = 182
            GOTO 9999
         ENDIF
*        Output data record isn't used
         LOESC = 0

*     Escape -3: Suppress clearing of workstation on open
*     The data record contains:
*     IA(1) = workstation type
*     IA(2) = No clear on open flag (GNO => clear, GYES = > don't clear)
      ELSE IF (KESCID.EQ.-3) THEN
*        Unpack data record and pass information down to workstation
         CALL GUREC(LIESC, IESCDR, 2, 1, 1, IER, IL, IA, IRL, RA,
     :              ISL, LSTR, STR)
         IF (IER.NE.0) KERROR = 182
*        Check that flag value is legal
         IF (IA(2).NE.GNO .AND. IA(2).NE.GYES) KERROR = 182
         IF (KERROR.NE.0) GOTO 9999

         KWI1 = KESCID
         KWI2 = IA(2)
*        Send escape to the specified workstation type.
         CALL GKSOTW(IA(1), KESC, 1, KDAT, 1, QDAT, QDAT, LIESC, IESCDR)
*        If error, contents of data record must be invalid.
         IF(KERROR .NE. 0)THEN
            KERROR = 182
            GOTO 9999
         ENDIF
*        Output data record isn't used
         LOESC = 0

      ENDIF
*
*     -- valid parameter list, but no escapes yet via this route
C     IF (sacw function) then
C        CALL GKSACW(KESC,1,KDAT,NPTS,RX,RY,LDR,IESCDR)
C     ELSEIF (sonw function)
C        ID = integer held in datarec????
C        CALL GKSONW(ID,KESC,1,KDAT,NPTS,RX,RY,LDR,IESCDR)
C     ELSEIF (sopw function)
C        CALL GKSOPW(KESC,1,KDAT,NPTS,RX,RY,LDR,IESCDR)
C     ENDIF

 9999 CONTINUE
      IF (KERROR.NE.0) CALL GKERR(KERROR)
      END
