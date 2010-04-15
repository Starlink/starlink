      SUBROUTINE GKIOCL(KEY1, KEY2, ICHAN)
*
* Copyright (C) SERC 1986
*
*-----------------------------------------------------------------------
*
*  Type of routine:  SYSTEM INTERFACE
*  Author:           PJWR
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Close the unit(s) indicated by KEY1 and KEY2 or return an error.
*
*  MAINTENANCE LOG
*  ---------------
*     06/08/86  PJWR  Original UNIX version stabilised.
*     12/12/86  PJWR  Added use of GKIOSB() to reset GKS break character
*                     to any previous use.
*     09/07/87  PJWR  Updated error numbers for GKS 7.4.
*     05/12/90  PLP   Added section of the code to close the Input or
*                     Output workstation's stream only when an appropriate
*                     environment variable is defined (S412=C62).
*     28/01/91  PLP   Modified the above fix so that only those workstations
*                     that have not been flagged as preconnected in GKIOOP
*                     are closed (S434).
*     04/02/91  KEVP  Prevented error stream from being closed, if 6 (S440).
*     07/02/91  PLP   Added preconnection flag checking for the error
*                     logfile(S441).
*
*  ARGUMENTS
*  ---------
*     KEY1    INP  Function code giving file type to be closed.
*     KEY2    INP  Workstation connection identifier.
*     ICHAN   INP  Internal channel number.
*
      INTEGER KEY1, KEY2, ICHAN
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read    /GKYWCA/  KWKIX
*     Modify  /GKYERR/  KERROR
*             /GKYFLS/  KEMFLS, KWDFLS, KDBFLS, KCDFLS
*             /GKZXIO/  ISWKT, ISPREC, ERPREC
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkxio.cmn'
*
*  LOCALS
*  ------
*     IOS     Status return for OPEN.
*     ISOPEN  Status return for INQUIRE.
*
      INTEGER IOS, ICHAN1
      LOGICAL ISOPEN
*
*  STREAMS USED
*  ------------
*     See COMMENTS below.
*
*  ERRORS
*  ------
*    -1041 Error while trying to close internal segment store
*    -1014 Error while trying to close the error file
*    -1013 Error while trying to close the database file
*    -1012 Error while trying to close the error message file
*    -1011 Error while trying to close the workstation description table
*          file
*       25 Specified workstation is not open
*
*  COMMENTS
*  --------
*     Usually,  the stream associated with ICHAN is closed.  However, it
*     is illegal to close logical unit 0 under UNIX,  and closing the
*     preconnected streams 5 and 6 is inadvisable, so these are trapped.
*
*     For an interactive workstation,  the stream (ICHAN + 1) must
*     also be closed if a stream other than 5 is given in ICHAN.
*
*     This routine also updates the logical array ISWKT when it closes
*     an interactive workstation.
*
*-----------------------------------------------------------------------

*     WDT File

      IF (KEY1.EQ.KFWDT.AND.KWDFLS.EQ.KFLOP) THEN
	CLOSE(UNIT = ICHAN, IOSTAT = IOS)
	IF (IOS.EQ.0) THEN
	  KWDFLS = KFLCL
	ELSE
	  KWDFLS = KFLNA
	  KERROR = -1011
	ENDIF

*     Error Message File

      ELSE IF (KEY1.EQ.KFEMES.AND.KEMFLS.EQ.KFLOP) THEN
	CLOSE(UNIT = ICHAN, IOSTAT = IOS)
	IF (IOS.EQ.0) THEN
	  KEMFLS = KFLCL
	ELSE
	  KEMFLS = KFLNA
	  KERROR = -1012
	ENDIF

*     Error Logfile

      ELSE IF (KEY1.EQ.KFERR) THEN
        INQUIRE(UNIT = ICHAN, OPENED = ISOPEN)
	IF (ISOPEN.AND.ICHAN.NE.0.AND.ICHAN.NE.6) THEN
*          Close if the error stream has not been preconnected.
           IF (.NOT.ERPREC) THEN
              CLOSE(UNIT = ICHAN, IOSTAT = IOS)
              IF (IOS.NE.0) THEN
*                Problem when closing
	         KERROR = -1014
	      ENDIF
           ELSE
*             Preconnected - let the application close the file!
           ENDIF
	ENDIF

*     Font Database File

      ELSE IF (KEY1.EQ.KFDATA.AND.KDBFLS.EQ.KFLOP) THEN
	CLOSE(UNIT = KDBFLU, IOSTAT = IOS)
	IF (IOS.NE.0) THEN
	  KDBFLS = KFLNA
	  KERROR = -1013
	ELSE
	  KDBFLS = KFLCL
	ENDIF

*     Central Segment Store

      ELSE IF (KEY1.EQ.KFCSS.AND.KCSFLS.EQ.KFLOP) THEN
	CLOSE(UNIT = KCSFLU, IOSTAT = IOS)
	IF (IOS.NE.0) THEN
	  KCSFLS = KFLNA
	  KERROR = -1041
	ELSE
	  KCSFLS = KFLCL
	ENDIF

*     Input or Output Workstation

      ELSE IF (KEY1.EQ.KFWKFR.OR.KEY1.EQ.KFWKFW) THEN
        INQUIRE(UNIT = ICHAN, OPENED = ISOPEN)
        IF (ISOPEN.AND.ICHAN.NE.5.AND.ICHAN.NE.6) THEN
*          Close if the workstation's stream has not been preconnected.
           IF (.NOT.ISPREC(KWKIX)) THEN
              CLOSE(UNIT = ICHAN, IOSTAT = IOS)
              IF (IOS.NE.0) THEN
*                Problem when closing
	         KERROR = 25
	      ENDIF
           ELSE
*             Preconnected - let the application close the file!
           ENDIF
        ENDIF

*     Interactive Workstation

      ELSE IF (KEY1.EQ.KFWKT) THEN
        INQUIRE(UNIT = ICHAN, OPENED = ISOPEN)
        IF (ISOPEN) THEN
	  CALL GKIOSB(ICHAN, KOFF)
	  IF (ICHAN.NE.5) THEN
	    CLOSE(UNIT = ICHAN, IOSTAT = IOS)
            ICHAN1 = ICHAN + 1
	    CLOSE(UNIT = ICHAN1, IOSTAT =IOS)
	    IF (IOS.NE.0) THEN
	      KERROR = 25
	    ENDIF
	  ENDIF
	  ISWKT(KWKIX) = .FALSE.
        ENDIF

      ENDIF

      RETURN

      END
