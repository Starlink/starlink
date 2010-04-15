      INTEGER FUNCTION GRCURS(IDENT,IX,IY,IXREF,IYREF,MODE,POSN,CH)
*+
*
*     - - - - - - - -
*       G R C U R S
*     - - - - - - - -
*
*   Returns cursor position and key selected by the user
*
*   Given
*      IDENT    i     Device identifier (IGNORED)
*      IX       r     Cursor position X (absolute coordinates)
*      IY       r     Cursor position Y (absolute coordinates)
*      IXREF    i     x-coordinate of anchor point.
*      IYREF    i     y-coordinate of anchor point.
*      MODE     i     type of rubber-band feedback.
*      POSN     i     ???????????
*
*   Returned
*      IX       r     Cursor position X (absolute coordinates)
*      IY       r     Cursor position Y (absolute coordinates)
*      CH       c     Character code of key pressed, or CHAR(0) if an
*                     error occurs
*
*   Read from COMMON
*      GRCIDE   i     Device identifier
*      GRWKID   i()   Workstation id
*      GRTYP    i()   Workstation type
*
*   Constants from GKS_PAR
*      GNONE    i     No Input
*      GNCHOI   i     No choice
*
*   Constants from GRECOM
*      TRN      i     Normalization transformation number
*
*   D.L.Terrett  Starlink  Jul 1987
*
*   This function can only be implemented properly with a level c GKS
*   implementation but RAL-GKS contains an escape function that allows a
*   choice device and a locator to be triggered by the same key
*   depression. All workstations with keyboards have the keyboard as
*   choice device number 2.
*
*   20 May 1988  Assume that all devices which support the RAL escape
*                use a keyboard
*   07 Oct 1991  Don't assume that transformation 0 has the highest input
*                priority
*   15 May 1992  Translate GKS break and no choice back to the appropriate
*                keyboard keys
*   13 Feb 1995  New arguments for pgplot 5.0
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'
      INCLUDE 'PGP_ERR'
      INCLUDE 'GKS_PAR'

      INTEGER IDENT, IX, IY, IXREF, IYREF, MODE, POSN
      CHARACTER*1 CH

      INTEGER GRCHKT

      REAL VIEWP(4), WINDO(4), VIEWP1(4), WINDO1(4), XNDC, YNDC, X, Y
      REAL CVIEWP(4), RVIEWP(4), CWINDO(4), RWINDO(4)
      INTEGER ISTAT, IERR, ITR, ITUS, KCHNR
      INTEGER NLCD, NSKD, NVLD, NCHD, NPCD, NSTD
      CHARACTER*80 CA(1)
      INTEGER LCA(1)
*  Declarations for escape function
      INTEGER IA(5), IER, LIESC, LOESC
      REAL RA(1)
      CHARACTER*80 IESCDR(1), OESCDR(1)

*  ASCII character codes
      INTEGER ISPACE, ICR, IDEL
      PARAMETER ( ISPACE = 32, ICR = 13, IDEL = 127)

*  Count of failures
      INTEGER IFAILS
      DATA IFAILS/0/

* Data statements for escape function
      DATA IA/1, GLOCAT, 1, GCHOIC, 2/
      DATA RA, LIESC, LOESC /0.0, 1, 1/

      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRCURS - No PGPLOT device open',
     :   GRNODO)
         GO TO 9000
      END IF

*   Check that this workstation supports cursor input
      CALL GQLI(GRTYP(GRCIDE),IERR,NLCD,NSKD,NVLD,NCHD,NPCD,NSTD)
      IF (IERR.NE.0) THEN
         CALL ERR_REP('GROUON',
     :   'GRCURS - PGPLOT device is output only', GROUON)
         GO TO 9000
      ELSE IF (NLCD.LT.1) THEN
         CALL ERR_REP('GRNOCU',
     :   'GRCURS - PGPLOT device does not have a cursor', GRNOCU)
         GO TO 9000
      ELSE IF (NCHD.LT.2) THEN
         CALL ERR_REP('GRNOKE',
     :   'GRCURS - PGPLOT device does not have a keyboard', GRNOKE)
         GO TO 9000
      END IF

*   Transform initial cursor position to NDC
      CALL GQNT(TRN,IERR,WINDO,VIEWP)
      IF (IERR.NE.0) THEN
         CALL GRQREP('GRCURS', 'GQNT', IERR)
         GO TO 9000
      END IF
      XNDC = VIEWP(1) + (REAL(IX) - WINDO(1)) *
     :          (VIEWP(2) - VIEWP(1)) / (WINDO(2) - WINDO(1))
      YNDC = VIEWP(3) + (REAL(IY) - WINDO(3)) *
     :          (VIEWP(4) - VIEWP(3)) / (WINDO(4) - WINDO(3))

*   Check that it is inside the workstation window
      CALL GQWKT(GRWKID(GRCIDE),IERR,ITUS,RWINDO,CWINDO,RVIEWP,
     :                                                   CVIEWP)
      IF (IERR.NE.0) THEN
         CALL GRQREP('GRCURS', 'GQWKT', IERR)
         GO TO 9000
      END IF
      XNDC = MAX(RWINDO(1),MIN(RWINDO(2),XNDC))
      YNDC = MAX(RWINDO(3),MIN(RWINDO(4),YNDC))

*   Initialize the data record
      CALL GPREC(0, 0, 0, 0.0, 0, 1, ' ', 1, IERR, LCA, CA)

*   Set the initial position
      CALL GINLC(GRWKID(GRCIDE),1,0,XNDC,YNDC,1,RVIEWP(1),RVIEWP(2),
     :           RVIEWP(3),RVIEWP(4),1,CA)

*   Use RAL-GKS escape function to associate the locator and the
*   keyboard. (The 5th argument ought to be 0 but this causes an
*   adjustable array dimension error).
      CALL GPREC(5, IA, 0, RA, 0, 1, " ", 1, IER, LIESC, IESCDR)
      CALL GESC(-1, LIESC, IESCDR, 1, LOESC, OESCDR)

*   Request the cursor position
      CALL GRQLC(GRWKID(GRCIDE),1,ISTAT,ITR,X,Y)
      IF (ISTAT.EQ.GNONE) THEN

*   This is a real error; we don't have a valid x,y position
         CALL ERR_REP('GRCUPO',
     :   'GRCURS - Error getting cursor position', GRCUPO)
         GO TO 9000
      END IF

*   Request the choice (which doesn't require any operator action).
      CALL GRQCH(GRWKID(GRCIDE),2,ISTAT,KCHNR)

*   Turn off escape
      CALL GESC(-2, LIESC, IESCDR, 1, LOESC, OESCDR)

*   Convert key hit as returned by RAL-GKS to an ASCII code. GKS returns the
*   character position in the ASCII order with space = 1. NOCHOICE for
*   carriage return and NONE for other non-printing characters. NONE is
*   mapped to delete.

      IF (ISTAT.EQ.GNONE) THEN
         CH = CHAR(IDEL)
      ELSE IF ( ISTAT.EQ.GNCHOI) THEN
         CH = CHAR(ICR)
      ELSE
         CH = CHAR(KCHNR + ISPACE - 1)
      END IF

*   Convert input position to NDC
      CALL GQNT(ITR,IERR,WINDO1,VIEWP1)
      IF (IERR.NE.0) THEN
         CALL GRQREP('GRCURS', 'GQNT', IERR)
         GO TO 9000
      END IF
      XNDC = VIEWP1(1) + (X - WINDO1(1)) *
     :          (VIEWP1(2) - VIEWP1(1)) / (WINDO1(2) - WINDO1(1))
      YNDC = VIEWP1(3) + (Y - WINDO1(3)) *
     :          (VIEWP1(4) - VIEWP1(3)) / (WINDO1(4) - WINDO1(3))

*   Convert to absolute coordinates
      IX = NINT(WINDO(1) + (XNDC - VIEWP(1)) *
     :          (WINDO(2) - WINDO(1))/(VIEWP(2) - VIEWP(1)))
      IY = NINT(WINDO(3) + (YNDC - VIEWP(3)) *
     :          (WINDO(4) - WINDO(3))/(VIEWP(4) - VIEWP(3)))

*   Success
      GRCURS = 1
      GO TO 9999

 9000 CONTINUE
      IF (IFAILS.EQ.10) CALL ERR_REP('GRTMCF',
     : 'GRCURS - too many cursor failures', GRTMCF)
      IFAILS = IFAILS + 1
      GRCURS = 0
      CH = CHAR(0)

 9999 CONTINUE
      END
