      SUBROUTINE sgs_REQCU (X,Y, N)
*+
*  Name:
*     REQCU

*  Purpose:
*     Obtain cursor position and choice number.

*  Language:
*     Starlink Fortran 77

*  Description:
*     This routine uses an escape function specific to the RAL GKS.

*  Arguments:
*     X = REAL (Returned)
*         Cursor position (x)
*     Y = REAL (Returned)
*         "      "     (y)
*     N = INTEGER (Returned)
*         Choice. Zero if no choice, -1 if X & Y are not
*         valid

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From GKS_PAR:
*     GREQU   i      mode - request
*     GOK     i      input status - OK

*  Errors:
*     Error returned by GKS inquiry

*  Externals:
*     sgs_ICUAV, sgs_1ILCMO, sgs_SETCU, sgs_1UPCAS, sgs_OPOLY,
*     sgs_OTEXT, sgs_1ERR, GSLCM, GRQLC, GQCNTN, GQNT

*  Read From Common:
*     ISZID   i      current zone ID
*     IZTW    i()    zone table - SGS workstation ID
*     IWTID   i()    workstation table - GKS workstation ID
*     NCHODV  i      current choice device
*     CHOIST  c      valid choice keys
*     LCHOST  i      number of valid choice keys
*     NPOLY   i      length of current polyline
*     NTEXT   i      length of current text string

*-

      IMPLICIT NONE

      REAL X,Y
      INTEGER N

      INCLUDE 'sgscom'
      INCLUDE 'GKS_PAR'
      INCLUDE 'SGS_ERR'

      INTEGER MODE,IESW,ISTAT,ISWKID,ITNR,JSTAT,IERR,ICNTR
      INTEGER NLCD, NSKD, NVLD, NCHD, NPCD, NSTD
      LOGICAL AVAIL
      REAL WINDO(4),VIEWP(4),XNDC,YNDC
      CHARACTER*1 C
      CHARACTER*5 RNAME
      PARAMETER (RNAME='REQCU')

*  Declarations for escape function
      INTEGER IA(5), IER, LIESC, LOESC
      REAL RA(1)
      CHARACTER*80 STR(1), IESCDR(1), OESCDR(1)
      DATA IA/1, GLOCAT, 1, GCHOIC, 2/

      DATA RA/0.0/, STR/' '/


*  Initialize output arguments
      X = 0.0
      Y = 0.0
      N = -1

      CALL sgs_ICUAV(AVAIL)
      IF (AVAIL) THEN

*     Flush any outstanding plotting
         IF (NPOLY.GT.1) CALL sgs_OPOLY
         IF (NTEXT.GT.0) CALL sgs_OTEXT

*     Save current mode of locator and set to request if necessary
         ISWKID = IWTID(ABS(IZTW(ISZID)))
         CALL sgs_1ILCMO(MODE,IESW,JSTAT)
         IF (JSTAT.NE.0) GO TO 9999

         IF (MODE.NE.GREQU) CALL GSLCM(ISWKID,1,GREQU,IESW)

*     Check that the device a keyboard (choice device number 2).
         CALL GQLI(IWTTY(ISWKID),IERR,NLCD,NSKD,NVLD,NCHD,NPCD,NSTD)
         IF (IERR.NE.0) THEN
            CALL SGS_1ERR(SGS__INQER,RNAME,'Error returned by GQLI',
     :      JSTAT)
            GO TO 9999
         END IF

*     Use RAL-GKS escape function to associate the locator and the
*     keyboard. (The 5th argument ought to be 0 but this causes an
*     adjustable array dimension error.)
         IF (NCHD.GE.2) THEN
            CALL GPREC(5, IA, 1, RA, 1, 1, STR, 1, IER, LIESC,
     :      IESCDR)
            CALL GESC(-1, LIESC, IESCDR, 1, LOESC, OESCDR)
         END IF

*     Request a locator position
         CALL GRQLC(ISWKID,1,ISTAT,ITNR,X,Y)

         IF (ISTAT.EQ.GOK)  THEN

*        Request the choice (which doesn't require any operator action)
           IF (NCHD.GE.2) THEN
              CALL GRQCH(ISWKID,2,ISTAT,N)

*          Turn off escape
              CALL GESC(-2, LIESC, IESCDR, 1, LOESC, OESCDR)
           END IF

*       Convert position to current transformation
           CALL GQCNTN(IERR,ICNTR)
           IF (IERR.NE.0) THEN
             CALL SGS_1ERR(SGS__INQER,RNAME,'Error returned by GQCNTN',
     :       JSTAT)
             GO TO 9999
           END IF
           IF (ITNR.NE.ICNTR) THEN

*          Convert position to NDC
             CALL GQNT(ITNR,IERR,WINDO,VIEWP)
             IF (IERR.NE.0) THEN
               CALL SGS_1ERR(SGS__INQER,RNAME,'Error returned by GQNT',
     :         JSTAT)
               GO TO 9999
             END IF
             XNDC = (X - WINDO(1))/(WINDO(2) - WINDO(1)) *
     :              (VIEWP(2) - VIEWP(1)) + VIEWP(1)
             YNDC = (Y - WINDO(3))/(WINDO(4) - WINDO(3)) *
     :              (VIEWP(4) - VIEWP(3)) + VIEWP(3)

*         Convert to current normalization transformation
             CALL GQNT(ICNTR,IERR,WINDO,VIEWP)
             IF (IERR.NE.0) THEN
               CALL SGS_1ERR(SGS__INQER,RNAME,'Error returned by GQNT',
     :         JSTAT)
               GO TO 9999
             END IF
             X = (XNDC - VIEWP(1))/(VIEWP(2) - VIEWP(1)) *
     :              (WINDO(2) - WINDO(1)) + WINDO(1)
             Y = (YNDC - VIEWP(3))/(VIEWP(4) - VIEWP(3)) *
     :              (WINDO(4) - WINDO(3)) + WINDO(3)
           END IF

*        Set new cursor position
           CALL sgs_SETCU(X,Y)
         END IF

*     Restore locator mode
         IF (MODE.NE.GREQU) CALL GSLCM(ISWKID,1,MODE,IESW)

         IF (ISTAT.EQ.GOK) THEN

*        If choice device is the keyboard then search the choice string for
*        the character. This will set N to zero if the key is not in the
*        choice string.
            IF (NCHODV.EQ.0) THEN
               CALL sgs_1UPCAS(CHAR(N+31),C)
               N = INDEX(CHOIST(1:LCHOST),C)
            END IF
         ELSE IF (ISTAT.EQ.GNCHOI) THEN
            N = 0
         END IF
      END IF

 9999 CONTINUE

      END
