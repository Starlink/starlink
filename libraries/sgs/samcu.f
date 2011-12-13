      SUBROUTINE sgs_SAMCU (X,Y)
*+
*  Name:
*     SAMCU

*  Purpose:
*     Sample cursor position.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     X = REAL (Returned)
*         Cursor position in world coordinates (x)
*     Y = REAL (Returned)
*         "       "      "   "        "      (y)

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
*     GSAMPL   i     mode - sample

*  Errors:
*     Errror returned by GKS inquiry

*  Externals:
*     sgs_SETCU, sgs_OPOLY, sgs_OTEXT, sgs_1ERR, GSMLC, GQNTN, GQNT

*  Read From Common:
*     ISZID    i     current zone ID
*     IZTW     i()   zone table - workstation ID
*     NPOLY    i     length of current polyline
*     NTEXT    i     length of current text string

*-

      IMPLICIT NONE

      REAL X,Y

      INCLUDE 'GKS_PAR'

      INCLUDE 'sgscom'

      INCLUDE 'SGS_ERR'


      REAL WINDO(4),VIEWP(4),XNDC,YNDC
      INTEGER JSTAT,IERR,ITNR,ICNTR
      CHARACTER*5 RNAME
      PARAMETER (RNAME='SAMCU')



*  Flush any outstanding plotting
      IF (NPOLY.GT.1) CALL sgs_OPOLY
      IF (NTEXT.GT.0) CALL sgs_OTEXT

*  Sample locator
      CALL GSMLC(ABS(IZTW(ISZID)),1,ITNR,X,Y)

*  Convert position to current transformation
      CALL GQCNTN(IERR,ICNTR)
      IF (IERR.NE.0) THEN
        CALL SGS_1ERR(SGS__INQER,RNAME,'Error returned by GQCNTN',JSTAT)
        GO TO 9999
      END IF
      IF (ITNR.NE.ICNTR) THEN

*    Convert position to NDC
        CALL GQNT(ITNR,IERR,WINDO,VIEWP)
        IF (IERR.NE.0) THEN
          CALL SGS_1ERR(SGS__INQER,RNAME,'Error returned by GQNT',JSTAT)
          GO TO 9999
        END IF
        XNDC = (X - WINDO(1))/(WINDO(2) - WINDO(1)) *
     :         (VIEWP(2) - VIEWP(1)) + VIEWP(1)
        YNDC = (Y - WINDO(3))/(WINDO(4) - WINDO(3)) *
     :         (VIEWP(4) - VIEWP(3)) + VIEWP(3)

*    Convert to current norm trans
        CALL GQNT(ICNTR,IERR,WINDO,VIEWP)
        IF (IERR.NE.0) THEN
           CALL SGS_1ERR(SGS__INQER,RNAME,'Error returned by GQNT',
     :                                                            JSTAT)
           GO TO 9999
        END IF
        X = (XNDC - VIEWP(1))/(VIEWP(2) - VIEWP(1)) *
     :      (WINDO(2) - WINDO(1)) + WINDO(1)
        Y = (YNDC - VIEWP(3))/(VIEWP(4) - VIEWP(3)) *
     :      (WINDO(4) - WINDO(3)) + WINDO(3)
      END IF

*  Set new cursor position
      CALL sgs_SETCU(X,Y)

 9999 CONTINUE

      END
