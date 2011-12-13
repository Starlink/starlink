      SUBROUTINE sgs_SPEN (NPEN)
*+
*  Name:
*     SPEN

*  Purpose:
*     Select a pen for line marker and text plotting.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     NPEN = INTEGER (Given)
*         Pen number (1,2,3...)

*  Notes:
*     The following ASF flag settings are assumed:
*           Polyline colour index:   GBUNDL
*           Polymarker colour index: GINDIV
*           Text colour index:       GINDIV

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
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From GKS_PAR:
*     GOUTPT     i     workstation category - output
*     COUTIN     i          "          "    - input/output

*  Externals:
*     sgs_1ERR, sgs_OTEXT, sgs_OPOLY, GQPLR, GSTXCI, GSPMCI, GSPLI

*  Read From Common:
*     IZTW       i()   zone table - workstation
*     IWTID      i()   workstation table - workstation ID
*     IWTCA      i()        "        "   - category
*     ISZID      i     current zone ID
*     NPOLY      i     length of current polyline
*     NTEXT      i     length of current text string

*  Written To Common:
*     IPEN       i     current SGS pen

*-

      IMPLICIT NONE

      INTEGER NPEN

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      INTEGER IERR,LNTYPE,LWIDTH,ICOL,JSTAT,IWKID
      CHARACTER*4 RNAME
      PARAMETER (RNAME='SPEN')



*  Flush any pending output
      IF (NPOLY.GT.1) CALL sgs_OPOLY
      IF (NTEXT.GT.0) CALL sgs_OTEXT

*  Current workstation ID
      IWKID = ABS(IZTW(ISZID))

*  If not an output workstation skip setting the text and marker colour
      IF (IWTCA(IWKID).NE.GOUTPT.AND.IWTCA(IWKID).NE.GOUTIN) GO TO 9000

*  Inquire the colour index for this pen on the current workstation
      CALL GQPLR(IWTID(IWKID),NPEN,GSET,IERR,LNTYPE,LWIDTH,ICOL)
      IF (IERR.EQ.61) THEN
         CALL sgs_1ERR(SGS__PENER,RNAME,'SGS pen does not exist',JSTAT)
         GO TO 9999
      ELSE IF (IERR.NE.0) THEN
         CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQPLR',JSTAT)
         GO TO 9999
      END IF

*  Set the marker and text attribute to this colour
      CALL GSTXCI(ICOL)
      CALL GSPMCI(ICOL)

9000  CONTINUE

*  Set the pen (for line drawing)
      CALL GSPLI(NPEN)

*  Remember it
      IPEN=NPEN

9999  CONTINUE

      END
