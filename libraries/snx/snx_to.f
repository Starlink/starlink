      SUBROUTINE snx_TO (SORN)
*+
*  Name:
*     TO

*  Purpose:
*     Switch from NCAR plotting to SGS or vice versa

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     SORN = CHAR (Given)
*         New plotting interface

*  Notes:
*     The first character of SORN specifies which form of plotting
*     is about to start:
*
*       SORN(1:1) = 'S' or 's' means we have been plotting with
*                              NCAR and wish now to use SGS
*
*       SORN(1:1) = 'N' or 'n' means we have been using SGS and
*                              wish now to revert to NCAR
*
*     At the time this routine is first called, SGS must have been
*     opened and NCAR plotting must have occurred, so that the call
*     is to switch to SGS.  Subsequent calls must alternate
*     strictly between NCAR and SGS.  If this sequence is violated,
*     a message is output to the NCAR error reporting I/O unit.
*
*     An illegal SORN value is reported via a message to the
*     NCAR error reporting I/O unit.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council. All
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
*     {enter_new_authors_here}

*  History:
*     29-JUN-1988 (PTW):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     NCAR/SPPS - I1MACH, PLOTIT
*     SGS - sgs_FLUSH, sgs_ICURZ, sgs_SELZ
*     GKS - GQCNTN, GQNT, GSVP, GSWN, GSELNT

*-

      IMPLICIT NONE

      CHARACTER*(*) SORN

      INTEGER NT,J,IZ
      REAL WINDOW(4),VIEWP(4)
      INTEGER I1MACH
      CHARACTER STATE
      DATA STATE /'N'/
      SAVE STATE,NT,WINDOW,VIEWP



*  Examine argument
      IF (SORN(1:1).EQ.'S'.OR.SORN(1:1).EQ.'s') THEN

*     Changing from NCAR to SGS: check toggle state
         IF (STATE.NE.'N') GO TO 9000
         STATE='S'

*     Flush NCAR and GKS
         CALL PLOTIT(0,0,2)
         CALL sgs_FLUSH

*     Save the current normalisation transformation
         CALL GQCNTN(J,NT)
         CALL GQNT(NT,J,WINDOW,VIEWP)

*     Reselect current zone
         CALL sgs_ICURZ(IZ)
         CALL sgs_SELZ(IZ,J)

      ELSE IF (SORN(1:1).EQ.'N'.OR.SORN(1:1).EQ.'n') THEN

*     Changing from SGS to NCAR:

*     Changing from NCAR to SGS: check toggle state
         IF (STATE.NE.'S') GO TO 9000
         STATE='N'

*     Flush SGS/GKS
         CALL sgs_FLUSH

*     Restore normalisation transformation
         CALL GSVP(NT,VIEWP(1),VIEWP(2),VIEWP(3),VIEWP(4))
         CALL GSWN(NT,WINDOW(1),WINDOW(2),WINDOW(3),WINDOW(4))
         CALL GSELNT(NT)

      ELSE
         GO TO 9010
      END IF

      GO TO 9999

*  Errors
 9000 CONTINUE
      WRITE (I1MACH(4),'('' snx_TO: sequence error!'')')
      GO TO 9999

 9010 CONTINUE
      WRITE (I1MACH(4),'('' snx_TO: invalid argument!'')')

*  Exit
 9999 CONTINUE

      END
