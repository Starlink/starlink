      SUBROUTINE sgs_SELZ (IZONID, JSTAT)
*+
*  Name:
*     SELZ

*  Purpose:
*     Select a new zone.

*  Language:
*     Starlink Fortran 77

*  Description:
*     1) If the current and new zones are on different workstations,
*     the current workstation is deactivated and the new one is
*     activated instead.
*
*     2) The window/viewport for the new zone is set.

*  Arguments:
*     IZONID = INTEGER (Given)
*         Zone identifier
*     JSTAT = INTEGER (Given & Returned)
*         Inherited status (if option selected)
*         Status: 0=OK (if non-inherited)

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
*     GMI           i        workstation category - metafile input
*     GINPUT        i             "         "     - input
*     GWISS         i             "         "     - workstation-
*     independent
*     segment Storage
*     GAVTIV        i        workstation state - active
*     GHIGHR        i        transformation priority - higher

*  Constants From Sgscom:
*     MXZ           i        maximum number of zones allowed

*  Errors:
*     INVALID ZONE ID
*     SPECIFIED ZONE DOES NOT EXIST

*  Externals:
*     sgs_1HSTAT, sgs_1ERR, sgs_FLUSH, sgs_1SETTX, sgs_SPEN,
*     sgs_1GKERR, GQWKS, GDAWK, GACWK, GSELNT, GSVPIP, GSWN, GSVP

*  Read From Common:
*     ISZID         i        current zone ID
*     IZTW          i()      zone table - workstation ID
*     ZTW           r()        "    "   - window
*     ZTV           r()        "    "   - viewport
*     IWTID         i()      workstation table - workstation ID
*     IWTCA         i()      workstation table - category
*     IPEN          i        current SGS pen

*  Written To Common:
*     ISZID         i        current zone ID

*-

      IMPLICIT NONE

      INTEGER IZONID,JSTAT

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      INTEGER IWKID,IWC,IERR,ISTATE

      CHARACTER RNAME*4
      PARAMETER (RNAME='SELZ')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Validate zone ID
      IF (IZONID.LT.1 .OR. IZONID.GT.MXZ) THEN
         CALL sgs_1ERR(SGS__INVZN,RNAME,'Invalid zone ID',JSTAT)
         GO TO 9999
      END IF

*  Flush any pending output
      CALL sgs_FLUSH

*  Old workstation ID
      IWC = ABS(IZTW(ISZID))

*  New workstation ID
      IWKID=ABS(IZTW(IZONID))
      IF (IWKID.EQ.0) THEN
         CALL sgs_1ERR(SGS__ZONNF,RNAME,'Specified zone does not exist',
     :   JSTAT)
         GO TO 9999
      END IF

*  Set new current zone ID
      ISZID=IZONID

*  Switch workstation if appropriate
      IF (IWC.NE.IWKID) THEN
        IF (IWC.NE.0) THEN

*     Deactivate if active
          IF (IWTCA(IWC).NE.GMI.AND.IWTCA(IWC).NE.GINPUT
     :                            .AND.IWTCA(IWC).NE.GWISS) THEN
             CALL GQWKS(IWTID(IWC),IERR,ISTATE)
             IF (IERR.NE.0) THEN
               CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQWKS',
     :         JSTAT)
               GO TO 9999
             END IF

             IF (ISTATE.EQ.GACTIV) CALL GDAWK(IWTID(IWC))
           END IF
        END IF
      END IF
      IF (IWTCA(IWKID).NE.GMI.AND.IWTCA(IWKID).NE.GINPUT
     :                             .AND.IWTCA(IWKID).NE.GWISS) THEN

*     Activate if not active
         CALL GQWKS(IWTID(IWKID),IERR,ISTATE)
         IF (IERR.NE.0) THEN
           CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQWKS',
     :     JSTAT)
           GO TO 9999
         END IF

         IF (ISTATE.NE.GACTIV) CALL GACWK(IWTID(IWKID))

*     Recalculate text attributes
         CALL sgs_1SETTX

*     Reselect the current SGS pen to transfer the colour to markers and
*     text
         CALL sgs_SPEN(IPEN)
      END IF

*  Ensure that transformation 1 is selected and that its input priority is
*  greater that transformation zero.
      CALL GSELNT(1)
      CALL GSVPIP(1,0,GHIGHR)

*  Window
      CALL GSWN(1,ZTW(1,IZONID),ZTW(2,IZONID),
     :            ZTW(3,IZONID),ZTW(4,IZONID))

*  Viewport
      CALL GSVP(1,ZTV(1,IZONID),ZTV(2,IZONID),
     :             ZTV(3,IZONID),ZTV(4,IZONID))

*  Check for GKS errors
      CALL sgs_1GKERR(RNAME,JSTAT)

*  Exit
 9999 CONTINUE

      END

