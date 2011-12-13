      SUBROUTINE GNS_IGAG ( AGINAM, NAME, STATUS )

*+
*  Name:
*     GNS_IGAG

*  Purpose:
*     Inquire GKS workstation name from AGI name

*  Invocation:
*     CALL GNS_IGAG( AGINAM, NAME, STATUS )

*  Description:
*     A GKS workstation name that corresponds to the specified AGI name
*     is returned. If the name is longer than the supplied character
*     variable the name is truncated.

*  Arguments:
*     AGINAM = CHARACTER*(*) (Given)
*        AGI name
*     NAME = CHARACTER*(GNS__SZDEV) (Returned)
*        Device name
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Notes:
*     This routine is used by AGI and will not normally be called by an
*     applications program.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     NE: Nick Eaton (Durham University)

*  History:
*     11-JUN-1990 (NE):
*        Original version.
*      4-AUG-1992 (NE):
*        Replace NAMNR with AGNNR
*      1-SEP-1992 (NE):
*        Updated prologue.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'GNS_ERR'
      INCLUDE 'GNS_PAR'
      INCLUDE 'gns.cmn'

*  Arguments Given:
      CHARACTER*(*) AGINAM

*  Arguments Returned:
      CHARACTER*(*) NAME

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER*(GNS__SZAGI) LAGI
      INTEGER I, ISEQNO, ISTAT, IWKTYP, J, MATCH
*.

*   Initialise the returned name
      NAME = ' '

*   Check status on entry
      IF (STATUS.EQ.0) THEN

*   Copy the name to a local variable and convert to upper case
         LAGI = AGINAM
         CALL CHR_UCASE(LAGI)

*   Check that the name begins with 'AGI_'
         IF ( LAGI(:4) .EQ. 'AGI_' ) THEN
            I = 5
         ELSE
            STATUS = GNS__AGNNR
            CALL EMS_REP( 'GNS_IGAG_AGNNR1',
     :                    'AGI name not recognised', STATUS )
            GOTO 99
         ENDIF

*   Make sure GNS has started
         CALL GNS_1INITG(STATUS)

*   Decode the workstation type and sequnce number from the AGI name
         ISTAT = 0
         CALL GNS_1INTIN(LAGI,I,IWKTYP,ISTAT)
         IF ( ISTAT .EQ. 0 ) THEN
            CALL GNS_1INTIN(LAGI,I,ISEQNO,ISTAT)
         ENDIF
         IF ( ISTAT .NE. 0 ) THEN
            STATUS = GNS__AGNNR
            CALL EMS_REP( 'GNS_IGAG_AGNNR2',
     :                    'AGI name not recognised', STATUS )
            GOTO 99
         ENDIF

*   Look through the common block entries for the workstation type
*   and sequence number
         MATCH = 0
         DO 10 J = 1, NUMNAM
            IF ( IWKTYP .EQ. ITYPES(J) ) THEN
               MATCH = MATCH + 1
               NAME = NAMES(J)
            ENDIF

*   If the sequence number is non-zero and it matches then jump out of loop
            IF ( ISEQNO .NE. 0 ) THEN
               IF ( ISEQNO .EQ. ISEQNG(J) ) GOTO 99
            ENDIF
  10     CONTINUE

*   If the sequence number is zero and there is at least one match
*   then return a name
         IF ( ( ISEQNO .EQ. 0 ) .AND. ( MATCH .GE. 1 ) ) GOTO 99

*   If this point was reached then no match was found
         NAME = ' '
         STATUS = GNS__AGNNR
         CALL EMS_REP( 'GNS_IGAG_AGNNR3',
     :                 'AGI name not recognised', STATUS )
      ENDIF

  99  CONTINUE

      END

