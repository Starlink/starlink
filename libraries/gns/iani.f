      SUBROUTINE GNS_IANI ( NAME, AGINAM, STATUS )

*+
*  Name:
*     GNS_IANI

*  Purpose:
*     Inquire AGI name of IDI workstation

*  Invocation:
*     CALL GNS_IANI( NAME, AGINAM, STATUS )

*  Description:
*     The AGI name corresponding to the specified IDI workstation is
*     returned.
*
*     If the name is longer than the supplied character variable the
*     name is truncated.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        IDI workstation name
*     AGINAM = CHARACTER*(GNS__SZAGI) (Returned)
*        AGI name
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Notes:
*     The AGI name is constructed from 'AGI_<wktype>_<seqno>' using the
*     GKS workstation type and a sequence number. The GKS workstation
*     type is obtained from the AGITYPE attribute. A sequence number of
*     zero is used unless there is an explicit sequence number in the
*     IDINAMES file.
*
*     This routine is used by AGI and will not normally be called by an
*     applications program.

*  Copyright:
*     Copyright (C) 1990, 1991, 1992 Science & Engineering Research Council.
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
*      2-DEC-1991 (NE):
*        Call gns_1AGIN
*      4-AUG-1992 (NE):
*        Added name to NAMNR error report
*     21-AUG-1992 (NE):
*        Remove any special characters from the name
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
      CHARACTER*(*) NAME

*  Arguments Returned:
      CHARACTER*(GNS__SZAGI) AGINAM

*  Status:
      INTEGER STATUS

*  Local Constants:
*   Maximum number of translations allowed
      INTEGER MAXTRY
      PARAMETER (MAXTRY=5)

*  Local Variables:
      CHARACTER EQNAM*64
      CHARACTER*(GNS__SZNAM) CNAME, CNAME1
      CHARACTER*(GNS__SZDEV) CDEV
      CHARACTER*(GNS__SZTYP) WKTYPE
      INTEGER CLEN, ISEQNO, IWKTYP, J, LEDEV, LENEQ, LENAM, LNAME1,
     :        MATCH, NTRIES
      LOGICAL PLUS
*.

*   Initialise the AGI name and components
      AGINAM = ' '
      IWKTYP = 0
      ISEQNO = 0

*   Check status on entry
      IF (STATUS.EQ.0) THEN

*   Copy the workstation name to a local string
         CNAME = NAME
         NTRIES = 0

*   Replace the '+' special character with a space
         CALL GNS_1SPECH( '+', ' ', CNAME, PLUS )

  10     CONTINUE

*   Split into name and device components
*   Note. At present the device name is ignored.
         CALL GNS_1DECSP(CNAME,CNAME1,LNAME1,CDEV,LEDEV)
         CNAME = CNAME1
         LENAM = LNAME1

*   See if the name is a logical name that will translate to another
         CALL GNS_1LOGTR(CNAME,EQNAM,LENEQ,STATUS)
         IF ( ( LENEQ .GT. 0 ) .AND. ( NTRIES .LT. MAXTRY ) )THEN
            CNAME = EQNAM(:LENEQ)
            LENAM = LENEQ
            NTRIES = NTRIES + 1
            GOTO 10
         ENDIF

*   Make sure GNS has started
         CALL GNS_1INITI(STATUS)
         IF ( STATUS .NE. 0 ) GOTO 99

*   Look through the common block entries for a name match
         MATCH = 0
         DO 20 J = 1, NUMNAI

*   If the names match exactly then jump out of the loop
            IF ( CNAME(:LENAM) .EQ. NAMESI(J) ) THEN
               WKTYPE = TYPESI(J)
               ISEQNO = ISEQNI(J)
               GOTO 30
            ENDIF

*   If the name matches an abbreviation then remember it
            IF ( CNAME(:LENAM) .EQ. NAMESI(J)(:LENAM) ) THEN
               WKTYPE = TYPESI(J)
               ISEQNO = ISEQNI(J)
               MATCH = MATCH + 1
            ENDIF
  20     CONTINUE

*   Flag an error if no matches were found
         IF ( MATCH .LT. 1 ) THEN
            AGINAM = ' '
            STATUS = GNS__NAMNR
            CALL EMS_SETC( 'NAME', CNAME1(:LNAME1) )
            CALL EMS_REP( 'GNS_IANI_NAMNR',
     :                    'Device name ^NAME not recognised', STATUS )
            GOTO 99

*   And flag an error if more than one match was found
         ELSEIF( MATCH .GT. 1 ) THEN
            STATUS = GNS__AMBNAM
            CALL EMS_REP( 'GNS_IANI_AMBNAM',
     :                    'Ambiguous workstation name', STATUS )
            GOTO 99
         ENDIF
      ENDIF

*   Get the AGITYPE for this device. Make sure the common block
*   contains data for this type
  30  CONTINUE
      CALL GNS_1RDWSI(WKTYPE,STATUS)
      IF ( ( AGITYI .NE. 0 ) .AND. ( STATUS .EQ. 0 ) ) THEN
         IWKTYP = AGITYI
      ENDIF

*   Now have a workstation type and sequence number from which
*   to construct the AGI name.
      IF ( IWKTYP .NE. 0 ) THEN
         CALL GNS_1AGIN(IWKTYP,ISEQNO,AGINAM,CLEN)
      ELSE
         STATUS = GNS__AGTND
         CALL EMS_REP( 'GNS_IANI_AGTND',
     :                 'AGI type not defined', STATUS )
      ENDIF

  99  CONTINUE

      END

