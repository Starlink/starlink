      SUBROUTINE GNS_IANG ( NAME, AGINAM, STATUS )

*+
*  Name:
*     GNS_IANG

*  Purpose:
*     Inquire AGI name of GKS workstation

*  Invocation:
*     CALL GNS_IANG( NAME, AGINAM, STATUS )

*  Description:
*     The AGI name corresponding to the specified GKS workstation is
*     returned. If the name is longer then the supplied character
*     variable the name is truncated.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        GKS workstation name
*     AGINAM = CHARACTER*(GNS__SZAGI) (Returned)
*        AGI name
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Notes:
*     The AGI name is constructed from 'AGI_<wktype>_<seqno>' using the
*     GKS workstation type and a sequence number. The GKS workstation
*     type from the GKSNAMES file is used unless there is an AGITYPE
*     keyword for that device. A sequence number of zero is used unless
*     there is an explicit sequence number in the GKSNAMES file.
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
      CHARACTER*(GNS__SZNAM + GNS__SZDEV) CNAME, CNAME1
      CHARACTER*(GNS__SZDEV) CDEV, CDEV1
      INTEGER CLEN, I, ISEQNO, ISTAT, IWKTYP, J, L, LEDEV, LENEQ, LENAM,
     :        LNAME1, MATCH, NTRIES
      LOGICAL PLUS
*.

*   Initialise AGI name and components
      AGINAM = ' '
      IWKTYP = 0
      ISEQNO = 0

*   Check status on entry
      IF ( STATUS .EQ. 0 ) THEN

*   Copy the workstation name to a local string
         CNAME = NAME

*   Replace the '+' special character with a space
         CALL GNS_1SPECH( '+', ' ', CNAME, PLUS )

*   Initialise the device string length to zero to indicate none found
         LEDEV = 0
         NTRIES = 0

  10     CONTINUE

*   Split into name and device components
         CALL GNS_1DECSP(CNAME,CNAME1,LNAME1,CDEV1,L)
         CNAME = CNAME1
         LENAM = LNAME1

*   If there was a device name and we haven't got one already, save it
         IF ( ( LEDEV .EQ. 0 ) .AND. ( L .GT. 0 ) ) THEN
            CDEV = CDEV1
            LEDEV = L
         ENDIF

*   See if the name begins with 'GKS_'
         IF ( CNAME(:4) .EQ. 'GKS_' ) GOTO 20

*   See if the name is a logical name that will translate to another
         CALL GNS_1LOGTR(CNAME,EQNAM,LENEQ,STATUS)
         IF ( ( LENEQ .GT. 0 ) .AND. ( NTRIES .LT. MAXTRY ) )THEN
            CNAME = EQNAM(:LENEQ)
            LENAM = LENEQ
            NTRIES = NTRIES + 1
            GOTO 10
         ENDIF

*   Make sure GNS has started
  20     CONTINUE
         ISTAT = 0
         CALL GNS_1INITG(ISTAT)
         IF ( ISTAT .EQ. 0 ) THEN

*   Look through the common block entries for a name match
            MATCH = 0
            DO 30 J = 1, NUMNAM

*   If the workstation name matches an abbreviation then remember it
               IF ( CNAME(:LENAM) .EQ. NAMES(J)(:LENAM) ) THEN
                  MATCH = MATCH + 1
                  IWKTYP = ITYPES(J)
                  ISEQNO = ISEQNG(J)

*   If the device name matches then jump out of loop
                  IF ( LEDEV .GT. 0 ) THEN
                     IF ( CDEV(:LEDEV) .EQ. VMSNAM(J) ) GOTO 50
                  ENDIF
               ENDIF

*   If the workstation name matches exactly then jump out of the loop
               IF ( CNAME(:LENAM) .EQ. NAMES(J) ) GOTO 50

  30        CONTINUE

*   If only one match was found then continue with name handling
            IF ( MATCH .EQ. 1 ) THEN
               GOTO 50

*   And flag an error if more than one match was found
            ELSEIF( MATCH .GT. 1 ) THEN
               STATUS = GNS__AMBNAM
               CALL EMS_REP( 'GNS_IANG_AMBNAM',
     :                       'Ambiguous workstation name', STATUS )
               GOTO 99
            ENDIF
         ENDIF

*   No match was found (maybe because the database could not be
*   opened) so see if it is a 'GKS_' type name
         IF ( CNAME(:4) .EQ. 'GKS_' ) THEN
            I = 5
         ELSE
            I = 1
         ENDIF

*   Try extracting the workstation type from the name
         ISTAT = 0
         CALL GNS_1INTIN(CNAME,I,IWKTYP,ISTAT)

*   If it was successful then look for that type in the names file
         IF ( ISTAT .EQ. 0 ) THEN

*   Look through the common block entries for a type match
            MATCH = 0
            DO 40 J = 1, NUMNAM

*   If the workstation type matches then remember it
               IF ( IWKTYP .EQ. ITYPES(J) ) THEN
                  MATCH = MATCH + 1
                  ISEQNO = ISEQNG(J)
               ENDIF

*   If the device name matches then jump out of loop
               IF ( LEDEV .GT. 0 ) THEN
                  IF ( CDEV(:LEDEV) .EQ. VMSNAM(J) ) GOTO 50
               ENDIF
  40        CONTINUE

*   If more than one match was found then continue with name handling
            IF ( MATCH .GE. 1 ) THEN
               GOTO 50

*   If no matches or more than one match was found then will have to
*   use a sequence number of zero
            ELSE
               ISEQNO = 0
            ENDIF

*   Unable to decode name
         ELSE
            STATUS = GNS__NAMNR
            CALL EMS_SETC( 'NAME', CNAME1(:LNAME1) )
            CALL EMS_REP( 'GNS_IANG_NAMNR',
     :                    'Device name ^NAME not recognised', STATUS )
            GOTO 99
         ENDIF

*   See if this device has an AGITYPE associated with it
*   Make sure that the common block contains data for this type
  50     CONTINUE
         CALL GNS_1RDWST(IWKTYP,STATUS)
         IF ( ( AGITYG .NE. 0 ) .AND. ( STATUS .EQ. 0 ) ) THEN
            IWKTYP = AGITYG
         ENDIF

*   Now have a workstation type and sequence number from which
*   to construct the AGI name.
         CALL GNS_1AGIN( IWKTYP, ISEQNO, AGINAM, CLEN )
      ENDIF

  99  CONTINUE

      END

