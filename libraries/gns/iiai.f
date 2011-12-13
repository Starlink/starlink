      SUBROUTINE GNS_IIAI ( AGINAM, NAME, STATUS )

*+
*  Name:
*     GNS_IIAI

*  Purpose:
*     Inquire IDI workstation name from AGI name

*  Invocation:
*     CALL GNS_IIAI( AGINAM, NAME, STATUS )

*  Description:
*     An IDI workstation name that corresponds to the supplied AGI name
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
*   Buffer for record from file
      INTEGER IBUF(RECSIZ)
      REAL RBUF(RECSIZ)
      EQUIVALENCE (IBUF,RBUF)

      CHARACTER*(GNS__SZAGI) LAGI
      CHARACTER*(GNS__SZTYP) MATCWK, TEMPWK
      INTEGER I, ISEQNO, ISTAT, IWKTYP, J, LMATCW, LTEMPW, MAGITY,
     :        MATCH, NREC, NP, TAGITY
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
            CALL EMS_REP( 'GNS_IIAI_AGNNR1',
     :                    'AGI name not recognised', STATUS )
            GOTO 99
         ENDIF

*   Make sure GNS has started
         CALL GNS_1INITI(STATUS)

*   Decode the workstation type and sequence number from the AGI name
         ISTAT = 0
         CALL GNS_1INTIN(LAGI,I,IWKTYP,ISTAT)
         IF ( ISTAT .EQ. 0 ) THEN
            CALL GNS_1INTIN(LAGI,I,ISEQNO,ISTAT)
         ENDIF
         IF ( ISTAT .NE. 0 ) THEN
            STATUS = GNS__AGNNR
            CALL EMS_REP( 'GNS_IIAI_AGNNR2',
     :                    'AGI name not recognised', STATUS )
            GOTO 99
         ENDIF

*   Have to look sequentially through the IDIDEVICES file for this
*   workstation type
         MATCH = 0
         NREC = 1
  10     CONTINUE
         NREC = NREC + 1
         READ( UNIT=LUNIDI, REC=NREC, ERR=30 ) IBUF
         IF ( IBUF(1) .EQ. -2 ) GOTO 10

*   Have to read the workstation type before we get to the AGITYPE
         IF ( IBUF(1) .GT. 0 ) THEN
            LTEMPW = IBUF(1)
            NP = 1
            DO 20 J = 1, LTEMPW
               NP = NP + 1
               IF ( NP .GT. RECSIZ ) THEN
                  NREC = NREC + 1
                  READ( UNIT=LUNIDI, REC=NREC, ERR=100 ) IBUF
                  IF ( IBUF(1) .NE. -1 ) GOTO 110
                  NP = 2
               ENDIF
               TEMPWK(J:J) = CHAR(IBUF(NP))
  20        CONTINUE

*   Can now read the AGITYPE
            NP = NP + 1
            IF ( NP .GT. RECSIZ ) THEN
               NREC = NREC + 1
               READ( UNIT=LUNIDI, REC=NREC, ERR=100 ) IBUF
               IF ( IBUF(1) .NE. -1 ) GOTO 110
               NP = 2
            ENDIF
            TAGITY = IBUF(NP)
            IF ( TAGITY .EQ. IWKTYP ) THEN
               MATCH = MATCH + 1
               MATCWK = TEMPWK
               LMATCW = LTEMPW
               MAGITY = TAGITY
            ENDIF
         ENDIF
         GOTO 10

*   Have reached the end of the file so see if there has been any matches
  30     CONTINUE
         IF ( MATCH .LT. 1 ) THEN
            STATUS = GNS__AGNNR
            CALL EMS_REP( 'GNS_IIAI_AGNNR3',
     :                    'AGI name not recognised', STATUS )
            GOTO 99
         ENDIF
         IF ( MATCH .GT. 1 ) THEN
            STATUS = GNS__AMBNAM
            CALL EMS_REP( 'GNS_IIAI_AMBNAM',
     :                    'Ambiguous workstation name', STATUS )
            GOTO 99
         ENDIF

*   Now look through the common blocks for a name for this type
         MATCH = 0
         DO 40 J = 1, NUMNAI
            IF ( MATCWK(:LMATCW) .EQ. TYPESI(J) ) THEN
               MATCH = MATCH + 1
               NAME = NAMESI(J)

*   If the sequence number is non-zero and it matches then jump out of loop
               IF ( ISEQNO .NE. 0 ) THEN
                  IF ( ISEQNO .EQ. ISEQNI(J) ) GOTO 99
               ENDIF
            ENDIF
  40     CONTINUE

*   If the sequence number is zero and there is at least one match
*   then return a name
         IF ( ( ISEQNO .EQ. 0 ) .AND. ( MATCH .GE. 1 ) ) GOTO 99

*   If this point was reached then no match was found
         NAME = ' '
         STATUS = GNS__AGNNR
         CALL EMS_REP( 'GNS_IIAI_AGNNR4',
     :                 'AGI name not recognised', STATUS )
         GOTO 99
      ENDIF

 100  CONTINUE
      STATUS = GNS__DBRDE
      CALL EMS_REP( 'GNS_IIAI_DBRDE',
     :              'Error while reading the GNS database', STATUS )
      GOTO 99
 110  CONTINUE
      STATUS = GNS__DBFME
      CALL EMS_REP( 'GNS_IIAI_DBFME',
     :              'GNS database has an invalid format', STATUS )

  99  CONTINUE

      END

