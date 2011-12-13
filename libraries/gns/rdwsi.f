      SUBROUTINE GNS_1RDWSI(WKTYPE, STATUS)
*+
*  Name:
*     GNS_1RDWSI

*  Purpose:
*     Ensure that the GNS comon block is filled in with the data for
*     the specified workstation type.

*  Language:
*     {routine_language}

*  Invocation:
*     CALL GNS_1RDWSI(WKTYPE, STATUS)

*  Description:
*     The workstation type of the current contents of the common block
*     are compared with the requested type and the common block filled
*     by reading from the description file if necessary.

*  Arguments:
*     WKTYPE = CHARACTER*(GNS__SZTYP) (Given)
*        IDI workstation type
*     STATUS = INTEGER (Given & Returned)
*        Inherited status

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council. All
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
*     NE: Nick Eaton (Starlink)
*     {enter_new_authors_here}

*  History:
*     11-JUN-1990 (NE):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Libraries Used:
*     EMS

*-
      IMPLICIT NONE
      CHARACTER*(*) WKTYPE
      INTEGER STATUS

      INCLUDE 'GNS_PAR'
      INCLUDE 'gns.cmn'
      INCLUDE 'GNS_ERR'

*   Workstation currently in common block
      CHARACTER*(GNS__SZTYP) CURWK
      INTEGER LCURWK

      INTEGER NREC, ITYPE, J, LTEMPW, NP
      CHARACTER*(GNS__SZTYP) TEMPWK

*   Buffer for record from file
      INTEGER IBUF(RECSIZ)
      REAL RBUF(RECSIZ)
      EQUIVALENCE (IBUF,RBUF)

      DATA LCURWK /0/
      SAVE CURWK, LCURWK

      IF (STATUS.EQ.0) THEN

*   If the current workstation does not match the one requested
         IF (CURWK(:LCURWK).NE.WKTYPE) THEN

*   Make sure that the database is open
            CALL GNS_1INITI(STATUS)
            IF (STATUS.NE.0) GO TO 9999

*   Hash the first two characters of the workstation name to get
*   a record number and search the file from that point until we
*   get an empty record or find the workstation we want.
            ITYPE = 0
            IF ( WKTYPE(1:1) .NE. ' ' ) THEN
               ITYPE = ITYPE + ICHAR(WKTYPE(1:1))
               IF ( WKTYPE(2:2) .NE. ' ' ) THEN
                  ITYPE = ITYPE + ICHAR(WKTYPE(2:2))
               ENDIF
            ENDIF
            NREC = MOD(ITYPE*IHASH1,IHASH2)
   10       CONTINUE
            NREC = NREC + 1
            READ (UNIT=LUNIDI ,REC=NREC, ERR=100) IBUF
            IF (IBUF(1).EQ.-2) THEN

*   We read an empty record so the workstation we want isn't in the
*   file so fill the common block with defaults.
               AGITYI = 0
            ELSE

*   Recreate the workstation type from the character values
*   and see if it matches the requested workstation type
               IF (IBUF(1).GT.0) THEN
                  LTEMPW = IBUF(1)
                  NP = 1
                  DO 20 J = 1, LTEMPW
                     NP = NP + 1
                     IF (NP.GT.RECSIZ) THEN
                        NREC = NREC + 1
                        READ (UNIT=LUNGKS ,REC=NREC, ERR=100) IBUF
                        IF (IBUF(1).NE.-1) GO TO 110
                        NP = 2
                     END IF
                     TEMPWK(J:J) = CHAR(IBUF(NP))
  20              CONTINUE

*   If this is the correct workstation then remember it
                  IF (TEMPWK(:LTEMPW).EQ.WKTYPE) THEN
                     CURWK = TEMPWK
                     LCURWK = LTEMPW

*   Otherwise look for another workstation entry
                  ELSE
                     GOTO 10
                  ENDIF

               ELSE
                  GOTO 10
               ENDIF

*   We have got the right record so copy the data into the common block.
*   Repeat for the next item ( AGITYI )
               NP = NP + 1
               IF (NP.GT.RECSIZ) THEN
                  NREC = NREC + 1
                  READ (UNIT=LUNGKS ,REC=NREC, ERR=100) IBUF
                  IF (IBUF(1).NE.-1) GO TO 110
                  NP = 2
               END IF
               AGITYI = IBUF(NP)

            END IF
         END IF
      END IF
      GO TO 9999

  100 CONTINUE
      STATUS = GNS__DBRDE
      CALL EMS_REP( 'GNS_1RDWSI_DBRE',
     :              'Error while reading the GNS database', STATUS )
      GO TO 9999
  110 CONTINUE
      STATUS = GNS__DBFME
      CALL EMS_REP( 'GNS_1RDWSI_DBFE',
     :              'GNS datbase has invalid format', STATUS )
 9999 CONTINUE
      END

