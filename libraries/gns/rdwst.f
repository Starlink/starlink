      SUBROUTINE GNS_1RDWST(ITYPE, STATUS)
*+
*  Name:
*     GNS_1RDWST

*  Purpose:
*     Ensure that the GNS comon block is filled in with the data for
*     the specified workstation type.

*  Language:
*     {routine_language}

*  Invocation:
*     CALL GNS_1RDWST(ITYPE, STATUS)

*  Description:
*     The workstation type of the current contents of the common block
*     are compared with the requested type and the common block filled
*     by reading from the description file if necessary.

*  Arguments:
*     ITYPE =INTEGER (Given)
*        GKS workstation type
*     STATUS  (INTEGER (Given & Returned)
*        Inherited status

*  Copyright:
*     Copyright (C) 1989-1990 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     DLT: D L Terrett (Starlink)
*     NE: Nick Eaton (Starlink)
*     {enter_new_authors_here}

*  History:
*     11-MAY-1989 (DLT):
*        Original.
*      2-APR-1990 (NE):
*        Added OPEN keyword
*     18-MAY-1990 (NE):
*        Added AGI types
*      9-JUL-1990 (NE):
*        Added error reporting
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Libraries Used:
*     EMS

*-
      IMPLICIT NONE
      INTEGER ITYPE, STATUS

      INCLUDE 'GNS_PAR'
      INCLUDE 'gns.cmn'
      INCLUDE 'GNS_ERR'

*   Workstation currently in common block
      INTEGER ICURWK

      INTEGER NREC, I, NP

*   Buffer for record from file
      INTEGER IBUF(RECSIZ)
      REAL RBUF(RECSIZ)
      EQUIVALENCE (IBUF,RBUF)

      DATA ICURWK /-1/

      IF (STATUS.EQ.0) THEN

*   If the current workstation does not match the one requested
         IF (ICURWK.NE.ITYPE) THEN

*     Make sure that the database is open
            CALL GNS_1INITG(STATUS)
            IF (STATUS.NE.0) GO TO 9999

*     Hash the workstation type to get a record number and search the
*     file from that point until we get an empty record or find the
*     workstation we want.
            NREC = MOD(ITYPE*GHASH1,GHASH2)
   10       CONTINUE
            NREC = NREC + 1
            READ (UNIT=LUNGKS ,REC=NREC, ERR=100) IBUF
            IF (IBUF(1).EQ.-2) THEN

*        We read an empty record so the workstation we want isn't in the
*        file so fill the common block with defaults.
               CLASS = 0
               SCALE = 0.0
               OUTPUT = 0
               CLEAR = 0
               LERTXT = 0
               LDEFNA = 0
               IOPEN = 0
               AGITYG = 0
            ELSE

*           If the type doesn't match then read another record
               IF (IBUF(1).NE.ITYPE) GO TO 10

*           We have got the right record so copy the data into the
*           common block.
               CLASS = IBUF(2)
               SCALE = RBUF(3)
               OUTPUT = IBUF(4)
               CLEAR = IBUF(5)
               LERTXT = IBUF(6)
               NP = 6
               DO 20 I=1,LERTXT
                  NP = NP + 1

*              If we have reached the end of the record then read the
*              next one
                  IF (NP.GT.RECSIZ) THEN
                     NREC = NREC + 1
                     READ (UNIT=LUNGKS ,REC=NREC, ERR=100) IBUF

*                 it must be a continuation record
                     IF (IBUF(1).NE.-1) GO TO 110

                     NP = 2
                  END IF

                  ERTXT(I:I) = CHAR(IBUF(NP))
   20          CONTINUE

*           Repeat for the next character item
               NP = NP + 1
               IF (NP.GT.RECSIZ) THEN
                  NREC = NREC + 1
                  READ (UNIT=LUNGKS ,REC=NREC, ERR=100) IBUF
                  IF (IBUF(1).NE.-1) GO TO 110
                  NP = 2
               END IF
               LDEFNA = IBUF(NP)
               DO 30 I=1,LDEFNA
                  NP = NP + 1
                  IF (NP.GT.RECSIZ) THEN
                     NREC = NREC + 1
                     READ (UNIT=LUNGKS ,REC=NREC, ERR=100) IBUF
                     IF (IBUF(1).NE.-1) GO TO 110
                     NP = 2
                  END IF
                  DEFNAM(I:I) = CHAR(IBUF(NP))
   30          CONTINUE

*           Repeat for the next item ( IOPEN )
               NP = NP + 1
               IF (NP.GT.RECSIZ) THEN
                  NREC = NREC + 1
                  READ (UNIT=LUNGKS ,REC=NREC, ERR=100) IBUF
                  IF (IBUF(1).NE.-1) GO TO 110
                  NP = 2
               END IF
               IOPEN = IBUF(NP)

*           Repeat for the next item ( AGITYG )
               NP = NP + 1
               IF (NP.GT.RECSIZ) THEN
                  NREC = NREC + 1
                  READ (UNIT=LUNGKS ,REC=NREC, ERR=100) IBUF
                  IF (IBUF(1).NE.-1) GO TO 110
                  NP = 2
               END IF
               AGITYG = IBUF(NP)

            END IF
         END IF
      END IF
      GO TO 9999

  100 CONTINUE
      STATUS = GNS__DBRDE
      CALL EMS_REP( 'GNS_1RDWST_DBRE',
     :              'Error while reading the GNS database', STATUS )
      GO TO 9999
  110 CONTINUE
      STATUS = GNS__DBFME
      CALL EMS_REP( 'GNS_1RDWST_DBFE',
     :              'GNS database has invalid format', STATUS )
 9999 CONTINUE
      END

