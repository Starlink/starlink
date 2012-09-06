#include "config.h"
      SUBROUTINE gns_1INITG (STATUS)
*+
*  Name:
*     gns_1INITG

*  Purpose:
*     Prepares the GKS name database for access

*  Language:
*     Starlink Fortran 77

*  Description:
*     If the names database has already been loaded then the routine
*     exits immediately with a success status. Otherwise the database
*     file is opened, read into common, and closed.

*  Arguments:
*     STATUS = INTEGER (Given & Returned)
*         Status

*  Copyright:
*     Copyright (C) 1989-1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 2004 Central Laboratory of the Research
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     Councils. All Rights Reserved.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: P W Draper (Starlink)
*     {enter_new_authors_here}

*  History:
*     4-APR-1989 (DLT):
*        Modified.
*     17-MAY-1990 (NE):
*        Added sequence numbers.
*     9-JUL-1990 (NE):
*        Added error reporting.
*     15-JAN-1991 (DLT):
*        Move file names to include file + fix LUN logic.
*     16-MAR-1992 (NE):
*        Initialise the connection id arrays.
*     25-JUL-1995 (DLT):
*        Search for data files.
*     15-APR-2004 (TIMJ):
*        Use autoconf for RECL.
*     29-JUL-2004 (PWD):
*        Increase file and device names to 256 chars.
*     19-JUN-2006 (PWD):
*        Change error messages so that it's easier to say which file is
*        corrupt.
*     02-JUL-2007 (PWD):
*        Include database name in error reports.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Implicit Inputs:
*     The contents of the GKS names database file

*  Implicit Outputs:
*     none

*  Libraries Used:
*     EMS

*-
      IMPLICIT NONE
      INCLUDE 'GNS_PAR'
      INCLUDE 'gns.cmn'
      INCLUDE 'GNS_ERR'
      INTEGER STATUS

      LOGICAL OPEN
      INTEGER NDELS, LREC, IREC(RECSIZ), IOERR
      PARAMETER (LREC = 134, NDELS = 6)
      INTEGER I, J, K, IDEL(NDELS), L, M
      LOGICAL LUNOPN, NMD
      CHARACTER*132 REC
      CHARACTER*6 NODE, LOCNOD
      CHARACTER*256 FNAME, TNAME
      CHARACTER*256 NAMFNG, DEVFNG
      SAVE FNAME, OPEN

*   Size of each record in platform specific units
      INTEGER RECLEN

*   Number of bytes per record unit
      INTEGER BYTEPRU

*   Use autoconf without trying to do variable substitution
#if FC_RECL_UNIT == 1
       PARAMETER ( BYTEPRU = 1 )
#elif FC_RECL_UNIT == 2
       PARAMETER ( BYTEPRU = 2 )
#elif FC_RECL_UNIT == 4
       PARAMETER ( BYTEPRU = 4 )
#else
 error "Impossible FC_RECL_UNIT"
#endif

      DATA IDEL/NDELS*LREC/

*   Calculate record length in local units
*   We assume we are using variables that are 4 bytes long
      RECLEN = RECSIZ * 4 / BYTEPRU

*   Initialise name so we know when it has just been opened.
      NAMFNG = ' '

      IF (STATUS.EQ.0) THEN

*     If the database is not already open
         IF (.NOT.OPEN) THEN

*        Initialise the connection identifier arrays
            NUMCOG = 0
            DO M = 1, MAXIDS
               ICOIDG( M ) = -1
               IWKTYG( M ) = -1
               CDENAG( M ) = ' '
            ENDDO

*        Check that the logical unit number is not already in use
            LUNGKS = LUNGNS
   10       CONTINUE
            INQUIRE(UNIT = LUNGKS, OPENED=LUNOPN)
            IF (LUNOPN) THEN
               LUNGKS = LUNGKS + 1
               GO TO 10
            END IF

*        Open the GNS names file
            CALL GNS_1FNDF('GKS', 'NAMES', NAMFNG)
            OPEN (UNIT=LUNGKS, FILE=NAMFNG, STATUS='OLD',
#if HAVE_FC_OPEN_ACTIONREAD
     :           ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :           READONLY,
#endif
     :           ERR=101)

*        Get our own node name
            CALL gns_1HOSTN( LOCNOD, L, STATUS)

            I = 1
   20       CONTINUE
            IF (I.GT.MAXNAI+1) GO TO 60
            READ (UNIT=LUNGKS, END=70, FMT='(A)', ERR=120) REC
            IF (I.LE.MAXNAI) THEN

*           Locate the field delimiters. IDEL(J) points to the character
*           after the Jth delimiter; if there is no Jth delimiter
*           IDEL(J) is two greater than the length of REC
               K = 1
               DO 30 J = 1,NDELS
                  IDEL(J) = INDEX (REC(K:),'/') + K
                  IF (IDEL(J).EQ.K) THEN
                     IDEL(J) = LREC
                     GO TO 40
                  END IF
                  K = IDEL(J)
   30          CONTINUE
   40          CONTINUE

*        If there is a "node" field then check it against our own node
*        name
               IF (IDEL(5)-IDEL(4).GT.1) THEN
                  NODE = REC(IDEL(4):IDEL(5)-2)
                  IF (NODE.NE.'      '.AND.NODE.NE.LOCNOD) GO TO 50
               END IF

*        Copy the data from the input record to the common block
               IF (IDEL(1).GT.2) THEN
                  NAMES(I) = REC(:IDEL(1)-2)
               ELSE
                  STATUS = GNS__DBINV
                  CALL EMS_REP( 'GNS_1INITG_DBN1',
     :                 'GNS names database file has invalid format (1)',
     :                          STATUS )
                  GO TO 70
               END IF

               IF (IDEL(2)-IDEL(1).GT.1) THEN
                  READ (UNIT=REC(IDEL(1):IDEL(2)-2), FMT='(I99)',
     :               IOSTAT=IOERR) ITYPES(I)
                  IF (IOERR.NE.0) THEN
                      STATUS = GNS__DBINV
                      CALL EMS_REP( 'GNS_1INITG_DBN2',
     :                'GNS names database file has invalid format (2)',
     :                          STATUS )
                      GO TO 70
                  END IF
               ELSE
                  STATUS = GNS__DBINV
                  CALL EMS_REP( 'GNS_1INITG_DBN2',
     :                 'GNS names database file has invalid format (3)',
     :                          STATUS )
                  GO TO 70
               END IF

               IF (IDEL(3)-IDEL(2).GT.1) THEN
                  VMSNAM(I) = REC(IDEL(2):IDEL(3)-2)
               ELSE
                  VMSNAM(I) = ' '
               END IF

               IF (IDEL(4)-IDEL(3).GT.1) THEN
                  WSDESC(I) = REC(IDEL(3):IDEL(4)-2)
                  LDESC(I) = IDEL(4) - IDEL(3) - 1
               ELSE
                  LDESC(I) = 0
               END IF

               IF (IDEL(6)-IDEL(5).GT.1) THEN
                  READ (UNIT=REC(IDEL(5):IDEL(6)-2), FMT='(I99)',
     :               IOSTAT=IOERR) ISEQNG(I)
                  IF (IOERR.NE.0) ISEQNG(I) = 0
               ELSE
                  ISEQNG(I) = 0
               END IF
            END IF
            I = I + 1
   50       CONTINUE
            GO TO 20

*        If we get to here then there are more records in the file than
*        space in the common block
   60       CONTINUE
            STATUS = GNS__DBOVF
            CALL EMS_REP( 'GNS_1INITG_DBOV',
     :                   'Too many workstation names have been defined',
     :                    STATUS )

   70       CONTINUE
            CLOSE (UNIT=LUNGKS)
            NUMNAM = I - 1
            OPEN = .TRUE.
         END IF

         IF (STATUS.EQ.0) THEN

*        Check the state of the logical unit
            INQUIRE (UNIT=LUNGKS, OPENED=LUNOPN, NAMED=NMD, NAME=TNAME)

            IF (.NOT.LUNOPN .OR. .NOT.NMD .OR. TNAME.NE.FNAME) THEN

*           if the logical unit was in use then we have to search for a
*           free unit
   80          CONTINUE
               IF (LUNOPN) THEN
                  LUNGKS = LUNGKS + 1
                  INQUIRE (UNIT=LUNGKS, OPENED=LUNOPN)
                  GO TO 80
               END IF

*           Open the workstation description file
               CALL GNS_1FNDF('GKS', 'DEVICES', DEVFNG)
               OPEN (UNIT=LUNGKS, FILE=DEVFNG, STATUS='OLD',
     :              RECL=RECLEN,
#if HAVE_FC_OPEN_ACTIONREAD
     :              ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :              READONLY,
#endif
     :              ACCESS='DIRECT',
     :               FORM='UNFORMATTED', ERR=100)

*           Save the file name returned by INQUIRE
               INQUIRE (UNIT=LUNGKS, NAME=FNAME)

*           Read the first record to check the version number and find the
*           constants used in the hashing algorithm
               READ (UNIT=LUNGKS, REC=1, ERR=110) IREC
               IF (IREC(1).GT.1) THEN
                  STATUS = GNS__VNSUP
                  CALL EMS_REP( 'GNS_1INITG_VNSP',
     :                         'Description file version not supported',
     :                          STATUS )
               ELSE
                  GHASH1 = IREC(2)
                  GHASH2 = IREC(3)
               END IF

            END IF
         END IF
      END IF
      GO TO 999

  100 CONTINUE
      STATUS = GNS__DBOPE
      CALL EMS_REP( 'GNS_1INITG_DBOP',
     :              'Unable to open GNS devices database file', STATUS )
      GO TO 999
  101 CONTINUE
      STATUS = GNS__DBOPE
      CALL EMS_REP( 'GNS_1INITG_DBOP',
     :              'Unable to open GNS names database file', STATUS )
      GO TO 999
  120 CONTINUE
      STATUS = GNS__DBINV
      CALL EMS_REP( 'GNS_1INITG_DBN3',
     :              'GNS names database file has invalid format (0)',
     :              STATUS )
      CLOSE (UNIT=LUNGKS)
      GO TO 999
  110 CONTINUE
      STATUS = GNS__DBINV
      CALL EMS_REP( 'GNS_1INITG_DBN3',
     :              'GNS devices database file has invalid format (0)',
     :              STATUS )
      CLOSE (UNIT=LUNGKS)
  999 CONTINUE

      IF ( STATUS .NE. 0 .AND. NAMFNG .NE. ' ' ) THEN
         CALL EMS_SETC( 'FILENAME', NAMFNG )
         CALL EMS_REP( 'GNS_INITG_NAMESFILE',
     :                 'GNS names database: ^FILENAME', STATUS )
      END IF
      END
