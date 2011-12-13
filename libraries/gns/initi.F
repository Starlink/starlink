#include "config.h"
      SUBROUTINE gns_1INITI (STATUS)
*+
*  Name:
*     gns_1INITI

*  Purpose:
*     Prepares the IDI name database for access

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
*     Copyright (C) 1989-1991 Science & Engineering Research Council.
*     Copyright (C) 1995, 2004 Central Laboratory of the Research
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
*     18-JUL-1989 (DLT):
*        Modified.
*     17-MAY-1990 (NE):
*        Added sequence number.
*     11-JUN-1990 (NE):
*        Access IDIDEVICES file.
*     9-JUL-1990 (NE):
*        Added error reporting.
*     16-JAN-1991 (DLT):
*        Move file names to include file + use GNS routine to get node name + fix LUN logic.
*     2-DEC-1991 (NE):
*        Allow blank fields for the node name.
*     25-JUL-1995 (DLT):
*        Search for data files.
*     15-APR-2004 (TIMJ):
*        Use autoconf for RECL.
*     29-JUL-2004 (PWD):
*        Increase file and device names to 256 chars.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Implicit Inputs:
*     The contents of the IDI names database file

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
      INTEGER NDELS, LREC, IREC(RECSIZ)
      PARAMETER (LREC = 134, NDELS = 6)
      INTEGER I, J, K, IDEL(NDELS), L, IOERR
      LOGICAL LUNOPN, NMD
      CHARACTER*132 REC
      CHARACTER*6 NODE, LOCNOD
      CHARACTER*256 FNAME, TNAME
      CHARACTER*256 NAMFNI, DEVFNI
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

      IF (STATUS.EQ.0) THEN

*   If the database has not already been loaded
         IF (.NOT.OPEN) THEN

*   Check that the logical unit number is not in use before using it
            LUNIDI = LUNGNS
   10       CONTINUE
            INQUIRE(UNIT=LUNIDI,OPENED=LUNOPN)
            IF (LUNOPN) THEN
               LUNIDI = LUNIDI +1
               GO TO 10
            END IF

*   Open the database file
            CALL GNS_1FNDF('IDI','NAMES',NAMFNI)
            OPEN (UNIT=LUNIDI, FILE=NAMFNI, STATUS='OLD',
#if HAVE_FC_OPEN_READONLY
     :           READONLY,
#endif
     :           ERR=100)

*   Get our own node name
            CALL gns_1HOSTN( LOCNOD, L, STATUS)

            I = 1
   20       CONTINUE
            IF (I.GT.MAXNAI+1) GO TO 60
            READ (UNIT=LUNIDI, END=70, FMT='(A)', ERR=110) REC
            IF (I.LE.MAXNAI) THEN

*   Locate the field delimiters. IDEL(J) points to the character
*   after the Jth delimiter; if there is no Jth delimiter
*   IDEL(J) is two greater than the length of REC
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

*   If there is a "node" field then check it against our own node name
               IF (IDEL(5)-IDEL(4).GT.1) THEN
                  NODE = REC(IDEL(4):IDEL(5)-2)
                  IF (NODE.NE.'      '.AND.NODE.NE.LOCNOD) GO TO 50
               END IF

*   Copy the data from the input record to the common block
               IF (IDEL(1).GT.2) THEN
                  NAMESI(I) = REC(:IDEL(1)-2)
               ELSE
                  STATUS = GNS__DBINV
                  CALL EMS_REP( 'GNS_1INITI_DBN1',
     :                          'GNS database file has invalid format',
     :                          STATUS )
                  GO TO 70
               END IF

               IF (IDEL(2)-IDEL(1).GT.1) THEN
                  TYPESI(I) = REC(IDEL(1):IDEL(2)-2)
               ELSE
                  STATUS = GNS__DBINV
                  CALL EMS_REP( 'GNS_1INITI_DBN2',
     :                          'GNS database file has invalid format',
     :                          STATUS )
                  GO TO 70
               END IF

               IF (IDEL(3)-IDEL(2).GT.1) THEN
                  VMSNAI(I) = REC(IDEL(2):IDEL(3)-2)
               ELSE
                  STATUS = GNS__DBINV
                  CALL EMS_REP( 'GNS_1INITI_DBN3',
     :                          'GNS database file has invalid format',
     :                          STATUS )
                  GO TO 70
               END IF

               IF (IDEL(4)-IDEL(3).GT.1) THEN
                  WSDESI(I) = REC(IDEL(3):IDEL(4)-2)
                  LDESCI(I) = IDEL(4) - IDEL(3) - 1
               ELSE
                  LDESCI(I) = 0
               END IF

               IF (IDEL(6)-IDEL(5).GT.1) THEN
                  READ( UNIT=REC(IDEL(5):IDEL(6)-2), FMT='(I99)',
     :               IOSTAT=IOERR ) ISEQNI( I )
                  IF (IOERR.NE.0) ISEQNI(I) = 0
               ELSE
                  ISEQNI(I) = 0
               END IF
            END IF
            I = I + 1
   50       CONTINUE
            GO TO 20

*   If we get to here then there are more records in the file than
*   space in the common block
   60       CONTINUE
            STATUS = GNS__DBOVF
            CALL EMS_REP( 'GNS_1INITI_DBOV',
     :                   'Too many workstation names have been defined',
     :                    STATUS )

   70       CONTINUE
            CLOSE (UNIT=LUNIDI)
            NUMNAI = I - 1
            OPEN = .TRUE.
         END IF

         IF ( STATUS .EQ. 0 ) THEN

*   Check the state of the logical unit
            INQUIRE( UNIT=LUNIDI, OPENED=LUNOPN, NAMED=NMD, NAME=TNAME )

            IF ( .NOT.LUNOPN .OR. .NOT.NMD .OR. TNAME.NE.FNAME ) THEN

*   If the logical unit was in use then search for a free unit
  80           CONTINUE
               IF ( LUNOPN ) THEN
                  LUNIDI = LUNIDI + 1
                  INQUIRE( UNIT=LUNIDI, OPENED=LUNOPN )
                  GOTO 80
               ENDIF

*   Open the workstation description file
               CALL GNS_1FNDF('IDI','DEVICES',DEVFNI)
               OPEN( UNIT=LUNIDI, FILE=DEVFNI, STATUS='OLD',
     :              RECL=RECLEN,
#if HAVE_FC_OPEN_READONLY
     :              READONLY,
#endif
     :              ACCESS='DIRECT', FORM='UNFORMATTED', ERR=100 )

*   Save the file name returned by INQUIRE
               INQUIRE( UNIT=LUNIDI, NAME=FNAME )

*   Read the first record to check the version number and find the
*   constants used in the hashing algorithm
               READ( UNIT=LUNIDI, REC=1, ERR=110 ) IREC
               IF ( IREC(1) .GT. 1 ) THEN
                  STATUS = GNS__VNSUP
                  CALL EMS_REP( 'GNS_1INITI_VNSP',
     :                         'Description file version not supported',
     :                          STATUS )
               ELSE
                  IHASH1 = IREC(2)
                  IHASH2 = IREC(3)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      GO TO 999

  100 CONTINUE
      STATUS = GNS__DBOPE
      CALL EMS_REP( 'GNS_1INITI_DBOP',
     :              'Unable to open GNS database file', STATUS )
      GO TO 999
  110 CONTINUE
      STATUS = GNS__DBINV
      CALL EMS_REP( 'GNS_1INITI_DBN4',
     :              'GNS database file has invalid format', STATUS )
  999 CONTINUE
      END
