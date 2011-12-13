      SUBROUTINE GNS_TNDG ( NAME, DEVICE, IWKTYP, ICONID, STATUS )

*+
*  Name:
*     GNS_TNDG

*  Purpose:
*     Translate name and device to GKS specification

*  Invocation:
*     CALL GNS_TNDG( NAME, DEVICE, IWKTYP, ICONID, STATUS )

*  Description:
*     The workstation name and physical device specification are
*     translated to a GKS workstation type and connection identifier
*     and, if necessary, a logical name created to map the connection
*     identifier onto the specified physical device or file.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        Workstation name
*     DEVICE = CHARACTER*(*) (Given)
*        Physical device name
*     IWKTYP = INTEGER (Returned)
*        GKS workstation type
*     ICONID = INTEGER (Returned)
*        Connection identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Side Effects:
*     A hard link to the file fort.n may be created.
*
*     The GNS database may be opened.
*
*     A GWM window with an overlay may be created.

*  Deficiencies:
*     If the device is identical ( workstation type and device name )
*     to a previous one then the same connection identifier is recovered
*     from the common block arrays. This, however, assumes that the
*     connection id. hasn't been hijacked in some way in between calls.

*  Copyright:
*     Copyright (C) 1990, 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     DLT: D.L. Terrett (STARLINK)
*     NE: Nick Eaton (Durham University)

*  History:
*     18-APR-1990 (DLT):
*        Original version.
*     12-DEC-1991 (NE):
*        Set environment variable GKS_xxxx for xwindows
*     24-FEB-1992  Correct setting of GKS_xxxx for xwindows:
*                  check workstation type not name.
*     16-MAR-1992 (NE):
*        Recover connection id from common block arrays
*     31-JUL-1992 (NE):
*        Check name for GKS_ before logical translation
*     21-AUG-1992 (NE):
*        Create X-window with overlay if '+' in name
*      1-SEP-1992 (NE):
*        Updated prologue.
*     14-JUL-1995 (DLT):
*        Don't create recursive GKS_xxxx_xxxx environment variables.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'GKS_PAR'
      INCLUDE 'GNS_ERR'
      INCLUDE 'GNS_PAR'
      INCLUDE 'gns.cmn'
      INCLUDE 'chars.par'

*  Arguments Given:
      CHARACTER*(*) NAME
      CHARACTER*(*) DEVICE

*  Arguments Returned:
      INTEGER IWKTYP
      INTEGER ICONID

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER*64 CNAME, CNAME1, CDEV, CDEV1, LOGNAM, TNAME
      CHARACTER CM*10
      INTEGER LNAME, LNAME1, LDEV, ISTATE, IERR, NOP, IWKID
      INTEGER I, J, K, L, M, LLOG, JFLAG, MATCH, ICAT
      LOGICAL OPEN, GOTONE, PLUS

*  Local Data:
*  The connection id is saved between calls to reduce the amount of
*  searching for unused numbers and initialized to greater than the
*  range of logical unit numbers reserved by GKS-UK.
      INTEGER ICON
      DATA ICON/25/
      SAVE ICON
*.

      IF (STATUS.EQ.0) THEN

*     Copy input device specification
         CNAME = NAME

*     See if there is a '+' special character in the name
         CALL GNS_1SPECH( '+', ' ', CNAME, PLUS )

*     No device found yet
         LDEV = 0

*     Default connection id is 0
         ICONID = 0

   10    CONTINUE

*     Split into name and device components (this also strips leading
*     and trailing blanks).
         CALL gns_1DECSP(CNAME,CNAME1,LNAME1,CDEV1,L)
         CNAME = CNAME1
         LNAME = LNAME1

*     If there was a device name and we haven't got one already, save
*     it.
         IF (LDEV.EQ.0 .AND. L.GT.0) THEN
            CDEV = CDEV1
            LDEV = L
         END IF

*     If the name doesn't begin with GKS_ then see if it is a logical name
         IF (CNAME(:4).NE.'GKS_') THEN

*     Try logical name translation
            CALL GETENV( CNAME(:LNAME), TNAME )

*     If it succeded then go back for another go
            IF (INDEX( TNAME, ' ').NE.1) THEN
               CNAME = TNAME
               LNAME = INDEX( TNAME, ' ') -1
               GOTO 10
            END IF
         ENDIF

*     If still no device component then subsitute the DEVICE argument
         IF (LDEV.EQ.0) CALL gns_1TRIM(DEVICE,CDEV,LDEV)

*     Open GNS database
         CALL gns_1INITG(STATUS)
         IF (STATUS.EQ.0) THEN

*        Convert name to GKS workstation type
            MATCH = 0
            DO 30 I = 1,NUMNAM
               IF (CNAME(:LNAME).EQ.NAMES(I)(:LNAME)) THEN
                  IWKTYP = ITYPES(I)
                  IF (LDEV.EQ.0) THEN
                     IF (VMSNAM(I).NE.' ') THEN
                        CALL gns_1TRIM(VMSNAM(I),CDEV,LDEV)
                     END IF
                  END IF
                  MATCH = MATCH + 1

*              If the names match exactly then abandon any futher
*              checking and ignore any matching abbreviations.
                  IF (CNAME(:LNAME).EQ.NAMES(I)) GO TO 40
               END IF
   30       CONTINUE

*        If only one match was found then continue with device name
*        handling
            IF (MATCH.EQ.1) THEN
               GO TO 40

*        More than one match is an error
            ELSE IF (MATCH.GT.1) THEN
               STATUS = gns__AMBNAM
               CALL EMS_REP( 'GNS_TNDG_AMBNAM',
     :                       'Ambiguous workstation name', STATUS )
               GO TO 9000
            END IF
         END IF

*     No match was found (maybe because we couldn't open the database
*     but maybe it is an explicit type,connection id specification so
*     try decoding it.
         IF (CNAME(:4).EQ.'GKS_') THEN
            I = 5
         ELSE
            I = 1
         END IF

         CALL gns_1INTIN(CNAME,I,IWKTYP,JFLAG)

         IF (JFLAG.EQ.0) THEN

*       Try decoding connection id field
            CALL gns_1INTIN(CNAME,I,ICONID,JFLAG)

*       A null connection id field (JFLAG=1) is OK
            IF (JFLAG.EQ.0 .OR. JFLAG.EQ.1) THEN
               STATUS = 0
            ELSE

*       Unable to decode the connection id (preserve the existing
*       status if has already been set)
               IF (STATUS.EQ.0) THEN
                  STATUS = GNS__NAMNR
                  CALL EMS_SETC( 'NAME', CNAME1(:LNAME1) )
                  CALL EMS_REP( 'GNS_TNDG_NMNR1',
     :                      'Device name ^NAME not recognised', STATUS )
               ENDIF
               GO TO 9000
            END IF
         ELSE

*     Unable to decode the name (preserve the existing status if has
*     already been set)
            IF (STATUS.EQ.0) THEN
               STATUS = GNS__NAMNR
               CALL EMS_SETC( 'NAME', CNAME1(:LNAME1) )
               CALL EMS_REP( 'GNS_TNDG_NMNR2',
     :                      'Device name ^NAME not recognised', STATUS )
            ENDIF
            GO TO 9000
         END IF

   40    CONTINUE

*   Make sure that the common block contains data for this type
         CALL GNS_1RDWST(IWKTYP, STATUS)
         IF (STATUS.NE.0) GO TO 9000

*     If
*        an explicit device name has been supplied
*     or
*        an explicit connection identifier was not supplied
*     then
*        select a suitable connection id (ie. one that isn't in use or
*        has a file connected to it).
*     else
*        If
*           the device is a terminal and the connection id is zero
*        then
*           use connection id 5.
*
         IF (LDEV.NE.0 .OR. (OUTPUT.EQ.FILE .AND. ICONID.EQ.0)) THEN

*     We must have a device name; get the default name from the database
            IF (LDEV.EQ.0) THEN
               CDEV = DEFNAM
               LDEV = LDEFNA
            END IF

*     See if this device already has an associated connection if.
            GOTONE = .FALSE.
            DO M = 1, NUMCOG
               IF ( ( IWKTYP .EQ. IWKTYG( M ) ) .AND.
     :              ( CDEV(:LDEV) .EQ. CDENAG( M ) ) ) THEN
                  ICON = ICOIDG( M )
                  GOTONE = .TRUE.
               ENDIF
            ENDDO

*     If a match hasn't been found then find a fresh connectino id.
            IF ( .NOT. GOTONE ) THEN

*     Find the catagory of the workstation (pretend that it is interactive
*     if GKS is not open or the type doesn't exist.
               CALL GQOPS(ISTATE)
               IF (ISTATE.NE.GGKCL) THEN
                  CALL GQWKCA(IWKTYP, IERR, ICAT)
                  IF (IERR.NE.0) ICAT = GOUTIN
               ELSE
                  ICAT = GOUTIN
               END IF

*     Are there any workstations open?
   50          CONTINUE
               IF (ISTATE.EQ.GGKCL .OR. ISTATE.EQ.GGKOP) THEN

*        No - so any connection id will do
                  ICON = ICON + 1
               ELSE

*        Yes - so we have to check all open workstations.
                  CALL GQOPWK(1,IERR,NOP,IWKID)
   60             CONTINUE
                  ICON   = ICON + 1
                  DO 70 I = 1, NOP
                     CALL GQOPWK(I,IERR,NOP,IWKID)
                     CALL GQWKC(IWKID,IERR,J,K)
                     IF (ICON.EQ.J) GO TO 60
   70             CONTINUE
               END IF

*        Check that this connection id isn't already in use
               DO M = 1, NUMCOG
                  IF ( ICON .EQ. ICOIDG( M ) ) GOTO 50
               ENDDO

*        Now check that there isn't a file open on this logical unit
               INQUIRE ( UNIT = ICON, OPENED = OPEN)
               IF (OPEN) GO TO 50

*        On an interactive workstation the next logical unit is used as
*        well so we must check it too.
               IF ( ICAT.EQ.GOUTIN ) THEN
                  INQUIRE ( UNIT = ICON, OPENED = OPEN)
                  IF (OPEN) GO TO 50
               END IF

*        Save the new connection id. in the common arrays
               NUMCOG = NUMCOG + 1
               IF ( NUMCOG .GT. MAXIDS ) THEN
                  STATUS = GNS__CIOVF
                  CALL EMS_REP( 'GNS_TNDG_CIOVF',
     :                        'No more connection identifiers', STATUS )
                  GOTO 9000
               ENDIF
               ICOIDG( NUMCOG ) = ICON
               IWKTYG( NUMCOG ) = IWKTYP
               CDENAG( NUMCOG ) = CDEV(:LDEV)
            ENDIF

*        It's ok to use this id
            ICONID = ICON

*        Construct the appropriate environment string
            WRITE(LOGNAM,'(''GCON'',I2.2)') ICONID
            LLOG = 6

*        Create the environment variable
            CALL PSX_PUTENV(LOGNAM(:LLOG), CDEV(:LDEV), STATUS)

*        Construct the GKS_xxxx environment string for xwindow devices
            WRITE( CM, '(SP,I10)' ) IWKTYP
            K = INDEX( CM, '+' )
            IF ( ( K + 4 .LE. 10 ) .AND.
     :           ( CM( K+1 : K+3 ) .EQ. '380' ) ) THEN
               WRITE(LOGNAM,'(''GKS_'',I4.4)') IWKTYP
               LLOG = 8

*        Create the environment variable
               IF (LOGNAM(:LLOG).NE. CDEV(:LDEV)) THEN
                  CALL PSX_PUTENV(LOGNAM(:LLOG), CDEV(:LDEV), STATUS)
               ENDIF
            ENDIF

         ELSE IF (CLASS.EQ.TERMIN .AND. ICONID.EQ.0 ) THEN
            ICONID = 5

         END IF

*     If the device is an X-window and there has been a special character
         IF ( PLUS .AND. ( IWKTYP / 10 .EQ. 380 ) ) THEN

*     Create a GWM window with an overlay using the device name
            IF ( LDEV .GT. 0 ) CALL GNS_1GWMOV( CDEV, LDEV, STATUS )
         ENDIF

      ENDIF

 9000 CONTINUE
      END

