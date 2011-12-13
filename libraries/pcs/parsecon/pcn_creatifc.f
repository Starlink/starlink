      SUBROUTINE PARSECON_CREATIFC
     : ( FNAME, LENGTH, FSTAT, LUCON, STATUS )
*+
*  Name:
*     PARSECON_CREATIFC

*  Purpose:
*     Create compiled interface file and return channel.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_CREATIFC ( FNAME, LENGTH, FSTAT, LUCON, STATUS )

*  Description:
*     Create a file to contain the compiled interface module and return
*     its logical unit number.

*  Arguments:
*     FNAME=CHARACTER*(*) (given)
*        name of compiled interface file to be created
*     LENGTH=INTEGER (given)
*        length of FNAME
*     FSTAT=CHARACTER*(*) (given)
*        the STATUS, 'NEW' or 'UNKNOWN' to be used to open the file
*     LUCON=INTEGER (returned)
*        logical unit number of interface file
*     STATUS=INTEGER

*  Algorithm:
*     The name of the application program is given, and the name of the
*     corresponding compiled interface file created from it.
*     A free Fortran unit number is obtained and the file is created.

*  Copyright:
*     Copyright (C) 1984, 1991, 1992 Science & Engineering Research Council.
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
*     B.D.Kelly (REVAD::BDK)
*     A.J.Chipperfield (RLVAD::AJC)
*     {enter_new_authors_here}

*  History:
*     10.10.1984:  Original (REVAD::BDK)
*     25.10.1984:  create file in default directory rather than
*        in ADAM_IFL (REVAD::BDK)
*     17.06.1991:  Get logical unit number portably (RLVAD::AJC)
*     10.03.1992:  Change to import the complete filename
*        and the NEW or UNKNOWN status (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PARSECON_ERR'


*  Arguments Given:
      CHARACTER*(*) FNAME               ! name of file to be created

      INTEGER LENGTH                    ! length of FNAME

      CHARACTER*(*) FSTAT               ! OPEN STATUS keyword


*  Arguments Returned:
      INTEGER LUCON                     ! FORTRAN unit number


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER ISTAT                    ! Local status
      LOGICAL OPEN                     ! Whether unit number is in use

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Obtain an unused unit number
      DO LUCON = 1,99
*     For numbers 1 to 99 inquire if number is in use
         INQUIRE (UNIT=LUCON, OPENED=OPEN )
         IF ( .NOT. OPEN ) THEN
*        If number is not in use, use it to open the file
            ISTAT = 0
            OPEN ( UNIT = LUCON, FILE = FNAME(1:LENGTH),
     :        STATUS = FSTAT, FORM = 'UNFORMATTED', IOSTAT = ISTAT )

*        If failed, report reason
            IF ( ISTAT .NE. 0 ) THEN
               STATUS = PARSE__IFCOPN
               CALL EMS_SETC( 'FILE', FNAME(1:LENGTH) )
               CALL EMS_REP( 'PCN_CREATIFC1',
     :          'Failed to create interface module ^FILE', STATUS )
               CALL EMS_FIOER( 'FIOSTAT', ISTAT )
               CALL EMS_REP( 'PCN_CREATIFC2', '^FIOSTAT', STATUS )

*        Otherwise, if it might not be NEW,  ensure file is at start
            ELSEIF ( FSTAT .NE. 'NEW' ) THEN
               REWIND ( UNIT = LUCON, IOSTAT = ISTAT )
*           If failed, report reason
               IF ( ISTAT .NE. 0 ) THEN
                  STATUS = PARSE__IFCOPN
                  CALL EMS_SETC( 'FILE', FNAME(1:LENGTH) )
                  CALL EMS_REP( 'PCN_CREATIFC3',
     :            'Failed to REWIND ^FILE', STATUS )
                  CALL EMS_FIOER( 'FIOSTAT', ISTAT )
                  CALL EMS_REP( 'PCN_CREATIFC4', '^FIOSTAT', STATUS )
               ENDIF

            ENDIF

*        and exit
            GOTO 100

         ENDIF

      ENDDO

*  No unit number available - report
      STATUS = PARSE__IFCOPN
      CALL EMS_SETC( 'FILE', FNAME(1:LENGTH) )
      CALL EMS_REP( 'PCN_CREATIFC3',
     : 'Failed to create interface module ^FILE', STATUS )
      CALL EMS_REP( 'PCN_CREATIFC4',
     : 'No Fortran unit numbers available', STATUS )

100   CONTINUE

      END
