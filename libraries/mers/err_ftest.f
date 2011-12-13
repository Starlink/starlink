      PROGRAM ERR_TEST
*+
*  Name:
*     ERR_TEST

*  Purpose:
*     Test the installation of the ERR/MSG libraries.

*  Language:
*     Starlink Fortran 77

*  Description:
*     Test calls to the ERR/MSG libraries.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1993 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1993 (PCTR):
*        Original version.
*     20-JUL-2008 (TIMJ):
*        Add features for C port.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'
      INCLUDE 'ERR_PAR'

*  Status:
      INTEGER STATUS             ! Global status
      CHARACTER *(30)  OPSTR
      INTEGER OPLEN
      INTEGER LEVEL
      CHARACTER *(10) PARAM
      CHARACTER *(ERR__SZMSG) OPSTRE
      INTEGER PARLEN

*.

*  Initialise status.
      STATUS = SAI__OK

*  Call MSG_OUT.
      CALL MSG_BELL( STATUS )
      CALL MSG_BLANK( STATUS )
      CALL MSG_SETC( 'TOK', '(SETC)')
      CALL MSG_SETD( 'DTK', 0.0D0 )
      CALL MSG_SETR( 'RTK', 0.0 )
      CALL MSG_SETI( 'ITK', 0 )
      CALL MSG_SETL( 'LTK', .FALSE. )
      CALL ERR_SYSER( 'ERR', 32 )
      CALL ERR_FIOER( 'IOERR', 10 )
      CALL MSG_OUT( ' ', 'MSG is installed and working. - ^TOK' /
     :/ ' ^DTK - ^RTK - ^ITK - ^LTK - ^ERR ^IOERR',
     :     STATUS )
      CALL MSG_SYNC( STATUS )

      CALL MSG_SETC( 'D', 'Test')
      CALL MSG_SETC( 'D', ' Consecutive')
      CALL MSG_SETC( 'D', ' Calls')
      CALL MSG_SETC( 'D', ' To')
      CALL MSG_SETC( 'D', ' MSG_SETC')
      CALL MSG_SETI( 'D', 5)
      CALL MSG_OUT( ' ', 'Combo - ^D', STATUS )

      CALL MSG_SETC('D', 'A')
      CALL MSG_SETC('D', 'B')
      CALL MSG_SETC('D', 'C')
      CALL MSG_SETC('D', 'D')
      CALL MSG_SETC('D', 'E')
      CALL MSG_OUT( ' ','Should be ABCDE - ^D', STATUS)
      CALL ERR_BEGIN( STATUS )
      CALL ERR_MARK
      CALL MSG_SETC('D', 'A')
      CALL MSG_SETC('D', 'B')
      CALL MSG_SETC('D', 'C')
      CALL MSG_SETC('D', 'D')
      CALL MSG_SETC('D', 'E')
      CALL MSG_OUT( ' ','Should be ABCDE - ^D', STATUS)
      CALL ERR_RLSE
      CALL MSG_SETC('D', 'A')
      CALL MSG_SETC('D', 'B')
      CALL MSG_SETC('D', 'C')
      CALL MSG_SETC('D', 'D')
      CALL MSG_SETC('D', 'E')
      CALL MSG_OUT( ' ','Should be ABCDE - ^D', STATUS)
      CALL MSG_SETC('D', 'A')
      CALL MSG_SETC('D', 'B')
      CALL MSG_SETC('D', 'C')
      CALL MSG_SETC('D', 'D')
      CALL MSG_SETC('D', 'E')
      CALL MSG_OUT( ' ','Should be ABCDE - ^D', STATUS)
      CALL ERR_END( STATUS )
      CALL MSG_SETC('D', 'A')
      CALL MSG_SETC('D', 'B')
      CALL MSG_SETC('D', 'C')
      CALL MSG_SETC('D', 'D')
      CALL MSG_SETC('D', 'E')
      CALL MSG_OUT( ' ','Should be ABCDE - ^D', STATUS)

      CALL MSG_IFSET( MSG__VERB, STATUS )
      CALL MSG_IFLEV( LEVEL, ' ', STATUS )
      CALL MSG_OUTIF( MSG__DEBUG, ' ', 'Message should not be seen',
     :     STATUS )

      CALL MSG_OUTIF( MSG__NORM, ' ', 'Message should appear',
     :     STATUS )

*  Control width
      CALL MSG_TUNE( 'SZOUT', 20, STATUS )
      CALL MSG_SETC( 'X', 'ABCDEFGHIJKLMN OPQRSTUVWXYZ')
      CALL MSG_OUTIF( MSG__QUIET, ' ', 'Line wrap: ^X ^X ^X ^X',
     :     STATUS)

*  Loading
      CALL MSG_SETC( 'XX', 'LOAD WORKING' );
      CALL MSG_LOAD( ' ','Load test: ^XX', OPSTR, OPLEN, STATUS )
      PRINT *, 'Output from LOAD:',OPSTR

*  ERR_OUT
      STATUS = SAI__ERROR
      CALL ERR_OUT( ' ', 'Calling ERR_OUT', STATUS )

*  Call ERR_REP and ERR_FLUSH.
      CALL ERR_BEGIN( STATUS )
      CALL ERR_TUNE( 'SZOUT', 40, STATUS )
      STATUS = SAI__ERROR
      CALL ERR_MARK
      CALL ERR_FACER( 'ERR', STATUS )
      CALL ERR_REP( ' ', 'ERR is installed and working. - ^ERR '/
     :/ 'and this is a very long message that should be wrapped '/
     :/ 'hopefully to 4 lines',
     :     STATUS )
      CALL ERR_REP( ' ', 'Second line of error report',
     :     STATUS )
      CALL ERR_REP( ' ', 'Third line of error report',
     :     STATUS )
      CALL ERR_FLBEL( STATUS )
      CALL ERR_RLSE
      CALL ERR_TUNE( 'SZOUT', 79, STATUS )

      STATUS = SAI__ERROR
      CALL ERR_REP( ' ',
     :     'Flushed error with ^^X escape ^^Y char and a % or 2',
     :     STATUS)
      CALL ERR_FLUSH(STATUS)

      STATUS = SAI__ERROR
      CALL ERR_REP( ' ','Will annul this', STATUS )
      CALL ERR_ANNUL( STATUS )
      CALL ERR_END( STATUS )

      CALL ERR_LEVEL( LEVEL )

*  ERR_LOAD
      CALL ERR_BEGIN( STATUS )
      STATUS = SAI__ERROR
      CALL ERR_REP( ' ','Text should be retrieved with ERR_LOAD',
     :     STATUS )
      CALL ERR_LOAD( PARAM, PARLEN, OPSTRE, OPLEN, STATUS )
      PRINT *,'ERR_LOAD TEXT: ',OPSTRE
      CALL ERR_END( STATUS )
      END
