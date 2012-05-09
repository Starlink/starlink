      SUBROUTINE KPG1_GETYP( PARAM, HDSTYP, STATUS )
*+
*  Name:
*     KPG1_GETYP

*  Purpose:
*     Obtains a valid HDS primitive data type via a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GETYP( PARAM, HDSTYP, STATUS )

*  Description:
*     This routine obtains and validates an HDS primitive data type,
*     including the _CHAR*n and LITERAL forms.  Unamibguous
*     abbreviations may be supplied.  The user is reprompted up to four
*     times if the value is not one of the allowed types.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter through which the value is to be obtained.  The
*        parameter data type should be LITERAL or _CHAR.
*     HDSTYP = CHARACTER * ( DAT__SZTYP ) (Given)
*        The unabbreviated HDS primitive type obtained.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Error reports are made and flushed inside the loop if the
*     _CHAR*n has an invalid "n" value.
*     -  Bad status is returned if no valid type has been obtained.
*     - _INT64 and _INTEGER require 5 characters for disambiguation.
*     This is a change from when only _INTEGER was available.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 May 23 (MJC):
*        Original version.
*     2012-05-09 (TIMJ):
*        Add _INT64 support. Note that this changes the match for
*        _INTEGER to use 5 characters.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      CHARACTER * ( DAT__SZTYP ) HDSTYP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_ABBRV
      LOGICAL CHR_ABBRV          ! Is the string an abbreviation?
      INTEGER CHR_LEN            ! Character length sans trailing blanks

*  Local Constants:
      INTEGER MXLOOP             ! Maximum number of attempts to obtain
                                 ! a valid value
      PARAMETER ( MXLOOP = 5 )   ! This is the same as SUBPAR

*  Local Variables:
      INTEGER CLIND              ! Location of the asterisk in _CHAR*n
      INTEGER ICLEN              ! Length of a character type
      LOGICAL NOTYPE             ! A valid type has yet to be found.
      INTEGER NLOOP              ! Number of attempts to obtain the type
      INTEGER TLEN               ! Number of characters in the type

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NOTYPE = .TRUE.
      NLOOP = 1
      DO WHILE ( STATUS .EQ. SAI__OK .AND. NOTYPE
     :           .AND. NLOOP .LE. MXLOOP )

*  Start a new error context.
         CALL ERR_MARK

*  Obtain the new type for the component. (An existing component will
*  be retyped).
         CALL PAR_GET0C( PARAM, HDSTYP, STATUS )
         CALL CHR_UCASE( HDSTYP )

*  Check the value is one of the allowed options.  A simple
*  PAR_CHOIC is inadequate because of the _CHAR*n type.

*  First check for LITERAL.  Equate this to a _CHAR*80 data type and
*  exit the loop.
         IF ( CHR_ABBRV( HDSTYP, 'LITERAL', 2 ) ) THEN
            HDSTYP = '_CHAR*80'
            NOTYPE = .FALSE.

*  For strings, expand abbreviations if there is no * in the type.
*  Exit the loop in this case.
         ELSE IF ( CHR_ABBRV( HDSTYP( :5 ), '_CHAR', 2 ) ) THEN
            CLIND = INDEX( HDSTYP, '*' )
            IF ( CLIND .EQ. 0 ) THEN
               HDSTYP = '_CHAR*80'
               NOTYPE = .FALSE.

*  If there is an asterisk, check that the length is a positive integer.
*  To do this obtain the length of the string.  Treat _CHAR* as _CHAR
*  and exit the loop.
            ELSE
               TLEN = CHR_LEN( HDSTYP )
               IF ( CLIND .EQ. TLEN ) THEN
                  HDSTYP = '_CHAR*80'
                  NOTYPE = .FALSE.

*  For text following the asterisk, try converting it to integer.
*  Report an error if it is invalid.
               ELSE
                  CALL ERR_MARK
                  CALL CHR_CTOI( HDSTYP( CLIND+1:TLEN ), ICLEN, STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_REP( 'KPG1_GETYP_NOTINT',
     :                 'Length of _CHAR*n type is not an integer',
     :                 STATUS )
                     CALL ERR_FLUSH( STATUS )

*  For text following the asterisk, check that it is positive.  If not
*  report the error.
                  ELSE IF ( ICLEN .LE. 0 ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'CL', ICLEN )
                     CALL ERR_REP( 'KPG1_GETYP_NOTPOS',
     :                 'Length of _CHAR*n type (^CL) is not positive.',
     :                 STATUS )
                     CALL ERR_FLUSH( STATUS )

*  The supplied string is satisfactory.  Exit the loop.
                  ELSE
                     NOTYPE = .FALSE.
                  END IF

*  Release the error context.
                  CALL ERR_RLSE
               END IF
            END IF

*  Test for _BYTE.  Exit the loop if there is a match.
         ELSE IF ( CHR_ABBRV( HDSTYP, '_BYTE', 2 ) ) THEN
            HDSTYP = '_BYTE'
            NOTYPE = .FALSE.

*  Test for _DOUBLE.  Exit the loop if there is a match.
         ELSE IF ( CHR_ABBRV( HDSTYP, '_DOUBLE', 2 ) ) THEN
            HDSTYP = '_DOUBLE'
            NOTYPE = .FALSE.

*  Test for _INTEGER.  Exit the loop if there is a match.
         ELSE IF ( CHR_ABBRV( HDSTYP, '_INTEGER', 5 ) ) THEN
            HDSTYP = '_INTEGER'
            NOTYPE = .FALSE.

*  Test for _INTEGER.  Exit the loop if there is a match.
         ELSE IF ( CHR_ABBRV( HDSTYP, '_INT64', 5 ) ) THEN
            HDSTYP = '_INT64'
            NOTYPE = .FALSE.

*  Test for _LOGICAL.  Exit the loop if there is a match.
         ELSE IF ( CHR_ABBRV( HDSTYP, '_LOGICAL', 2 ) ) THEN
            HDSTYP = '_LOGICAL'
            NOTYPE = .FALSE.

*  Test for _REAL.  Exit the loop if there is a match.
         ELSE IF ( CHR_ABBRV( HDSTYP, '_REAL', 2 ) ) THEN
            HDSTYP = '_REAL'
            NOTYPE = .FALSE.

*  Test for _UBYTE.  Exit the loop if there is a match.
         ELSE IF ( CHR_ABBRV( HDSTYP, '_UBYTE', 3 ) ) THEN
            HDSTYP = '_UBYTE'
            NOTYPE = .FALSE.

*  Test for _UWORD.  Exit the loop if there is a match.
         ELSE IF ( CHR_ABBRV( HDSTYP, '_UWORD', 3 ) ) THEN
            HDSTYP = '_UWORD'
            NOTYPE = .FALSE.

*  Test for _WORD.  Exit the loop if there is a match.
         ELSE IF ( CHR_ABBRV( HDSTYP, '_WORD', 2 ) ) THEN
            HDSTYP = '_WORD'
            NOTYPE = .FALSE.

*  Report an error message.
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'CT', HDSTYP )
            CALL ERR_REP( 'KPG1_GETYP_INVTYP',
     :        'An unknown type (^CT) was supplied.  Valid types are: '/
     :        /'_BYTE, _CHAR[*n], _DOUBLE, _INTEGER, LITERAL, _REAL, '/
     :        /'_UBYTE, _UWORD, _WORD.', STATUS )
            CALL ERR_FLUSH( STATUS )
         END IF

*  Increment the loop counter and cancel the parameter and try again.
         IF ( NOTYPE ) THEN
            NLOOP = NLOOP + 1
            CALL PAR_CANCL( PARAM, STATUS )
         END IF

      END DO

*  Report an error when the maximum number of loops has been exceeded.
      IF ( NLOOP .GT. MXLOOP ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N', MXLOOP )
         CALL ERR_REP( 'KPG1_GETYP_GIVEUP',
     :     'Repeated attempts to obtain an HDS primitive data type '/
     :     /'has failed after ^N attempts.', STATUS )
      END IF

      END
