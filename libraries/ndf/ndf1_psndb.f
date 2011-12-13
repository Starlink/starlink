      SUBROUTINE NDF1_PSNDB( STR, DEF, AXIS, IWCS, WCSSEC, VALUE, FRAME,
     :                       ISDEF, STATUS )
*+
*  Name:
*     NDF1_PSNDB

*  Purpose:
*     Parse an NDF dimension bound.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PSNDB( STR, DEF, AXIS, IWCS, WCSSEC, VALUE, FRAME,
*                      ISDEF, STATUS )

*  Description:
*     The routine parses a string representing an upper or lower
*     dimension bound of an NDF section. If the string is blank, then a
*     default value is returned. Leading and trailing spaces are
*     ignored.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String to be parsed.
*     DEF = DOUBLE PRECISION (Given)
*        Default value to be returned if the string is blank.
*     AXIS = INTEGER (Given)
*        The index of the axis number to which STR relates. If WCSSEC
*        is .FALSE., then the AXIS value is the index of a pixel axis.
*        Otherwise, it is the index of a WCS axis.
*     IWCS = INTEGER (Given)
*        An AST pointer to the NDF's WCS FrameSet.
*     WCSSEC = LOGICAL (Given)
*        If .TRUE., then the section specifier uses "WCS syntax". Otherwise,
*        it uses the old pixel/axis syntax. In WCS syntax, the supplied
*        STR string should contain a specification for the bounds on each
*        WCS axis, supplied in the order of the WCS axes. Each bound
*        specification must be given using WCS axis values. The number of
*        bounds specifications must equal the number of WCS axes. If WCSSEC
*        is .FALSE., the supplied STR string should contain a specification
*        for the bounds on each pixel axis, supplied in the order of the
*        pixel axes. Each bound specification must be given using either
*        pixel indices (integers), or WCS values (non-integers).
*     VALUE = DOUBLE PRECISION (Returned)
*        Dimension bound value.
*     FRAME = LOGICAL (Returned)
*        0 ==> VALUE is to be interpreted as a WCS or axis coordinate
*        value, 1 ==> it is a pixel index, 2 ==> it is a FRACTION value.
*     ISDEF = LOGICAL (Returned)
*        .TRUE. ==> the VALUE value is a default value and was not
*        specified in the supplied string. .FALSE. ==> VALUE was
*        specified explicitly in the supplied string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S Berry (JACH, UCLan)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1991 (RFWS):
*        Original version.
*     21-MAY-2007 (DSB):
*        Add support for sections given in terms of WCS coords.
*     4-AUG-2009 (DSB):
*        Logical ISPIX arguments changed to integer FRAME, and
*        support include for bounds specified as FRACTION values.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'AST_PAR'          ! AST_ constants and functions

*  Arguments Given:
      CHARACTER * ( * ) STR
      DOUBLE PRECISION DEF
      INTEGER AXIS
      INTEGER IWCS
      LOGICAL WCSSEC

*  Arguments Returned:
      DOUBLE PRECISION VALUE
      INTEGER FRAME
      LOGICAL ISDEF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATTR*10          ! AST attribute name
      INTEGER F                  ! Position of first non-blank character
      INTEGER IAT                ! Current string length
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCUSED             ! Number of characters read from STR
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the string.
      CALL CHR_FANDL( STR, F, L )

*  If the input string is blank, then return the default value.
      ISDEF = ( F .GT. L )
      IF ( ISDEF ) THEN
         VALUE = DEF

         IF( WCSSEC ) THEN
            FRAME = 0
         ELSE
            FRAME = 1
         END IF

      ELSE

*  If we are using the old pixel/axis syntax, see if the supplied value is
*  an integer, in which case it is assumed to be a pixel index.
         FRAME = 0
         IF( .NOT. WCSSEC ) THEN

*  If the last character is a percent sign, the value is a FRACTION.
*  Remove the %, convert the remaining percentage value to a fraction and
*  flag the value as a FRACTION value.
            IF( STR( L : L ) .EQ. '%' ) THEN
               FRAME = 2
               CALL CHR_CTOD( STR( F : L - 1 ), VALUE, STATUS )
               IF( STATUS .NE. SAI__OK ) THEN
                  STATUS = NDF__BNDIN
                  CALL MSG_SETC( 'BADBOUND', STR( F : L ) )
                  CALL ERR_REP( 'NDF1_PSNDB_SYN', 'Invalid FRACTION ' //
     :                          'bound ''^BADBOUND'' specified; bad '//
     :                          'syntax.', STATUS )
               ELSE
                  VALUE = 0.01D0*VALUE
               END IF

*  Otherwise, first see if it is numerical. STATUS will be set but no error
*  will be reported if not.
            ELSE
               CALL CHR_CTOD( STR( F : L ), VALUE, STATUS )

*  If it is not numerical, clear the status value.
               IF( STATUS .NE. SAI__OK ) THEN
                  STATUS = SAI__OK

*  If it is numerical, see if it is integer.
               ELSE IF( ( INDEX( STR( F : L ), '.' ) .EQ. 0 ) .AND.
     :                  ( INDEX( STR( F : L ), 'E' ) .EQ. 0 ) .AND.
     :                  ( INDEX( STR( F : L ), 'e' ) .EQ. 0 ) .AND.
     :                  ( INDEX( STR( F : L ), 'D' ) .EQ. 0 ) .AND.
     :                  ( INDEX( STR( F : L ), 'd' ) .EQ. 0 ) ) THEN
                  FRAME = 1
               END IF
            END IF
         END IF

*  If the value is not a pixel index, and is not a FRACTION value, we
*  interpret the string using the AST_UNFORMAT method of the supplied
*  FrameSet.
         IF( FRAME .EQ. 0 ) THEN

*  Now read the value from the formatted text.
            NCUSED = AST_UNFORMAT( IWCS, AXIS, STR, VALUE, STATUS )

*  Report an error if there was any spurious text in the string.
            IF( NCUSED .LT. LEN( STR ) .AND. STATUS .EQ. SAI__OK ) THEN
               ATTR = 'Label('
               IAT = 6
               CALL CHR_PUTI( AXIS, ATTR, IAT )
               CALL CHR_APPND( ')', ATTR, IAT )
               CALL MSG_SETC( 'L', AST_GETC( IWCS, ATTR( : IAT ),
     :                                       STATUS ) )
               CALL MSG_SETC( 'BADBOUND', STR( F : L ) )

               STATUS = NDF__BNDIN
               CALL ERR_REP( 'NDF1_PSNDB_SYN', 'Invalid ^L bound ' //
     :                       '''^BADBOUND'' specified; bad syntax.',
     :                       STATUS )
            END IF

         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PSNDB', STATUS )

      END
