      SUBROUTINE NDF1_PSNDF( STR, LBND, UBND, AXIS, IWCS, WCSSEC,
     :                       VALUE1, VALUE2, FRAME1, FRAME2, ISBND,
     :                       ISDEF1, ISDEF2, STATUS )
*+
*  Name:
*     NDF1_PSNDF

*  Purpose:
*     Parse an NDF dimension bound field.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PSNDF( STR, LBND, UBND, AXIS, IWCS, WCSSEC, VALUE1,
*                      VALUE2, FRAME1, FRAME2, ISBND, ISDEF1, ISDEF2,
*                      STATUS )

*  Description:
*     The routine parses a dimension bound field for an NDF to
*     determine two values which specify the bounds for a dimension
*     when selecting an NDF section. The lower and upper bounds may be
*     separated in the normal way by a colon or semi-colon (e.g. '10:20'),
*     or by '~' (e.g. '31~10'). The latter indicates that the bounds should
*     be centred on the first value and have a dimension size equal to the
*     second value. Suitable default values are returned if either or
*     both halves of the field are omitted (e.g. '100:', ':100', ':',
*     '~15', '33~' etc.). If no field separator is present, then the
*     upper bound is set to equal the lower bound (unless the string is
*     blank, which is equivalent to ':'). If the values of bounds are
*     supplied using integer format, then they are interpreted as pixel
*     indices. Otherwise, they are interpreted as value in the current
*     Frame of the supplied WCS FrameSet.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        The string to be parsed.
*     LBND = DOUBLE PRECISION (Given)
*        Default lower axis bound. This should be a WCS axis value if WCSSEC
*        is .TRUE., and a pixel coordinate otherwise.
*     UBND = DOUBLE PRECISION (Given)
*        Default upper axis bound. This should be a WCS axis value if WCSSEC
*        is .TRUE., and a pixel coordinate otherwise.
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
*     VALUE1 = DOUBLE PRECISION (Returned)
*        First value specifying the dimension bounds.
*     VALUE2 = DOUBLE PRECISION (Returned)
*        Second value specifying the dimension bounds.
*     FRAME1 = INTEGER (Returned)
*        0 ==> VALUE1 is to be interpreted as a WCS or axis coordinate
*        value, 1 ==> it is a pixel index, 2 ==> it is a FRACTION value.
*     FRAME2 = INTEGER (Returned)
*        0 ==> VALUE2 is to be interpreted as a WCS or axis coordinate
*        value, 1 ==> it is a pixel index, 2 ==> it is a FRACTION value.
*     ISBND = LOGICAL (Returned)
*        Whether VALUE1 and VALUE2 specify the lower and upper bounds
*        directly (i.e. .TRUE. ==> a ':' separator was given or
*        implied, whereas .FALSE. ==> a '~' separator was given).
*     ISDEF1 = LOGICAL (Returned)
*        .TRUE. ==> the VALUE1 value is a default value and was not
*        specified in the supplied string. .FALSE. ==> VALUE1 was
*        specified explicitly in the supplied string.
*     ISDEF2 = LOGICAL (Returned)
*        .TRUE. ==> the VALUE1 value is a default value and was not
*        specified in the supplied string. .FALSE. ==> VALUE1 was
*        specified explicitly in the supplied string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The values obtained by parsing the string are not constrained
*     to lie within the NDF bounds. The lower bound returned may also
*     exceed the upper bound.

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
*     25-FEB-1991 (RFWS):
*        Fixed error in substring limits.
*     14-MAR-1991 (RFWS):
*        Added check that dimension extents are at least 1 pixel.
*     21-MAY-2007 (DSB):
*        Add support for sections given in terms of WCS coords.
*     6-JUN-2007 (DSB):
*        Allow zero increments on a WCS axis.
*     4-AUG-2009 (DSB):
*        Logical ISPIX1/2 arguments changed to integer FRAME1/2, and
*        support include for bounds specified as FRACTION values.
*     {enter_further_changes_here}

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
      DOUBLE PRECISION LBND
      DOUBLE PRECISION UBND
      INTEGER AXIS
      INTEGER IWCS
      LOGICAL WCSSEC

*  Arguments Returned:
      DOUBLE PRECISION VALUE1
      DOUBLE PRECISION VALUE2
      INTEGER FRAME1
      INTEGER FRAME2
      LOGICAL ISBND
      LOGICAL ISDEF1
      LOGICAL ISDEF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATTR*10          ! AST attribute name
      DOUBLE PRECISION DEF1      ! Default first value
      DOUBLE PRECISION DEF2      ! Default second value
      INTEGER F                  ! Position of first non-blank character
      INTEGER IAT                ! Current string length
      INTEGER ISEP               ! Character position of separator
      INTEGER L                  ! Position of last non-blank character
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the string.
      CALL CHR_FANDL( STR, F, L )

*  If the string is blank, then return the default field values (a ':'
*  separator being implied).
      IF ( F .GT. L ) THEN
         VALUE1 = LBND
         VALUE2 = UBND

         IF( WCSSEC ) THEN
            FRAME1 = 0
            FRAME2 = 0
         ELSE
            FRAME1 = 1
            FRAME2 = 1
         END IF

         ISBND = .TRUE.
         ISDEF1 = .TRUE.
         ISDEF2 = .TRUE.

*  Otherwise, locate the separator between the two values. We use ";" as
*  an alternative to ":" because ":" is used as a separator within formatted
*  angle and time values.
      ELSE
         ISEP = INDEX( STR, '~' )
         IF( ISEP .EQ. 0 ) ISEP = INDEX( STR, ';' )
         IF( ISEP .EQ. 0 ) ISEP = INDEX( STR, ':' )

*  Determine if the separator is a ':' either explicitly or by
*  implication.
         ISBND = .TRUE.
         IF ( ISEP .NE. 0 ) THEN
            ISBND = ( STR( ISEP : ISEP ) .EQ. ':' ) .OR.
     :              ( STR( ISEP : ISEP ) .EQ. ';' )
         ELSE
            ISEP = LEN( STR ) + 1
         END IF

*  Set up suitable defaults for each value, depending on which separator
*  was found.
         IF ( ISBND ) THEN
            DEF1 = LBND
            DEF2 = UBND

         ELSE IF( .NOT. WCSSEC ) THEN
            DEF1 = ( LBND + UBND ) / 2.0D0
            DEF2 = UBND - LBND + 1.0D0

         ELSE
            DEF2 = 0.5D0*AST_AXDISTANCE( IWCS, AXIS, LBND, UBND,
     :                                   STATUS )
            DEF1 = AST_AXOFFSET( IWCS, AXIS, LBND, DEF2, STATUS )
         END IF

*  If the separator appears at the start, then use the default first
*  value.
         IF ( ISEP .LE. F ) THEN
            VALUE1 = DEF1

            IF( WCSSEC ) THEN
               FRAME1 = 0
            ELSE
               FRAME1 = 1
            END IF

            ISDEF1 = .TRUE.

*  Otherwise, parse the string in front of the separator to obtain the
*  first bound, supplying the appropriate default.
         ELSE
            CALL NDF1_PSNDB( STR( F : ISEP - 1 ), DEF1, AXIS, IWCS,
     :                       WCSSEC, VALUE1, FRAME1, ISDEF1, STATUS )
         END IF

*  If there is no separator present, then the second value equals the
*  first value.
         IF ( ISEP .GT. L ) THEN
            VALUE2 = VALUE1
            FRAME2 = FRAME1
            ISDEF2 = ISDEF1

*  Otherwise, if the separator appears at the end of the string, then
*  use the default second value.
         ELSE IF ( ISEP .EQ. L ) THEN
            VALUE2 = DEF2

            IF( WCSSEC ) THEN
               FRAME2 = 0
            ELSE
               FRAME2 = 1
            END IF

            ISDEF2 = .TRUE.

*  Otherwise, parse the string which follows the separator to determine
*  the second value.
         ELSE
            CALL NDF1_PSNDB( STR( ISEP + 1 : L ), DEF2, AXIS, IWCS,
     :                       WCSSEC, VALUE2, FRAME2, ISDEF2, STATUS )
         END IF
      END IF

*  If no error has occurred and the second value obtained specifies the
*  extent of the dimension (rather than its upper bound), then check
*  that this extent is not negative.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. ISBND ) THEN

*  If the extent is in pixels, then the nearest integer value must be
*  positive.
            IF ( FRAME2 .EQ. 1 .AND. NINT( VALUE2 ) .LE. 0 ) THEN
               STATUS = NDF__BNDIN
               CALL ERR_REP( 'NDF1_PSNDF_PEXT',
     :                       'Invalid dimension extent specified; ' //
     :                       'a positive number of pixels is required.',
     :                       STATUS )

*  If the extent is in WCS coords, then the value itself must be
*  non-negative.
            ELSE IF ( WCSSEC .AND. VALUE2 .LT. 0 ) THEN

               ATTR = 'Label('
               IAT = 6
               CALL CHR_PUTI( AXIS, ATTR, IAT )
               CALL CHR_APPND( ')', ATTR, IAT )
               CALL MSG_SETC( 'L', AST_GETC( IWCS, ATTR( : IAT ),
     :                                       STATUS ) )

               STATUS = NDF__BNDIN
               CALL ERR_REP( 'NDF1_PSNDF_WEXT',
     :                       'Invalid dimension extent specified; ' //
     :                       'a positive increment in ^L is required.',
     :                       STATUS )

*  If the extent is in axis units, then we can also allow a value of
*  zero (which translates into an extent of one pixel).
            ELSE IF ( ( FRAME2 .EQ. 0 ) .AND.
     :                ( VALUE2 .LT. 0.0D0 ) ) THEN
               STATUS = NDF__BNDIN
               CALL ERR_REP( 'NDF1_PSNDF_AEXT',
     :                       'Invalid dimension extent specified; ' //
     :                       'value must not be negative.',
     :                       STATUS )
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PSNDF', STATUS )

      END
