      SUBROUTINE NDF1_PSNDF( STR, LBND, UBND, VALUE1, VALUE2, ISPIX1,
     :                       ISPIX2, ISBND, STATUS )
*+
*  Name:
*     NDF1_PSNDF

*  Purpose:
*     Parse an NDF dimension bound field.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PSNDF( STR, LBND, UBND, VALUE1, VALUE2, ISPIX1, ISPIX2,
*                      ISBND, STATUS )

*  Description:
*     The routine parses a dimension bound field for an NDF to
*     determine two values which specify the bounds for a dimension
*     when selecting an NDF section. The lower and upper bounds may be
*     separated in the normal way by a colon (e.g. '10:20'), or by '~'
*     (e.g. '31~10'). The latter indicates that the bounds should be
*     centred on the first value and have a dimension size equal to the
*     second value. Suitable default values are returned if either or
*     both halves of the field are omitted (e.g. '100:', ':100', ':',
*     '~15', '33~' etc.). If no field separator is present, then the
*     upper bound is set to equal the lower bound (unless the string is
*     blank, which is equivalent to ':'). If the values of bounds are
*     supplied using integer format, then they are interpreted as pixel
*     indices. If floating-point format is used (including
*     double-precision) then they are interpreted as axis coordinate
*     system values.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        The string to be parsed.
*     LBND = INTEGER (Given)
*        The NDF's lower pixel-index bound, to be used as the default
*        lower bound.
*     UBND = INTEGER (Given)
*        The NDF's upper pixel-index bound, to be used as the default
*        upper bound.
*     VALUE1 = DOUBLE PRECISION (Returned)
*        First value specifying the dimension bounds.
*     VALUE2 = DOUBLE PRECISION (Returned)
*        Second value specifying the dimension bounds.
*     ISPIX1 = LOGICAL (Returned)
*        Whether VALUE1 is a pixel index (as opposed to an axis value).
*     ISPIX2 = LOGICAL (Returned)
*        Whether VALUE2 is a pixel index (as opposed to an axis value).
*     ISBND = LOGICAL (Returned)
*        Whether VALUE1 and VALUE2 specify the lower and upper bounds
*        directly (i.e. .TRUE. ==> a ':' separator was given or
*        implied, whereas .FALSE. ==> a '~' separator was given).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The values obtained by parsing the string are not constrained
*     to lie within the NDF bounds. The lower bound returned may also
*     exceed the upper bound.

*  Algorithm:
*     -  Find the first and last non-blank characters in the string.
*     -  If the string is blank, then return the default field values
*     (a ':' separator being implied).
*     -  Otherwise, locate the separator between the two values.
*     -  Determine if the separator is a ':' either explicitly or by
*     implication.
*     -  Set up suitable defaults for each value, depending on which
*     separator was found.
*     -  If the separator appears at the start, then use the default
*     first value.
*     -  Otherwise, parse the string in front of the separator to
*     obtain the first bound, supplying the appropriate default.
*     -  If there is no separator present, then the second value equals
*     the first value.
*     -  Otherwise, if the separator appears at the end of the string,
*     then use the default second value.
*     -  Otherwise, parse the string which follows the separator to
*     determine the second value.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1991 (RFWS):
*        Original version.
*     25-FEB-1991 (RFWS):
*        Fixed error in substring limits.
*     14-MAR-1991 (RFWS):
*        Added check that dimension extents are at least 1 pixel.
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

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER LBND
      INTEGER UBND

*  Arguments Returned:
      DOUBLE PRECISION VALUE1
      DOUBLE PRECISION VALUE2
      LOGICAL ISPIX1
      LOGICAL ISPIX2
      LOGICAL ISBND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DEF1      ! Default first value
      DOUBLE PRECISION DEF2      ! Default second value
      INTEGER F                  ! Position of first non-blank character
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
         VALUE1 = DBLE( LBND )
         VALUE2 = DBLE( UBND )
         ISPIX1 = .TRUE.
         ISPIX2 = .TRUE.
         ISBND = .TRUE.

*  Otherwise, locate the separator between the two values.
      ELSE
         ISEP = INDEX( STR, ':' )
         IF ( ISEP .EQ. 0 ) ISEP = INDEX( STR, '~' )

*  Determine if the separator is a ':' either explicitly or by
*  implication.
         ISBND = .TRUE.
         IF ( ISEP .NE. 0 ) THEN
            ISBND = ( STR( ISEP : ISEP ) .EQ. ':' )
         ELSE
            ISEP = LEN( STR ) + 1
         END IF

*  Set up suitable defaults for each value, depending on which separator
*  was found.
         IF ( ISBND ) THEN
            DEF1 = DBLE( LBND )
            DEF2 = DBLE( UBND )
         ELSE
            DEF1 = DBLE( ( LBND + UBND ) / 2 )
            DEF2 = DBLE( UBND - LBND + 1 )
         END IF

*  If the separator appears at the start, then use the default first
*  value.
         IF ( ISEP .LE. F ) THEN
            VALUE1 = DEF1
            ISPIX1 = .TRUE.

*  Otherwise, parse the string in front of the separator to obtain the
*  first bound, supplying the appropriate default.
         ELSE
            CALL NDF1_PSNDB( STR( F : ISEP - 1 ), DEF1, VALUE1, ISPIX1,
     :                       STATUS )
         END IF

*  If there is no separator present, then the second value equals the
*  first value.
         IF ( ISEP .GT. L ) THEN
            VALUE2 = VALUE1
            ISPIX2 = ISPIX1

*  Otherwise, if the separator appears at the end of the string, then
*  use the default second value.
         ELSE IF ( ISEP .EQ. L ) THEN
            VALUE2 = DEF2
            ISPIX2 = .TRUE.

*  Otherwise, parse the string which follows the separator to determine
*  the second value.
         ELSE
            CALL NDF1_PSNDB( STR( ISEP + 1 : L ), DEF2, VALUE2, ISPIX2,
     :                       STATUS )
         END IF
      END IF

*  If no error has occurred and the second value obtained specifies the
*  extent of the dimension (rather than its upper bound), then check
*  that this extent is not negative.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. ISBND ) THEN

*  If the extent is in pixels, then it must be positive.
            IF ( ISPIX2 .AND. ( NINT( VALUE2 ) .LE. 0 ) ) THEN
               STATUS = NDF__BNDIN
               CALL ERR_REP( 'NDF1_PSNDF_PEXT',
     :                       'Invalid dimension extent specified; ' //
     :                       'a positive number of pixels is required.',
     :                       STATUS )

*  If the extent is in axis units, then we can also allow a value of
*  zero (which translates into an extent of one pixel).
            ELSE IF ( ( .NOT. ISPIX2 ) .AND.
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
