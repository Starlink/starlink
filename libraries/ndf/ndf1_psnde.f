      SUBROUTINE NDF1_PSNDE( STR, NDIM, LBND, UBND, VALUE1, VALUE2,
     :                       NVAL, ISPIX1, ISPIX2, ISBND, STATUS )
*+
*  Name:
*     NDF1_PSNDE

*  Purpose:
*     Parse an NDF dimension bounds expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PSNDE( STR, NDIM, LBND, UBND, VALUE1, VALUE2, NVAL,
*                      ISPIX1, ISPIX2, ISBND, STATUS )

*  Description:
*     The routine parses an NDF section bound expression (such as
*     '1:10,2', '3:,,~7' or '31~5,,6') and returns two values
*     specifying the section's bounds in each dimension, together with
*     additional information specifying how the bounds should be
*     calculated from the returned values. Suitable defaults are used
*     where appropriate.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String containing the expression to be parsed.
*     NDIM = INTEGER (Given)
*        Number of NDF bounds.
*     LBND( NDIM ) = INTEGER (Given)
*        NDF lower bounds (used to calculate defaults).
*     UBND( NDIM ) = INTEGER (Given)
*        NDF upper bounds (used to calculate defaults).
*     VALUE1( NDF__MXDIM ) = DOUBLE PRECISION (Returned)
*        First value specifying section bounds for each dimension.
*     VALUE2( NDF__MXDIM ) = DOUBLE PRECISION (Returned)
*        Second value specifying section bounds for each dimension.
*     NVAL = INTEGER (Returned)
*        Number of dimensions for which values are returned (cannot
*        exceed NDF__MXDIM).
*     ISPIX1( NDF__MXDIM ) = LOGICAL (Returned)
*        .FALSE. ==> the corresponding VALUE1 value is to be
*        interpreted as an axis coordinate value, .TRUE. ==> it is a
*        pixel index.
*     ISPIX2( NDF__MXDIM ) = LOGICAL (Returned)
*        .FALSE. ==> the corresponding VALUE2 value is to be
*        interpreted as an axis coordinate value, .TRUE. ==> it is a
*        pixel index.
*     ISBND( NDF__MXDIM ) = LOGICAL (Returned)
*        .TRUE. ==> the corresponding VALUE1 and VALUE2 values specify
*        the lower and upper bounds of the section directly, .FALSE.
*        ==> VALUE1 specifies the centre of the dimension's extent and
*        VALUE2 specifies the dimension's size.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The number of dimension bounds implied by the expression
*     supplied (one more than the number of separating commas which it
*     contains) must not exceed NDF__MXDIM. An error will be reported
*     if it does. It need not match the number of NDF dimensions
*     supplied.

*  Algorithm:
*     -  Initialise.
*     -  Loop to extract each dimension bound field from the
*     expression.
*     -  If we are still within the bounds of the expression string,
*     then search for the end of the next field (the last character
*     before a comma or end of string). Note if a comma did not
*     terminate this field.
*     -  If we are outside the bounds of the expression, but have to
*     make one more pass to process the (blank) field following a final
*     comma, then use the end of string as the end of the field.
*     -  Increment the count of dimension bounds and report an error if
*     this exceeds the maximum number of dimensions.
*     -  Set up values of the NDF lower and upper bounds for the
*     current dimension.
*     -  If the field does not exist (i.e. there are two consecutive
*     commas or a comma at the start or end of the string) then use the
*     default values for the current dimension.
*     -  Otherwise, find the first and last non-blank characters in the
*     current dimension field.
*     -  If the field is blank, then apply the default bounds.
*     -  Otherwise, parse the field to determine the values which
*     specify the dimension bounds.
*     -  Make a contextual error report if an error occurs.
*     -  Increment the pointer to the start of the next field and
*     return to process it.

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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants      
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Arguments Returned:
      DOUBLE PRECISION VALUE1( NDF__MXDIM )
      DOUBLE PRECISION VALUE2( NDF__MXDIM )
      INTEGER NVAL
      LOGICAL ISPIX1( NDF__MXDIM )
      LOGICAL ISPIX2( NDF__MXDIM )
      LOGICAL ISBND( NDF__MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER F                  ! First non-blank character in field
      INTEGER I1                 ! First character position in field
      INTEGER I2                 ! Last character position in field
      INTEGER L                  ! Last non-blank character in field
      INTEGER LBND0              ! Default lower dimension bound
      INTEGER UBND0              ! Default upper dimension bound
      LOGICAL COMMA              ! Comma terminated a field?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      NVAL = 0
      I1 = 1
      COMMA = .TRUE.

*  Loop to extract each dimension bound field from the expression.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND. COMMA ) THEN

*  If we are still within the bounds of the expression string, then
*  search for the end of the next field (the last character before a
*  comma or end of string). Note if a comma did not terminate this
*  field.
         IF ( I1 .LE. LEN( STR ) ) THEN
            I2 = INDEX( STR( I1 : ) , ',' )
            IF ( I2 .EQ. 0 ) THEN
               I2 = LEN( STR )
               COMMA = .FALSE.
            ELSE
               I2 = I2 + I1 - 2
            END IF

*  If we are outside the bounds of the expression, but have to make one
*  more pass to process the (blank) field following a final comma, then
*  use the end of string as the end of the field.
         ELSE
            I2 = LEN( STR )
            COMMA = .FALSE.
         END IF

*  Increment the count of dimension bounds and report an error if this
*  exceeds the maximum number of dimensions.
         NVAL = NVAL + 1
         IF ( NVAL .GT. NDF__MXDIM ) THEN
            STATUS = NDF__BNDIN
            CALL MSG_SETC( 'SECTION', STR )
            CALL MSG_SETI( 'NDIM', NVAL )
            CALL MSG_SETI( 'MXDIM', NDF__MXDIM )
            CALL ERR_REP( 'NDF1_PSNDE_XS',
     :                    'Too many dimensions given in the NDF ' //
     :                    'section expression ''(^SECTION)''; the ' //
     :                    'maximum number of NDF dimensions is ^MXDIM.',
     :                    STATUS )

*  Set up values of the NDF lower and upper bounds for the current
*  dimension.
         ELSE
            IF ( NVAL .LE. NDIM ) THEN
               LBND0 = LBND( NVAL )
               UBND0 = UBND( NVAL )
            ELSE
               LBND0 = 1
               UBND0 = 1
            END IF

*  If the field does not exist (i.e. there are two consecutive commas
*  or a comma at the start or end of the string) then use the default
*  values for the current dimension.
            IF ( I1 .GT. I2 ) THEN
               VALUE1( NVAL ) = DBLE( LBND0 )
               VALUE2( NVAL ) = DBLE( UBND0 )
               ISPIX1( NVAL ) = .TRUE.
               ISPIX2( NVAL ) = .TRUE.
               ISBND( NVAL ) = .TRUE.

*  Otherwise, find the first and last non-blank characters in the
*  current dimension field.
            ELSE
               CALL CHR_FANDL( STR( I1 : I2 ), F, L )

*  If the field is blank, then apply the default bounds.
               IF ( F .GT. L ) THEN
                  VALUE1( NVAL ) = DBLE( LBND0 )
                  VALUE2( NVAL ) = DBLE( UBND0 )
                  ISPIX1( NVAL ) = .TRUE.
                  ISPIX2( NVAL ) = .TRUE.
                  ISBND( NVAL ) = .TRUE.

*  Otherwise, parse the field to determine the values which specify the
*  dimension bounds.
               ELSE
                  F = F + I1 - 1
                  L = L + I1 - 1
                  CALL NDF1_PSNDF( STR( F : L ), LBND0, UBND0,
     :                             VALUE1( NVAL ), VALUE2( NVAL ),
     :                             ISPIX1( NVAL ), ISPIX2( NVAL ),
     :                             ISBND( NVAL ), STATUS )

*  Make a contextual error report if an error occurs.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETI( 'NBND', NVAL )
                     CALL MSG_SETC( 'SECTION', STR )
                     CALL ERR_REP( 'NDF1_PSNDE_ERR',
     :                             'Error in dimension ^NBND of the ' //
     :                             'NDF section expression ' //
     :                             '''(^SECTION)''.', STATUS )
                  END IF
               END IF
            END IF
         END IF

*  Increment the pointer to the start of the next field and return to
*  process it.
         I1 = I2 + 2
         GO TO 1
      END IF     

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PSNDE', STATUS )

      END
