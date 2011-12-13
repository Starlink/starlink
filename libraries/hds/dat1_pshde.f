      SUBROUTINE DAT1_PSHDE( STR, NDIM, DIM, LBND, UBND, STATUS )
*+
*  Name:
*     DAT1_PSHDE

*  Purpose:
*     Parse an HDS dimension bounds expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DAT1_PSHDE( STR, NDIM, DIM, LBND, UBND, STATUS )

*  Description:
*     The routine parses an HDS dimension bound expression (such as
*     '1:10,2', '3:,,:7' or '3,,6') and returns the lower and upper
*     bounds for each dimension, using supplied defaults wherever
*     appropriate.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String containing the expression to be parsed.
*     NDIM = INTEGER (Given)
*        Number of dimension bounds expected.
*     DIM( NDIM ) = INTEGER (Given)
*        Object dimension sizes.
*     LBND( NDIM ) = INTEGER (Returned)
*        Lower bounds for each dimension.
*     UBND( NDIM ) = INTEGER (Returned)
*        Upper bounds for each dimension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The number of dimension bounds implied by the expression
*     supplied (one more than the number of separating commas which it
*     contains) must match the number specified via the NDIM argument.
*     An error will result if this is not the case.
*     -  A value of NDIM=0 is not permitted; dimension bounds cannot be
*     used with a scalar object.

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
*     this exceeds the expected number of dimensions.
*     -  If the field does not exist (i.e. there are two consecutive
*     commas or a comma at the start or end of the string) then use the
*     default bounds for the current dimension.
*     -  Otherwise, find the first and last non-blank characters in the
*     current dimension field.
*     -  If the field is blank, then apply the default bounds.
*     -  Otherwise, parse the field to determine the dimension bounds.
*     -  Make a contextual error report if an error occurs.
*     -  Increment the pointer to the start of the next field and
*     return to process it.
*     -  If the number of dimension bounds obtained is less than
*     expected, then report an error.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David Berry (UCLan, Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     29-OCT-1990 (RFWS):
*        Original version.
*     14-NOV-1990 (RFWS):
*        Added extra status check.
*     6-DEC-1990 (RFWS):
*        Added check to see whether a dimension bound field is blank
*        and to apply the default bounds if it is.
*     11-DEC-1990 (RFWS):
*        Improved error reports.
*     15-FEB-1998 (DSB):
*        Brought into NDG from NDF.
*     23-DEC-2005 (TIMJ):
*        Brought into HDS
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'DAT_ERR'          ! DAT_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER NDIM
      INTEGER DIM( NDIM )

*  Arguments Returned:
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER F                  ! First non-blank character in field
      INTEGER I1                 ! First character position in field
      INTEGER I2                 ! Last character position in field
      INTEGER L                  ! Last non-blank character in field
      INTEGER NBND               ! Number of dimension bounds
      LOGICAL COMMA              ! Comma terminated a field?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      NBND = 0
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
*  exceeds the expected number of dimensions.
         NBND = NBND + 1
         IF ( NBND .GT. NDIM ) THEN
            STATUS = DAT__DIMIN
            CALL EMS_SETC( 'SUBSET', STR )
            CALL EMS_SETI( 'NDIM', NDIM )
            CALL EMS_REP( 'DAT1_PSHDE_XS',
     :                    'Too many dimensions given in the subset ' //
     :                    'expression ''(^SUBSET)''; the associated ' //
     :                    'object is ^NDIM-dimensional.', STATUS )

*  If the field does not exist (i.e. there are two consecutive commas
*  or a comma at the start or end of the string) then use the default
*  bounds for the current dimension.
         ELSE
            IF ( I1 .GT. I2 ) THEN
               LBND( NBND ) = 1
               UBND( NBND ) = DIM( NBND )

*  Otherwise, find the first and last non-blank characters in the
*  current dimension field.
            ELSE
               CALL CHR_FANDL( STR( I1 : I2 ), F, L )

*  If the field is blank, then apply the default bounds.
               IF ( F .GT. L ) THEN
                  LBND( NBND ) = 1
                  UBND( NBND ) = DIM( NBND )

*  Otherwise, parse the field to determine the dimension bounds.
               ELSE
                  F = F + I1 - 1
                  L = L + I1 - 1
                  CALL DAT1_PSHDF( STR( F : L ), DIM( NBND ),
     :                             LBND( NBND ), UBND( NBND ), STATUS )

*  Make a contextual error report if an error occurs.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL EMS_SETI( 'NBND', NBND )
                     CALL EMS_SETC( 'SUBSET', STR )
                     CALL EMS_REP( 'DAT1_PSHDE_ERR',
     :                             'Error in dimension ^NBND of the ' //
     :                             'subset expression ''(^SUBSET)''.',
     :                             STATUS )
                  END IF
               END IF
            END IF
         END IF

*  Increment the pointer to the start of the next field and return to
*  process it.
         I1 = I2 + 2
         GO TO 1
      END IF

*  If the number of dimension bounds obtained is less than expected,
*  then report an error.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NBND .LT. NDIM ) ) THEN
         STATUS = DAT__DIMIN
         CALL EMS_SETC( 'SUBSET', STR )
         CALL EMS_SETI( 'NDIM', NDIM )
         CALL EMS_REP( 'DAT1_PSHDE_NE',
     :                 'Too few dimensions given in the subset ' //
     :                 'expression ''(^SUBSET)''; the associated ' //
     :                 'object is ^NDIM-dimensional.', STATUS )
      END IF

      END
