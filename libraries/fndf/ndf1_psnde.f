      SUBROUTINE NDF1_PSNDE( STR, NAX, LBND, UBND, IWCS, WCSSEC, VALUE1,
     :                       VALUE2, NVAL, FRAME1, FRAME2, ISBND,
     :                       ISDEF1, ISDEF2, STATUS )
*+
*  Name:
*     NDF1_PSNDE

*  Purpose:
*     Parse an NDF dimension bounds expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PSNDE( STR, NAX, LBND, UBND, IWCS, WCSSEC, VALUE1,
*                      VALUE2, NVAL, FRAME1, FRAME2, ISBND, ISDEF1,
*                      ISDEF2, STATUS )

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
*     NAX = INTEGER (Given)
*        Number of axes for which default bounds are available. If WCSSEC
*        is .TRUE., this should be the number of WCS axes. Otherwise it
*        should be the number of pixel axes.
*     LBND( NAX ) = DOUBLE PRECISION (Given)
*        Axis lower bounds (used to calculate defaults). These should be
*        pixel coordinates.
*     UBND( NAX ) = DOUBLE PRECISION (Given)
*        Axis upper bounds (used to calculate defaults). These should be
*        WCS axis values if WCSSEC is .TRUE., and should be pixel coordinates
*        otherwise.
*     IWCS = INTEGER (Given)
*        An AST pointer to the NDF's WCS FrameSet.
*     WCSSEC = LOGICAL (Given)
*        If .TRUE., then the section specifier uses WCS syntax. Otherwise,
*        it uses the old pixel/axis syntax. In WCS syntax, the supplied
*        STR string should contain a specification for the bounds on each
*        WCS axis, supplied in the order of the WCS axes. Each bound
*        specification must be given using WCS axis values. The number of
*        bounds specifications must equal the number of WCS axes (supplied
*        in NAX). If WCSSEC is .FALSE., the supplied STR string should
*        contain a specification for the bounds on each pixel axis, supplied
*        in the order of the pixel axes. Each bound specification must be
*        given using either pixel indices (integers), or WCS axis values.
*     VALUE1( NDF__MXDIM ) = DOUBLE PRECISION (Returned)
*        First value specifying section bounds for each dimension.
*     VALUE2( NDF__MXDIM ) = DOUBLE PRECISION (Returned)
*        Second value specifying section bounds for each dimension.
*     NVAL = INTEGER (Returned)
*        Number of axes for which values are returned (cannot exceed
*        NDF__MXDIM).
*     FRAME1( NDF__MXDIM ) = INTEGER (Returned)
*        0 ==> the corresponding VALUE1 value is to be interpreted as a
*        WCS or axis coordinate value, 1 ==> it is a pixel index, 2 ==> it
*        is a FRACTION value.
*     FRAME2( NDF__MXDIM ) = LOGICAL (Returned)
*        0 ==> the corresponding VALUE2 value is to be interpreted as a
*        WCS or axis coordinate value, 1 ==> it is a pixel index, 2 ==> it
*        is a FRACTION value.
*     ISBND( NDF__MXDIM ) = LOGICAL (Returned)
*        .TRUE. ==> the corresponding VALUE1 and VALUE2 values specify
*        the lower and upper bounds of the section directly, .FALSE.
*        ==> VALUE1 specifies the centre of the dimension's extent and
*        VALUE2 specifies the dimension's size.
*     ISDEF1( NDF__MXDIM ) = LOGICAL (Returned)
*        .TRUE. ==> the corresponding VALUE1 value is a default value and
*        was not specified in the supplied string. .FALSE. ==> VALUE1
*        was specified explicitly in the supplied string.
*     ISDEF2( NDF__MXDIM ) = LOGICAL (Returned)
*        .TRUE. ==> the corresponding VALUE2 value is a default value and
*        was not specified in the supplied string. .FALSE. ==> VALUE2
*        was specified explicitly in the supplied string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The number of dimension bounds implied by the expression
*     supplied (one more than the number of separating commas which it
*     contains) must not exceed NDF__MXDIM. An error will be reported
*     if it does. It need not match the number of NDF dimensions
*     supplied.

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
*        Logical ISPX1/2 arguments changed to integer FRAME1/2, and
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
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'AST_PAR'          ! AST_ constants and functions

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER NAX
      DOUBLE PRECISION LBND( NAX )
      DOUBLE PRECISION UBND( NAX )
      INTEGER IWCS
      LOGICAL WCSSEC

*  Arguments Returned:
      DOUBLE PRECISION VALUE1( NDF__MXDIM )
      DOUBLE PRECISION VALUE2( NDF__MXDIM )
      INTEGER NVAL
      INTEGER FRAME1( NDF__MXDIM )
      INTEGER FRAME2( NDF__MXDIM )
      LOGICAL ISBND( NDF__MXDIM )
      LOGICAL ISDEF1( NDF__MXDIM )
      LOGICAL ISDEF2( NDF__MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION LBND0     ! Default lower dimension bound
      DOUBLE PRECISION UBND0     ! Default upper dimension bound
      INTEGER F                  ! First non-blank character in field
      INTEGER I1                 ! First character position in field
      INTEGER I2                 ! Last character position in field
      INTEGER L                  ! Last non-blank character in field
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
         IF ( WCSSEC .AND. NVAL .GT. NAX ) THEN
            STATUS = NDF__BNDIN
            CALL MSG_SETC( 'SECTION', STR )
            CALL MSG_SETI( 'MXDIM', NAX )
            CALL ERR_REP( 'NDF1_PSNDE_XS',
     :                    'Too many WCS axes given in the NDF ' //
     :                    'section expression ''(^SECTION)''; the ' //
     :                    'maximum number of WCS axes is ^MXDIM.',
     :                    STATUS )

         ELSE IF( NVAL .GT. NDF__MXDIM ) THEN
            STATUS = NDF__BNDIN
            CALL MSG_SETC( 'SECTION', STR )
            CALL MSG_SETI( 'MXDIM', NDF__MXDIM )
            CALL ERR_REP( 'NDF1_PSNDE_XS',
     :                    'Too many dimensions given in the NDF ' //
     :                    'section expression ''(^SECTION)''; the ' //
     :                    'maximum number of NDF dimensions is ^MXDIM.',
     :                    STATUS )

*  Set up values of the default lower and upper bounds for the current
*  dimension.
         ELSE
            IF ( NVAL .LE. NAX ) THEN
               LBND0 = LBND( NVAL )
               UBND0 = UBND( NVAL )
            ELSE
               LBND0 = 1.0D0
               UBND0 = 1.0D0
            END IF

*  If the field does not exist (i.e. there are two consecutive commas
*  or a comma at the start or end of the string) then use the default
*  values for the current dimension.
            IF ( I1 .GT. I2 ) THEN
               VALUE1( NVAL ) = LBND0
               VALUE2( NVAL ) = UBND0

               IF( WCSSEC ) THEN
                  FRAME1( NVAL ) = 0
                  FRAME2( NVAL ) = 0
               ELSE
                  FRAME1( NVAL ) = 1
                  FRAME2( NVAL ) = 1
               END IF

               ISBND( NVAL ) = .TRUE.
               ISDEF1( NVAL ) = .TRUE.
               ISDEF2( NVAL ) = .TRUE.

*  Otherwise, find the first and last non-blank characters in the
*  current dimension field.
            ELSE
               CALL CHR_FANDL( STR( I1 : I2 ), F, L )

*  If the field is blank, then apply the default bounds.
               IF ( F .GT. L ) THEN
                  VALUE1( NVAL ) = LBND0
                  VALUE2( NVAL ) = UBND0

                  IF( WCSSEC ) THEN
                     FRAME1( NVAL ) = 0
                     FRAME2( NVAL ) = 0
                  ELSE
                     FRAME1( NVAL ) = 1
                     FRAME2( NVAL ) = 1
                  END IF

                  ISBND( NVAL ) = .TRUE.
                  ISDEF1( NVAL ) = .TRUE.
                  ISDEF2( NVAL ) = .TRUE.

*  Otherwise, parse the field to determine the values which specify the
*  dimension bounds.
               ELSE
                  F = F + I1 - 1
                  L = L + I1 - 1
                  CALL NDF1_PSNDF( STR( F : L ), LBND0, UBND0, NVAL,
     :                             IWCS, WCSSEC, VALUE1( NVAL ),
     :                             VALUE2( NVAL ), FRAME1( NVAL ),
     :                             FRAME2( NVAL ), ISBND( NVAL ),
     :                             ISDEF1( NVAL ), ISDEF2( NVAL ),
     :                             STATUS )

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
