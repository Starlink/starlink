      SUBROUTINE ARD1_UBBOX( NDIM, FRM, TYPE, NPAR, PAR, UBXLB, UBXUB,
     :                       STATUS )
*+
*  Name:
*     ARD1_UBBOX

*  Purpose:
*     Return the bounds within user coordinates of a box enclosing the
*     supplied region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_UBBOX( NDIM, FRM, TYPE, NPAR, PAR, UBXLB, UBXUB, STATUS )

*  Description:
*     The user coordinate bounds of an n-D box enclosing the supplied
*     region are returned.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of pixel axes in the array.
*     FRM = INTEGER (Given)
*        The user cooridnate Frame.
*     TYPE = INTEGER (Given)
*        The identifier for the region type.
*     NPAR = INTEGER (Given)
*        The number of values in the PAR array.
*     PAR( NPAR ) = DOUBLE PRECISION (Given)
*        The parameters defining the region.
*     UBXLB( NDIM ) = DOUBLE PRECISION (Returned)
*        The lower bounds of the bounding box enclosing the region.
*     UBXUB( NDIM ) = DOUBLE PRECISION (Returned)
*        The upper bounds of the bounding box enclosing the region.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUN-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_CONST'        ! ARD private constants
      INCLUDE 'ARD_ERR'          ! ARD error constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_KWSYM( ARD__NKEYW)*( ARD__SZKEY ) = CHARACTER (Read)
*           The symbols used to represent each keyword.

*  Arguments Given:
      INTEGER NDIM
      INTEGER FRM
      INTEGER TYPE
      INTEGER NPAR
      DOUBLE PRECISION PAR( NPAR )

*  Arguments Returned:
      DOUBLE PRECISION UBXLB( NDIM )
      DOUBLE PRECISION UBXUB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :      I,                   ! Axis index
     :      NEED,                ! No. of parameters required by the region
     :      NWCS                 ! No. of user axes

      LOGICAL
     :      BADPAR,              ! A bad no. of parameters supplied?
     :      ERR2D                ! 2D keyword used in non-2D expression?

      DOUBLE PRECISION
     :      COST,                ! Cosine of rotation angle
     :      HW,                  ! Half the requested box width
     :      RAD,                 ! Radius
     :      SINT,                ! Sine of rotation angle
     :      XHW,                 ! Half length of a ellipse on X axis
     :      YHW                  ! Half length of a ellipse on Y axis
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialize the returned bounds.
      DO I = 1, NDIM
         UBXLB( I ) = VAL__MAXD
         UBXUB( I ) = VAL__MIND
      END DO

*  Nose the number of user axes.
      NWCS = AST_GETI( FRM, 'Naxes', STATUS )

*  Initialize a flag indicating if an invalid number of parameters have
*  been supplied.
      BADPAR = .FALSE.

*  Initialize a flag indicating if a 2D keyword has been used in a non-2D
*  context.
      ERR2D = .FALSE.

*  BOX: Parameters are the supplied user co-ordinates of the box centre,
*  followed by the lengths of the box sides in user co-ordinates. Report
*  an error if the wrong number of parameters has been supplied.
      IF( TYPE .EQ. ARD__BOX ) THEN
         NEED = 2*NWCS
         IF( NPAR .NE. NEED ) THEN
            BADPAR = .TRUE.

         ELSE
            DO I = 1, NWCS
               HW = ABS( 0.5*PAR( I + NDIM ) )
               UBXUB( I ) = AST_AXOFFSET( FRM, I, PAR( I ), HW,
     :                                    STATUS )
               UBXLB( I ) = AST_AXOFFSET( FRM, I, PAR( I ), -HW,
     :                                    STATUS )
            END DO
         END IF

*  POLYGON: Parameters are pairs of user co-ordinates, each being a vertex of
*  the polygon. 2-D only.
      ELSE IF( TYPE .EQ. ARD__POL ) THEN

*  Report an error if the dimensionality is not 2.
         IF( NWCS .NE. 2 ) THEN
            ERR2D = .TRUE.

*  Report an error if an incomplete parameter list is supplied.
         ELSE IF( MOD( NPAR, 2 ) .NE. 0 ) THEN
            STATUS = ARD__ARGS
            CALL MSG_SETI( 'NP', NPAR )
            CALL ERR_REP( 'ARD1_UBBOX_ERR1', 'Odd number of '//
     :                    'parameters (^NP) supplied for POLYGON '//
     :                    'region.', STATUS )

*  Report an error if less than 3 vertices have been supplied.
         ELSE IF( NPAR .LT. 6 ) THEN
            STATUS = ARD__INTER
            CALL MSG_SETI( 'NP', NPAR )
            CALL ERR_REP( 'ARD1_UBBOX_ERR1B', 'Wrong no. of '//
     :                    'parameters (^NP) supplied for ARD1_POL '//
     :                    '(programming error).', STATUS )
            GO TO 999

*  If OK...
         ELSE

            DO I = 1, NPAR - 1, 2
               UBXUB( 1 ) = MAX( UBXUB( 1 ), PAR( I ) )
               UBXLB( 1 ) = MIN( UBXLB( 1 ), PAR( I ) )
               UBXUB( 2 ) = MAX( UBXUB( 2 ), PAR( I + 1 ) )
               UBXLB( 2 ) = MIN( UBXLB( 2 ), PAR( I + 1 ) )
            END DO

         END IF

*  CIRCLE: Parameters are the user co-ordinates of the centre of the circle
*  or sphere, followed by the radius.
      ELSE IF( TYPE .EQ. ARD__CIR ) THEN

*  Report an error if the number of parameters is wrong.
         NEED = NWCS + 1
         IF( NPAR .NE. NEED ) THEN
            BADPAR = .TRUE.

         ELSE
            RAD = ABS( PAR( NPAR ) )
            DO I = 1, NWCS
               UBXUB( I ) = PAR( I ) + RAD
               UBXLB( I ) = PAR( I ) - RAD
            END DO
         END IF

*  ELLIPSE: Parameters are the user co-ordinates of the centre of the ellipse,
*  the half-lengths of the two axes of the ellipse, and the angle (in degrees)
*  between the first user axis and the first of the two ellipse axes.
*  Rotation from the 1st to the 2nd axis is positive. 2D only.
      ELSE IF( TYPE .EQ. ARD__ELL ) THEN

*  Report an error and abort if the dimensionality is not 2.
         IF( NWCS .NE. 2 ) THEN
            ERR2D = .TRUE.
         ELSE

*  Report an error if the number of parameters is wrong.
            NEED = 5
            IF( NPAR .NE. NEED ) THEN
               BADPAR = .TRUE.

*  If OK...
            ELSE

*  Store the sine and cosine of the orientation of the ellipse in user
*  co-ordinates.
               SINT = SIN( PAR( 5 )*ARD__DTOR )
               COST = COS( PAR( 5 )*ARD__DTOR )

*  Find the half-ranges of user co-ordinates encompassed by the
*  ellipse.
               XHW = SQRT( MAX( 0.0D0, ( PAR( 3 )*COST )**2 +
     :                                 ( PAR( 4 )*SINT )**2 ) )
               YHW = SQRT( MAX( 0.0D0, ( PAR( 4 )*COST )**2 +
     :                                 ( PAR( 3 )*SINT )**2 ) )

*  Abort if the ellipse is actually a line.
               IF( XHW .LE. 0.0 .OR. YHW .LE. 0.0 ) THEN
                  STATUS = ARD__ARGS
                  CALL ERR_REP( 'ARD1_UBBOX_ERR2', 'Null ELLIPSE '//
     :                          'region encountered in ARD '//
     :                          'description.', STATUS )
               END IF

*  Find the maximum and minimum Y pixel co-ordinates covered by the
*  ellipse.
               UBXUB( 1 ) = PAR( 1 ) + XHW
               UBXLB( 1 ) = PAR( 1 ) - XHW
               UBXUB( 2 ) = PAR( 2 ) + YHW
               UBXLB( 2 ) = PAR( 2 ) - YHW

            END IF
         END IF

*  Report an error and abort for any other keyword.
      ELSE
         STATUS = ARD__INTER
         CALL MSG_SETI( 'TYPE', TYPE )
         CALL ERR_REP( 'ARD1_UBBOX_ERR3', 'Illegal keyword identifier'//
     :                 ' (^TYPE) encountered in routine ARD1_UBBOX '//
     :                 '(programming error).', STATUS )
      END IF

 999  CONTINUE

*  Report an error if a 2D keyword was used in a non-2D context.
      IF( ERR2D .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = ARD__NOT2D
         CALL MSG_SETC( 'KW', CMN_KWSYM( TYPE ) )
         CALL ERR_REP( 'ARD1_UBBOX_ERR4', '^KW keyword found in '//
     :                 'non-2D ARD description.', STATUS )
         GO TO 999
      END IF

*  Report an error if an incorrect number of parameters was supplied.
      IF( BADPAR .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NEED', NEED )
         CALL MSG_SETI( 'NPAR', NPAR )
         CALL MSG_SETI( 'NDIM', NDIM )
         CALL MSG_SETC( 'KW', CMN_KWSYM( TYPE ) )
         CALL ERR_REP( 'ARD1_UBBOX_ERR5', 'A ^NDIM dimensional '//
     :                 '''^KW'' keyword requires ^NEED parameters, '//
     :                 'but ^NPAR were supplied to routine '//
     :                 'ARD1_UBBOX (programming error).', STATUS )
      END IF

      END
