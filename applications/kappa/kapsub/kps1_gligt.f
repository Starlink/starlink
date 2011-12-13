      SUBROUTINE KPS1_GLIGT( IWCS, IPIX, PARAM, MAXPIX, PIXPOS, NPOS,
     :                       STATUS )
*+
*  Name:
*     KPS1_GLIGT

*  Purpose:
*     Get pixel positions to be de-glitched from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_GLIGT( IWCS, IPIX, PARAM, MAXPIX, PIXPOS, NPOS, STATUS )

*  Description:
*     This routine gets upto MAXPIX current Frame positions from the user
*     using the specified parameter repeatedly, and returns the corresponding
*     PIXEL Frame positions.

*  Arguments:
*     IWCS = INTEGER (Given)
*        Pointer to the FrameSet. The Frame given by IPIX is the current
*        Frame on return.
*     IPIX = INTEGER (Given)
*        Index of the PIXEL Frame within IWCS.
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter to use.
*     MAXPIX = INTEGER (Given)
*        The maximum number of pixels which can be supplied.
*     PIXPOS( MAXPIX, 2 ) = DOUBLE PRECISION (Returned)
*        The returned pixel co-ordinate positions.
*     NPOS = INTEGER (Returned)
*        The number of returned positions.
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-MAR-2000 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE parameters
      INCLUDE 'NDF_PAR'        ! NDF definitions
      INCLUDE 'SUBPAR_PAR'     ! SUBPAR constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'AST_PAR'        ! AST constants and function declarations

*  Arguments Given:
      INTEGER IWCS
      INTEGER IPIX
      CHARACTER PARAM*(*)
      INTEGER MAXPIX

*  Arguments Returned:
      DOUBLE PRECISION PIXPOS( MAXPIX, 2 )
      INTEGER NPOS

*  Global Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION BPOS( 2 )         ! Base Frame position
      DOUBLE PRECISION CPOS( NDF__MXDIM )! Current Frame position
      INTEGER IAX                        ! Axis index
      INTEGER STATE                      ! Parameter state
      LOGICAL FIRST                      ! Doing the first position?
      LOGICAL LOOP                       ! Loop for more positions?
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  See if the parameter was supplied on the command line. If so, we do
*  not loop.
      CALL LPG_STATE( PARAM, STATE, STATUS )
      LOOP = ( STATE .NE. SUBPAR__ACTIVE )

*  Make the PIXEL Frame the Base Frame in the supplied FrameSet.
      CALL AST_SETI( IWCS, 'BASE', IPIX, STATUS )

*  Loop if required.
      FIRST = .TRUE.
      NPOS = 0
      DO WHILE( ( LOOP .OR. FIRST ) .AND. STATUS .EQ. SAI__OK )

*  Get a position from the parameter. Do not supply a suggested default.
*  The pixel co-ordinates are returned in BPOS, and the Current Frame
*  co-ordinates in CPOS.
         CPOS( 1 ) = AST__BAD
         CALL KPG1_GTPOS( PARAM, IWCS, .FALSE., CPOS, BPOS, STATUS )

*  If succesfull, store the position.
         IF( STATUS .EQ. SAI__OK ) THEN

*  If the array is full, warn the user, and leave the loop using the
*  positions supplied so far.
            IF( NPOS .EQ. MAXPIX ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'MX', MAXPIX )
               CALL ERR_REP( 'KPS1_GLIGT_ERR1', 'Too many positions '//
     :                       'supplied. Only the first ^MX will be'//
     :                       ' used.', STATUS )

               CALL ERR_ANNUL( STATUS )
               LOOP = .FALSE.

*  Otherwise, increment the number of supplied positions and store the
*  new pixel position.
            ELSE
               NPOS = NPOS + 1
               DO IAX = 1, 2
                  PIXPOS( NPOS, IAX ) = BPOS( IAX )
               END DO

*  Cancel the parameter value.
               CALL PAR_CANCL( PARAM, STATUS )
            END IF

*  If a null value was supplied, annull the error and leave the loop.
         ELSE IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            LOOP = .FALSE.
         END IF

         FIRST = .FALSE.

      END DO

      END
