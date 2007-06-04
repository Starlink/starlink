      SUBROUTINE NDF1_WCLIM( IWCS, NAX, NDIM, VALUE1, VALUE2, ISBND,
     :                       LBND, UBND, STATUS )
*+
*  Name:
*     NDF1_WCLIM

*  Purpose:
*     Determine pixel limits for all NDF axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_WCLIM( IWCS, NAX, NDIM, VALUE1, VALUE2, ISBND, LBND, 
*                      UBND, STATUS )

*  Description:
*     This routine accepts values which have been supplied as WCS axis 
*     bounds in a NDF section specification, and calculates the 
*     corresponding NDF pixel-index bounds.

*  Arguments:
*     IWCS = INTEGER (Given)
*        AST pointer to the NDFs WCS FrameSet.
*     NAX = INTEGER (Given)
*        The number of WCS axis bound supplied.
*     NDIM = INTEGER (Given)
*        The number of pixel axes in the NDF.
*     VALUE1( NAX ) = DOUBLE PRECISION (Given and Returned)
*        First value specifying the bound on each WCS axis. On exit, any 
*        "centre/width" values are turned into "lbnd/ubnd" values, and
*        the positions are normalised using the AST_NORM method of the 
*        current WCS Frame.
*     VALUE2( NAX ) = DOUBLE PRECISION (Given and Returned)
*        Second value specifying the bound on each WCS axis. On exit, any 
*        "centre/width" values are turned into "lbnd/ubnd" values. and
*        the positions are normalised using the AST_NORM method of the 
*        current WCS Frame.
*     ISBND( NAX ) = LOGICAL (Given and Returned)
*        Whether VALUE1 and VALUE2 specify the lower and upper bounds
*        directly (as opposed to specifying the centre and width). On exit, 
*        any "centre/width" values are turned into "lbnd/ubnd" values.
*     LBND( NDIM ) = INTEGER (Returned)
*        Lower pixel-index bounds.
*     UBND( NDIM ) = INTEGER (Returned)
*        Upper pixel-index bounds.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S Berry (JACH, UCLan)
*     {enter_new_authors_here}

*  History:
*     21-MAY-2007 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants      
      INCLUDE 'AST_PAR'          ! AST_ constants and functions
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      INTEGER IWCS
      INTEGER NAX
      INTEGER NDIM

*  Arguments Given and Returned:
      DOUBLE PRECISION VALUE1( NAX )
      DOUBLE PRECISION VALUE2( NAX )
      LOGICAL ISBND( NAX )

*  Arguments Returned:
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION XU( NDF__MXDIM )
      DOUBLE PRECISION XL( NDF__MXDIM )
      DOUBLE PRECISION DLBND
      DOUBLE PRECISION DUBND
      INTEGER I
      INTEGER MAP
      INTEGER NWCS
      INTEGER TEMP
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if insufficient bounds have been supplied.
      NWCS = AST_GETI( IWCS, 'Nout', STATUS )
      IF( NAX .NE. NWCS ) THEN
         STATUS = NDF__BNDIN
         CALL MSG_SETI( 'NAX', NAX )
         CALL MSG_SETI( 'NWCS', NWCS )
         CALL ERR_REP( 'NDF1_WCSLIM_WAX', 'Number of axis bounds '//
     :                 'supplied (^NAX) does not equal the number '//
     :                  'of WCS axes (^NWCS) in the NDF.', STATUS )

*  Otherwise, ensure we have upper and lower bounds on all WCS axes.
      ELSE
         DO I = 1, NWCS
            IF( .NOT. ISBND( I ) ) THEN
               VALUE1( I ) = AST_AXOFFSET( IWCS, I, VALUE1( I ), 
     :                                     -0.5D0*VALUE2( I ), STATUS )
               VALUE2( I ) = AST_AXOFFSET( IWCS, I, VALUE1( I ), 
     :                                     VALUE2( I ), STATUS )
               ISBND( I ) = .TRUE.
            END IF
         END DO

*  Normalise the WCS coordinates at the two box cornres using the
*  AST_NORM method associated with the current WCS Frame.
         CALL AST_NORM( IWCS, VALUE1, STATUS )
         CALL AST_NORM( IWCS, VALUE2, STATUS )

*  Get the (current Frame)->PIXEL mapping from the FrameSet, and check it
*  is defined. Report an error if not.
         MAP = AST_GETMAPPING( IWCS, AST__CURRENT, 2, STATUS )
         IF( .NOT. AST_GETL( MAP, 'TranForward', STATUS ) ) THEN
            STATUS = NDF__BNDIN
            CALL ERR_REP( 'NDF1_WCLIM_WAX', 'The transformation from '//
     :                    'the current WCS coordinate system to pixel'//
     :                    ' coordinates is undefined, and so the '//
     :                    'supplied NDF section specified cannot be '//
     :                    'used.', STATUS )

*  If the Mapping is defined, use it to determine the bounds of the box in
*  grid coordinates that encompasses the WCS box.
         ELSE

      write(*,*) 'NDF1_WCLIM: WCS bounds'
            DO I = 1, NWCS
      write(*,*) '   ',VALUE1(i),':',value2(i)
            end do

      write(*,*) 'pixel bounds'

*  Find the extent of the WCS box on each pixel axis in turn.
            DO I = 1, NDIM
               CALL AST_MAPBOX( MAP, VALUE1, VALUE2, .TRUE., I, DLBND, 
     :                         DUBND, XL, XU, STATUS )

      write(*,*) '   ',DLBND,':',DUBND


*  Report an error if the GRID box is undefined.
               IF( DLBND .EQ. AST__BAD .OR. DUBND .EQ. AST__BAD ) THEN
                  IF( STATUS .EQ. SAI__OK ) THEN
                     STATUS = NDF__BNDIN
                     CALL ERR_REP( 'NDF1_WCLIM_WAX', 'The extent of '//
     :                         'the requested NDF section in pixel '//
     :                         'coordinates cannot be determined.', 
     :                         STATUS )
                  END IF

*  Otherwise convert to pixel indices
               ELSE
                  LBND( I ) = NINT( DLBND )
                  UBND( I ) = NINT( DUBND )
               END IF

            END DO

         END IF

*  Free the Mapping pointer.
         CALL AST_ANNUL( MAP, STATUS )

      END IF

*  If no error has occurred, then ensure that lower bound does not
*  exceed the upper bound, swapping the bounds if required.
      IF ( STATUS .EQ. SAI__OK ) THEN
         DO I = 1, NDIM 
            IF ( LBND( I ) .GT. UBND( I ) ) THEN
               TEMP = LBND( I )
               LBND( I ) = UBND( I )
               UBND( I ) = TEMP
            END IF
         END DO
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_WCLIM', STATUS )

      END
