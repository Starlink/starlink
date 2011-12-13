      SUBROUTINE NDF1_WCLIM( IWCS, NAX, NDIM, NLBND, NUBND, ISDEF1,
     :                       ISDEF2, VALUE1, VALUE2, ISBND, LBND,
     :                       UBND, STATUS )
*+
*  Name:
*     NDF1_WCLIM

*  Purpose:
*     Determine pixel limits for all NDF axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_WCLIM( IWCS, NAX, NDIM, NLBND, NUBND, ISDEF1, ISDEF2,
*                      VALUE1, VALUE2, ISBND, LBND, UBND, STATUS )

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
*     NLBND( NDIM ) = INTEGER (Given)
*        The NDF lower pixel bounds.
*     NUBND( NDIM ) = INTEGER (Given)
*        The NDF upper pixel bounds.
*     ISDEF1( NAX ) = LOGICAL (Given)
*        Is the value supplied VALUE1 a default value?
*     ISDEF2( NAX ) = LOGICAL (Given)
*        Is the value supplied VALUE2 a default value?
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry (JACH, UCLan)
*     {enter_new_authors_here}

*  History:
*     21-MAY-2007 (DSB):
*        Original version.
*     5-JUN-2007 (DSB):
*        Use better defaults if bounds on one or more axes are defaulted.
*     6-JUN-2007 (DSB):
*        Exclude "centre/width" bounds from the check described above.
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
      INTEGER NLBND( NDIM )
      INTEGER NUBND( NDIM )
      LOGICAL ISDEF1( NAX )
      LOGICAL ISDEF2( NAX )

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
      DOUBLE PRECISION DELTA
      DOUBLE PRECISION DLBND
      DOUBLE PRECISION DUBND
      DOUBLE PRECISION NDL( NDF__MXDIM )
      DOUBLE PRECISION NDU( NDF__MXDIM )
      DOUBLE PRECISION PLBND( NDF__MXDIM )
      DOUBLE PRECISION PUBND( NDF__MXDIM )
      DOUBLE PRECISION V1
      DOUBLE PRECISION V2
      DOUBLE PRECISION XL( NDF__MXDIM )
      DOUBLE PRECISION XU( NDF__MXDIM )
      INTEGER CFRM
      INTEGER CMPREG
      INTEGER I
      INTEGER MAP
      INTEGER NWCS
      INTEGER PBOX
      INTEGER PFRM
      INTEGER TEMP
      INTEGER WBOX
      INTEGER WBOXP
      LOGICAL DEF
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

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
*  Set a flag if any WCS limits were defaulted (but only if there were
*  supplied as upper/lower bound).
      ELSE
         CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
         DEF = .FALSE.

         DO I = 1, NWCS

            IF( .NOT. ISBND( I ) ) THEN
               VALUE1( I ) = AST_AXOFFSET( CFRM, I, VALUE1( I ),
     :                                     -0.5D0*VALUE2( I ), STATUS )
               VALUE2( I ) = AST_AXOFFSET( CFRM, I, VALUE1( I ),
     :                                     VALUE2( I ), STATUS )
               ISBND( I ) = .TRUE.

            ELSE IF( ISDEF1( I ) .OR. ISDEF2( I ) ) THEN
               DEF = .TRUE.
            END IF

         END DO

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

*  Only proceed if the Mapping is defined.
         ELSE

*  If any WCS limit was defaulted, it will have been set to the value
*  that encloses the whole NDF. But the non-defaulted limits have
*  restricted the area of interest and so these defaults may no longer be
*  appropriate. We now find better defaults for any missing limits that
*  span only the region implied by the other limits.
            IF( DEF ) THEN

*  The AST Box class knows nothing about axis normalisation. To avoid
*  problems ensure that the upper and lower axis values are in the same
*  "cylce". This applied particularly to RA values where the lower limit
*  may have a value of (say) 359 degrees and the upper limit be (say) 2
*  degrees. In this example the following code converts the upper limit
*  to 361 degrees.
               DO I = 1, NWCS
                  DELTA = AST_AXDISTANCE( CFRM, I, VALUE1( I ),
     :                                    VALUE2( I ), STATUS )
                  VALUE2( I ) = VALUE1( I ) + DELTA
               END DO

*  Create an AST Box describing the original (excesively large) WCS box.
               WBOX = AST_BOX( CFRM, 1, VALUE1, VALUE2, AST__NULL, ' ',
     :                         STATUS )

*  Map this Box into the PIXEL Frame. The resulting Region will (in
*  general) be a rotated box with curvi-linear edges.
               PFRM = AST_GETFRAME( IWCS, 2, STATUS )
               WBOXP = AST_MAPREGION( WBOX, MAP, PFRM, STATUS )

*  Create an AST Box describing the NDF pixel bounds.
               DO I = 1, NDIM
                  NDL( I ) = DBLE( NLBND( I ) ) - 1.0D0
                  NDU( I ) = DBLE( NUBND( I ) )
               END DO
               PBOX = AST_BOX( PFRM, 1, NDL, NDU, AST__NULL, ' ',
     :                         STATUS )

*  Now form a compound region that is the intersection of the two aboves
*  Boxes (both now defined in the PIXEL Frame).
               CMPREG = AST_CMPREGION( PBOX, WBOXP, AST__AND, ' ',
     :                                 STATUS )

*  Find the bounds (in PIXEL coords) of the compound Region.
               CALL AST_GETREGIONBOUNDS( CMPREG, PLBND, PUBND, STATUS )

*  Use this box to determine new defaults for any WCS limits that were
*  originally defaulted.
               DO I = 1, NWCS

*  Pass on to the next axis if this WCS axis did not have either limit
*  defaulted.
                  IF( ISDEF1( I ) .OR. ISDEF2( I ) ) THEN

*  Map the pixel box found above into WCS coords and get the limits of
*  the box on this WCS axis.
                     CALL AST_MAPBOX( MAP, PLBND, PUBND, .FALSE., I, V1,
     :                                V2, XL, XU, STATUS )

*  Whether a WCS value is a "lower" or "upper" bound is determined not by
*  the WCS values themselves but by which one gives the lower or upper
*  value on the corresponding pixel axis. Use this criterion to fill in
*  values for which ever WCS bound has not been supplied.
                     IF( ISDEF1( I ) ) THEN
                        IF( XL( I ) .LT. XU( I ) ) THEN
                           VALUE1( I ) = V1
                        ELSE
                           VALUE1( I ) = V2
                        END IF
                     END IF

                     IF( ISDEF2( I ) ) THEN
                        IF( XL( I ) .GT. XU( I ) ) THEN
                           VALUE2( I ) = V1
                        ELSE
                           VALUE2( I ) = V2
                        END IF
                     END IF
                  END IF
               END DO

            END IF

*  Now we use the Mapping to find the pixel box enclosing the
*  (potentially modified) WCS box. First, normalise the WCS
*  coordinates at the two box corners using the AST_NORM method
*  associated with the current WCS Frame.
            CALL AST_NORM( CFRM, VALUE1, STATUS )
            CALL AST_NORM( CFRM, VALUE2, STATUS )

*  Find the extent of the WCS box on each pixel axis in turn.
            DO I = 1, NDIM

*  Find the extent of the box on the I'th pixel axis.
               CALL AST_MAPBOX( MAP, VALUE1, VALUE2, .TRUE., I, DLBND,
     :                         DUBND, XL, XU, STATUS )

*  Report an error if the PIXEL box is undefined.
               IF( DLBND .EQ. AST__BAD .OR. DUBND .EQ. AST__BAD .OR.
     :             ABS( DLBND ) .GT. 1.0D7 .OR.
     :             ABS( DUBND ) .GT. 1.0D7 ) THEN
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

*  End the AST context.
      CALL AST_END( STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_WCLIM', STATUS )

      END
