      SUBROUTINE IRG1_AXLIM( IAX, INDF1, VALUE1, VALUE2, ISPIX1, ISPIX2,
     :                       ISBND, LBND, UBND, STATUS )
*+
*  Name:
*     IRG1_AXLIM

*  Purpose:
*     Determine pixel limits for an NDF axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG1_AXLIM( IAX, INDF1, VALUE1, VALUE2, ISPIX1, ISPIX2,
*                      ISBND, LBND, UBND, STATUS )

*  Description:
*     This routine accepts two values which have been supplied as
*     dimension bounds in a NDF section specification, and which may
*     refer to coordinates in the NDF's axis coordinate system, and
*     calculates the corresponding NDF pixel-index bounds.

*  Arguments:
*     IAX = INTEGER (Given)
*        Number of the NDF axis.
*     INDF1 = INTEGER (Given)
*        NDF identifier.
*     VALUE1 = DOUBLE PRECISION (Given)
*        First value specifying the NDF dimension bounds.
*     VALUE2 = DOUBLE PRECISION (Given)
*        Second value specifying the NDF dimension bounds.
*     ISPIX1 = LOGICAL (Given)
*        Whether VALUE1 is a pixel index (as opposed to an axis value).
*     ISPIX2 = LOGICAL (Given)
*        Whether VALUE2 is a pixel index (as opposed to an axis value).
*     ISBND = LOGICAL (Given)
*        Whether VALUE1 and VALUE2 specify the lower and upper bounds
*        directly (as opposed to specifying the centre and width).
*     LBND = INTEGER (Returned)
*        Lower pixel-index bound.
*     UBND = INTEGER (Returned)
*        Upper pixel-index bound.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-JAN-1992 (DSB):
*        Original version (based on NDF_$AXLIM by R.F. Warren-Smith)
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes      

*  Arguments Given:
      INTEGER IAX
      INTEGER INDF1
      DOUBLE PRECISION VALUE1
      DOUBLE PRECISION VALUE2
      LOGICAL ISPIX1
      LOGICAL ISPIX2
      LOGICAL ISBND

*  Arguments Returned:
      INTEGER LBND
      INTEGER UBND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AX( 2 )   ! Axis coordinate array
      DOUBLE PRECISION CEN( 1 )  ! Pixel centre value
      DOUBLE PRECISION CENT0( 2 ) ! "Lower" pixel centre array
      DOUBLE PRECISION CENT1( 2 ) ! "Nearest" pixel centre array
      DOUBLE PRECISION SPACE0( 2 ) ! Pixel spacing
      DOUBLE PRECISION VAR( 1 )  ! Pixel variance value
      DOUBLE PRECISION VARIAN( 1 ) ! Dummy variance array
      DOUBLE PRECISION WID( 1 )  ! Pixel width value
      DOUBLE PRECISION WIDTH1( 2 ) ! "Nearest" pixel width array
      INTEGER DPNTR              ! Pointer to mapped axis data array
      INTEGER EL                 ! Number of elements mapped
      INTEGER IPIX0( 2 )         ! "Lower" pixel index array
      INTEGER IPIX1( 2 )         ! "Nearest" pixel index array
      INTEGER LBND0( NDF__MXDIM )! Lower bounds of supplied NDF.
      INTEGER NDIM               ! Dimensionality of supplied NDF.
      INTEGER UBND0( NDF__MXDIM )! Upper bounds of supplied NDF.
      INTEGER WPNTR              ! Pointer to mapped axis width array
      LOGICAL INC                ! Axis values increase?
      LOGICAL INPIX( 2 )         ! Coordinate lies in a pixel?
      LOGICAL NEEDAX             ! Access to axis values is needed?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the bounds of the supplied NDF.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND0, UBND0, NDIM, STATUS )

*  Obtain access to the axis arrays if necessary.
*  =============================================

*  See if access to the NDF's axis arrays will be required.
      NEEDAX = .NOT. ( ISPIX1 .AND. ISPIX2 )

*  If so, then map the array holding the axis centre positions.
      IF( NEEDAX ) THEN
         CALL NDF_AMAP( INDF1, 'CENTRE', IAX, '_DOUBLE', 'READ', DPNTR,
     :                  EL, STATUS )      

*  If the attempt to access the axis data array failed, then report
*  contextual information.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'IRG1_AXLIM_ERR1',
     :                    'Unable to access the axis CENTRE ' //
     :                    'array while converting axis ' //
     :                    'coordinates to pixel indices.',
     :                    STATUS )
         END IF

*  Map the array holding the axis widths.
         CALL NDF_AMAP( INDF1, 'WIDTH', IAX, '_DOUBLE', 'READ', WPNTR,
     :                  EL, STATUS )      

*  If the attempt to access the axis width array failed, then report
*  contextual information.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'IRG1_AXLIM_ERR2',
     :                    'Unable to access the axis WIDTH ' //
     :                    'array while converting axis ' //
     :                    'coordinates to pixel indices.',
     :                    STATUS )
         END IF

      END IF

*  Calculate the pixel limits for the NDF dimension.
*  ================================================

*  If the values given specify the dimension bounds directly, then
*  determine the lower and upper bounds.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( ISBND ) THEN

*  Use the lower bound directly if it is a pixel index.
            IF ( ISPIX1 ) THEN
               LBND = NINT( VALUE1 )

*  Otherwise, convert from an axis value to a pixel index and use this
*  new value.
            ELSE
               AX( 1 ) = VALUE1
               CALL NDF_$A2P( 1, AX, LBND0( IAX ), UBND0( IAX ),
     :                        .TRUE., .TRUE., %VAL( DPNTR ),
     :                        %VAL( WPNTR), INC, IPIX0, CENT0, SPACE0,
     :                        INPIX, IPIX1, CENT1, WIDTH1, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  LBND = IPIX1( 1 )
               END IF
            END IF

*  Similarly, use the upper bound directly if it is a pixel index.
            IF ( ISPIX2 ) THEN
               UBND = NINT( VALUE2 )

*  Otherwise, convert from an axis value first.
            ELSE
               AX( 1 ) = VALUE2
               CALL NDF_$A2P( 1, AX, LBND0( IAX ), UBND0( IAX ),
     :                        .TRUE., .TRUE., %VAL( DPNTR ),
     :                        %VAL( WPNTR), INC, IPIX0, CENT0, SPACE0,
     :                        INPIX, IPIX1, CENT1, WIDTH1, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  UBND = IPIX1( 1 )
               END IF
            END IF

*  If the values supplied specify the centre and width of the axis
*  range, then each combination of pixel-index/axis value must be
*  handled in turn...
         ELSE

*  If both values are pixel indices, then derive the lower and upper
*  bounds directly.
            IF ( ISPIX1 .AND. ISPIX2 ) THEN
               LBND = NINT( VALUE1 ) - ( NINT( VALUE2 ) / 2 )
               UBND = NINT( VALUE1 ) + ( NINT( VALUE2 ) / 2 )
               IF ( UBND - LBND + 1 .GT. NINT( VALUE2 ) )
     :            LBND = LBND + 1

*  If the first value is an axis value and the second is a pixel index,
*  then first convert the axis value to a pixel index.
            ELSE IF ( ( .NOT. ISPIX1 ) .AND. ISPIX2 ) THEN
               AX( 1 ) = VALUE1
               CALL NDF_$A2P( 1, AX, LBND0( IAX ), UBND0( IAX ),
     :                        .TRUE., .TRUE., %VAL( DPNTR ),
     :                        %VAL( WPNTR), INC, IPIX0, CENT0, SPACE0,
     :                        INPIX, IPIX1, CENT1, WIDTH1, STATUS )

*  Then derive the lower and upper bounds, as above.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  LBND = IPIX1( 1 ) - ( NINT( VALUE2 ) / 2 )
                  UBND = IPIX1( 1 ) + ( NINT( VALUE2 ) / 2 )
                  IF ( UBND - LBND + 1 .GT. NINT( VALUE2 ) )
     :               LBND = LBND + 1
               END IF

*  If neither value is a pixel index, then derive the lower and upper
*  limits in terms of axis values and convert each of these to pixel
*  indices.
            ELSE IF ( ( .NOT. ISPIX1 ) .AND. ( .NOT. ISPIX2 ) ) THEN
               AX( 1 ) = VALUE1 - 0.5D0 * VALUE2
               AX( 2 ) = VALUE1 + 0.5D0 * VALUE2
               CALL NDF_$A2P( 2, AX, LBND0( IAX ), UBND0( IAX ),
     :                        .TRUE., .TRUE., %VAL( DPNTR ),
     :                        %VAL( WPNTR ), INC, IPIX0, CENT0, SPACE0,
     :                        INPIX, IPIX1, CENT1, WIDTH1, STATUS )

*  Ensure that the pixel indices are in the correct order (remember,
*  axis values may decrease as well as increase with increasing pixel
*  index).
               IF ( STATUS .EQ. SAI__OK ) THEN
                  LBND = MIN( IPIX1( 1 ), IPIX1( 2 ) )
                  UBND = MAX( IPIX1( 1 ), IPIX1( 2 ) )
               END IF

*  If the first value is a pixel index, but the second is not, then
*  first convert the first value to an axis value.
            ELSE IF ( ISPIX1 .AND. ( .NOT. ISPIX2 ) ) THEN
               IPIX1( 1 ) = NINT( VALUE1 )
               CALL NDF_$P2A( 1, IPIX1, LBND0( IAX ), UBND0( IAX ),
     :                        .TRUE., .TRUE., .FALSE.,
     :                        %VAL( DPNTR ), %VAL( WPNTR ), VARIAN,
     :                        CEN, WID, VAR, STATUS )

*  Derive the lower and upper bounds in terms of axis values and convert
*  these to pixel indices.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  AX( 1 ) = CEN( 1 ) - 0.5D0 * VALUE2
                  AX( 2 ) = CEN( 1 ) + 0.5D0 * VALUE2
                  CALL NDF_$A2P( 2, AX, LBND0( IAX ), UBND0( IAX ),
     :                           .TRUE., .TRUE., %VAL( DPNTR ),
     :                           %VAL( WPNTR ), INC, IPIX0, CENT0,
     :                           SPACE0, INPIX, IPIX1, CENT1, WIDTH1,
     :                           STATUS )

*  Ensure that the pixel indices are in the correct order.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     LBND = MIN( IPIX1( 1 ), IPIX1( 2 ) )
                     UBND = MAX( IPIX1( 1 ), IPIX1( 2 ) )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Release the axis arrays if they were accessed.
*  =============================================
      IF ( NEEDAX ) THEN
         CALL NDF_AUNMP( INDF1, 'CENTRE', IAX, STATUS )
         CALL NDF_AUNMP( INDF1, 'WIDTH', IAX, STATUS )
      END IF

*  Make final adjustments and checks.
*  =================================
*  If no error has occurred, then check that the lower bound does not
*  exceed the upper bound and report an error if it does.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( LBND .GT. UBND ) THEN
            STATUS = NDF__BNDIN
            CALL MSG_SETI( 'LBND', LBND )      
            CALL MSG_SETI( 'UBND', UBND )
            CALL ERR_REP( 'IRG1_AXLIM_ERR3',
     :                    'Lower pixel bound (^LBND) exceeds ' //
     :                    'the upper bound (^UBND).',
     :                    STATUS )
         END IF
      END IF

      END
* $Id$
