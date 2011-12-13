      SUBROUTINE NDF1_AXLIM( IAX, IACB, VALUE1, VALUE2, ISPIX1, ISPIX2,
     :                       ISBND, LBND, UBND, STATUS )
*+
*  Name:
*     NDF1_AXLIM

*  Purpose:
*     Determine pixel limits for an NDF axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AXLIM( IAX, IACB, VALUE1, VALUE2, ISPIX1, ISPIX2,
*                      ISBND, LBND, UBND, STATUS )

*  Description:
*     This routine accepts two values which have been supplied as
*     dimension bounds in a NDF section specification, and which may
*     refer to coordinates in the NDF's axis coordinate system, and
*     calculates the corresponding NDF pixel-index bounds.

*  Arguments:
*     IAX = INTEGER (Given)
*        Number of the NDF axis.
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
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

*  Algorithm:
*     -  Initialise.
*     -  Obtain an index to the data object entry in the DCB and obtain
*     the bounds of the data object from the ARY_ system identifier for
*     its main data array, held in the DCB.
*     -  Obtain access to the axis arrays if necessary; see if access
*     to the NDF's axis arrays will be required.
*     -  If so, then ensure that axis data array information is
*     available in the DCB.
*     -  Determine the array's state. If it does not exist, then its
*     state is .FALSE..  Otherwise, use its identifier to determine its
*     state.
*     -  If no error has occurred, and the axis data array is in a
*     defined state, then ensure that axis width array information is
*     also available.
*     -  Determine the state of the axis width array. If it does not
*     exist, then its state is .FALSE..  Otherwise, use its identifier
*     to determine its state.
*     -  If the axis data array is in a defined state, then its values
*     must be accessed.
*     -  If the NDF is a section, then obtain an index to the data
*     object entry in the DCB and create a new base NDF entry in the
*     ACB to refer to it.  Note that we must do this so as to access
*     the entire axis array, in case extrapolation outside the original
*     NDF bounds is required.
*     -  Map the required axis data array for the base NDF for reading
*     as double precision values.
*     -  If the NDF supplied is a base NDF, then we can access its axis
*     arrays directly. First check if the required axis data array is
*     already mapped for access. If so, then the currently mapped
*     values will be used, but a double precision copy of them must be
*     made.
*     -  Create and map a temporary array to provide workspace for the
*     copy.
*     -  Convert the mapped values to double precision.
*     -  If the axis data array is not mapped, then note this fact and
*     map it in the required manner.
*     -  If an attempt was made to access the axis data array, but this
*     failed, then report contextual information.
*     -  If no error has occurred and the axis width array is in a
*     defined state, then its values must be accessed.
*     -  If the NDF is a section, then map the required axis width
*     array for the base NDF for reading as double precision values.
*     -  If the NDF supplied is a base NDF, then we can access its axis
*     arrays directly. First check if the required axis width array is
*     already mapped for access. If so, then the currently mapped
*     values will be used, but a double precision copy of them must be
*     made.
*     -  Create and map a temporary array to provide workspace for the
*     copy.
*     -  Convert the mapped values to double precision.
*     -  If the axis width array is not mapped, then note this fact and
*     map it in the required manner.
*     -  If an attempt was made to access the axis width array, but
*     this failed, then report contextual information.
*     -  Calculate the pixel limits for the NDF dimension.
*     -  If the values given specify the dimension bounds directly,
*     then determine the lower and upper bounds.
*     -  Use the lower bound directly if it is a pixel index.
*     -  Otherwise, convert from an axis value to a pixel index and use
*     this new value.
*     -  Similarly, use the upper bound directly if it is a pixel
*     index.
*     -  Otherwise, convert from an axis value first.
*     -  If the values supplied specify the centre and width of the
*     axis range, then each combination of pixel-index/axis value must
*     be handled in turn...
*     -  If both values are pixel indices, then derive the lower and
*     upper bounds directly.
*     -  If the first value is an axis value and the second is a pixel
*     index, then first convert the axis value to a pixel index.
*     -  Then derive the lower and upper bounds, as above.
*     -  If neither value is a pixel index, then derive the lower and
*     upper limits in terms of axis values and convert each of these to
*     pixel indices.
*     -  Ensure that the pixel indices are in the correct order
*     (remember, axis values may decrease as well as increase with
*     increasing pixel index).
*     -  If the first value is a pixel index, but the second is not,
*     then first convert the first value to an axis value.
*     -  Derive the lower and upper bounds in terms of axis values and
*     convert these to pixel indices.
*     -  Ensure that the pixel indices are in the correct order.
*     -  Release the axis arrays if they were accessed.
*     -  If the axis data array has been accessed, then release it. If
*     the NDF is a section, then unmap the base NDF axis data array and
*     annul its temporary entry in the ACB.
*     -  If access was to a temporary copy of the array, then annul the
*     identifier for this temporary copy. Otherwise, simply unmap the
*     array.
*     -  Similarly, if the axis width array has been accessed, then
*     release it. If the NDF is a section, then unmap the base NDF axis
*     width array and annul its temporary entry in the ACB.
*     -  If access was to a temporary copy of the array, then annul the
*     identifier for this temporary copy. Otherwise, simply unmap the
*     array.
*     -  Make final adjustments and checks.
*     -  If the NDF is a section, then obtain the value of any pixel
*     offset between the base NDF and the section, and correct the
*     bounds for this offset.
*     -  If no error has occurred, then check that the lower bound does
*     not exceed the upper bound and report an error if it does.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     12-MAR-1991 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ADID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifiers for axis data arrays.
*        DCB_AWID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for axis width arrays.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ADMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis data arrays are currently mapped for
*           access.
*        ACB_ADMPT( NDF__MXDIM, NDF__MXACB ) = INTEGER (Read)
*           Pointer to mapped axis data array.
*        ACB_ADMTP( NDF__MXDIM, NDF__MXACB ) = CHARACTER * ( NDF__SZTYP
*        ) (Read)
*           Numeric type used to map axis data arrays.
*        ACB_AWMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis width arrays are currently mapped for
*           access.
*        ACB_AWMPT( NDF__MXDIM, NDF__MXACB ) = INTEGER (Read)
*           Pointer to mapped axis width array.
*        ACB_AWMTP( NDF__MXDIM, NDF__MXACB ) = CHARACTER * ( NDF__SZTYP
*        ) (Read)
*           Numeric type used to map axis width arrays.
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IAX
      INTEGER IACB
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
      INTEGER IACB0              ! Index to base NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IDD                ! ID for temporary axis data array
      INTEGER IDW                ! ID for temporary axis width array
      INTEGER IPIX0( 2 )         ! "Lower" pixel index array
      INTEGER IPIX1( 2 )         ! "Nearest" pixel index array
      INTEGER LBND0( NDF__MXDIM ) ! Lower pixel bounds of base NDF
      INTEGER NDIM0              ! Number of base NDF dimensions
      INTEGER OFFS( NDF__MXDIM ) ! Pixel offsets for NDF section
      INTEGER PLACE              ! ARY_ system placeholder
      INTEGER UBND0( NDF__MXDIM ) ! Upper pixel bounds of base NDF
      INTEGER WPNTR              ! Pointer to mapped axis width array
      LOGICAL DATMAP             ! Axis data array already mapped?
      LOGICAL DCE                ! Data conversion errors?
      LOGICAL DSTATE             ! Logical state of axis data array
      LOGICAL INC                ! Axis values increase?
      LOGICAL INPIX( 2 )         ! Coordinate lies in a pixel?
      LOGICAL NEEDAX             ! Access to axis values is needed?
      LOGICAL WIDMAP             ! Axis width array already mapped?
      LOGICAL WSTATE             ! Logical state of axis width array

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      DSTATE = .FALSE.
      DPNTR = 0
      WSTATE = .FALSE.
      WPNTR = 0
      IACB0 = 0

*  Obtain an index to the data object entry in the DCB and obtain the
*  bounds of the data object from the ARY_ system identifier for its
*  main data array, held in the DCB.
      IDCB = ACB_IDCB( IACB )
      CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBND0, UBND0,
     :                NDIM0, STATUS )

*  Obtain access to the axis arrays if necessary.
*  =============================================

*  See if access to the NDF's axis arrays will be required.
      NEEDAX = .NOT. ( ISPIX1 .AND. ISPIX2 )

*  If so, then ensure that axis data array information is available in
*  the DCB.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. NEEDAX ) THEN
         CALL NDF1_DAD( IAX, IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Determine the array's state. If it does not exist, then its state is
*  .FALSE..  Otherwise, use its identifier to determine its state.
            DSTATE = DCB_ADID( IAX, IDCB ) .NE. ARY__NOID
            IF ( DSTATE ) THEN
               CALL ARY_STATE( DCB_ADID( IAX, IDCB ), DSTATE, STATUS )
            END IF
         END IF

*  If no error has occurred, and the axis data array is in a defined
*  state, then ensure that axis width array information is also
*  available.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. DSTATE ) THEN
            CALL NDF1_DAW( IAX, IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Determine the state of the axis width array. If it does not exist,
*  then its state is .FALSE..  Otherwise, use its identifier to
*  determine its state.
               WSTATE = DCB_AWID( IAX, IDCB ) .NE. ARY__NOID
               IF ( WSTATE ) THEN
                  CALL ARY_STATE( DCB_AWID( IAX, IDCB ), WSTATE,
     :                            STATUS )
               END IF
            END IF
         END IF

*  If the axis data array is in a defined state, then its values must be
*  accessed.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. DSTATE ) THEN

*  If the NDF is a section, then obtain an index to the data object
*  entry in the DCB and create a new base NDF entry in the ACB to refer
*  to it.  Note that we must do this so as to access the entire axis
*  array, in case extrapolation outside the original NDF bounds is
*  required.
            IF ( ACB_CUT( IACB ) ) THEN
               CALL NDF1_CRNBN( IDCB, IACB0, STATUS )

*  Map the required axis data array for the base NDF for reading as
*  double precision values.
               CALL NDF1_ADMAP( IAX, IACB0, '_DOUBLE', 'READ', DPNTR,
     :                          EL, STATUS )

*  If the NDF supplied is a base NDF, then we can access its axis
*  arrays directly. First check if the required axis data array is
*  already mapped for access. If so, then the currently mapped values
*  will be used, but a double precision copy of them must be made.
            ELSE IF ( ACB_ADMAP( IAX, IACB ) ) THEN
               DATMAP = .TRUE.

*  Create and map a temporary array to provide workspace for the copy.
               CALL ARY_TEMP( PLACE, STATUS )
               CALL ARY_NEW( '_DOUBLE', 1, LBND0( IAX ), UBND0( IAX ),
     :                       PLACE, IDD, STATUS )
               CALL ARY_MAP( IDD, '_DOUBLE', 'WRITE', DPNTR, EL,
     :                       STATUS )

*  Convert the mapped values to double precision.
               CALL NDF1_CVTD( .TRUE., EL, ACB_ADMTP( IAX, IACB ),
     :                         ACB_ADMPT( IAX, IACB ),
     :                         %VAL( CNF_PVAL( DPNTR ) ), DCE, STATUS )

*  If the axis data array is not mapped, then note this fact and map it
*  in the required manner.
            ELSE
               DATMAP = .FALSE.
               CALL NDF1_ADMAP( IAX, IACB, '_DOUBLE', 'READ', DPNTR, EL,
     :                          STATUS )
            END IF

*  If an attempt was made to access the axis data array, but this
*  failed, then report contextual information.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'NDF1_AXLIM_DMAP',
     :                       'Unable to access the axis CENTRE ' //
     :                       'array while converting axis ' //
     :                       'coordinates to pixel indices.',
     :                       STATUS )
            END IF
         END IF

*  If no error has occurred and the axis width array is in a defined
*  state, then its values must be accessed.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. WSTATE ) THEN

*  If the NDF is a section, then map the required axis width array for
*  the base NDF for reading as double precision values.
            IF ( ACB_CUT( IACB ) ) THEN
               CALL NDF1_AWMAP( IAX, IACB0, '_DOUBLE', 'READ', WPNTR,
     :                          EL, STATUS )

*  If the NDF supplied is a base NDF, then we can access its axis
*  arrays directly. First check if the required axis width array is
*  already mapped for access. If so, then the currently mapped values
*  will be used, but a double precision copy of them must be made.
            ELSE IF ( ACB_AWMAP( IAX, IACB ) ) THEN
               WIDMAP = .TRUE.

*  Create and map a temporary array to provide workspace for the copy.
               CALL ARY_TEMP( PLACE, STATUS )
               CALL ARY_NEW( '_DOUBLE', 1, LBND0( IAX ), UBND0( IAX ),
     :                       PLACE, IDW, STATUS )
               CALL ARY_MAP( IDW, '_DOUBLE', 'WRITE', WPNTR, EL,
     :                       STATUS )

*  Convert the mapped values to double precision.
               CALL NDF1_CVTD( .TRUE., EL, ACB_AWMTP( IAX, IACB ),
     :                         ACB_AWMPT( IAX, IACB ),
     :                         %VAL( CNF_PVAL( WPNTR ) ), DCE, STATUS )

*  If the axis width array is not mapped, then note this fact and map
*  it in the required manner.
            ELSE
               WIDMAP = .FALSE.
               CALL NDF1_AWMAP( IAX, IACB, '_DOUBLE', 'READ', WPNTR, EL,
     :                          STATUS )
            END IF

*  If an attempt was made to access the axis width array, but this
*  failed, then report contextual information.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'NDF1_AXLIM_WMAP',
     :                       'Unable to access the axis WIDTH array ' //
     :                       'while converting axis coordinates to ' //
     :                       'pixel indices.',
     :                       STATUS )
            END IF
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
               CALL NDF1_A2P( 1, AX, LBND0( IAX ), UBND0( IAX ),
     :                        DSTATE, WSTATE, %VAL( CNF_PVAL( DPNTR ) ),
     :                        %VAL( CNF_PVAL( WPNTR ) ), INC, IPIX0,
     :                        CENT0, SPACE0, INPIX, IPIX1, CENT1,
     :                        WIDTH1, STATUS )
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
               CALL NDF1_A2P( 1, AX, LBND0( IAX ), UBND0( IAX ),
     :                        DSTATE, WSTATE, %VAL( CNF_PVAL( DPNTR ) ),
     :                        %VAL( CNF_PVAL( WPNTR ) ), INC, IPIX0,
     :                        CENT0, SPACE0, INPIX, IPIX1, CENT1,
     :                        WIDTH1, STATUS )
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
               CALL NDF1_A2P( 1, AX, LBND0( IAX ), UBND0( IAX ),
     :                        DSTATE, WSTATE, %VAL( CNF_PVAL( DPNTR ) ),
     :                        %VAL( CNF_PVAL( WPNTR ) ), INC, IPIX0,
     :                        CENT0, SPACE0, INPIX, IPIX1, CENT1,
     :                        WIDTH1, STATUS )

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
               CALL NDF1_A2P( 2, AX, LBND0( IAX ), UBND0( IAX ),
     :                        DSTATE, WSTATE, %VAL( CNF_PVAL( DPNTR ) ),
     :                        %VAL( CNF_PVAL( WPNTR ) ), INC, IPIX0,
     :                        CENT0, SPACE0, INPIX, IPIX1, CENT1,
     :                        WIDTH1, STATUS )

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
               CALL NDF1_P2A( 1, IPIX1, LBND0( IAX ), UBND0( IAX ),
     :                        DSTATE, WSTATE, .FALSE.,
     :                        %VAL( CNF_PVAL( DPNTR ) ),
     :                        %VAL( CNF_PVAL( WPNTR ) ), VARIAN, CEN,
     :                        WID, VAR, STATUS )

*  Derive the lower and upper bounds in terms of axis values and convert
*  these to pixel indices.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  AX( 1 ) = CEN( 1 ) - 0.5D0 * VALUE2
                  AX( 2 ) = CEN( 1 ) + 0.5D0 * VALUE2
                  CALL NDF1_A2P( 2, AX, LBND0( IAX ), UBND0( IAX ),
     :                           DSTATE, WSTATE,
     :                           %VAL( CNF_PVAL( DPNTR ) ),
     :                           %VAL( CNF_PVAL( WPNTR ) ), INC, IPIX0,
     :                           CENT0, SPACE0, INPIX, IPIX1, CENT1,
     :                           WIDTH1, STATUS )

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

*  If the axis data array has been accessed, then release it. If the
*  NDF is a section, then unmap the base NDF axis data array and annul
*  its temporary entry in the ACB.
      IF ( NEEDAX ) THEN
         IF ( DSTATE ) THEN
            IF ( ACB_CUT( IACB ) ) THEN
               IF ( IACB0 .NE. 0 ) THEN
                  CALL NDF1_ADUMP( IAX, IACB0, STATUS )
                  CALL NDF1_ANL( IACB0, STATUS )
               END IF

*  If access was to a temporary copy of the array, then annul the
*  identifier for this temporary copy. Otherwise, simply unmap the
*  array.
            ELSE IF ( DATMAP ) THEN
               CALL ARY_ANNUL( IDD, STATUS )
            ELSE
               CALL NDF1_ADUMP( IAX, IACB, STATUS )
            END IF
         END IF

*  Similarly, if the axis width array has been accessed, then release
*  it. If the NDF is a section, then unmap the base NDF axis width
*  array and annul its temporary entry in the ACB.
         IF ( WSTATE ) THEN
            IF ( ACB_CUT( IACB ) ) THEN
               IF ( IACB0 .NE. 0 ) THEN
                  CALL NDF1_AWUMP( IAX, IACB0, STATUS )
                  CALL NDF1_ANL( IACB0, STATUS )
               END IF

*  If access was to a temporary copy of the array, then annul the
*  identifier for this temporary copy. Otherwise, simply unmap the
*  array.
            ELSE IF ( WIDMAP ) THEN
               CALL ARY_ANNUL( IDW, STATUS )
            ELSE
               CALL NDF1_AWUMP( IAX, IACB, STATUS )
            END IF
         END IF
      END IF

*  Make final adjustments and checks.
*  =================================

*  If the NDF is a section, then obtain the value of any pixel offset
*  between the base NDF and the section, and correct the bounds for this
*  offset.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ACB_CUT( IACB ) ) THEN
         CALL ARY_OFFS( DCB_DID( IDCB ), ACB_DID( IACB ), NDF__MXDIM,
     :                  OFFS, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            LBND = LBND + OFFS( IAX )
            UBND = UBND + OFFS( IAX )
         END IF
      END IF

*  If no error has occurred, then check that the lower bound does not
*  exceed the upper bound and report an error if it does.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( LBND .GT. UBND ) THEN
            STATUS = NDF__BNDIN
            CALL MSG_SETI( 'LBND', LBND )
            CALL MSG_SETI( 'UBND', UBND )
            CALL ERR_REP( 'NDF1_AXLIM_BND',
     :                    'Lower pixel bound (^LBND) exceeds ' //
     :                    'the upper bound (^UBND).',
     :                    STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AXLIM', STATUS )

      END
