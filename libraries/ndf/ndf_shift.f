      SUBROUTINE NDF_SHIFT( NSHIFT, SHIFT, INDF, STATUS )
*+
*  Name:
*     NDF_SHIFT

*  Purpose:
*     Apply pixel-index shifts to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_SHIFT( NSHIFT, SHIFT, INDF, STATUS )

*  Description:
*     The routine applies pixel-index shifts to an NDF. An integer
*     shift is applied to each dimension so that its pixel-index
*     bounds, and the indices of each pixel, change by the amount of
*     shift applied to the corresponding dimension. The NDF's pixels
*     retain their values and none are lost.

*  Arguments:
*     NSHIFT = INTEGER (Given)
*        Number of dimensions to which shifts are to be applied. This
*        must not exceed the number of NDF dimensions. If fewer shifts
*        are applied than there are NDF dimensions, then the extra
*        dimensions will not be shifted.
*     SHIFT( NSHIFT ) = INTEGER (Given)
*        The pixel-index shifts to be applied to each dimension.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Pixel-index shifts applied to a base NDF will affect the
*     appearance of that NDF as seen by all base-NDF identifiers
*     associated with it. However, NDF sections derived from that base
*     NDF will remain unchanged (as regards both pixel-indices and
*     array values).
*     -  Pixel-index shifts applied to an NDF section only affect that
*     section itself, and have no effect on other NDF identifiers.
*     -  Pixel-index shifts cannot be applied to a base NDF while any
*     of its components (or any of its axis arrays) is mapped for
*     access, even through another identifier.
*     -  Pixel-index shifts cannot be applied to an NDF section while
*     any of its components (or any of its axis arrays) is mapped for
*     access through the identifier supplied to this routine.

*  Algorithm:
*     -  If the number of shifts specified is not positive, then report
*     an error.
*     -  Otherwise, import the NDF identifier.
*     -  Determine the NDF bounds and number of dimensions from the
*     ARY_ system identifier for the data array, held in the ACB.
*     -  Check that the number of shifts does not exceed the number of
*     NDF dimensions. Report an error if it does.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Check that SHIFT access to the NDF is available.
*     -  Ensure that quality and variance information is available in
*     the DCB and ACB (note that this must be done at the start because
*     once one component has been shifted, the bounds of subsequent
*     components will no longer match it, so an error would result due
*     to the checks which take place when information is obtained about
*     these subsequent components).
*     -  Similarly, ensure that information is available about all of
*     the NDF's axis components.
*     -  Check that no NDF component is mapped through the current ACB
*     entry.
*     -  Check all the axis arrays for current mappings as well if
*     necessary.
*     -  If any component is mapped, then report an error.
*     -  If a base NDF has been specified, then check that none of the
*     NDF's components is mapped for access.  Report an error if any
*     component is.
*     -  Calculate the new NDF bounds by applying the shifts.
*     -  Apply pixel-index shifts to the data component.
*     -  If this is a base NDF, then convert its default storage form
*     to take account of the new NDF bounds, if necessary.
*     -  See if the ARY_ system identifier for the quality component is
*     valid.
*     -  If not, then the component is not defined.
*     -  If it is defined, then apply pixel-index shifts to it.
*     -  If it is not defined, and this is a base NDF, then convert its
*     default storage form to take account of the new NDF bounds, if
*     necessary.
*     -  See if the ARY_ system identifier for the variance component
*     is valid. If not, then the component is not defined.
*     -  If it is defined, then apply pixel-index shifts to it.
*     -  If it is not defined, and this is a base NDF, then convert its
*     default storage form to take account of the new NDF bounds, if
*     necessary.
*     -  Axis arrays will only need a shift applied to them if this is
*     a base NDF. Check if this is so.
*     -  If required, loop to apply the shift to each affected NDF
*     axis.
*     -  If the axis data array exists, then apply the appropriate
*     shift to it.
*     -  If it does not exist, then convert its default storage form to
*     take account of the new NDF bounds, if necessary.
*     -  If the axis variance array exists, then apply the appropriate
*     shift to it.
*     -  If it does not exist, then convert its default storage form to
*     take account of the new NDF bounds, if necessary.
*     -  If the axis width array exists, then apply the appropriate
*     shift to it.
*     -  If it does not exist, then convert its default storage form to
*     take account of the new NDF bounds, if necessary.
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1990 (RFWS):
*        Original version.
*     21-MAR-1990 (RFWS):
*        Re-structured the checks on whether NDF components are mapped.
*     17-OCT-1990 (RFWS):
*        Installed support for the axis component and fixed a bug
*        causing the storage form of undefined array components to
*        remain set to 'PRIMITIVE' after a shift had been applied.
*     2-NOV-1990 (RFWS):
*        Improved handling of default storage form conversions.
*     28-NOV-1990 (RFWS):
*        Removed conversion of the default array storage form, which now
*        remains fixed at its initial value, regardless of changes to
*        the NDF's bounds.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ADFRM( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
*        ) (Read and Write)
*           Storage form of axis data arrays.
*        DCB_ADID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifiers for axis data arrays.
*        DCB_AVFRM( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
*        ) (Read and Write)
*           Storage form of axis variance arrays.
*        DCB_AVID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifiers for axis variance arrays.
*        DCB_AWFRM( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
*        ) (Read and Write)
*           Storage form of axis width arrays.
*        DCB_AWID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for axis width arrays.
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read)
*           Total number of current mappings to each data object.
*        DCB_QFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (
*        Read and Write)
*           Storage form used for the quality array.
*        DCB_VFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (
*        Read and Write)
*           The form of array used to store data in the NDF's variance
*           component.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ADMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis data arrays are currently mapped for
*           access.
*        ACB_AVMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis variance arrays are currently mapped for
*           access.
*        ACB_AWMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis width arrays are currently mapped for
*           access.
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF is a section.
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_DMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's data array is mapped for access.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_QID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's quality array.
*        ACB_QMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's quality array is mapped for access.
*        ACB_VID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's variance array.
*        ACB_VMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's variance array is mapped for access.

*  Arguments Given:
      INTEGER NSHIFT
      INTEGER SHIFT( * )
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IAX                ! Loop counter for axes
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      LOGICAL MAPPED             ! Is an NDF component mapped?
      LOGICAL THERE              ! Whether component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the number of shifts specified is not positive, then report an
*  error.
      IF ( NSHIFT .LE. 0 ) THEN
         STATUS = NDF__SFTIN
         CALL MSG_SETI( 'BADNSFT', NSHIFT )
         CALL ERR_REP( 'NDF_SHIFT_NSLO',
     :   'Invalid number of shifts (^BADNSFT) specified (possible ' //
     :   'programming error).', STATUS )

*  Otherwise, import the NDF identifier.
      ELSE
         CALL NDF1_IMPID( INDF, IACB, STATUS )
      END IF

*  Determine the NDF bounds and number of dimensions from the ARY_
*  system identifier for the data array, held in the ACB.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ARY_BOUND( ACB_DID( IACB ), NDF__MXDIM, LBND, UBND,
     :                   NDIM, STATUS )

*  Check that the number of shifts does not exceed the number of NDF
*  dimensions. Report an error if it does.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( NSHIFT .GT. NDIM ) THEN
               STATUS = NDF__SFTIN
               CALL MSG_SETI( 'BADNSFT', NSHIFT )
               CALL MSG_SETI( 'NDIM', NDIM )
               CALL ERR_REP( 'NDF_SHIFT_NSHI',
     :         'Number of shifts specified (^BADNSFT) exceeds ' //
     :         'the number of NDF dimensions (^NDIM) (possible ' //
     :         'programming error).', STATUS )
            END IF
         END IF

*  Obtain an index to the data object entry in the DCB.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IDCB = ACB_IDCB( IACB )

*  Check that SHIFT access to the NDF is available.
            CALL NDF1_CHACC( IACB, 'SHIFT', STATUS )

*  Ensure that quality and variance information is available in the DCB
*  and ACB (note that this must be done at the start because once one
*  component has been shifted, the bounds of subsequent components will
*  no longer match it, so an error would result due to the checks which
*  take place when information is obtained about these subsequent
*  components).
            CALL NDF1_QIMP( IACB, STATUS )
            CALL NDF1_VIMP( IACB, STATUS )

*  Similarly, ensure that information is available about all of the
*  NDF's axis components.
            DO 1 IAX = 1, NDIM
               CALL NDF1_DAD( IAX, IDCB, STATUS )
               CALL NDF1_DAV( IAX, IDCB, STATUS )
               CALL NDF1_DAW( IAX, IDCB, STATUS )
 1          CONTINUE
         END IF

*  Check that no NDF component is mapped through the current ACB entry.
         IF ( STATUS .EQ. SAI__OK ) THEN
            MAPPED = ACB_DMAP( IACB ) .OR.
     :               ACB_QMAP( IACB ) .OR.
     :               ACB_VMAP( IACB )

*  Check all the axis arrays for current mappings as well if necessary.
            IF ( .NOT. MAPPED ) THEN
               DO 2 IAX = 1, NDIM
                  IF ( ACB_ADMAP( IAX, IACB ) .OR.
     :                 ACB_AVMAP( IAX, IACB ) .OR.
     :                 ACB_AWMAP( IAX, IACB ) ) THEN
                     MAPPED = .TRUE.
                     GO TO 3
                  END IF
 2             CONTINUE
 3             CONTINUE
            END IF

*  If any component is mapped, then report an error.
            IF ( MAPPED ) THEN
               STATUS = NDF__ISMAP
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL ERR_REP( 'NDF_SHIFT_MAP1',
     :         'The NDF structure ^NDF is already mapped for access ' //
     :         'through the specified identifier (possible ' //
     :         'programming error).', STATUS )

*  If a base NDF has been specified, then check that none of the NDF's
*  components is mapped for access.  Report an error if any component
*  is.
            ELSE IF ( ( .NOT. ACB_CUT( IACB ) ) .AND.
     :                ( DCB_NMAP( IDCB ) .NE. 0 ) ) THEN
               STATUS = NDF__ISMAP
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF_SHIFT_MAP2',
     :         'The NDF structure ^NDF is already mapped for access ' //
     :         'through another identifier (possible programming ' //
     :         'error).', STATUS )
            END IF
         ENDIF

*  Calculate the new NDF bounds by applying the shifts.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO 4 I = 1, NSHIFT
               LBND( I ) = LBND( I ) + SHIFT( I )
               UBND( I ) = UBND( I ) + SHIFT( I )
 4          CONTINUE
         END IF

*  DATA component:
*  ==============
*  Apply pixel-index shifts to the data component.
         CALL ARY_SHIFT( NSHIFT, SHIFT, ACB_DID( IACB ), STATUS )

*  QUALITY component:
*  =================
*  See if the ARY_ system identifier for the quality component is valid.
*  If not, then the component is not defined.
         CALL ARY_VALID( ACB_QID( IACB ), THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If it is defined, then apply pixel-index shifts to it.
            IF ( THERE ) THEN
               CALL ARY_SHIFT( NSHIFT, SHIFT, ACB_QID( IACB ), STATUS )

*  If it is not defined, and this is a base NDF, then convert its
*  default storage form to take account of the new NDF bounds, if
*  necessary.
            ELSE IF ( .NOT. ACB_CUT( IACB ) ) THEN
               CALL NDF1_CBFRM( NDIM, LBND, UBND, DCB_QFRM( IDCB ),
     :                          STATUS )
            END IF
         END IF

*  VARIANCE:
*  ========
*  See if the ARY_ system identifier for the variance component is
*  valid. If not, then the component is not defined.
         CALL ARY_VALID( ACB_VID( IACB ), THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If it is defined, then apply pixel-index shifts to it.
            IF ( THERE ) THEN
               CALL ARY_SHIFT( NSHIFT, SHIFT, ACB_VID( IACB ), STATUS )

*  If it is not defined, and this is a base NDF, then convert its
*  default storage form to take account of the new NDF bounds, if
*  necessary.
            ELSE IF ( .NOT. ACB_CUT( IACB ) ) THEN
               CALL NDF1_CBFRM( NDIM, LBND, UBND, DCB_VFRM( IDCB ),
     :                          STATUS )
            END IF
         END IF

*  AXIS:
*  ====
*  Axis arrays will only need a shift applied to them if this is a base
*  NDF. Check if this is so.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( .NOT. ACB_CUT( IACB ) ) THEN

*  If required, loop to apply the shift to each affected NDF axis.
               DO 5 IAX = 1, NSHIFT

*  Axis data component.
*  ===================
*  If the array exists, then apply the appropriate shift to it.
                  IF ( DCB_ADID( IAX, IDCB ) .NE. ARY__NOID ) THEN
                     CALL ARY_SHIFT( 1, SHIFT( IAX ),
     :                               DCB_ADID( IAX, IDCB ), STATUS )

*  If it does not exist, then convert its default storage form to take
*  account of the new NDF bounds, if necessary.
                  ELSE
                     CALL NDF1_CBFRM( 1, LBND( IAX ), UBND( IAX ),
     :                                DCB_ADFRM( IAX, IDCB ), STATUS )
                  END IF

*  Axis variance component.
*  =======================
*  If the array exists, then apply the appropriate shift to it.
                  IF ( DCB_AVID( IAX, IDCB ) .NE. ARY__NOID ) THEN
                     CALL ARY_SHIFT( 1, SHIFT( IAX ),
     :                               DCB_AVID( IAX, IDCB ), STATUS )

*  If it does not exist, then convert its default storage form to take
*  account of the new NDF bounds, if necessary.
                  ELSE
                     CALL NDF1_CBFRM( 1, LBND( IAX ), UBND( IAX ),
     :                                DCB_AVFRM( IAX, IDCB ), STATUS )
                  END IF

*  Axis width component.
*  ====================
*  If the array exists, then apply the appropriate shift to it.
                  IF ( DCB_AWID( IAX, IDCB ) .NE. ARY__NOID ) THEN
                     CALL ARY_SHIFT( 1, SHIFT( IAX ),
     :                               DCB_AWID( IAX, IDCB ), STATUS )

*  If it does not exist, then convert its default storage form to take
*  account of the new NDF bounds, if necessary.
                  ELSE
                     CALL NDF1_CBFRM( 1, LBND( IAX ), UBND( IAX ),
     :                                DCB_AWFRM( IAX, IDCB ), STATUS )
                  END IF
 5             CONTINUE
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_SHIFT_ERR',
     :   'NDF_SHIFT: Error applying pixel-index shifts to an NDF.',
     :   STATUS )
         CALL NDF1_TRACE( 'NDF_SHIFT', STATUS )
      END IF

      END
