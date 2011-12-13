      SUBROUTINE NDF1_AVSBN( LBNDV, UBNDV, IAX, IACB, STATUS )
*+
*  Name:
*     NDF1_AVSBN

*  Purpose:
*     Set new bounds for an axis variance array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AVSBN( LBNDV, UBNDV, IAX, IACB, STATUS )

*  Description:
*     The routine sets new pixel-index bounds for an NDF's axis
*     variance array, retaining the existing array values (if they
*     exist).  The existing values will be extrapolated if the new
*     bounds extend beyond the old ones.

*  Arguments:
*     LBNDV( 1 ) = INTEGER (Given)
*        New lower pixel-index bound for the axis.
*     UBNDV( 1 ) = INTEGER (Given)
*        New upper pixel-index bound for the axis.
*     IAX = INTEGER (Given)
*        Number of the NDF axis whose variance array bounds are to be
*        changed.
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine may only be used to change the bounds of an axis
*     variance array for a base NDF. It will return without action if
*     the NDF supplied is a section.
*     -  When changing the pixel-index bounds of an NDF, this routine
*     should be called prior to making any changes to the NDF's main
*     data array, as it depends on this array's bounds matching the
*     initial axis variance array bounds.

*  Algorithm:
*     -  Check that the NDF is not a section. There is nothing to do if
*     it is.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Ensure that information about the axis variance array is
*     available in the DCB.
*     -  Check whether the axis variance array exists. If not, then no
*     modifications will be needed to the data object, but the array's
*     default storage form in the DCB must be updated to take account
*     of the new bounds, if necessary.
*     -  If the array does exist, then obtain the bounds and number of
*     dimensions of the NDF data object from the ARY_ system identifier
*     for its main data array, held in the DCB.
*     -  Determine whether the new axis bounds lie inside the current
*     ones.
*     -  If so, then simply set appropriate new bounds for the axis
*     variance array.
*     -  If the new bounds lie outside the existing bounds, then the
*     new axis variance array will have to hold values which are an
*     extrapolation of the current one.
*     -  Obtain the variance array's numeric type and storage form and
*     convert the storage form to take account of the new array bounds
*     if necessary.
*     -  Generate the pixel index bounds of an NDF section which has
*     the required bounds along the current axis, leaving other axis
*     bounds unchanged.
*     -  Create a new ACB entry describing the section.
*     -  Generate a temporary component name for use in the parent axis
*     structure and obtain an ARY_ system placeholder for a new array
*     with this name.
*     -  Test the array storage form against each valid value in turn
*     and take the appropriate action.
*     -  If required, then create a primitive array.
*     -  Map the axis array of the NDF section for reading (this causes
*     any necessary extrapolation of values to take place), and the new
*     array for writing, and copy the values across.
*     -  Unmap the arrays when done.
*     -  Otherwise, if required, perform the equivalent operations for a
*     simple array.
*     -  If an unsupported array storage form was encountered, then
*     report an error.
*     -  Annul the ACB entry for the temporary NDF section and the
*     identifier for the temporary array.
*     -  Delete the original axis variance array.
*     -  Obtain a locator to the temporary array and rename it to
*     become the new axis variance array.
*     -  Import the array into the ARY_ system, storing its identifier
*     in the DCB.
*     -  Annul the locator afterwards.
*     -  Note if the DCB axis variance array information is now correct.

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     23-OCT-1990 (RFWS):
*        Original version, derived from the NDF1_ADSBN routine.
*     2-NOV-1990 (RFWS):
*        Re-structured the re-importation of the new variance array.
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
*        DCB_AVFRM( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
*        ) (Read and Write)
*           Storage form of axis variance arrays.
*        DCB_AVID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifiers for axis variance arrays.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_KAV( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Whether information about axis variance arrays is
*           available.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER LBNDV( 1 )
      INTEGER UBNDV( 1 )
      INTEGER IAX
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) TLOC ! Locator to temporary component
      CHARACTER * ( DAT__SZNAM ) TNAME ! Temporary component name
      CHARACTER * ( NDF__SZFRM ) FORM ! Array storage form
      CHARACTER * ( NDF__SZTYP ) TYPE ! Array numeric type
      INTEGER EL                 ! Number of mapped elements
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACBS              ! ACB index of temporary NDF section
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER LBNDS( NDF__MXDIM ) ! Lower bounds of temporary section
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER PLACE              ! ARY_ system placeholder
      INTEGER PNTR               ! Pointer to mapped values
      INTEGER TIARY              ! Temporary ARY_ system identifier
      INTEGER TPNTR              ! Pointer to mapped temporary array
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      INTEGER UBNDS( NDF__MXDIM ) ! Upper bounds of temporary section
      LOGICAL INSIDE             ! New bounds inside old ones?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the NDF is not a section. There is nothing to do if it
*  is.
      IF ( .NOT. ACB_CUT( IACB ) ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Ensure that information about the axis variance array is available
*  in the DCB.
         CALL NDF1_DAV( IAX, IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check whether the axis variance array exists. If not, then no
*  modifications will be needed to the data object, but the array's
*  default storage form in the DCB must be updated to take account of
*  the new bounds, if necessary.
            IF ( DCB_AVID( IAX, IDCB ) .EQ. ARY__NOID ) THEN
               CALL NDF1_CBFRM( 1, LBNDV, UBNDV, DCB_AVFRM( IAX, IDCB ),
     :                          STATUS )

*  If the array does exist, then obtain the bounds and number of
*  dimensions of the NDF data object from the ARY_ system identifier
*  for its main data array, held in the DCB.
            ELSE
               CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBND, UBND,
     :                         NDIM, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Determine whether the new axis bounds lie inside the current ones.
                  INSIDE = ( LBNDV( 1 ) .GE. LBND( IAX ) ) .AND.
     :                     ( UBNDV( 1 ) .LE. UBND( IAX ) )

*  If so, then simply set appropriate new bounds for the axis variance
*  array.
                  IF ( INSIDE ) THEN
                     CALL ARY_SBND( 1, LBNDV, UBNDV,
     :                              DCB_AVID( IAX, IDCB ), STATUS )

*  If the new bounds lie outside the existing bounds, then the new axis
*  variance array will have to hold values which are an extrapolation
*  of the current one.
                  ELSE

*  Obtain the variance array's numeric type and storage form and
*  convert the storage form to take account of the new array bounds if
*  necessary.
                     CALL ARY_TYPE( DCB_AVID( IAX, IDCB ), TYPE,
     :                              STATUS )
                     CALL ARY_FORM( DCB_AVID( IAX, IDCB ), FORM,
     :                              STATUS )
                     CALL NDF1_CBFRM( 1, LBNDV, UBNDV, FORM, STATUS )

*  Generate the pixel index bounds of an NDF section which has the
*  required bounds along the current axis, leaving other axis bounds
*  unchanged.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        DO 1 I = 1, NDIM
                           LBNDS( I ) = LBND( I )
                           UBNDS( I ) = UBND( I )
 1                      CONTINUE
                        LBNDS( IAX ) = LBNDV( 1 )
                        UBNDS( IAX ) = UBNDV( 1 )

*  Create a new ACB entry describing the section.
                        CALL NDF1_CUT( IACB, NDIM, LBNDS, UBNDS, IACBS,
     :                                 STATUS )

*  Generate a temporary component name for use in the parent axis
*  structure and obtain an ARY_ system placeholder for a new array with
*  this name.
                        CALL NDF1_TCNAM( DCB_ALOC( IAX, IDCB ), TNAME,
     :                                   STATUS )
                        CALL ARY_PLACE( DCB_ALOC( IAX, IDCB ), TNAME,
     :                                  PLACE, STATUS )

*  Test the array storage form against each valid value in turn and
*  take the appropriate action.

*  PRIMITIVE:
*  =========
*  Create a primitive array.
                        IF ( FORM .EQ. 'PRIMITIVE' ) THEN
                           CALL ARY_NEWP( TYPE, 1, UBNDV, PLACE, TIARY,
     :                                    STATUS )

*  Map the axis array of the NDF section for reading (this causes any
*  necessary extrapolation of values to take place), and the new array
*  for writing, and copy the values across.
                           CALL NDF1_AVMAP( IAX, IACBS, TYPE, 'READ',
     :                                      .FALSE., PNTR, EL, STATUS )
                           CALL ARY_MAP( TIARY, TYPE, 'WRITE', TPNTR,
     :                                   EL, STATUS )
                           CALL NDF1_MOVE( TYPE, EL, PNTR, TPNTR,
     :                                     STATUS )

*  Unmap the arrays when done.
                           CALL NDF1_AVUMP( IAX, IACBS, STATUS )
                           CALL ARY_UNMAP( TIARY, STATUS )

*  SIMPLE:
*  ======
*  Create a simple array.
                        ELSE IF ( FORM .EQ. 'SIMPLE' ) THEN
                           CALL ARY_NEW( TYPE, 1, LBNDV, UBNDV, PLACE,
     :                                   TIARY, STATUS )

*  Map the axis array of the NDF section for reading (this causes any
*  necessary extrapolation of values to take place), and the new array
*  for writing, and copy the values across.
                           CALL NDF1_AVMAP( IAX, IACBS, TYPE, 'READ',
     :                                      .FALSE., PNTR, EL, STATUS )
                           CALL ARY_MAP( TIARY, TYPE, 'WRITE', TPNTR,
     :                                   EL, STATUS )
                           CALL NDF1_MOVE( TYPE, EL, PNTR, TPNTR,
     :                                     STATUS )

*  Unmap the arrays when done.
                           CALL NDF1_AVUMP( IAX, IACBS, STATUS )
                           CALL ARY_UNMAP( TIARY, STATUS )

*  If an unsupported array storage form was encountered, then report an
*  error.
                        ELSE
                           STATUS = NDF__FATIN
                           CALL MSG_SETC( 'BADFORM', FORM )
                           CALL ERR_REP( 'NDF1_AVSBN_FORM',
     :                                   'Invalid axis array ' //
     :                                   'storage form ''BADFORM'' ' //
     :                                   'encountered in the NDF_ ' //
     :                                   'system Data Control Block ' //
     :                                   '(internal programming ' //
     :                                   'error).', STATUS )
                        END IF

*  Annul the ACB entry for the temporary NDF section and the identifier
*  for the temporary array.
                        CALL NDF1_ANL( IACBS, STATUS )
                        CALL ARY_ANNUL( TIARY, STATUS )
                     END IF

*  Delete the original axis variance array.
                     CALL ARY_DELET( DCB_AVID( IAX, IDCB ), STATUS )

*  Obtain a locator to the temporary array and rename it to become the
*  new axis variance array.
                     CALL DAT_FIND( DCB_ALOC( IAX, IDCB ), TNAME, TLOC,
     :                              STATUS )
                     CALL DAT_RENAM( TLOC, 'VARIANCE', STATUS )

*  Import the array into the ARY_ system, storing its identifier in the
*  DCB.
                     CALL ARY_IMPRT( TLOC, DCB_AVID( IAX, IDCB ),
     :                               STATUS )

*  Annul the locator afterwards.
                     CALL DAT_ANNUL( TLOC, STATUS )
                  END IF

*  Note if the DCB axis variance array information is now correct.
                  DCB_KAV( IAX, IDCB ) = STATUS .EQ. SAI__OK
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AVSBN', STATUS )

      END
