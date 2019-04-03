      SUBROUTINE NDF1_DAV( IAX, IDCB, STATUS )
*+
*  Name:
*     NDF1_DAV

*  Purpose:
*     Ensure that information about an NDF's axis variance array is
*     available in the DCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DAV( IAX, IDCB, STATUS )

*  Description:
*     The routine ensures that information about an NDF's axis variance
*     array is available in the DCB. If this information is already
*     available, then it returns without action. Otherwise, the actual
*     data object is examined to obtain this information, and an ARY_
*     system identifier for the axis variance array is stored in the
*     DCB.  Only those checks necessary to obtain and validate this
*     information are performed.

*  Arguments:
*     IAX = INTEGER (Given)
*        Number of the NDF axis for which information is required.
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check if information about the axis variance array is already
*     available.  There is nothing to do if it is.
*     -  Ensure that information about the NDF's axis structure is
*     available.
*     -  Set an initial null ARY_ system identifier in the DCB for the
*     axis variance array.
*     -  Check that the required axis structure element locator in the
*     DCB is not null. If it is, then there is no axis structure, so
*     the axis variance array identifier remains null.
*     -  See if the axis variance array component exists. If not, then
*     its DCB identifier remains null.
*     -  Import the axis variance array into the ARY_ system, storing
*     the identifier in the DCB.
*     -  Obtain the complex value flag and pixel index bounds of the
*     axis variance array and the pixel index bounds of the main NDF
*     data array.
*     -  Check that the axis variance array does not hold complex
*     values.  Report an error if it does.
*     -  Check that the dimensionality of the axis variance array is 1.
*     Report an error if it is not.
*     -  Check that the lower and upper pixel index bounds of the axis
*     variance array match the bounds of the corresponding NDF
*     dimension.  Report an error if they do not.
*     -  If an error occurred, then annul any ARY_ system identifier
*     which may have been allocated.
*     -  Set the default axis variance type to be _REAL and the default
*     axis variance storage form to match the NDF default storage form
*     derived from the data array.
*     -  Note whether axis variance array information is now available.

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
*     11-OCT-1990 (RFWS):
*        Original version, derived from the NDF1_DAD routine.
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.
*        DCB_AVID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Write)
*           ARY_ system identifiers for axis variance arrays.
*        DCB_AVFRM( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
*        ) (Write)
*           Storage form of axis variance arrays.
*        DCB_AVTYP( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP
*        ) (Write)
*           Numeric data type of axis variance arrays.
*        DCB_DEFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Read)
*           Default NDF array storage form.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_KAV( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether information about axis variance arrays is available.

*  Arguments Given:
      INTEGER IAX
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER LBNDV( NDF__MXDIM ) ! Axis variance array lower bounds
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NDIMV              ! Number of variance array dimensions
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      INTEGER UBNDV( NDF__MXDIM ) ! Axis variance array upper bounds
      LOGICAL CMPLXV             ! Whether variance array is complex
      LOGICAL THERE              ! Whether variance array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check if information about the axis variance array is already
*  available.  There is nothing to do if it is.
      IF ( .NOT. DCB_KAV( IAX, IDCB ) ) THEN

*  Ensure that information about the NDF's axis structure is available.
         CALL NDF1_DA( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Set an initial null ARY_ system identifier in the DCB for the axis
*  variance array.
            DCB_AVID( IAX, IDCB ) = ARY__NOID

*  Check that the required axis structure element locator in the DCB is
*  not null. If it is, then there is no axis structure, so the variance
*  array identifier remains null.
            IF ( DCB_ALOC( IAX, IDCB ) .NE. DAT__NOLOC ) THEN

*  See if the axis variance array component exists. If not, then its
*  DCB identifier remains null.
               CALL DAT_THERE( DCB_ALOC( IAX, IDCB ), 'VARIANCE',
     :                         THERE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( THERE ) THEN

*  Import the axis variance array into the ARY_ system, storing the
*  identifier in the DCB.
                     CALL ARY_FIND( DCB_ALOC( IAX, IDCB ), 'VARIANCE',
     :                              DCB_AVID( IAX, IDCB ), STATUS )

*  Obtain the complex value flag and pixel index bounds of the axis
*  variance array and the pixel index bounds of the main NDF data
*  array.
                     CALL ARY_CMPLX( DCB_AVID( IAX, IDCB ), CMPLXV,
     :                               STATUS )
                     CALL ARY_BOUND( DCB_AVID( IAX, IDCB ), NDF__MXDIM,
     :                               LBNDV, UBNDV, NDIMV, STATUS )
                     CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM,
     :                               LBND, UBND, NDIM, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the axis variance array does not hold complex values.
*  Report an error if it does.
                        IF ( CMPLXV ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'AXIS',
     :                                   DCB_ALOC( IAX, IDCB ) )
                           CALL ERR_REP( 'NDF1_DAV_CMPLX',
     :                     'The VARIANCE array in the NDF axis ' //
     :                     'structure ^AXIS holds illegal complex ' //
     :                     'values.', STATUS )

*  Check that the dimensionality of the axis variance array is 1.
*  Report an error if it is not.
                        ELSE IF ( NDIMV .NE. 1 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'AXIS',
     :                                   DCB_ALOC( IAX, IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIMV )
                           CALL ERR_REP( 'NDF1_DAV_NDIMV',
     :                     'The VARIANCE array in the NDF axis ' //
     :                     'structure ^AXIS is ' //
     :                     '^BADNDIM-dimensional; it should be ' //
     :                     '1-dimensional.', STATUS )

*  Check that the lower and upper pixel index bounds of the axis
*  variance array match the bounds of the corresponding NDF dimension.
*  Report an error if they do not.
                        ELSE IF ( ( LBNDV( 1 ) .NE. LBND( IAX ) ) .OR.
     :                            ( UBNDV( 1 ) .NE. UBND( IAX ) ) ) THEN
                           STATUS = NDF__BNDIN
                           CALL DAT_MSG( 'AXIS',
     :                                   DCB_ALOC( IAX, IDCB ) )
                           CALL MSG_SETI( 'LBNDV', LBNDV( 1 ) )
                           CALL MSG_SETI( 'UBNDV', UBNDV( 1 ) )
                           CALL MSG_SETI( 'LBND', LBND( IAX ) )
                           CALL MSG_SETI( 'UBND', UBND( IAX ) )
                           CALL ERR_REP( 'NDF1_DAV_BNDD',
     :                     'The pixel-index bounds (^LBNDV:^UBNDV) ' //
     :                     'of the VARIANCE array in the NDF axis ' //
     :                     'structure ^AXIS do not match the bounds ' //
     :                     'of the corresponding NDF dimension ' //
     :                     '(^LBND:^UBND).', STATUS )
                        END IF
                     END IF

*  If an error occurred, then annul any ARY_ system identifier which
*  may have been allocated.
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ARY_ANNUL( DCB_AVID( IAX, IDCB ), STATUS )
                     END IF
                  END IF
               END IF
            END IF
         END IF

*  Set the default axis variance type to be _REAL and the default axis
*  variance storage form to match the NDF default storage form derived
*  from the data array.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DCB_AVTYP( IAX, IDCB ) = '_REAL'
            DCB_AVFRM( IAX, IDCB ) = DCB_DEFRM( IDCB )
         END IF

*  Note whether axis variance array information is now available.
         DCB_KAV( IAX, IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DAV', STATUS )

      END
