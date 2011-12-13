      SUBROUTINE NDF1_DAD( IAX, IDCB, STATUS )
*+
*  Name:
*     NDF1_DAD

*  Purpose:
*     Ensure that information about an NDF's axis data array is
*     available in the DCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DAD( IAX, IDCB, STATUS )

*  Description:
*     The routine ensures that information about an NDF's axis data
*     array is available in the DCB. If this information is already
*     available, then it returns without action. Otherwise, the actual
*     data object is examined to obtain this information, and an ARY_
*     system identifier for the axis data array is stored in the DCB.
*     Only those checks necessary to obtain and validate this
*     information are performed.

*  Arguments:
*     IAX = INTEGER (Given)
*        Number of the NDF axis for which information is required.
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check if information about the axis data array is already
*     available.  There is nothing to do if it is.
*     -  Ensure that information about the NDF's axis structure is
*     available.
*     -  Set an initial null ARY_ system identifier in the DCB for the
*     axis data array.
*     -  Check that the required axis structure element locator in the
*     DCB is not null. If it is, then there is no axis structure, so
*     the axis data array identifier remains null.
*     -  See if the axis data array component exists. If not, then
*     report an error.
*     -  Import the axis data array into the ARY_ system, storing the
*     identifier in the DCB.
*     -  Obtain the complex value flag and pixel index bounds of the
*     axis data array and the pixel index bounds of the main NDF data
*     array.
*     -  Check that the axis data array does not hold complex values.
*     Report an error if it does.
*     -  Check that the dimensionality of the axis data array is 1.
*     Report an error if it is not.
*     -  Check that the lower and upper pixel index bounds of the axis
*     data array match the bounds of the corresponding NDF dimension.
*     Report an error if they do not.
*     -  If an error occurred, then annul any ARY_ system identifier
*     which may have been allocated.
*     -  Set the default axis data type to be _REAL and the default
*     axis data storage form to match the NDF default storage form
*     derived from the data array.
*     -  Note whether axis data array information is now available.

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
*     30-MAY-1990 (RFWS):
*        Original version.
*     29-NOV-1990 (RFWS):
*        Added initialisation of default axis data array attributes.
*        This was previously performed by NDF1_DA.
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

      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ADFRM( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
*        ) (Write)
*           Storage form of axis data arrays.
*        DCB_ADID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Write)
*           ARY_ system identifiers for axis data arrays.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.
*        DCB_ADTYP( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP
*        ) (Write)
*           Numeric data type of axis data arrays.
*        DCB_DEFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Read)
*           Default NDF array storage form.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_KAD( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether information about axis data arrays is available.

*  Arguments Given:
      INTEGER IAX
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER LBNDD( NDF__MXDIM ) ! Axis data array lower bounds
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NDIMD              ! Number of axis data array dimensions
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      INTEGER UBNDD( NDF__MXDIM ) ! Axis data array upper bounds
      LOGICAL CMPLXD             ! Whether axis data array is complex
      LOGICAL THERE              ! Whether axis data array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check if information about the axis data array is already available.
*  There is nothing to do if it is.
      IF ( .NOT. DCB_KAD( IAX, IDCB ) ) THEN

*  Ensure that information about the NDF's axis structure is available.
         CALL NDF1_DA( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Set an initial null ARY_ system identifier in the DCB for the axis
*  data array.
            DCB_ADID( IAX, IDCB ) = ARY__NOID

*  Check that the required axis structure element locator in the DCB is
*  not null. If it is, then there is no axis structure, so the data
*  array identifier remains null.
            IF ( DCB_ALOC( IAX, IDCB ) .NE. DAT__NOLOC ) THEN

*  See if the axis data array component exists. If not, then report an
*  error.
               CALL DAT_THERE( DCB_ALOC( IAX, IDCB ), 'DATA_ARRAY',
     :                         THERE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( .NOT. THERE ) THEN
                     STATUS = NDF__NOAXD
                     CALL DAT_MSG( 'AXIS', DCB_ALOC( IAX, IDCB ) )
                     CALL ERR_REP( 'NDF1_DAD_NODAT',
     :               'The DATA_ARRAY component in the NDF axis ' //
     :               'structure ^AXIS is missing.', STATUS )

*  Import the axis data array into the ARY_ system, storing the
*  identifier in the DCB.
                  ELSE
                     CALL ARY_FIND( DCB_ALOC( IAX, IDCB ), 'DATA_ARRAY',
     :                              DCB_ADID( IAX, IDCB ), STATUS )

*  Obtain the complex value flag and pixel index bounds of the axis
*  data array and the pixel index bounds of the main NDF data array.
                     CALL ARY_CMPLX( DCB_ADID( IAX, IDCB ), CMPLXD,
     :                               STATUS )
                     CALL ARY_BOUND( DCB_ADID( IAX, IDCB ), NDF__MXDIM,
     :                               LBNDD, UBNDD, NDIMD, STATUS )
                     CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM,
     :                               LBND, UBND, NDIM, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the axis data array does not hold complex values. Report
*  an error if it does.
                        IF ( CMPLXD ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'AXIS',
     :                                   DCB_ALOC( IAX, IDCB ) )
                           CALL ERR_REP( 'NDF1_DAD_CMPLX',
     :                     'The DATA_ARRAY array in the NDF axis ' //
     :                     'structure ^AXIS holds illegal complex ' //
     :                     'values.', STATUS )

*  Check that the dimensionality of the axis data array is 1. Report an
*  error if it is not.
                        ELSE IF ( NDIMD .NE. 1 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'AXIS',
     :                                   DCB_ALOC( IAX, IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIMD )
                           CALL ERR_REP( 'NDF1_DAD_NDIMD',
     :                     'The DATA_ARRAY array in the NDF axis ' //
     :                     'structure ^AXIS is ' //
     :                     '^BADNDIM-dimensional; it should be ' //
     :                     '1-dimensional.', STATUS )

*  Check that the lower and upper pixel index bounds of the axis data
*  array match the bounds of the corresponding NDF dimension. Report an
*  error if they do not.
                        ELSE IF ( ( LBNDD( 1 ) .NE. LBND( IAX ) ) .OR.
     :                            ( UBNDD( 1 ) .NE. UBND( IAX ) ) ) THEN
                           STATUS = NDF__BNDIN
                           CALL DAT_MSG( 'AXIS',
     :                                   DCB_ALOC( IAX, IDCB ) )
                           CALL MSG_SETI( 'LBNDD', LBNDD( 1 ) )
                           CALL MSG_SETI( 'UBNDD', UBNDD( 1 ) )
                           CALL MSG_SETI( 'LBND', LBND( IAX ) )
                           CALL MSG_SETI( 'UBND', UBND( IAX ) )
                           CALL ERR_REP( 'NDF1_DAD_BNDD',
     :                     'The pixel-index bounds (^LBNDD:^UBNDD) ' //
     :                     'of the DATA_ARRAY array in the NDF axis ' //
     :                     'structure ^AXIS do not match the bounds ' //
     :                     'of the corresponding NDF dimension ' //
     :                     '(^LBND:^UBND).', STATUS )
                        END IF
                     END IF

*  If an error occurred, then annul any ARY_ system identifier which may
*  have been allocated.
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ARY_ANNUL( DCB_ADID( IAX, IDCB ), STATUS )
                     END IF
                  END IF
               END IF
            END IF
         END IF

*  Set the default axis data type to be _REAL and the default axis data
*  storage form to match the NDF default storage form derived from the
*  data array.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DCB_ADTYP( IAX, IDCB ) = '_REAL'
            DCB_ADFRM( IAX, IDCB ) = DCB_DEFRM( IDCB )
         END IF

*  Note whether axis data array information is now available.
         DCB_KAD( IAX, IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DAD', STATUS )

      END
