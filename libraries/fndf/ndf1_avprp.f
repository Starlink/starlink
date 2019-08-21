      SUBROUTINE NDF1_AVPRP( IACB1, ACPF, IDCB2, STATUS )
*+
*  Name:
*     NDF1_AVPRP

*  Purpose:
*     Propagate axis variance array information from one NDF to
*     another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AVPRP( IACB1, ACPF, IDCB2, STATUS )

*  Description:
*     The routine propagates axis variance array information from an
*     existing NDF to a new one which is being created. Optionally,
*     only the array attributes may be propagated, rather than their
*     values.

*  Arguments:
*     IACB1 = INTEGER (Given)
*        ACB index for the input NDF.
*     ACPF = LOGICAL (Given)
*        Whether axis variance array values are to be propagated (as
*        opposed to simply propagating their attributes).
*     IDCB2 = INTEGER (Given)
*        DCB index for the output (new) NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  If axis variance array values are being propagated, then the
*     output NDF should already contain an axis structure but should
*     not contain any axis variance arrays.

*  Algorithm:
*     -  Obtain indices to the data object entries in the DCB.
*     -  Determine the bounds and number of dimensions of the input NDF
*     from the ARY_ system identifier for its data array held in the
*     ACB.
*     -  Loop to process each input NDF dimension.
*     -  Set an initial null axis variance array identifier in the
*     output DCB entry.
*     -  Determine the storage form and numeric type of each input axis
*     variance array, storing the results as the defaults for the new
*     DCB entry.
*     -  Convert the storage form to take account of the axis bounds if
*     necessary.
*     -  If axis variance array information is to be propagated and the
*     current axis lies inside the dimensionality of the input NDF and
*     an input axis variance array exists, then obtain an ARY_ system
*     placeholder for a new axis variance array in the appropriate
*     element of the output axis structure.
*     -  If the input NDF is not a section, then the axis variance
*     array can simply be copied.
*     -  Otherwise, the array values must be transferred explicitly.
*     Test the storage form against each valid value and take the
*     appropriate action.
*     -  If the new array is primitive, then create it.
*     -  Map the axis variance array of the input NDF section for
*     reading (this causes any necessary extrapolation of values to
*     take place), and the new array for writing.
*     -  Copy the values across and unmap the arrays when done.
*     -  Perform the equivqlent operation for simple arrays.
*     -  If an unsupported array storage form was encountered, then
*     report an error.
*     -  If an error occurred, then delete any new axis variance array
*     which may have been created.
*     -  Note if the DCB axis variance array information is correct.

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
*        Original version, derived from the NDF1_ADPRP routine.
*     29-NOV-1990 (RFWS):
*        Changed so that only the default attributes of the input NDF's
*        axes are propagated, rather than those for all possible NDF
*        axes.
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
*        ) (Write)
*           Storage form of axis variance arrays.
*        DCB_AVID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifiers for axis variance arrays.
*        DCB_AVTYP( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP
*        ) (Write)
*           Numeric data type of axis variance arrays.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.
*        DCB_KAV( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Whether information about axis variance arrays is
*           available.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB1
      LOGICAL ACPF
      INTEGER IDCB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EL                 ! Number of mapped elements
      INTEGER IAX                ! Loop counter for axes
      INTEGER IDCB1              ! DCB index of input data object
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER PLACE              ! ARY_ placeholder for new axis array
      INTEGER PNTR1              ! Pointer to mapped input values
      INTEGER PNTR2              ! Pointer to mapped output values
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the input data object entry in the DCB.
      IDCB1 = ACB_IDCB( IACB1 )

*  Determine the bounds and number of dimensions of the input NDF from
*  the ARY_ system identifier for its data array held in the ACB.
      CALL ARY_BOUND( ACB_DID( IACB1 ), NDF__MXDIM, LBND, UBND, NDIM,
     :                STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop to process each input NDF dimension.
         DO 1 IAX = 1, NDIM

*  Set an initial null axis variance array identifier in the output DCB
*  entry.
            DCB_AVID( IAX, IDCB2 ) = ARY__NOID

*  Determine the numeric type and storage form of each input axis
*  variance array, storing the results as the defaults for the new DCB
*  entry.  Convert the storage form to take account of the axis bounds
*  if necessary (since we may be propagating an NDF section and must
*  accommodate the new lower bounds).
            CALL NDF1_AVTYP( IAX, IACB1, DCB_AVTYP( IAX, IDCB2 ),
     :                       STATUS )
            CALL NDF1_AVFRM( IAX, IACB1, DCB_AVFRM( IAX, IDCB2 ),
     :                       STATUS )
            CALL NDF1_CBFRM( 1, LBND( IAX ), UBND( IAX ),
     :                       DCB_AVFRM( IAX, IDCB2 ), STATUS )

*  If axis variance array information is to be propagated and an input
*  axis variance array exists, then obtain an ARY_ system placeholder
*  for a new axis variance array in the appropriate element of the
*  output axis structure.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( ACPF .AND.
     :              ( DCB_AVID( IAX, IDCB1 ) .NE. ARY__NOID ) ) THEN
                  CALL ARY_PLACE( DCB_ALOC( IAX, IDCB2 ), 'VARIANCE',
     :                            PLACE, STATUS )

*  If the input NDF is not a section, then the axis variance array can
*  simply be copied.
                  IF ( .NOT. ACB_CUT( IACB1 ) ) THEN
                     CALL ARY_COPY( DCB_AVID( IAX, IDCB1 ), PLACE,
     :                              DCB_AVID( IAX, IDCB2 ), STATUS )

*  Otherwise, the array values must be transferred explicitly.  Test
*  the storage form against each valid value and take the appropriate
*  action.

*  PRIMITIVE:
*  =========
*  If the new array is primitive, then create it.
                  ELSE IF ( DCB_AVFRM( IAX, IDCB2 ) .EQ.
     :                      'PRIMITIVE' ) THEN
                     CALL ARY_NEWP( DCB_AVTYP( IAX, IDCB2 ),
     :                              1, UBND( IAX ), PLACE,
     :                              DCB_AVID( IAX, IDCB2 ),
     :                              STATUS )

*  Map the axis variance array of the input NDF section for reading
*  (this causes any necessary extrapolation of values to take place),
*  and the new array for writing.
                     CALL NDF1_AVMAP( IAX, IACB1,
     :                                DCB_AVTYP( IAX, IDCB2 ),
     :                                'READ', .FALSE., PNTR1, EL,
     :                                STATUS )
                     CALL ARY_MAP( DCB_AVID( IAX, IDCB2 ),
     :                             DCB_AVTYP( IAX, IDCB2 ),
     :                             'WRITE', PNTR2, EL, STATUS )

*  Copy the values across and unmap the arrays when done.
                     CALL NDF1_MOVE( DCB_AVTYP( IAX, IDCB2 ),
     :                               EL, PNTR1, PNTR2, STATUS )
                     CALL NDF1_AVUMP( IAX, IACB1, STATUS )
                     CALL ARY_UNMAP( DCB_AVID( IAX, IDCB2 ), STATUS )

*  SIMPLE:
*  ======
*  If the new array is simple, then create it.
                  ELSE IF ( DCB_AVFRM( IAX, IDCB2 ) .EQ. 'SIMPLE' ) THEN
                     CALL ARY_NEW( DCB_AVTYP( IAX, IDCB2 ),
     :                             1, LBND( IAX ), UBND( IAX ), PLACE,
     :                             DCB_AVID( IAX, IDCB2 ),
     :                             STATUS )

*  Map the axis variance array of the input NDF section for reading
*  (this causes any necessary extrapolation of values to take place),
*  and the new array for writing.
                     CALL NDF1_AVMAP( IAX, IACB1,
     :                                DCB_AVTYP( IAX, IDCB2 ),
     :                                'READ', .FALSE., PNTR1, EL,
     :                                STATUS )
                     CALL ARY_MAP( DCB_AVID( IAX, IDCB2 ),
     :                             DCB_AVTYP( IAX, IDCB2 ),
     :                             'WRITE', PNTR2, EL, STATUS )

*  Copy the values across and unmap the arrays when done.
                     CALL NDF1_MOVE( DCB_AVTYP( IAX, IDCB2 ),
     :                               EL, PNTR1, PNTR2, STATUS )
                     CALL NDF1_AVUMP( IAX, IACB1, STATUS )
                     CALL ARY_UNMAP( DCB_AVID( IAX, IDCB2 ), STATUS )

*  If an unsupported array storage form was encountered, then report an
*  error.
                  ELSE
                     STATUS = NDF__FATIN
                     CALL MSG_SETC( 'BADFORM', DCB_AVFRM( IAX, IDCB2 ) )
                     CALL ERR_REP( 'NDF1_AVPRP_FORM',
     :                             'Invalid axis array storage ' //
     :                             'form ''BADFORM'' encountered ' //
     :                             'in the NDF_ system Data Control ' //
     :                             'Block (internal programming ' //
     :                             'error).', STATUS )
                  END IF
               END IF
            END IF

*  If an error occurred, then delete any new axis variance array which
*  may have been created.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ARY_DELET( DCB_AVID( IAX, IDCB2 ), STATUS )
            END IF

*  Note if the DCB axis variance array information is correct.
            DCB_KAV( IAX, IDCB2 ) = STATUS .EQ. SAI__OK
 1       CONTINUE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AVPRP', STATUS )

      END
