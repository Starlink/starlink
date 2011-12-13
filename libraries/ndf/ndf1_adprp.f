      SUBROUTINE NDF1_ADPRP( IACB1, ADCPF, IDCB2, STATUS )
*+
*  Name:
*     NDF1_ADPRP

*  Purpose:
*     Propagate axis data array information from one NDF to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADPRP( IACB1, ADCPF, IDCB2, STATUS )

*  Description:
*     The routine propagates axis data array information from an
*     existing NDF to a new one which is being created. Optionally,
*     only the array attributes may be propagated, rather than their
*     values.

*  Arguments:
*     IACB1 = INTEGER (Given)
*        ACB index for the input NDF.
*     ADCPF = LOGICAL (Given)
*        Whether axis data array values are to be propagated (as opposed
*        to simply propagating their attributes).
*     IDCB2 = INTEGER (Given)
*        DCB index for the output (new) NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  If axis data array values are being propagated, then the
*     output NDF should already contain an axis structure but should
*     not contain any axis data arrays. If values are not being
*     propagated, then it should not contain an axis structure.

*  Algorithm:
*     -  Obtain an index to the input data object entry in the DCB.
*     -  Determine the bounds and number of dimensions of the input NDF
*     from the ARY_ system identifier for its data array held in the
*     ACB.
*     -  Set an initial null DCB identifier for each possible output
*     axis data array.
*     -  Loop to process each input NDF dimension.
*     -  Determine the numeric type and storage form of each input axis
*     data array, storing the results as the defaults for the new DCB
*     entry.
*     -  Convert the storage form to take account of the axis bounds if
*     necessary.
*     -  If axis data array information is to be propagated and an
*     input axis structure exists, then axis data array values must be
*     propagated to the output axis structure.
*     -  First test if the input axis data array for the current
*     dimension is absent (this may occur if a section is being
*     propagated and it has more dimensions than the associated base
*     NDF). If so, then a new output axis data arrray must be created
*     and filled with default values.
*     -  Otherwise, obtain an ARY_ system placeholder for the  new axis
*     data array in the appropriate element of the output axis
*     structure.
*     -  If the input NDF is not a section, then the axis data array
*     can simply be copied.
*     -  Otherwise, the array values must be transferred explicitly.
*     Test the storage form against each valid value and take the
*     appropriate action.
*     -  If the new array is primitive, then create it.
*     -  Map the axis data array of the input NDF section for reading
*     (this causes any necessary extrapolation of values to take
*     place), and the new array for writing.
*     -  Copy the values across and unmap the arrays when done.
*     -  If the new array is simple, then perform the equivalent
*     operations.
*     -  If an unsupported array storage form was encountered, then
*     report an error.
*     -  If an error occurred, then delete any new axis data arrays
*     which may have been created.
*     -  Note if the new DCB data array information is correct.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     22-OCT-1990 (RFWS):
*        Original version.
*     1-NOV-1990 (RFWS):
*        Improved the cleanup sequence if an error occurs.
*     25-JAN-1991 (RFWS):
*        Fixed bug which prevented an output axis data array being
*        created for dimensions which exist in an input section, but
*        not in its associated base NDF.
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
*        ) (Write)
*           Storage form of axis data arrays.
*        DCB_ADID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifiers for axis data arrays.
*        DCB_ADTYP( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP
*        ) (Write)
*           Numeric data type of axis data arrays.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.
*        DCB_KAD( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Whether information about axis data arrays is available.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB1
      LOGICAL ADCPF
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

*  Set an initial null DCB identifier for each possible output axis
*  data array.
         DO 1 IAX = 1, NDIM
            DCB_ADID( IAX, IDCB2 ) = ARY__NOID
 1       CONTINUE

*  Loop to process each input NDF dimension.
         DO 2 IAX = 1, NDIM

*  Determine the numeric type and storage form of each input axis data
*  array, storing the results as the defaults for the new DCB entry.
*  Convert the storage form to take account of the axis bounds if
*  necessary (since we may be propagating an NDF section and need to
*  accommodate the new lower bound).
            CALL NDF1_ADTYP( IAX, IACB1, DCB_ADTYP( IAX, IDCB2 ),
     :                       STATUS )
            CALL NDF1_ADFRM( IAX, IACB1, DCB_ADFRM( IAX, IDCB2 ),
     :                       STATUS )
            CALL NDF1_CBFRM( 1, LBND( IAX ), UBND( IAX ),
     :                       DCB_ADFRM( IAX, IDCB2 ), STATUS )

*  If axis data array information is to be propagated and an input axis
*  structure exists, then axis data array values must be propagated to
*  the output axis structure.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( ADCPF .AND.
     :              ( DCB_ADID( 1, IDCB1 ) .NE. ARY__NOID ) ) THEN

*  First test if the input axis data array for the current dimension is
*  absent (this may occur if a section is being propagated and it has
*  more dimensions than the associated base NDF). If so, then a new
*  output axis data arrray must be created and filled with default
*  values.
                  IF ( DCB_ADID( IAX, IDCB1 ) .EQ. ARY__NOID ) THEN
                     CALL NDF1_ADCRE( LBND( IAX ), UBND( IAX ), IAX,
     :                                IDCB2, STATUS )

*  Otherwise, obtain an ARY_ system placeholder for the  new axis data
*  array in the appropriate element of the output axis structure.
                  ELSE
                     CALL ARY_PLACE( DCB_ALOC( IAX, IDCB2 ),
     :                               'DATA_ARRAY', PLACE, STATUS )

*  If the input NDF is not a section, then the axis data array can
*  simply be copied.
                     IF ( .NOT. ACB_CUT( IACB1 ) ) THEN
                        CALL ARY_COPY( DCB_ADID( IAX, IDCB1 ), PLACE,
     :                                 DCB_ADID( IAX, IDCB2 ), STATUS )

*  Otherwise, the array values must be transferred explicitly.  Test
*  the storage form against each valid value and take the appropriate
*  action.

*  PRIMITIVE:
*  =========
*  If the new array is primitive, then create it.
                     ELSE IF ( DCB_ADFRM( IAX, IDCB2 ) .EQ.
     :                         'PRIMITIVE' ) THEN
                        CALL ARY_NEWP( DCB_ADTYP( IAX, IDCB2 ),
     :                                 1, UBND( IAX ), PLACE,
     :                                 DCB_ADID( IAX, IDCB2 ),
     :                                 STATUS )

*  Map the axis data array of the input NDF section for reading (this
*  causes any necessary extrapolation of values to take place), and the
*  new array for writing.
                        CALL NDF1_ADMAP( IAX, IACB1,
     :                                   DCB_ADTYP( IAX, IDCB2 ),
     :                                   'READ', PNTR1, EL, STATUS )
                        CALL ARY_MAP( DCB_ADID( IAX, IDCB2 ),
     :                                DCB_ADTYP( IAX, IDCB2 ),
     :                                'WRITE', PNTR2, EL, STATUS )

*  Copy the values across and unmap the arrays when done.
                        CALL NDF1_MOVE( DCB_ADTYP( IAX, IDCB2 ),
     :                                  EL, PNTR1, PNTR2, STATUS )
                        CALL NDF1_ADUMP( IAX, IACB1, STATUS )
                        CALL ARY_UNMAP( DCB_ADID( IAX, IDCB2 ), STATUS )

*  SIMPLE:
*  ======
*  If the new array is simple, then create it.
                     ELSE IF ( DCB_ADFRM( IAX, IDCB2 ) .EQ.
     :                         'SIMPLE' ) THEN
                        CALL ARY_NEW( DCB_ADTYP( IAX, IDCB2 ),
     :                                1, LBND( IAX ), UBND( IAX ),
     :                                PLACE, DCB_ADID( IAX, IDCB2 ),
     :                                STATUS )

*  Map the axis data array of the input NDF section for reading (this
*  causes any necessary extrapolation of values to take place), and the
*  new array for writing.
                        CALL NDF1_ADMAP( IAX, IACB1,
     :                                   DCB_ADTYP( IAX, IDCB2 ),
     :                                   'READ', PNTR1, EL, STATUS )
                        CALL ARY_MAP( DCB_ADID( IAX, IDCB2 ),
     :                                DCB_ADTYP( IAX, IDCB2 ),
     :                                'WRITE', PNTR2, EL, STATUS )

*  Copy the values across and unmap the arrays when done.
                        CALL NDF1_MOVE( DCB_ADTYP( IAX, IDCB2 ),
     :                                  EL, PNTR1, PNTR2, STATUS )
                        CALL NDF1_ADUMP( IAX, IACB1, STATUS )
                        CALL ARY_UNMAP( DCB_ADID( IAX, IDCB2 ), STATUS )

*  If an unsupported array storage form was encountered, then report an
*  error.
                     ELSE
                        STATUS = NDF__FATIN
                        CALL MSG_SETC( 'BADFORM',
     :                                 DCB_ADFRM( IAX, IDCB2 ) )
                        CALL ERR_REP( 'NDF1_ADPRP_FORM',
     :                                'Invalid axis array storage ' //
     :                                'form ''BADFORM'' encountered ' //
     :                                'in the NDF_ system Data ' //
     :                                'Control Block (internal ' //
     :                                'programming error).', STATUS )
                     END IF
                  END IF
               END IF
            END IF
 2       CONTINUE

*  If an error occurred, then delete any new axis data arrays which may
*  have been created.
         IF ( STATUS .NE. SAI__OK ) THEN
            DO 3 IAX = 1, NDIM
               CALL ARY_DELET( DCB_ADID( IAX, IDCB2 ), STATUS )
 3          CONTINUE
         END IF

*  Note if the new DCB data array information is correct.
         DO 4 IAX = 1, NDIM
            DCB_KAD( IAX, IDCB2 ) = STATUS .EQ. SAI__OK
 4       CONTINUE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADPRP', STATUS )

      END
