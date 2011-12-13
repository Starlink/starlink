      SUBROUTINE NDF_ACGET( INDF, COMP, IAXIS, VALUE, STATUS )
*+
*  Name:
*     NDF_ACGET

*  Purpose:
*     Obtain the value of an NDF axis character component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ACGET( INDF, COMP, IAXIS, VALUE, STATUS )

*  Description:
*     The routine obtains the value of the specified axis character
*     component of an NDF (i.e. the value of the LABEL or UNITS
*     component for an NDF axis).

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the axis character component whose value is required:
*        'LABEL' or 'UNITS'.
*     IAXIS = INTEGER (Given)
*        Number of the axis for which a value is required.
*     VALUE = CHARACTER * ( * ) (Given and Returned)
*        The component's value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the requested axis component is in an undefined state and
*     VALUE is set to a blank string on entry, then an appropriate
*     default value will be returned. If VALUE is not blank on entry,
*     then it will be returned unchanged.
*     -  If the length of the VALUE argument is too short to
*     accommodate the returned result without losing significant
*     (non-blank) trailing characters, then this will be indicated by
*     an appended ellipsis, i.e. '...'. No error will result.

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

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  History:
*     4-JUL-1990 (RFWS):
*        Original version.
*     4-DEC-1991 (RFWS):
*        Changed DAT__TRUNC to DAT__CONER to reflect changes in HDS
*        behaviour.
*     14-OCT-1992 (RFWS):
*        Re-instated test for DAT__TRUNC as well, since it may still
*        occur.
*     7-SEP-1993 (RFWS):
*        Fixed wrong length used when returning units of 'pixel'.
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
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'DAT_ERR'          ! HDS error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ACLOC( NDC__MXDIM, NDF__MXACN, NDF__MXDCB ) = CHARACTER * (
*        DAT__SZLOC ) (Read)
*           Locators to axis character components.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF is a cut (i.e. section).
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP
      INTEGER IAXIS

*  Arguments Given and Returned:
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 5 + VAL__SZI ) VAL ! Default value string
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER IAX1               ! First (only) axis to process
      INTEGER IAX2               ! Last (only) axis to process
      INTEGER ICCOMP             ! Character component identifier
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER LBND( NDF__MXDIM ) ! Data object lower bounds
      INTEGER N                  ! Position of start of ellipsis
      INTEGER NC                 ! Default value string length
      INTEGER NDIM               ! Number of data object dimensions
      INTEGER UBND( NDF__MXDIM ) ! Data object upper bounds
      LOGICAL THERE              ! Whether component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Validate the axis character component name.
      CALL NDF1_VACCN( COMP, ICCOMP, STATUS )

*  Validate the axis number.
      CALL NDF1_VAN( IACB, IAXIS, .FALSE., IAX1, IAX2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  If this is an NDF section, then obtain the number of dimensions of
*  the actual data object to which it refers from the ARY_ system
*  identifier for its data array.
         THERE = .TRUE.
         IF ( ACB_CUT( IACB ) ) THEN
            CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBND, UBND,
     :                      NDIM, STATUS )

*  Note if the required axis exists in the actual data object.
            IF ( STATUS .EQ. SAI__OK ) THEN
               THERE = IAX1 .LE. NDIM
            END IF
         END IF

*  If the required axis exists, then ensure that axis character
*  component information is available.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( THERE ) THEN
               CALL NDF1_DAC( IAX1, ICCOMP, IDCB, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Note whether the required character component exists.
                  THERE = DCB_ACLOC( IAX1, ICCOMP, IDCB ) .NE.
     :                    DAT__NOLOC
               END IF
            END IF
         END IF

*  If the component (or its axis) does not exist and the VALUE argument
*  was blank on input, then create a default value.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( .NOT. THERE ) THEN
               IF ( VALUE .EQ. ' ' ) THEN

*  The label component defaults to 'Axis n'.
                  IF ( ICCOMP .EQ. NDF__ALAB ) THEN
                     VAL = 'Axis'
                     NC = 5
                     CALL CHR_PUTI( IAX1, VAL, NC )

*  The units component defaults to 'pixel'.
                  ELSE IF ( ICCOMP .EQ. NDF__AUNI ) THEN
                     VAL = 'pixel'
                     NC = 5
                  END IF

*  Return the default value, adding an ellipsis if it is truncated.
                  VALUE = VAL( : NC )
                  IF ( NC .GT. LEN( VALUE ) ) THEN
                     N = MAX( 1, LEN( VALUE ) - 2 )
                     VALUE( N : ) = '...'
                  END IF
               END IF

*  If the required component exists, then start a new error context and
*  read its value.
            ELSE
               CALL ERR_MARK
               CALL DAT_GET0C( DCB_ACLOC( IAX1, ICCOMP, IDCB ),
     :                         VALUE, STATUS )

*  If the value was truncated, then annul the error and add an ellipsis.
               IF ( ( STATUS .EQ. DAT__CONER ) .OR.
     :              ( STATUS .EQ. DAT__TRUNC ) ) THEN
                  CALL ERR_ANNUL( STATUS )
                  N = MAX( 1, LEN( VALUE ) - 2 )
                  VALUE( N : ) = '...'
               END IF

*  End the error context.
               CALL ERR_RLSE
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ACGET_ERR',
     :   'NDF_ACGET: Error obtaining the value of an NDF axis ' //
     :   'character component.', STATUS )
         CALL NDF1_TRACE( 'NDF_ACGET', STATUS )
      END IF

      END
