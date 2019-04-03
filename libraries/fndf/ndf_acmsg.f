      SUBROUTINE NDF_ACMSG( TOKEN, INDF, COMP, IAXIS, STATUS )
*+
*  Name:
*     NDF_ACMSG

*  Purpose:
*     Assign the value of an NDF axis character component to a message
*     token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ACMSG( TOKEN, INDF, COMP, IAXIS, STATUS )

*  Description:
*     The routine assigns the value of the specified axis character
*     component of an NDF to a message token, for use in constructing
*     messages using the MSG_ or ERR_ routines (see SUN/104).

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        Name of the message token.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the axis character component whose value is to be used:
*        'LABEL' or 'UNITS'.
*     IAXIS = INTEGER (Given)
*        Number of the NDF axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the requested axis component is in an undefined state, then
*     an appropriate default value will be assigned to the token.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Validate the axis character component name.
*     -  Validate the axis number.
*     -  Obtain an index to the data object entry in the DCB.
*     -  If this is an NDF section, then obtain the number of
*     dimensions of the actual data object to which it refers from the
*     ARY_ system identifier for its data array.
*     -  Note if the required axis exists in the actual data object.
*     -  If the required axis exists, then ensure that axis character
*     component information is available.
*     -  Note whether the required character component exists.
*     -  If the component (or its axis) does not exist, then create a
*     default value.
*     -  Assign the default value.
*     -  If the required component exists, then map it for reading.
*     -  Assign the mapped value to the message token.
*     -  Unmap the component.
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
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
*     4-JUL-1990 (RFWS):
*        Original version.
*     11-FEB-1992 (RFWS):
*        Added length value when passing mapped character value (for
*        UNIX compatibility).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
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
      CHARACTER * ( * ) TOKEN
      INTEGER INDF
      CHARACTER * ( * ) COMP
      INTEGER IAXIS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 5 + VAL__SZI ) VAL ! Default value string
      INTEGER DIM( 1 )           ! Component dimension array
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER IAX1               ! First (only) axis to process
      INTEGER IAX2               ! Last (only) axis to process
      INTEGER ICCOMP             ! Character component identifier
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER LBND( NDF__MXDIM ) ! Data object lower bounds
      INTEGER LENC               ! Langth of mapped character value
      INTEGER NC                 ! Default value string length
      INTEGER NDIM               ! Number of data object dimensions
      INTEGER PNTR               ! Pointer to mapped component
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

*  If the component (or its axis) does not exist, then create a default
*  value.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( .NOT. THERE ) THEN

*  The label component defaults to 'Axis n'.
               IF ( ICCOMP .EQ. NDF__ALAB ) THEN
                  VAL = 'Axis'
                  NC = 5
                  CALL CHR_PUTI( IAX1, VAL, NC )

*  The units component defaults to 'pixel'.
               ELSE IF ( ICCOMP .EQ. NDF__AUNI ) THEN
                  VAL = 'pixel'
                  NC = 6
               END IF

*  Assign the default value.
               CALL MSG_SETC( TOKEN, VAL( : NC ) )

*  If the required component exists, then map it for reading and
*  determine its length.
            ELSE
               DIM( 1 ) = 0
               CALL DAT_MAPC( DCB_ACLOC( IAX1, ICCOMP, IDCB ), 'READ',
     :                        0, DIM, PNTR, STATUS )

               CALL DAT_CLEN( DCB_ACLOC( IAX1, ICCOMP, IDCB ), LENC,
     :                        STATUS )

*  Assign the mapped value to the message token.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL NDF1_SETC( %VAL( CNF_PVAL( PNTR ) ), TOKEN,
     :                            %VAL( CNF_CVAL( LENC ) ) )
               END IF

*  Unmap the component.
               CALL NDF1_HUNMP( DCB_ACLOC( IAX1, ICCOMP, IDCB ),
     :                          STATUS )
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ACMSG_ERR',
     :   'NDF_ACMSG: Error assigning the value of an NDF axis ' //
     :   'character component to a message token.', STATUS )
         CALL NDF1_TRACE( 'NDF_ACMSG', STATUS )
      END IF

      END
