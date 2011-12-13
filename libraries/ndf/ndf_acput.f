      SUBROUTINE NDF_ACPUT( VALUE, INDF, COMP, IAXIS, STATUS )
*+
*  Name:
*     NDF_ACPUT

*  Purpose:
*     Assign a value to an NDF axis character component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ACPUT( VALUE, INDF, COMP, IAXIS, STATUS )

*  Description:
*     The routine assigns a value to the specified axis character
*     component of an NDF (i.e. to the LABEL or UNITS component of an
*     NDF axis).

*  Arguments:
*     VALUE = CHARACTER * ( * ) (Given)
*        The value to be assigned.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the axis character component whose value is to be
*        assigned: 'LABEL' or 'UNITS'.
*     IAXIS = INTEGER (Given)
*        Number of the axis to receive the new value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The entire VALUE string (including trailing blanks if present)
*     is assigned to the specified axis component, whose length is
*     adjusted to accommodate it.
*     -  A value of zero may be given for the IAXIS argument, in which
*     case the routine will assign the same value to all the NDF axes.
*     -  This routine may only be used to assign values to the axes of
*     a base NDF. If an NDF section is supplied, then it it will return
*     without action. No error will result.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Validate the axis character component name.
*     -  Validate the axis number.
*     -  Check that write access to the NDF is available.
*     -  Check that this is a base NDF. Return without action if it is
*     not.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Ensure that an axis structure exists.
*     -  Loop to assign character values to each specified axis.
*     -  Ensure that axis character component information is available.
*     -  If the required component already exists, then determine its
*     length.
*     -  If the length does not match that of the value to be assigned,
*     then annul the component's locator and erase the component.
*     -  If the required component does not (now) exist, then create
*     one of the correct length.
*     -  Obtain a locator to the component for storage in the DCB.
*     -  Assign the new component value.
*     -  Quit processing axes if an error occurs.
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
*     4-JUL-1990 (RFWS):
*        Original version.
*     15-OCT-1990 (RFWS):
*        Changed to pass a DCB index to NDF1_ACRE instead of an ACB
*        index.
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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ACCN( NDF__MXACN ) = CHARACTER * ( DAT__SZNAM ) (Read)
*           Axis character component names.
*        DCB_ACLOC( NDC__MXDIM, NDF__MXACN, NDF__MXDCB ) = CHARACTER * (
*        DAT__SZLOC ) (Read and Write)
*           Locators to axis character components.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MADCB ) = LOGICAL (Read )
*           Whether the NDF is a cut (i.e. section).
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) VALUE
      INTEGER INDF
      CHARACTER * ( * ) COMP
      INTEGER IAXIS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IAX                ! Loop counter for axes
      INTEGER IAX1               ! First axis to process
      INTEGER IAX2               ! Last axis to process
      INTEGER ICCOMP             ! Axis character component identifier
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER L                  ! Character component length

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Validate the axis character component name.
      CALL NDF1_VACCN( COMP, ICCOMP, STATUS )

*  Validate the axis number.
      CALL NDF1_VAN( IACB, IAXIS, .TRUE., IAX1, IAX2, STATUS )

*  Check that write access to the NDF is available.
      CALL NDF1_CHACC( IACB, 'WRITE', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that this is a base NDF. Return without action if it is not.
         IF ( .NOT. ACB_CUT( IACB ) ) THEN

*  Obtain an index to the data object entry in the DCB.
            IDCB = ACB_IDCB( IACB )

*  Ensure that an axis structure exists.
            CALL NDF1_ACRE( IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop to assign character values to each specified axis.
               DO 1 IAX = IAX1, IAX2

*  Ensure that axis character component information is available.
                  CALL NDF1_DAC( IAX, ICCOMP, IDCB, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If the required component already exists, then determine its length.
                     IF( DCB_ACLOC( IAX, ICCOMP, IDCB ) .NE.
     :                   DAT__NOLOC ) THEN
                        CALL DAT_LEN( DCB_ACLOC( IAX, ICCOMP, IDCB ),
     :                                L, STATUS )
                        IF ( STATUS .EQ. SAI__OK ) THEN

*  If the length does not match that of the value to be assigned, then
*  annul the component's locator and erase the component.
                           IF ( L .NE. LEN( VALUE ) ) THEN
                              CALL DAT_ANNUL( DCB_ACLOC( IAX, ICCOMP,
     :                                                   IDCB ),
     :                                        STATUS )
                              CALL DAT_ERASE( DCB_ALOC( IAX, IDCB ),
     :                                        DCB_ACCN( ICCOMP ),
     :                                        STATUS )
                           END IF
                        END IF
                     END IF

*  If the required component does not (now) exist, then create one of
*  the correct length.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( DCB_ACLOC( IAX, ICCOMP, IDCB ) .EQ.
     :                       DAT__NOLOC ) THEN
                           CALL DAT_NEW0C( DCB_ALOC( IAX, IDCB ),
     :                                     DCB_ACCN( ICCOMP ),
     :                                     LEN( VALUE ), STATUS )

*  Obtain a locator to the component for storage in the DCB.
                           CALL DAT_FIND( DCB_ALOC( IAX, IDCB ),
     :                                    DCB_ACCN( ICCOMP ),
     :                                    DCB_ACLOC( IAX, ICCOMP,
     :                                               IDCB ),
     :                                    STATUS )
                        END IF

*  Assign the new component value.
                        CALL DAT_PUT0C( DCB_ACLOC( IAX, ICCOMP, IDCB ),
     :                                  VALUE, STATUS )
                     END IF
                  END IF

*  Quit processing axes if an error occurs.
                  IF ( STATUS .NE. SAI__OK ) GO TO 2
1              CONTINUE
2              CONTINUE
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ACPUT_ERR',
     :   'NDF_ACPUT: Error assigning a new value to an NDF axis ' //
     :   'character component.', STATUS )
         CALL NDF1_TRACE( 'NDF_ACPUT', STATUS )
      END IF

      END
