      SUBROUTINE NDF_ASNRM( NORM, INDF, IAXIS, STATUS )
*+
*  Name:
*     NDF_ASNRM

*  Purpose:
*     Set a new logical value for an NDF axis normalisation flag.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ASNRM( NORM, INDF, IAXIS, STATUS )

*  Description:
*     The routine sets a new logical value for the normalisation flag
*     associated with an NDF axis.

*  Arguments:
*     NORM = LOGICAL (Given)
*        Normalisation flag value to be set.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     IAXIS = INTEGER (Given)
*        Number of the NDF axis whose normalisation flag value is to be
*        set.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A value of zero may be supplied for the IAXIS component, in
*     which case the routine will set the same normalisation flag value
*     for all the NDF's axes.
*     -  This routine may only be used to set an axis normalisation
*     flag value for a base NDF. If an NDF section is supplied, then it
*     will return without action. No error will result.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Check the axis number for validity.
*     -  Check that write access to the NDF is available.
*     -  Check that the NDF is not a section. Return without action if
*     it is.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Loop to process each relevant axis.
*     -  Ensure that axis normalisation flag information is available
*     in the DCB.
*     -  See if the new flag value differs from its current value. If
*     not, then there is nothing to do.
*     -  If an axis structure exists, then see if it contains a
*     NORMALISED component.
*     -  If the new normalisation value is .TRUE., then create a
*     NORMALISED component if it does not already exist and write a
*     .TRUE. value to it.
*     -  If the normalisation value is .FALSE., then erase any
*     NORMALISED component which may exist.
*     -  Store the new normalisation value in the DCB and note whether
*     this DCB value is now up to date.
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
*     5-JUL-1990 (RFWS):
*        Original version.
*     1-AUG-1990 (RFWS):
*        Added check on write access being available. Also changed to
*        note whether Data Control Block information is up to date
*        after a new normalisation flag value is set.
*     15-OCT-1990 (RFWS):
*        Changed to pass a DCB index to NDF1_ACRE instead of an ACB
*        index.
*     29-NOV-1990 (RFWS):
*        Changed so that a new axis structure is not created if it
*        doesn't already exist.
*     4-DEC-1990 (RFWS):
*        Improved the creation of the new NORMALISED component.
*     18-DEC-1990 (RFWS):
*        Improved test for axis structure existence.
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
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.
*        DCB_ANRM( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read and Write)
*           Axis normalisation value.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      LOGICAL NORM
      INTEGER INDF
      INTEGER IAXIS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER IAX                ! Loop counter for axes
      INTEGER IAX1               ! First axis to process
      INTEGER IAX2               ! Last axis to process
      INTEGER IDCB               ! Index to data object entry in the DCB
      LOGICAL THERE              ! Whether component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check the axis number for validity.
      CALL NDF1_VAN( IACB, IAXIS, .TRUE., IAX1, IAX2, STATUS )

*  Check that write access to the NDF is available.
      CALL NDF1_CHACC( IACB, 'WRITE', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the NDF is not a section. Return without action if it is.
         IF ( .NOT. ACB_CUT( IACB ) ) THEN

*  Obtain an index to the data object entry in the DCB.
            IDCB = ACB_IDCB( IACB )

*  Loop to process each relevant axis.
            DO 1 IAX = IAX1, IAX2

*  Ensure that axis normalisation flag information is available in the
*  DCB.
               CALL NDF1_DAN( IAX, IDCB, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  See if the new flag value differs from its current value. If not,
*  then there is nothing to do.
                  IF ( NORM .NEQV. DCB_ANRM( IAX, IDCB ) ) THEN

*  If an axis structure exists, then see if it contains a NORMALISED
*  component.
                     IF ( DCB_ALOC( IAX, IDCB ) .NE. DAT__NOLOC ) THEN
                        CALL DAT_THERE( DCB_ALOC( IAX, IDCB ),
     :                                  'NORMALISED', THERE, STATUS )
                        IF ( STATUS .EQ. SAI__OK ) THEN

*  If the new normalisation value is .TRUE., then create a NORMALISED
*  component if it does not already exist and write a .TRUE. value to
*  it.
                           IF ( NORM ) THEN
                              IF ( .NOT. THERE ) THEN
                                 CALL DAT_NEW0L( DCB_ALOC( IAX, IDCB ),
     :                                           'NORMALISED', STATUS )
                              END IF
                              CALL CMP_PUT0L( DCB_ALOC( IAX, IDCB ),
     :                                        'NORMALISED', NORM,
     :                                        STATUS )

*  If the normalisation value is .FALSE., then erase any NORMALISED
*  component which may exist.
                           ELSE
                              IF ( THERE ) THEN
                                 CALL DAT_ERASE( DCB_ALOC( IAX, IDCB ),
     :                                           'NORMALISED', STATUS )
                              END IF
                           END IF
                        END IF
                     END IF

*  Store the new normalisation value in the DCB and note whether this
*  DCB value is now up to date.
                     DCB_ANRM( IAX, IDCB ) = NORM
                     DCB_KAN( IAX, IDCB ) = STATUS .EQ. SAI__OK
                  END IF
               END IF
 1          CONTINUE
 2          CONTINUE
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ASNRM_ERR',
     :   'NDF_ASNRM: Error setting a new logical value for an NDF ' //
     :   'axis normalisation flag.', STATUS )
         CALL NDF1_TRACE( 'NDF_ASNRM', STATUS )
      END IF

      END
