      SUBROUTINE NDF_ANORM( INDF, IAXIS, NORM, STATUS )
*+
*  Name:
*     NDF_ANORM

*  Purpose:
*     Obtain the logical value of an NDF axis normalisation flag.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ANORM( INDF, IAXIS, NORM, STATUS )

*  Description:
*     The routine returns a logical value for the normalisation flag
*     associated with an NDF axis.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     IAXIS = INTEGER (Given)
*        Number of the axis whose normalisation flag value is required.
*     NORM = LOGICAL (Returned)
*        Normalisation flag value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A value of zero may be supplied for the IAXIS argument, in
*     which case the routine will return the logical "OR" of the
*     normalisation flag values for all the NDF's axes.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Check the axis number for validity.
*     -  Obtain an index to the data object entry in the DCB and
*     initialise the returned result.
*     -  Loop to inspect each requested axis, ensuring that
*     normalisation information is available in the DCB.
*     -  Set the returned result to .TRUE. and quit checking when the
*     first .TRUE. normalisation value is obtained.
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
*     4-DEC-1990 (RFWS):
*        Changed to return the logical OR of the results for all axes
*        if an axis number of zero is supplied.
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
*        DCB_ANRM( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read)
*           Axis normalisation value.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      INTEGER IAXIS

*  Arguments Returned:
      LOGICAL NORM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER IAX                ! Loop counter for axes
      INTEGER IAX1               ! First axis to process
      INTEGER IAX2               ! Last axis to process
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check the axis number for validity.
      CALL NDF1_VAN( IACB, IAXIS, .TRUE., IAX1, IAX2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB and initialise
*  the returned result.
         IDCB = ACB_IDCB( IACB )
         NORM = .FALSE.

*  Loop to inspect each requested axis, ensuring that normalisation
*  information is available in the DCB.
         DO 1 IAX = IAX1, IAX2
            CALL NDF1_DAN( IAX, IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Set the returned result to .TRUE. and quit checking when the first
*  .TRUE. normalisation value is obtained.
               IF ( DCB_ANRM( IAX, IDCB ) ) THEN
                  NORM = .TRUE.
                  GO TO 2
               END IF
            END IF
 1       CONTINUE
 2       CONTINUE
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ANORM_ERR',
     :   'NDF_ANORM: Error obtaining the logical value of an NDF ' //
     :   'axis normalisation flag.',STATUS )
         CALL NDF1_TRACE( 'NDF_ANORM', STATUS )
      END IF

      END
