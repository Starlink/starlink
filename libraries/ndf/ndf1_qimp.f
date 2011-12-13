      SUBROUTINE NDF1_QIMP( IACB, STATUS )
*+
*  Name:
*     NDF1_QIMP

*  Purpose:
*     Import information about an NDF's quality component into the ACB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_QIMP( IACB, STATUS )

*  Description:
*     The routine imports information about an NDF's quality component
*     so that it is available in the ACB. No action is taken if this
*     information is already available. Otherwise, DCB quality
*     information is first acquired. Then, if the quaility array
*     exists, ARY_ system identifiers for relevant sections of it are
*     entered into all the ACB entries which refer to that NDF data
*     object. If the quality array does not exist, then null
*     identifiers (value ARY__NOID) are entered instead. The NDF is
*     identified to this routine by its ACB entry.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF's ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine should normally be used instead of a direct call
*     to NDF1_DQ followed by updating of a single ACB entry.  It
*     ensures that all the ACB entries which refer to the quality
*     object are updated simultaneously so that their state reflects
*     that of the actual data object.

*  Algorithm:
*     -  Obtain an index to the data object entry in the DCB.
*     -  Check if the DCB already contains quality information. If so,
*     then there is nothing to do.
*     -  Ensure that the DCB does contain quality information.
*     -  See if the quality array identifier in the DCB is valid. If
*     not, then the component does not exist.
*     -  Loop to identify all the ACB entries which refer to the data
*     object, selecting those with the correct DCB index.
*     -  If the quality component exists, then see if the ACB quality
*     array identifier is already valid. If not, then create a section
*     of the quality array which matches the ACB's data array section
*     and enter the resulting ARY_ system identifier into the ACB.
*     -  Otherwise, enter a null identifier.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     29-JAN-1990 (RFWS):
*        Original, derived from the NDF1_VIMP routine.
*     20-JAN-1999 (RFWS):
*        Call NDF1_SSDUP to eliminate problems with ARY_SSECT when the
*        dimensionality of a section differs from that of the base
*        array.
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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_KQ( NDF__MXDCB ) = LOGICAL (Read)
*           Whether information about the NDF's quality component is
*           available in the DCB.
*        DCB_QID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's quality array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_QID( NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the NDF's quality array.

*  Arguments Given:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACBT              ! ACB entry to test
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IDCBT              ! DCB index to test
      INTEGER NEXT               ! Next ACB entry to test
      LOGICAL THERE              ! Whether the quality array exists
      LOGICAL VALID              ! Whether ACB array identifier is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  Check if the DCB already contains quality information. If so, then
*  there is nothing to do.
      IF ( .NOT. DCB_KQ( IDCB ) ) THEN

*  Ensure that the DCB does contain quality information.
         CALL NDF1_DQ( IDCB, STATUS )

*  See if the quality array identifier in the DCB is valid. If not,
*  then the component does not exist.
         CALL ARY_VALID( DCB_QID( IDCB ), THERE, STATUS )

*  Loop to identify all the ACB entries which refer to this data object.
         NEXT = 0
         IACBT = 0
1        CONTINUE                ! Start of 'DO WHILE' loop
         CALL NDF1_NXTSL( NDF__ACB, IACBT, NEXT, STATUS )
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN
            IACBT = NEXT

*  Select those with the correct DCB index.
            IDCBT = ACB_IDCB( IACBT )
            IF ( IDCBT .EQ. IDCB ) THEN

*  If the quality component exists, then see if the ACB quality array
*  identifier is already valid.
               IF ( THERE ) THEN
                  CALL ARY_VALID( ACB_QID( IACBT ), VALID, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If not, then create a section of the quality array to match the
*  ACB's data array section and enter the resulting ARY_ system
*  identifier into the ACB.
                     IF ( .NOT. VALID ) THEN
                        CALL NDF1_SSDUP( DCB_QID( IDCB ),
     :                                   ACB_DID( IACBT ),
     :                                   ACB_QID( IACBT ), STATUS )
                     END IF
                  END IF

*  Otherwise, enter a null identifier.
               ELSE
                  ACB_QID( IACBT ) = ARY__NOID
               END IF
            END IF
            GO TO 1
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_QIMP', STATUS )

      END
