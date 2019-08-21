      SUBROUTINE NDF1_QRST( IACB, STATUS )
*+
*  Name:
*     NDF1_QRST

*  Purpose:
*     Reset the quality component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_QRST( IACB, STATUS )

*  Description:
*     The routine sets the quality component of an NDF into the
*     "undefined" state. No action is taken if it is already in this
*     state or if the specified NDF is not a base NDF.  The NDF is
*     identified by its ACB entry.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF's ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Resetting this component involves the deletion of the
*     associated array (if it exists).

*  Algorithm:
*     -  Ensure that quality information is available in the DCB and
*     ACB.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Check that the quality component is not mapped through the
*     current ACB entry. Report an error if it is.
*     -  Only take further action if this is a base NDF.  Check that
*     the quality component is not mapped at all. Report an error if it
*     is.
*     -  See if the DCB quality array identifier is valid. If not, then
*     the component is already undefined.
*     -  If it is defined, then obtain its storage form and save this
*     in the DCB for use if the array is later re-created.
*     -  Delete the quality array.

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
*     29-JAN-1990 (RFWS):
*        Original, derived from the NDF1_VRST routine.
*     21-MAR-1990 (RFWS):
*        Added full checks on whether the quality component is mapped
*        for access.
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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_NQMAP( NDF__MXDCB ) = INTEGER (Read)
*           Number of mappings to the NDF's quality component.
*        DCB_QFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Write)
*           The default form of array used to store data in the NDF's
*           quality component.
*        DCB_QID( NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the NDF's quality array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF_MMXACB ) = LOGICAL (Read)
*           Whether the NDF is a cut (i.e. section).
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_QMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's quality array is mapped for access.

*  Arguments Given:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB
      LOGICAL VALID              ! Whether quality array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that quality information is available in the DCB and ACB.
      CALL NDF1_QIMP( IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Check that the quality component is not mapped through the current
*  ACB entry. Report an error if it is.
         IF ( ACB_QMAP( IACB ) ) THEN
            STATUS = NDF__ISMAP
            CALL NDF1_AMSG( 'NDF', IACB )
            CALL ERR_REP( 'NDF1_QRST_MAP',
     :      'The quality component in the NDF structure ^NDF is ' //
     :      'already mapped for access through the specified ' //
     :      'identifier (possible programming error).', STATUS )

*  Only take further action if this is a base NDF.  Check that the
*  quality component is not mapped at all. Report an error if it is.
         ELSE IF ( .NOT. ACB_CUT( IACB ) ) THEN
            IF ( DCB_NQMAP( IDCB ) .NE. 0 ) THEN
               STATUS = NDF__ISMAP
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF1_QRST_BMAP',
     :         'The quality component in the NDF structure ^NDF is ' //
     :         'already mapped for access through another ' //
     :         'identifier (possible programming error).', STATUS )

*  See if the DCB quality array identifier is valid. If not, then the
*  quality component is already undefined.
            ELSE
               CALL ARY_VALID( DCB_QID( IDCB ), VALID, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If it is defined, then obtain the quality array storage form, which
*  may have changed since it was created. Save this as the default for
*  use if the array is re-created.
                  IF ( VALID ) THEN
                     CALL ARY_FORM( DCB_QID( IDCB ), DCB_QFRM( IDCB ),
     :                              STATUS )

*  Delete the quality array. Note that all identifiers referring to it
*  (e.g. those in the ACB) thereby become invalid.
                     CALL ARY_DELET( DCB_QID( IDCB ), STATUS )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_QRST', STATUS )

      END
