      SUBROUTINE NDF1_CRNBN( IDCB, IACB, STATUS )
*+
*  Name:
*     NDF1_CRNBN

*  Purpose:
*     Create a new ACB base NDF entry from a DCB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CRNBN( IDCB, IACB, STATUS )

*  Description:
*     The routine creates a new ACB entry describing a base NDF which
*     refers to the data object whose DCB entry is supplied. The new
*     NDF acquires its attributes directly from the data object. The
*     reference count for the DCB entry is incremented by one.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB to which the ACB
*        entry is to refer.
*     IACB = INTEGER (Returned)
*        Index to the new ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If STATUS is set on entry, then the routine will return a
*     value of zero for the IACB argument, although no further
*     processing will occur.
*     -  A value of zero will also be returned for the IACB argument if
*     the routine fails for any reason.

*  Algorithm:
*     -  Set an initial value of zero for the IACB argument before
*     checking the inherited status.
*     -  Ensure that necessary information about the object's data
*     array is available in the DCB.
*     -  Obtain a free slot for the new entry in the ACB.
*     -  Initialise the ACB entry.
*     -  Clone the data array identifier from the DCB.
*     -  Clone a quality array identifier from the DCB, if available.
*     Otherwise set a null identifier.
*     -  Clone a variance array identifier from the DCB, if available.
*     Otherwise set a null identifier.
*     -  If there was an error, then annul any identifiers which may
*     have been acquired, free the allocated slot in the ACB.
*     -  If there was no error, then increment the DCB reference count
*     for the data object.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-SEP-1989 (RFWS):
*        Original, derived from the ARY_ system routine ARY_$CRNBA.
*     3-OCT-1989 (RFWS):
*        Minor corrections to prologue.
*     12-DEC-1989 (RFWS):
*        Installed support for the variance component.
*     13-DEC-1989 (RFWS):
*        Added annulling of identifiers if an error occurs.
*     29-JAN-1990 (RFWS):
*        Changed position of clean-up code to avoid invalid array index
*        under error conditions.
*     30-JAN-1990 (RFWS):
*        Installed support for the quality component.
*     1-FEB-1990 (RFWS):
*        Installed initialisation of the quality component control
*        flags.
*     23-OCT-1990 (RFWS):
*        Removed initialisation of ACB quanitities which are now
*        processed by NDF1_FFS.
*     21-AUG-2000 (DSB):
*        Annul the DCB entry if its DATA_ARRAY cannot be accessed.
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
      INCLUDE 'AST_PAR'          ! AST_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_KQ( NDF__MXDCB ) = LOGICAL (Read)
*           Whether quality information is available in the DCB.
*        DCB_KV( NDF__MXDCB ) = LOGICAL (Read)
*           Whether variance information is available in the DCB.
*        DCB_MOD( NDF__MXDCB ) = CHARACTER * ( NDF__SZMOD ) (Read)
*           Data object access mode.
*        DCB_QID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the quality array.
*        DCB_REFCT( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of ACB entries which refer to the data object.
*        DCB_VID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the variance array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ACC( NDF__MXACC, NDF__MXACB ) = LOGICAL (Write)
*           Access control flags.
*        ACB_DID( NDF__MXACB ) = INTEGER (Write)
*           ARY_ system identifier for the data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Write)
*           Index to the data object entry in the DCB.
*        ACB_QID( NDF__MXACB ) = INTEGER (Write)
*           ARY_ system identifier for the quality component.
*        ACB_VID( NDF__MXACB ) = INTEGER (Write)
*           ARY_ system identifier for the variance component.

*  Arguments Given:
      INTEGER IDCB

*  Arguments Returned:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACC               ! Loop counter for access control flags
      LOGICAL VALID              ! Whether array identifier is valid

*.

*  Set an initial value of zero for the IACB argument.
      IACB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that information about the data object's data array is
*  available in the DCB. If not, annul the DCB entry.
      CALL NDF1_DD( IDCB, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF1_DANL( .TRUE., IDCB, STATUS )
      END IF

*  Obtain an index to a free slot in the ACB.
      CALL NDF1_FFS( NDF__ACB, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the ACB entry to point to the DCB entry.
         ACB_IDCB( IACB ) = IDCB

*  Initialise the access control flags if the data object can be
*  modified.
         IF ( DCB_MOD( IDCB ) .EQ. 'UPDATE' ) THEN
            DO 1 IACC = 1, NDF__MXACC
               ACB_ACC( IACC, IACB ) = .TRUE.
 1          CONTINUE
         END IF

*  DATA component:
*  ==============
*  Clone an ARY_ system identifier for the data array.
         CALL ARY_CLONE( DCB_DID( IDCB ), ACB_DID( IACB ), STATUS )

*  QUALITY component:
*  ==================
*  Set an initial null ARY_ system identifier for the new ACB entry's
*  quality array.
         ACB_QID( IACB ) = ARY__NOID

*  If quality information is available in the DCB, then see if the DCB
*  ARY_ system identifier for the quality array is valid. If not, then
*  the quality array does not exist.
         IF ( DCB_KQ( IDCB ) ) THEN
            CALL ARY_VALID( DCB_QID( IDCB ), VALID, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If valid, then clone a quality array identifier for the ACB.
               IF ( VALID ) THEN
                  CALL ARY_CLONE( DCB_QID( IDCB ), ACB_QID( IACB ),
     :                            STATUS )
               END IF
            END IF
         END IF

*  VARIANCE component:
*  ==================
*  Set an initial null ARY_ system identifier for the new ACB entry's
*  variance array.
         ACB_VID( IACB ) = ARY__NOID

*  If variance information is available in the DCB, then see if the DCB
*  ARY_ system identifier for the variance array is valid. If not, then
*  the variance array does not exist.
         IF ( DCB_KV( IDCB ) ) THEN
            CALL ARY_VALID( DCB_VID( IDCB ), VALID, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If valid, then clone a variance array identifier for the ACB.
               IF ( VALID ) THEN
                  CALL ARY_CLONE( DCB_VID( IDCB ), ACB_VID( IACB ),
     :                            STATUS )
               END IF
            END IF
         END IF

*  If there was an error, then annul any identifiers which may have
*  been acquired, release the slot allocated in the ACB.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ARY_ANNUL( ACB_DID( IACB ), STATUS )
            CALL ARY_ANNUL( ACB_QID( IACB ), STATUS )
            CALL ARY_ANNUL( ACB_VID( IACB ), STATUS )
            CALL NDF1_RLS( NDF__ACB, IACB, STATUS )

*  Otherwise, increment the DCB reference count.
         ELSE
            DCB_REFCT( IDCB ) = DCB_REFCT( IDCB ) + 1
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CRNBN', STATUS )

      END
