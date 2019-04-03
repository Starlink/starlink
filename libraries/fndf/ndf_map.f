      SUBROUTINE NDF_MAP( INDF, COMP, TYPE, MMOD, PNTR, EL, STATUS )
*+
*  Name:
*     NDF_MAP

*  Purpose:
*     Obtain mapped access to an array component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_MAP( INDF, COMP, TYPE, MMOD, PNTR, EL, STATUS )

*  Description:
*     The routine obtains mapped access to an array component of an
*     NDF, returning a pointer to the mapped values and a count of the
*     number of elements mapped.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component to be mapped: 'DATA',
*        'QUALITY' or 'VARIANCE' (or 'ERROR').
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type to be used for access (e.g. '_REAL').
*     MMOD = CHARACTER * ( * ) (Given)
*        Mapping mode for access to the array: 'READ', 'UPDATE' or
*        'WRITE', with an optional initialisation mode '/BAD' or
*        '/ZERO' appended.
*     PNTR( * ) = INTEGER (Returned)
*        Pointer(s) to the mapped values (see the Notes section).
*     EL = INTEGER (Returned)
*        Number of elements mapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be given,
*     in which case the routine will map all the requested components
*     using the same data type and mapping mode. Pointers to the values
*     of these mapped components will be returned (in the specified
*     order) in the elements of the array PNTR, which must be of
*     sufficient size to accommodate them.
*     -  The result of mapping the QUALITY component with a data type
*     other than '_UBYTE' is not defined and should not be used.
*     -  If the array is stored in scaled form, then the mapped values
*     will be the result of applying the approproate scale and zero terms
*     to the elements of the underlying array.
*     -  If the array is stored in delta compressed form, then the mapped
*     values will be the original uncompressed values.
*     -  Scaled and delta arrays are read-only. An error will be reported
*     if the array is stored in scaled or delta form and the access mode
*     is UPDATE or WRITE.
*     -  If the MMOD argument includes the '/ZERO' option, the bad pixel
*     flag for the array will be reset to false to indicate that there are
*     no bad values present in the array (see NDF_BAD). If any bad values
*     are then placed into the mapped array, NDF_SBAD should normally be
*     called to set the bad pixel flag true. If this is not done, the bad
*     values in the array may be treated as literal values by subsequent
*     applications.
*     -  If this routine is called with STATUS set, then a value of 1
*     will be returned for the EL argument, although no further
*     processing will occur.  The same value will also be returned if
*     the routine should fail for any reason.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Map the array component(s).
*     -  Obtain the number of mapped elements.
*     -  If an error occurred, then report context information.
*     -  Under error conditions, return a "safe" value of EL.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     12-OCT-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     15-JAN-1990 (RFWS):
*        Added pointer array for use when mapping several NDF components
*        at once. Changed ACOMP argument to COMP.
*     23-JAN-1990 (RFWS):
*        Renamed from NDF_MAPV to NDF_MAP.
*     4-DEC-1990 (RFWS):
*        Changed to return a "safe" value of EL under error conditions.
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
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) MMOD

*  Arguments Returned:
      INTEGER PNTR( * )
      INTEGER EL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DUMMY              ! Dummy variable
      INTEGER IACB               ! Index to NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Import the NDF identifier.
         CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Map the array component(s).
         CALL NDF1_MAP( IACB, COMP, TYPE, .FALSE., MMOD, PNTR, DUMMY,
     :                  STATUS )

*  Calculate the number of mapped data elements.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL ARY_SIZE( ACB_DID( IACB ), EL, STATUS )
         END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'NDF_MAP_ERR',
     :      'NDF_MAP: Error obtaining mapped access to an array ' //
     :      'component of an NDF.', STATUS )
            CALL NDF1_TRACE( 'NDF_MAP', STATUS )
         END IF
      END IF

*  Under error conditions, return a "safe" value of EL.
      IF ( STATUS .NE. SAI__OK ) THEN
         EL = 1
      END IF

      END
