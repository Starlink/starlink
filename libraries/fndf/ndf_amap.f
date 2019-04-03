      SUBROUTINE NDF_AMAP( INDF, COMP, IAXIS, TYPE, MMOD, PNTR, EL,
     :                     STATUS )
*+
*  Name:
*     NDF_AMAP

*  Purpose:
*     Obtain mapped access to an NDF axis array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_AMAP( INDF, COMP, IAXIS, TYPE, MMOD, PNTR, EL, STATUS )

*  Description:
*     The routine obtains mapped access to an NDF axis array, returning
*     a pointer to the mapped values and a count of the number of
*     elements mapped.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the axis array component to be mapped: 'CENTRE',
*        'VARIANCE' (or 'ERROR') or 'WIDTH'.
*     IAXIS = INTEGER (Given)
*        Number of the NDF axis whose array is to be mapped.
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type to be used for access (e.g. '_REAL').
*     MMOD = CHARACTER * ( * ) (Given)
*        Mapping mode for access to the array: 'READ', 'UPDATE' or
*        'WRITE'.
*     PNTR = INTEGER( * ) (Returned)
*        Pointer(s) to the mapped values (see the Notes section).
*     EL = INTEGER (Returned)
*        Number of elements mapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of axis array component names may also
*     be given, in which case the routine will map all the requested
*     axis arrays using the same numeric type and mapping mode.
*     Pointers to the values of these mapped arrays will be returned
*     (in the specified order) in the elements of the array PNTR, which
*     must be of sufficient size to accommodate them.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Map the axis array.
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
*     9-OCT-1990 (RFWS):
*        Original version.
*     10-OCT-1990 (RFWS):
*        Upgraded to allow a comma-separated list of array names.
*     21-DEC-1990 (RFWS):
*        Changed to return a "safe" value for EL under error
*        conditions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP
      INTEGER IAXIS
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) MMOD

*  Arguments Returned:
      INTEGER PNTR( * )
      INTEGER EL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Import the NDF identifier.
         CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Map the axis array.
         CALL NDF1_AMAP( IAXIS, IACB, COMP, TYPE, MMOD, PNTR, EL,
     :                   STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'NDF_AMAP_ERR',
     :      'NDF_AMAP: Error obtaining mapped access to an NDF ' //
     :      'axis array.', STATUS )
            CALL NDF1_TRACE( 'NDF_AMAP', STATUS )
         END IF
      END IF

*  Return a "safe" value for EL under error conditions.
      IF ( STATUS .NE. SAI__OK ) THEN
         EL = 1
      END IF

      END
