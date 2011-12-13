      SUBROUTINE NDF_BAD( INDF, COMP, CHECK, BAD, STATUS )
*+
*  Name:
*     NDF_BAD

*  Purpose:
*     Determine if an NDF array component may contain bad pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_BAD( INDF, COMP, CHECK, BAD, STATUS )

*  Description:
*     The routine returns a logical value indicating whether an array
*     component of an NDF may contain bad pixels for which checks must
*     be made when the array's values are processed. Only if the
*     returned value is .FALSE. can such checks be omitted. If the
*     CHECK argument to this routine is set .TRUE., then it will also
*     perform an explicit check (if necessary) to see whether bad
*     pixels are actually present.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component: 'DATA', 'QUALITY' or
*        'VARIANCE'.
*     CHECK = LOGICAL (Given)
*        Whether to perform an explicit check to see whether bad pixels
*        are actually present.
*     BAD = LOGICAL (Returned)
*        Whether it is necessary to check for bad pixels when
*        processing the array's values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be
*     supplied, in which case the routine returns the logical "OR" of
*     the results for each component.
*     -  If CHECK is set .FALSE., then the returned value of BAD will
*     indicate whether bad pixels might be present and should therefore
*     be checked for during subsequent processing. However, even if BAD
*     is returned .TRUE. in such circumstances, it is still possible
*     that there may not actually be any bad pixels present (for
*     instance, in an NDF section, the accessible region of an array
*     component might happen to avoid all the bad pixels).
*     -  If CHECK is set .TRUE., then an explicit check will be made,
*     if necessary, to ensure that BAD is only returned .TRUE. if bad
*     pixels are actually present.
*     -  If a component is mapped for access through the identifier
*     supplied, then the value of BAD will refer to the actual mapped
*     values. It may differ from its original (unmapped) value if
*     conversion errors occurred during the mapping process, if an
*     initialisation option of '/ZERO' was specified for a component
*     whose value was initially undefined, or if the mapped values have
*     subsequently been modified.
*     -  A BAD=.TRUE. result will be returned for any components which
*     are in an undefined state, except in the case of the QUALITY
*     component for which a .FALSE. result is always returned under
*     these circumstances.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Determine if bad pixels may be present.
*     -  If an error has occurred, then report context information.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-FEB-1990 (RFWS):
*        Original version.
*     19-JUL-2007 (TIMJ):
*        Initialise return value regardless of STATUS.
*     {enter_changes_here}

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
      LOGICAL CHECK

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB

*.

      BAD = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Determine if bad pixels may be present.
      CALL NDF1_BAD( IACB, COMP, CHECK, BAD, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_BAD_ERR',
     :   'NDF_BAD: Error determining if an NDF array component may ' //
     :   'contain bad pixels.', STATUS )
         CALL NDF1_TRACE( 'NDF_BAD', STATUS )
      END IF

      END
