      SUBROUTINE NDF_MBAD( BADOK, INDF1, INDF2, COMP, CHECK, BAD,
     :                     STATUS )
*+
*  Name:
*     NDF_MBAD

*  Purpose:
*     Merge the bad-pixel flags of the array components of a pair of
*     NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_MBAD( BADOK, INDF1, INDF2, COMP, CHECK, BAD, STATUS )

*  Description:
*     The routine merges the bad-pixel flag values of an array
*     component (or components) for a pair of NDFs, returning the
*     logical "OR" of the separate values for each NDF. In addition, if
*     bad pixels are found to be present in either NDF but the
*     application indicates that it cannot correctly handle such
*     values, then an error to this effect is reported and a STATUS
*     value is set.

*  Arguments:
*     BADOK = LOGICAL (Given)
*        Whether the application can correctly handle NDF array
*        components containing bad pixel values.
*     INDF1 = INTEGER (Given)
*        Identifier for the first NDF whose bad-pixel flag value is to
*        be merged.
*     INDF2 = INTEGER (Given)
*        Identifier for the second NDF.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component: 'DATA', 'QUALITY' or
*        'VARIANCE'.
*     CHECK = LOGICAL (Given)
*        Whether to perform explicit checks to see whether bad pixels
*        are actually present. (This argument performs the same
*        function as in the routine NDF_BAD.)
*     BAD = LOGICAL (Returned)
*        The combined bad-pixel flag value (the logical "OR" of the
*        values obtained for each NDF).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be
*     supplied, in which case the routine will take the logical "OR" of
*     all the specified components when calculating the combined
*     bad-pixel flag value.
*     -  The effective value of the bad-pixel flag for each NDF array
*     component which this routine uses is the same as would be returned
*     by a call to the routine NDF_BAD.
*     -  If this routine detects the presence of bad pixels which the
*     application cannot support (as indicated by a .FALSE. value for
*     the BADOK argument), then an error will be reported to this
*     effect and a STATUS value of NDF__BADNS (bad pixels not
*     supported) will be returned.  The value of the BAD argument will
*     be set to .TRUE. under these circumstances. The NDF__BADNS
*     constant is defined in the include file NDF_ERR.

*  Algorithm:
*     -  Assign the first NDF identifier to the first element of an
*     array.
*     -  If the second identifier is different, then assign it to the
*     second array element.
*     -  Otherwise, use only one identifier.
*     -  Merge the array component bad pixel flags.
*     -  If an error occurred, then report context information and call
*     the error tracing routine.

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
*     22-FEB-1990 (RFWS):
*        Original version.
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
      LOGICAL BADOK
      INTEGER INDF1
      INTEGER INDF2
      CHARACTER * ( * ) COMP
      LOGICAL CHECK

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER N                  ! Number of different identifiers
      INTEGER NDFS( 2 )          ! Array for NDF identifiers

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Assign the first NDF identifier to the first element of an array.
      NDFS( 1 ) = INDF1

*  If the second identifier is different, then assign it to the second
*  array element.
      IF ( INDF2 .NE. INDF1 ) THEN
         NDFS( 2 ) = INDF2
         N = 2

*  Otherwise, use only one identifier.
      ELSE
         N = 1
      END IF

*  Merge the array component bad pixel flags.
      CALL NDF1_MBAD( BADOK, N, NDFS, COMP, CHECK, BAD, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_MBAD_ERR',
     :   'NDF_MBAD: Error merging the bad-pixel flags of the array ' //
     :   'components of a pair of NDFs.', STATUS )
         CALL NDF1_TRACE( 'NDF_MBAD', STATUS )
      END IF

      END
