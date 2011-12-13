      SUBROUTINE NDF1_MBAD( BADOK, N, NDFS, COMP, CHECK, BAD, STATUS )
*+
*  Name:
*     NDF1_MBAD

*  Purpose:
*     Merge NDF bad pixel flag values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_MBAD( BADOK, N, NDFS, COMP, CHECK, BAD, STATUS )

*  Description:
*     The routine obtains the bad pixel flag values for an array
*     component of a sequence of NDFs and returns the logical "or" of
*     the result. If the result is .TRUE. but the BADOK argument is set
*     to .FALSE. (indicating that the application cannot process bad
*     values) then an error is reported to this effect and STATUS set.

*  Arguments:
*     BADOK = LOGICAL (Given)
*        Whether the application can handle arrays containing bad
*        values.
*     N = INTEGER (Given)
*        Number of NDFs whose bad pixel flags are to be matched.
*     NDFS( N ) = INTEGER (Given)
*        Array of identifiers for the NDFs.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the array component whose bad pixel flag is to be
*        used: 'DATA', 'QUALITY' or 'VARIANCE'.
*     CHECK = LOGICAL (Given)
*        Whether to make an explicit check, if necessary. to ensure
*        that bad pixels are actually present.
*     BAD = LOGICAL (Returned)
*        The value of the combined bad pixel flag.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied
*     in which case the routine will take all the specified components
*     into consideration when calculating the combined bad pixel flag
*     value.
*     -  The effective value of the bad pixel flag used by this routine
*     for each NDF component is the same as would be returned by the
*     NDF_BAD routine.
*     -  If the routine detects the presence of bad pixels which the
*     application cannot support, then a STATUS value of NDF__BADNS
*     (bad pixels not supported) will be returned, as defined in the
*     include file NDF_ERR. The value of the BAD argument will be set
*     to .TRUE. under these circumstances.

*  Algorithm:
*     -  Initialise the result.
*     -  Loop to consider each NDF in turn.
*     -  Import the NDF identifier.
*     -  If the result is not yet set to .TRUE., then determine the bad
*     pixel flag value for this NDF.
*     -  If a .TRUE. result was obtained, but the application cannot
*     handle bad pixels, then report an error.
*     -  Quit considering NDFs once an error occurs.

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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      LOGICAL BADOK
      INTEGER N
      INTEGER NDFS( N )
      CHARACTER * ( * ) COMP
      LOGICAL CHECK

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for NDFs
      INTEGER IACB               ! Index to NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the result.
      BAD = .FALSE.

*  Loop to consider each NDF in turn.
      DO 1 I = 1, N

*  Import the NDF identifier.
         CALL NDF1_IMPID( NDFS( I ), IACB, STATUS )

*  If the result is not yet set to .TRUE., then determine the bad pixel
*  flag value for this NDF.
         IF ( .NOT. BAD ) THEN
            CALL NDF1_BAD( IACB, COMP, CHECK, BAD, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If a .TRUE. result was obtained, but the application cannot handle
*  bad pixels, then report an error.
               IF ( BAD .AND. ( .NOT. BADOK ) ) THEN
                  STATUS = NDF__BADNS
                  CALL NDF1_AMSG( 'NDF', IACB )

*  Ensure that the error message indicates the appropriate degree of
*  certainty about whether bad pixel values are present.
                  IF ( CHECK ) THEN
                     CALL ERR_REP( 'NDF1_MBAD_IS',
     :               'The NDF structure ^NDF contains "bad" ' //
     :               'pixel values which cannot be handled ' //
     :               'correctly by this application.', STATUS )
                  ELSE
                     CALL ERR_REP( 'NDF1_MBAD_MAY',
     :               'The NDF structure ^NDF may contain "bad" ' //
     :               'pixel values which cannot be handled ' //
     :               'correctly by this application.', STATUS )
                  END IF
               END IF
            END IF
         END IF

*  Quit considering NDFs once an error occurs.
         IF ( STATUS .NE. SAI__OK ) GO TO 2
1     CONTINUE
2     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_MBAD', STATUS )

      END
