      SUBROUTINE KPG1_AGREF( PICID, ACCESS, THERE, NAME, STATUS )
*+
*  Name:
*     KPG1_AGREF

*  Purpose:
*     Obtains a name of or a locator to an object referenced in the
*     graphics database.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_AGREF( PICID, ACCESS, THERE, NAME, STATUS )

*  Description:
*     This routine determines whether a given picture in the AGI
*     graphics database has an object associated with it by reference.
*     If it has, the name of the object or a locator to the object is
*     returned with the desired access mode.

*  Arguments:
*     PICID = INTEGER (Given)
*        The identifier of the picture with which a data object may
*        be associated by reference.
*     ACCESS = CHARACTER * ( * ) (Given)
*        Access mode to the object: 'READ', 'WRITE' or 'UPDATE'.
*     THERE = LOGICAL (Returned)
*        If true the picture has an associated object and the returned
*        name is meaningful.
*     NAME = CHARACTER * ( * ) (Returned)
*        The name of the data object referenced by picture PICID, or a
*        locator to that object.  It should be ignored if THERE is
*        .FALSE..  The value should be tested to see if it a locator or
*        a name.  If it is a locator, it should be annulled by
*        REF_ANNUL.  A reasonable number of characters should be allowed
*        to accommodate a name including its path and section.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The code is a little convoluted because of a bug in AGI_GTREF
*     which means that a long name cannot be passed to it, if the
*     reference is via a locator.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research
*                   Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     1991 February 7 (MJC):
*        Original version.
*     1996 March 18 (MJC):
*        Obtains a name or a locator.  The LOC argument and
*        documentation renamed to NAME to reflect that.  Altered to
*        modern-style commenting.
*     8-FEB-2006 (DSB):
*        Initialise LOC before calling AGI_GTREF to avoid valgrind
*        reports.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'DAT_ERR'          ! DAT__ error constants

*  Arguments Given:
      INTEGER PICID

      CHARACTER * ( * ) ACCESS

*  Arguments Returned:
      LOGICAL THERE
      CHARACTER * ( * ) NAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Reference locator

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context.
      CALL ERR_MARK

*  Initialise the flag.
      THERE = .FALSE.

*  Get the locator to, or the name (with length of no more than
*  DAT__SZLOC) of, the NDF associated with the DATA picture.
      LOC = DAT__NOLOC
      CALL AGI_GTREF( PICID, ACCESS, LOC, STATUS )

*  The string was truncated implying the reference object is a name,
*  and not a locator.  So annul the error and get the name.
      IF ( STATUS .EQ. DAT__TRUNC ) THEN
         CALL ERR_ANNUL( STATUS )

*  Obtain this name of the NDF associated with the DATA picture.
         CALL AGI_GTREF( PICID, ACCESS, NAME, STATUS )

*  Check the status to double-check that a reference was found, and
*  isn't still too long.  If there was another error we handle it
*  transparently, since we can ask for an input NDF.
         IF ( STATUS .NE. SAI__OK .AND.  STATUS .NE. DAT__TRUNC ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE

*  The reference is still truncated, but continue, as it may be better
*  than no reference, and KPG1_ASREF will complain.
            IF ( STATUS .EQ. DAT__TRUNC ) CALL ERR_ANNUL( STATUS )

*  Record that a reference name was found, albeit truncated in some
*  cases.
            THERE = .TRUE.
         END IF

*  Check the status to determine whether or not a reference was found.
      ELSE IF ( STATUS .NE. SAI__OK ) THEN

*  Not found, but since we can ask for an input NDF the error can be
*  handled transparently.
         CALL ERR_ANNUL( STATUS )

      ELSE

*  Record that the locator or short-length name was found by reference.
*  It may be needed to avoid obtaining the object again, and will be
*  required to annul the locator via REF, if that is what it is.
*  Transfer the locator or name to the returned argument.
         THERE = .TRUE.
         NAME = LOC

      END IF

*  Release the error context.
      CALL ERR_RLSE

      END
