      SUBROUTINE NDF1_DELOB( LOC, STATUS )
*+
*  Name:
*     NDF1_DELOB

*  Purpose:
*     Delete an HDS object (and all sub-objects).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DELOB( LOC, STATUS )

*  Description:
*     The routine recursively deletes an HDS object (and all
*     sub-objects) passed by locator, and also annuls the locator. The
*     object may be a top-level object, in which case the container file
*     itself will be deleted. If the object is a cell or slice from a
*     structure array, then it will simply be emptied (i.e. all its
*     components will be erased).

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given and Returned)
*        Locator to the HDS object to be deleted. This will be annulled,
*        and a value of DAT__NOLOC will be returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine will attempt to execute even if it is called with
*     STATUS set, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  This routine will handle top-level HDS objects and should
*     always be used if the object may be a top level object. In cases
*     where the object is known to be a sub-component, the NDF1_ANTMP
*     routine may be called (and will be more efficient).
*     -  An error will result and a STATUS value of NDF__BNDIN will be
*     returned (unless STATUS was set on entry) if an attempt is made to
*     delete a cell or slice of a primitive HDS array.

*  Implementation Deficiencies:
*     -  The method by which this routine determines if it has been
*     passed a cell from a structure array is rather unsatisfactory.
*     This should be improved when facilities for obtaining this
*     information become available in HDS.

*  Copyright:
*     Copyright (C) 1994 Particle Physics and Astronomy Research Council

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
*     24-NOV-1989 (RFWS):
*        Original version.
*     1-DEC-1989 (RFWS):
*        Changed to allow the routine to execute under error
*        conditions.
*     10-DEC-1990 (RFWS):
*        Changed to cope with deletion of a cell in a structure array
*        (by simply erasing its contents).
*     29-APR-1994 (RFWS):
*        Fixed problem in erasing top-level objects - if the container
*        file was already marked for deletion, it could disappear when
*        annulling the supplied locator, causing the subsequent attempt
*        to erase it to fail. Replaced original code with a simple call
*        to HDS_ERASE (can't remember why it was originally more
*        complicated than this).
*     29-JUL-1994 (RFWS):
*        Do not reset cells extracted from primitive arrays, as this
*        also resets other elements of the array.
*     29-JUL-1994 (RFWS):
*        Ensure that the locator is always annulled, even if an error
*        occurs.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given and Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( NDF__SZFIL ) FILE ! Container file name
      CHARACTER * ( NDF__SZPTH ) PATH ! Object path name
      INTEGER NC                 ! Character count in object path name
      INTEGER NLEV               ! Depth of object nesting
      LOGICAL PRIM               ! Whether object is primitive

*.

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Obtain the depth of HDS object nesting, together with its container
*  file and path name.
      CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  See if the object is a cell (by seeing whether its path name ends in
*  ')'). If it is, then it cannot be deleted because other elements of
*  the array from which it is drawn must continue to exist. In this
*  case, we will simply reset the object.
         NC = MAX( 1, CHR_LEN( PATH ) )
         IF ( PATH( NC : NC ) .EQ. ')' ) THEN

*  Before resetting the object, determine if it is primitive (we cannot
*  reset it if it is, as other elements of the same array would also be
*  reset). Report an error if necessary.
            CALL DAT_PRIM( LOC, PRIM, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( PRIM ) THEN
                  STATUS = NDF__BNDIN
                  CALL DAT_MSG( 'OBJECT', LOC )
                  CALL ERR_REP( 'NDF1_DELOB_PRIM',
     :                 '^OBJECT is a subset of a primitive HDS ' //
     :                 'object and cannot be deleted independently ' //
     :                 'of the rest of the array.', STATUS )

*  If OK, we have a cell or slice of a structure array, so reset each
*  element.
               ELSE
                  CALL NDF1_HRST( LOC, STATUS )
               END IF
            END IF

*  Annul the object locator.
            CALL DAT_ANNUL( LOC, STATUS )

*  If it is a top level object, then mark it for deletion through HDS.
         ELSE IF ( NLEV .LE. 1 ) THEN
            CALL HDS_ERASE( LOC, STATUS )

*  If it is not a top level object, then call NDF1_ANTMP to erase it.
         ELSE
            CALL NDF1_ANTMP( LOC, STATUS )
         END IF

*  If an error occurred while obtaining information about the object,
*  then simply annul its locator.
      ELSE
         CALL DAT_ANNUL( LOC, STATUS )
      END IF

*  Reset the returned locator value.
      LOC = DAT__NOLOC

*  Call error tracing routine if appropriate.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DELOB', STATUS )

*  End the error reporting environment.
      CALL ERR_END( STATUS )

      END
