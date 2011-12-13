      SUBROUTINE IRA_FIND( INDF, THERE, XNAME, ASNAME, LOC, STATUS )
*+
*  Name:
*     IRA_FIND

*  Purpose:
*     Finds an astrometry structure within an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_FIND( INDF, THERE, XNAME, ASNAME, LOC, STATUS )

*  Description:
*     A search is made for an astrometry structure within an NDF.  If
*     one is found, THERE is returned true. If one is not found, no
*     error is reported but THERE is returned false.  The name of the
*     extension in which it was found is returned, together with the
*     name of the astrometry structure, and a locator to the extension.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     THERE = LOGICAL (Returned)
*        True if an astrometry structure was found, false otherwise.
*     XNAME = CHARACTER * ( * ) (Returned)
*        The name of the NDF extension in which the astrometry
*        structure was found. Blank if none found.
*     ASNAME = CHARACTER * ( * ) (Returned)
*        The name of a component of the NDF extension holding the
*        astrometry information. Blank if none found.
*     LOC = CHARACTER * ( * ) (Returned)
*        A locator to the extension identified by XNAME. DAT__NOLOC if
*        no astrometry structure is found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1993 (DSB):
*        Original version.
*     8-NOV-2005 (DSB):
*        Annul the error set by DAT_NCOMP when an empty structure is
*        encountered.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'DAT_ERR'          ! DAT error constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      LOGICAL THERE
      CHARACTER XNAME*(*)
      CHARACTER ASNAME*(*)
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        CLOC*(DAT__SZLOC), ! HDS locator to a component.
     :        NAMEX*(DAT__SZNAM),! Name of current extension.
     :        TYPE*(DAT__SZTYP), ! HDS type of current component.
     :        XLOC*(DAT__SZLOC)  ! HDS locator to an extension.

      INTEGER
     :        I,                 ! Index of current extension.
     :        J,                 ! Index of current component.
     :        NCOMP,             ! No. of components in the extension.
     :        NFOUND,            ! No. of astrometry structures found
     :        XNUMB              ! No. of extensions in the NDF.

      LOGICAL
     :        DEF,               ! True if AS is in a defined state.
     :        STRUC              ! True if object is a structure.

*.

*  Initialise the returned arguments.
      THERE = .FALSE.
      XNAME = ' '
      ASNAME = ' '
      LOC = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of astrometry structures found.
      NFOUND = 0

*  Get the number of extensions in the NDF.
      CALL NDF_XNUMB( INDF, XNUMB, STATUS )

*  Loop round each one.
      DO I = 1, XNUMB

*  Get the name of the next extension.
         CALL NDF_XNAME( INDF, I, NAMEX, STATUS )

*  Get a locator to the next extension.
         CALL NDF_XLOC( INDF, NAMEX, 'READ', XLOC, STATUS )

*  See if the extension is a structure.
         CALL DAT_STRUC( XLOC, STRUC, STATUS )

*  If so, find the number of components within the extension. DAT_NCOMP
*  reports an error (DAT__OBJIN) if the structure is empty. If this
*  happens, annul the error and set the number of components to zero
         IF( STRUC .AND. STATUS .EQ. SAI__OK ) THEN
            CALL DAT_NCOMP( XLOC, NCOMP, STATUS )
            IF( STATUS .EQ. DAT__OBJIN ) THEN
               CALL ERR_ANNUL( STATUS )
               NCOMP = 0
            END IF

*  If the extension is a primitive, set the number of components to
*  zero.
         ELSE
            NCOMP = 0
         END IF

*  Loop round each component.
         DO J = 1, NCOMP

*  Get a locator to the next component.
            CALL DAT_INDEX( XLOC, J, CLOC, STATUS )

*  Get the type of this component.
            CALL DAT_TYPE( CLOC, TYPE, STATUS )

*  If it matches the type required for IRA astrometry structures,
*  check the AS is in a defined state.
            IF( TYPE .EQ. IRA__HDSTY ) THEN
               CALL IRA1_ASDEF( CLOC, DEF, STATUS )

*  Report an error if it is not in a defined state.
               IF( .NOT. DEF .AND. STATUS .EQ. SAI__OK ) THEN
                  STATUS = IRA__BADAS
                  CALL ERR_REP( 'IRA_FIND_ERR1',
     : 'IRA_FIND: Incomplete or corrupted astrometry information found',
     :                          STATUS )
               END IF

*  If it is OK, increment the number of astrometry structures found, and
*  store the names and extension locator for the first one.
               IF( STATUS .EQ. SAI__OK ) THEN
                  NFOUND = NFOUND + 1
                  IF( NFOUND .EQ. 1 ) THEN
                     THERE = .TRUE.
                     XNAME = NAMEX
                     CALL DAT_NAME( CLOC, ASNAME, STATUS )
                     LOC = XLOC
                  END IF
               END IF

            END IF

*  Annul the component locator.
            CALL DAT_ANNUL( CLOC, STATUS )

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Check the next component in this extension.
         END DO

*  Annul the locator to the current extension, so long as it is not the
*  returned locator.
         IF( LOC .NE. XLOC ) CALL DAT_ANNUL( XLOC, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Check the next extension.
      END DO

*  Report an error if more than one astrometry structure was found.
      IF( NFOUND .GT. 1 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRA__BADAS
         CALL MSG_SETI( 'N', NFOUND )
         CALL ERR_REP( 'IRA_FIND_ERR2',
     :                 'IRA_FIND: ^N astrometry structures found',
     :                 STATUS )
      END IF

*  If anything went wrong, ensure locators are annulled and add a
*  context report.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN

         CALL DAT_ANNUL( CLOC, STATUS )
         CALL DAT_ANNUL( XLOC, STATUS )
         CALL DAT_ANNUL( LOC, STATUS )

         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'IRA_FIND_ERR3',
     : 'IRA_FIND: Unable to locate astrometry information within ^NDF',
     :                 STATUS )

      END IF

      END
