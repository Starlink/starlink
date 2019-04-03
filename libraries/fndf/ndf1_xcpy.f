      SUBROUTINE NDF1_XCPY( XLOC1, NEXTN, EXTN, LOC, XLOC2, STATUS )
*+
*  Name:
*     NDF1_XCPY

*  Purpose:
*     Copy an extension structure to a new MORE component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_XCPY( XLOC1, NEXTN, EXTN, LOC, XLOC2, STATUS )

*  Description:
*     The routine copies an existing extension (MORE) structure into a
*     new MORE component which it creates (if necessary) in a
*     designated HDS structure.  Selected components of the extension
*     structure may be omitted from the copying operation. The output
*     MORE structure should not exist before this routine is called and
*     the routine will only create it if it is actually needed to hold
*     extension components (i.e. if the number of components copied is
*     not zero).  A locator to the new structure is returned; this will
*     have the value DAT__NOLOC if creation of an output structure was
*     not necessary.

*  Arguments:
*     XLOC1 = CHARACTER * ( * ) (Given)
*        Locator to the input extension (MORE) structure to be copied.
*        A value of DAT__NOLOC may be used to indicate that the input
*        structure does not exist, in which case the XLOC2 argument
*        will simply be set to the same value.
*     NEXTN = INTEGER (Given)
*        The number of extension components to be excluded from the
*        copying operation (may be zero).
*     EXTN( NEXTN ) = CHARACTER * ( DAT__SZNAM ) (Given)
*        A list of the names of extension components which are to be
*        excluded from the copying operation. These components need not
*        necessarily exist within the input extension (MORE) structure.
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to an existing HDS structure within which the new
*        output MORE component is to be created.
*     XLOC2 = CHARACTER * ( * ) (Returned)
*        Locator to the new MORE structure, if it exists. Otherwise a
*        value of DAT__NOLOC is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A value of DAT__NOLOC will be returned for the XLOC2 argument
*     if this routine is called with STATUS set, although no further
*     processing will occur.
*     -  A value of DAT__NOLOC will also be returned for the XLOC2
*     argument if the routine should fail for any reason.

*  Algorithm:
*     -  Set an initial value for the XLOC2 argument, before checking
*     the inherited status.
*     -  Check that the input extension structure exists and see how
*     many components it has.
*     -  If the number of components is zero, there is nothing to do.
*     -  If no components are to be excluded, then copy the entire
*     structure and obtain a locator to the resulting new object.
*     -  Otherwise, each component must be copied separately. Note that
*     an output structure has not yet been created.
*     -  Get a locator to each input component in turn and obtain its
*     name.
*     -  Check the name against the list of components to be excluded.
*     -  If the component is not to be excluded, then an output
*     structure will be needed to hold it. Check whether this structure
*     has been created yet. If not, then create it and obtain a locator
*     to it.
*     -  Copy the component into the output structure.
*     -  Annul the component locator.
*     -  Repeat for each remaining input component.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     11-OCT-1989 (RFWS):
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Arguments Given:
      CHARACTER * ( * ) XLOC1
      INTEGER NEXTN
      CHARACTER * ( DAT__SZNAM ) EXTN( * )
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      CHARACTER * ( * ) XLOC2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CLOC ! Extension component locator
      CHARACTER * ( DAT__SZNAM ) NAME ! Extension component name
      INTEGER DUMMY( 1 )         ! Dummy dimension array
      INTEGER ICOMP              ! Loop counter for extension components
      INTEGER IEXTN              ! Loop counter for excluded components
      INTEGER NCOMP              ! Number of extension components
      LOGICAL CREATE             ! Whether output structure created
      LOGICAL EXCLUD             ! Whether to exclude a component

*.

*  Set an initial value for the XLOC2 argument.
      XLOC2 = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the input extension (MORE) structure exists.
      IF ( XLOC1 .NE. DAT__NOLOC ) THEN

*  See how many components it has.
         CALL DAT_NCOMP( XLOC1, NCOMP, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  There is nothing to do unless it has some components to copy.
            IF ( NCOMP .GT. 0 ) THEN

*  If there are no extension components to exclude from the copying
*  operation, then copy the entire extension (MORE) structure, including
*  all its components. Obtain a locator to the new structure.
               IF ( NEXTN .EQ. 0 ) THEN
                  CALL DAT_COPY( XLOC1, LOC, 'MORE', STATUS )
                  CALL DAT_FIND( LOC, 'MORE', XLOC2, STATUS )

*  Otherwise, each component must be checked before copying. Note
*  whether the output extension (MORE) structure has been created yet.
               ELSE
                  CREATE = .FALSE.

*  Get a locator to each input extension component in turn and obtain
*  its name.
                  DO 3 ICOMP = 1, NCOMP
                     CALL DAT_INDEX( XLOC1, ICOMP, CLOC, STATUS )
                     CALL DAT_NAME( CLOC, NAME, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Search the list of extension components to be excluded to see if
*  this name appears. Set the EXCLUD flag accordingly.
                        EXCLUD = .FALSE.
                        DO 1 IEXTN = 1, NEXTN
                           IF ( EXTN( IEXTN ) .EQ. NAME ) THEN
                              EXCLUD = .TRUE.
                              GO TO 2
                           END IF
1                       CONTINUE
2                       CONTINUE

*  If the extension component is not to be excluded, then an output
*  extension (MORE) structure is needed. Check to see if it has been
*  created yet.
                        IF ( .NOT. EXCLUD ) THEN
                           IF ( .NOT. CREATE ) THEN

*  If not, then create it and obtain a locator to it. Note it has now
*  been created.
                              DUMMY( 1 ) = 0
                              CALL DAT_NEW( LOC, 'MORE', 'EXT',
     :                                      0, DUMMY, STATUS )
                              CALL DAT_FIND( LOC, 'MORE', XLOC2,
     :                                       STATUS )
                              CREATE = .TRUE.
                           END IF

*  Copy the extension component into the output structure.
                           CALL DAT_COPY( CLOC, XLOC2, NAME, STATUS )
                        END IF
                     END IF

*  Annul the input extension component locator.
                     CALL DAT_ANNUL( CLOC, STATUS )

*  Quit the component loop if an error occurs.
                     IF ( STATUS .NE. SAI__OK ) GO TO 4
3                 CONTINUE
4                 CONTINUE
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_XCPY', STATUS )

      END
