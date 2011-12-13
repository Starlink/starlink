      SUBROUTINE CCD1_KPGT( PARAM, MAXKEY, QUNKEY, ULOC, NK, KEYS,
     :                      KLOCS, STATUS )
*+
*  Name:
*     CCD1_KPGT

*  Purpose:
*     Get the global and keyed values of keyed parameters.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_KPGT( PARAM, MAXKEY, QUNKEY, ULOC, NK, KEYS, KLOCS,
*                     STATUS )

*  Description:
*     This routine queries the ADAM global parameter database to find
*     the values of global variables.  It not only gets the normal
*     global variable values, but any keyed values (as written using
*     CCD1_KPSV) as well.  The values are returned as HDS primary
*     locators to the parameter structures (as got by HDSPAR) and
*     should be annulled in due course by the calling routine.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the basic parameter to which the global (and
*        possibly keyed) parameter corresponds.
*     MAXKEY = INTEGER (Given)
*        The maximum number of keyed values that can be returned
*        (size of arrays KEYS and KVALS).  If MAXKEY is zero, no
*        attempt will be made to return the keyed values.
*     QUNKEY = LOGICAL (Returned)
*        True if a non-empty value was found for the unkeyed global
*        variable.
*     ULOC = CHARACTER * ( * ) (Returned)
*        HDS locator of the unkeyed global variable (nothing returned
*        if QUNKEY is false).
*     NK = INTEGER (Returned)
*        The number of keyed values returned in KEYS and KVALS.
*        If more than MAXKEY keyed values are available, only the
*        first MAXKEY will be returned in KEYS and KLOCS and NK
*        will be returned equal to MAXKEY.
*     KEYS( * ) = INTEGER (Returned)
*        The keys for which values were found.
*     KLOCS( * ) = CHARACTER * ( * ) (Returned)
*        The HDS locators corresponding to KEYS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The locators returned in ULOC and KLOCS are primary locators.
*     They should be annulled by the calling routine.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAY-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS system constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER MAXKEY

*  Arguments Returned:
      LOGICAL QUNKEY
      CHARACTER * ( * ) ULOC
      INTEGER NK
      INTEGER KEYS( * )
      CHARACTER * ( * ) KLOCS( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) ALOC ! HDS locator for this keyed parameter
      CHARACTER * ( DAT__SZLOC ) GLOC ! HDS locator for GLOBAL file
      CHARACTER * ( DAT__SZNAM ) GPARAM ! Name of the global parameter
      CHARACTER * ( DAT__SZLOC ) KLOC ! HDS locator for this keyed value
      CHARACTER * ( DAT__SZLOC ) SLOC ! HDS locator for keyed params structure
      CHARACTER * ( DAT__SZNAM ) STNAME ! Key/value component name
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER K                  ! Loop variable
      INTEGER KEY                ! Key integer
      INTEGER NCOMP              ! Number of components in structure
      LOGICAL THERE              ! Is component present?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of keyed parameters found.
      NK = 0
      QUNKEY = .FALSE.

*  Get the name of the global parameter corresponding to the local
*  one named.
      CALL CCD1_GPNAM( PARAM, GPARAM, STATUS )

*  Get locators for the GLOBAL parameter file and the keyed parameters
*  structure.
      CALL CCD1_GPARF( 'READ', .FALSE., GLOC, SLOC, STATUS )

*  Only proceed if the global parameter file was opened successfully.
      IF ( GLOC .NE. DAT__NOLOC ) THEN

*  Try to get a locator for the unkeyed value.
         CALL DAT_THERE( GLOC, GPARAM, THERE, STATUS )
         IF ( THERE ) THEN
            CALL DAT_FIND( GLOC, GPARAM, ULOC, STATUS )

*  If there was any error, annul it and continue.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL DAT_ANNUL( ULOC, STATUS )
               CALL ERR_ANNUL( STATUS )
               QUNKEY = .FALSE.
            ELSE
               QUNKEY = .TRUE.
            END IF
         END IF

*  Only proceed to try to find the keyed values if we found a locator
*  to the keyed values structure.
         IF ( SLOC .NE. DAT__NOLOC ) THEN

*  Try to get a locator for the structure associated with the parameter
*  in question within the keyed variables structure.
            CALL DAT_THERE( SLOC, PARAM, THERE, STATUS )
            IF ( THERE ) THEN
               CALL DAT_FIND( SLOC, PARAM, ALOC, STATUS )

*  Get the number of components in the structure.
               CALL DAT_NCOMP( ALOC, NCOMP, STATUS )

*  In case of an error so far annul it and take no further action.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
               ELSE

*  Loop over each component in the structure.
                  DO I = 1, NCOMP
                     STNAME = ' '

*  Ensure that we don't have too many keyed values.
                     IF ( NK .LT. MAXKEY ) THEN

*  Get a locator for this component.
                        CALL DAT_INDEX( ALOC, I, KLOC, STATUS )

*  Get its name.
                        CALL DAT_NAME( KLOC, STNAME, STATUS )

*  Try to convert the name into a key integer.
                        IF ( STNAME( 1:4 ) .EQ. 'KEY_' ) THEN
                           CALL CHR_CTOI( STNAME( 5: ), KEY, STATUS )

*  We have a key integer and a locator representing the keyed value.
*  Insert the pair into corresponding positions in the arrays KEYS
*  and KLOCS, which are sorted into ascending order of KEYS, as
*  long as the key value is not already represented there.
*  This is not the most sophisticated insertion algorithm in the
*  world, but we are most unlikely to be dealing with large lists
*  here, so inefficiency will not be a problem in practice.
                           IF ( STATUS .EQ. SAI__OK ) THEN

*  First entry: put it at the start of the list.
                              IF ( NK .EQ. 0 ) THEN
                                 NK = NK + 1
                                 KEYS( NK ) = KEY
                                 KLOCS( NK ) = KLOC

*  Larger than last entry: put it at the end of the list.
                              ELSE IF ( KEY .GT. KEYS( NK ) ) THEN
                                 NK = NK + 1
                                 KEYS( NK ) = KEY
                                 KLOCS( NK ) = KLOC

*  Smaller than first entry: put it at the start of the list and
*  shift up higher entries.
                              ELSE IF ( KEY .LT. KEYS( 1 ) ) THEN
                                 NK = NK + 1
                                 DO J = NK, 2, -1
                                    KEYS( J  ) = KEYS( J - 1 )
                                    KLOCS( J ) = KLOCS( J - 1 )
                                 END DO
                                 KEYS( 1 ) = KEY
                                 KLOCS( 1 ) = KLOC

*  It must be in the middle of the range of existing entries: go through
*  the list looking for its place.
                              ELSE
                                 DO K = 2, NK
                                    IF ( KEY .GT. KEYS( K - 1 ) .AND.
     :                                   KEY .LT. KEYS( K ) ) THEN

*  It's between these two entries.  Shift up the higher ones, insert
*  it here, and exit the search loop.
                                       NK = NK + 1
                                       DO J = NK, K + 1, -1
                                          KEYS( J ) = KEYS( J - 1 )
                                          KLOCS( J ) = KLOCS( J - 1 )
                                       END DO
                                       KEYS( K ) = KEY
                                       KLOCS( K ) = KLOC
                                       GO TO 3
                                    END IF
                                 END DO

*  If we got here, it must be because it was equal to one of the
*  existing entries.  It will not be entered in the list.  Annul the
*  locator.
                                 CALL DAT_ANNUL( KLOC, STATUS )
 3                               CONTINUE
                              END IF

*  There was some error in getting the key integer or locator.  Ensure
*  the locator is no longer active.
                           ELSE
                              CALL DAT_ANNUL( KLOC, STATUS )
                           END IF
                        END IF

*  An error indicates some trouble in reading a component of the keyed
*  parameter structure.  Probably the structure was corrupted or
*  something.  This is not fatal, other components may be readable.
*  Annul the error and continue.
                        IF ( STATUS .NE. SAI__OK ) THEN
                           CALL ERR_ANNUL( STATUS )
                        END IF
                     END IF
                  END DO
               END IF

*  Release locators.
               CALL DAT_ANNUL( ALOC, STATUS )
            END IF
            CALL DAT_ANNUL( SLOC, STATUS )
         END IF

*  If everything is in order, promote the exported locators to primary
*  ones so that they will remain valid when the GLOBAL file is closed.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( QUNKEY ) THEN
               CALL DAT_PRMRY( .TRUE., ULOC, .TRUE., STATUS )
            END IF
            IF ( NK .GT. 0 ) THEN
               DO I = 1, NK
                  CALL DAT_PRMRY( .TRUE., KLOCS( I ), .TRUE., STATUS )
               END DO
            END IF
         END IF

*  Finally, annul the locator of the GLOBAL parameter file.
         CALL DAT_ANNUL( GLOC, STATUS )
      END IF

      END
* $Id$
