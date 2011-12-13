      SUBROUTINE CCD1_KPLD( PARAM, KEY, STATUS )
*+
*  Name:
*     CCD1_KPLD

*  Purpose:
*     Load the keyed value of a parameter into the parameter file.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_KPLD( PARAM, KEY, STATUS )

*  Description:
*     This routine loads the version of a globally associated
*     parameter keyed by an integer value into the GLOBAL ADAM
*     parameter file.  It should be called before a key-senstive
*     parameter with a READ global association is accessed for reading.
*     If no value exists for the requested key (because it has not
*     been stored using a previous corresponding invocation of
*     CCD1_KPSV), then no action is taken.  The routine should not
*     be called more than once with the same value of PARAM and KEY
*     during the same task, or it may result in asking the user
*     more than once for the same information.
*
*     The routine is essentially abusing the ADAM parameter system in
*     order to do what it has to, and does a number of not-entirely-
*     recommended things.  It first ensures that the parameter
*     system believes it has not solicited the value of the
*     variable in question, and secondly inserts the appropriate
*     keyed parameter value into the GLOBAL parameter file, so
*     that a subsequent PAR_GET will retrieve a value as if that's
*     what the global variable was in the first place.
*     It also modifies the prompt string to indicate that the value
*     being solicited is specific to a given key value.
*
*     You should not perform a PAR_GET on the variable in question
*     before calling this routine.
*
*     If the routine is called with PARAM = ' ', the effect is to
*     initialise the info block rather than the routine's normal
*     behaviour.  This should be done at the start of a CCDPACK
*     task, otherwise multiple tasks within a monolith may
*     interfere with each other.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the parameter in question.  If the value ' ' is
*        supplied, then instead of the routine's normal behaviour
*        the info blocks will be initialised and no other action
*        will be taken.
*     KEY = INTEGER (Given)
*        The key to be used.  The value of the parameter corresponding
*        to this value will be substituted for the variable in the ADAM
*        parameter file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The keyed values are stored in the same ADAM parameter file
*     as the variables which PAR looks for them in, within a
*     structure called CCDPACK_KEYPARS.
*
*     Meticulous explicit error processing is not undertaken in this
*     routine.  If the GLOBAL parameter file differs badly from the
*     expected structure, the routine will exit with the error status
*     corresponding to the HDS call which failed.

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
*     2-MAY-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard HDS constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER KEY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXPAR             ! The most parameters this can be called on
      PARAMETER ( MAXPAR = 20 )

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) ALOC ! HDS locator for associative array
      CHARACTER * ( DAT__SZLOC ) GLOC ! HDS locator for GLOBAL parameter file
      CHARACTER * ( DAT__SZNAM ) GNAME( MAXPAR ) ! Global names array
      CHARACTER * ( DAT__SZNAM ) KEYNAM ! Name of key component
      CHARACTER * ( DAT__SZNAM ) PNAME( MAXPAR ) ! Parameter names array
      CHARACTER * ( 80 ) PROMPT  ! Parameter prompt string
      CHARACTER * ( DAT__SZLOC ) SLOC ! HDS locator for CCDPACK_KEYPARS struct
      CHARACTER * ( DAT__SZLOC ) VLOC ! HDS locator for matched value
      INTEGER I                  ! Loop variable
      INTEGER IPARAM             ! Index into info block for this parameter
      INTEGER ISTATE( MAXPAR )   ! Parameter initial SUBPAR state array
      INTEGER LENG               ! Length of string
      INTEGER NPARAM             ! Number of entries in info block
      INTEGER PCODE( MAXPAR )    ! Parameter SUBPAR index array
      LOGICAL THERE              ! Does HDS component exist?

*  Local Data:
      DATA NPARAM / 0 /

*  Save persistent variables.
      SAVE ISTATE
      SAVE PCODE
      SAVE PNAME
      SAVE GNAME
      SAVE NPARAM

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if we are to initialise the common block or get a particular
*  parameter.
      IF ( PARAM .EQ. ' ' ) THEN

*  If we are initialising, just do that and jump to the exit.
         NPARAM = 0
         GO TO 99
      END IF

*  See whether this routine has been called on this parameter before.
      IPARAM = 0
      DO I = 1, NPARAM
         IF ( PNAME( I ) .EQ. PARAM ) THEN
            IPARAM = I
            GO TO 1
         END IF
      END DO
 1    CONTINUE

*  We have not encountered this one before, so store information about
*  it in this routine's saved info block.
      IF ( IPARAM .EQ. 0 ) THEN

*  Check we have enough space to expand the info block.
         IF ( NPARAM .GE. MAXPAR ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_KPLD_TOOMANY',
     :      'CCD1_KPLD: Maximum number of keyed variables exceeded' //
     :      '(programming error)', STATUS )
            GO TO 99
         END IF

*  Extend the parameter info block and write an entry for this parameter
*  at the end.
         NPARAM = NPARAM + 1
         IPARAM = NPARAM

*  Save the parameter name.
         PNAME( IPARAM ) = PARAM

*  Save the name of the corresponding global parameter.
         CALL CCD1_GPNAM( PARAM, GNAME( IPARAM ), STATUS )

*  Save the SUBPAR namecode of this parameter.
         CALL SUBPAR_FINDPAR( PARAM, PCODE( IPARAM ), STATUS )

*  Save the initial SUBPAR state of this parameter.
         CALL SUBPAR_STATE( PCODE( IPARAM ), ISTATE( IPARAM ), STATUS )

*  We have encountered this parameter before (presumably for a different
*  KEY value), so the chances are it has already been solicited by
*  the parameter system and needs restoring to its initial SUBPAR state.
      ELSE
         CALL SUBPAR_INIT( PCODE( IPARAM ), ISTATE( IPARAM ), STATUS )
      END IF

*  Get locators to the global parameter file and the keyed variable
*  structure within it.
      CALL CCD1_GPARF( 'UPDATE', .FALSE., GLOC, SLOC, STATUS )
      IF ( GLOC .NE. DAT__NOLOC .AND. SLOC .NE. DAT__NOLOC ) THEN

*  Try to get a locator for the structure associated with the parameter
*  in question within the keyed variables structure.
         CALL DAT_THERE( SLOC, PARAM, THERE, STATUS )
         IF ( THERE ) THEN
            CALL DAT_FIND( SLOC, PARAM, ALOC, STATUS )

*  Generate the name of the component of this structure corresponding
*  to the key we are going to load.
            CALL MSG_SETI( 'KEY', KEY )
            CALL MSG_LOAD( ' ', 'KEY_^KEY', KEYNAM, LENG, STATUS )

*  Try to get a locator for this structure.
            CALL DAT_THERE( ALOC, KEYNAM, THERE, STATUS )
            IF ( THERE ) THEN
               CALL DAT_FIND( ALOC, KEYNAM, VLOC, STATUS )

*  We have found the value corresponding to the key requested.
*  Now to copy this into the correct place.  If a component representing
*  the global variable already exists in the parameter file, erase it.
               CALL DAT_THERE( GLOC, GNAME( IPARAM ), THERE, STATUS )
               IF ( THERE ) THEN
                  CALL DAT_ERASE( GLOC, GNAME( IPARAM ), STATUS )
               END IF

*  Copy the structure we found into the right place.
               CALL DAT_COPY( VLOC, GLOC, GNAME( IPARAM ), STATUS )

*  Annul HDS locators.
               CALL DAT_ANNUL( VLOC, STATUS )
            END IF
            CALL DAT_ANNUL( ALOC, STATUS )
         END IF
      END IF
      IF ( SLOC .NE. DAT__NOLOC ) CALL DAT_ANNUL( SLOC, STATUS )
      IF ( GLOC .NE. DAT__NOLOC ) CALL DAT_ANNUL( GLOC, STATUS )

*  Rewrite the parameter's prompt string to indicate that a value
*  for this key is being solicited.  First get the existing prompt
*  string.
      CALL CCD1_GPRMT( PARAM, PROMPT, STATUS )

*  Construct a new prompt string which contains the value of KEY.
      CALL MSG_SETC( 'BASE', PROMPT )
      CALL MSG_SETI( 'KEY', KEY )
      CALL MSG_LOAD( ' ', '^BASE - Set Index ^KEY', PROMPT, LENG,
     :               STATUS )

*  Set the parameter prompt string.
      CALL PAR_PROMT( PARAM, PROMPT( 1:LENG ), STATUS )

*  Error exit label.
 99   CONTINUE

      END
* $Id$
