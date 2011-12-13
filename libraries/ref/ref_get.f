      SUBROUTINE REF_GET( ELOC, MODE, LOC, STATUS )
*+
*  Name:
*     REF_GET

*  Purpose:
*     Get locator to referenced data object.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL REF_GET( ELOC, MODE, LOC, STATUS )

*  Description:
*     This routine gets a locator to an HDS object referenced in a
*     reference object and links it to the group $$REFERENCED$.  Any
*     locator obtained in this way should be annulled, when finished
*     with, by REF_ANNUL so that the top-level object will also be
*     closed.

*  Arguments:
*     ELOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        A locator associated with the reference object
*     MODE = CHARACTER * ( * ) (Given)
*        Mode of access required to the object. ('READ', 'WRITE' or
*        'UPDATE'). This is specified so that the container file of any
*        referenced object can be opened in the correct mode.
*     LOC = CHARACTER * ( DAT__SZLOC ) (Returned)
*        A locator pointing to the object referenced.
*     STATUS = INTEGER (Given and Returned)
*        Inherited global status.

*  Copyright:
*     Copyright (C) 1982, 1987, 1988, 1989, 1990, 1992, 1993 Science & Engineering Research Council.
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
*     SLW: Sid Wright (UCL)
*     AJC: A.J. Chipperfield (STARLINK, RAL)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     24-APR-1982 (SLW):
*        Original version.
*     17_FEB-1987 (AJC):
*        Expanded reference structure.
*     24-DEC-1987 (AJC):
*        Change ERR_REP to DAT_ERTXT.
*     13-JAN-1988 (AJC):
*        Improve error reporting.
*     15-JAN-1988 (AJC):
*        Ensure all temp locators annulled.
*     20-JAN-1988 (AJC):
*        Improve prologue.
*     19-SEP-1989 (AJC):
*        Remove HDS_CLOSE top-level object.
*     29-NOV-1989 (AJC):
*        Link the locator to group $$REFERENCED$.
*     6-MAR-1990 (AJC):
*        Use BLOC as top-level locator also.
*     20-FEB-1992 (RFWS):
*        Standardise prologue.
*     5-JAN-1993 (RFWS):
*        Added fudge to allow use with versions of HDS before and after
*        V4.1, to avoid having to synchronise releases.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_ERR'          ! DAT_ error codes
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'REF_ERR'          ! REF_ error codes
      INCLUDE 'REF_PAR'		 ! REF_ public constants
      INCLUDE 'SAE_PAR'		 ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) ELOC
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS		 ! Global status

*  Local Constants:
      INTEGER MAXLEV             ! Max no. of levels in path
      PARAMETER ( MAXLEV = 16 )

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) BLOC ! Intermediate locator
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary locator (for fudge)
      CHARACTER * ( DAT__SZNAM ) COMP( MAXLEV ) ! Components of path
      CHARACTER * ( DAT__SZTYP ) TYPE ! Type of 'reference' object
      CHARACTER * ( REF__SZREF ) FILE ! Name of container file
      CHARACTER * ( REF__SZREF ) PATH ! Path name of object.
      CHARACTER * ( REF__SZREF ) TPATH ! Temp path name.
      INTEGER JUNK               ! Junk variable
      INTEGER LEVEL              ! Do loop control
      INTEGER NLEV               ! Number of components in path
      LOGICAL NEWHDS             ! HDS V4.1 or later in use?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the object is a reference object. Report an error if it is
*  not.
      CALL DAT_TYPE( ELOC, TYPE, STATUS )
      IF ( TYPE .NE. 'REFERENCE_OBJ' ) THEN
         STATUS = REF__OBJIN
         CALL DAT_MSG( 'OBJECT', ELOC )
         CALL EMS_SETC( 'TYPE', TYPE )
         CALL EMS_REP( 'REF_GET_INV',
     :                 'The HDS object ^OBJECT has an invalid type ' //
     :                 'of ''^TYPE''; it should be of type ' //
     :                 '''REFERENCE_OBJ''.', STATUS )

*  Get reference components.
      ELSE
         CALL CMP_GET0C( ELOC, 'FILE', FILE, STATUS )
         CALL CMP_GET0C( ELOC, 'PATH', PATH, STATUS )

*  Use the file containing the reference if necessary.
         IF ( FILE( 1 : 6 ) .EQ. '      ' ) THEN
            CALL HDS_TRACE( ELOC, NLEV, TPATH, FILE, STATUS )
         END IF

******* HORRIBLE FUDGE *******

* This is a fudge to allow REF to work with versions of HDS both before
* and after the introduction of "primary/secondary" locators (V4.1) and
* is present to remove the requirement for a simultaneous release of
* both systems. It should be removed, and a call to DAT_PRMRY
* introduced, after HDS V4.1 is fully in use.

*  First make a call to HDS_GTUNE specifying the SHELL parameter. This
*  tuning parameter is introduced at HDS V4.1, so will give an error
*  with previous versions. Use this fact to determine if the new HDS is
*  in use.
         NEWHDS = .TRUE.
         CALL EMS_MARK
         CALL HDS_GTUNE( 'SHELL', JUNK, STATUS )
         IF ( STATUS .EQ. DAT__NAMIN ) THEN
            CALL EMS_ANNUL( STATUS )
            NEWHDS = .FALSE.
         END IF
         CALL EMS_RLSE

*  If the old HDS is in use, then open the required HDS container file
*  (this is the way it was always done).
         IF ( .NOT. NEWHDS ) THEN
            CALL HDS_OPEN( FILE, MODE, BLOC, STATUS )

*  If the new HDS is in use, then keep a separate top-level locator
*  active to hold the file open. Its resources are not released until
*  HDS_CLOSE is called to close the file. Note this is not a documented
*  feature of HDS and should not be exploited.
         ELSE
            CALL HDS_OPEN( FILE, MODE, TLOC, STATUS )
            CALL DAT_CLONE( TLOC, BLOC, STATUS )
         END IF

******* END OF HORRIBLE FUDGE *******

*  Split the pathname into its components.
         CALL REF1_SPLIT( PATH, MAXLEV, NLEV, COMP, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  BLOC locates the top level of the required container file.
            IF ( NLEV .GT. 1 ) THEN

*  Search for component.
               LEVEL = 2
               DOWHILE ( ( LEVEL .LE. NLEV ) .AND.
     :                   (STATUS .EQ. SAI__OK ) )
                  CALL DAT_FIND( BLOC, COMP( LEVEL ), LOC, STATUS )
                  CALL DAT_ANNUL( BLOC, STATUS )
                  CALL DAT_CLONE( LOC, BLOC, STATUS )
                  CALL DAT_ANNUL( LOC, STATUS )
                  LEVEL = LEVEL + 1
               END DO
            END IF

*  Set output argument locator.
            CALL DAT_CLONE( BLOC, LOC, STATUS )

*  Mark it as a reference.
            CALL HDS_LINK( LOC, '$$REFERENCED$', STATUS )

*  Annul the intermediate locator.
            CALL DAT_ANNUL( BLOC, STATUS )
         END IF
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_REP( 'REF_GET_ERR',
     :   'REF_GET: Error obtaining a locator to a referenced object.',
     :   STATUS )
      END IF

      END
