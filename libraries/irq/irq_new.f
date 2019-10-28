      SUBROUTINE IRQ_NEW( INDF, XNAME, LOCS, STATUS )
*+
*  Name:
*     IRQ_NEW

*  Purpose:
*     Create a new structure to hold quality name information within an
*     NDF extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_NEW( INDF, XNAME, LOCS, STATUS )

*  Description:
*     An HDS object (named QUALITY_NAMES) is created to hold quality
*     name information within the specified NDF extension. An error is
*     reported if the NDF extension does not exist. If the extension does
*     exist, an array of five HDS locators is returned which is needed
*     when calling other IRQ routines. The first locator points to a
*     temporary object which holds a cloned identifier for the NDF,
*     the other four point to components of the QUALITY_NAMES structure
*     contained in the NDF. IRQ_RLSE should be called to annul these
*     locators (and the NDF identifier) when no further access to the
*     NDFs quality names information is required.
*
*     The QUALITY component of the NDF is reset to an undefined state by
*     this routine. Therefore, the QUALITY component should not be mapped
*     for access prior to calling this routine.
*
*     The LOCS argument returned by this routine specifies the NDF
*     which will be operated on by subsequent IRQ routines.
*     Specifically, LOCS determines the bounds of the NDF. Care should
*     therefore be taken that subsequent calls to IRQ routines refer to
*     the NDF specified by the INDF argument to this routine, and not
*     for instance to a section of the NDF which will in general have
*     different bounds.
*
*     Note, an error is reported if only READ access is available to the
*     NDF.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     XNAME = CHARACTER * ( * ) (Given)
*        The name of the NDF extension in which the quality name
*        information is to be stored. If this extension does not exist
*        then an error is reported.
*     LOCS(5) = CHARACTER * ( * ) (Returned)
*        A set of HDS locators as described above.  The character
*        variables supplied for this argument should have a declared
*        length equal to symbolic constant DAT__SZLOC. These locator
*        are annulled by calling IRQ_RLSE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-JUL-1991 (DSB):
*        Original version.
*     4-JUL-2012 (DSB):
*        21 years on - ensure the VALID component has a defined value.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      INTEGER INDF
      CHARACTER XNAME*(*)

*  Arguments Returned:
      CHARACTER LOCS( 5 )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CINDF              ! Cloned NDF identifier.
      CHARACTER * ( DAT__SZLOC ) QILOC ! Locator to the QUALITY_NAMES
                                 ! structure.
      INTEGER SLOT               ! Loop count.
      LOGICAL STRUC              ! True if an object is a structure.
      LOGICAL THERE              ! True if a QUALITY_NAMES structure
                                 ! already exists in the named NDF
                                 ! extension.
      LOGICAL WRITE              ! True if write access to NDF is
                                 ! available.
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to the NDF extension.

*.

*  Initialise
      LOCS( 1 ) = DAT__NOLOC
      LOCS( 2 ) = DAT__NOLOC
      LOCS( 3 ) = DAT__NOLOC
      LOCS( 4 ) = DAT__NOLOC
      LOCS( 5 ) = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied character variables LOCS are long enough to
*  hold HDS locators.
      IF( LEN( LOCS( 1 ) ) .LT. DAT__SZLOC ) THEN
         STATUS = IRQ__LSHRT
         CALL ERR_REP( 'IRQ1_NEW_ERR1',
     :  'IRQ_NEW: Declared length of character variables within LOCS'//
     :  ' is wrong',
     :                 STATUS )
      END IF

*  Check that write access is available to the NDF.
      CALL NDF_ISACC( INDF, 'WRITE', WRITE, STATUS )
      IF( .NOT. WRITE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRQ__NOWRT
         CALL ERR_REP( 'IRQ_NEW_ERR2',
     :   'IRQ_NEW: Write access is not available to the NDF.',
     :   STATUS )
      END IF

*  Attempt to get a locator to the named extension.
      CALL NDF_XLOC( INDF, XNAME, 'UPDATE', XLOC, STATUS )

*  See if the extension is a structure.
      CALL DAT_STRUC( XLOC, STRUC, STATUS )

*  If so, check that the extension does not already contain a
*  QUALITY_NAMES component.
      IF( STRUC ) THEN
         CALL DAT_THERE( XLOC, IRQ__QINAM, THERE, STATUS )

*  If the extension is a primative, QUALITY_NAMES cannot exist.
      ELSE
         THERE = .FALSE.
      END IF

*  Report an error if it does.
      IF( THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRQ__QIEXS
         CALL ERR_REP( 'IRQ_NEW_ERR3',
     : 'IRQ_NEW: Extension already contains quality names information',
     :                 STATUS )
      END IF

*  Create and locate the QUALITY_NAMES component within the extension.
      CALL DAT_NEW( XLOC, IRQ__QINAM, IRQ__QITYP, 0, 0, STATUS )
      CALL DAT_FIND( XLOC, IRQ__QINAM, QILOC, STATUS )

*  Create and locate all the components of the QUALITY_NAMES
*  structure...

*  1) An array of QUAL structures. Each cell of this array holds all the
*  information related to a single quality name. The structure within
*  each cell (or "slot") is established when the slot is initialised
*  using IRQ1_ISLOT. This array has an initial size of IRQ__INCQ
*  (currently 8) and is extended as needed by routine IRQ1_RSLOT.
      CALL DAT_NEW( QILOC, IRQ__QUNAM, IRQ__QUTYP, 1, IRQ__INCQ,
     :              STATUS )
      CALL DAT_FIND( QILOC, IRQ__QUNAM, LOCS(2), STATUS )

*  2) An integer holding the index of the highest slot in the QUAL array
*  currently in use.
      CALL DAT_NEW( QILOC, IRQ__LUNAM, '_INTEGER', 0, 0, STATUS )
      CALL DAT_FIND( QILOC, IRQ__LUNAM, LOCS(3), STATUS )

*  3) An integer holding the current number of free slots in the QUAL
*  array.
      CALL DAT_NEW( QILOC, IRQ__NFNAM, '_INTEGER', 0, 0, STATUS )
      CALL DAT_FIND( QILOC, IRQ__NFNAM, LOCS(4), STATUS )

*  4) An integer array holding the indices of each free slot in the QUAL
*  array. This array is always the same size as the QUAL array. It is
*  extended by IRQ1_RSLOT whenever the QUAL array is extended.
      CALL DAT_NEW( QILOC, IRQ__FRNAM, '_INTEGER', 1, IRQ__INCQ,
     :              STATUS )
      CALL DAT_FIND( QILOC, IRQ__FRNAM, LOCS(5), STATUS )

*  Initialise the index of the last used slot to zero.
      CALL DAT_PUT0I( LOCS(3), 0, STATUS )

*  Initialise the number of free slots to zero (IRQ1_ISLOT increments
*  the number of free slots as each slot is initialised).
      CALL DAT_PUT0I( LOCS(4), 0, STATUS )

*  Initialise all the newly created slots. This creates the structure
*  within each slot of the QUAL array, adds the slot to the list of
*  free slots located by LOCS(5), and increments the number of free
*  slots located by LOCS(4).
      DO SLOT = IRQ__INCQ, 1, -1
         CALL IRQ1_ISLOT( LOCS, SLOT, STATUS )
      END DO

*  If this routine is interupted before completion it can result in an
*  invalid QUALITY_NAMES structure being left in the NDF extension.
*  Add a logical component called VALID to the QUALITY_NAMES structure
*  to indicate that the structure is now valid.
      CALL DAT_NEW0L( QILOC, IRQ__VANAM, STATUS )
      CALL CMP_PUT0L( QILOC, IRQ__VANAM, .TRUE., STATUS )

*  Annul the locators to the extension and the QUALITY_NAMES
*  structure.
      CALL DAT_ANNUL( XLOC, STATUS )
      CALL DAT_ANNUL( QILOC, STATUS )

*  Create a temporary structure holding a cloned NDF identifier.  The
*  cloned NDF identifier is annulled when IRQ_RLSE is called.
      CALL NDF_CLONE( INDF, CINDF, STATUS )
      CALL IRQ1_TEMP( '_INTEGER', 0, LOCS(1), STATUS )
      CALL DAT_PUT0I( LOCS(1), CINDF, STATUS )

*  Reset the QUALITY component of the NDF.
      CALL NDF_RESET( INDF, 'QUALITY', STATUS )

*  If an error occur, give context information.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETC( 'X', XNAME )
         CALL ERR_REP( 'IRQ_NEW_ERR4',
     :       'IRQ_NEW: Unable to create new quality names structure '//
     :       'in extension ^X of NDF ^NDF', STATUS )
      END IF

      END
