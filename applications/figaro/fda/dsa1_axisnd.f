      SUBROUTINE DSA1_AXISND( SLOT, AXIS,
     :   AXISND, ARYLOC, DELLOC, DELNAM, STATUS )
*+
*  Name:
*     DSA1_AXISND

*  Purpose:
*     Tell whether an axis has N-dimensional centre array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL  DSA1_AXISND( SLOT, AXIS,
*        AXISND, ARYLOC, DELLOC, DELNAM, STATUS )

*  Description:
*     This routine tells whether the specified axis has an N-dimensional
*     array of pixel centres. If it does, then an HDS locator to the
*     primitive array is returned. This routine also returns information
*     suitable for deleting the array. In order to delete an HDS component
*     one has to locate its parent and know its name. When deleting the
*     array in question here, one may even want to delete more of the
*     HDS structure tree, insofar as it exists only for the benefit of
*     this array. The caller can access the array with
*
*     IF ( AXISND )
*        CALL DAT_MAPV( ARYLOC, TYPE, MODE, PNTR, NELM, STATUS )
*        ...
*        CALL DAT_UNMAP( ARYLOC, STATUS )
*        CALL DAT_ANNUL( ARYLOC, STATUS )
*        CALL DAT_ANNUL( DELLOC, STATUS )
*     END IF
*
*     or it can delete the array and its otherwise obsolete parents with
*
*     IF ( AXISND ) THEN
*        CALL DAT_ANNUL( ARYLOC, STATUS )
*        CALL DAT_ERASE( DELLOC, DELNAM, STATUS )
*        CALL DAT_ANNUL( DELLOC, STATUS )
*     END IF
*
*     This assumes of course that the structure sub-tree is not being
*     added to in the meantime.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The reference slot number associated with the NDF.
*     AXIS = INTEGER (Given)
*        The number of the axis in question.
*     AXISND = LOGICAL (Returned)
*        True/false if the structure ...DATA_ARRAY exists / does not
*        exist.
*     ARYLOC = CHARACTER * ( * ) (Returned)
*        The HDS locator to the primitive array of centre values.
*     DELLOC = CHARACTER * ( * ) (Returned)
*        The HDS locator to the parent of the sub-tree that should be
*        deleted in place of the primitive array itself.
*     DELNAM = CHARACTER * ( * ) (Returned)
*        The name of the sub-tree that should be deleted in place of the
*        primitive array itself.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     If AXISND is true, this routine returns two distinct HDS locators,
*     which both must be annulled by the caller.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     30 Jan 1996 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER SLOT
      INTEGER AXIS

*  Arguments Returned:
      LOGICAL AXISND
      CHARACTER * ( * ) ARYLOC
      CHARACTER * ( * ) DELLOC
      CHARACTER * ( * ) DELNAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL ISIMPL             ! Whether DATA_ARRAY is simple
      INTEGER PRIM, DELE         ! Indices for ARYLOC, DELLOC
      INTEGER I                  ! Loop variable
      CHARACTER * ( DAT__SZLOC ) TLOC( 7 ) ! HDS locators

*.

*  Default return values.
      AXISND = .FALSE.
      ARYLOC = DAT__NOLOC
      DELLOC = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there is no AXIS component in the NDF, this axis is 1-D.
      CALL NDF_STATE( DSA__REFID1(SLOT), 'AXIS', AXISND, STATUS )
      IF ( .NOT. AXISND ) GO TO 500
      AXISND = .FALSE.

*  1 ndf
*  2 ndf.AXIS
*  3 ndf.AXIS(axis)
*  4 ndf.AXIS(axis).MORE
*  5 ndf.AXIS(axis).MORE.FIGARO
*  6 ndf.AXIS(axis).MORE.FIGARO.DATA_ARRAY
*  7 ndf.AXIS(axis).MORE.FIGARO.DATA_ARRAY.DATA

*  Establish whether the N-D centre array exists.
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL ERR_MARK
         CALL NDF_LOC( DSA__REFID1(SLOT), 'READ', TLOC(1), STATUS )
         CALL DAT_FIND(  TLOC(1), 'AXIS',   TLOC(2), STATUS )
         CALL DAT_CELL(  TLOC(2), 1, AXIS,  TLOC(3), STATUS )
         CALL DAT_FIND(  TLOC(3), 'MORE',   TLOC(4), STATUS )
         CALL DAT_FIND(  TLOC(4), 'FIGARO', TLOC(5), STATUS )
         CALL DAT_THERE( TLOC(5), 'DATA_ARRAY', AXISND, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL DAT_ANNUL( TLOC(5), STATUS )
            CALL DAT_ANNUL( TLOC(4), STATUS )
            CALL DAT_ANNUL( TLOC(3), STATUS )
            CALL DAT_ANNUL( TLOC(2), STATUS )
            CALL DAT_ANNUL( TLOC(1), STATUS )
            CALL ERR_ANNUL( STATUS )
            AXISND = .FALSE.
         END IF
      CALL ERR_RLSE
      IF ( .NOT. AXISND ) GO TO 500

*  We know it exists and have located all levels down to what may be
*  the simple or the primitive array.
      CALL DAT_FIND(  TLOC(5), 'DATA_ARRAY', TLOC(6), STATUS )
      CALL DAT_THERE( TLOC(6), 'DATA', ISIMPL, STATUS )
      IF ( ISIMPL ) CALL DAT_FIND( TLOC(6), 'DATA', TLOC(7), STATUS )

*  Decide on locator to primitive array.
      IF ( ISIMPL ) THEN
         PRIM = 7
      ELSE
         PRIM = 6
      END IF

*  Decide on locator for deletion.
*  One can always delete TLOC(5), 'DATA_ARRAY'.
*  If TLOC(5) contains nothing else, one can delete TLOC(4), 'FIGARO'.
*  If TLOC(4) contains nothing else, one can delete TLOC(3), 'MORE'.
      DELE   = 5
      DELNAM = 'DATA_ARRAY'
      CALL DAT_NCOMP( TLOC(5), I, STATUS )
      IF ( I .LE. 1 ) THEN
         DELE   = 4
         DELNAM = 'FIGARO'
         CALL DAT_NCOMP( TLOC(4), I, STATUS )
         IF ( I .LE. 1 ) THEN
            DELE   = 3
            DELNAM = 'MORE'
         END IF
      END IF

*  Annull all locators, except the two we return to the caller.
      DO 1 I = PRIM - 1, 1, -1
         IF ( I .NE. DELE ) CALL DAT_ANNUL( TLOC(I), STATUS )
 1    CONTINUE

*  Copy the two remaining locators into the returned arguments.
      ARYLOC = TLOC(PRIM)
      DELLOC = TLOC(DELE)

*  Return.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( ARYLOC, STATUS )
         CALL DAT_ANNUL( DELLOC, STATUS )
      END IF
      END
