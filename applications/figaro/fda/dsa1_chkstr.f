      SUBROUTINE DSA1_CHKSTR( SLOT, STATUS )
*+
*  Name:
*     DSA1_CHKSTR

*  Purpose:
*     Tidy up an NDF just prior to its close-down.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_CHKSTR( SLOT, STATUS )

*  Description:
*     This routine should be called just before an NDF is shut down. It
*     checks that it has been treated well. These are the checks for
*     the NDF array components:
*
*      1 If the NDF was re-shaped, but the data were left unchanged, a
*        warning is given. The data component is left alone.
*      2 If the NDF was re-shaped, but the variance was left unchanged,
*        the variance is deleted and a message to that effect is given.
*      3 If the NDF was re-shaped, but the quality was left unchanged,
*        the quality is deleted and a message to that effect is given.
*      4 If the data were modified, but the variance was left unchanged,
*        the variance is deleted and a message to that effect is given.
*      5 If the data were modified, but the quality was left unchanged,
*        a warning message is given.
*
*     And these checks are made for each axis:
*
*      6 If the shape of an axis is incompatible with the shape of the
*        NDF itself, the axis is 'deleted' and a warning message is
*        given to that effect.
*      7 If the axis or the NDF were re-shaped, but the centres were
*        left unchanged, any N-dimensional centre and width array are
*        deleted and a warning message is given to that effect. The
*        compulsory 1-D centre array and any 1-D width array are left
*        unchanged.
*      8 If the axis or the NDF were re-shaped, but the widths were left
*        unchanged, any N-dimensional width array is deleted and a
*        warning message is given to that effect. Any 1-D width array is
*        left unchanged.
*      9 If the centres were modified, but the width was left unchanged,
*        the width is deleted and a message to that effect is given.
*        (This test is new in this implementation.)

*  Arguments:
*     SLOT = INTEGER (Given)
*        The reference slot number associated with the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine tries to do its job even if
*        entered with a bad status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     04 Jul 1988 (ks):
*        Original version.
*     11 Dec 1989 (ks):
*        Now deletes reshaped arrays that were not updated. All
*        knowledge of structure contents moved into DSA__ routines.
*     17 Jan 1990 (ks):
*        Calling sequence for DSA__ERROR_NAME changed.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     16 Oct 1992 (hme):
*        Call DSA_ENCDIM instead of FIG_ENCDIM. Check the AXIS structure
*        array for the NDF case.
*     07 Feb 1995 (ks):
*        Warns about but no longer deletes quality arrays when they
*        might have become invalid.
*     28 Nov 1995 (ks):
*        Now deletes the quality array in in the case where the data has
*        been reshaped and the quality array has not been modified.
*     11 Dec 1995 (ks):
*        Now uses DSA__DELETE_QUALITY to delete the quality array, to
*        make sure the whole structure is deleted.
*     26 Jan 1996 (hme):
*        FDA library. DSA_CHECK_STRUCTURE becomes DSA1_CHKSTR.
*     12 Feb 1996 (hme):
*        Complete review of what checks and actions are necessary.
*     16 Feb 1996 (hme):
*        Review error and message handling.
*     21 Feb 1996 (hme):
*        Handle width. Add eighth and ninth check.
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

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE              ! Whether NDF has AXIS at all
      LOGICAL MATCH              ! True if axis dimensions match data
      LOGICAL FLAGS( NDF__MXDIM ) ! Flags dimensions in data
      INTEGER AXIS               ! Axis number (loop index)
      INTEGER I                  ! Loop index through axis dimensions
      INTEGER J                  ! Loop index through data dimensions
      INTEGER MNDIM              ! Main data dimensionality
      INTEGER MDIMS( NDF__MXDIM ) ! Main data dimensions
      INTEGER NDIM               ! Axis dimensionality
      INTEGER DIMS( NDF__MXDIM ) ! Axis dimensions
      INTEGER NELM               ! Ignored
      CHARACTER * ( DAT__SZNAM ) NAME ! Component name
      CHARACTER * ( DAT__SZNAM ) DELNAM ! Component name and locator
      CHARACTER * ( DAT__SZLOC ) DELLOC ! for deletion of N-D centres
      CHARACTER * ( DAT__SZLOC ) ARYLOC ! Ignored
      CHARACTER * ( DAT__SZLOC ) TLOC1 ! HDS locator
      CHARACTER * ( DAT__SZLOC ) TLOC2 ! HDS locator

*.

*  Begin an error context.
      CALL ERR_BEGIN( STATUS )

*  First check.
      IF ( DSA__REFRSD(SLOT) .AND. .NOT. DSA__REFMDD(SLOT) ) THEN
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
         CALL MSG_OUT( 'FDA_M001', 'Warning: The data ' //
     :      'in the reference ^FDA_T001 have been re-shaped ' //
     :      'but were never updated. The data are probably ' //
     :      'invalid now.', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Second check.
      IF ( DSA__REFRSD(SLOT) .AND. .NOT. DSA__REFMDV(SLOT) ) THEN
         CALL NDF_STATE( DSA__REFID1(SLOT), 'VARIANCE', THERE, STATUS )
         IF ( THERE ) THEN
            CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
            CALL MSG_OUT( 'FDA_M002', 'Warning: The data ' //
     :         'in the reference ^FDA_T001 have been re-shaped ' //
     :         'but the variance array was never updated. ' //
     :         'It will now be deleted.', STATUS )
            CALL NDF_RESET( DSA__REFID1(SLOT), 'VARIANCE', STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Third check.
      IF ( DSA__REFRSD(SLOT) .AND. .NOT. DSA__REFMDQ(SLOT) ) THEN
         CALL NDF_STATE( DSA__REFID1(SLOT), 'QUALITY', THERE, STATUS )
         IF ( THERE ) THEN
            CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
            CALL MSG_OUT( 'FDA_M003', 'Warning: The data ' //
     :         'in the reference ^FDA_T001 have been re-shaped ' //
     :         'but the quality array was never updated. ' //
     :         'It will now be deleted.', STATUS )
            CALL NDF_RESET( DSA__REFID1(SLOT), 'QUALITY', STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Fourth check.
      IF ( DSA__REFMDD(SLOT) .AND. .NOT. DSA__REFMDV(SLOT) ) THEN
         CALL NDF_STATE( DSA__REFID1(SLOT), 'VARIANCE', THERE, STATUS )
         IF ( THERE ) THEN
            CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
            CALL MSG_OUT( 'FDA_M004', 'Warning: The data ' //
     :         'in the reference ^FDA_T001 have been updated ' //
     :         'but the variance array was never updated. ' //
     :         'It will now be deleted.', STATUS )
            CALL NDF_RESET( DSA__REFID1(SLOT), 'VARIANCE', STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Fifth check.
      IF ( DSA__REFMDD(SLOT) .AND. .NOT. DSA__REFMDQ(SLOT) ) THEN
         CALL NDF_STATE( DSA__REFID1(SLOT), 'QUALITY', THERE, STATUS )
         IF ( THERE ) THEN
            CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
            CALL MSG_OUT( 'FDA_M005', 'Warning: The data ' //
     :         'in the reference ^FDA_T001 have been updated ' //
     :         'but the quality array was never updated. ' //
     :         'The quality is possibly invalid now.', STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  If there is no axis in the NDF, we have finished.
      CALL NDF_STATE( DSA__REFID1(SLOT), 'AXIS', THERE, STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      IF ( .NOT. THERE ) GO TO 500

*  Get the main data shape.
      CALL NDF_DIM( DSA__REFID1(SLOT), NDF__MXDIM,
     :   MDIMS, MNDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  For each axis.
      DO 6 AXIS = 1, MNDIM

*     Sixth check.
         CALL DSA1_AXSIZ( SLOT, AXIS, NDF__MXDIM,
     :      NDIM, DIMS, NELM, STATUS )
         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
         MATCH = NDIM .LE. MNDIM .AND. DIMS(1) .EQ. MDIMS(AXIS)
         IF ( MATCH ) THEN
            DO 1 J = 1, MNDIM
               FLAGS(J)=.FALSE.
 1          CONTINUE
            FLAGS(AXIS)=.TRUE.
            DO 4 I = 2, NDIM
               MATCH = .FALSE.
               DO 2 J = 1, MNDIM
                  IF ( DIMS(I) .EQ. MDIMS(J) ) THEN
                     FLAGS(J) = .TRUE.
                     MATCH = .TRUE.
                     GO TO 3
                  END IF
 2             CONTINUE
 3             CONTINUE
               IF ( .NOT. MATCH ) GO TO 5
 4          CONTINUE
 5          CONTINUE
         END IF
         IF ( .NOT. MATCH ) THEN
            CALL MSG_SETI( 'FDA_T002', AXIS )
            CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
            CALL MSG_OUT( 'FDA_M006', 'Warning: The axis number ' //
     :         '^FDA_T002 in the reference ^FDA_T001 has a shape ' //
     :         'incompatible with the shape of the main data array. ' //
     :         'The axis in question will now be deleted.', STATUS )
            CALL DSA1_DELAX( SLOT, AXIS, STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*     Seventh check.
*     When deleting, do the width first, since it must be located
*     relative to the centre array locator.
         IF ( ( DSA__REFRSD(SLOT) .OR. DSA__REFRSA(AXIS,SLOT) ) .AND.
     :        .NOT. DSA__REFMDC(AXIS,SLOT) ) THEN
            CALL DSA1_AXISND( SLOT, AXIS,
     :         THERE, ARYLOC, DELLOC, DELNAM, STATUS )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
            IF ( THERE ) THEN
               CALL MSG_SETI( 'FDA_T002', AXIS )
               CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
               CALL MSG_OUT( 'FDA_M007', 'Warning: The axis ' //
     :            'number ^FDA_T002 in the reference ^FDA_T001 ' //
     :            'was re-shaped, but its centre array was never ' //
     :            'updated. The multi-dimensional centre ' //
     :            'and width arrays will now be deleted.', STATUS )
               CALL DAT_NAME( ARYLOC, NAME, STATUS )
               IF ( NAME .EQ. 'DATA' ) THEN
                  CALL DAT_PAREN( ARYLOC, TLOC2, STATUS )
                  CALL DAT_PAREN( TLOC2,  TLOC1, STATUS )
                  CALL DAT_ANNUL( TLOC2, STATUS )
               ELSE
                  CALL DAT_PAREN( ARYLOC, TLOC1, STATUS )
               END IF
               CALL DAT_THERE( TLOC1, 'WIDTH', THERE, STATUS )
               IF ( THERE ) CALL DAT_ERASE( TLOC1, 'WIDTH', STATUS )
               CALL DAT_ANNUL( TLOC1,  STATUS )
               CALL DAT_ANNUL( ARYLOC, STATUS )
               CALL DAT_ERASE( DELLOC, DELNAM, STATUS )
               CALL DAT_ANNUL( DELLOC, STATUS )
            END IF
         END IF
         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*     Eighth check.
         IF ( ( DSA__REFRSD(SLOT) .OR. DSA__REFRSA(AXIS,SLOT) ) .AND.
     :        .NOT. DSA__REFMDW(AXIS,SLOT) ) THEN
            CALL DSA1_AXISND( SLOT, AXIS,
     :         THERE, ARYLOC, DELLOC, DELNAM, STATUS )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
            IF ( THERE ) THEN
               CALL DAT_NAME( ARYLOC, NAME, STATUS )
               IF ( NAME .EQ. 'DATA' ) THEN
                  CALL DAT_PAREN( ARYLOC, TLOC2, STATUS )
                  CALL DAT_PAREN( TLOC2,  TLOC1, STATUS )
                  CALL DAT_ANNUL( TLOC2, STATUS )
               ELSE
                  CALL DAT_PAREN( ARYLOC, TLOC1, STATUS )
               END IF
               CALL DAT_THERE( TLOC1, 'WIDTH', THERE, STATUS )
               IF ( THERE ) THEN
                  CALL MSG_SETI( 'FDA_T002', AXIS )
                  CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
                  CALL MSG_OUT( 'FDA_M017', 'Warning: The axis ' //
     :               'number ^FDA_T002 in the reference ^FDA_T001 ' //
     :               'was re-shaped, but its width array was never ' //
     :               'updated. The multi-dimensional ' //
     :               'width array will now be deleted.', STATUS )
                  CALL DAT_ERASE( TLOC1, 'WIDTH', STATUS )
               END IF
               CALL DAT_ANNUL( TLOC1,  STATUS )
               CALL DAT_ANNUL( ARYLOC, STATUS )
               CALL DAT_ANNUL( DELLOC, STATUS )
            END IF
         END IF
         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*     Ninth check.
         IF ( DSA__REFMDC(AXIS,SLOT) .AND.
     :        .NOT. DSA__REFMDW(AXIS,SLOT) ) THEN
            CALL NDF_ASTAT( DSA__REFID1(SLOT), 'WIDTH', AXIS,
     :         THERE, STATUS )
            IF ( THERE ) THEN
               CALL MSG_SETI( 'FDA_T002', AXIS )
               CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
               CALL MSG_OUT( 'FDA_M018', 'Warning: The centre array ' //
     :            'for axis number ^FDA_T002 in the reference ' //
     :            '^FDA_T001 has been updated ' //
     :            'but the width array was never updated. ' //
     :            'It will now be deleted.', STATUS )
               CALL NDF_AREST( DSA__REFID1(SLOT), 'WIDTH',AXIS, STATUS )
            END IF
            CALL DSA1_AXISND( SLOT, AXIS,
     :         THERE, ARYLOC, DELLOC, DELNAM, STATUS )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
            IF ( THERE ) THEN
               CALL DAT_NAME( ARYLOC, NAME, STATUS )
               IF ( NAME .EQ. 'DATA' ) THEN
                  CALL DAT_PAREN( ARYLOC, TLOC2, STATUS )
                  CALL DAT_PAREN( TLOC2,  TLOC1, STATUS )
                  CALL DAT_ANNUL( TLOC2, STATUS )
               ELSE
                  CALL DAT_PAREN( ARYLOC, TLOC1, STATUS )
               END IF
               CALL DAT_THERE( TLOC1, 'WIDTH', THERE, STATUS )
               IF ( THERE ) THEN
                  CALL MSG_SETI( 'FDA_T002', AXIS )
                  CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
                  CALL MSG_OUT( 'FDA_M018', 'Warning: The centre ' //
     :               'array for axis number ^FDA_T002 in the ' //
     :               'reference ^FDA_T001 has been updated ' //
     :               'but the width array was never updated. ' //
     :               'It will now be deleted.', STATUS )
                  CALL DAT_ERASE( TLOC1, 'WIDTH', STATUS )
               END IF
               CALL DAT_ANNUL( TLOC1,  STATUS )
               CALL DAT_ANNUL( ARYLOC, STATUS )
               CALL DAT_ANNUL( DELLOC, STATUS )
            END IF
         END IF
         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

 6    CONTINUE

*  End the error context.
 500  CONTINUE
      CALL ERR_END( STATUS )
      END
