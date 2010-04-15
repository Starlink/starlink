      SUBROUTINE DSA_CLOSE_STRUCTURE( DSAREF, STATUS )
*+
*  Name:
*     DSA_CLOSE_STRUCTURE

*  Purpose:
*     Close down an NDF opened by e.g. DSA_INPUT or DSA_OUTPUT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_CLOSE_STRUCTURE( DSAREF, STATUS )

*  Description:
*     This routine closes down the NDF(s) associated with a single
*     DSA reference. The NDF would have been opened with DSA_INPUT,
*     or DSA_OUTPUT etc. This routine checks the NDF for consistency,
*     unmaps all arrays mapped through the given DSA reference, and
*     annuls the NDF accessed and any buffer NDFs used for this
*     reference.
*
*     If the NDF to be closed is in fact temporary and to be copied
*     into a permanent NDF before closure, then it is possible that this
*     routine will quietly close a second NDF as well. The NDF to be
*     closed would be temporary because an open NDF was blocking it from
*     becoming permanent when it was created by DSA_OUTPUT etc. If that
*     blocking NDF is still open when this routine is called, it will
*     have to be closed before the specified NDF can be made permanent
*     and be closed.
*
*     Closing such a blocking NDF is done quietly, no warning or error
*     message will result. If the calling routine later refers to the
*     blocking NDF, errors have to be expected, because the DSA
*     reference name will have become unused.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The DSA reference name associated with the NDF to be closed
*        down.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine attempts to function even when
*        entered with a bad status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     14 Aug 1987 (ks):
*        Original version.
*     05 Jul 1988 (ks):
*        Call to DSA_CHECK_STRUCTURE added.
*     22 Jul 1988 (ks):
*        Test for post processing added.
*     29 Nov 1988 (ks):
*        Call to DSA_FLUSH_FITS added.
*     11 Aug 1989 (ks):
*        Now releases associated work arrays.
*     13 Feb 1990 (ks):
*        DSA_FLUSH_FITS renamed to DSA__FLUSH_FITS.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     17 Dec 1992 (ks):
*        Call to DSA_RENAME_TEMP added.
*     18 Dec 1995 (hme):
*        FDA library.
*     26 Jan 1996 (hme):
*        Check axis/data shape consistency.
*     19 Feb 1996 (hme):
*        Move the action to DSA1_CLOSTR.
*        Translate between application-side status and Starlink status.
*     22 Feb 1996 (hme):
*        If the NDF is temporary and a name for it is stored, copy it to
*        overwrite the named NDF.
*     23 Feb 1996 (hme):
*        Release FITS extension if mapped.
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
      CHARACTER * ( * ) DSAREF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISTAT              ! Global status
      INTEGER SLOT               ! The reference slot

*.

*  Begin error context and translate status.
      ISTAT = STATUS
      CALL ERR_MARK
      STATUS = SAI__OK

*  Find the slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK )  GO TO 500

*  Call the work routine.
      CALL DSA1_CLOSTR( SLOT, STATUS )

*  Tidy up.
 500  CONTINUE

*  Translate status and end error context.
      IF ( ISTAT .NE. 0 ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = ISTAT
      END IF
      CALL ERR_RLSE

*  Return.
      END



      SUBROUTINE DSA1_CLOSTR( SLOT, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_ERR'          ! DAT status values
      INCLUDE 'NDF_ERR'          ! NDF status values

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER SLOT               ! The reference slot

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL ISTEMP             ! Whether NDF is temporary
      LOGICAL ISSAME             ! Whether two NDFs have same base
      LOGICAL IGNORE             ! Ignored
      INTEGER I                  ! Loop index
      INTEGER BSLOT              ! A reference slot
      INTEGER INDF               ! NDF identifier
      INTEGER PLACE              ! NDF placeholder

*.

*  Begin error context.
      CALL ERR_BEGIN( STATUS )

*  If the placeholder is valid, create a trivial NDF on it.
*  Else if the identifier is valid, check consistency.
      IF ( DSA__REFPLC(SLOT) .NE. NDF__NOPL ) THEN
         CALL NDF_NEW( '_REAL', 1, 1, 1, DSA__REFPLC(SLOT),
     :      DSA__REFID1(SLOT), STATUS )
      ELSE IF ( DSA__REFID1(SLOT) .NE. NDF__NOID ) THEN
         CALL DSA1_CHKSTR( SLOT, STATUS )
      END IF

*  Release the FITS extension.
      IF ( DSA__REFFLO(SLOT) .NE. DAT__NOLOC ) THEN
         CALL DAT_UNMAP( DSA__REFFLO(SLOT), STATUS )
         CALL DAT_ANNUL( DSA__REFFLO(SLOT), STATUS )
      END IF

*  Reset all map slots that refer to this reference slot.
      DO 1 I = 1, DSA__MAXMAP
         IF ( DSA__MAPREF(I) .EQ. SLOT ) THEN
            IF ( DSA__MAPUSD(I) ) CALL DSA1_UNMAP( I, STATUS )
         END IF
 1    CONTINUE

*  If any identifiers other than DSA__REFID1 are valid, annul them.
      IF ( DSA__REFID2(SLOT) .NE. NDF__NOID )
     :   CALL NDF_ANNUL( DSA__REFID2(SLOT), STATUS )
      IF ( DSA__REFID3(SLOT) .NE. NDF__NOID )
     :   CALL NDF_ANNUL( DSA__REFID3(SLOT), STATUS )


*  If the NDF is temporary and a name is available.
      CALL NDF_ISTMP( DSA__REFID1(SLOT), ISTEMP, STATUS )
      IF ( ISTEMP .AND. DSA__REFNDF(SLOT) .NE. ' ' ) THEN

*     Attempt to open the named NDF for overwriting.
         CALL NDF_OPEN( DAT__ROOT, DSA__REFNDF(SLOT), 'WRITE',
     :      'NEW', INDF, PLACE, STATUS )

*     If opening fails due to the NDF being currently open.
         IF ( STATUS .EQ. DAT__FILIN .OR. STATUS .EQ. NDF__FILIN ) THEN

*        Annul the error.
            CALL ERR_ANNUL( STATUS )

*        Open the named NDF for read-only.
            CALL NDF_OPEN( DAT__ROOT, DSA__REFNDF(SLOT), 'READ',
     :         'OLD', INDF, PLACE, STATUS )

*        Go through all slots to find the one on which the NDF is open.
            DO 2 BSLOT = 1, DSA__MAXREF
               IF ( DSA__REFUSD(BSLOT) ) THEN
                  CALL NDF_SAME( INDF, DSA__REFID1(BSLOT),
     :               ISSAME, IGNORE, STATUS )
                  IF ( ISSAME ) GO TO 3
               END IF
 2          CONTINUE
               BSLOT = 0
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
               CALL ERR_REP( 'FDA_E088', 'DSA1_CLOSTR: Error ' //
     :            'closing the reference ^FDA_T001. It is ' //
     :            'impossible to copy this output structure to its ' //
     :            'permanent location. The output will have to be ' //
     :            'discarded.', STATUS )
 3          CONTINUE

*        Close the read access.
            CALL NDF_ANNUL( INDF, STATUS )

*        Close the slot.
            IF ( BSLOT .GT. 0 ) THEN

*           This is a slightly simpler version of the code of this
*           routine in general. Fortunately there is no real recursion,
*           we can be sure that this NDF is not temporary and does not
*           imply further closures.

               IF ( DSA__REFPLC(BSLOT) .NE. NDF__NOPL ) THEN
                  CALL NDF_NEW( '_REAL', 1, 1, 1, DSA__REFPLC(BSLOT),
     :            DSA__REFID1(BSLOT), STATUS )
               ELSE IF ( DSA__REFID1(BSLOT) .NE. NDF__NOID ) THEN
                  CALL DSA1_CHKSTR( BSLOT, STATUS )
               END IF

               DO 4 I = 1, DSA__MAXMAP
                  IF ( DSA__MAPREF(I) .EQ. BSLOT ) THEN
                     IF ( DSA__MAPUSD(I) ) CALL DSA1_UNMAP( I, STATUS )
                  END IF
 4             CONTINUE

               IF ( DSA__REFID2(BSLOT) .NE. NDF__NOID )
     :            CALL NDF_ANNUL( DSA__REFID2(BSLOT), STATUS )
               IF ( DSA__REFID3(BSLOT) .NE. NDF__NOID )
     :            CALL NDF_ANNUL( DSA__REFID3(BSLOT), STATUS )
               IF ( DSA__REFID1(BSLOT) .NE. NDF__NOID )
     :            CALL NDF_ANNUL( DSA__REFID1(BSLOT), STATUS )

               DSA__REFUSD(BSLOT) = .FALSE.
               DSA__REFBAD(BSLOT) = .FALSE.
               DSA__REFQUA(BSLOT) = .FALSE.
               DSA__REFRSD(BSLOT) = .FALSE.
               DSA__REFMDD(BSLOT) = .FALSE.
               DSA__REFMDQ(BSLOT) = .FALSE.
               DSA__REFMDV(BSLOT) = .FALSE.
               DO 5 I = 1, NDF__MXDIM
                  DSA__REFRSA(I,BSLOT) = .FALSE.
                  DSA__REFMDC(I,BSLOT) = .FALSE.
                  DSA__REFMDW(I,BSLOT) = .FALSE.
 5             CONTINUE
               DSA__REFID1(BSLOT) = NDF__NOID
               DSA__REFID2(BSLOT) = NDF__NOID
               DSA__REFID3(BSLOT) = NDF__NOID
               DSA__REFPLC(BSLOT) = NDF__NOPL
               DSA__REFDPT(BSLOT) = 0
               DSA__REFQPT(BSLOT) = 0
               DSA__REFFPT(BSLOT) = 0
               DSA__REFFNE(BSLOT) = 0
               DSA__REFNDF(BSLOT) = ' '
               DSA__REFNAM(BSLOT) = ' '
               DSA__REFFLO(BSLOT) = DAT__NOLOC
               DSA__REFLOC(BSLOT) = DAT__NOLOC

            END IF

*        Open the named NDF for overwriting.
            CALL NDF_OPEN( DAT__ROOT, DSA__REFNDF(SLOT), 'WRITE',
     :         'NEW', INDF, PLACE, STATUS )

         END IF

*     Copy from the temporary to the new NDF.
         CALL NDF_SCOPY( DSA__REFID1(SLOT),
     :      'DATA,QUAL,VAR,AXIS,UNITS', PLACE, INDF, STATUS )

*     Annul the new NDF.
         CALL NDF_ANNUL( INDF, STATUS )

      END IF


*  Annul the remaining NDF in the slot.
      IF ( DSA__REFID1(SLOT) .NE. NDF__NOID )
     :   CALL NDF_ANNUL( DSA__REFID1(SLOT), STATUS )


*  Reset the reference slot.
      DSA__REFUSD(SLOT) = .FALSE.
      DSA__REFBAD(SLOT) = .FALSE.
      DSA__REFQUA(SLOT) = .FALSE.
      DSA__REFRSD(SLOT) = .FALSE.
      DSA__REFMDD(SLOT) = .FALSE.
      DSA__REFMDQ(SLOT) = .FALSE.
      DSA__REFMDV(SLOT) = .FALSE.
      DO 6 I = 1, NDF__MXDIM
         DSA__REFRSA(I,SLOT) = .FALSE.
         DSA__REFMDC(I,SLOT) = .FALSE.
         DSA__REFMDW(I,SLOT) = .FALSE.
 6    CONTINUE
      DSA__REFID1(SLOT) = NDF__NOID
      DSA__REFID2(SLOT) = NDF__NOID
      DSA__REFID3(SLOT) = NDF__NOID
      DSA__REFPLC(SLOT) = NDF__NOPL
      DSA__REFDPT(SLOT) = 0
      DSA__REFQPT(SLOT) = 0
      DSA__REFFPT(SLOT) = 0
      DSA__REFFNE(SLOT) = 0
      DSA__REFNDF(SLOT) = ' '
      DSA__REFNAM(SLOT) = ' '
      DSA__REFFLO(SLOT) = DAT__NOLOC
      DSA__REFLOC(SLOT) = DAT__NOLOC


*  Return.
 500  CONTINUE
      CALL ERR_END( STATUS )
      END
