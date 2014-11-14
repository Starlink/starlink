      SUBROUTINE DSA_NAMED_OUTPUT( DSAREF, NDFNAM, BASREF,
     :   NOCOPY, NEW, STATUS )
*+
*  Name:
*     DSA_NAMED_OUTPUT

*  Purpose:
*     Open an NDF for output NDF using a name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_NAMED_OUTPUT( DSAREF, NDFNAM, BASREF,
*        NOCOPY, NEW, STATUS )

*  Description:
*     This routine takes the name of an NDF. The NDF is then opened and
*     associated with the given reference name.
*     The NDF name can be a filename, or a filename combined with an HDS
*     structure name within the file. A full example might be
*
*        myfile.more.some.struct(5).data
*
*     If a model NDF is specified via the base DSA reference and NOCOPY
*     is 0, then the new NDF will be a copy of the model. Otherwise the
*     output NDF will be initially undefined.
*
*     If the NDF cannot be opened as a new NDF because this would
*     overwrite an NDF that is currently open, and if the NEW argument
*     is 0, then this routine opens a temporary NDF instead and defers
*     overwriting the intended output to the time when the temporary NDF
*     is to be closed (see DSA_CLOSE_STRUCTURE).
*
*     Note that the following call sequence is frowned upon:
*
*        CALL PAR_RDCHAR()
*        CALL DSA_NAMED_OUTPUT()
*
*     The PAR_RDCHAR may cause at least inconsistencies in the
*     user interface. DSA_NAMED_* should be used if the application
*     genuinely knows the NDF name without consulting the parameter
*     system. Applications that use the above sequence should instead:
*
*        CALL DSA_OUTPUT()
*        CALL DSA_GET_ACTUAL_NAME()
*
*     In this sequence the NDF name is acquired from the parameter
*     system in a consistent way and then made available to the
*     application for output to the terminal, an ASCII file or a plot
*     label.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name to be associated with the opened NDF.
*        Only the first 16 characters are significant. The
*        name is case-insensitive.
*     NDFNAM = CHARACTER * ( * ) (Given)
*        The name of the NDF.
*     BASREF = CHARACTER * ( * ) (Given)
*        The reference name of an already open NDF of which
*        the new NDF should be a copy. Ignored, if NOCOPY is 1.
*     NOCOPY = INTEGER (Given)
*        If zero and BASREF is not blank, then the model NDF is
*        copied to the new NDF.
*     NEW = INTEGER (Given)
*        If zero overwriting a currently open NDF is to be allowed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     14 Jul 1987 (ks):
*        Original version.
*     21 Jul 1988 (ks):
*        Additional common items initialised.
*     09 Sep 1988 (ks):
*        PARM_VALUE initialised.
*     28 Nov 1988 (ks):
*        DSA_INIT_REF_SLOT now used for initialisation.
*     14 Feb 1989 (ks):
*        Comments revised.
*     15 Jan 1990 (ks):
*        Call to DSA_SET_FILE_TYPE added.
*     19 Jan 1990 (ks):
*        DSA_SET_FILE_TYPE is now DSA__etc.  Use of new routine
*        DSA__SAME_FILE_FORMAT added.
*     15 Feb 1990 (ks):
*        Call was still to DSA_SET_FILE_TYPE, and not DSA__etc!
*        Call corrected.
*     26 Feb 1990 (ks):
*        Explicit knowledge of data structure item names (.X,
*        .Y etc) removed.  Now uses DSA__ routines rather than
*        assuming original Figaro format.
*     12 Mar 1990 (ks):
*        Parameters to DSA__TOP_ITEM_TYPE revised.
*     15 Feb 1991 (ks):
*        Now forces file extension to match that for the basis file type.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     25 Nov 1995 (hme):
*        FDA library.
*     12 Dec 1995 (hme):
*        Delete (rather than annull) the NDF if the model is not
*        available.
*     18 Dec 1995 (hme):
*        Reviewed use of ERR_BEGIN/END.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     22 Feb 1996 (hme):
*        If opening output fails because it is already open, open a
*        temporary NDF instead.
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
      INCLUDE 'DAT_ERR'          ! DAT status values
      INCLUDE 'NDF_ERR'          ! NDF status values

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) NDFNAM
      CHARACTER * ( * ) BASREF
      INTEGER NOCOPY
      INTEGER NEW

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! Slot number
      INTEGER BSLOT              ! Model slot number
      CHARACTER * ( 16 ) REFUC   ! Given reference in upper case

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Upper case reference name.
      REFUC = DSAREF
      CALL CHR_UCASE( REFUC )

*  Try to find the reference, in order to check that the same name is
*  not used twice.
      DO 1 SLOT = 1, DSA__MAXREF
         IF ( DSA__REFUSD(SLOT) .AND.
     :        DSA__REFNAM(SLOT) .EQ. REFUC ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
            CALL ERR_REP( 'FDA_E037', 'DSA_NAMED_OUTPUT: ' //
     :         'Reference name ^FDA_T001 already in use.', STATUS )
            GO TO 500
         END IF
 1    CONTINUE

*  Find a free slot for the NDF identifier or placeholder.
      DO 2 SLOT = 1, DSA__MAXREF
         IF ( .NOT. DSA__REFUSD(SLOT) ) GO TO 3
 2    CONTINUE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', REFUC )
         CALL ERR_REP( 'FDA_E038', 'DSA_NAMED_OUTPUT: ' //
     :      'No slot left for reference ^FDA_T001.', STATUS )
         GO TO 500
 3    CONTINUE

*  Attempt to open the named NDF for write access.
      CALL NDF_OPEN( DAT__ROOT, NDFNAM, 'WRITE',
     :   'NEW', DSA__REFID1(SLOT), DSA__REFPLC(SLOT), STATUS )

*  If failure due to the named NDF being open already.
*  Annul the error.
*  Open a new temporary NDF instead of the permanent one.
*  Store intended name in common block.
      IF ( NEW .EQ. 0 .AND.
     :   ( STATUS .EQ. DAT__FILIN .OR. STATUS .EQ. DAT__FILCR .OR.
     :     STATUS .EQ. NDF__FILIN ) ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL NDF_TEMP( DSA__REFPLC(SLOT), STATUS )
         IF ( STATUS .EQ. SAI__OK ) DSA__REFNDF(SLOT) = NDFNAM
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Copy from model, if that is asked for.
      IF ( BASREF .NE. ' ' .AND. NOCOPY .EQ. 0 ) THEN

*     Find the slot for the model NDF.
*     If we failed to find the model, we must first turn the placeholder
*     for the new NDF into a minimal NDF and then delete it. There is
*     no other way to get rid of a place holder.
         CALL DSA1_RFND( BASREF, BSLOT, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_BEGIN( STATUS )
               CALL NDF_NEW( '_REAL', 1, 1, 1, DSA__REFPLC(SLOT),
     :            DSA__REFID1(SLOT), STATUS )
            CALL ERR_END( STATUS )
            CALL NDF_DELET( DSA__REFID1(SLOT), STATUS )
            GO TO 500
         END IF

*     Propagate from model to ouput NDF (title, label, history and
*     extensions are propagated by default and need no mention here).
*     If this fails, we get back invalid placeholder and identifier.
*     That's just fine.
         CALL NDF_SCOPY( DSA__REFID1(BSLOT),
     :      'DATA,QUAL,VAR,AXIS,UNITS', DSA__REFPLC(SLOT),
     :      DSA__REFID1(SLOT), STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Fill in the slot.
*  There is a neat little trick in this routine: We fill in the slot only
*  at the end. Therefore, if the caller gave as model the same reference
*  than the new reference (DSAREF == BASREF) then the model refrence would
*  simply not be found, since no slot has yet been assigned.
      DSA__REFUSD(SLOT) = .TRUE.
      DSA__REFBAD(SLOT) = .FALSE.
      DSA__REFQUA(SLOT) = .FALSE.
      DSA__REFID2(SLOT) = NDF__NOID
      DSA__REFDPT(SLOT) = 0
      DSA__REFQPT(SLOT) = 0
      DSA__REFNAM(SLOT) = REFUC

*  Tidy up.
 500  CONTINUE

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
