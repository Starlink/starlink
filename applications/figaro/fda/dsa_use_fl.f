      SUBROUTINE DSA_USE_FLAGGED_VALUES( DSAREF, STATUS )
*+
*  Name:
*     DSA_USE_FLAGGED_VALUES

*  Purpose:
*     Indicate that the calling routine will accept bad values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_USE_FLAGGED_VALUES( DSAREF, STATUS )

*  Description:
*     An application uses this routine to indicate that it expects
*     bad values ('flagged' values, or 'magic numbers') in the main
*     data array of an NDF. 'Expects' here merely means that it is
*     prepared to process such values should there be any in the data,
*     and it is not necessary to protect it from receiving such
*     values.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     14 Jul 1988 (ks):
*        Original version.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     06 Feb 1995 (ks):
*        Now allows both this and DSA_USE_QUALITY to be
*        called for the same file.
*     17 Feb 1995 (ks):
*        Now sets and checks QF_HANDLING.
*     26 Nov 1995 (hme):
*        FDA library.
*     28 Nov 1995 (hme):
*        Allow use of both bad values and quality only if
*        DSA_QUALITY_AND_FLAG_OK has been called as well.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
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
      INTEGER SLOT               ! The reference slot

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  It is not allowed to call this routine after data or quality have
*  been mapped.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( DSA__REFDPT(SLOT) .NE. 0 .OR.
     :        DSA__REFQPT(SLOT) .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
            CALL ERR_REP( 'FDA_E055', 'DSA_USE_FLAGGED_VALUES: ' //
     :         'Attempt to declare acceptance of bad values for ' //
     :         'reference slot ^FDA_T001 after data or quality ' //
     :         'have been accessed.', STATUS )
         ELSE IF ( DSA__REFQUA(SLOT) .AND. .NOT. DSA__BADQUA ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
            CALL ERR_REP( 'FDA_E056', 'DSA_USE_FLAGGED_VALUES: ' //
     :         'Attempt to declare acceptance of bad values for ' //
     :         'reference slot ^FDA_T001 after use of quality ' //
     :         'has been declared.', STATUS )
         ELSE
            DSA__REFBAD(SLOT) = .TRUE.
         END IF
      END IF

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
