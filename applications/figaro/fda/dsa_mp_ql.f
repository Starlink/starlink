      SUBROUTINE DSA_MAP_QUALITY( DSAREF, MODE, TYPE,
     :   ADDRESS, MSLOT, STATUS )
*+
*  Name:
*     DSA_MAP_QUALITY

*  Purpose:
*     Map the quality array in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_MAP_QUALITY( DSAREF, MODE, TYPE, ADDRESS, MSLOT, STATUS )

*  Description:
*     This routine maps the quality array in an NDF. If there is in fact
*     no quality array, then one will be created and filled with zeros,
*     hence flagging all pixels as good.
*
*     If the main data array contains flagged values, and the
*     application has not indicated through a call to
*     DSA_USE_FLAGGED_VALUES that it wants to handle these directly,
*     then any flagged values in the data array will be removed and the
*     corresponding elements of the quality array will be set.
*
*     The compulsory calling sequence for applications that use quality
*     is (other processing can be interspersed):
*
*        DSA_USE_QUALITY()
*        DSA_MAP_DATA()
*        DSA_MAP_QUALITY()
*
*     Access to both arrays is achieved by the DSA system when the data
*     are mapped. Therefore the intent to use quality must be declared
*     beforehand. Since the quality incorporates bad-value information
*     from the data and since this is definitive only after the data
*     have been accessed with a certain data type, the application
*     cannot map quality before the data have been accessed.
*
*     Contrary to previous implementations, the map type must be 'BYTE'
*     and the actual map type in NDF speak is '_UBYTE'.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode, 'READ', 'WRITE', or 'UPDATE'.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type to be used to map the array. If this is not
*        'BYTE' an error results.
*     ADDRESS = INTEGER (Returned)
*        The memory address of the mapped array.
*     MSLOT = INTEGER (Returned)
*        The map slot, a handle that can be used to unmap this array
*        later.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22 Jul 1988 (ks):
*        Original version.
*     08 Sep 1989 (ks):
*        Call to DSA_MAP_ARRAY now sets propagation flag false.
*     11 Dec 1989 (ks):
*        Now sets QUAL_UPDATE flag on a write or update mapping.
*     21 Feb 1990 (ks):
*        Uses DSA__QUAL_NAME to remove assumptions about
*        file format details.
*     22 Apr 1991 (ks):
*        Now uses DSA__CREATE_QUAL_ENV to create the
*        environment for the quality array - allows support
*        of structured arrays including BADBITS values.
*     01 May 1991 (ks):
*        Now gets BADBITS value and tests to see if this
*        is likely to cause problems.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     16 Jun 1993 (ks):
*        Now sets 'quality exist' flag if array created.
*     26 Oct 1994 (ks):
*        Now uses new calling sequence for DSA_MAP_ARRAY.
*     03 Feb 1995 (ks):
*        Now supports files with both flagged data values and
*        quality arrays.
*     07 Feb 1995 (ks):
*        DSA_MAP_QUALITY now renamed to DSA_ACT_MAP_QUALITY
*        and this new DSA_MAP_QUALITY becomes merely a
*        wrap-up for that routine.
*     26 Nov 1995 (hme):
*        FDA library.
*     18 Dec 1995 (hme):
*        Review use of common variables. Now data and quality are always
*        mapped on the same slot.
*     18 Jan 1996 (hme):
*        When looking for the map slot, do not insist on 'DATA,QUALITY',
*        since some map slots have only 'DATA' as the component to be
*        unmapped. This is the case when the quality is in a buffer, but
*        the data are not.
*     12 Feb 1996 (hme):
*        For UW access mark reference as quality-modified.
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
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER ADDRESS
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot

*  Internal References:
      LOGICAL CHR_SIMLR          ! Case insensitive comparison

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Check the given type.
      IF ( .NOT. CHR_SIMLR( 'BYTE', TYPE ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', DSAREF )
         CALL MSG_SETC( 'FDA_T004', TYPE )
         CALL ERR_REP( 'FDA_E020', 'DSA_MAP_QUALITY: ' //
     :      'Attempt to map the quality for reference ' //
     :      '^FDA_T001 with type ^FDA_T004. Can map quality ' //
     :      'only as type BYTE.', STATUS )
         GO TO 500
      END IF

*  Find the reference slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  If use of quality has not been declared, the compulsory call
*  sequence has been violated.
      IF ( .NOT. DSA__REFQUA(SLOT) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
         CALL ERR_REP( 'FDA_E021', 'DSA_MAP_QUALITY: ' //
     :      'Attempt to map the quality for reference ' //
     :      '^FDA_T001 without declaring such intent before ' //
     :      'accessing the data.', STATUS )
         GO TO 500
      END IF

*  If data has not been accessed yet, the compulsory call
*  sequence has been violated.
      IF ( DSA__REFDPT(SLOT) .EQ. 0 .OR. DSA__REFQPT(SLOT) .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
         CALL ERR_REP( 'FDA_E022', 'DSA_MAP_QUALITY: Attempt ' //
     :      'to map the quality for reference ^FDA_T001 ' //
     :      'without accessing the data beforehand.', STATUS )
         GO TO 500
      END IF

*  Find the map slot that was used.
*  It refers back to this reference slot and maps the component list
*  'DATA,QUALITY' (or just 'DATA' if the quality is in a buffer NDF).
      DO 1 MSLOT = 1, DSA__MAXMAP
         IF ( DSA__MAPUSD(MSLOT) .AND.
     :        DSA__MAPREF(MSLOT) .EQ. SLOT .AND.
     :        DSA__MAPNAM(MSLOT)(:4) .EQ. 'DATA' ) GO TO 2
 1    CONTINUE
         MSLOT = 1
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
         CALL ERR_REP( 'FDA_E023', 'DSA_MAP_QUALITY: ' //
     :      'Failed to find quality map slot for reference ' //
     :      '^FDA_T001.', STATUS )
         GO TO 500
 2    CONTINUE

*  Set the array address.
      ADDRESS = DSA__REFQPT(SLOT)

*  Set the quality-modified flag.
      IF ( INDEX( 'UuWw', MODE(:1) ) .NE. 0 )
     :   DSA__REFMDQ(SLOT) = .TRUE.

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
