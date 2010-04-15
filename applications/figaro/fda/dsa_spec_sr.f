      SUBROUTINE DSA_SPECIFIC_STRUCTURE( REF_NAME, STRUCT_ID, MODE,
     :   STRUCTURE, STATUS )
*+
*  Name:
*     DSA_SPECIFIC_STRUCTURE

*  Purpose:
*     Return DTA_ system name of an application-specific sub-structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SPECIFIC_STRUCTURE( DSAREF, IDENT, MODE, STRUCT, STATUS )

*  Description:
*     This routine translates between the DSA reference name and a
*     multi-level HDS structure name in the NDF's Figaro extension on
*     one hand and the DTA structure name on the other.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     IDENT = CHARACTER * ( * ) (Given)
*        A string identifying the sub-structure in question.
*     MODE = CHARACTER * ( * ) (Given)
*        One of blank, 'READ', 'WRITE' or 'UPDATE' (only the first
*        character is significant). If the mode is 'READ' or 'UPDATE',
*        the sub-structure must already exist, and it will be considered
*        an error if it does not. If it is 'WRITE', the sub-structure
*        will be created if it does not exist. If blank, the existence
*        or otherwise of the sub-structure is ignored.
*     STRUCT = CHARACTER * ( * ) (Returned)
*        The DTA_ system name of the sub-structure. This can be used to
*        generate the names of the structure elements (preferably using
*        DTA_CRNAM).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     19 Aug 1987 (ks):
*        Original version.
*     30 Aug 1988 (ks):
*        Warning removed for .OBS.
*     12 Mar 1990 (ks):
*        Now uses DSA__ routines rather than assuming the original
*        Figaro data format.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     16 Jun 1993 (hme):
*        Make local upper case STRUCT_ID longer (80 char.).
*     29 Jan 1996 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) REF_NAME
      CHARACTER * ( * ) STRUCT_ID
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      CHARACTER * ( * ) STRUCTURE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EXIST              ! Whether structure exists
      INTEGER I                  ! Loop variable
      INTEGER REF_SLOT           ! Reference slot number
      CHARACTER * ( 80 ) STRUCT_ID_UC ! Upper case identifier
      CHARACTER * ( 10 ) NDF_ITEM_NAMES( 7 ) ! Structures to warn about
      CHARACTER * ( DAT__SZLOC ) LOC ! An HDS locator

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*  Data Statements:
      DATA NDF_ITEM_NAMES
     :   / 'DATA_ARRAY', 'LABEL     ', 'UNITS     ', 'VARIANCE  ',
     :     'BAD_PIXEL ', 'QUALITY   ', 'AXIS      ' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference slot.
      CALL DSA1_RFND( REF_NAME, REF_SLOT, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500

*  Upper-case identifier.
      STRUCT_ID_UC = STRUCT_ID
      CALL CHR_UCASE( STRUCT_ID_UC )

*  See if this is one of the standard axis or data structures - if so,
*  it isn't applications-specific, and someone is cheating.
      DO 1 I = 1, 7
         IF ( STRUCT_ID_UC .EQ. NDF_ITEM_NAMES(I) ) THEN
            CALL MSG_SETC( 'FDA_T016', STRUCT_ID )
            CALL MSG_OUT( 'FDA_M016', 'Warning: This application is ' //
     :         'treating the standard sub-structure ^FDA_T016 as an ' //
     :         'application-specific structure. While this may ' //
     :         'work, it is bad practice.', STATUS )
         END IF
 1    CONTINUE

*  Construct the name of the required sub-structure.
*  (In the old implementation a DTA name would begin with something
*  representing the container file top level plus any structure
*  specification to indicate where in the tree the NDF was. In practice
*  it was usually the DSA reference name. In this implementation we use
*  the DSA reference name and leave its translation to DTA.)
      STRUCTURE = REF_NAME(:CHR_LEN(REF_NAME)) //
     :   '.MORE.FIGARO.' // STRUCT_ID
      CALL CHR_UCASE( STRUCTURE )

*  If necessary, test to see if the sub-structure does in fact exist.
      IF (MODE .NE. ' ' ) THEN
         CALL DTA1_THERE( STRUCTURE, EXIST, STATUS )

*     Error if it had to exist and didn't.
         IF ( MODE(1:1) .NE. 'W' .AND. MODE(1:1) .NE. 'w' ) THEN
            IF ( .NOT. EXIST ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'FDA_T016', STRUCTURE )
               CALL ERR_REP( 'FDA_E078', 'DSA_SPECIFIC_STRUCTURE: ' //
     :            'Error trying to access the specific structure ' //
     :            '^FDA_T016. The structure does not exist.', STATUS )
               GO TO 500
            END IF
         ELSE

*        If it didn't exist but has to, try to create it.
            IF ( .NOT. EXIST ) THEN
               CALL NDF_XSTAT( DSA__REFID1(REF_SLOT), 'FIGARO',
     :            EXIST, STATUS )
               IF ( .NOT. EXIST ) THEN
                  CALL NDF_XNEW( DSA__REFID1(REF_SLOT),
     :               'FIGARO', 'FIGARO_EXT', 0, 0, LOC, STATUS )
                  CALL DAT_ANNUL( LOC, STATUS )
               END IF
               CALL DTA1_CRVAR( STRUCTURE, 'STRUCT', STATUS )
            END IF

         END IF

      END IF

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
