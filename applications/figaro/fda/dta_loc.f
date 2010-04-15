      SUBROUTINE DTA_LOC( PATH, LOC, STATUS )
*+
*  Name:
*     DTA_LOC

*  Purpose:
*     Return HDS locator for named data structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTA_LOC( PATH, LOC, DTA_STATUS )

*  Description:
*     This routine looks up a named structure, and returns an HDS
*     locator for it. Such a locator is the principal means to access
*     HDS structures via the HDS library (as opposed to the FDA
*     library). Note that the locator will have to be annulled
*     after use by a call to DTA_ANNUL or DAT_ANNUL.

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The DTA structure name.
*     LOC = CHARACTER * ( * ) (Returned)
*        The HDS locator.
*     DTA_STATUS = INTEGER (Returned)
*        The DTA status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acd: Clive Davenhall (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     05 Mar 1996 (hme):
*        Original version.
*     05 Mar 1996 (hme):
*        Allow for temporary HDS structure during creation by structure
*        definition.
*     21 Dec 2000 (acd):
*        Removed unused variable.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PATH

*  Arguments Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Call the work routine.
      CALL DTA1_LOC( PATH, LOC, STATUS )

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END




      SUBROUTINE DTA1_LOC( PATH, LOC, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) PATH

*  Arguments Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL CANWRI             ! Whether write access available
      INTEGER SLOT               ! The reference slot
      INTEGER IL                 ! TLOC index
      INTEGER IDOT1, IDOT2, IBRA ! Pointers into PATH
      INTEGER IGNOR1, IGNOR2     ! Ignored
      INTEGER NDIM               ! Dimensionality of cell specification
      INTEGER DIMS( DAT__MXDIM ) ! Cell specification from LEVEL
      CHARACTER * (   DAT__SZLOC ) TLOC( 0 : 1 ) ! Local HDS locators
      CHARACTER * ( 2*DAT__SZNAM ) LEVEL ! Level from PATH
      CHARACTER * (   DAT__SZNAM ) NAME  ! Level name from LEVEL

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initial index into array of locators.
      IL = 0

*  Find top level name in PATH.
      CALL DTA1_SPLIT( PATH, DAT__MXDIM,
     :   IDOT1, IDOT2, IBRA, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( IDOT1 .EQ. 0 ) IDOT1 = 1 + CHR_LEN( PATH )

*  Get the locator for this first level. The first level is a DSA
*  reference name, so we want to locate the NDF referred to.
      CALL DSA1_RFND( PATH(:IDOT1-1), SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( DSA__REFID1(SLOT) .NE. NDF__NOID ) THEN
         CALL NDF_ISACC( DSA__REFID1(SLOT), 'WRITE', CANWRI, STATUS )
         IF ( CANWRI ) THEN
            CALL NDF_LOC( DSA__REFID1(SLOT), 'UPDATE',TLOC(IL),STATUS )
         ELSE
            CALL NDF_LOC( DSA__REFID1(SLOT), 'READ', TLOC(IL), STATUS )
         END IF
      ELSE
         CALL DAT_CLONE( DSA__REFLOC(SLOT), TLOC(IL), STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Locate in path string where first level name starts and ends.
*  If there is no level specification, IDOT1=IDOT2=0.
      IDOT1 = INDEX( PATH, '.' )
      IF ( IDOT1 .NE. 0 ) THEN
         IF ( IDOT1 .LT. CHR_LEN(PATH) ) THEN
            IDOT2 = IDOT1 + INDEX( PATH(IDOT1+1:), '.' )
            IF ( IDOT2 .EQ. IDOT1 ) IDOT2 = CHR_LEN( PATH ) + 1
         ELSE
            IDOT1 = 0
            IDOT2 = 0
         END IF
      ELSE
         IDOT2 = IDOT1
      END IF

*  While path not exhausted.
 3    CONTINUE                  ! Start of 'DO WHILE' loop
      IF ( IDOT2 .GE. IDOT1+2 ) THEN

*     Extract next level (name with cell number).
         LEVEL = PATH(IDOT1+1:IDOT2-1)

*     Split into component name and cell specification.
         CALL DTA1_SPLIT( LEVEL, DAT__MXDIM,
     :      IGNOR1, IGNOR2, IBRA, NDIM, DIMS, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         IF ( IBRA .EQ. 0 ) IBRA = 1 + CHR_LEN( LEVEL )

*     Extract the level name.
         NAME = LEVEL(:IBRA-1)

*     Locate name as component. Swap locators.
         CALL DAT_FIND( TLOC(IL), NAME, TLOC(1-IL), STATUS )
         CALL DAT_ANNUL( TLOC(IL), STATUS )
         IL = 1 - IL

*     If there is a cell number, locate cell.
*     If not, the structure we look for does not exist either, we can
*     bail out of the path parsing.
         IF ( NDIM .NE. 0 ) THEN
            CALL DAT_CELL( TLOC(IL), NDIM, DIMS, TLOC(1-IL), STATUS )
            CALL DAT_ANNUL( TLOC(IL), STATUS )
            IL = 1 - IL
         END IF

*     Ready for next level, so there is one.
*     If IDOT2 just beyond end of string, next level is empty and loop
*     is exited (IDOT1=IDOT2 means there is no string between them.).
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         IDOT1 = IDOT2
         IF ( IDOT1 .LT. CHR_LEN(PATH) ) THEN
            IDOT2 = IDOT1 + INDEX( PATH(IDOT1+1:), '.' )
            IF ( IDOT2 .EQ. IDOT1 ) IDOT2 = CHR_LEN( PATH ) + 1
         END IF
         GO TO 3
      END IF

*  Return.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL DAT_ANNUL( TLOC(IL), STATUS )
      LOC = TLOC(IL)
      END
