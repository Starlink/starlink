      SUBROUTINE FIG_HDSDIG( FILE, PATH, MODE, LOC, STATUS )
*+
*  Name:
*     FIG_HDSDIG

*  Purpose:
*     Climb down a hierarchy path in an HDS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_HDSDIG( FILE, PATH, MODE, LOC, STATUS )

*  Description:
*     Given the name of an HDS file and a path down its hierarchy this
*     routine locates the HDS object specified by the path.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the HDS file.
*     PATH = CHARACTER * ( * ) (Given)
*        The path to the HDS object to be located. The path is a string
*        like "TOPLEV.LEV1.LEV2(5,3).LEV3" as would be returned by
*        HDS_TRACE.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode for opening the HDS file, can be 'READ',
*        'WRITE' or 'UPDATE'.
*     LOC = CHARACTER * ( * ) (Returned)
*        The HDS locator to the object specified by FILE and PATH.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02 Oct 1992 (hme):
*        Original version.
*     16 Aug 1993 (hme):
*        This routine calls HDS_OPEN, so it should perhaps do a matching
*        call to HDS_CLOSE.
*     14 Jan 1994 (hme):
*        With HDS 4.1 it definitely shouldn't! Instead we have to make
*        the returned locator a primary locator (a doorstop to prevent
*        closing the file) and then annull the file locator. It is up to
*        the caller to annull the returned locator, thus probably
*        closing the file.
*     20 Mar 1996 (hme):
*        The mechanism for finding the number of commas in a cell spec
*        was flawed. It would probably not work for 3-D and higher.
*        Now use a simple DO loop to check each character inside the
*        parentheses.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Arguments Given:
      CHARACTER * ( * ) FILE
      CHARACTER * ( * ) PATH
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER IL                 ! TLOC index
      INTEGER IDOT1, IDOT2       ! Pointers into PATH
      INTEGER IBRACK, ICOMMA     ! Pointers into LEVEL
      INTEGER NDIM               ! Dimensionality of cell specification
      INTEGER CELL( DAT__MXDIM ) ! Cell specification from LEVEL
      CHARACTER * ( 2*DAT__SZNAM ) LEVEL ! Level from PATH
      CHARACTER * (   DAT__SZNAM ) NAME  ! Level name from LEVEL
      CHARACTER * (   DAT__SZLOC ) TLOC( 0 : 1 ) ! Local HDS locators
      CHARACTER * (   DAT__SZLOC ) FLOC ! Top level locator

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise locator stuff.
*  The returned locator will be invalid in case of a failure.
*  IL is the index for the local locator array TLOC and ranges from zero
*  to one. Thus one can simply refer to "the other" locator as
*  TLOC(1-IL). We need only these two locators to go progressively down
*  the hierarchy of HDS structures. As soon as one locator results in
*  the next one, the old one is annulled and free for re-use.
      LOC = DAT__NOLOC
      IL = 0

*  Open the HDS file (locate the top level).
*  The original locator is retained for a call to HDS_CLOSE, a cloned
*  locator is used to climb down.
      CALL HDS_OPEN( FILE, MODE, FLOC, STATUS )
      CALL DAT_CLONE( FLOC, TLOC(IL), STATUS )

*  Locate in path string where first level name starts and ends.
      IDOT1 = INDEX( PATH, '.' )
      IF ( IDOT1 .NE. 0 ) THEN
         IDOT2 = IDOT1 + INDEX( PATH(IDOT1+1:), '.' )
         IF ( IDOT2 .EQ. IDOT1 ) IDOT2 = CHR_LEN( PATH ) + 1
      ELSE
         IDOT2 = IDOT1
      END IF

*  While path not exhausted.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( IDOT2 .GE. IDOT1+2 ) THEN

*     Extract next level (name with cell number).
         LEVEL = PATH(IDOT1+1:IDOT2-1)

*     Split level into name and cell number.

*     If there is a cell number in the level spec.
         IF ( LEVEL(IDOT2-IDOT1-1:) .EQ. ')' ) THEN

*        Find the start of the cell number in level spec.
            IBRACK = INDEX( LEVEL, '(' )
            IF ( IBRACK .EQ. 0 ) THEN
               CALL DAT_ANNUL( TLOC(IL), STATUS )
               CALL MSG_SETC( 'FIG_HDSDIG_T01', LEVEL )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'FIG_HDSDIG_E01', 'Error extracting ' //
     :            'cell number while climbing down an HDS ' //
     :            ' hierarchy. The invalid level specification is ' //
     :            '^FIG_HDSDIG_T01.', STATUS )
               GO TO 500
            END IF

*        Extract the level name.
            NAME = LEVEL(:IBRACK-1)

*        Find the dimensionality of the cell specification.
            NDIM = 1
            DO 2 ICOMMA = IBRACK + 1, IDOT2 - IDOT1 - 2
               IF ( LEVEL(ICOMMA:ICOMMA) .EQ. ',' ) NDIM = NDIM + 1
 2          CONTINUE

*        Extract the cell number.
            READ ( LEVEL(IBRACK+1:IDOT2-IDOT1-2),
     :         *, IOSTAT = STATUS ) ( CELL(I), I = 1, NDIM )
            IF ( STATUS .NE. 0 ) THEN
               CALL DAT_ANNUL( TLOC(IL), STATUS )
               CALL MSG_SETC( 'FIG_HDSDIG_T01', LEVEL )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'FIG_HDSDIG_E01', 'Error extracting ' //
     :            'cell number while climbing down an HDS ' //
     :            ' hierarchy. The invalid level specification is ' //
     :            '^FIG_HDSDIG_T01.', STATUS )
               GO TO 500
            END IF

*     Else (no cell number in level spec).
         ELSE
            NDIM = 0
            NAME = LEVEL
         END IF

*     Locate name as component.
         CALL DAT_FIND( TLOC(IL), NAME, TLOC(1-IL), STATUS )
         CALL DAT_ANNUL( TLOC(IL), STATUS )
         IL = 1 - IL

*     If there is a cell number, locate cell.
         IF ( NDIM .NE. 0 ) THEN
            CALL DAT_CELL( TLOC(IL), NDIM, CELL, TLOC(1-IL), STATUS )
            CALL DAT_ANNUL( TLOC(IL), STATUS )
            IL = 1 - IL
         END IF

*     Ready for next level, so there is one.
         IDOT1 = IDOT2
         IDOT2 = IDOT1 + INDEX( PATH(IDOT1+1:), '.' )
         IF ( IDOT2 .EQ. IDOT1 ) IDOT2 = CHR_LEN( PATH ) + 1
         GO TO 1
      END IF

*  Clone locator to the one returned.
*  Annul the local locator.
*  Make the returned locator primary (doorstop against closing the
*  file).
*  Annul the now obsolete file locator.
      CALL DAT_CLONE( TLOC(IL), LOC, STATUS )
      CALL DAT_ANNUL( TLOC(IL), STATUS )
      CALL DAT_PRMRY( .TRUE., LOC, .TRUE., STATUS )
      CALL DAT_ANNUL( FLOC, STATUS )

*  Return.
 500  CONTINUE

      END
