      SUBROUTINE SCULIB_SPLIT_FILE_SPEC( NAME, MAX_SPEC, FILENAME,
     :     NSPEC, DATA_SPEC, STATUS)
*+
*  Name:
*     SCULIB_SPLIT_FILE_SPEC

*  Purpose:
*     Splits a string into a filename and SCUBA section

*  Invocation:
*      SUBROUTINE SCULIB_SPLIT_FILE_SPEC( NAME, MAX_SPEC, FILENAME, NSPEC, 
*     :     DATA_SPEC, STATUS)

*  Description:
*     This routine takes a string and returns the filename (ie text
*     before a {) and the data specifications. Each specification
*     is indicated by a {}. More than one spec can be returned.

*  Arguments:
*     NAME = CHAR (Given)
*        Input string
*     MAX_SPEC = INTEGER (Given)
*        Max number of specifications allowed
*     FILENAME = CHAR (Returned)
*        The name of the file (text before first spec)
*     NSPEC = INTEGER (Returned)
*        Number of specs found/returned (can be 0)
*     DATA_SPEC(NSPEC) = CHAR (Returned)
*        The actual specification for the SCUBA section
*     STATUS = INTEGER (Given and Returned)
*        Global Status value

*  Implementation Status:

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL: John Lightfoot (RoE)
*     {enter_new_authors_here}
 
*  History:
*     1997 May 14 (TIMJ):
*       Original version (extract from REDS_WTFN_REBIN.F)
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
 
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions

*  Arguments Given:
      INTEGER MAX_SPEC
      CHARACTER*(*) NAME

*  Arguments Returned:
      CHARACTER*(*) DATA_SPEC(MAX_SPEC)
      CHARACTER*(*) FILENAME
      INTEGER NSPEC

*  Status:
      INTEGER STATUS                 ! Global status

*  External references:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER*(1) CCURLY          ! Different search string
      INTEGER IPOSN                  ! Current position in string
      CHARACTER*(1) OCURLY           ! Search string
      INTEGER SLENGTH                ! Length of input string
      INTEGER START                  ! Pos of opening brace

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Find length of input string
      SLENGTH = CHR_LEN(NAME)

*     Set initial position to zero
      IPOSN = 1
      NSPEC = 0

*     Set search string
      OCURLY = '{'
      CCURLY = '}'
      
*     First extract the filename from the section

      CALL CHR_FIND(NAME, OCURLY, .TRUE., IPOSN)

      IF (IPOSN .GT. SLENGTH) THEN

*     There is only the name
         FILENAME = NAME(:SLENGTH)
         NSPEC = 0

      ELSE IF (IPOSN .EQ. 1) THEN

*     Oops, no name at all
         NSPEC = 0
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_SPLIT_FILE_SPEC: no filename '//
     :        'specified', STATUS)

      ELSE

*     Extract the name
         FILENAME = NAME(:IPOSN - 1)
         
      END IF

*     Now loop through all the specifications

      DO WHILE (IPOSN .LT. SLENGTH) 

*     Find the start of the specification
         CALL CHR_FIND(NAME, OCURLY, .TRUE., IPOSN)

         START = IPOSN

*     Find the end
         CALL CHR_FIND(NAME, CCURLY, .TRUE., IPOSN)

*     Make sure there is a closing brace before I store the spec
         IF (IPOSN .LE. SLENGTH) THEN
            NSPEC = NSPEC + 1
            DATA_SPEC(NSPEC) = NAME(START:IPOSN)
         END IF

      END DO

      END 

