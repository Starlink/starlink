      SUBROUTINE NDG1_MATCH( FMTS, FTYPE, WILD, MATCH, STATUS )
*+
*  Name:
*     NDG_MATCH

*  Purpose:
*     Se if a given file type is included in a list of known NDF file types.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_MATCH( FMTS, FTYPE, WILD, MATCH, STATUS )

*  Description:
*     This routine searches for a supplied file type within the file types
*     included in FMTS, optionally allowing wild-cards to be present
*     in the file type, and returns a flag indicating if it was found.

*  Arguments:
*     FMTS = CHARACTER*(*) (Given)
*        A list of known file formats, in the style of the NDF_FORMATS_OUT 
*        or NDF_FORMATS_IN environment variable.
*     FTYPE = CHARACTER*(*) (Given)
*        The file type to be checked, including the initial ".".
*     WILD = LOGICAL (Given)
*        Should any wild-card characters within FTYPE be treated as
*        wild-cards when comparing FTYPE and FMTS? If not, they must be 
*        matched literally by characters in FMTS.
*     MATCH = LOGICAL (Returned)
*        Is the file type matched by an entry in FMTS?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JAN-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'NDG_CONST'        ! NDG private constants.

*  Arguments Given:
      CHARACTER FMTS*(*)
      CHARACTER FTYPE*(*)
      LOGICAL WILD

*  Arguments Returned:
      LOGICAL MATCH

*  Status:
      INTEGER STATUS             ! Global status

*  Externals:
      INTEGER CHR_LEN
      LOGICAL CHR_WILD

*  Local Variables:
      CHARACTER MAT*(NDG__SZFMT)   ! Wild-card match string
      CHARACTER TEXT*(GRP__SZFNM)  ! File type template
      CHARACTER TEXT2*(GRP__SZFNM) ! Temporary file type template
      INTEGER IAT                ! Index of concatenation point
      INTEGER NSUB               ! No. of substitutions made
*.

*  Initialise the returned flag.
      MATCH = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If wild-cards are to be used, we use CHR_WILD to look for a
*  sub-string in FMTS which matches the supplied file type (enclosed in
*  parentheses). 
      IF( WILD ) THEN
         
*  Add a multi-character wild-card to the start and end of the template 
*  file type text so that a matching sub-string will be found no matter where 
*  it occurs within FMTS. Also enclose the supplied file type in parentheses, 
*  as it is in FMTS.
         TEXT = NDG__WILD2//'('
         IAT = 2
         CALL CHR_APPND( FTYPE, TEXT, IAT )
         CALL CHR_APPND( ')'//NDG__WILD2, TEXT, IAT )

*  CHR_WILD uses * for its multi-character wild card and % for its single
*  character wild-card. If these are not the same as those used by the 
*  native operating system, then we need to put a CHR_WILD escape character
*  "\" in front of any existing CHR_WILD wild cards in the file type 
*  template, and translate the native wild-cards into CHR_WILD wild-cards.
         IF( NDG__WILD1 .NE. '%' ) THEN
            CALL NDG1_SUBST( TEXT, '%', '\\%', .TRUE., TEXT2, NSUB, 
     :                       STATUS )
            TEXT = TEXT2
            CALL CHR_TRCHR( NDG__WILD1, '%', TEXT, STATUS )
         END IF

         IF( NDG__WILD2 .NE. '*' ) THEN
            CALL NDG1_SUBST( TEXT, '*', '\\*', .TRUE., TEXT2, NSUB, 
     :                       STATUS )
            TEXT = TEXT2
            CALL CHR_TRCHR( NDG__WILD2, '*', TEXT, STATUS )
         END IF

*  Now match the file type template against the FMTS string.
         MATCH = CHR_WILD( FMTS, TEXT, MAT )

*  If wild-cards are to be matched as literal characters instead of 
*  wild-cards, search for the file type string in FMTS. 
      ELSE

*  Enclose the supplied file type in parentheses, as it is in FMTS.
         TEXT = '('
         IAT = 1
         CALL CHR_APPND( FTYPE, TEXT, IAT )
         CALL CHR_APPND( ')', TEXT, IAT )

*  Search for the file type string in FMTS. 
         IF( INDEX( FMTS, TEXT( : IAT ) ) .NE. 0 ) MATCH = .TRUE.

      END IF

      END
      
