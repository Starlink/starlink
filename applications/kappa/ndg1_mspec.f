      SUBROUTINE NDG1_MSPEC( FMTS, FDIRN, FNAME, FTYPE, COMP, SLICE, 
     :                       WILD, SPEC, FORM, STATUS )
*+
*  Name:
*     NDG1_MSPEC

*  Purpose:
*     Make a file specification from its component parts.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_MSPEC( FMTS, FDIRN, FNAME, FTYPE, COMP, SLICE, 
*                      WILD, SPEC, FORM, STATUS )

*  Description:
*     This file is Unix specific.
*
*     The returned file specification (SPEC) is created from the supplied
*     component parts using the same conventions as NDG1_HSPEC. If
*     NDG1_HSPEC is used to split a file specification up into its component
*     parts, this routine may be used to re-create the file specification
*     from the values returned by NDG1_HSPEC.
*
*     If a file type is supplied but no HDS component path is supplied, then 
*     form 1) is used if the file type is included in the list of known 
*     formats supplied in FMTS. Form 2) is used if the file type is not
*     included in FMTS.
*
*     If an HDS component path is supplied but no file type is supplied,
*     then form 3) is used.
*
*     If both an HDS component path and a file type are supplied then
*     form 2) is used (whether or not the file type is included in FMTS).

*  Arguments:
*     FMTS = CHARACTER*(*) (Given)
*        A list of known NDF file formats, in the style of the 
*        NDF_FORMATS_OUT or NDF_FORMATS_IN environment variable.
*     FDIRN = CHARACTER*(*) (Given)
*        The directory path, including the final "/".
*     FNAME = CHARACTER*(*) (Given)
*        The file base name (without file type).
*     FTYPE = CHARACTER*(*) (Given)
*        The file type, including the initial ".".
*     COMP = CHARACTER*(*) (Given)
*        The HDS component path, including the initial ".".
*     SLICE = CHARACTER*(*) (Given)
*        The NDF slice specification, including the enclosing parenthesise.
*     WILD = LOGICAL (Given)
*        Should wild-card characters within FTYPE be used as wild-cards 
*        when searching for the file type within FMTS? If not, they must 
*        be matched literally by characters in FMTS.
*     SPEC = CHARACTER*(*) (Returned)
*        The complete file specification.
*     FORM = INTEGER (Returned)
*        The form of the file spec returned in SPEC (1, 2 or 3 - see 
*        NDG1_HSPEC for a description of these values).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JAN-1998 (DSB):
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

*  Arguments Given:
      CHARACTER FMTS*(*) 
      CHARACTER FDIRN*(*) 
      CHARACTER FNAME*(*) 
      CHARACTER FTYPE*(*) 
      CHARACTER COMP*(*) 
      CHARACTER SLICE*(*) 
      LOGICAL WILD

*  Arguments Returned:
      CHARACTER SPEC*(*) 
      INTEGER FORM

*  Status:
      INTEGER STATUS             ! Global status

*  Externals:
      INTEGER CHR_LEN

*  Local Variables:
      INTEGER IAT                ! Index of concatenation point
      LOGICAL MATCH              ! Was FTYPE matched by an entry in FMTS?
*.

*  Set the returned file specification blank.
      SPEC = ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Decide on the form to use for the returned file specification...

*  If both COMP and FTYPE have been supplied, use form 2).
      IF( COMP .NE. ' ' .AND. FTYPE .NE. ' ' ) THEN
         FORM = 2

*  If COMP has been supplied but not FTYPE, use form 3).
      ELSE IF( COMP .NE. ' ' ) THEN
         FORM = 3

*  If FTYPE has been supplied but not COMP, then the form used depends
*  on whether or not the file type is included in FMTS.
      ELSE IF( FTYPE .NE. ' ' ) THEN
         CALL NDG1_MATCH( FMTS, FTYPE, WILD, MATCH, STATUS )

*  If found, use form 1), otherwise use form 2).
         IF( MATCH ) THEN
            FORM = 1
         ELSE
            FORM = 2
         END IF

*  If neither COMP nor FTYPE have been supplied, use form 1).
      ELSE 
         FORM = 1
      END IF

*  Construct the file specification according to the form being used...
*  Form 1:  dir/name.type(slice)
      IF( FORM .EQ. 1 ) THEN
         SPEC = FDIRN
         IAT = CHR_LEN( FDIRN )
         CALL CHR_APPND( FNAME, SPEC, IAT )
         CALL CHR_APPND( FTYPE, SPEC, IAT )
         CALL CHR_APPND( SLICE, SPEC, IAT )

*  Form 2:  "dir/name.type".comp(slice)
      ELSE IF( FORM .EQ. 2 ) THEN
         SPEC = '"'
         IAT = 1
         CALL CHR_APPND( FDIRN, SPEC, IAT )
         CALL CHR_APPND( FNAME, SPEC, IAT )
         CALL CHR_APPND( FTYPE, SPEC, IAT )
         CALL CHR_APPND( '"', SPEC, IAT )
         CALL CHR_APPND( COMP, SPEC, IAT )
         CALL CHR_APPND( SLICE, SPEC, IAT )

*  Form 3:  dir/name.comp(slice)
      ELSE
         SPEC = FDIRN
         IAT = CHR_LEN( FDIRN )
         CALL CHR_APPND( FNAME, SPEC, IAT )
         CALL CHR_APPND( COMP, SPEC, IAT )
         CALL CHR_APPND( SLICE, SPEC, IAT )

      END IF

      END
