      SUBROUTINE NDG1_HSPEC( SPEC, FMTS, WILD, FDIRN, FNAME, FTYPE,
     :                       COMP, SLICE, FORM, STATUS )
*+
*  Name:
*     NDG1_HSPEC

*  Purpose:
*     Split a file specification up into its component parts.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_HSPEC( SPEC, FMTS, WILD, FDIRN, FNAME, FTYPE, COMP, 
*                     SLICE, FORM, STATUS )

*  Description:
*     This file is Unix specific.
*
*     The supplied file specification (SPEC) is assumed to conform to one
*     of the following forms:
*
*     1) dir/name.type(slice)
*     2) "dir/name.type".comp(slice)
*     3) dir/name.comp(slice)
*
*     where:
*        - dir is a directory path, which may be null (in which case the
*        following "/" should be omitted).
*        - name is the base name of the file.
*        - type is a file type, which may be null (in which case the
*        initial "." should be omitted). This file type may contain dots
*        (eg ".sdf.Z"), and should be one of the file types listed in
*        FMTS.
*        - comp is an HDS component path, which may be null (in which case
*        the initial "." should be omitted). 
*        - slice is an NDF slice specification, which may be null (in
*        which case the enclosing parenthesise should be omitted).
*
*     Form 2) is recognised by the NDF_ library but is not documented
*     anywhere (that I can find). Forms 2) and 3) can only be used to 
*     specify native NDF structures. Form 1) should be used for foreign 
*     format NDFs. Forms 1) and 3) are distinguished by comparing the
*     sub-string between the base name and the slice with the file types 
*     listed in FMTS. If the sub-string matches any of the file types
*     then the form 1) is assumed. Otherwise, form 3) is assumed.

*  Arguments:
*     SPEC = CHARACTER*(*) (Given)
*        The complete file specification.
*     FMTS = CHARACTER*(*) (Given)
*        A list of known file formats, in the style of the NDF_FORMATS_OUT 
*        or NDF_FORMATS_IN environment variable.
*     WILD = LOGICAL (Given)
*        Should wild-card characters within the file type implied by SPEC
*        be used as wild-cards when searching for the file type within
*        FMTS? If not, they must be matched literally by characters in
*        FMTS.
*     FDIRN = CHARACTER*(*) (Returned)
*        The directory path implied by SPEC, including the final "/".
*     FNAME = CHARACTER*(*) (Returned)
*        The file base name (without file type) implied by SPEC.
*     FTYPE = CHARACTER*(*) (Returned)
*        The file type implied by SPEC, including the initial ".".
*     COMP = CHARACTER*(*) (Returned)
*        The HDS component path implied by SPEC, including the initial ".".
*     SLICE = CHARACTER*(*) (Returned)
*        The NDF slice specification implied by SPEC, including the
*        enclosing parenthesise.
*     FORM = INTEGER (Returned)
*        The form of the supplied file specification. This will be 1, 2 or
*        3 corresponding to the values described earlier in this prologue.
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
      CHARACTER SPEC*(*) 
      CHARACTER FMTS*(*) 
      LOGICAL WILD

*  Arguments Returned:
      CHARACTER FDIRN*(*) 
      CHARACTER FNAME*(*) 
      CHARACTER FTYPE*(*) 
      CHARACTER COMP*(*) 
      CHARACTER SLICE*(*) 
      INTEGER FORM

*  Status:
      INTEGER STATUS             ! Global status

*  Externals:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER DNT*(GRP__SZFNM)  ! Group type string
      INTEGER DOT                ! The index of the first dot
      INTEGER F                  ! Index of first non-blank character
      INTEGER IAT                ! Index of concatenation point
      INTEGER L                  ! Index of last non-blank character
      INTEGER LDNT               ! Length of DNT
      INTEGER QUOTE              ! The index of the second quote 
      LOGICAL MATCH              ! Is the file type included in FMTS?
*.

*  Set the returned strings blank.
      FDIRN = ' '
      FNAME = ' '
      FTYPE = ' '
      COMP = ' '
      SLICE = ' '
      FORM = 1

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the supplied spec.
      CALL CHR_FANDL( SPEC, F, L )

*  Return without further action if the supplied spec is blank.
      IF( L .GE. F ) THEN

*  If the last character is a closing parenthesis, and there is an
*  opening parenthesis, assume everything after (and including) the last 
*  opening parenthesis is an NDF slice specification. If a slice is
*  found, reduce the number of characters which will be used from SPEC to
*  exclude the slice from the rest of the processing.
         IF( SPEC( L : L ) .EQ. ')' ) THEN
            CALL NDG1_LASTO( SPEC( : L ), '(', IAT, STATUS )
            IF( IAT .GT. 0 ) THEN
               SLICE = SPEC( IAT : L )
               L = IAT - 1 
            END IF
         END IF

      END IF

*  Return without further action if nothing now remains in the supplied spec.
      IF( L .GE. F ) THEN

*  Assume form 1) until we find evidence for another form. Save the string 
*  holding the directory, basename and type (i.e. the whole supplied spec
*  assuming form 1) ).
         FORM = 1
         DNT = SPEC( F : L )

*  If the first character is a double quote, and there is another quote, 
*  use form 2). Extract the string holding the directory, basename and
*  type (i.e. the string between the first two quotes). Any text ocurring
*  after the second quote is saved as the HDS component path.
         IF( SPEC( F : F ) .EQ. '"' ) THEN
            IF( F .LT. L ) THEN
               QUOTE = INDEX( SPEC( F + 1 : L ), '"' )
               IF( QUOTE .NE. 0 ) THEN
                  QUOTE = QUOTE + F
                  IF( F + 1 .LE. QUOTE - 1 ) THEN
                     DNT = SPEC( F + 1 : QUOTE - 1 )
                     FORM = 2

                     IF( QUOTE + 1 .LE. L ) THEN
                        COMP = SPEC( QUOTE + 1 : L )
                     END IF

                  END IF
               END IF         
            END IF
         END IF

*  Find the last occurence of "/" in the DNT string. This marks the end
*  of the directory path.
         CALL NDG1_LASTO( DNT, '/', IAT, STATUS )

*  If found, return the string up to and including IAT as the directory path.
         IF( IAT .GT. 0 ) FDIRN = DNT( : IAT )

*  Find the first dot following the last "/". Store any non-null string 
*  between the two as the file base name. Store the dot and any remaining
*  characters in DNT as the file type.
         LDNT = CHR_LEN( DNT )
         IF( IAT + 1 .LE. LDNT ) THEN
            DOT = INDEX( DNT( IAT + 1 : LDNT ), '.' )
            IF( DOT .GT. 0 ) THEN
               DOT = DOT + IAT 
               IF( IAT + 1 .LE. DOT - 1 ) FNAME = DNT( IAT + 1 : 
     :                                                 DOT - 1 )
               FTYPE = DNT( DOT : LDNT ) 
            ELSE
               FNAME = DNT( IAT + 1 : LDNT )
            END IF
         END IF

*  If form 2) is being used we assume the file type is OK, otherwise
*  we check it against those contained in FMTS. 
         IF( FORM .NE. 2 .AND. FTYPE .NE. ' ' ) THEN
            CALL NDG1_MATCH( FMTS, FTYPE, WILD, MATCH, STATUS )

*  If not found, assume form 3). Use the file type as the HDS component 
*  path, and return a blank file type.
            IF( .NOT. MATCH ) THEN
               FORM = 3
               COMP = FTYPE
               FTYPE = ' '
            END IF
   
         END IF

      END IF

      END
