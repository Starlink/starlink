      SUBROUTINE KPG1_FSPEC( FSPEC, DFSPEC, FLDNAM, FIELD, STATUS )
*+
*  Name:
*     KPG1_FSPEC

*  Purpose:
*     Produces a new file specification by modification of an old file
*     specification.

*  Language:
*     VAX/VMS Fortran.

*  Invocation:
*     CALL KPG1_FSPEC( FSPEC, DFSPEC, FLDNAM, FIELD, STATUS )

*  Description:
*     This is a UNIX specific routine.
*
*     The supplied file specification (FSPEC) is parsed, and the
*     requested fields returned. Any fields missing from FSPEC are
*     defaulted to the corresponding field values from the default file
*     specification (DFSPEC). The full file specification can be
*     returned by supplying a blank value for FLDNAM.

*  Arguments:
*     FSPEC = CHARACTER (Given)
*        The original file specification. 
*     DFSPEC = CHARACTER (Given)
*        The default file specification. 
*     FLDNAM = CHARACTER (Given)
*        A string specifying which fields are to be returned in FIELD.
*        FLDNAM can take any of the values: DEVICE, DIRECTORY, NAME,
*        TYPE, VERSION. In addition, a blank value will return the full
*        file specification in FIELD. Unambiguous abbreviations may be
*        given.
*        The DEVICE and VERSION fields return ' ' in this version.
*     FIELD = CHARACTER (Returned)
*        The requested field from the full file specification.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This is a simple minded look-a-like to the VMS version its
*     capabilities are limited and should not be exceeded.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1992 (PDRAPER):
*        Original Unix version - based on VMS version of same name.
*     1997 May 19 (MJC):
*        Rebadged and edited for KAPPA.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP__ parameters
      INCLUDE 'IRG_ERR'          ! IRG error values

*  Arguments Given:
      CHARACTER * ( * ) FSPEC
      CHARACTER * ( * ) DFSPEC
      CHARACTER * ( * ) FLDNAM

*  Arguments Returned:
      CHARACTER * ( * ) FIELD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) DIR1 ! Directory specification in FSPEC
      CHARACTER * ( GRP__SZNAM ) DIR2 ! Directory specification in
                                      ! DFSPEC
      CHARACTER * ( GRP__SZNAM ) NAM1 ! Name specification in FSPEC
      CHARACTER * ( GRP__SZNAM ) NAM2 ! Name specification in DFSPEC
      CHARACTER * ( GRP__SZNAM ) TYP1 ! Type specification in FSPEC
      CHARACTER * ( GRP__SZNAM ) TYP2 ! Type specification in DFSPEC
      CHARACTER UPFLD * ( 2 )    ! First two characters of FLDNAM
                                 ! converted to upper case
      INTEGER DIREND             ! Position of last directory character
      INTEGER NAMEND             ! Position of last name character
      INTEGER IAT                ! Temporary string position
      INTEGER STRLEN             ! Length of input strings excluding
                                 ! trailing blanks
      INTEGER TYPEND             ! Position of last type character
*.

*  Check THE inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert THE input type to upper case.
      UPFLD = FLDNAM( 1 : MIN( LEN( FLDNAM ), 2 ) )
      CALL CHR_UCASE( UPFLD )

*=======================================================================
*  Current name parsing.
*=======================================================================
*  Get length of string.
      STRLEN = MAX( CHR_LEN( FSPEC ), 1 )

*  Parse input file name into directory, name and type.  Look for
*  directory terminator first -- last '/'
      CALL KPG1_LASTO( FSPEC, '/', DIREND, STATUS )
      IF ( DIREND .EQ. 0 ) THEN

*  No directory information just supply a blank.
         DIR1 = ' '
      ELSE

*  Extract the value.
         DIR1 = FSPEC( 1 : DIREND )
      END IF

*  Look for name value, this should be indicated by the . terminator or
*  end of string (excluding trailing blanks) after the directory
*  specification.
      IF ( DIREND .EQ. 0 ) THEN
         IAT = 1
      ELSE
         IAT = DIREND + 1
      END IF
      NAMEND = INDEX( FSPEC( IAT : ), '.' )
      IF ( NAMEND .EQ. 0 ) THEN

*  May be no name, are there any characters after the directory?
         IF ( DIREND .EQ. STRLEN ) THEN

*  No there are none. Just return a ' '
            NAM1 = ' '
         ELSE

*  Set NAMEND to end of string.
            NAMEND = STRLEN
         END IF
      ELSE

*  NAMEND non-zero, adjust for IAT offset.
         NAMEND = NAMEND + IAT - 2
      END IF

*  Extract the NAME.
      IF ( NAMEND .NE. 0 ) THEN
         NAM1 = FSPEC( DIREND + 1 : NAMEND )

*  Look for file type---any characters after the name.
         IF ( NAMEND .NE. STRLEN ) THEN
            TYP1 = FSPEC( NAMEND + 1 : STRLEN )
         ELSE

*  No type supply a null.
            TYP1 = ' '
         END IF
      ELSE

*  NAMEND zero---may be just a type here, so determine wether or not
*  there is there a fullstop?
         IF ( INDEX( FSPEC( IAT : ), '.' ) .NE. 0 ) THEN 
            TYP1 = FSPEC
         ELSE
            TYP1 = ' '
         END IF         
      END IF

*=======================================================================
*  Default string parsing
*=======================================================================
*  Get length of string.
      STRLEN = MAX( 1, CHR_LEN( DFSPEC ) )

*  Parse input file name into directory, name and type.
*  Look for directory terminator first -- last '/'
      CALL KPG1_LASTO( DFSPEC, '/', DIREND, STATUS )
      IF ( DIREND .EQ. 0 ) THEN

*  There is no directory information so just supply a blank.
         DIR2 = ' '
      ELSE

*  Extract the value.
         DIR2 = DFSPEC( 1 : DIREND )
      END IF

*  Look for name value, this should be indicated by the . terminator or
*  end of string (excluding trailing blanks) after the directory
*  specification.
      IF ( DIREND .EQ. 0 ) THEN
         IAT = 1
      ELSE
         IAT = DIREND + 1
      END IF
      NAMEND = INDEX( DFSPEC( IAT : ), '.' )
      IF ( NAMEND .EQ. 0 ) THEN

*  There may be no name.  Are there any characters after the directory?
         IF ( DIREND .EQ. STRLEN ) THEN

*  No there are none.  Just return a ' '.
            NAM2 = ' '
         ELSE

*  Set NAMEND to the end of the string.
            NAMEND = STRLEN
         END IF
      ELSE

*  NAMEND is non-zero, so adjust for the IAT offset.
         NAMEND = NAMEND + IAT - 2
      END IF

*  Extract the NAME.
      IF ( NAMEND .NE. 0 ) THEN
         NAM2 = DFSPEC( DIREND + 1 : NAMEND )

*  Look for file type.  These are any characters after the name.
         IF ( NAMEND .NE. STRLEN ) THEN
            TYP2 = DFSPEC( NAMEND + 1 : STRLEN )
         ELSE

*  No file type present.  So supply a null.
            TYP2 = ' '
         END IF
      ELSE

*  NAMEND zero---may be just a type here so determine whether or not
*  there is there a fullstop.
         IF ( INDEX( DFSPEC( IAT : ), '.' ) .NE. 0 ) THEN 
            TYP2 = DFSPEC
         ELSE
            TYP2 = ' '
         END IF         
      END IF

*=======================================================================
*  If a specific field is required.
*=======================================================================
      IF ( FLDNAM .NE. ' ' ) THEN

*  Copy the relevant field to the FIELD argument.
         IF ( INDEX( UPFLD, 'DE') .EQ. 1 ) THEN

*  Not activated in UNIX, so supply a blank.
            FIELD = ' '

         ELSE IF ( INDEX( UPFLD, 'DI') .EQ. 1 ) THEN

*  Supply the directory name.
            IF ( DIR1 .NE. ' ' ) THEN
               FIELD = DIR1
            ELSE
               FIELD = DIR2
            END IF

         ELSE IF ( INDEX( UPFLD, 'N') .EQ. 1 ) THEN

*  Supply the name.
            IF ( NAM1 .NE. ' ' ) THEN
               FIELD = NAM1
            ELSE
               FIELD = NAM2
            END IF

         ELSE IF ( INDEX( UPFLD, 'T') .EQ. 1 ) THEN

*  Supply the type.
            IF ( TYP1 .NE. ' ' ) THEN
               FIELD = TYP1
            ELSE
               FIELD = TYP2
            END IF

         ELSE IF ( INDEX( UPFLD, 'V') .EQ. 1 ) THEN

*  There are no versions in UNIX, so return a null value.
            FIELD = ' '

*  If the field name was unrecognised, report an error.
         ELSE
            STATUS = IRG__BADFN
            CALL MSG_SETC( 'NAM', FLDNAM )
            CALL ERR_REP( 'KPG1_FSPEC_ERR1',
     :        'KPG1_FSPEC: Unrecognised field requested - "^NAM"',
     :        STATUS )
            GO TO 999
         END IF

      ELSE

*  The full name is required.  Use strings that we've got.
         IF ( DIR1 .EQ. ' ' ) DIR1 = DIR2
         DIREND = MAX(1, CHR_LEN( DIR1 ) )
         IF ( NAM1 .EQ. ' ' ) NAM1 = NAM2
         NAMEND = MAX(1, CHR_LEN( NAM1 ) )
         IF ( TYP1 .EQ. ' ' ) TYP1 = TYP2
         TYPEND = MAX(1, CHR_LEN( TYP1 ) )
         
         FIELD = DIR1( : DIREND ) // 
     :           NAM1( : NAMEND ) // 
     :           TYP1( : TYPEND ) 
      END IF

 999  CONTINUE

      END
