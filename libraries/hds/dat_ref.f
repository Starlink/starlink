      SUBROUTINE DAT_REF( LOC, REF, LREF, STATUS )
*+
*  Name:
*     DAT_REF

*  Purpose:
*     Obtain a reference for an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DAT_REF( LOC, REF, LREF, STATUS )

*  Description:
*     The routine returns a "reference name" for an HDS object whose
*     locator is supplied. This name identifies the object uniquely by
*     including both the name of the container file and the "path name"
*     which locates the object within this file. If a locator to a cell
*     or a slice is supplied, then subscript information will also be
*     included.  Appropriate syntax is used to represent file names
*     which do not have the standard (.sdf) file extension.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to the HDS object.
*     REF = CHARACTER * ( * ) (Returned)
*        The object's reference name.
*     LREF = INTEGER (Returned)
*        Number of significant characters in the reference name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Machine-specific features used:
*     -  This routine makes assumptions about the form of VMS and Unix
*     file names.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     9-AUG-1991 (RFWS):
*        Original version, derived from the DAT_MSG routine.
*     16-AUG-1991 (RFWS):
*        Changed to make tests for file extensions case sensitive. Also
*        append an extra '.' to Unix file names when necessary.
*     20-AUG-1991 (RFWS):
*        Removed dependence on PSX_ routines - use global constant
*        instead.
*     25-SEP-1991 (RFWS):
*        Added the LREF argument.
*     1-OCT-1991 (RFWS):
*        Set an initial "safe" value for the LREF argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT1_SYS'         ! Internal system-dependent constants
      INCLUDE 'EMS_PAR'          ! EMS_ public constants
      INCLUDE 'DAT_ERR'          ! DAT_ error codes

*  Arguments Given:
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      CHARACTER * ( * ) REF
      INTEGER LREF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER SZFIL              ! Max. length of a file name
      PARAMETER ( SZFIL = 512 )
      INTEGER SZPTH              ! Max. length of a path name
      PARAMETER ( SZPTH = 512 )

*  Local Variables:
      CHARACTER * ( SZFIL + 1 ) FILE ! Container file name
      CHARACTER * ( SZPTH ) PATH ! Object path name
      INTEGER BRA                ! Position of '(' character
      INTEGER DOT                ! Position of '.'
      INTEGER I                  ! Loop counter for file name characters
      INTEGER N                  ! Start of truncated text portion
      INTEGER NC                 ! Length of reference name
      INTEGER NCF                ! No. characters in filename
      INTEGER NCP                ! No. characters in path name
      INTEGER NLEV               ! Object level in HDS
      INTEGER SEMI               ! Position of ';'
      INTEGER START              ! Position to start in path name
      LOGICAL ADDDOT             ! Append '.' to file name?
      LOGICAL ODD                ! Is the filename odd?

*.

*  Set an initial "safe" value for the LREF argument.
      LREF = 1

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the data object path and container file name.
      CALL HDS_TRACE( LOC, NLEV, PATH, FILE( : SZFIL ), STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If necessary, handle VAX/VMS file names.  Locate the semicolon which
*  delimits the version number in the file name.
         IF ( DAT__VMS ) THEN
            SEMI = INDEX( FILE( : SZFIL ), ';' )

*  If found, then select the file name prior to it.
            IF ( SEMI .NE. 0 ) THEN
               NCF = MAX( 1, SEMI - 1 )

*  Otherwise, use the whole file name, excluding trailing blanks.
            ELSE
               NCF = MAX( 1, CHR_LEN( FILE( : SZFIL ) ) )
            END IF

*  See if the file is "odd". Check to see if it has the default file
*  extension of '.SDF' with at least one character preceding it.
            ODD = .TRUE.
            IF ( NCF .GE. 5 ) THEN
               ODD = ( FILE( NCF - 3 : NCF ) .NE. '.SDF' )
            END IF

*  Assume Unix file names if not using VAX/VMS. First find the file
*  name length.
         ELSE
            NCF = MAX( 1, CHR_LEN( FILE( : SZFIL ) ) )

*  See if the file is "odd". Check to see if it has the default file
*  extension of '.sdf' with at least one character preceding it.
            ODD = .TRUE.
            IF ( NCF .GE. 5 ) THEN
               ODD = ( FILE( NCF - 3 : NCF ) .NE. '.sdf' )
            END IF

*  If the file name is odd, then we must also decide whether to append
*  a '.' to the end of it. This is done to counteract the removal of a
*  terminating '.' which HDS performs on all Unix file names (to permit
*  the creation of files without a '.' in their names if required).
*  Initially assume an extra '.' is needed.
            IF ( ODD ) THEN
               ADDDOT = .TRUE.

*  If the file name already ends with a '.'. then another '.' will be
*  needed to protect it.  Otherwise, search backwards through the final
*  field of the file name (stopping when a '/' is encountered) to see
*  whether there is already a '.' present.
               IF ( FILE ( NCF : NCF ) .NE. '.' ) THEN
                  DO 1 I = NCF, 1, -1
                     IF ( FILE( I : I ) .EQ. '/' ) THEN
                        GO TO 2

*  If a '.' is present, then note that another one need not be added
*  (otherwise one must be added to prevent the default ".sdf" extension
*  being appended if HDS re-opens the file using this name).
                     ELSE IF ( FILE ( I : I ) .EQ. '.' ) THEN
                        ADDDOT = .FALSE.
                        GO TO 2
                     END IF
 1                CONTINUE
 2                CONTINUE                 
               END IF

*  If an extra '.' is needed, then append it to the file name (note
*  that an extra character is reserved in FILE for this purpose).
               IF ( ADDDOT ) THEN
                  NCF = NCF + 1
                  FILE( NCF : NCF ) = '.'
               END IF
            END IF
         END IF

*  If the file name is not odd, then omit the file extension.
         IF ( .NOT. ODD ) NCF = NCF - 4

*  Enter the file name into the output argument, surrounding it in
*  quotes if it is odd.
         LREF = 0
         IF ( ODD ) CALL CHR_PUTC( '"', REF, LREF )
         CALL CHR_PUTC( FILE( : NCF ), REF, LREF )
         IF ( ODD ) CALL CHR_PUTC( '"', REF, LREF )

*  Record the length of the reference name so far.
         NC = NCF
         IF ( ODD ) NC = NC + 2

*  If the object is not a top-level object, then find the position of
*  the first '.' in its path name, which marks the start of the first
*  component name.
         NCP = MAX( 1, CHR_LEN( PATH ) )
         IF ( NLEV .GT. 1 ) THEN
            DOT = INDEX( PATH( : NCP ), '.' )

*  If successful, see if the '.' is preceded by a '(' indicating that
*  the top-level object is subscripted. Derive the starting position in
*  the path name so that the subscript is used if present.
            IF ( DOT .NE. 0 ) THEN
               BRA = INDEX( PATH( : DOT ), '(' )
               IF ( BRA .NE. 0 ) THEN
                  START = BRA
               ELSE
                  START = DOT
               END IF

*  Append the required part of the path name to the output and
*  increment the reference name length.
               CALL CHR_PUTC( PATH( START : NCP ), REF, LREF )
               NC = NC + ( NCP - START + 1 )
            END IF

*  If the object is a top-level object, then see if it is subscripted.
*  If so, then append the subscript to the output and increment the
*  reference name length.
         ELSE
            START = INDEX( PATH( : NCP ), '(' )
            IF ( START .NE. 0 ) THEN
               CALL CHR_PUTC( PATH( START : NCP ), REF, LREF )
               NC = NC + ( NCP - START + 1 )
            END IF
         END IF

*  Fill any remaining characters in the output string with blanks.
         IF ( LREF .LT. LEN( REF ) ) REF( LREF + 1 : ) = ' '

*  If the length of the reference name exceeded the length of the output
*  argument, then append an ellipsis.
         IF ( NC .GT. LEN( REF ) ) THEN
            N = MAX( 1, LEN( REF ) - 2 )
            REF( N : ) = '...'

*  Report an error, showing the truncated character string.
            STATUS = DAT__TRUNC
            N = MAX( 1, ( LEN( REF ) - EMS__SZTOK + 32 ) )
            CALL EMS_SETC( 'STRING', REF( N : ) )
            CALL EMS_REP( 'DAT_REF_1',
     :                    'Character string truncated: ''^STRING''.',
     :                    STATUS )
            CALL EMS_REP( 'DAT_REF_2',
     :                    'Output character variable is too short ' //
     :                    'to accommodate the returned result.',
     :                    STATUS )
         END IF
      END IF

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_REP( 'DAT_REF_ERR',
     :                 'DAT_REF: Error obtaining a reference name ' //
     :                 'for an HDS object.', STATUS )
      END IF

*  Return a "safe" value for the LREF argument under error conditions.
      IF ( STATUS .NE. SAI__OK ) LREF = 1

      END
