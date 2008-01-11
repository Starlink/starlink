      SUBROUTINE PHO1_AGRP( FDIN, OBJIND, OBJINF, PSFIND, PSFINF,
     :                SKYIND, SKYINF, OPTIMA, STATUS )
*+
*  Name:
*     PHO1_AGRP

*  Purpose:
*     Converts an AUTOPHOTOM description list into object and sky
*     region groups.

*  Language:
*     Starlink Fortran-77

*  Invocation:
*     CALL PHO1_AGRP( FDIN, OBJIND, OBJINF, PSFIND, PSFINF,
*    :                SKYIND, SKYINF, OPTIMA, STATUS )

*  Description:
*     This routine converts a description file as used by the AUTOPHOTOM
*     routine into four GRP groups of information. Objects descriptions
*     are return in two groups, one containing the object indices and
*     one containing the other necessary information (X & Y positions
*     etc.). Sky regions are similarly processed into a list of indices
*     and the other necessary information. The indices are separated
*     from the other information so that quicker comparisons of which
*     sky data matches objects can be made.

*  Arguments:
*     FDIN = INTEGER (Given)
*        FIO identifier of file that contains the description.
*     OBJIND = INTEGER (Returned)
*        GRP group of object indices.
*     OBJINF = INTEGER (Returned)
*        GRP group of additional information describing object aperture.
*     PSFIND = INTEGER (Returned)
*        GRP group of PSF indices.
*     PSFINF = INTEGER (Returned)
*        GRP group of additional information describing PSF aperture.
*     SKYIND = INTEGER (Returned)
*        GRP group of object indices, that correspond to this sky region.
*     SKYINF = INTEGER (Returned)
*        GRP group of additional information describing sky apertures.
*     OPTIMA = LOGICAL (Given)
*        Are we doing an optimal extraction?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The output groups should be deleted (using GRP_DELET) before the
*     application exits.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     AALLAN: Alasdair Allan (STARLINK - Keele University)
*     {enter_new_authors_here}

*  History:
*     12-APR-1996 (PDRAPER):
*        Original version.
*     01-FEB-1999 (AALLAN)
*        Reflects changes to the input file for optimal extraction
*     28-MAY-1999 (PDRAPER):
*        Changed number of PSF fields to match documentation.
*     11-JAN-2008 (PDRAPER):
*        Clean TABs from input strings. Not documented that only spaces
*        are allowed, so bend on this.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'GRP_PAR'         ! GRP constants
      INCLUDE 'PRM_PAR'         ! Primitive data constants
      INCLUDE 'FIO_ERR'         ! FIO error codes

*  Arguments Given:
      INTEGER FDIN              ! Input FIO identifier
      LOGICAL OPTIMA		! Are we doing optimal extraction?

*  Arguments Returned:
      INTEGER OBJIND
      INTEGER OBJINF
      INTEGER PSFIND
      INTEGER PSFINF
      INTEGER SKYIND
      INTEGER SKYINF

*  Local constants:
      INTEGER MAXWRD
      INTEGER MAXOPT
      INTEGER MAXPSF
      PARAMETER ( MAXWRD = 13 ) ! Maximum number of words in line
      PARAMETER ( MAXOPT = 9 )  ! Maximum number of words in a optimal line
      PARAMETER ( MAXPSF = 10 ) ! Maximum number of words in a PSF line

*  Status:
      INTEGER STATUS            ! Global status

*  External references:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of string.

*  Local constants:
      INTEGER TABVAL            ! ASCII TAB
      PARAMETER ( TABVAL = 9 )

*  Local Constants:
      CHARACTER * ( 1 ) TAB     ! TAB character value
      CHARACTER * ( GRP__SZNAM ) BUFFER ! Input line buffer
      CHARACTER * ( VAL__SZD ) WORDS( MAXWRD ) ! Input line broken into words
      INTEGER J                 ! Loop variable
      INTEGER L                 ! Used length of string
      INTEGER LSTAT             ! Local status
      INTEGER NLINE             ! Line count
      INTEGER NOBJ              ! Number of object apertures
      INTEGER NPSF		! Number of PSF apertures
      INTEGER NSKY              ! Number of sky regions
      INTEGER NWRD              ! Number of words in line
      INTEGER START( MAXWRD )   ! Starting positions of words in buffer
      INTEGER STOP( MAXWRD )    ! End positions of words in buffer
      LOGICAL OK                ! Ok to read file again
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise TAB value.
      TAB = CHAR( TABVAL )

*  Create the groups to contain the decoded information.
      CALL GRP_NEW( 'Object indices', OBJIND, STATUS )
      CALL GRP_NEW( 'Object aperture information', OBJINF, STATUS )
      CALL GRP_NEW( 'PSF object indices ', PSFIND, STATUS )
      CALL GRP_NEW( 'PSF aperture information', PSFINF, STATUS )
      CALL GRP_NEW( 'Object indices (for sky regions)', SKYIND, STATUS )
      CALL GRP_NEW( 'Sky aperture information', SKYINF, STATUS )

*  Loop and read the contents of the file.
      OK = .TRUE.
      NLINE = 0
      NOBJ = 0
      NPSF = 0
      NSKY = 0
 1    CONTINUE                  ! Start of DO WHILE loop.
      IF ( OK .AND. STATUS .EQ. SAI__OK ) THEN
         CALL FIO_READF( FDIN, BUFFER, STATUS )
         IF ( STATUS .EQ. FIO__EOF ) THEN

*  End of file, annul error and make sure loop exits.
            CALL ERR_ANNUL( STATUS )
            OK = .FALSE.
         ELSE

*  Used length for efficiency.
            L = CHR_LEN( BUFFER )

*  Pre-process line by replacing any TABs with spaces.
            DO 2 J = 1, L
               IF ( BUFFER( J : J ) .EQ. TAB ) THEN
                  BUFFER( J : J ) = ' '
               END IF
 2          CONTINUE

*  Read a line need to determine what it is.
            NLINE = NLINE + 1
            IF ( BUFFER( 1:1 ) .NE. '#' ) THEN

*  Probable object line (note we skip blank lines in this part).
               CALL CHR_DCWRD( BUFFER( :L ), MAXWRD, NWRD, START, STOP,
     :                         WORDS, LSTAT )
               IF ( LSTAT .EQ. SAI__ERROR ) THEN
                  CALL MSG_SETI( 'NUM', NLINE )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'PHO1_AGRP_TOOMANY',
     :            'Line ^NUM contains too many fields.', STATUS )
                  OK = .FALSE.
               ELSE IF ( NWRD .EQ. 13 ) THEN

*  13 fields in line, must be an object so record the information.
                  NOBJ = NOBJ + 1
                  CALL GRP_PUT( OBJIND, 1, WORDS( 1 ), 0, STATUS )
                  CALL GRP_PUT( OBJINF, 1, BUFFER( START( 2 ): L ), 0,
     :                          STATUS )
               ELSE IF ( NWRD .EQ. 9 ) THEN

*  9 fields in line, must be an optimal object so record the information.
                  NOBJ = NOBJ + 1
                  CALL GRP_PUT( OBJIND, 1, WORDS( 1 ), 0, STATUS )
                  CALL GRP_PUT( OBJINF, 1, BUFFER( START( 2 ): L ), 0,
     :                          STATUS )

               ELSE IF ( NWRD .EQ. 10 ) THEN

*  10 fields in line, must be a PSF so record the information.
                  IF( .NOT. OPTIMA ) THEN
		       OPTIMA = .NOT. OPTIMA
		  ENDIF
                  NPSF = NPSF + 1
                  CALL GRP_PUT( PSFIND, 1, WORDS( 1 ), 0, STATUS )
                  CALL GRP_PUT( PSFINF, 1, BUFFER( START( 2 ): L ), 0,
     :                          STATUS )

               ELSE IF ( NWRD .NE. 0 ) THEN

*  Unknown number of records, so report an error.
                  CALL MSG_SETI( 'NUM', NLINE )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'PHO1_AGRP_TOOFEW',
     :                        'Line ^NUM has missing fields.', STATUS )
                  OK = .FALSE.
               END IF
            ELSE IF ( BUFFER( 1:4 ) .EQ. '#SKY' ) THEN

*  Probable SKY region line.
               CALL CHR_DCWRD( BUFFER( :L ), 8, NWRD, START, STOP,
     :                         WORDS, LSTAT )
               IF ( LSTAT .EQ. SAI__ERROR ) THEN
                  CALL MSG_SETI( 'NUM', NLINE )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'PHO1_AGRP_TOOMANY',
     :            'Line ^NUM contains too many fields.', STATUS )
                  OK = .FALSE.
               ELSE IF ( NWRD .NE. 8 ) THEN
                  CALL MSG_SETI( 'NUM', NLINE )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'PHO1_AGRP_TOOFEW',
     :            'Line ^NUM has missing fields.', STATUS )
                  OK = .FALSE.
               ELSE

*  Correct number of words so record information.
                  NSKY = NSKY + 1
                  CALL GRP_PUT( SKYIND, 1, WORDS( 2 ), 0, STATUS )
                  CALL GRP_PUT( SKYINF, 1, BUFFER( START( 3 ): L ), 0,
     :                          STATUS )
               END IF
            ELSE IF ( BUFFER( 1:4 ) .EQ. '#ANN' ) THEN

*  Probable annulus sky region line.
               CALL CHR_DCWRD( BUFFER( :L ), 4, NWRD, START, STOP,
     :                         WORDS, LSTAT )
               IF ( LSTAT .EQ. SAI__ERROR ) THEN
                  CALL MSG_SETI( 'NUM', NLINE )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'PHO1_AGRP_TOOMANY',
     :            'Line ^NUM contains too many fields.', STATUS )
                  OK = .FALSE.
               ELSE IF ( NWRD .NE. 4 ) THEN
                  CALL MSG_SETI( 'NUM', NLINE )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'PHO1_AGRP_TOOFEW',
     :            'Line ^NUM has missing fields.', STATUS )
                  OK = .FALSE.
               ELSE

*  Correct number of words so record information.
                  NSKY = NSKY + 1
                  CALL GRP_PUT( SKYIND, 1, WORDS( 2 ), 0, STATUS )
                  CALL GRP_PUT( SKYINF, 1, BUFFER( START( 3 ): L ), 0,
     :                          STATUS )
               END IF
            END IF
         END IF
         GO TO 1                ! Next loop.
      END IF

*  Check that we have some objects.
      IF ( NOBJ .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PHO1_AGRP_NOOBJ',
     :   'File contains no object information.', STATUS )
      END IF

*  Check that we have some PSF stars.
      IF ( OPTIMA .AND. NPSF .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PHO1_AGRP_NOPSF',
     :   'File contains no PSF information.', STATUS )
      END IF

*  Delete the GRP groups in exiting in error.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( OBJIND, STATUS )
         CALL GRP_DELET( OBJINF, STATUS )
         CALL GRP_DELET( PSFINF, STATUS )
         CALL GRP_DELET( PSFIND, STATUS )
	 CALL GRP_DELET( SKYIND, STATUS )
         CALL GRP_DELET( SKYINF, STATUS )
      END IF
      END
