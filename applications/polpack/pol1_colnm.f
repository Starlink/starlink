      SUBROUTINE POL1_COLNM( INAME, ENAME, STATUS )
*+ 
*  Name: 
*     POL1_COLNM

*  Purpose:
*     Return the catalogue column name to use for a given quantity.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_COLNM( INAME, ENAME, STATUS )

*  Description:
*     This routine returns the name to use for catalogue columns holding
*     the quantity specified by INAME.
*
*     The returned external column name is read from the POLPACK
*     configuration file $HOME/.polpackrc. Any lines in this file which
*     begin with the word "Column" are assumed to define an external
*     column name; the second word on the line must be an internal
*     quantity name, and the third word (if supplied) should be the
*     column name for that quantity. If the third word is omitted, the
*     quantity given by INAME is not stored in the catalogue. This will
*     result in an error being reported if the column in mandatory.
*     Any internal quantities which are not named in the file are stored
*     in columns with the same name as the quantity.
*
*     The following internal quantity names are recognised:
*        "X" - X pixel coordinate (mandatory)
*        "Y" - Y pixel coordinate (mandatory)
*        "RA" - Right ascension (mandatory only if RA/DEC columns requested)
*        "DEC" - Declination (mandatory only if RA/DEC columns requested)
*        "I" - Total intensity (mandatory)
*        "Q" - Stokes Q parameter (mandatory for linear polarization)
*        "U" - Stokes U parameter (mandatory for linear polarization)
*        "V" - Stokes V parameter (mandatory for circular polarization)
*        "DI" - Std. devn. on I (mandatory only if variances are requested)
*        "DQ" - Std. devn. on Q (mandatory only if variances are requested)
*        "DU" - Std. devn. on U (mandatory only if variances are requested)
*        "DV" - Std. devn. on V (mandatory only if variances are requested)
*        "P" - Percentage polarization (optional)
*        "PI" - Polarized intensity (optional)
*        "ANG" - Polarization angle (optional)
*        "DP" - Std. devn. on P (mandatory only if variances are requested 
*               and P is in use)
*        "DPI" - Std. devn. on PI (mandatory only if variances are requested 
*               and P is in use)
*        "DANG" - Std. devn. on ANG (mandatory only if variances are requested 
*               and P is in use)

*  Arguments:
*     INAME = CHARACTER * ( * ) (Given)
*        The name of the quantity (e.g. X, Y, RA, DEC, I, Q, U, V, DI, DQ, 
*        DU, DV, P, DP, ANG, DANG, PI, DPI, etc)
*     ENAME = CHARACTER * ( * ) (Returned)
*        The catalogue column name to use for the given quantity.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-DEC-2000 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO_ error constants

*  Arguments Given:
      CHARACTER INAME*(*)

*  Arguments Returned:
      CHARACTER ENAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Two strings equal aprt from case?
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MXCOL              ! Max number of column definitions
      PARAMETER ( MXCOL = 17 )

*  Local Variables:
      CHARACTER BUF*255          ! Buffer for line read from config file
      CHARACTER COL(MXCOL)*20    ! External column names from rc file
      CHARACTER NAME(MXCOL)*10   ! Internal column names from rc file
      CHARACTER NAMES(MXCOL)*4   ! All known internal column names
      CHARACTER RCFILE*255       ! Path to polpack config file
      CHARACTER WORDS( 3 )*30    ! Words read from the config file
      INTEGER FD                 ! FIO identifier for open config file
      INTEGER I                  ! Loop count
      INTEGER LINE               ! Current line number in config file
      INTEGER LSTAT              ! CHR local status value
      INTEGER NC                 ! Length of line read from config file
      INTEGER NCOL               ! Number of column definitions in rc file
      INTEGER NWRD               ! Number of words read from the config file
      INTEGER START( 3 )         ! Word starting positions
      INTEGER STOP( 3 )          ! Word ending positions
      LOGICAL INIT               ! Have the column names been read yet?

*  Local Data:
      DATA INIT /.FALSE./,
     :     NAMES / "X", "Y", "RA", "DEC", "I", "Q", "U", "V", "DI", 
     :             "DQ", "DV", "P", "PI", "ANG", "DP", "DPI", "DANG" /


*  Ensure the necessary variale values are retained between invocations
*  of this routine.
      SAVE INIT, NAME, COL, NCOL

*.

*  Initialize
      ENAME = ' '

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the column names have not yet been read from the polpack
*  configuration file, do so now.
      IF ( .NOT. INIT .AND. STATUS .EQ. SAI__OK ) THEN
         NCOL = 0

*  Get the users home directory, and then append the name of the config
*  file.
         CALL PSX_GETENV( 'HOME', RCFILE, STATUS )
         NC = CHR_LEN( RCFILE )         
         CALL CHR_APPND( '/.polpackrc', RCFILE, NC )

*  Attempt to open this file.
         CALL FIO_OPEN( RCFILE( : NC ), 'READ', 'NONE', 0, FD, STATUS ) 

*  Annul any error.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )

*  If OK, read in each line of the file.
         ELSE
            CALL FIO_READ( FD, BUF, NC, STATUS ) 
            LINE = 1
            DO WHILE( STATUS .EQ. SAI__OK ) 

*  Extract the first three words.
               CALL CHR_DCWRD( BUF( : NC ), 3, NWRD, START, STOP, WORDS, 
     :                         LSTAT )

*  If the first word is "Column"...
               IF( CHR_SIMLR( WORDS( 1 ), 'Column' ) ) THEN

*  Check there are either 2 or 3 columns in the line.
                  IF( LSTAT .NE. SAI__OK .OR. NWRD .LT. 2 ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'L', BUF( : NC ) )
                     CALL MSG_SETC( 'F', RCFILE )
                     CALL MSG_SETC( 'N', RCFILE )
                     CALL ERR_REP( 'POL1_COLNM_ERR1', 'Wrong number '//
     :                             'of words in line ^N of POLPACK '//
     :                             'configuration file ^F (should be '//
     :                             '2 or 3): ^L', STATUS )

*  Check the internal name is known.
                  ELSE
                     DO I = 1, MXCOL         
                        IF( WORDS( 2 ) .EQ. NAMES( I ) ) THEN 
                           GO TO 10
                        END IF
                     END DO

*  Arrive here if the internal name is not known.
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'Q', WORDS( 2 ) )
                     CALL MSG_SETC( 'F', RCFILE )
                     CALL MSG_SETC( 'N', RCFILE )
                     CALL ERR_REP( 'POL1_COLNM_ERR1', 'Unknown '//
     :                             'quantity ''^Q'' specified at line'//
     :                             ' ^N of POLPACK configuration file'//
     :                             ' ^F.', STATUS )


*  The second word is the internal name and the third word (if any) is the
*  external name.
 10                  CONTINUE
                     IF( STATUS .EQ. SAI__OK ) THEN 
                        NCOL = NCOL + 1
                        NAME( NCOL ) = WORDS( 2 )
                        IF( NWRD .GT. 2 ) THEN 
                           COL( NCOL ) = WORDS( 3 )
                           CALL CHR_UCASE( COL( NCOL ) )
                        ELSE
                           COL( NCOL ) = ' '
                        END IF
                     END IF
                  END IF
               END IF

*  Read in the next line.
               CALL FIO_READ( FD, BUF, NC, STATUS ) 
               LINE = LINE + 1
            END DO

*  Annul the end of file error.
            IF( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

*  Close the config file.
            CALL FIO_CLOSE( FD, STATUS )

         END IF

*  If the above initialization went OK, indicate that we do not need to
*  do it again.
         IF( STATUS .EQ. SAI__OK ) THEN
            INIT = .TRUE.
         ELSE
            NCOL = 0
         END IF

      END IF

*  Check the supplied internal name is known.
      DO I = 1, MXCOL         
         IF( INAME .EQ. NAMES( I ) ) THEN 
            GO TO 20
         END IF
      END DO

*  Arrive here if the internal name is not known.
      STATUS = SAI__ERROR
      CALL MSG_SETC( 'Q', NAME )
      CALL ERR_REP( 'POL1_COLNM_ERR1', 'POL1_COLNM: Unknown quantity '//
     :              '''^Q'' supplied (programming error).', STATUS )

 20   CONTINUE
      IF( STATUS .EQ. SAI__OK ) THEN      

*  Find and return the external column name for the given internal column 
*  name. If no matching internal column name was included in the config
*  file, use the internal name.
         ENAME = INAME 
         DO I = 1, NCOL
            IF( NAME( I ) .EQ. INAME ) THEN
               ENAME = COL( I )
               GO TO 30
            END IF
         END DO

 30      CONTINUE

      END IF

      END
