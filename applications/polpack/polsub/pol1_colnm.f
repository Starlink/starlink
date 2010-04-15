      SUBROUTINE POL1_COLNM( INAME, INVERT, ENAME, STATUS )
*+
*  Name:
*     POL1_COLNM

*  Purpose:
*     Return the catalogue column name to use for a given quantity.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_COLNM( INAME, INVERT, ENAME, STATUS )

*  Description:
*     This routine returns the name to use for catalogue columns holding
*     the quantity specified by INAME (or the opposite if INVERT is TRUE).
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
*        "Z" - Z pixel coordinate (optional)
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
*        "ID" - Identifier for the row (optional)

*  Arguments:
*     INAME = CHARACTER * ( * ) (Given)
*        The name of the quantity (e.g. X, Y, Z, RA, DEC, I, Q, U, V, DI, DQ,
*        DU, DV, P, DP, ANG, DANG, PI, DPI, ID, etc). If INAME is blank,
*        the config file is re-read, but no ENAME value is returned.
*     INVERT = LOGICAL (Given)
*        If .TRUE., then the role of INAME and ENAME are swapped (i.e. INAME
*        is assumed to be the external column name, and ENAME is returned
*        holding the corresponding internal quantity name, or blank if the
*        column does not store any of the known quantities).
*     ENAME = CHARACTER * ( * ) (Given and Returned)
*        The catalogue column name to use for the given quantity is
*        returned in ENAME if INAME is not blank. If INAME is blank,
*        ENAME optionally supplies the name of the polpack configuration
*        file to use.
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
*     2-DEB-2001 (DSB):
*        Added Z column.
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
      LOGICAL INVERT

*  Arguments Returned:
      CHARACTER ENAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Two strings equal aprt from case?
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MXCOL              ! Max number of column definitions
      PARAMETER ( MXCOL = 20 )

*  Local Variables:
      CHARACTER BUF*255          ! Buffer for line read from config file
      CHARACTER COL(MXCOL)*20    ! External column names from rc file
      CHARACTER NAME(MXCOL)*10   ! Internal column names from rc file
      CHARACTER NAMES(MXCOL)*4   ! All known internal column names
      CHARACTER RCFILE*255       ! Path to polpack config file
      CHARACTER WORDS( 3 )*30    ! Words read from the config file
      INTEGER FD                 ! FIO identifier for open config file
      INTEGER I                  ! Loop count
      INTEGER J                  ! Loop count
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
     :     NAMES / 'X', 'Y', 'Z', 'RA', 'DEC', 'I', 'Q', 'U', 'V', 'DI',
     :             'DQ', 'DU', 'DV', 'P', 'PI', 'ANG', 'DP', 'DPI',
     :             'DANG', 'ID' /


*  Ensure the necessary variale values are retained between invocations
*  of this routine.
      SAVE INIT, NAME, COL, NCOL

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the column names have not yet been read from the polpack
*  configuration file, do so now.
      IF ( ( .NOT. INIT .OR. INAME .EQ. ' ' ) .AND.
     :      STATUS .EQ. SAI__OK ) THEN
         NCOL = 0

*  If INAME is blank, ENAME can be used to supplied an rc file.
         IF( INAME .EQ. ' ' ) THEN
            RCFILE = ENAME
         ELSE
            RCFILE = ' '
         END IF

*  If not supplied...
         IF( RCFILE .EQ. ' ' ) THEN

*  See if the environment variable POLPACKRC is defined. If it is, is is
*  assumed to hold the name of the polpack config file.
            CALL PSX_GETENV( 'POLPACKRC', RCFILE, STATUS )

*  If not, annul the error and construct the name of the file as
*  "$HOME/.polpackrc"
            IF( STATUS .NE.SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               CALL PSX_GETENV( 'HOME', RCFILE, STATUS )
               NC = CHR_LEN( RCFILE )
               CALL CHR_APPND( '/.polpackrc', RCFILE, NC )
            END IF

         END IF

*  Attempt to open this file.
         CALL FIO_OPEN( RCFILE, 'READ', 'NONE', 0, FD, STATUS )

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

*  See if this internal column already has an array entry.
                  ELSE
                     J = 0
                     DO I = 1, NCOL
                        IF( WORDS( 2 ) .EQ. NAME( I ) ) THEN
                           J = I
                           GO TO 5
                        END IF
                     END DO
 5                   CONTINUE

*  If so, replace its external column name with the new value.
                     IF( J .GT. 0 ) THEN
                        IF( NWRD .EQ. 3 ) THEN
                           COL( J ) = WORDS( 3 )
                           CALL CHR_UCASE( COL( J ) )
                        ELSE
                           COL( J ) = ' '
                        END IF

*  Otherwise, check the internal name is known.
                     ELSE
                        J = 0
                        DO I = 1, MXCOL
                           IF( WORDS( 2 ) .EQ. NAMES( I ) ) THEN
                              J = I
                              GO TO 10
                           END IF
                        END DO
 10                     CONTINUE

*  Report an error if the internal name is not known.
                        IF( J .EQ. 0 ) THEN
                           STATUS = SAI__ERROR
                           CALL MSG_SETC( 'Q', WORDS( 2 ) )
                           CALL MSG_SETC( 'F', RCFILE )
                           CALL MSG_SETC( 'N', RCFILE )
                           CALL ERR_REP( 'POL1_COLNM_ERR2', 'Unknown '//
     :                             'quantity ''^Q'' specified at line'//
     :                             ' ^N of POLPACK configuration file'//
     :                             ' ^F.', STATUS )

*  Otherwise, add a new entry to the arrays.
                        ELSE
                           NCOL = NCOL + 1
                           IF( NCOL .GT. MXCOL ) THEN
                              STATUS = SAI__ERROR
                              CALL MSG_SETI( 'N', MXCOL )
                              CALL MSG_SETC( 'F', RCFILE )
                              CALL ERR_REP( 'POL1_COLNM_ERR3', 'Too '//
     :                                   'many column definitions in '//
     :                                   'POLPACK configuration file '//
     :                                   '''^F''.', STATUS )
                           ELSE
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

*  Return without action if the initialization failed, or if no quantity
*  name was given.
      IF( INIT .AND. INAME .NE. ' ' ) THEN
         ENAME = ' '

*  First handle the normal case...
         IF( .NOT. INVERT ) THEN

*  Check the supplied internal name is known.
            DO I = 1, MXCOL
               IF( INAME .EQ. NAMES( I ) ) THEN
                  GO TO 20
               END IF
            END DO

*  Arrive here if the internal name is not known.
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'Q', INAME )
            CALL ERR_REP( 'POL1_COLNM_ERR4', 'POL1_COLNM: Unknown '//
     :                 'quantity ''^Q'' supplied (programming error).',
     :                 STATUS )

 20         CONTINUE
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

 30            CONTINUE

            END IF

*  Now handle the inverted case.
         ELSE

*  If the external column name is the same as one of the internal
*  quantity names, use the internal quanity name as the default value.
*  If the external column name is unknown, use a blank default.
            ENAME = ' '
            DO I = 1, MXCOL
               IF( INAME .EQ. NAMES( I ) ) THEN
                  ENAME = INAME
                  GO TO 40
               END IF
            END DO

 40         CONTINUE

*  Find and return the internal quantity name for the given external column
*  name.
            DO I = 1, NCOL
               IF( COL( I ) .EQ. INAME ) THEN
                  ENAME = NAME( I )
                  GO TO 50
               END IF
            END DO

 50         CONTINUE

         END IF

      END IF

      END
