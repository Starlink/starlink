*+  RED4_READ_COEFFS - Read linearisation coefficients from a file.
      SUBROUTINE RED4_READ_COEFFS( FILE, MAXCOEFFS, NCOEFFS, COEFFS,
     :  STATUS )
*    Description :
*     This routine reads the linearisation coefficients from the
*     given text file. Any number of coefficients can be included
*     in the file, up to a maximum of MAXCOEFFS. The coefficients
*     should be arranged one per line in the file as follows:
*        Coefficient of X
*        Coefficient of X**2
*        Coefficient of X**3
*        Coefficient of X**4
*        etc...
*     If a directory specification is not given, the file will be
*     assumed to be in the CGS4_MASKS directory.
*     Comments lines can be included at the beginning of the file
*     preceded by "!" characters.
*    Invocation :
*     CALL RED4_READ_COEFFS( FILE, MAXCOEFFS, NCOEFFS, COEFFS, STATUS )
*    Parameters :
*     FILE                = CHARACTER*(*)( READ )
*           The name of the file to be opened.
*     MAXCOEFFS           = INTEGER( READ )
*           The maximum number of coefficients allowed.
*     NCOEFFS             = INTEGER( WRITE )
*           The actual number of coefficients read
*     COEFFS( MAXCOEFFS ) = DOUBLE PRECISION( WRITE )
*           The coefficients read from the file.
*     STATUS        = INTEGER( UPDATE )
*           Global status. This must be ADAM__OK on entry.
*           If this routine completes successfully, the STATUS
*           will be ADAM__OK on exit. Any other value indicates
*           an error.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     29-Nov-1990: Original version.                     (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed,
*                  which would have made this routine
*                  fail under ADAM V1.9.                 (SMB)
*     22-Feb-1993: Conform to error strategy             (PND)
*     19-Jan-1994: Number of coeffs reported correctly   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'          ! Includes SAI_ERROR
*    Import :
      CHARACTER*(*)
     :  FILE                     ! Name of coefficients file
      INTEGER
     :  MAXCOEFFS                ! Maximum allowed number of coefficients
*    Export :
      INTEGER
     :  NCOEFFS                  ! Actual number of coefficients
      DOUBLE PRECISION
     :  COEFFS( MAXCOEFFS )      ! Coefficients read.
*    Status :
      INTEGER
     :  STATUS                   ! Global status
*    External references :
*    Global variables :
*    Local constants :
      INTEGER FOR__OK            ! Fortran I/O success status
      PARAMETER ( FOR__OK = 0 )
      INTEGER FOR__EOF           ! Fortran end-of-file status
      PARAMETER ( FOR__EOF = -1 )
*    Local variables :
      CHARACTER*80
     :  ERRMSG,                  ! Fortran I/O error message buffer
     :  LINE                     ! Line read from file
      INTEGER
     :  N,                       ! Record counter
     :  LUN,                     ! Fortran logical unit number
     :  IOS                      ! Fortran I/O error status
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain a free logical unit number
      CALL FIO_GUNIT( LUN, STATUS )

*   Check this has worked
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open the coefficients file
         OPEN ( UNIT = LUN,
     :          FILE = FILE,
     :          FORM = 'FORMATTED',
     :          STATUS = 'OLD',
     :          IOSTAT = IOS )

*      Check this has worked
         IF ( IOS .EQ. FOR__OK ) THEN

*         Skip any comment records (containing a "!") at the
*         beginning of the file.
            READ( LUN, '(A)', IOSTAT=IOS ) LINE

            DO WHILE ( ( IOS .EQ. FOR__OK ) .AND.
     :                 ( INDEX( LINE, '!' ) .NE. 0 ) )

               READ( LUN, '(A)', IOSTAT=IOS ) LINE
            END DO

*         Check the read has worked.
            IF ( IOS .EQ. FOR__OK ) THEN

*              A line without a "!" has been read successfully.
*              This must be the first coefficients record.
*              Backspace the file and read the line again with a
*              free format.
               BACKSPACE( LUN, IOSTAT=IOS )

               READ( LUN, *, IOSTAT=IOS ) COEFFS(1)

*            Initialise the number of coefficients read so far.
               NCOEFFS = 1

*            Read the rest of the coefficients, until an end-of-file
*            or error occurs, or until the maximum allowed number
*            of coefficients have been read.
               N = 1
               DO WHILE ( ( IOS .EQ. FOR__OK ) .AND.
     :                    ( N .LT. MAXCOEFFS ) )

                  N = N + 1
                  READ( LUN, *, IOSTAT=IOS ) COEFFS(N)

*               Only update the number of coefficients if the line
*               has been read successfully.
                  IF ( IOS .EQ. FOR__OK ) NCOEFFS = N
               END DO

*            Check the file has been read successfully. (The status
*            should either be "ok" or "end-of-file" at this point.
               IF ( ( IOS .EQ. FOR__OK ) .OR.
     :              ( IOS .EQ. FOR__EOF ) ) THEN

*               If the maximum number of coefficients have been read
*               and an end-of-file has not been reached, give a
*               warning but carry on using the coefficients read.
                  IF ( ( NCOEFFS .GT. MAXCOEFFS ) .AND.
     :                 ( IOS .EQ. FOR__OK ) ) THEN

                     CALL MSG_SETI( 'MAXCOEFFS', MAXCOEFFS )
                     CALL MSG_OUT( ' ', 'WARNING - '/
     :                 /'linearisation coefficients file '/
     :                 /'contains more than ^MAXCOEFFS '/
     :                 /'coefficients', STATUS )
                     CALL MSG_OUT( ' ', ' - The rest will be '/
     :                 /'ignored', STATUS )
                  END IF

*               Reset the status, to ensure that a legitimate
*               "end-of-file" is not reported as an error later on.
                  IOS = FOR__OK
               END IF
            END IF

*         If an error has occurred while reading the file, then
*         report it.
            IF ( IOS .NE. FOR__OK ) THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_READ_COEFFS: '/
     :           /'Error reading linearisation '/
     :           /'coefficients file (reason follows)', STATUS )
               CALL MSG_SETI( 'IOS', IOS )
               CALL ERR_REP( ' ', 'RED4_READ_COEFFS: '/
     :           /'File I/O Status = ^IOS', STATUS )
               CALL GEN_FORTERR( IOS, .TRUE., ERRMSG )
               CALL MSG_SETC( 'ERRMSG', ERRMSG )
               CALL ERR_REP( ' ', 'RED4_READ_COEFFS: '/
     :           /'^ERRMSG', STATUS )
            END IF

*         Close the coefficients file (ignoring the status
*         resulting from this).
            CLOSE( LUN, IOSTAT=IOS )
            CALL FIO_PUNIT( LUN, STATUS )
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_READ_COEFFS: '/
     :        /'Error opening linearisation '/
     :        /'coefficients file (reason follows)', STATUS )
            CALL MSG_SETI( 'IOS', IOS )
            CALL ERR_REP( ' ', 'RED4_READ_COEFFS: '/
     :        /'File I/O Status = ^IOS', STATUS )
            CALL GEN_FORTERR( IOS, .TRUE., ERRMSG )
            CALL MSG_SETC( 'ERRMSG', ERRMSG )
            CALL ERR_REP( ' ', 'RED4_READ_COEFFS: '/
     :        /'^ERRMSG', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETI( 'STATUS', STATUS )
         CALL ERR_REP( ' ', 'RED4_READ_COEFFS: '/
     :     /'Error obtaining logical unit '/
     :     /'number, Status = ^STATUS', STATUS )
      END IF

      END
