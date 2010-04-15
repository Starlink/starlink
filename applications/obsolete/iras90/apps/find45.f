      SUBROUTINE FIND45( DECHIS, DECLOS, DISFIL, ILEVEL, PDISFI,
     :   PLPOS, PNEXTP, POFFIL, RAHIS, RALOS, SOPOS, STATUS )
*+
*  Name:
*     FIND45

*  Purpose:
*     To display a report comparing the area required for a source with
*     that available on the plate containing the source reference
*     position. This is used to report possible off edge sources to the
*     user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND45( DECHIS, DECLOS, DISFIL, ILEVEL, PDISFI,
*     :            PLPOS, PNEXTP, POFFIL, RAHIS, RALOS, SOPOS, STATUS )

*  Description:
*     To display a report comparing the area required for a source with
*     that available on the plate containing the source reference
*     position. This is used to report possible off edge sources to the
*     user.
*     The output can be made either to the terminal, a file, or both.
*     If the output is to a terminal only the user if offered the
*     opportunity of also requesting a file output.
*     In each case the user is first warned that the source area
*     used in this comparison is determined on a worst case assumption.

*  Arguments:
*     DECHIS = REAL (Given)
*        High Dec value for source area
*     DECLOS = REAL (Given)
*        Low Dec value for source area
*     DISFIL = CHARACTER * ( * ) (Given)
*        Value of the DISPLAYORFILE parameter
*     ILEVEL = INTEGER (Given)
*        Program interaction level
*     PDISFI = CHARACTER * ( * ) (Given)
*        Parameter DISPLAYORFILE, A = ask, B = both, D = display on
*        terminal, F = put to file
*     PLPOS = INTEGER (Given)
*        Index number of plate details in plate array
*     PNEXTP = CHARACTER * ( * ) (Given)
*        Parameter NEXTPAGE to trigger next page of display
*     POFFIL = CHARACTER * ( * ) (Given)
*        Parameter OFFEDGEFILE, the name of the file in which off edge
*        details are saved.
*     RAHIS = REAL (Given)
*        High RA value for source area
*     RALOS = REAL (Given)
*        Low RA value for source area
*     SOPOS = INTEGER (Given)
*        Pointer to the source currently being processed
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     FINDCRDD:
*        FIND10
*     IRA:
*        IRA_DTOC
*     CHR:
*        CHR_ISNAM, CHR_LEN, CHR_LCASE
*     ERR:
*        ERR_ANNUL,ERR_FLUSH, ERR_STAT
*     FIO:
*        FIO_CLOSE, FIO_OPEN, FIO_WRITE
*     MSG:
*        MSG_FMTC, MSG_FMTI, MSG_FMTR, MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_CHOIC, PAR_DEF0C, PAR_GET0C, PAR_GET0L,
*        PAR_PROMT

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     14-NOV-1991 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors
      INCLUDE 'FIO_PAR'          ! FIO constants
      INCLUDE 'FIO_ERR'          ! FIO errors
      INCLUDE 'PSX_ERR'          ! PSX errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      REAL DECHIS
      REAL DECLOS
      CHARACTER * ( * ) DISFIL
      INTEGER ILEVEL
      CHARACTER * ( * ) PDISFI
      INTEGER PLPOS
      CHARACTER * ( * ) PNEXTP
      CHARACTER * ( * ) POFFIL
      REAL RAHIS
      REAL RALOS
      INTEGER SOPOS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_ISNAM
      LOGICAL CHR_ISNAM          ! CHR routine to test whether the
                                 ! string is a valid filename
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! CHR routine to find the length of
                                 ! a string

*  Local Variables:
      CHARACTER * ( FIO__SZMOD ) ACMODE ! Access mode for FIO
      REAL AMCROS                ! Arc Minute value of cross scan
      REAL AMINSC                ! Arc Minute value of in scan
      CHARACTER * ( 80 ) BUFFER( 3 ) ! Buffer for output of source file
      CHARACTER * ( 1 ) CARRET   ! Contains the carrage control for
                                 ! internal writes/reads
      CHARACTER * ( IRA__SZFSC ) DEHIPC ! Char DEC High plate
      DOUBLE PRECISION DEHIPD    ! Doubl DEC High plate
      CHARACTER * ( IRA__SZFSC ) DEHISC    ! Char DEC High source
      DOUBLE PRECISION DEHISD    ! Doubl DEC High source
      CHARACTER * ( IRA__SZFSC ) DELOPC    ! Char DEC Low plate
      DOUBLE PRECISION DELOPD    ! Doubl DEC Low plate
      CHARACTER * ( IRA__SZFSC ) DELOSC    ! Char DEC Low source
      DOUBLE PRECISION DELOSD    ! Doubl DEC Low source
      INTEGER FD                 ! File descriptor obtained from FIO
      INTEGER LENNAM             ! Length of the source name
      CHARACTER * ( 30 ) MACHIN  ! Name of the hardware of the computer
      LOGICAL NEXTPA             ! Output of a parameter which triggers
                                 ! next page of display
      CHARACTER * ( 30 ) NODENM  ! Node name of the computer
      CHARACTER * ( FIO__SZFNM) OFFIL  ! String for actual off edge
                                       ! file name
      CHARACTER * ( 16 ) OFFNAM  ! String for default off edge file name
      CHARACTER * ( IRA__SZFSC ) RAHIPC ! Char RA High plate
      DOUBLE PRECISION RAHIPD    ! Doubl RA High plate
      CHARACTER * ( IRA__SZFSC ) RAHISC    ! Char RA High source
      DOUBLE PRECISION RAHISD    ! Doubl RA High source
      CHARACTER * ( IRA__SZFSC ) RALOPC    ! Char RA Low plate
      DOUBLE PRECISION RALOPD    ! Doubl RA Low plate
      CHARACTER * ( IRA__SZFSC ) RALOSC ! Char RA Low source
      DOUBLE PRECISION RALOSD    ! Doubl RA Low source
      INTEGER RECSZ              ! Record size for FIO (0=default)
      CHARACTER * ( 30 ) RELEAS  ! Version of the operating system
      CHARACTER * ( IRA__SZSCS ) SCS ! Value of coordinate system
      CHARACTER * ( 16 ) STNAME  ! String to contain source name and
                                 ! coordinate system
      CHARACTER * ( 30 ) SYSNAM  ! Name of the operating system
      CHARACTER * ( 1 ) TEMPDF   ! Temporary store for display/file var
      CHARACTER * ( 30 ) VERSON  ! Subversion of the operating system
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  *********************************************************************
*  Set up variables which are common to both display and file output
*  *********************************************************************

*  For both terminal and file output call subroutine to generate a
*  string containing the source name and coordinate system
                  CALL FIND10( SOPOS, STNAME, STATUS )

*  For both terminal and file output translate the 1950 coordinate
*  positions for both source and plate HI's and LOW's to character
*  strings.

*  First make double precision copies of the source and plate HI's and
*  LOW's
      RAHISD = DBLE ( RAHIS )
      DEHISD = DBLE ( DECHIS )
      RALOSD = DBLE ( RALOS )
      DELOSD = DBLE ( DECLOS )
      RAHIPD = DBLE ( PLHIRA( PLPOS ) )
      DEHIPD = DBLE ( PLHIDE( PLPOS ) )
      RALOPD = DBLE ( PLLORA( PLPOS ) )
      DELOPD = DBLE ( PLLODE( PLPOS ) )

*  Then use IRA_CTOD to translate then into character strings
      SCS = 'EQUATORIAL(B1950)        '
      CALL IRA_DTOC( RAHISD, DEHISD, SCS, 4, RAHISC, DEHISC, STATUS)
      CALL IRA_DTOC( RALOSD, DELOSD, SCS, 4, RALOSC, DELOSC, STATUS)
      CALL IRA_DTOC( RAHIPD, DEHIPD, SCS, 4, RAHIPC, DEHIPC, STATUS)
      CALL IRA_DTOC( RALOPD, DELOPD, SCS, 4, RALOPC, DELOPC, STATUS)

*  Change the radian values for cross scan and in scan to arc minutes
      AMCROS = SOCRSZ( SOPOS ) / AMTOR
      AMINSC = SOINSZ( SOPOS ) / AMTOR

*  *********************************************************************
*  Check the current value of the display or file parameter
*  *********************************************************************

*  First store the current value for restoring at the end of the
*  subroutine.
      TEMPDF = DISFIL

*  If the value of DISFIL is A ( ask ), or another invalid value
*  (ie not B, D or F ) ask user for a new value
 100  CONTINUE                 ! Start of 'DO WHILE' loop
         IF ( DISFIL .NE. 'B' ) THEN
            IF ( DISFIL .NE. 'D' ) THEN
               IF ( DISFIL .NE. 'F' ) THEN
                  CALL PAR_DEF0C( PDISFI, 'D', STATUS )
                  CALL PAR_PROMT( PDISFI, 'List to:- Display(D),'//
     :            ' File(F), Both(B), Neither(!)', STATUS )
                  CALL PAR_CHOIC( PDISFI, 'D', 'D,F,B', .FALSE.,
     :            DISFIL, STATUS )

*  Cancel the parameter for next time through
                  CALL PAR_CANCL( PDISFI, STATUS )

*  If the user enters abort !! return from the subroutine
                  IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  If the display of file parameter is null then do not display or
*  write an output file. Go to the tidy up at the end of the subroutine
                  IF ( STATUS .EQ. PAR__NULL ) THEN
                     CALL ERR_ANNUL( STATUS )

*  Go to the end of the subroutine
                     GO TO 300
*  Else go back to begining of check value of display or file
                  ELSE
                     GO TO 100
                  END IF
*  End if for if not B,D or F (or null)
               END IF
            END IF
         END IF

*  *********************************************************************
*  If DISFIL is B ( both) or D ( display) display output to terminal
*  ********************************************************************
      IF ( ( DISFIL .EQ. 'B' ) .OR. ( DISFIL .EQ. 'D' ) ) THEN

*  Output page heading
         CALL MSG_OUT( ' ', '  Area Required Off Edge Of Plate',
     :   STATUS )
         CALL MSG_OUT( ' ', ' _________________________________',
     :   STATUS )
         CALL MSG_OUT( ' ', ' ', STATUS )

*  Output source identifier heading
         CALL MSG_OUT( ' ', ' Source    Coord             Pos'//
     :   'ition              Region size', STATUS )
         CALL MSG_OUT( ' ', '  Name      Sys      1st Coord'//
     :   '       2nd Coord    Inscan  Xscan', STATUS )

*  Set up the variables to be included in source identifier message
         CALL MSG_FMTC( 'C1', 'A16', STNAME)
         CALL MSG_FMTC( 'C2', 'A12', SOCO1( SOPOS ) )
         CALL MSG_FMTC( 'C3', 'A12', SOCO2( SOPOS ) )
         CALL MSG_FMTR( 'R4', 'F5.1', AMINSC )
         CALL MSG_FMTR( 'R5', 'F5.1', AMCROS )

*  Output source identifier message
         CALL MSG_OUT( ' ', '^C1   ^C2    ^C3    ^R4 ^R5', STATUS )

*  Output messages giving plate number and tape identifier for source
         CALL MSG_OUT( ' ', ' ', STATUS )
         CALL MSG_FMTI( 'I1', 'I5', PLNUM( PLPOS ) )
         CALL MSG_FMTC( 'C4', 'A6', PLID( PLPOS ) )
         CALL MSG_OUT( ' ', ' From Plate number ^I1 on tape id  ^C4 ',
     :   STATUS )

*  Output region size heading
         CALL MSG_OUT( ' ', ' ', STATUS )
         CALL MSG_OUT( ' ', ' ', STATUS )
         CALL MSG_OUT( ' ', '                              Plate'//
     :               '         Required', STATUS )
         CALL MSG_OUT( ' ','                           hrs min sec'//
     :               '     hrs min sec', STATUS )

*  Alternately set up the variables to be included in RA message, and
*  display message
         CALL MSG_FMTC( 'C1', 'A12', RALOPC)
         CALL MSG_FMTC( 'C2', 'A12', RALOSC)
         CALL MSG_OUT( ' ', '  R. A.       LOW         ^C1    ^C2',
     :               STATUS )

         CALL MSG_FMTC( 'C1', 'A12', RAHIPC)
         CALL MSG_FMTC( 'C2', 'A12', RAHISC)
         CALL MSG_OUT( ' ', '  R. A.      HIGH         ^C1    ^C2',
     :               STATUS )

*  Output dec heading
         CALL MSG_OUT( ' ', ' ', STATUS )
         CALL MSG_OUT( ' ', '                           deg min sec'//
     :               '     deg min sec', STATUS )
*  Alternately set up the variables to be included in RA message, and
*  display message
         CALL MSG_FMTC( 'C1', 'A12', DELOPC)
         CALL MSG_FMTC( 'C2', 'A12', DELOSC)
         CALL MSG_OUT( ' ', '  DEC         LOW         ^C1    ^C2',
     :               STATUS )

         CALL MSG_FMTC( 'C1', 'A12', DEHIPC)
         CALL MSG_FMTC( 'C2', 'A12', DEHISC)
         CALL MSG_OUT( ' ', '  DEC        HIGH         ^C1    ^C2',
     :               STATUS )

*  *********************************************************************
*  If DISFIL is D ( display only) offer the user the opportunity to
*  output to file as well.
*  ********************************************************************

         IF ( DISFIL .EQ. 'D' ) THEN

*  To obtain a printable file of the display the user may enter F to the
*  display or file parameter, if he doesn't he should enter !
            CALL PAR_DEF0C( PDISFI, 'F', STATUS )
            CALL PAR_PROMT( PDISFI,
     :      'For a filed output of this display enter F, else enter !'
     :      , STATUS )
            CALL PAR_CHOIC( PDISFI, 'F', 'F', .FALSE., DISFIL, STATUS )

*  Cancel the parameter for next time through
            CALL PAR_CANCL( PDISFI, STATUS )

*  If the user enters abort !! return from the subroutine
            IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  If the display of file parameter is null then do not display or
*  write an output file. Go to the tidy up at the end of the subroutine
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               GO TO 300
            END IF

*  If DISFIL is 'B' then use nextpage prompt to hold display until the
*  user wants to move on
         ELSE

*  Ask user if he wants the next page
            CALL PAR_GET0L( PNEXTP, NEXTPA, STATUS )

*  Cancel the parameter for next time through
            CALL PAR_CANCL( PNEXTP, STATUS )

*  If the user enters abort !! return from the subroutine
            IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  Check whether the parameter was entered as null
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               GO TO 300
            END IF

         END IF

*  End if for if DISFIL is terminal or both
      END IF

*  *********************************************************************
*  If DISFIL is B ( both) or F ( file ) put output to file
*  ********************************************************************
      IF ( ( ( DISFIL .EQ. 'B' ) .OR. ( DISFIL .EQ. 'F' ) ) ) THEN

*  Use default Fortran maximum record size ( 133 bytes)
         RECSZ = 0

*  Use access mode write
         ACMODE = 'WRITE'

*  Start of loop which checks that the users file name is a valid file
*  name
 200     CONTINUE

*  Generate a suitable dynamic default file name for the off edge file
         LENNAM = CHR_LEN( SONAME( SOPOS ) )
         OFFNAM(1:LENNAM)  = SONAME( SOPOS )
         OFFNAM( LENNAM + 1 : LENNAM + 9) = '_OFFEDGE'
         OFFNAM( LENNAM + 10: 16) = '      '

*  Determine the operating system being used
         CALL PSX_UNAME( SYSNAM, NODENM, RELEAS, VERSON, MACHIN,
     :   STATUS )

*  If the system is not VMS change the name to lower case
         IF ( SYSNAM .NE. 'VMS' ) THEN
            CALL CHR_LCASE( OFFNAM )
         END IF

*  Enter the generated name as the default for the parameter
         CALL PAR_DEF0C( POFFIL, OFFNAM, STATUS )

*  *********************************************************************
*  Ask the user for an output file name. If this is ! the data is not
*  stored. If a valid file name is given the file is opened for writing.
*  *********************************************************************
         CALL PAR_GET0C( POFFIL, OFFIL, STATUS )

*  Cancel the filename parameter so that a new value can be obtained
*  next time through
         CALL PAR_CANCL( POFFIL, STATUS )

*  Check that the file name given by the user is a valid filename
         IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :        ( .NOT. CHR_ISNAM( OFFIL ) ) ) THEN
            CALL MSG_OUT( ' ',
     :      ' The name was not a valid file name, please reenter ',
     :      STATUS )
            GO TO 200
         END IF

*  If the user enters abort !! return from the subroutine
         IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  If the filename association parameter is null then do not write an
*  output file. Annul the error message which sets the STATUS back to
*  SAI__OK and display a message.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL MSG_OUT( ' ', ' The source off edge details will'//
     :      ' not be saved, but you can continue processing',STATUS )

*  If a valid file name is supplied by the user then open file
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Open the off edge file
            CALL FIO_OPEN( OFFIL, ACMODE, 'FORTRAN', RECSZ,
     :      FD, STATUS )

*  ********************************************************************
*  List details of off edge area and plate area to file
*  *********************************************************************

*  Output page heading
            BUFFER( 1 ) = '   Area Required Off Edge Of Plate'
            CALL FIO_WRITE( FD, BUFFER( 1 ), STATUS )
            BUFFER( 2 ) = '  _________________________________'
            CALL FIO_WRITE( FD, BUFFER( 2 ), STATUS )
            BUFFER( 3 ) = '                                  '
            CALL FIO_WRITE( FD, BUFFER( 3 ), STATUS )

*  Output source identifier heading
            BUFFER( 1 ) = ' Source     Coord              Position'//
     :      '                Region size'
            CALL FIO_WRITE( FD, BUFFER( 1 ), STATUS )
            BUFFER( 2 ) = '  Name       Sys        1st Coord'//
     :      '       2nd Coord     Inscan   Xscan'
            CALL FIO_WRITE( FD, BUFFER( 2 ), STATUS )

*  Set up the variables to be included in source identifier message
            WRITE( BUFFER( 3 )(2:18), '(A16)' ) STNAME
            WRITE( BUFFER( 3 )(22:34), '(A12)' ) SOCO1( SOPOS )
            WRITE( BUFFER( 3 )(39:51), '(A12)' ) SOCO2( SOPOS )
            WRITE( BUFFER( 3 )(56:61), '(F5.1)' ) AMINSC
            WRITE( BUFFER( 3 )(63:68), '(F5.1)' ) AMCROS

* 9999       FORMAT ( A1, A16, 3X, A12, 4X, A12, 4X, F5.1, 1X, F5.1 )
*  Output source identifier message
            CALL FIO_WRITE( FD, BUFFER( 3 ), STATUS )

*  Output messages giving plate number and tape identifier for source
            BUFFER( 1 ) = '                                  '
            CALL FIO_WRITE( FD, BUFFER( 1 ), STATUS )
            BUFFER( 2 ) = ' From Plate number         on tape id     '
            WRITE( BUFFER( 2 )(19:23), '(I5)' ) PLNUM( PLPOS )
            WRITE( BUFFER( 2 )(40:45), '(A6)' ) PLID( PLPOS )
            CALL FIO_WRITE( FD, BUFFER( 2 ), STATUS )

*  Output region size heading
            BUFFER( 1 ) = '                                  '
            CALL FIO_WRITE( FD, BUFFER( 1 ), STATUS )
            CALL FIO_WRITE( FD, BUFFER( 1 ), STATUS )
            BUFFER( 2 ) = '                            Plate'//
     :              '         Required'
            CALL FIO_WRITE( FD, BUFFER( 2 ), STATUS )
            BUFFER( 3 ) = '                         hrs min sec'//
     :              '     hrs min sec'
            CALL FIO_WRITE( FD, BUFFER( 3 ), STATUS )

*  Alternately set up the variables to be included in RA message, and
*  display message
            BUFFER( 2 ) = '   R. A.       LOW          '
            WRITE( BUFFER( 2 )(25:36), '(A12)' ) RALOPC
            WRITE( BUFFER( 2 )(41:52), '(A12)' ) RALOSC
            CALL FIO_WRITE( FD, BUFFER( 2 ), STATUS )
            BUFFER( 3 ) = '   R. A.      HIGH           '
            WRITE( BUFFER( 3 )(25:36), '(A12)' ) RAHIPC
            WRITE( BUFFER( 3 )(41:52), '(A12)' ) RAHISC
            CALL FIO_WRITE( FD, BUFFER( 3 ), STATUS )

*  Output dec heading ( BUFFER( 1 ) contains a blank line )
            CALL FIO_WRITE( FD, BUFFER( 1 ), STATUS )
            BUFFER( 2 ) = '                         deg min sec'//
     :              '     deg min sec'
            CALL FIO_WRITE( FD, BUFFER( 2 ), STATUS )

*  Alternately set up the variables to be included in RA message, and
*  display message
            BUFFER( 2 ) = '   DEC         LOW          '
            WRITE( BUFFER( 2 )(25:36), '(A12)' ) DELOPC
            WRITE( BUFFER( 2 )(41:52), '(A12)' ) DELOSC
            CALL FIO_WRITE( FD, BUFFER( 2 ), STATUS )
            BUFFER( 3 ) = '   DEC        HIGH           '
            WRITE( BUFFER( 3 )(25:36), '(A12)' ) DEHIPC
            WRITE( BUFFER( 3 )(41:52), '(A12)' ) DEHISC
            CALL FIO_WRITE( FD, BUFFER( 3 ), STATUS )

*  *********************************************************************
*  Check whether an error has occured
*  *********************************************************************
            IF ( STATUS .NE. SAI__OK ) THEN

*  Check whether there was an error message , indicated by the STATUS
*  from ERR_STAT not SAI__OK, and flush the message, which sets the
*  status to SAI__OK. The net result is that the STATUS should be O.K.
*  on exit from this block
               CALL ERR_STAT( STATUS )
               IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

            END IF

*  Close the file
            CALL FIO_CLOSE( FD, STATUS )

         END IF

*  End if for if DISFIL is file or both
      END IF

*  If any parameter is entered as null at any point the program GO TO's
*  here
 300  CONTINUE

*  Restore the correct value of DISFIL
      DISFIL = TEMPDF

      END
