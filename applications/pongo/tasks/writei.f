      SUBROUTINE WRITEI( STATUS )
*+
*  Name:
*     WRITEI

*  Purpose:
*     Write information to an output file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Write specified information concerning the current data-set to an
*     output file.

*  Usage:
*     writei action file

*  ADAM Parameters:
*     ACTION = _CHAR (Read and Write)
*        The type of information to be written. This may be one of the
*        following:
*           - "LABLST" -- Write the internal list of labels out.
*           - "DATA" -- Write out selected data.
*           - "AGIPIC" -- Write the label, name and comment for the
*           current AGI picture to the AGI database.
*
*        [The value is prompted for.]
*     FILE = FILENAME (Read and Write)
*        The name of the output file to be written.
*
*        [The value is prompted for.]
*     FORMAT = _CHAR (Read and Write)
*        The Fortran FORMAT to be used.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to "G25.16".
*     AGINAME = _CHAR (Read and Write)
*        The AGI name for the current picture. This may be one of the
*        following:
*           - "DATA" -- Used to indicate that the AGI picture contains
*           the representation of data in some graphical form (i.e. a
*           graph).
*           - "FRAME" -- Used to indicate that the AGI picture contains
*           a group of other plots (i.e. several "DATA" pictures).
*
*        ["DATA"]
*     AGICOMMENT = _CHAR (Read and Write)
*        The AGI comment for the current picture.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is set to "User viewport".
*     AGILABEL = _CHAR (Read and Write)
*        The AGI label for the current picture.
*
*        [The value is prompted for.]
*     X = _LOGICAL (Read and Write)
*        If TRUE, the XCOL data area will be output.
*        [FALSE]
*     Y = _LOGICAL (Read and Write)
*        If TRUE, the YCOL data area will be output.
*        [FALSE]
*     Z = _LOGICAL (Read and Write)
*        If TRUE, the ZCOL data area will be output.
*        [FALSE]
*     EX = _LOGICAL (Read and Write)
*        If TRUE, the EXCOL data area will be output.
*        [FALSE]
*     EY = _LOGICAL (Read and Write)
*        If TRUE, the EYCOL data area will be output.
*        [FALSE]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     10-APR-1990 (JBVAD::PAH):
*        Original version.
*     28-NOV-1990 (JBVAD::PAH):
*        Added the AGI save picture option.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     22-JUN-1994 (PDRAPER):
*        Changed to overwrite existing files (UNIX port).
*     23-FEB-1995 (PDRAPER):
*        Changed to write variable formats on OSF/1.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'AGI_PAR'          ! AGI global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global data

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NCMDS              ! Number of actions
      PARAMETER ( NCMDS = 3 )

*  External References:
      EXTERNAL INTCMD
      INTEGER INTCMD             ! Converts string into representative
                                 ! integer
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! String length
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT is open

*  Local Variables:
      CHARACTER * ( 20 ) ACTION  ! Action to be taken
      CHARACTER * ( AGI__SZNAM ) AGILAB ! AGI picture label
      CHARACTER * ( AGI__SZNAM ) AGINAM ! AGI picture name
      CHARACTER * ( 20 ) COMMANDS( NCMDS+1 ) ! List of commands
      CHARACTER * ( 80 ) COMMENT
      CHARACTER * ( 40 ) COMHED
      CHARACTER * ( 30 ) FMT
      CHARACTER * ( 132 ) OUTBUF ! Output buffer

      LOGICAL EXW
      LOGICAL EYW
      LOGICAL XW
      LOGICAL YW
      LOGICAL ZW
      LOGICAL OPEN               ! Output file is open

      INTEGER BASEID             ! AGI base picture ID
      INTEGER I                  ! Counter
      INTEGER ICMD               ! Command number
      INTEGER IOERR              ! Fortran I/O status
      INTEGER FD                 ! File descriptor
      INTEGER LITEM
      INTEGER PICID              ! AGI picture ID


*  Local Data:
      DATA COMMANDS /'LABLST', 'DATA', 'AGIPIC', ' ' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the Fortran I/O status.
      IOERR = 0
      OPEN = .FALSE.

*  Get the action.
      CALL PAR_GET0C( 'ACTION', ACTION, STATUS )
      CALL CHR_UCASE( ACTION )
      ICMD = INTCMD( COMMANDS, ACTION )

*  Check the action.
      IF ( ICMD .LE. 0 ) THEN

*     Action not known, report an error.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'WRITEI_NOCMD', 'Action not recognised.',
     :                 STATUS )
      ELSE IF ( ICMD .EQ. 2 ) THEN

*     Write the data out.
         CALL PON_ASFIO( 'FILE', 'WRITE', 'LIST', 132, FD, OPEN,
     :                   STATUS )
         CALL PAR_GET0L( 'X', XW, STATUS )
         CALL PAR_GET0L( 'Y', YW, STATUS )
         CALL PAR_GET0L( 'Z', ZW, STATUS )
         CALL PAR_GET0L( 'EX', EXW, STATUS )
         CALL PAR_GET0L( 'EY', EYW, STATUS )
         CALL PAR_GET0C( 'FORMAT', FMT, STATUS )
         FMT = '( A, 1X,' // FMT(:CHR_LEN(FMT)) // ')'
         OUTBUF = '!$'
         IF ( XW ) WRITE( OUTBUF, '( A, A )' )
     :                OUTBUF( : CHR_LEN( OUTBUF ) ), '$X$'
         IF ( EXW ) WRITE( OUTBUF, '( A, A )' )
     :                 OUTBUF( : CHR_LEN( OUTBUF ) ), '$X-ERR$'
         IF ( YW ) WRITE( OUTBUF, '( A, A )' )
     :                OUTBUF( : CHR_LEN( OUTBUF ) ), '$Y$'
         IF ( EYW ) WRITE( OUTBUF, '( A, A )' )
     :                 OUTBUF( : CHR_LEN( OUTBUF ) ), '$Y-ERR$'
         IF ( ZW ) WRITE( OUTBUF, '( A, A )' )
     :                OUTBUF( : CHR_LEN( OUTBUF ) ), '$Z$'
         CALL FIO_WRITE( FD, OUTBUF, STATUS )

*  Create output line by appending each new value to OUTBUF (hence
*  format (A,1X,...). First characters written to line are blank.
         DO 50 I = 1, NDAT
            OUTBUF = ' '
            IF ( XW ) WRITE( OUTBUF, FMT, IOSTAT=IOERR, ERR=200 )
     :                   ' ', XDATA( I )
            IF ( EXW ) WRITE( OUTBUF, FMT, IOSTAT=IOERR, ERR=200 )
     :                    OUTBUF( : CHR_LEN( OUTBUF ) ), ERRX( I )
            IF ( YW ) WRITE( OUTBUF, FMT, IOSTAT=IOERR, ERR=200 )
     :                   OUTBUF( : CHR_LEN( OUTBUF ) ), YDATA( I )
            IF ( EYW ) WRITE( OUTBUF, FMT, IOSTAT=IOERR, ERR=200 )
     :                    OUTBUF( : CHR_LEN( OUTBUF ) ), ERRY( I )
            IF ( ZW ) WRITE( OUTBUF, FMT, IOSTAT=IOERR, ERR=200 )
     :                   OUTBUF( : CHR_LEN( OUTBUF ) ), ZDATA( I )
            CALL FIO_WRITE( FD, OUTBUF, STATUS )
 50      CONTINUE

         IF ( OPEN ) THEN
            CALL FIO_CLOSE( FD, STATUS )
            CALL PAR_CANCL( 'FILE', STATUS )
         END IF
      ELSE IF ( ICMD.EQ.3 ) THEN

*  Save the current AGI picture in the database. Note that there is no
*  checking on the user giving sensible values here - this is done in
*  the interface file.
         IF ( PON_DEVOP( .TRUE., STATUS ) ) THEN
            CALL PAR_GET0C( 'AGINAME', AGINAM, STATUS )
            CALL PAR_GET0C( 'AGICOMMENT', COMMENT, STATUS )
            CALL PAR_GET0C( 'AGILABEL', AGILAB, STATUS )
            CALL PGQINF( 'NOW', COMHED, LITEM )
            CALL AGI_BEGIN
            CALL AGI_IBASE( BASEID, STATUS )
            CALL AGI_SELP( BASEID, STATUS )
            CALL AGP_SVIEW( AGINAM, 'PONGO: ' // COMHED( : LITEM ) //
     :                   ' ' // COMMENT, PICID, STATUS )
            CALL AGI_SLAB( PICID, AGILAB, STATUS )
            CALL AGI_END( -1, STATUS )
         END IF
      ELSE

*     Write the 'labels' out.
         CALL PON_ASFIO( 'FILE', 'WRITE', 'LIST', 0, FD, OPEN, STATUS )

*        CALL FIO_WRITE( FD, 'PROC DOLABELS', STATUS )

         DO 100 I = 1, ILABPTR

            IF ( LABLST( I )( 1 : 1 ) .EQ. 'L' ) THEN
               WRITE( OUTBUF,
     :            '( X, ''PTEXT'', X, 2( E16.9, X ), F7.2, X, ' //
     :            'F4.2, '' ~'' )' ) XLABAN( I ), YLABAN( I ),
     :            LABANG( I ), LABJUST( I )
               CALL FIO_WRITE( FD, OUTBUF( : CHR_LEN( OUTBUF ) ),
     :                         STATUS )
               WRITE( OUTBUF, '( 2X, ''"'', A, ''"'' )' )
     :                   LABLST( I )( 2 : CHR_LEN( LABLST( I ) ) )
               CALL FIO_WRITE( FD, OUTBUF( : CHR_LEN( OUTBUF ) ),
     :                         STATUS )
            ELSE IF ( LABLST( I )( 1 : 1 ) .EQ. 'M' ) THEN
               WRITE( OUTBUF, '( X, ''MARK'', X, 2( E16.9, X ), I4 )' )
     :                   XLABAN( I ), YLABAN( I ), INT( LABJUST( I ) )
               CALL FIO_WRITE( FD, OUTBUF( : CHR_LEN( OUTBUF ) ),
     :                         STATUS )
            ELSE IF ( LABLST( I )( 1 : 1 ) .EQ. 'V' ) THEN
               WRITE( OUTBUF, '( X, ''MOVE'', X, 2( E16.9, X ) )' )
     :                   XLABAN( I ), YLABAN( I )
               CALL FIO_WRITE( FD, OUTBUF( : CHR_LEN( OUTBUF ) ),
     :                         STATUS )
            ELSE IF ( LABLST( I )( 1 : 1 ) .EQ. 'D' ) THEN
               WRITE( OUTBUF, '( X, ''DRAW'', X, 2( E16.9, X ) )' )
     :                   XLABAN( I ), YLABAN( I )
               CALL FIO_WRITE( FD, OUTBUF( : CHR_LEN( OUTBUF ) ),
     :                         STATUS )
            END IF
 100     CONTINUE

*        CALL FIO_WRITE( FD, 'END PROC', STATUS )

         IF ( OPEN ) THEN
            CALL FIO_CLOSE( FD, STATUS )
            CALL PAR_CANCL( 'FILE', STATUS )
         END IF
      END IF

*  Abort.
 200  CONTINUE

*  Check the returned Fortran I/O status and act.
      IF ( IOERR .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_FIOER( 'IOERR', IOERR )
         CALL ERR_REP( 'WRITEI_FORT', 'Fortran error: ^IOERR', STATUS )
         CALL FIO_CANCL( 'FILE', STATUS )
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'WRITEI_END',
     :                              'WRITEI: Unable to write ' //
     :                              'information to the output file.',
     :                              STATUS )

      END
* $Id$
