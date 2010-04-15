

*********************************************
*** MODIFIED KAPPA/KAPGEN CODE ADDED HERE ***
*********************************************


      SUBROUTINE ESP1_AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN,
     :                      EXCLAIM,STATUS)
*+
*    Description :
*
*     This routine opens a sequential file via FIO_ASSOC.  Up to four
*     attempts may be made to open the file.  If a null response is
*     supplied the file is not opened, and the flag returned indicates
*     this fact.
*
*    Invocation :
*
*      CALL ESP1_AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN,
*                      EXCLAIM,STATUS)

*
*    Arguments :
*
*     PNFILE=CHARACTER*(*)
*         Parameter name by which file is to be opened
*     ACMODE=CHARACTER*(*)
*         Expression giving the required access mode.
*           Valid modes are: 'READ', 'WRITE', 'UPDATE' and 'APPEND'.
*           For details, see FIO_OPEN.
*     FORM=CHARACTER*(*)( READ )
*         Expression giving the required formatting of the file.
*           Valid formats are: 'FORTRAN', 'LIST', 'NONE' and
*           'UNFORMATTED'. For details, see FIO_OPEN.
*     RECSZ=INTEGER( READ )
*         Expression giving the maximum record size in bytes.
*           Set it to zero if the Fortran default is required.
*     FD=INTEGER( WRITE )
*         Variable to contain the file descriptor.
*     OPEN=LOGICAL( WRITE )
*         If true the file has been opened.
*     EXCLAIM=LOGICAL( WRITE )
*         If true then the user input was '!'.
*     STATUS=INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise looping flag
*     Do while no error obtaining the name and opening the output file
*       and maximum number of attempts not exceeded
*        Get file name and open file
*        If null returned then
*           Set flag so that a log file will not be created
*           Annul the error
*           Exit from the loop
*        Else if error occurred then
*           If abort requested, do so
*           Increment loop counter
*           If maximum number of attempts not exceeded then
*              Report error
*           Else
*              Set looping flag to exit
*           Endif
*             Cancel parameter used to get filename
*        Else
*           Set flag to indicate that the file has been opened
*           Set looping flag to false
*        Endif
*     Enddo
*     If error then
*        Report and abort
*     Endif
*     Return
*
*    Bugs :
*
*     None known.
*-
*    Authors :
*
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     1989 Jul 25: Original (RL.STAR::CUR).
*     1990 Feb 20: Renamed from AIF_OPFIO (RAL::CUR).
*     1994 Mar 1: Modified to return EXCLAIM (CARDIFF::GJP).
*     1997 Feb 24: MOdified for Linux (GJP).
*
*    Type definitions :

      IMPLICIT  NONE           ! no implicit typing allowed

*    Global constants :
      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE  'PAR_ERR'       ! parameter-system errors

*    Import :
      CHARACTER*(*) PNFILE     ! File Parameter Name
      CHARACTER*(*) ACMODE     ! File access mode
      CHARACTER*(*) FORM       ! Required form of carriagecontrol
      INTEGER RECSZ            ! File record size

*    Export :
      LOGICAL OPEN             ! File opened successfully
      LOGICAL EXCLAIM          ! File name was exclaimation
      INTEGER FD               ! File descriptor

*    Status :
      INTEGER STATUS

*    Local Constants :
      INTEGER MXLOOP           ! Maximum number of attempts at
                               ! opening a data file
      PARAMETER ( MXLOOP=4 )

      INTEGER LOOP             ! Number of attempts to open the file

      LOGICAL LOOPAG           ! Loop again to open output file

*.

*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

      LOOP=0
      LOOPAG=.TRUE.
      OPEN=.FALSE.
      EXCLAIM=.FALSE.
      DO WHILE ( LOOPAG )

*       attempt to obtain and open a file to output listing

         CALL FIO_ASSOC( PNFILE, ACMODE, FORM, RECSZ, FD, STATUS )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            OPEN=.FALSE.
            LOOPAG=.FALSE.
            EXCLAIM=.TRUE.
            CALL ERR_ANNUL( STATUS )
         ELSE IF ( STATUS .NE. SAI__OK ) THEN

            IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

*         Here if filename is not allowed or file is not opened
*         - try again
*         Need to flush error here, as not quitting routine

            LOOP=LOOP + 1
            IF ( LOOP .LE. MXLOOP ) THEN
               CALL MSG_SETC( 'FILNAM', PNFILE )
               CALL ERR_REP( 'ERR_AIF_ASFIO_NOFI',
     :           'AIF_ASFIO: Could not open file $^FILNAM - try again',
     :           STATUS )
               CALL ERR_FLUSH( STATUS )
            ELSE

*             end looping as user is having serious problems

               LOOPAG=.FALSE.
            END IF

            CALL PAR_CANCL( PNFILE, STATUS )

         ELSE

*          no problem, so exit loop

            LOOPAG=.FALSE.
            OPEN=.TRUE.

*       end of file-opened-successfully check

         END IF
      END DO

*    abort for repeated error

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_AIF_ASFIO_NOOPEN',
     :     'AIF_ASFIO: Repeatedly unable to open a file.', STATUS )
      END IF

 999  CONTINUE

      END
