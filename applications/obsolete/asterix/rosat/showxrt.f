*+  SHOWXRT - Produces a 1 page summary of a ROSAT XRT observation
      SUBROUTINE SHOWXRT( STATUS )
*    Description :
*     Program to provide a summary of an XRT observation to a file
*     or to the screen. Gets the info. from the XRT header file -
*     Rootname.HDR. It may also be used to provide a list of the
*     MJDS of the on and off times of the XRT observation.
*    Parameters :
*     RAWDIR            = CHAR (read) Directory name
*     ROOTNAME          = CHAR (read) Rootname for header file
*     FILENAME          = CHAR (read) Ouput text file or TT for screen
*     SHOWOBS           = LOGICAL (READ) Produce the observation listing ?
*     TIM
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*       Richard Saxton  (LTVAD::RDS)
*
*    History :
*
*         Nov 91 Original
*         Feb 92 : V1.5-2  Formats changed to handle large observation lengths
*                          e.g. 200 days. (RDS)
*         Apr 94 : V1.7-0  RATionalised for new release of asterix (LTVAD::JKA)
*      20 May 94 : V1.7-1  Use AIO for output (DJA)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'XRTLIB(INC_XRTSRT)'
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Status :
      INTEGER                 STATUS
*    Functions :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local Constants :
      CHARACTER*30            VERSION
         PARAMETER          ( VERSION = 'SHOWXRT - version 1.7-1')
      INTEGER MAXRAW
         PARAMETER          ( MAXRAW = 20 )

*    Local variables :
      RECORD /XRT_HEAD/ HEAD         ! Observation header information
      RECORD /XRT_SCFDEF/ SRT        ! Sort info.
      CHARACTER*30 VERS              ! SASS version date
      LOGICAL LSHOW,LTIME
*-
*   Version anouncement
      CALL MSG_PRNT(VERSION)

*   Initialize ASTERIX common blocks
      CALL AST_INIT
*
*     Get directory name from user and display the available observations.
*     Get rootname of file wanted.
      CALL XSORT_FILESELECT(SRT, HEAD, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999

      CALL CHR_UCASE(HEAD.ORIGIN)
      IF (HEAD.ORIGIN.EQ.'OMD') THEN
*      Read header info from the corresponding .HDR file
         CALL XRT_RDHEAD( .TRUE., SRT, HEAD, VERS, STATUS)
      ELSE
         CALL RAT_GETXRTHEAD(SRT.ROOTNAME, HEAD, STATUS)
      ENDIF

*   Produce an observation summary ?
      CALL PAR_GET0L('SHOWOBS', LSHOW, STATUS)

*   Produce a list of ON/OFF times ?
      CALL PAR_GET0L('TIMLIST', LTIME, STATUS)

*   Write observation description
      CALL SHOWXRT_OUT(LSHOW, LTIME, HEAD, STATUS)

 999  CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END


*+  SHOWXRT_OUT - Outputs description of observation
      SUBROUTINE SHOWXRT_OUT(LSHOW, LTIME, HEAD,  STATUS)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Import :
      LOGICAL LSHOW                      ! Produce an observation summary ?
      LOGICAL LTIME                      ! Produce a list of On/OFF times ?
      RECORD /XRT_HEAD/ HEAD
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
*     <declarations and descriptions for exported arguments>
*    Status :
      INTEGER STATUS
*    Local constants :
      DOUBLE PRECISION DTOR
         PARAMETER(DTOR = 3.1415926535 / 180.)
*    Local variables :
      CHARACTER*79 SBUF
      CHARACTER*20 TSTRING1,TSTRING2
      CHARACTER*10 RASTRING,DECSTRING
      DOUBLE PRECISION ENDMJD                 ! Last MJD of the obs.
      DOUBLE PRECISION SMJD,EMJD              ! Start and end MJDs of each slot
      REAL EXPOS
      REAL RA,DEC
      INTEGER LP
      INTEGER IND1A,IND1B,IND2A,IND2B
      CHARACTER*24 CSTRING1,CSTRING2 ! Time strings
      INTEGER MUNIT

      INTEGER			AID			! AIO identifier
      INTEGER			OWIDTH			! output width
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Produce an observation summary if wanted
      IF (LSHOW) THEN

*      Get name of output file for obs. summary
        CALL AIO_ASSOCO( 'SUMFILE', 'LIST', AID, OWIDTH, STATUS )
        IF (STATUS .NE. SAI__OK) GOTO 999

*      Convert base time to character string
        CALL CONV_MJDDAT(HEAD.BASE_MJD, TSTRING1)

*      Convert end time to char. string
        ENDMJD = HEAD.BASE_MJD + HEAD.TEND(HEAD.NTRANGE) / 86400.
        CALL CONV_MJDDAT(ENDMJD, TSTRING2)

*      Output general info.
        CALL AIO_BLNK( AID, STATUS )
        WRITE( SBUF,1010) HEAD.OBSERVER, HEAD.TITLE, HEAD.FILTER
 1010   FORMAT( 1X, 'ROR_ID: ',A12,2X,'TARGET :',A12,'FILTER :', A8 )
        CALL AIO_WRITE( AID, SBUF, STATUS )
        CALL AIO_BLNK( AID, STATUS )
        WRITE(SBUF,1015) TSTRING1, TSTRING2
 1015   FORMAT(1X,'Start_time : ',A20,2X,'End_time : ',A20)
        CALL AIO_WRITE( AID, SBUF, STATUS )

*      Write SASS version id.
        CALL AIO_BLNK( AID, STATUS )
        CALL MSG_SETC( 'SASS', HEAD.SASS_DATE )
        CALL AIO_WRITE( AID, ' SASS version: ^SASS', STATUS )
        CALL AIO_BLNK( AID, STATUS )

*      Write detector ID
        CALL MSG_SETC( 'DET', HEAD.DETECTOR )
        CALL AIO_WRITE( AID, ' Detector : ^DET', STATUS )
        CALL AIO_BLNK( AID, STATUS )

*      Header for ON/OFF times
        CALL AIO_WRITE( AID, ' Observation slots : '/
     :            /'(secs from start_time)', STATUS )

*      Write list of ON/OFF times
        CALL AIO_WRITE( AID, '      ON        OFF     Duration',
     :                  STATUS )
        EXPOS = 0.0
        DO LP=1,HEAD.NTRANGE
          WRITE(SBUF,1020)HEAD.TSTART(LP),HEAD.TEND(LP),
     :                  (HEAD.TEND(LP)-HEAD.TSTART(LP))
          CALL AIO_WRITE( AID, SBUF, STATUS )
          EXPOS = EXPOS + HEAD.TEND(LP) - HEAD.TSTART(LP)
        END DO
 1020   FORMAT(2X, F10.1, X, F10.1, X, F10.1)
        CALL AIO_BLNK( AID, STATUS )

*      Write exposure time
        WRITE( SBUF,1030) HEAD.TEND(HEAD.NTRANGE) / 86400.0
 1030   FORMAT(1X,'Total elapsed time (days):',F7.2)
        CALL AIO_WRITE( AID, SBUF, STATUS )
        CALL MSG_SETR( 'EXP', EXPOS )
        CALL AIO_WRITE( AID, ' Total slot duration       '/
     :                  /': ^EXP seconds', STATUS )
        CALL AIO_BLNK( AID, STATUS )

*      Write RA and DEC of observation
        RA = HEAD.AXIS_RA * DTOR
        DEC = HEAD.AXIS_DEC * DTOR
        CALL STR_RRADTOC( RA, 'HH:MM:SS.S', RASTRING, STATUS )
        CALL STR_RRADTOC( DEC, 'SDD:MM:SS', DECSTRING, STATUS )
        CALL MSG_SETC( 'RA', RASTRING )
        CALL MSG_SETC( 'DEC', DECSTRING )
        CALL AIO_WRITE( AID, ' RA: ^RA DEC: ^DEC', STATUS )

*      Close device
        CALL AIO_CANCL( 'SUMFILE', STATUS )

      END IF

*    Produce a list of ON/OFF times if wanted
      IF (LTIME) THEN

*      Get logical unit
        CALL FIO_GUNIT(MUNIT, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error getting unit for output file')
            GOTO 999
         ENDIF
*
*   Open the output file
         OPEN(UNIT=MUNIT, FILE='XRT_TIMES.LIS', STATUS='NEW')
*
         DO LP=1,HEAD.NTRANGE
            SMJD = HEAD.BASE_MJD + HEAD.TSTART(LP)/86400.
            EMJD = HEAD.BASE_MJD + HEAD.TEND(LP)/86400.
*
*    Convert start and stop time to an MJD and put an M in front of it.
            WRITE(CSTRING1, FMT='(E23.15)')SMJD
            CALL CHR_FANDL(CSTRING1,IND1A,IND1B)
*
            WRITE(CSTRING2, FMT='(E23.15)')EMJD
            CALL CHR_FANDL(CSTRING2,IND2A,IND2B)
*
*      Write these two MJDs to the output file
            WRITE(MUNIT, 1000)CSTRING1(IND1A:IND1B),
     &                              CSTRING2(IND2A:IND2B)
         ENDDO
*
1000     FORMAT(X,'M',A,4X,'M',A)
*
         CALL FIO_PUNIT(MUNIT, STATUS)

      END IF
*
999   CONTINUE
*
      END
