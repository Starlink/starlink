*+  RED4_OBSTOOBSN - Convert observation name to first observation name
      SUBROUTINE RED4_OBSTOOBSN( OBS_FILE, OBS1_FILE, STATUS )
*    Description :
*     This routine converts an observation file name of the form
*     Oyymmdd_oooo or ODIR:Oyymmdd_oooo into the name of the first
*     observation file, of the form ODIR:Oyymmdd_N where N is the
*     first suitable file that actually exists.
*    Invocation :
*     CALL RED4_OBSTOOBSN( OBS_FILE, OBS1_FILE, STATUS )
*    Parameters :
*     OBS_FILE  = CHARACTER*(*)( READ )
*         The observation file name (Oyymmdd_oooo or ODIR:Oyymmdd_oooo)
*     OBS1_FILE = CHARACTER*(*)( WRITE )
*         The first observation file name (ODIR:Oyymmdd_N)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     P.N.Daly     (JACH::PND)
*    History :
*     17-Jul-1992: Original version.                       (PND)
*      7-Nov-1994: Make vaguely portable                   (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Global variables:
*    Import :
      CHARACTER*(*)
     :  OBS_FILE              ! observation file name
*    Export:
      CHARACTER*(*)
     :  OBS1_FILE             ! First observation file name
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      CHARACTER*20
     :  TMP                   ! A small temporary character string
      CHARACTER*20 LPREFIX    ! prefix to add to file
      INTEGER
     :  UNDER_POS,            ! Position of underscore in character string
     :  COLON_POS,            ! Position of colon in character string
     :  FILE_COUNTER,         ! A counter
     :  MAX_FILE_CNT,         ! The maximum number of files
     :  CLEN                  ! A string length
      LOGICAL
     :  FILE_EXISTS           ! .TRUE. if a file exists
*    Local data :
*-

*    Check status on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    Initialise some variables
      FILE_EXISTS  = .FALSE.
      FILE_COUNTER = 0
      OBS1_FILE    = ' '

*    Check for an underscore in the input filename
      UNDER_POS = INDEX( OBS_FILE, '_' )

      IF ( UNDER_POS .GT. 0 ) THEN

*      Check for a prefix in the input filename
        COLON_POS = INDEX( OBS_FILE, ':' )
        IF ( COLON_POS .EQ. 0 ) COLON_POS = INDEX( OBS_FILE, '/' )

        CALL RED4_GET_PREFIX ('O', LPREFIX, STATUS)

        OBS1_FILE = LPREFIX(:CHR_LEN(LPREFIX)) /
     :   / OBS_FILE(COLON_POS+1:UNDER_POS)

*    Set a maximum file counter
        TMP = OBS_FILE( UNDER_POS+1:LEN(OBS_FILE) )
        CALL CHR_CTOI( TMP, MAX_FILE_CNT, STATUS )

        DO WHILE (.NOT. FILE_EXISTS)

*       Increment the file counter
          FILE_COUNTER = FILE_COUNTER + 1

*       If the file counter is exceeded report an error (should never happen!)
          IF ( FILE_COUNTER .GT. MAX_FILE_CNT ) THEN
            OBS1_FILE = ' '
            CALL MSG_OUT( ' ',
     :        'There are no observations available', STATUS )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_OBSTOOBSN: '/
     :        /'Unable to find a time reference!', STATUS )
            RETURN
          END IF

*       Form a filename
          TMP = ' '
          CALL CHR_ITOC( FILE_COUNTER, TMP, CLEN )
          OBS1_FILE = LPREFIX(:CHR_LEN(LPREFIX)) /
     :      / OBS_FILE(COLON_POS+1:UNDER_POS) /
     :      / TMP(1:CLEN)

*       Inquire if file exists
          CALL DSA_SEEK_NAMED_STRUCTURE( OBS1_FILE,
     :       FILE_EXISTS, STATUS )

        END DO
      ELSE

        STATUS = SAI__ERROR
        CALL MSG_SETC( 'OBS_FILE', OBS_FILE )
        CALL ERR_REP( ' ', 'RED4_OBSTOOBSN: '/
     :    /'Invalid observation filename ^OBS_FILE', STATUS )
      END IF

      IF ( FILE_COUNTER .NE. 1 ) THEN
        CALL MSG_SETC( 'OBS1_FILE', OBS1_FILE )
        CALL MSG_OUT( ' ',
     :    'Using ^OBS1_FILE as a time reference', STATUS )
      END IF

      END

