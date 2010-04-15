*+  RED4_GRPTOOBSN - Convert group name to first observation name
      SUBROUTINE RED4_GRPTOOBSN( GRP_FILE, OBS1_FILE, STATUS )
*    Description :
*     This routine converts a group file name of the form
*     RGyymmdd_gggg or RGDIR:RGyymmdd_gggg into the name of the first
*     observation file, of the form ODIR:Oyymmdd_N where N is the
*     first suitable file that actually exists.
*    Invocation :
*     CALL RED4_GRPTOOBSN( GRP_FILE, OBS1_FILE, STATUS )
*    Parameters :
*     GRP_FILE  = CHARACTER*(*)( READ )
*         The group file name (RGyymmdd_gggg or RGDIR:RGyymmdd_gggg)
*     OBS1_FILE = CHARACTER*(*)( WRITE)
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
*     19-Feb-1993: Conform to error strategy               (PND)
*      7-Nov-1994: Make vaguely portable                   (AB)
*     13-Jun-1996: Allow _<num>_pf_dbs etc!                (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Global variables:
      INCLUDE 'RED4_COMMON.INC'
*    Import :
      CHARACTER*(*) GRP_FILE  ! observation file name
*    Export:
      CHARACTER*(*) OBS1_FILE ! First observation file name
*    Status :
      INTEGER STATUS
*    External :
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      CHARACTER*20 TMP1       ! A small temporary character string
      CHARACTER*20 TMP2       ! A small temporary character string
      CHARACTER*20 LPREFIX    ! Prefix to add to file
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
      CALL CHR_FILL( ' ', OBS1_FILE )

*    Check for an underscore in the input filename
      UNDER_POS = INDEX( GRP_FILE, '_' )

      IF ( UNDER_POS .GT. 0 ) THEN

*      Look for a file separator in the input file
        COLON_POS = INDEX( GRP_FILE, SEPARATOR )
        CALL RED4_GET_PREFIX( 'O', LPREFIX, STATUS )
        OBS1_FILE = LPREFIX(:CHR_LEN(LPREFIX)) // 'o' /
     :   / GRP_FILE(COLON_POS+3:UNDER_POS)
        CALL CHR_RMBLK( OBS1_FILE )

        IF ( VERBOSE ) THEN
          CALL MSG_SETC( 'OROOT', OBS1_FILE )
          CALL MSG_OUT( ' ', 'RED4_GRPTOOBSN: Observation root is ^OROOT', STATUS )
        ENDIF

*    Set a maximum file counter
        CALL CHR_FILL( ' ', TMP1 )
        CALL CHR_FILL( ' ', TMP2 )
        TMP1 = GRP_FILE( UNDER_POS+1:LEN(GRP_FILE) )
        UNDER_POS = INDEX( TMP1, '_' )
        IF ( UNDER_POS .GT. 0 ) THEN
          TMP2 = TMP1( 1:UNDER_POS-1 )
        ELSE
          TMP2 = TMP1
        ENDIF
        CALL CHR_CTOI( TMP2, MAX_FILE_CNT, STATUS )
        IF ( VERBOSE) CALL MSG_OUT( ' ', 'Maximum file counter = '//TMP2(1:CHR_LEN(TMP2)), STATUS )

        DO WHILE (.NOT. FILE_EXISTS)

*       Increment the file counter
          FILE_COUNTER = FILE_COUNTER + 1

*       If the file counter is exceeded report an error (should never happen!)
          IF ( FILE_COUNTER .GT. MAX_FILE_CNT ) THEN
            CALL CHR_FILL( ' ', OBS1_FILE )
            CALL MSG_OUT( ' ', 'There are no observations available', STATUS )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_GRPTOOBSN: '/
     :        /'Unable to find a time reference!', STATUS )
            RETURN
          ENDIF

*       Form a filename
          CALL CHR_FILL( ' ', TMP1 )
          CALL CHR_ITOC( FILE_COUNTER, TMP1, CLEN )
          OBS1_FILE = OBS1_FILE(1:CHR_LEN(OBS1_FILE)) // TMP1(1:CLEN)

*       Inquire if file exists
          CALL DSA_SEEK_NAMED_STRUCTURE( OBS1_FILE, FILE_EXISTS, STATUS )
        ENDDO
      ELSE

        STATUS = SAI__ERROR
        CALL MSG_SETC( 'GRP_FILE', GRP_FILE )
        CALL ERR_REP( ' ', 'RED4_GRPTOOBSN: '/
     :    /'Invalid group filename ^GRP_FILE', STATUS )
      ENDIF

*    This is vastly simpler than what was here before (see commented out bit
*    below).  Surely it does the same thing?

      IF ( FILE_COUNTER .NE. 1 ) THEN
        CALL MSG_SETC( 'OBS1_FILE', OBS1_FILE )
        CALL MSG_OUT( ' ',
     :    'Using ^OBS1_FILE as a time reference', STATUS )
      ENDIF

      END
