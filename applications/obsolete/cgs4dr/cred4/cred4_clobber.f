*+  CRED4_CLOBBER - Delete a reduced observation file
      SUBROUTINE CRED4_CLOBBER( INVAL, STATUS )
*    Description :
*     V4.2 of DSA introduced a bug whereby a RO file could not
*     be re-reduced if the original existed. This clobbers any
*     input RO file so letting the DR continue unabashed.
*    Invocation :
*     CALL CRED4_CLOBBER( INVAL, STATUS )
*    Parameters :
*     INVAL  = CHARACTER*(*)( READ )
*         The observation file name (Oyymmdd_oooo or oyymmdd_oooo)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status
*    Authors :
*     P.N.Daly  (JACH::PND)
*    History :
*     15-Apr-1996: Original Unix version.                  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables:
      INCLUDE 'CRED4COM.INC'
*    Import :
      CHARACTER*(*) INVAL            ! observation file name
*    Status :
      INTEGER STATUS
*    External :
      INTEGER CHR_LEN
*    Local variables :
      CHARACTER*255 ROFILE           ! The RO file
      INTEGER SEPPOS                 ! Position of separator
      INTEGER GUNIT                  ! I/O unit number
      INTEGER IOC_STAT               ! Open status code
*    Parameters:
      INTEGER IOC__OK                ! I/O code OK
      PARAMETER ( IOC__OK = 0 )
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find last occurrence of file separator (0 or N)
      SEPPOS = CHR_LEN( INVAL )
      CALL CHR_FIND( INVAL, SEPARATOR, .FALSE., SEPPOS )

*    Set the output file to it's full filename
      CALL CHR_FILL( ' ', ROFILE )
      IF ( INVAL(SEPPOS+1:SEPPOS+1).EQ.'O' .OR. INVAL(SEPPOS+1:SEPPOS+1).EQ.'o' ) THEN
        ROFILE = RODIR(1:CHR_LEN(RODIR)) // 'r' // INVAL(SEPPOS+1:CHR_LEN(INVAL))
      ELSE IF ( INVAL(SEPPOS+1:SEPPOS+1).EQ.'I' .OR. INVAL(SEPPOS+1:SEPPOS+1).EQ.'i' ) THEN
        ROFILE = RIDIR(1:CHR_LEN(RIDIR)) // 'r' // INVAL(SEPPOS+1:CHR_LEN(INVAL))
      ENDIF
      CALL CHR_RMBLK( ROFILE )

*    Now add the filetype
      IF ( CGS4_FORMAT(1:3).EQ.'ndf' .OR. CGS4_FORMAT(1:3).EQ.'NDF' ) THEN
        ROFILE = ROFILE(1:CHR_LEN(ROFILE)) //'.sdf'
      ELSE
        ROFILE = ROFILE(1:CHR_LEN(ROFILE)) //'.dst'
      ENDIF
      CALL CHR_RMBLK( ROFILE )

*    Now open the file and close it with delete specified
      CALL FIO_GUNIT( GUNIT, STATUS )
      OPEN( UNIT=GUNIT, NAME=ROFILE(1:CHR_LEN(ROFILE)), STATUS='UNKNOWN', IOSTAT=IOC_STAT )
      IF ( IOC_STAT .EQ. IOC__OK ) THEN
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Deleting reduced observation file '//ROFILE(1:CHR_LEN(ROFILE)), STATUS )
         CLOSE( UNIT=GUNIT, STATUS='DELETE' )
      ELSE
         IF ( VERBOSE ) CALL MSG_OUT( ' ', ROFILE(1:CHR_LEN(ROFILE))//' does not exist', STATUS )
      ENDIF
      CALL FIO_PUNIT( GUNIT, STATUS )

*    Exit subroutine
      END


