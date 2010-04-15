*+  CRED4_INTTOROBS - Convert integration file name to reduced observation
      SUBROUTINE CRED4_INTTOROBS( IFILE, ROFILE, STATUS )
*    Description :
*     Iyymmdd_oooo_iiii -> RODIR:ROyymmdd_oooo for VMS
*     Iyymmdd_oooo_iiii -> $RODIR/ROyymmdd_oooo for Unix
*    Invocation :
*     CALL CRED4_INTTOROBS( IFILE, ROFILE, STATUS )
*    Parameters :
*     IFILE  = CHARACTER*(*)( READ )
*         The integration file name (Iyymmdd_oooo_iiii or
*         IDIR:Iyymmdd_oooo_iiii)
*     ROFILE = CHARACTER*(*)( WRITE )
*         The reduced observation file name (RODIR:ROyymmdd_oooo)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Authors :
*     P.N.Daly     (JACH::PND)
*    History :
*     16-Dec-1994: Original Unix version.                          (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables:
      INCLUDE 'CRED4COM.INC'
*    Import :
      CHARACTER*(*) IFILE     ! Integration file name
*    Export:
      CHARACTER*(*) ROFILE    ! Reduced observation file name
*    Status :
      INTEGER STATUS
*    External :
      INTEGER CHR_LEN
*    Local variables :
      INTEGER
     :  SEPPOS,               ! Position of colon in character string
     :  USCPOS                ! Position of underscore in character string
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find last occurrence of file separator (0 or N)
      SEPPOS = CHR_LEN( IFILE )
      CALL CHR_FIND( IFILE, SEPARATOR, .FALSE., SEPPOS )

*    Find last occurrence of underscore
      USCPOS = CHR_LEN( IFILE )
      CALL CHR_FIND( IFILE, '_', .FALSE., USCPOS )

*    Set the output file
      CALL CHR_FILL( ' ', ROFILE )
      ROFILE = PREFIX // 'RODIR' // SEPARATOR // 'ro' // IFILE(SEPPOS+2:USCPOS-1)
*     ROFILE = RODIR(1:CHR_LEN(RODIR)) /
*    :  / 'ro' // IFILE(SEPPOS+2:USCPOS-1)
      CALL CHR_RMBLK( ROFILE )

*    Exit routine
      IF ( VERBOSE ) THEN
         CALL MSG_SETC( 'ROFILE', ROFILE )
         CALL MSG_OUT( ' ', 'CRED4_INTTOROBS: Reduced observation file is ^ROFILE', STATUS )
      END IF
      END
