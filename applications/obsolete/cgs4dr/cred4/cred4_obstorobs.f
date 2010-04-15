*+  CRED4_OBSTOROBS - Convert observation file name to reduced observation
      SUBROUTINE CRED4_OBSTOROBS( OFILE, ROFILE, STATUS )
*    Description :
*     Oyymmdd_oooo -> RODIR:ROyymmdd_oooo for VMS
*     Oyymmdd_oooo -> $RODIR/ROyymmdd_oooo for Unix
*    Invocation :
*     CALL CRED4_OBSTOROBS( OFILE, ROFILE, STATUS )
*    Parameters :
*     OFILE  = CHARACTER*(*)( READ )
*         The observation file name (Oyymmdd_oooo or ODIR:Oyymmdd_oooo)
*     ROFILE = CHARACTER*(*)( WRITE )
*         The reduced observation file name (RODIR:ROyymmdd_oooo)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Authors :
*     P.N.Daly  (JACH::PND)
*    History :
*     16-Dec-1994: Original Unix version.                  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables:
      INCLUDE 'CRED4COM.INC'
*    Import :
      CHARACTER*(*) OFILE            ! observation file name
*    Export:
      CHARACTER*(*) ROFILE           ! Reduced observation file name
*    Status :
      INTEGER STATUS
*    External :
      INTEGER CHR_LEN
*    Local variables :
      INTEGER SEPPOS                 ! Position of separator
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find last occurrence of file separator (0 or N)
      SEPPOS = CHR_LEN( OFILE )
      CALL CHR_FIND( OFILE, SEPARATOR, .FALSE., SEPPOS )

*    Set the output file
      CALL CHR_FILL( ' ', ROFILE )
      ROFILE = PREFIX // 'RODIR' // SEPARATOR // 'r' // OFILE(SEPPOS+1:CHR_LEN(OFILE))
*     ROFILE = RODIR(1:CHR_LEN(RODIR)) /
*    :  / 'r' // OFILE(SEPPOS+1:CHR_LEN(OFILE))
      CALL CHR_RMBLK( ROFILE )

*    Exit routine
      IF ( VERBOSE ) THEN
         CALL MSG_SETC( 'ROFILE', ROFILE )
         CALL MSG_OUT( ' ', 'CRED4_OBSTOROBS: Reduced observation file is ^ROFILE', STATUS )
      END IF

      END


