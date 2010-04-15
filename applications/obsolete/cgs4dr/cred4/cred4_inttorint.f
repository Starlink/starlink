*+  CRED4_INTTORINT - Convert integration file name to reduced integration
      SUBROUTINE CRED4_INTTORINT( IFILE, RIFILE, STATUS )
*    Description :
*     Converts Iyymmdd_oooo_iiii -> IDIR:Iyymmdd_oooo_iiii for VMS
*     Converts Iyymmdd_oooo_iiii -> $IDIR/Iyymmdd_oooo_iiii for Unix
*    Invocation :
*     CALL CRED4_INTTORINT( IFILE, RIFILE, STATUS )
*    Parameters :
*     IFILE  = CHARACTER*(*)( READ )
*         The observation file name (Oyymmdd_oooo or ODIR:Oyymmdd_oooo)
*     RIFILE = CHARACTER*(*)( WRITE )
*         The reduced observation file name (RODIR:ROyymmdd_oooo)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Authors :
*     P.N.Daly  (JACH::PND)
*    History :
*      1-Aug-1994: Original version.                       (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables:
      INCLUDE 'CRED4COM.INC'
*    Import :
      CHARACTER*(*) IFILE         ! Integration file name
*    Export:
      CHARACTER*(*) RIFILE        ! Reduced integration file name
*    Status :
      INTEGER STATUS              ! Inherited ADAM status
*    External references :
      INTEGER CHR_LEN
*    Local variables :
      INTEGER SEPPOS              ! Position of separator
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find last occurrence of file separator (0 or N)
      SEPPOS = CHR_LEN( IFILE )
      CALL CHR_FIND( IFILE, SEPARATOR, .FALSE., SEPPOS )

*    Set the output file
      CALL CHR_FILL( ' ', RIFILE )
      RIFILE = PREFIX // 'RIDIR' // SEPARATOR // 'r' // IFILE(SEPPOS+1:CHR_LEN(IFILE))
*     RIFILE = RIDIR(1:CHR_LEN(RIDIR)) /
*    :  / 'r' // IFILE(SEPPOS+1:CHR_LEN(IFILE))
      CALL CHR_RMBLK( RIFILE )

*    Exit routine
      IF ( VERBOSE ) THEN
         CALL MSG_SETC( 'RIFILE', RIFILE )
         CALL MSG_OUT( ' ', 'CRED4_INTTORINT: Reduced integration file is ^RIFILE', STATUS )
      END IF
      END
