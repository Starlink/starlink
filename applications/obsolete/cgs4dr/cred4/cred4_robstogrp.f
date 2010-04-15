*+  CRED4_ROBSTOGRP - Convert reduced observation name to reduced group
      SUBROUTINE CRED4_ROBSTOGRP( ROFILE, GRPNUM, RGFILE, STATUS )
*    Description :
*     ROyymmdd_oooo -> RGDIR:RGyymmdd_gggg for VMS
*     ROyymmdd_oooo -> $RGDIR/RGyymmdd_gggg for Unix
*    Invocation :
*     CALL CRED4_ROBSTOGRP( ROFILE, GRPNUM, RGFILE, STATUS )
*    Parameters :
*     ROFILE = CHARACTER*(*)( READ )
*         The reduced observation file name (ROyymmdd_oooo or
*         RODIR:ROyymmdd_oooo).
*     RGFILE  = CHARACTER*(*)( WRITE )
*         The reduced group file name (RGDIR:RGyymmdd_gggg)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Authors :
*     P.N.Daly (JACH::PND)
*    History :
*     16-Dec-1994: Original Unix version.                       (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables:
      INCLUDE 'CRED4COM.INC'
*    Import :
      CHARACTER*(*) ROFILE    ! Reduced observation file name
      INTEGER GRPNUM          ! Group number
*    Export:
      CHARACTER*(*) RGFILE    ! Reduced group file name
*    Status :
      INTEGER STATUS
*    External :
      INTEGER CHR_LEN
*    Local variables :
      INTEGER
     :  SEPPOS,               ! Position of separator in character string.
     :  USCPOS                ! Position of underscore in character string
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find last occurrence of file separator (0 or N)
      SEPPOS = CHR_LEN( ROFILE )
      CALL CHR_FIND( ROFILE, SEPARATOR, .FALSE., SEPPOS )

*    Find last occurrence of underscore
      USCPOS = CHR_LEN( ROFILE )
      CALL CHR_FIND( ROFILE, '_', .FALSE., USCPOS )

*    Set the output file
      CALL CHR_FILL( ' ', RGFILE )
      RGFILE = PREFIX // 'RGDIR' // SEPARATOR // 'rg' // ROFILE(SEPPOS+3:USCPOS)
*     RGFILE = RGDIR(1:CHR_LEN(RGDIR)) /
*    :  / 'rg' // ROFILE(SEPPOS+2:USCPOS)
      CALL CHR_RMBLK( RGFILE )
      SEPPOS = CHR_LEN( RGFILE )
      CALL CHR_PUTI( GRPNUM, RGFILE, SEPPOS )

*    Exit routine
      IF ( VERBOSE ) THEN
         CALL MSG_SETC( 'RGFILE', RGFILE )
         CALL MSG_OUT( ' ', 'CRED4_ROBSTOGRP: Reduced group file is ^RGFILE', STATUS )
      END IF
      END
