*+  USI_SHOW - Displays file and path using format string given
      SUBROUTINE USI_SHOW( INTEXT, STATUS )
*    Description :
*
*     Takes a string containing a application parameter name in curly brackets
*     and outputs the file and path information about it.
*
*    History :
*
*     13 Dec 88 : Original ( dja @ uk.ac.bham.sr.star )
*
*    Type definitions :
      IMPLICIT NONE
*
*    Global variables :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER                  STATUS
*
*    Import :
*
      CHARACTER*(*)            INTEXT           ! Message string
*
*    Functions :
*
      INTEGER                  CHR_LEN
*
*    Local constants :
*
      INTEGER                  MAXLINES
         PARAMETER             (MAXLINES=6)
*
*    Local variables :

      CHARACTER*132            WORK(MAXLINES)   ! Work text

      INTEGER                  I                ! Loop counter
      INTEGER                  NOUT             ! Lines to print
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      WORK(1) = INTEXT
      NOUT = MAXLINES
      CALL USI_TEXT( 1, WORK, NOUT, STATUS )
      DO I=1,NOUT
         CALL MSG_PRNT( WORK(I)(:CHR_LEN(WORK(I))) )
      END DO
      END
