*+  AST_PATH - resolve path to file
      SUBROUTINE AST_PATH(DEF,ALT,REL,PATH,L,STATUS)
*    Description :
*  Creates a path name from an environment variable and relative path.
*  It allows two possibilities for the environment variable - a default
*  and an alternative.  The relative path may be blank.  There is also
*  an overall default of AST_ETC
*    History :
*     27/2/98 : Original (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      CHARACTER*(*) DEF,ALT,REL
*    Export :
      CHARACTER*(*) PATH
      INTEGER L
     Functions :
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      CHARACTER*132 BUFF
*-
      IF (STATUS.NE.SAI__OK) THEN

        CALL PSX_GETENV(ALT,BUFF,STATUS)

      ENDIF

      END
