*+  AIO_PSRCH - Search for a file in a list of directories
      SUBROUTINE AIO_PSRCH( PATHVAR, NAME, FOUND, FULLNAME, STATUS )
*
*    Description :
*
*     Searches the list of directories in the list (whose elements are
*     separated by PATH_SEP_CH) held in the path variable specified by
*     PATHVAR for a file called NAME. If found FOUND is set TRUE,  and
*     FULLNAME to the full disk spec of the file found.
*
*    Method :
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_SYS_PAR'
*
*    Import :
*
      CHARACTER*(*)          	PATHVAR			! Path variable name
      CHARACTER*(*)          	NAME               	! Short name
*
*    Export :
*
      LOGICAL			FOUND			! Found the file
      CHARACTER*(*)		FULLNAME 		! Full file name
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER                CHR_LEN
*
*    Local variables :
*
      CHARACTER*132          PATH               ! Directory path

      INTEGER                CPOS               ! position of colon in PATH
      INTEGER                IC, D_S, D_E       ! Character pointers
      INTEGER                PLEN               ! Useful length of PATH
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      FOUND = .FALSE.

*    Translate path variable
      CALL PSX_GETENV( PATHVAR, PATH, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        PATH = ' '
      END IF

*    If path is blank,
      IF ( PATH(1:1) .EQ.' ' ) THEN

*      Just test simple file name
        INQUIRE( FILE=NAME, EXIST=FOUND, NAME=FULLNAME )

*    Path is non-blank
      ELSE

*      Search through directories
        IC = 1
        FOUND = .FALSE.
        PLEN = CHR_LEN(PATH)
        DO WHILE ( (IC.LE.PLEN) .AND. .NOT. FOUND )

*        Locate directory within path
          D_S = IC
          CPOS = INDEX(PATH(IC:),PATH_SEP_CH)
          IF ( CPOS .EQ. 0 ) THEN
            D_E = PLEN
          ELSE
            D_E = IC + CPOS - 2
          END IF

*        Try and open file with extension
          INQUIRE( FILE=PATH(D_S:D_E)//NAME, EXIST=FOUND,
     :             NAME=FULLNAME )

*        If not found advance pointer
          IF ( .NOT. FOUND ) IC = D_E + 2

        END DO

      END IF

      END
