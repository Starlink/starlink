*+  DISP_FILENAM - Displays filename on terminal for user info
      SUBROUTINE DISP_FILENAM(XLOC,STRNG,STATUS)
*    Description :
*     Performs HDS_TRACE on object with locator XLOC and writes results
*     to terminal. STRNG is a descriptive string to preface the filename,
*     it is used to indicate the nature of the object (eg input or
*     output, value_object, range_object etc).
*    Method :
*     Uses HDS_TRACE
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*    History :
*      4 Jul 88: Original, adapted from UTIL_SHOW (TJP)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      CHARACTER*(DAT__SZLOC) XLOC      ! Locator to container file
      CHARACTER*(*)           STRNG    ! Nature of object (eg Input, Output)
*    Import-Export :
*    Export :
*    Status :
      INTEGER                STATUS    ! Status variable
*    Local constants :
*    Local variables :
      CHARACTER*200          FILE      ! Fine name of container file
      CHARACTER*200          PATH      ! Path name of object within
                                       ! the container file.
      INTEGER                LEVELS    ! # levels in input
*    Internal References :
*    Local data :
*-

* Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

* Display details of file to user

*   First get container file...
      CALL HDS_TRACE( XLOC, LEVELS, PATH, FILE, STATUS )
      CALL MSG_SETC('STRNG',STRNG)
      CALL MSG_SETC('FILE',FILE)
      CALL MSG_PRNT( '^STRNG file :- ^FILE' )

*   Next get sub-levels (if any)
      IF ( LEVELS .GT. 1 ) THEN
        CALL MSG_SETC('PATH',PATH)
        CALL MSG_PRNT( '  Object:- ^PATH ' )
      END IF

* Report error
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( 'EXERR', 'from DISP_FILENAM', STATUS )
      END IF

      END
