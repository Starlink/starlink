*-----------------------------------------------------------------------
*+  IKNRSZ - Read Memory Scroll and Zoom

      SUBROUTINE IKNRSZ ( DISPID, MEMID, XOFF, YOFF, ZOOMF, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIZRSZ.
*     The arguments are identical to those in IIZRSZ.
*
*    Invocation :
*     CALL IKNRSZ( DISPID, MEMID, XOFF, YOFF, ZOOMF, STATUS )
*
*    Method :
*     Verify the input arguments and read the information from the
*     common blocks. The scroll factors are returned in a device
*     independent manner, as if the screen zooms about a position
*     coincident with the memory origin.
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     May 1989
*     December 1990  Changed name from IIZRSZ
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*     Memory identifier
      INTEGER MEMID

*    Export :
*     X offset
      INTEGER XOFF

*     Y offset
      INTEGER YOFF

*     Zoom factor
      INTEGER ZOOMF

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMPOS)'
*-

*   Recover the common blocks if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNUPD( DISPID, STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            STATUS = IDI__NOREC
            GOTO 99
         ENDIF
      ENDIF

*   Verify memory identifier
      IF ( ( MEMID .LT. 0 ) .OR. ( MEMID .GT. CNMEM ) ) THEN
         STATUS = IDI__INMID
         GOTO 99
      ENDIF

*   Read the scroll and zoom from the common block entries
      XOFF = CSCROX( MEMID )
      YOFF = CSCROY( MEMID )
      ZOOMF = CMEMZ( MEMID )

  99  CONTINUE

      END

