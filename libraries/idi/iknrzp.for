*-----------------------------------------------------------------------
*+  IKNRZP - Read Display Zoom and Pan

      SUBROUTINE IKNRZP ( DISPID, XOFF, YOFF, ZOOMF, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIZRZP.
*     The arguments are identical to those in IIZRZP.
*
*    Invocation :
*     CALL IKNRZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )
*
*    Method :
*     Verify the input arguments and read the information from the
*     common blocks. The pan factors are returned in a device
*     independent manner, as if the screen zooms about a position
*     coincident with the memory origin.
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     May 1990
*     December 1990  Changed name from IIZRZP
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

*    Local variables :
*     <declarations for local variables>
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

*   Read the scroll and zoom from the common block entries
*   Calculate the offsets from the centre of the unzoomed display
*   The Ikon scrolls all memories together so use memory 0.
      ZOOMF = CMEMZ( 0 )
      XOFF = CSCROX( 0 ) + ( CNPIX( 0 ) * ZOOMF ) /
     :       ( 2 * ( ZOOMF + 1 ) )
      YOFF = CSCROY( 0 ) - ( CNPIX( 1 ) * ZOOMF ) /
     :       ( 2 * ( ZOOMF + 1 ) )

  99  CONTINUE

      END
      
