*-----------------------------------------------------------------------
*+  IKNRRI - Read Rectangular Region of Interest

      SUBROUTINE IKNRRI ( DISPID, INMID, ROIID, XMIN, YMIN, XMAX, YMAX,
     :                    OUTMID, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIRRRI.
*     The arguments are identical to those in IIRRRI.
*
*    Invocation :
*     CALL IKNRRI ( DISPID, INMID, ROIID, XMIN, YMIN, XMAX, YMAX,
*    :              OUTMID, STATUS )
*
*    Method :
*     Check the input arguments, then calculate the ROI position
*     from entries in the common blocks.
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
*     March 1990
*     December 1990  Changed name from IIRRRI
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

*     Input memory identifier
      INTEGER INMID

*     ROI identifier
      INTEGER ROIID

*    Export :
*     Minimum x position
      INTEGER XMIN

*     Minimum y position
      INTEGER YMIN

*     Maximum x position
      INTEGER XMAX

*     Maximum y position
      INTEGER YMAX

*     Output memory identifier
      INTEGER OUTMID

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMID)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      INTEGER J, MAXPR
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

*   Check the ROI identifier
      IF ( ( ROIID .LT. 0 ) .OR. ( ROIID .GT. CNROI - 1 ) ) THEN
         STATUS = IDI__INRID
         GOTO 99
      ENDIF
      IF ( CROIID( ROIID ) .LT. 0 ) THEN
         STATUS = IDI__INRID
         GOTO 99
      ENDIF

*   If INMID = -1 then the position is relative to the screen origin
      IF ( INMID .EQ. -1 ) THEN
         XMIN = CROIXL( ROIID ) + CSCROX( 0 ) - CMEMX( 0 )
         YMIN = CROIYL( ROIID ) + CSCROY( 0 ) - CMEMY( 0 ) -
     :          CNPIX( 1 ) * CMEMZ( 0 ) / ( CMEMZ( 0 ) + 1 )
         XMAX = CROIXH( ROIID ) + CSCROX( 0 ) - CMEMX( 0 )
         YMAX = CROIYH( ROIID ) + CSCROY( 0 ) - CMEMY( 0 ) -
     :          CNPIX( 1 ) * CMEMZ( 0 ) / ( CMEMZ( 0 ) + 1 )

*   Output memory to which cursor currently points
*   The Ikon memories fill the screen and wrap around, so just find
*   the one with the highest priority
         OUTMID = 0
         MAXPR = CMEMPR( 0 )
         DO J = 1, CNMEM - 1
            IF ( CMEMPR( J ) .GT. MAXPR ) THEN
               MAXPR = CMEMPR( J )
               OUTMID = J
            ENDIF
         ENDDO

*   Otherwise it is relative to the memory origin
      ELSEIF ( ( INMID .GE. 0 ) .AND. ( INMID .LT. CNMEM ) ) THEN
         XMIN = CROIXL( ROIID ) - CMEMX( INMID )
         YMIN = CROIYL( ROIID ) - CMEMY( INMID )
         XMAX = CROIXH( ROIID ) - CMEMX( INMID )
         YMAX = CROIYH( ROIID ) - CMEMY( INMID )
         OUTMID = INMID

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INMID
      ENDIF

  99  CONTINUE

      END

