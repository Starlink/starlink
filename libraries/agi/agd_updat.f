************************************************************************
*+  AGD_UPDAT - Update the IDI zoom and scroll factors for the device

      SUBROUTINE AGD_UPDAT ( STATUS )

*    Description :
*     This updates the IDI zoom and scroll factors for the current
*     device. These are held in common blocks and not written out until
*     the end of the session, unless the device has changed.
*
*    Invocation :
*     CALL AGD_UPDAT( STATUS )
*
*    Method :
*     Check status on entry.
*     Check IDI has been activated.
*     Verify the IDI display identifier of the current picture.
*     If this display identifier does not match the one in the common
*     block then
*        Save the old zoom and scroll factors in the database.
*     Endif
*     Verify the memories on this device.
*     Read the zoom and scroll factors into the common blocks.
*     Update the display identifier in the common blocks.
*
*    Deficiencies :
*     <description of any deficiencies>
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     June 1990
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'agi_idips'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_pfree'

*    Local variables :
      CHARACTER STRING * 64

      INTEGER DISPID, ISTAT, J, JMEM, LENSTR, MEMS( MXMEMS )
*-

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Check that IDI has started
      IF ( .NOT. CIDION ) GOTO 99

*   Copy the display identifier of the current picture
      DISPID = CIDIID( CURPID )

*   Verify the display identifier
      IF ( DISPID .LE. 0 ) GOTO 99

*   If the display identifiers are different then put the currently
*   saved parameters into the database
      IF ( DISPID .NE. CDIPID ) THEN
         CALL AGD_1SIDIP( STATUS )
      ENDIF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Get the number of memories in current configuration
      CALL IIDQCI( DISPID, 21, MXMEMS, MEMS, CNMEMS, ISTAT )
      IF ( ISTAT .NE. 0 ) THEN
         CALL IIDERR( ISTAT, STRING, LENSTR )
         STATUS = AGI__IDERR
         CALL ERR_REP( 'AGD_UPDAT_IDQCI', STRING( :LENSTR ), STATUS )
         GOTO 99
      ENDIF

*   Check the number of memories
      IF ( CNMEMS .GT. MXMEMS ) THEN
         STATUS = AGI__MEMIN
         CALL ERR_REP( 'AGD_UPDAT_MEMIN', 'IDI memory invalid', STATUS )
         GOTO 99
      ENDIF

*   Get the current zoom and scroll factors
      DO 10 J = 1, CNMEMS
         JMEM = MEMS( J )
         CALL IIZRSZ( DISPID, JMEM, CXSCRL( JMEM ), CYSCRL( JMEM ),
     :                CZOOMF( JMEM ), ISTAT )
         IF ( ISTAT .NE. 0 ) THEN
            CALL IIDERR( ISTAT, STRING, LENSTR )
            STATUS = AGI__IDERR
            CALL ERR_REP( 'AGD_UPDAT_IZRSZ', STRING( :LENSTR ), STATUS )
            GOTO 99
         ENDIF
  10  CONTINUE

*   Copy the display identifier into the common block
      CDIPID = DISPID

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGD_UPDAT +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

