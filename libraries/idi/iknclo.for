*-----------------------------------------------------------------------
*+  IKNCLO - Close Ikon

      SUBROUTINE IKNCLO ( DISPID, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIDCLO.
*     The arguments are identical to those in IIDCLO.
*
*    Invocation :
*     CALL IKNCLO( DISPID, STATUS )
*
*    Method :
*     Disable the Ikon mouse ( GID ), and purge the buffer for this
*     device. Deassign the I/O channel, and remove this device from
*     the common blocks.
*
*    Deficiencies :
*     Very non-standard Fortran - INTEGER * 2
*     Uses VAX system call - SYS$DASSGN
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     November 1988
*     November 1989  Added clear flag
*     May 1990       Added call to IKNSTI and reset ROI identifiers
*     January 1991   Remove DTYPE
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE '($SSDEF)'
      INCLUDE '($SYSSRVNAM)'
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMID)'
      INCLUDE 'IDIINC(IKN_COMBUF)'

*    Local variables :
      INTEGER * 2 WORDS( 3 )

      INTEGER ISTAT, J, NUMWOR
*-

*   Recover the common blocks if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNOUT( STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            STATUS = IDI__NOREC
            GOTO 99
         ENDIF
      ENDIF

*   Stop all interactions using a local status
      CALL IKNSTI( DISPID, ISTAT )

*   Reset the ROI identifiers
      DO 10 J = 0, MAXROI - 1
         CROIID( J ) = -1
  10  CONTINUE

*   Disable the GID and purge the buffer for this device
*   Ikon command 96 = '60'X = Set Register - 8 bit
*   Ikon address 24 = '18'X = Graphic Input Device Mode
      WORDS( 1 ) = 96
      WORDS( 2 ) = 24
      WORDS( 3 ) = 0
      NUMWOR = 3
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )

*   Save the current context unless the clear flag has been set
      IF ( CLRFG .NE. 1 ) THEN
         CALL IDSACO( STATUS )
      ENDIF

*   Deassign the channel
      ISTAT = SYS$DASSGN( %VAL( ACHAN( DISPID ) ) )
      IF ( ISTAT .NE. SS$_NORMAL ) THEN
         STATUS = IDI__IOERR
      ENDIF

*   Clear the channel number and device type from the common block
      ACHAN( DISPID ) = 0
      ONOFF( DISPID ) = 0

  99  CONTINUE

      END

