*-----------------------------------------------------------------------
*+  IKNQCR - Query Capabilities Real

      SUBROUTINE IKNQCR ( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIDQCR.
*     The arguments are identical to those in IIDQCR.
*
*    Invocation :
*     CALL IKNQCR( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )
*
*    Method :
*     Verify the input arguments and obtain the requested capability
*     from the common blocks.
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
*     November 1988
*     December 1990  Changed name from IIDQCR
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

*     Capability
      INTEGER CAPID

*     Size of output array
      INTEGER NARR

*    Export :
*     Output array
      REAL OUTARR( NARR )

*     Number of values returned
      INTEGER NOUT

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'

*    Local variables :
      INTEGER J
*-

*   Recover the characterisitics if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNUPD( DISPID, STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            GOTO 99
         ENDIF
      ENDIF

*   At present there is only one real capability
*   Blink period for each memory ( sec )
      IF ( CAPID .EQ. 71 ) THEN
         NOUT = MIN( CNMEM, NARR )
         DO J = 0, NOUT - 1
            OUTARR( J + 1 ) = CBLINK( J )
         ENDDO

*   No capability of that number
      ELSE
         STATUS = IDI__NOCAP
      ENDIF

  99  CONTINUE

      END

