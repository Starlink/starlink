*-----------------------------------------------------------------------
*+  IKNWLT - Write Video Look Up Table

      SUBROUTINE IKNWLT ( DISPID, LUTNUM, START, NENT, VLUT, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IILWLT.
*     The arguments are identical to those in IILWLT.
*
*    Invocation :
*     CALL IKNWLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )
*
*    Method :
*     Verify the input arguments and update the common block entry
*     for the LUT. Update the LUT on the specific devices.
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
*     December 1988
*     March 1990     Use assumed size array for VLUT
*     December 1990  Changed name from IILWLT
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

*     VLUT number
      INTEGER LUTNUM

*     Start position
      INTEGER START

*     Number of entries
      INTEGER NENT

*     Video look-up table
      REAL VLUT( 3, * )

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMLUT)'

*    Local variables :
      INTEGER I, J, K, LNUM, MAXPR, MEMID
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

*   Verify the LUT number
      IF ( ( LUTNUM .LT. -1 ) .OR. ( LUTNUM .GT. CNLUT - 1 ) ) THEN
         STATUS = IDI__INLUT
         GOTO 99
      ENDIF

*   Verify the start number
      IF ( ( START .LT. 0 ) .OR. ( START .GT. MAXCOL - 1 ) ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Verify the number of entries
      IF ( ( NENT .LT. 1 ) .OR. ( NENT + START .GT. MAXCOL ) ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Use a local varaible for the LUTNUM
*   If LUTNUM = -1 then use the LUT assigne to the memory with
*   the highest priority
      IF ( LUTNUM .EQ. -1 ) THEN
         MAXPR = CMEMPR( 0 )
         MEMID = 0
         DO I = 1, CNMEM - 1
            IF ( CMEMPR( I ) .GT. MAXPR ) THEN
               MAXPR = CMEMPR( I )
               MEMID = I
            ENDIF
         ENDDO
         LNUM = CLUTBI( MEMID )
      ELSE
         LNUM = LUTNUM
      ENDIF

*   Save the new colours in the common blocks
      IF ( LNUM .EQ. 0 ) THEN
         DO K = 1, NENT
            DO J = 1, 3
               CLUT0( J, K + START ) = VLUT( J, K )
            ENDDO
         ENDDO
      ELSEIF ( LNUM .EQ. 1 ) THEN
         DO K = 1, NENT
            DO J = 1, 3
               CLUT1( J, K + START ) = VLUT( J, K )
            ENDDO
         ENDDO
      ENDIF

*   Send the new LUT to a specific devce
      CALL IKNOLT( DISPID, LNUM, STATUS )

  99  CONTINUE

      END

