*-----------------------------------------------------------------------
*+  IDLMAC - Match colour to entry in colour table

      SUBROUTINE IDLMAC ( MEMID, INCOL, OUTCOL, STATUS )

*    Description :
*     This looks through the colour tables and finds an entry that best
*     fits the colours required by the routines Polyline, Plot Text etc.
*
*    Invocation :
*     CALL IDLMAC( MEMID, INCOL, OUTCOL, STATUS )
*
*    Method :
*     Search through the LUTs looking for the colour that best fits the
*     required colour. The best fit is calculated in terms of a
*     Manhatten metric rather than a least squares to save effort.
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
*     February 1991
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Memory identifier
      INTEGER MEMID

*     Required colour
      INTEGER INCOL

*    Export :
*     Colour matching required colour
      INTEGER OUTCOL

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMLUT)'

*    Local variables :
      INTEGER I, J, LUTNUM, MAXPR

      REAL BEST, FIT

      REAL CDEF( 3, 8 )
      DATA CDEF / 0.0, 0.0, 0.0,
     :            1.0, 1.0, 1.0,
     :            1.0, 0.0, 0.0,
     :            0.0, 1.0, 0.0,
     :            0.0, 0.0, 1.0,
     :            1.0, 1.0, 0.0,
     :            1.0, 0.0, 1.0,
     :            0.0, 1.0, 1.0 /
*-

*   Check the value of incol is valid
      OUTCOL = INCOL
      IF ( ( INCOL .LT. 1 ) .OR. ( INCOL .GT. 8 ) ) THEN
         GOTO 99
      ENDIF

*   Find out which LUT is in use
      IF ( ( MEMID .GE. 0 ) .AND. ( MEMID .LE. CNMEM - 1 ) ) THEN
         LUTNUM = CLUTBI( MEMID )

*   Otherwise use the memory with the highest priority
      ELSE
         MAXPR = CMEMPR( 0 )
         LUTNUM = CLUTBI( 0 )
         DO I = 1, CNMEM - 1
            IF ( CMEMPR( I ) .GT. MAXPR ) THEN
               MAXPR = CMEMPR( I )
               LUTNUM = CLUTBI( I )
            ENDIF
         ENDDO
      ENDIF

*   Locate the closest colour in LUT 0
      BEST = 99.9
      IF ( LUTNUM .EQ. 0 ) THEN
         DO J = 1, MAXCOL
            FIT = ABS( CDEF( 1, INCOL ) - CLUT0( 1, J ) ) +
     :            ABS( CDEF( 2, INCOL ) - CLUT0( 2, J ) ) +
     :            ABS( CDEF( 3, INCOL ) - CLUT0( 3, J ) )
            IF ( FIT .LT. BEST ) THEN
               BEST = FIT
               OUTCOL = J
            ENDIF
         ENDDO

*   The actual colour indices go from 0 to MAXCOL - 1
         OUTCOL = OUTCOL - 1

*   Locate the closest colour in LUT 0
      ELSE
         DO J = 1, MAXCOL
            FIT = ABS( CDEF( 1, INCOL ) - CLUT1( 1, J ) ) +
     :            ABS( CDEF( 2, INCOL ) - CLUT1( 2, J ) ) +
     :            ABS( CDEF( 3, INCOL ) - CLUT1( 3, J ) )
            IF ( FIT .LT. BEST ) THEN
               BEST = FIT
               OUTCOL = J
            ENDIF
         ENDDO

*   The actual colour indices go from 0 to MAXCOL - 1
         OUTCOL = OUTCOL - 1
      ENDIF

  99  CONTINUE

      END

