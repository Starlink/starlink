      SUBROUTINE ECH_PROFILE_ATTRIB( PROFILE, ORDER_SIZE, SUBSTEPS,
     :           MODE, DEK_ABOVE, DEK_BELOW, MIN_INTEN, MAX_INTEN,
     :           Y_PEAK_INDEX )
*+
*  Name:
*     ECHOMOP - ECH_PROFILE_ATTRIB

*  Purpose:
*     Determine characteristics of profile: minimum intenstity,
*     maximum intensity and index of peak point.

*  Authors:
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     09-SEP-1996 (MJC):
*       Initial version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'

*  Local Constants:
      INTEGER PROF_HWID
      PARAMETER ( PROF_HWID = 1024 )

*  Arguments Given:
      REAL PROFILE( -PROF_HWID : PROF_HWID )
      INTEGER ORDER_SIZE
      INTEGER SUBSTEPS
      CHARACTER*1 MODE
      INTEGER DEK_ABOVE
      INTEGER DEK_BELOW

*  Arguments Returned:
      REAL MAX_INTEN
      REAL MIN_INTEN
      INTEGER Y_PEAK_INDEX

*  Local Variables:
      INTEGER ULIM
      INTEGER LLIM
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER I
      INTEGER II

      CHARACTER*8 REF_STR1
      CHARACTER*8 REF_STR2
*.

*  Find limits of profile.
      ULIM = ORDER_SIZE / 2 * SUBSTEPS
      LLIM = -ULIM
      IF ( MODE .NE. 'D' ) THEN
         IF ( DEK_BELOW .EQ. 0 .AND.
     :        DEK_ABOVE .EQ. 0 ) THEN
            CALL CHR_ITOC( LLIM, REF_STR1, NCHAR1 )
            CALL CHR_ITOC( ULIM, REF_STR2, NCHAR2 )
            REPORT_STRING = ' No dekker limits pre-set. ' //
     :            ' Using defaults of ' //
     :            REF_STR1( :NCHAR1 ) // ',' //
     :            REF_STR2( :NCHAR2 ) // ' pixels from trace.'
            CALL ECH_REPORT( 0, REPORT_STRING )

         ELSE
            LLIM = DEK_BELOW * SUBSTEPS
            ULIM = DEK_ABOVE * SUBSTEPS
         END IF
      END IF

*  Determine peak position and minimum value.
      MIN_INTEN =  1.0E20
      MAX_INTEN = -1.0E20
      DO I = LLIM, ULIM

*     If greater then previous maximum value.
         IF ( PROFILE( I ) .GT. MAX_INTEN ) THEN
            MAX_INTEN = PROFILE( I )
            II = 1

*        Check for a 'plateau' type peak and determine its size.
            DO WHILE ( ( I + II ) .LT. ULIM .AND.
     :                 ABS( PROFILE( I + II ) - MAX_INTEN ) .LT. 0.001 )
               II = II + 1
            END DO
            Y_PEAK_INDEX = I + ( II - 1 ) / 2
         END IF

*     If minimum value so far, record that.
         IF ( PROFILE( I ) .LT. MIN_INTEN ) THEN
            MIN_INTEN = PROFILE( I )
         END IF
      END DO

      END
