      SUBROUTINE IRM_STPHA( NLINE, LNLAB, LNLBPS, NPATTN, STATUS )
*+
*  Name:
*     IRM_STPHA

*  Purpose:
*     Set dashed-line pattern for NCAR routine EZMXY.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_STPHA( NLINE, LNLAB, LNLBPS, NPATTN, STATUS )

*  Description:
*     With the dashed-line pattern set by this routine, the NCAR routine
*     EZMXY (or EZMY ) will draw a multi-line graph in which each curve
*     is a solid line interrupted at a specified position by a specified
*     label.  26 different dashed-line patterns can be defined. The
*     EZMXY (or EZMY) will use the set patterns in a circular fashion.

*  Arguments:
*     NLINE = INTEGER
*        The number of lines to be plotted.
*     LNLAB( NLINE ) = CHARACTER*(*) (Given)
*        The in-line label for each dashed-line pattern.
*     LNLBPS( NLINE ) = REAL (Given)
*        The position of in-line label on each curve given in fraction
*        of the small dimension of the NCAR grid window from the
*        beginning of the curve.  A negative value or a value greater
*        than 1.0 means that the corresponding will be drawn with solid
*        line.
*     NPATTN = INTEGER (Given)
*        The number of different patterns. At most there can be 26 
*        different patterns. A value greater the 26 means that all line
*        in the plot will be solid. A zero or negative value means that 
*        EZMXY (or EZMY ) should use the "alphabetic" set of 26 dashed 
*        line patterns. For the value between 1 and 26, NCAR will draw
*        the NLINE lines with the pattern defined by LNLAB( NLINE ) and 
*        LNLBPS( NLINE ). If NLINE is greater than NPATTN, NCAR will
*        draw the lines using the first NPATTN patterns in a circular
*        fashion.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1991 (WG):
*        Original version.
*     1993 February 12 (MJC):
*        Tidied the prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NLINE
      CHARACTER*(*) LNLAB( NLINE )
      REAL LNLBPS( NLINE )
      INTEGER NPATTN

*  Status:
      INTEGER STATUS             ! Global status

*  External Reference:
      CHARACTER*16 AGDSHN        ! Dash-pattern-name generator
      INTEGER CHR_LEN            ! Get used length of a string
                                
*  Local Constants:
      INTEGER PATLN              ! Pattern character string length
      PARAMETER ( PATLN = 50 )

      INTEGER MXPAT              ! Max. number of different pattern
      PARAMETER ( MXPAT = 26 )

      REAL UNITLN                ! Length of solid line unit as a fraction 
      PARAMETER ( UNITLN = 0.1 ) ! of the smaller dimension of grid window.

      REAL CHRLN                 ! Length of character in-line label as a
      PARAMETER ( CHRLN = 0.02 ) ! fraction of the smaller dimension of 
                                 ! grid window.

      INTEGER MLABLN             ! Max in-line label length
      PARAMETER ( MLABLN = 10 )

*  Local Variables:
      CHARACTER*( PATLN ) DSHP( MXPAT )
                                 ! Pattern string of each pattern
      CHARACTER*( MLABLN ) LAB   ! In-line label of a curve 
      INTEGER I, J               ! Do loop index
      INTEGER LABLN              ! The used length of a in-line label
      INTEGER LBPOS              ! Begin position of the in-line label
      INTEGER DIFPTN             ! Number of different patterns
      INTEGER DSHPLN             ! Used length of a pattern string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If "alphabetic"-pattern is specified,
      IF ( NPATTN .LE. 0 ) THEN
         CALL AGSETI( 'DASH/SELECTOR.', -1 )

*  If user defined pattern is specified,
      ELSE IF ( NPATTN .GE. 1 .AND. NPATTN .LE. MXPAT ) THEN

*  Set the number of different patterns.
         DIFPTN = MIN( NPATTN, NLINE )
         CALL AGSETI( 'DASH/SELECTOR.', DIFPTN )


*  Set the length width of each character of the in-line label and the
*  length of each curve unit.
         CALL AGSETF( 'DASH/CHARACTER.', CHRLN )
         CALL AGSETF( 'DASH/DOLLAR-QUOTE.', UNITLN )

*  Set the length of pattern string.
         CALL AGSETI( 'DASH/LENGTH.', PATLN )      
    
*  Form the pattern string for each pattern.
         DO I = 1, DIFPTN

*  If this pattern is not a solid line, 
            IF ( LNLBPS( I ) .GE. 0.0 .AND. LNLBPS( I ) .LE. 1.0 ) THEN

*  Get the label for this pattern.
               LAB = LNLAB( I )

*  Remove the leading blank of the label string and get its used length.
               CALL CHR_LDBLK( LAB )     
               LABLN = CHR_LEN( LAB )
      
*  Find the position of the in-line label on the curve.
               LBPOS = INT( LNLBPS( I )/UNITLN )

*  If the in-line label is at the beginning of the curve,
               IF ( LBPOS .EQ. 0 ) THEN
                  DSHP( I ) = LAB( : LABLN )

*  Otherwise,construct the segment of the pattern from the beginning
*  to the in-line label position as solid line.
               ELSE
                  DSHP( I ) = '$'
                  DO J = 1, LBPOS-1
                     DSHP( I ) = DSHP( I )( : J )//'$'
                  END DO
      
*  Then append the in-line label to the pattern string.
                  DSHP( I ) = DSHP( I )( : LBPOS )//LAB( : LABLN )
               END IF

*  Set the curve after interruption as solid line.
               DSHPLN =CHR_LEN( DSHP( I ) )
               DO J = DSHPLN, PATLN-1
                  DSHP( I ) = DSHP( I )( : J )//'$'
               END DO

*  Set up this pattern
               CALL AGSETC( AGDSHN( I ), DSHP( I ) )

*  If this line is a solid line, ...
            ELSE
               CALL AGSETF( AGDSHN( I ), 65535. )
          
            END IF
         END DO

*  If all lines are required to draw solid, ...
      ELSE IF ( NPATTN .GT. MXPAT ) THEN
         CALL AGSETI( 'DASH/SELECTOR.', 1 )
         CALL AGSETF( 'DASH/PATTERNS/1.', 65535. )
      END IF

      END
