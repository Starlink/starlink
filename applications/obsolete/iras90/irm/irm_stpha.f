      SUBROUTINE IRM_STPHA( LNLAB, NPATTN, LNLBPS, STATUS )
*+
*  Name:
*     IRM_STPHA

*  Purpose:
*     Set dashed-line pattern for NCAR routine EZMXY.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_STPHA( LNLAB, NPATTN, LNLBPS, STATUS )

*  Description:
*     With the dashed-line pattern set by this routine, the NCAR routine
*     EZMXY (or EZMY ) will draw a multi-line graph in which each curve
*     is a solid line interrupted at a specified position by a specified
*     label.  26 different dashed-line patterns can be defined. The
*     EXMAY (or EZMY ) will use the set patterns in a circular fashion.

*  Arguments:
*     LNLAB( NPATTN ) = CHARACTER*(*) (Given)
*        The in-line label for each dashed-line pattern.
*     NPATTN = INTEGER (Given)
*        The number of different pattern. A value greater than 26
*        means that EZMXY (or EZMY ) should use the "alphabetic" set of
*        26 dashed line patterns.
*     LNLBPS( NPATTN ) = REAL (Given)
*        The position of in-line label on each curve given in fraction
*        of the small dimension of the NCAR grid window from the
*        beginning of the curve.
*        A negative value or a value greater than 1.0 means that the
*        corresponding will be drawn with solid line.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NPATTN
      CHARACTER*(*) LNLAB( NPATTN )
      REAL LNLBPS( NPATTN )

*  Status:
      INTEGER STATUS             ! Global status

*  External Reference:
      CHARACTER*16 AGDSHN        ! Dash-pattern-name generator
      INTEGER CHR_LEN            ! Get used length of a string

*  Local Constants:
      INTEGER PATLN              ! Pattern character string length
      PARAMETER ( PATLN = 80 )

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
      CHARACTER*( MLABLN) LAB    ! In-line of a curve
      INTEGER IAT                ! Position of last non-blank character.
      INTEGER I, J               ! Do loop index
      INTEGER LABLN              ! The used length of a in-line label
      INTEGER LBPOS              ! Begin position of the in-line label
      INTEGER DSHPLN             ! Used length of a pattern string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If "alphabetic"-pattern is specified,
      IF ( NPATTN .GT. MXPAT ) THEN
         CALL AGSETI( 'DASH/SELECTOR.', -1 )

*  If user defined pattern is specified,
      ELSE IF ( NPATTN .GE. 1 .AND. NPATTN .LE. MXPAT ) THEN

*  Set the number of different patterns.
         CALL AGSETI( 'DASH/SELECTOR.', NPATTN )

*  Set the length width of each character of the in-line label and the
*  length of each curve unit.
         CALL AGSETF( 'DASH/CHARACTER.', CHRLN )
         CALL AGSETF( 'DASH/DOLLAR-QUOTE.', UNITLN )

*  Set the length of pattern string.
         CALL AGSETI( 'DASH/LENGTH.', PATLN )

*  Form the pattern string for each pattern.
         DO I = 1, NPATTN

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
                  CALL CHR_FILL( '$', DSHP( I )( : LBPOS ) )

*  Then append the in-line label to the pattern string.
                  IAT = LBPOS
                  CALL CHR_APPND( LAB( : LABLN ), DSHP( I ), IAT )
               END IF

*  Set the curve after interruption as solid line.
               CALL CHR_FILL( '$', DSHP( I )( IAT + 1 : ) )

*  If this pattern is a solid line, set it full of dollars.
            ELSE
               CALL CHR_FILL( '$', DSHP( I ) )

            END IF

*  Set up this pattern
            CALL AGSETC( AGDSHN( I ), DSHP( I ) )

         END DO

      END IF

      END
