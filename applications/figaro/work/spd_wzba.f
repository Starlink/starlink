      SUBROUTINE SPD_WZBA( BADVAL, IMIN, IMAX, OMIN, OMAX,
     :   NELM, IDATA, ODATA, STATUS )
*+
*  Name:
*     SPD_WZBA

*  Purpose:
*     Prepare REAL array for PGPIXL.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZBA( BADVAL, IMIN, IMAX, OMIN, OMAX,
*        NELM, IDATA, ODATA, STATUS )

*  Description:
*     This routine will convert the value range [IMIN,IMAX] of the REAL
*     input data to the value range [OMIN,OMAX] of the INTEGER output
*     data. The transform is linear. Input values beyond the thresholds
*     are assigned the appropriate output extremal value. Ten input is
*     scanned for VAL__BADR and those values are turned into BADVAL in
*     the output.
*
*     The purpose of this routine is to prepare a REAL data array for
*     display with PGPIXL. Bad values should normally be converted to
*     background colour (0), foreground colour (1), or some standard pen
*     colour (2...7). Non-bad values should be scaled to the pen range
*     16 to maximum available. This routine does the scaling and
*     bad-value filtering.

*  Arguments:
*     BADVAL = INTEGER (Given)
*        The output value to replace bad input values.
*     IMIN = REAL (Given)
*        The lower threshold for input values.
*     IMAX = REAL (Given)
*        The upper threshold for input values. IMAX must not equal IMIN.
*     OMIN = INTEGER (Given)
*        The output value to correspond to input IMIN. OMIN must be
*        positive.
*     OMAX = INTEGER (Given)
*        The output value to correspond to input IMAX. OMAX must be
*        positive.
*     NELM = INTEGER (Given)
*        The size of the input and output data arrays.
*     IDATA( NLEM ) = REAL (Given)
*        The input data array.
*     ODATA( NELM ) = INTEGER (Returned)
*        The output data array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     18 May 1994 (hme):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      INTEGER BADVAL
      REAL    IMIN, IMAX
      INTEGER OMIN, OMAX
      INTEGER NELM
      REAL    IDATA( NELM )

*  Arguments Returned:
      INTEGER ODATA( NELM )

*  Status argument:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop indices
      REAL TEMP                  ! Current input value
      REAL DELTA, OFFSET         ! Scaling constants

*.

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check extrema.
      IF ( IMIN .EQ. IMAX ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WZBA_E01', 'SPD_WZBA: Error scaling ' //
     :      'array for display: input range is empty.', STATUS )
         GO TO 500
      ELSE IF ( OMIN .LE. 0 .OR. OMAX .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WZBA_E02', 'SPD_WZBA: Error scaling ' //
     :      'array for display: output range is not positive.', STATUS )
         GO TO 500
      END IF

*  Scaling constants.
*  Since output is integer, the range is one longer than you believe, it
*  goes from OMIN-0.5 to just below OMAX+0.5.
      DELTA  = FLOAT( OMAX - OMIN + 1 ) / ( IMAX - IMIN )
      OFFSET = FLOAT(OMIN) - 0.5

*  Loop through arrays.
*  Since the output is definitely positive, we can use INT(X+0.5)
*  instead of NINT(X).
      DO 1 I = 1, NELM
         TEMP = IDATA(I)
         IF ( TEMP .GE. IMIN .AND. TEMP .LT. IMAX ) THEN
            ODATA(I) = INT( OFFSET + ( TEMP - IMIN ) * DELTA + 0.5 )
         ELSE IF ( TEMP .EQ. VAL__BADR ) THEN
            ODATA(I) = BADVAL
         ELSE IF ( TEMP .LT. IMIN ) THEN
            ODATA(I) = OMIN
         ELSE IF ( TEMP .GE. IMAX ) THEN
            ODATA(I) = OMAX
         END IF
 1    CONTINUE

*  Return.
 500  CONTINUE
      END
