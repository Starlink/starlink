      REAL FUNCTION ARC_ARFIND( ARCN, NLINES, VALUE )
*+
*  Name:
*     FUNCTION ARC_ARFIND

*  Purpose:
*     Locate nearest arc line to given wavelength.

*  Description:
*     Given a wavelength, locates the nearest arc line to
*     that wavelength in a table.

*  Arguments:
*     ARCN = REAL (Given)
*        Holds the table of arc wavelengths.
*        These should be in ascending order and end with a zero
*        or with the end of the array.
*     NLINES = INTEGER (Given)
*        The dimension of ARCN.
*     VALUE = REAL (Given)
*        The given wavelength.

*  Returns:
*     ARFIND = REAL
*        The nearest tabulated wavelength.

*  Authors:
*     KS: Keith Shortridge (CIT).
*     MJC: Martin Clayton (Starlink, UCL).

*  History:
*     13-JAN-1983 (KS):
*       Original version.
*     05-JUN-1997 (MJC):
*       Rewrite to use window search for speed up.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER NLINES
      REAL ARCN( NLINES )

*  Arguments Returned:
      REAL VALUE

*  Local Variables:
      INTEGER LOLIM
      INTEGER HILIM
      INTEGER NEWLIM
      INTEGER NLARCS
*.

*  Find longest wavelength in table.
      NLARCS = NLINES
      DO WHILE ( ARCN( NLARCS ) .EQ. 0.0 )
         NLARCS = NLARCS - 1
      END DO

*  Check for a wavelength outside the range of the list.
      IF ( VALUE .LE. ARCN( 1 ) ) THEN
         ARC_ARFIND = ARCN( 1 )

      ELSE IF ( VALUE .GE. ARCN( NLARCS ) ) THEN
         ARC_ARFIND = ARCN( NLARCS )

*  Check for a wavelength within the range of the list.
      ELSE
         LOLIM = 1
         HILIM = NLARCS
         DO WHILE ( HILIM - LOLIM .GT. 1 )
            NEWLIM = ( HILIM + LOLIM ) / 2
            IF ( VALUE .GT. ARCN( NEWLIM ) ) THEN
               LOLIM = NEWLIM

            ELSE IF ( VALUE .LT. ARCN( NEWLIM ) ) THEN
               HILIM = NEWLIM

            ELSE
               ARC_ARFIND = ARCN( NEWLIM )
               GO TO 999
            END IF
         END DO
         IF ( ARCN( HILIM ) - VALUE .GT. VALUE - ARCN( LOLIM ) ) THEN
            ARC_ARFIND = ARCN( LOLIM )

         ELSE
            ARC_ARFIND = ARCN( HILIM )
         END IF
      END IF

  999 CONTINUE

      END
