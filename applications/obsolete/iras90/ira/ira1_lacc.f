      SUBROUTINE IRA1_LACC( SCS, GAPLAT, GAPLON, ACCLAT, ACCLON,
     :                      STATUS )
*+
*  Name:
*     IRA1_LACC

*  Purpose:
*     Find accuracies with which to display longitude and latitude
*     values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_LACC( SCS, GAPLAT, GAPLON, ACCLAT, ACCLON, STATUS )

*  Description:
*     The position of the least significant non-zero digit which would
*     result if the gap values were formated in the same way as the
*     actual coordinate values is found. The radian value equivalent to
*     a value of 1 in this least significant digit is returned as the
*     accuracy.

*  Arguments:
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system. An abbreviation will do.
*     GAPLAT = DOUBLE PRECISION (Given)
*        The latitude gap between parallels, in radians.
*     GAPLON = DOUBLE PRECISION (Given)
*        The longitude gap between meridians, in radians.
*     ACCLAT = DOUBLE PRECISION (Returned)
*        The accuracy with which to display latitude values, in radians.
*     ACCLON = DOUBLE PRECISION (Returned)
*        The accuracy with which to display longitude values, in
*        radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_ERR'          ! IRA error values.
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Arguments Given:
      CHARACTER SCS*(*)
      DOUBLE PRECISION  GAPLAT
      DOUBLE PRECISION GAPLON

*  Arguments Returned:
      DOUBLE PRECISION ACCLAT
      DOUBLE PRECISION ACCLON

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXDP              ! The maximum no. of decimal places
                                 ! with which sky coordinate values can
                                 ! be displayed (as defined in routine
                                 ! IRA1_IDRVA).
      PARAMETER ( MAXDP = 6 )

*  Local Variables:
      CHARACTER        BJ*1      ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQU.
      DOUBLE PRECISION EQU       ! The epoch of the reference equinox
                                 ! specified in argument SCS.
      INTEGER          FIELDS( 4 )! Integer fields.
      INTEGER          INTGAP    ! Integer holding scaled gap.
      DOUBLE PRECISION MAX10     ! Power of 10 corresponding to the max.
                                 ! no. of decimal places.
      CHARACTER        NAME*(IRA__SZSCS)! Full SCS name.
      INTEGER          POW10     ! An integer power of 10.
      CHARACTER        SIGN*1    ! The sign, "+" or "-".
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Identify the sky coordinate system.
      CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set up the power of 10 which has the same no. of trailing zeros as
*  the maximum no. of decimal places.
      MAX10 = DBLE( 10**MAXDP )

*  First deal with Galactic and Ecliptic coordinates, which are
*  formatted as decimal degrees values (IRA_DRGRD always uses style 0).
      IF( NAME( : 8 ) .EQ. 'GALACTIC' .OR.
     :    NAME( : 8 ) .EQ. 'ECLIPTIC' ) THEN

*  First deal with the longitude gap.  Convert the supplied floating
*  point gap size into an integer value representing the maximum number
*  of decimal places.
         INTGAP = NINT( ABS( IRA__RTOD*GAPLON*MAX10 ) )

*  Find the power of 10 corresponding to the position of the least
*  significant non-zero digit in the integer value.
         POW10 = 10

         IF( INTGAP .GT. 0 ) THEN

            DO WHILE( MOD( INTGAP, POW10 ) .EQ. 0 )
               POW10 = POW10*10
            END DO

         END IF

         POW10 = POW10/10

*  Return the decimal value corresponding to a value of 1 in the least
*  significant non-zero digit.
         ACCLON = IRA__DTOR*DBLE( POW10 )/MAX10

*  Do the same to find the latitude accuracy.
         INTGAP = NINT( ABS( IRA__RTOD*GAPLAT*MAX10 ) )

         POW10 = 10

         IF( INTGAP .GT. 0 ) THEN

            DO WHILE( MOD( INTGAP, POW10 ) .EQ. 0 )
               POW10 = POW10*10
            END DO

         END IF

         POW10 = POW10/10
         ACCLAT = IRA__DTOR*DBLE( POW10 )/MAX10

*  Now deal with Equatorial sky coordinates.
      ELSE IF( NAME( : 10 ) .EQ. 'EQUATORIAL' ) THEN

*  First deal with the RA gap. Convert the supplied floating
*  point gap value into 4 separate integer values representing the
*  hours, minutes, seconds and fraction of a second fields.
         CALL SLA_DR2TF( MAXDP, GAPLON, SIGN, FIELDS )

*  If the fractional seconds field is not zero...
         IF( FIELDS( 4 ) .NE. 0 ) THEN

*  Find the decimal value (in radians) corresponding to a value of 1 in
*  the least significant non-zero digit (see Galactic and Ecliptic
*  section above).
            POW10 = 10

            DO WHILE( MOD( FIELDS( 4 ), POW10 ) .EQ. 0 )
               POW10 = POW10*10
            END DO

            POW10 = POW10/10
            ACCLON = IRA__TS2R*DBLE( POW10 )/MAX10

*  Otherwise, if the seconds field is non-zero return an accuracy of 1
*  second.
         ELSE IF( FIELDS( 3 ) .NE. 0 ) THEN
            ACCLON = IRA__TS2R

*  Otherwise, if the minutes field is non-zero return an accuracy of 1
*  minute.
         ELSE IF( FIELDS( 2 ) .NE. 0 ) THEN
            ACCLON = IRA__TM2R

*  Otherwise, return an accuracy of 1 hour.
         ELSE
            ACCLON = IRA__TH2R

         END IF

*  Do the same for the latitude value, except using degrees, arc-minutes
*  and arc-seconds instead of hours, minutes and seconds.
         CALL SLA_DR2AF( MAXDP, GAPLAT, SIGN, FIELDS )

         IF( FIELDS( 4 ) .NE. 0 ) THEN

            POW10 = 10

            DO WHILE( MOD( FIELDS( 4 ), POW10 ) .EQ. 0 )
               POW10 = POW10*10
            END DO

            POW10 = POW10/10
            ACCLAT = IRA__AS2R*DBLE( POW10 )/MAX10

         ELSE IF( FIELDS( 3 ) .NE. 0 ) THEN
            ACCLAT = IRA__AS2R

         ELSE IF( FIELDS( 2 ) .NE. 0 ) THEN
            ACCLAT = IRA__AM2R

         ELSE
            ACCLAT = IRA__DTOR

         END IF

*  If the supplied coordinate system is not yet supported, report an
*  error.
      ELSE
         STATUS = IRA__BADSC
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'IRA1_LACC_ERR1',
     :  'IRA1_LACC: Sky coordinates ^NAME not yet supported.',
     :                 STATUS )

      END IF

 999  CONTINUE

      END
