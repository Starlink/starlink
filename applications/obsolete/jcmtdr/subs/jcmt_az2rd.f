      SUBROUTINE JCMT_ALTAZ2RADEC (RA2000, DEC2000, MJDSTART,
     :   DUT, LONG, LAT, HT, XP, YP, TDK, PMB, RH, TLR, OBFREQ,
     :   PROJ, NX, NY, XOFF, YOFF, LST, RAOUT, DECOUT, STATUS)

*+
*  Name:
*     JCMT_ALTAZ2RADEC

*  Purpose:
*     Produce a list of RA,DEC from a list of AZ,EL,LST

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL JCMT_ALTAZ2RADEC( RA2000, DEC2000, MJDSTART, DUT, LONG, LAT,
*     :  HT, XP, YP, TDK, PMB, RH, TLR, OBFREQ, PROJ, NX, NY,
*     :  XOFF, YOFF, LST, RAOUT, DECOUT, STATUS)

*  Description:
*     According to the Nautical Almanac:-
*     Apparent place is the position on a celestial sphere centred at the
*     Earth, determined by removing from the directly observed position
*     of a celestial body the effects that depend on the topocentric
*     location of the observer; i.e. refraction, diurnal aberration and
*     geocentric (diurnal) parallax. Thus the position at which the
*     object would actually be seen from the centre of the Earth, displaced
*     by planetary aberration (except the diurnal part) and referred
*     to the true equinox and equator.
*
*     Mean place is the co-ordinates, referred to the mean equator and
*     equinox of a standard epoch, of an object on the celestial sphere
*     centred on the Sun. A mean place is determined by removing from the
*     directly observed position the effects of refraction, geocentric
*     and stellar parallax, and stellar aberration, and by referring the
*     co-ordinates to the mean equator and equinox of a standard epoch.
*
*     The routine uses SLA routines to:-
*       Calculate the geocentric apparent RA, Dec of the mean observation
*       centre at the date and time of the observation.
*       For each pixel:-
*         Calculate the observed az,el corresponding to the geocentric
*         apparent observation centre at the LST at which the pixel was
*         observed.
*         Calculate the observed az,el of the pixel.
*         Calculate the geocentric apparent RA,Dec of the current pixel.
*         Calculate the mean RA,Dec of the current pixel.
*
*      This process of working back from RA2000, DEC2000 to the actual
*      RA, Dec observed by the telescope assumes that the JCMT telescope
*      task used the same method in reverse to work out RA2000 and DEC2000
*      from observed RA, Dec. This assumption may be invalid, particularly
*      regarding the atmospheric refraction parameters used.

*  Arguments:
*     RA2000 = DOUBLE PRECISION (Given)
*        The J2000 mean Right Ascension of the map centre (radians)
*     DEC2000 = DOUBLE PRECISION (Given)
*        The J2000 mean declination of the map centre (radians)
*     MJDSTART = DOUBLE PRECISION (Given)
*        The Modified Julian date of the start of the observation
*     DUT = DOUBLE PRECISION (Given)
*        Ut1-UTC for the observation (Seconds) - only needed for the
*        highest precision
*     LONG = DOUBLE PRECISION (Given)
*        The mean longitude of the observer (East +ve) (Radians)
*     LAT = DOUBLE PRECISION (Given)
*        The mean geodetic latitude of the observer (Radians)
*     HT = DOUBLE PRECISION (Given)
*        height above sea level (metres)
*     XP = DOUBLE PRECISION (Given)
*        X component of polar motion (radians)
*     YP = DOUBLE PRECISION (Given)
*        Y component of polar motion (radians)
*     TDK = DOUBLE PRECISION (Given)
*        Local ambient temperature (degK)
*     PMB = DOUBLE PRECISION (Given)
*        Local ambient pressure (mB)
*     RH = DOUBLE PRECISION (Given)
*        Local relative humidity
*     TLR = DOUBLE PRECISION (Given)
*        Tropospheric lapse rate (DegK/metre)
*     OBFREQ = DOUBLE PRECISION (Given)
*        The observing frequency (Hz)
*     PROJ = CHARACTER * ( * ) (Given)
*        The projection type of the output
*     NX = INTEGER (Given)
*        The number of input map pixels in the X direction
*     NY = INTEGER (Given)
*        The number of input map pixels in the Y direction
*     XOFF( NX ) = REAL (Given)
*        The offset from the map centre of each pixel in the X
*        direction. These offsets are given in the Alt-Az frame in a ARC
*        projection
*     YOFF( NX ) = REAL (Given)
*        The offset from the map centre of each pixel in the Y
*        direction. These offsets are given in the Alt-Az frame in a ARC
*        projection
*     LST( NX, NY ) = DOUBLE PRECISION (Given)
*        The Local apparent sidereal time of observation of each of the
*        map pixels.
*     RAOUT( NX, NY ) = DOUBLE PRECISION (Returned)
*        The mean right acensions of the map pixels at equinox EQNX
*     DECOUT( NX, NY ) = DOUBLE PRECISION (Returned)
*        The mean declinations of the map pixels at equinox EQNX
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JAN-1990 (JBVAD::PAH):
*        Original version.
*      9-MAY-1991 (REVAD::JFL): Comments expanded, wavelength now calculated
*                               in microns rather than metres.
*     13-MAY-1991 (REVAD::JFL): Changed parameter list of JCMT_RADREFR
*     23-SEP-1991 (REVAD::JFL): Removed calculation of max and min of RAs
*                               and Decs.
*     11-NOV-1991 (REVAD::JFL): Fixed MAJOR bug whereby azimuths where
*                               calculated without cos(altitude) effect taken
*                               into account.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'        ! standard astronomical constants

*  Arguments Given:
      DOUBLE PRECISION RA2000, DEC2000
      DOUBLE PRECISION MJDSTART
      DOUBLE PRECISION DUT
      DOUBLE PRECISION LONG, LAT
      DOUBLE PRECISION HT
      DOUBLE PRECISION XP, YP
      DOUBLE PRECISION TDK, PMB, RH, TLR, OBFREQ
      INTEGER NX
      INTEGER NY
      REAL XOFF( NX )
      REAL YOFF( NY )
      DOUBLE PRECISION LST( NX,NY )
      CHARACTER * ( * ) PROJ

*  Arguments Returned:
      DOUBLE PRECISION RAOUT( NX,NY )
      DOUBLE PRECISION DECOUT( NX,NY )

*  Status:
      INTEGER STATUS                ! Global status

*  Local Variables:
      DOUBLE PRECISION RA           ! RA of date of map centre
      DOUBLE PRECISION DEC          ! Dec of date of map centre
      DOUBLE PRECISION DUMMY        ! dummy argument
      DOUBLE PRECISION SPHI         ! sine of latitude
      DOUBLE PRECISION CPHI         ! cos of latitude
      DOUBLE PRECISION AZC          ! azimuth of the centre
      DOUBLE PRECISION ZENC         ! zenith distance of the centre
      DOUBLE PRECISION AZP          ! azimuth of current pixel
      DOUBLE PRECISION ZENP         ! zenith distance of current pixel
      INTEGER IX                    ! current pixel x index
      INTEGER IY                    ! current pixel y index
      DOUBLE PRECISION AMPRMS(21)   ! parameter array for SLA_MAPPA
      DOUBLE PRECISION AOPRMS(14)   ! parameter array for SLA_AOP
      DOUBLE PRECISION WL           !  wavelength
      DOUBLE PRECISION HAOB, DECOB, RAOB

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

*  precompute the parameters needed by the SLA apparent to observed
*  routines, first calculating the observing wavelength in microns

      WL = C * 1.0d6 / OBFREQ
      CALL SLA_AOPPA (MJDSTART, DUT, LONG, LAT, HT, XP, YP, TDK,
     :   PMB, RH, WL, TLR, AOPRMS)

*  correct the refraction constants here to something appropriate to mm
*  range

      CALL JCMT_RADREFR (HT, TDK, PMB, RH, WL, LAT, AOPRMS(11),
     :   AOPRMS(12))

*  precompute the parameters needed by SLA mean to apparent routines

      CALL SLA_MAPPA (2000D0, MJDSTART, AMPRMS)

*  compute geocentric apparent position of mean map centre at date of
*  observation

      CALL SLA_MAPQKZ (RA2000, DEC2000, AMPRMS, RA, DEC)

*  loop over the map pixels

      DO IY = 1, NY
         DO IX = 1, NX

*  load the pixel LST into the appropriate place in the parameter array

            AOPRMS(14) = LST(IX,IY)

*  find the observed az,zen of the centre of the observation at the LST
*  that this pixel was observed

            CALL SLA_AOPQK (RA, DEC, AOPRMS, AZC, ZENC, HAOB, DECOB,
     :         RAOB)

*  find observed az, zen of current pixel (correct for cos(altitude) effect
*  on azimuth values). Will become incorrect near zenith and crash at zenith.

            ZENP = ZENC - YOFF(IY) * DAS2R
            AZP = AZC + XOFF(IX) * DAS2R / SIN (ZENP)

*  find geocentric apparent ra, dec of current pixel

            CALL SLA_OAPQK ('A', AZP, ZENP, AOPRMS, RAOUT(IX,IY),
     :         DECOUT(IX,IY))

*  find mean ra,dec of current pixel

            CALL SLA_AMPQK (RAOUT(IX,IY), DECOUT(IX,IY),AMPRMS,
     :         RAOUT(IX,IY), DECOUT(IX,IY))

         END DO
      END DO

      END

