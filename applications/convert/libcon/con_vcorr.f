      SUBROUTINE CON_VCORR( RA, DEC, UTDATE, UTIME, LAT, LONG, SRTYPE,
     :                      VCORR, STATUS )
*+
*  Name:
*     CON_VCORR

*  Purpose:
*     Computes correction to adjust observed radial velocity to some
*     standard of rest.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL CON_VCORR( RA, DEC, UTDATE, UTIME, LAT, LONG, SRTYPE; VCORR;
*                     STATUS )

*  Description:
*     Compute the correction to adjust an observed radial velocity to
*     some standard of rest.  The equatorial coordinates and instant of
*     time for which the correction is to be computed are given.  The
*     required standard of rest is specified from a list.
*
*     This routine is given the UTC of the instant at which the radial
*     velocity correction is to be computed, expressed as two character
*     strings, one giving the date, the other the time of day.  This
*     UTC is used as an approximation to UT1 and converted to a modified
*     Julian day (MJD).  The MJD is then used to compute the Local
*     Apparent Sidereal Time (LAST).
*
*     The UTC/MJD is used as an approximation to various time systems
*     (see the 'Implementation Deficiencies' section, below, for
*     further details).  See SUN/67 for further details of the time
*     systems.

*  Arguments:
*     RA  =  DOUBLE PRECISION (Given)
*        Right Ascension of the observation for which a correction is
*        to be computed (J2000, radians).
*     DEC  =  DOUBLE PRECISION (Given)
*        Declination of the observation for which a correction is to be
*        computed (J2000, radians).
*     UTDATE  =  CHARACTER*(*) (Given)
*        UTC date of the observation, expressed as a character string
*        of the form 'dd-mmm-yy', eg. '16-APR-94'.
*     UTIME  =  CHARACTER*(*) (Given)
*        UTC time of the observation, expressed as a character string
*        of the form 'hh:mm:ss', eg. '20:37:57'.
*     LAT  =  DOUBLE PRECISION (Given)
*        Geodetic latitude of the observatory where the observation
*        was made, in radians.
*     LONG  =  DOUBLE PRECISION (Given)
*        Geodetic longitude of the observatory where the observation
*        was made, expressed in radians with the (conventional
*        astronomical) convention that east is positive.
*     SRTYPE  =  CHARACTER*(*) (Given)
*        The type of standard of rest for which the correction is to
*        computed.  The permitted types are:
*        SOLAR      - solar (heliocentric),
*        KINLSR     - kinematical local standard of rest,
*        DYNLSR     - dynamical local standard of rest,
*        GALAXY     - centre of the Galaxy,
*        LOCALGROUP - Local Group.
*     VCORR  =  REAL (Returned)
*        Computed radial velocity correction (Km/sec).  Positive values
*        indicate recession.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Convert the UT date to a modified Julian date (MJD).
*     If ok then
*       Compute the time as an MJD by adding the UT time to the MJ date.
*       Compute the Greenwich mean sidereal time.
*       Compute the Local Apparent Sidereal time.
*       Compute the radial velocity correction for the Earth's diurnal
*       rotation.
*       Compute the radial velocity correction for the Earth's
*       revolution about the Sun.
*       Compute the additional radial velocity correction to the
*       appropriate standard of rest.
*       Add the individual corrections to give the total correction.
*     end if

*  Implementation Deficiencies:
*     Various approximations are made in the treatment of time systems.
*     The UTC is used as an approximation for UT1 (for want of the
*     information to perform the necessary correction).
*
*     The UTC expressed as an MJD is used as an approximation for both
*     the Terrestrial Time (TT) and the Barycentric Dynamical Time
*     (TDB).  Also, the calculation of the Local Apparent Sidereal Time
*     (LAST) does not include the equation of the equinoxes.  All these
*     approximations are insignificant compared to the original
*     approximation of UT1 by UTC.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     28/7/97 (ACD):
*        Original version.
*     12/8/97 (ACD):
*        First stable version.
*     2009 June 29 (MJC):
*        Used modern coding style.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants

*  Arguments Given:
      DOUBLE PRECISION RA
      DOUBLE PRECISION DEC
      CHARACTER*(*) UTDATE
      CHARACTER*(*) UTIME
      DOUBLE PRECISION LAT
      DOUBLE PRECISION LONG
      CHARACTER*(*) SRTYPE

*  Arguments Returned:
      REAL VCORR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_GMST
      DOUBLE PRECISION SLA_DVDV
      DOUBLE PRECISION SLA_RVEROT
      DOUBLE PRECISION SLA_RVLSRK
      DOUBLE PRECISION SLA_RVLSRD
      DOUBLE PRECISION SLA_RVGALC
      DOUBLE PRECISION SLA_RVLG

*  Local Variables:
      CHARACTER*3 CMONTH         ! Month as three-character abbreviation
      INTEGER DAY                ! Day
      REAL DECR                  ! Declination (as a REAL)
      INTEGER DSTAT              ! Local SLA status
      DOUBLE PRECISION DUMMY1( 3 ) ! Dummy argument
      DOUBLE PRECISION DUMMY2( 3 ) ! Dummy argument
      DOUBLE PRECISION DUMMY3( 3 ) ! Dummy argument
      DOUBLE PRECISION DVH( 3 )  ! Heliocentric velocity of the Earth
                                 ! (AU/sec)
      DOUBLE PRECISION GMST      ! Greenwich mean sidereal time (radians)
      DOUBLE PRECISION HOUR      ! Hour of the UTC
      DOUBLE PRECISION LAST      ! Local apparent sidereal time
      INTEGER LOOP               ! Loop index
      DOUBLE PRECISION MIN       ! Minutes of the UTC
      INTEGER MONTH              ! Month
      DOUBLE PRECISION MJD       ! Date and time expressed as an UT MJD
      DOUBLE PRECISION MJDATE    ! Date expressed as an UT MJD
      REAL RAR                   ! Right Ascension (as a REAL)
      REAL RLAST                 ! LAST (as a REAL)
      REAL RLAT                  ! Latitude (as a REAL)
      DOUBLE PRECISION SCUV( 3 ) ! Cartesian unit vector of right
                                 ! ascension and declination
      DOUBLE PRECISION SEC       ! Seconds of the UTC
      REAL VROT                  ! Correction for Earth's diurnal rotation
                                 ! (km/sec)
      REAL VREV                  ! Correction for Earth's revolution
                                 ! about the Sun (km/sec)
      REAL VSR                   ! Additional correction to standard of
                                 ! rest (km/sec)
      INTEGER YEAR               ! Year

*  Local Data:
      CHARACTER*3 MONTHS( 12 )   ! Abbreviated months of the year
      DATA MONTHS /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL',
     :             'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
      SAVE MONTHS

*.

*  Check the global inherited status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Convert the UTC date to a modified Julian date (MJD).  First extract
*  the year, month and day of the UTC from the input character string
*  and convert them to integers.  Then attempt to calculate the MJD.
*  Report an error of the conversion fails.
         CALL CHR_CTOI( UTDATE( 1:2 ), DAY, STATUS )
         CALL CHR_CTOI( UTDATE( 8:9 ), YEAR, STATUS )

         CMONTH( 1:3 ) = UTDATE( 4:6 )
         CALL CHR_UCASE( CMONTH )

         MONTH = 0

         DO LOOP = 1, 12
            IF ( CMONTH .EQ. MONTHS( LOOP ) ) THEN
               MONTH = LOOP
            END IF
         END DO

         CALL SLA_CALDJ( YEAR, MONTH, DAY, MJDATE, DSTAT )

         IF ( DSTAT .NE. 0 ) THEN
            STATUS = SAI__ERROR

            CALL MSG_SETC( 'UTDATE', UTDATE)
            IF ( DSTAT .EQ. 1 ) THEN
               CALL MSG_SETC( 'CODE', 'bad year' )
            ELSE IF ( DSTAT .EQ. 2 ) THEN
               CALL MSG_SETC( 'CODE', 'bad month' )
            ELSE IF ( DSTAT .EQ. 3 ) THEN
               CALL MSG_SETC( 'CODE', 'bad day' )
            ELSE
               CALL MSG_SETC( 'CODE', 'bad error code' )
            END IF

            CALL ERR_REP( ' ', 'Error converting UTC date ^UTDATE '/
     :                    /'to MJD (^CODE).', STATUS )
         END IF

*  Proceed if OK.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Compute the time as an MJD by adding the UT time to the MJ date.
*  First convert the time from a character string to DOUBLE PRECISION
*  numbers.
            CALL CHR_CTOD( UTIME( 1:2 ), HOUR, STATUS )
            CALL CHR_CTOD( UTIME( 4:5 ), MIN, STATUS )
            CALL CHR_CTOD( UTIME( 7:8 ), SEC, STATUS )

            MJD = MJDATE + ( HOUR / 2.4D1 ) +
     :                     ( MIN / ( 2.4D1 * 6.0D1 ) ) +
     :                     ( SEC / ( 2.4D1 * 6.0D1 * 6.0D1 ) )

*  Compute the Greenwich mean sidereal time (GMST) corresponding
*  to the UTC.  Note that here UT1 is being approximated by UTC.
            GMST = SLA_GMST( MJD )

*  Compute the Local Apparent Sidereal time corresponding to the
*  GMST.  Remember the sign convention for the longitude.
            LAST = LONG + GMST

*  Compute the radial velocity correction for the Earth's diurnal
*  rotation.
            RLAT = SNGL(LAT)
            RAR = SNGL(RA)
            DECR = SNGL(DEC)
            RLAST = SNGL(LAST)

            VROT = SLA_RVEROT( RLAT, RAR, DECR, RLAST)

*  Compute the radial velocity correction for the Earth's revolution
*  about the Sun.
            CALL SLA_DCS2C( RA, DEC, SCUV )

            CALL SLA_EVP( MJD, 2.0D3, DUMMY1, DUMMY2, DVH, DUMMY3 )

            VREV = - SNGL( SLA_DVDV( SCUV, DVH ) * 1.49597870D8 )

*  Compute the additional radial velocity correction to the
*  appropriate standard of rest.
            IF ( SRTYPE .EQ. 'SOLAR' ) THEN

*  Solar (heliocentric); no further corrections are needed.
               VSR = 0.0E0

            ELSE IF (SRTYPE .EQ. 'KINLSR') THEN

*  Kinematical LSR (broadly observational).
               VSR = SLA_RVLSRK(RAR, DECR)

            ELSE IF (SRTYPE .EQ. 'DYNLSR') THEN

*  Dynamical LSR (broadly theoretical).
               VSR = SLA_RVLSRD( RAR, DECR )

            ELSE IF ( SRTYPE .EQ. 'GALAXY' ) THEN

*  Centre of the Galaxy.
               VSR = SLA_RVLSRD(RAR, DECR) + SLA_RVGALC(RAR, DECR)

            ELSE IF ( SRTYPE .EQ. 'LOCALGROUP' ) THEN

*  Relative to the Local Group.
               VSR = SLA_RVLG( RAR, DECR )

            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'SRTYPE', SRTYPE )
               CALL ERR_REP( 'CON_VCORR_SR', 'Unrecognised standard '/
     :                       /'of rest (^SRTYPE) specified.', STATUS )

               VSR = 0.0E0
            END IF

*  Add the individual corrections to give the total correction.
            VCORR = VROT + VREV + VSR
         END IF

      END IF

      END
