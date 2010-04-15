      SUBROUTINE CAP_CMPMO (EQUIN, MO, TSCOPE, LONG, LAT, HEIGHT,
     :  UTMJD, LSTFLG, UTDATE, LST, TEMP, ATMOSP, HUMID, WAVE, TROPL,
     :  STATUS)
*+
*  Name:
*     CAP_CMPMO
*  Purpose:
*     Compute the conversion array for mean to observed coordinates.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CMPMO (EQUIN; MO, TSCOPE, LONG, LAT, HEIGHT,
*       UTMJD, LSTFLG, UTDATE, LST, TEMP, ATMOSP, HUMID, WAVE, TROPL;
*       STATUS)
*  Description:
*     Compute the conversion array for mean to observed coordinates.
*
*     The observed coordinates pertain to a specific location
*     and instant.  Often the location will correspond to that of a
*     named telescope.
*
*     The details needed to compute the array are obtained from the
*     environment.
*  Arguments:
*     EQUIN  =  CHARACTER*(*) (Given)
*        Equinox of the coordinates (formatted as a character string).
*     MO(35)  =  DOUBLE PRECISION (Returned)
*        Conversion array.
*     TSCOPE  =  CHARACTER*(*) (Returned)
*        Name of the telescope.  If the telescope was not specified
*        then '?' is returned.
*     LONG  =  DOUBLE PRECISION (Returned)
*        Geographical longitude of the observation, expressed in
*        radians with the (conventional astronomical) convention that
*        east is positive.
*     LAT  =  DOUBLE PRECISION (Returned)
*        Geographical latitude of the observation, expressed in
*        radians .
*     HEIGHT  =  DOUBLE PRECISION (Returned)
*        Height of the observation above sea level (metres).
*     UTMJD  =  DOUBLE PRECISION (Returned)
*        UT of the observation, expressed as an MJD.
*     LSTFLG  =  LOGICAL (Returned)
*        Flag indicating whether the LST of the observation is also
*        available.
*     UTDATE  =  CHARACTER*(*) (Returned)
*        The UT date of observation, coded as a CHARACTER string, as
*        follows:  'YYMMDD'.
*     LST  =  CHARACTER*(*) (Returned)
*        The local mean sidereal time of the observation, coded as a
*        CHARACTER string, as follows: 'HHMM'.
*     TEMP  =  DOUBLE PRECISION (Returned)
*        Local ambient temperature (degrees Kelvin).
*     ATMOSP  =  DOUBLE PRECISION (Returned)
*        Local atmospheric pressure (mB).
*     HUMID  =  DOUBLE PRECISION (Returned)
*        Local relative humidity (in the range 0.0 - 1.0).
*     WAVE  =  DOUBLE PRECISION (Returned)
*        Effective wavelength of the plate/filter (Angstrom).
*     TROPL  =  DOUBLE PRECISION (Returned)
*        Tropospheric lapse rate (degrees K per metre).
*     STATUS = INTEGER (UPDATE)
*        The global status.
*  Algorithm:
*     Attempt to get the name of the telescope.
*     If ok then
*       Attempt to get the telescope details.
*       If failed to locate the given telescope then
*         Set the status.
*         Report an error.
*       end if
*     else a null status was returned then
*       Annull the error.
*       Get the longitude.
*       Get the latitude.
*       Get the height above sea level.
*     end if
*     Reverse the sign of the longitude.
*     Attempt the get the UT MJD of the observation.
*     If a null status was returned then
*       Annull the error.
*       Get the UT date.
*       Get the LST.
*       Calculate the UT MJD.
*       Set the flag indicating that the LST etc. are available.
*     else
*       Set the flag indicating that the LST etc. are not available.
*     end if
*     Get the temperature.
*     Get the atmospheric pressure.
*     Get the humidity.
*     Get the wavelength.
*     Get the tropospheric lapse rate.
*     If ok then
*       Convert the equinox to a separate system and date.
*       Calculate the desired Julian epoch.
*       Calculate the equivalent terrestrial time, TT.
*       Calculate the mean to apparent part of the array.
*       Calculate the apparent to observed part of the array.
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     20/5/98 (ACD): Original version.
*     22/5/98 (ACD): First stable version.
*-
*  Type Definitions:
      IMPLICIT NONE       ! No implicit typing
*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants.
      INCLUDE 'PAR_ERR'   ! PAR_ error constants.
      INCLUDE 'CAP_PAR'   ! CAP parametric constants.
*  Arguments Given:
      CHARACTER
     :  EQUIN*(*)
*  Arguments Returned:
      DOUBLE PRECISION
     :  MO(35),
     :  LONG,
     :  LAT,
     :  HEIGHT,
     :  UTMJD,
     :  TEMP,
     :  ATMOSP,
     :  HUMID,
     :  WAVE,
     :  TROPL
      CHARACTER
     :  TSCOPE*(*),
     :  UTDATE*(*),
     :  LST*(*)
      LOGICAL
     :  LSTFLG
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      DOUBLE PRECISION
     :  SLA_EPCO,
     :  SLA_DTT,
     :  SLA_DRANGE
*  Local Variables:
      CHARACTER
     :  OBSNAM*80, ! Returned name of the telescope.
     :  EQINT*1    ! Time system for the equinox: 'B' or 'J'.
      DOUBLE PRECISION
     :  EQIN,      ! Equinox (years).
     :  WAVEM,     ! Wavelength in micron.
     :  DEQUNJ,    ! Desired Julian equinox.
     :  TT         ! Terrestrial time (TT) equivalent to MJD.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Attempt to get the name of the telescope.

         CALL PAR_GET0C ('TSCOPE', TSCOPE, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Attempt to get the telescope details.  Set the status
*          and report an error of the specified telescope could
*          not be found.

            CALL CHR_UCASE (TSCOPE)
            CALL SLA_OBS (0, TSCOPE, OBSNAM, LONG, LAT, HEIGHT)

            IF (OBSNAM .EQ. '?') THEN
               STATUS = SAI__ERROR

               CALL MSG_SETC ('TSCOPE', TSCOPE)
               CALL ERR_REP ('CATCOORD_TSC', 'Unknown telescope '/
     :           /'specified: ^TSCOPE.', STATUS)
            END IF

         ELSE IF (STATUS .EQ. PAR__NULL) THEN

*
*          A null value was given for the telescope name; annull the
*          status and get the geographic details (longitude, latitude,
*          height above sea level) from the environment.

            CALL ERR_ANNUL (STATUS)

            TSCOPE = '?'

            CALL PAR_GET0D ('LONG', LONG, STATUS)
            CALL PAR_CANCL ('LONG', STATUS)

            CALL PAR_GET0D ('LAT', LAT, STATUS)
            CALL PAR_CANCL ('LAT', STATUS)

            CALL PAR_GET0D ('HEIGHT', HEIGHT, STATUS)
            CALL PAR_CANCL ('HEIGHT', STATUS)

         END IF

         CALL PAR_CANCL ('TSCOPE', STATUS)

*
*       Reverse the sign of the longitude.
*
*       SLA_OBS returns longitude with a sign such that west is
*       positive (in conformance with normal geographical
*       convention).  Longitudes supplied through the parameter
*       system should follow the same convention.
*
*       However, other SLA routines (in particular SLA_AOPPA)
*       which are called subsequently in the COSMOSRADEC software
*       require longitude with a sign such that east is positive
*       (in conformance with modern astronomical conventions).
*
*       Therefore it is necessary to reverse the sign of the
*       longitude.

         LONG = SLA_DRANGE(-LONG)

*
*       Attempt the get the UT MJD of the observation.  If a null
*       value is given then annull the error and attempt to obtain
*       the LST etc. and calculate the UT MJD.

         CALL PAR_GET0D ('UTMJD', UTMJD, STATUS)

         IF (STATUS .EQ. PAR__NULL) THEN

*
*          Annull the error.

            CALL ERR_ANNUL (STATUS)

*
*          Get the UT date and LST.

            CALL PAR_GET0C ('UTDATE', UTDATE, STATUS)
            CALL PAR_CANCL ('UTDATE', STATUS)

            CALL PAR_GET0C ('LST', LST, STATUS)
            CALL PAR_CANCL ('LST', STATUS)

*
*          Calculate the UT MJD.

            CALL CAP_CUTOB (UTDATE, LST, LONG, UTMJD, STATUS)

C           print3000, utmjd
C3000       format(1x, 'utmjd: ', 1pd20.8 )

*
*          Set the flag indicating that the LST etc. are available.

            LSTFLG = .TRUE.

         ELSE

*
*          Set the flag indicating that the LST etc. are not available.

            LSTFLG = .FALSE.

         END IF

         CALL PAR_CANCL ('UTMJD', STATUS)

*
*       Get the temperature, atmospheric pressure, humidity,
*       wavelength and tropospheric lapse rate.

         CALL PAR_GET0D ('TEMP', TEMP, STATUS)
         CALL PAR_CANCL ('TEMP', STATUS)

         CALL PAR_GET0D ('ATMOSP', ATMOSP, STATUS)
         CALL PAR_CANCL ('ATMOSP', STATUS)

         CALL PAR_GET0D ('HUMID', HUMID, STATUS)
         CALL PAR_CANCL ('HUMID', STATUS)

         CALL PAR_GET0D ('WAVE', WAVE, STATUS)
         CALL PAR_CANCL ('WAVE', STATUS)

         CALL PAR_GET0D ('TROPL', TROPL, STATUS)
         CALL PAR_CANCL ('TROPL', STATUS)

*
*       Proceed if all is ok.

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Convert the equinox to a separate system and date.

            CALL CAP_DCEQP (EQUIN, EQINT, EQIN, STATUS)

*
*          Calculate the desired equinox as a Julian equinox, if it
*          is not already one.

            IF (EQINT .EQ. 'J') THEN
               DEQUNJ = EQIN
            ELSE
               DEQUNJ = SLA_EPCO('J', 'B', EQIN)
            END IF

*
*          Calculate the terrestrial time, TT, corresponding to the
*          MJD.

            TT = UTMJD + SLA_DTT(UTMJD)

*
*          Calculate the mean to apparent part of the array.
*
*          Note the use of Terrestrial Time, TT, instead of Barycentric
*          Dynamical Time, TDB.  For the present purposes TT is a
*          sufficient approximation to TDB.

            CALL SLA_MAPPA (DEQUNJ, TT, MO)

*
*          Calculate the apparent to observed part of the array.
*
*          Note:
*
*           - the difference between UTC and UT1 is set to zero,
*
*           - the polar motion is set to zero.
*
*          These approximations are adequate for the present purposes.
*
*          Also, the effective wavelength has to be converted from
*          Angstrom to micron.

            WAVEM = WAVE / 1.0D4

            CALL SLA_AOPPA (UTMJD, 0.0D0, LONG, LAT, HEIGHT, 0.0D0,
     :        0.0D0, TEMP, ATMOSP, HUMID, WAVEM, TROPL, MO(22) )
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CATCOORD_ERR', 'Failed to compute '/
     :        /'the mean to observed coords. conversion array.',
     :        STATUS)
         END IF

      END IF

      END
