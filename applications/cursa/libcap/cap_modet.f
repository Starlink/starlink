      SUBROUTINE CAP_MODET (CIOUT, CRDTYP, TSCOPE, LONG, LAT, HEIGHT,
     :  UTMJD, LSTFLG, UTDATE, LST, TEMP, ATMOSP, HUMID, WAVE, TROPL,
     :  STATUS)
*+
*  Name:
*     CAP_MODET
*  Purpose:
*     Write out the details used to compute the observed coordinates.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_MODET (CIOUT, CRDTYP, TSCOPE, LONG, LAT, HEIGHT, UTMJD,
*       LSTFLG, UTDATE, LST, TEMP, ATMOSP, HUMID, WAVE, TROPL; STATUS)
*  Description:
*     Write out the details used to compute the observed coordinates
*     to the catalogue.
*  Arguments:
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     CRDTYP  =  INTEGER (Given)
*        Type of coordinates computed.
*     TSCOPE  =  CHARACTER*(*) (Given)
*        Name of the telescope.  If the telescope was not specified
*        then '?' is returned.
*     LONG  =  DOUBLE PRECISION (Given)
*        Geographical longitude of the observation, expressed in
*        radians with the (conventional astronomical) convention that
*        east is positive.
*     LAT  =  DOUBLE PRECISION (Given)
*        Geographical latitude of the observation, expressed in
*        radians .
*     HEIGHT  =  DOUBLE PRECISION (Given)
*        Height of the observation above sea level (metres).
*     UTMJD  =  DOUBLE PRECISION (Given)
*        UT of the observation, expressed as an MJD.
*     LSTFLG  =  LOGICAL (Given)
*        Flag indicating whether the LST of the observation is also
*        available.
*     UTDATE  =  CHARACTER*(*) (Given)
*        The UT date of observation, coded as a CHARACTER string, as
*        follows:  'YYMMDD'.
*     LST  =  CHARACTER*(*) (Given)
*        The local mean sidereal time of the observation, coded as a
*        CHARACTER string, as follows: 'HHMM'.
*     TEMP  =  DOUBLE PRECISION (Given)
*        Local ambient temperature (degrees Kelvin).
*     ATMOSP  =  DOUBLE PRECISION (Given)
*        Local atmospheric pressure (mB).
*     HUMID  =  DOUBLE PRECISION (Given)
*        Local relative humidity (in the range 0.0 - 1.0).
*     WAVE  =  DOUBLE PRECISION (Given)
*        Effective wavelength of the plate/filter (Angstrom).
*     TROPL  =  DOUBLE PRECISION (Given)
*        Tropospheric lapse rate (degrees K per metre).
*     STATUS = INTEGER (UPDATE)
*        The global status.
*  Algorithm:
*     Write header including the type of coordinates computed.
*     If available then
*       Write the name of the telescope.
*     end if
*     Write the geographic longitude.
*     Write the geographic latitude.
*     Write the height above sea level.
*     Write the UT MJD.
*     If the LST is available then
*       Write the UT date.
*       Write the LST.
*     end if
*     Write the temperature.
*     Write the atmospheric pressure.
*     Write the humidity.
*     Write the wavelength.
*     Write the tropospheric lapse rate.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     22/5/98 (ACD): Original version.
*-
*  Type Definitions:
      IMPLICIT NONE       ! No implicit typing
*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants.
      INCLUDE 'PAR_ERR'   ! PAR_ error constants.
      INCLUDE 'CAP_PAR'   ! CAP parametric constants.
*  Arguments Given:
      INTEGER
     :  CIOUT,
     :  CRDTYP
      DOUBLE PRECISION
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
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  BUFFER*75,   ! Output buffer for the current comment.
     :  TDESCR*50,   ! Description of the telescope.
     :  LONGD*20,    ! Longitude in sexagesimal degrees.
     :  LATD*20      ! Latitude  "      "          "   .
      INTEGER
     :  LENGTH,      ! Length of a string (excl. trail. blanks).
     :  BUFLEN       !   "    "  BUFFER   ( "  .   "  .   "   ).
      DOUBLE PRECISION
     :  LONGR,       ! Longitude in radians.
     :  LATR,        ! Latitude  "     "   .
     :  HT           ! Height above sea level in metres.
*.

      IF (STATUS .EQ. SAI__OK) THEN

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Details used to compute the ', BUFFER, BUFLEN)

         IF (CRDTYP .EQ. CAP__CDEQO) THEN
            CALL CHR_PUTC ('observed equatorial ', BUFFER, BUFLEN)

         ELSE IF (CRDTYP .EQ. CAP__CDEQL) THEN
            CALL CHR_PUTC ('local ', BUFFER, BUFLEN)

         ELSE IF (CRDTYP .EQ. CAP__CDHOR) THEN
            CALL CHR_PUTC ('horizon ', BUFFER, BUFLEN)

         ELSE
            CALL CHR_PUTC ('<unknown code: ', BUFFER, BUFLEN)
            CALL CHR_PUTI (CRDTYP, BUFFER, BUFLEN)
            CALL CHR_PUTC ('> ', BUFFER, BUFLEN)

         END IF

         CALL CHR_PUTC ('coordinates.', BUFFER, BUFLEN)

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)
         CALL CAT_PUTXT (CIOUT, 'COMMENT', ' ', STATUS)

*
*       If available, the name of the telescope.

         IF (TSCOPE .NE. '?') THEN
            CALL SLA_OBS (0, TSCOPE, TDESCR, LONGR, LATR, HT)

            BUFFER = ' '
            BUFLEN = 0

            CALL CHR_PUTC ('Telescope: ', BUFFER, BUFLEN)

            IF (TSCOPE .NE. ' ') THEN
               LENGTH = CHR_LEN(TSCOPE)
               CALL CHR_PUTC (TSCOPE(1 : LENGTH), BUFFER, BUFLEN)
            END IF

            IF (TDESCR .NE. ' ') THEN
               CALL CHR_PUTC (' (', BUFFER, BUFLEN)

               LENGTH = CHR_LEN(TDESCR)
               CALL CHR_PUTC (TDESCR(1 : LENGTH), BUFFER, BUFLEN)

               CALL CHR_PUTC (')', BUFFER, BUFLEN)
            END IF
         END IF

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

*
*       Write the geographic longitude, latitude and height above
*       sea level.   The longitude and latitude are reformatted as
*       sexagesimal degrees.  Note that the sign of the longitude is
*       changed in order to restore the normal geographical
*       convention.

         LONGR = - LONG
         CALL CAP_R2SGF (LONGR, 'DEGREES', 1, LONGD, STATUS)
         CALL CAP_R2SGF (LAT, 'DEGREES', 1, LATD, STATUS)

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Longitude: ', BUFFER, BUFLEN)
         IF (LONGD .NE. ' ') THEN
            LENGTH = CHR_LEN(LONGD)
            CALL CHR_PUTC (LONGD(1 : LENGTH), BUFFER, BUFLEN)
         END IF
         CALL CHR_PUTC (' degrees.', BUFFER, BUFLEN)

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Latitude: ', BUFFER, BUFLEN)
         IF (LONGD .NE. ' ') THEN
            LENGTH = CHR_LEN(LATD)
            CALL CHR_PUTC (LATD(1 : LENGTH), BUFFER, BUFLEN)
         END IF
         CALL CHR_PUTC (' degrees.', BUFFER, BUFLEN)

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Height above sea level: ', BUFFER, BUFLEN)
         CALL CHR_PUTD (HEIGHT, BUFFER, BUFLEN)
         CALL CHR_PUTC (' metre.', BUFFER, BUFLEN)

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

*
*       Write the UT MJD.

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('UT of observation (as an MJD): ', BUFFER,
     :     BUFLEN)
         CALL CHR_PUTD (UTMJD, BUFFER, BUFLEN)

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

*
*       If they are available, write the UT date and the LST.

         IF (LSTFLG) THEN

            BUFFER = ' '
            BUFLEN = 0

            CALL CHR_PUTC ('UT date: ', BUFFER, BUFLEN)
            IF (UTDATE .NE. ' ') THEN
               LENGTH = CHR_LEN(UTDATE)
               CALL CHR_PUTC (UTDATE(1 : LENGTH), BUFFER, BUFLEN)
            END IF

            CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN),
     :        STATUS)

            BUFFER = ' '
            BUFLEN = 0

            CALL CHR_PUTC ('Local sidereal time (LST): ', BUFFER,
     :        BUFLEN)
            IF (LST .NE. ' ') THEN
               LENGTH = CHR_LEN(LST)
               CALL CHR_PUTC (LST(1 : LENGTH), BUFFER, BUFLEN)
            END IF

            CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN),
     :        STATUS)
         END IF

*       Write the atmospheric details: temperature, atmospheric
*       pressure, humidity, wavelength of observation and the
*       tropospheric lapse rate.

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Local ambient temperature: ', BUFFER,
     :     BUFLEN)
         CALL CHR_PUTD (TEMP, BUFFER, BUFLEN)
         CALL CHR_PUTC (' degrees Kelvin.', BUFFER,
     :     BUFLEN)

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Local atmospheric pressure: ', BUFFER,
     :     BUFLEN)
         CALL CHR_PUTD (ATMOSP, BUFFER, BUFLEN)
         CALL CHR_PUTC (' mB.', BUFFER, BUFLEN)

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Local relative humidity: ', BUFFER,
     :     BUFLEN)
         CALL CHR_PUTD (HUMID, BUFFER, BUFLEN)
         CALL CHR_PUTC (' (in the range 0.0 to 1.0).', BUFFER,
     :     BUFLEN)

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Wavelength of observation: ', BUFFER,
     :     BUFLEN)
         CALL CHR_PUTD (WAVE, BUFFER, BUFLEN)
         CALL CHR_PUTC (' Angstrom.', BUFFER,
     :     BUFLEN)

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Tropospheric lapse rate: ', BUFFER,
     :     BUFLEN)
         CALL CHR_PUTD (TROPL, BUFFER, BUFLEN)
         CALL CHR_PUTC (' degrees Kelvin per metre.', BUFFER,
     :     BUFLEN)

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

         CALL CAT_PUTXT (CIOUT, 'COMMENT', ' ', STATUS)

      END IF

      END
