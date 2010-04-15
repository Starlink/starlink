      SUBROUTINE SCULIB_CALC_CLOCKERR(N_FITS, FITS, RA_CEN,
     :     LST_REF, CLOCK_ERR, LST_AZEL, STATUS)
*+
*  Name:
*     SCULIB_CALC_CLOCKERR

*  Purpose:
*     Calculate start up time and error in system clock

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_CALC_CLOCKERR( N_FITS, FITS, RA_CEN, LST_REF,
*    :     CLOCK_ERR, LST_AZEL, STATUS )

*  Description:
*     Checks the self-consistency of the FITS headers by calculating
*     the expected LST from the Azimuth and Elevation values stored
*     in the header and comparing this with the supplied reference
*     LST. Returns the difference between the two values and the
*     LST calculated from the headers.

*  Arguments:
*     N_FITS = INTEGER (Given)
*        Number of items in the FITS array
*     FITS() = CHAR*80 (Given)
*        FITS array
*     RA_CEN = DOUBLE (Given)
*        Apparent RA corresponding to the Az/El defined in the FITS header.
*        In radians.
*     LST_REF = DOUBLE (Given)
*        Local sidereal time at the start of data acquisition. This
*        usually comes from the LST_STRT array. In radians.
*     CLOCK_ERR = DOUBLE (Returned)
*        The time difference (in radians) between the times stored
*        in the data file (header items and LST_STRT) and the actual
*        time of the observation derived from the azimuth and elevation
*        of the observed source. The value is the number that must be
*        added to the supplied LST_REF in order to correct it.
*     LST_AZEL = DOUBLE (Returned)
*        Local Sidereal time calculated from the azimuth, elevation
*        and apparent RA/Dec (radians)
*     STATUS = INTEGER (Given & Returned)
*        Global status. An error will be returned if the observation
*        was not done in a tracking frame (eg NA or AZ) since those
*        frames can not be used to determine the time from the telescope
*        az and el.

*  Notes:
*     Requires the following FITS headers to be defined:
*     - LAT-OBS
*     - STRT_ELD
*     - STRT_AZD
*     - CENT_CRD
*
*     The clock error returned must be added to all times read from
*     the data file in order to make those times correct.

*     The centre coordinates of the observation must corresponde to
*     an astronomical frame. AZ can not be used (for obvious reasons).

*  Authors:
*     TIMJ: Tim Jenness (JACH)

*  Copyright:
*     Copyright (C) 2000 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.2  2000/07/11 02:37:59  timj
*     Remove unused variables
*
*     Revision 1.1  2000/06/17 00:54:38  timj
*     First version
*

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER          N_FITS
      CHARACTER *80    FITS ( N_FITS )
      DOUBLE PRECISION RA_CEN
      DOUBLE PRECISION LST_REF

*  Arguments Returned:
      DOUBLE PRECISION CLOCK_ERR
      DOUBLE PRECISION LST_AZEL

*  Status:
      INTEGER          STATUS

*  Local Constants:
      DOUBLE PRECISION DD2R     ! Degrees to radians
      PARAMETER ( DD2R = 0.017453292519943295769236907684886127134429 )
      DOUBLE PRECISION DAS2R    ! Arcseconds to radians
      PARAMETER ( DAS2R= 4.84813681109535993589914102357947975956D-6 )

*  External References:
      DOUBLE PRECISION SLA_DRANGE
      DOUBLE PRECISION SLA_DRANRM

*  Local Variables:
      CHARACTER * 10   CENTRE_COORDS ! Centre coordinate frame
      DOUBLE PRECISION DEC      ! Declination of observation (radians)
      DOUBLE PRECISION HA       ! Hour angle of observation (radians)
      DOUBLE PRECISION LAT_OBS  ! Latitude of observatory (radians)
      DOUBLE PRECISION STRT_DAZ ! Eror in azimuth (converted to radians)
      DOUBLE PRECISION STRT_DEL ! Eror in elevation (converted to radians)
      DOUBLE PRECISION STRT_AZ  ! Azimuth from header (converted to radians)
      DOUBLE PRECISION STRT_EL  ! elevation from header (cvted to radians)

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Read the centre coordinate frame from the header and check that
*     it is a tracking coordinate frame (not AZ or NA)
      CALL SCULIB_GET_FITS_C ( N_FITS, N_FITS, FITS,
     :     'CENT_CRD', CENTRE_COORDS, STATUS)
      CALL CHR_UCASE (CENTRE_COORDS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (CENTRE_COORDS .NE. 'RJ' .AND.
     :        CENTRE_COORDS .NE. 'RB' .AND.
     :        CENTRE_COORDS .NE. 'PLANET' .AND.
     :        CENTRE_COORDS .NE. 'GA') THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('CNT', CENTRE_COORDS)
            CALL ERR_REP( ' ','SCULIB_CALC_CLOCKERR: Can not '//
     :           'calculate clock error when observing with frame ^CNT',
     :           STATUS)
         END IF

      END IF

*     Read the azimuth and elevation from the header
*     and convert to radians

      CALL SCULIB_GET_FITS_D (N_FITS, N_FITS, FITS,
     :     'STRT_AZD', STRT_AZ, STATUS)
      STRT_AZ = STRT_AZ * DD2R

      CALL SCULIB_GET_FITS_D (N_FITS, N_FITS, FITS,
     :     'STRT_ELD', STRT_EL, STATUS)
      STRT_EL = STRT_EL * DD2R

*     Read the error in azimuth and elevation (arcsec, convert to radians)

      CALL SCULIB_GET_FITS_D (N_FITS, N_FITS, FITS,
     :     'AZ_ERR', STRT_DAZ, STATUS)
*      print *,' AZ error = ', STRT_DAZ
      STRT_DAZ = STRT_DAZ * DAS2R

      CALL SCULIB_GET_FITS_D (N_FITS, N_FITS, FITS,
     :     'EL_ERR', STRT_DEL, STATUS)
*      print *,' EL error = ', STRT_DEL
      STRT_DEL = STRT_DEL * DAS2R

*     Correct for the error
      STRT_AZ = STRT_AZ + STRT_DAZ
      STRT_EL = STRT_EL + STRT_DEL

*     Get the latitude of the telescope
      CALL SCULIB_GET_FITS_D (N_FITS, N_FITS, FITS,
     :     'LAT-OBS', LAT_OBS, STATUS)
      LAT_OBS = LAT_OBS * DD2R

      IF (STATUS .EQ. SAI__OK) THEN

*     Convert these values to hour angle
         CALL SLA_DH2E( STRT_AZ, STRT_EL, LAT_OBS, HA, DEC)

*     Add the hour angle to the RA to get the LST
*     and put it into range 0 to 2PI
         LST_AZEL = SLA_DRANRM( HA + RA_CEN )

*     Now calculate the difference between this value and the reference
*     value provided as an argument
*     Have to be careful about day boundaries so put this into
*     range -PI to PI

         CLOCK_ERR = SLA_DRANGE( LST_AZEL - LST_REF )

*         print *,' HA:', HA, RA_CEN, STRT_AZ, STRT_EL, DEC
*         print *,'LST:', LST_AZEL, LST_REF

      END IF

      END
