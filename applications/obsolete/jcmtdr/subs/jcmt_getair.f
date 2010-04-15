      SUBROUTINE JCMT_GET_AIRMASS (CENTRE_CRD, EPOCH, RACEN,
     :   DECCEN, LOCAL_CRD, V2Y, X2Y, MJDSTART, LAT, NX, NY,
     :   XOFF, YOFF, LST, FBAD, AIRMASS, STATUS)
*+
*  Name:
*     JCMT_GET_AIRMASS

*  Purpose:
*     Produce a list of AIRMASS from a list of XOFF, YOFF, LST

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE JCMT_GET_AIRMASS (CENTRE_CRD, EPOCH, RACEN,
*    :   DECCEN, LOCAL_CRD, V2Y, X2Y, MJDSTART, LAT, NX, NY,
*    :   XOFF, YOFF, LST, FBAD, AIRMASS, STATUS)

*  Description:
*   The purpose of the routine is to convert the map centre coords, and the
*   LST, x, y offset of each pixel into an airmass for each pixel.
*
*   The first step is to convert the centre coords into the same system
*   as the local offsets. The exception is for the case where local coordinates
*   are in AZ, in this case the centre coords are just precessed to the
*   current epoch.
*
*   The second step is to cycle through the pixels, calculating the required
*   values:-
*     In each case the offset of the pixel is transformed to be relative to
*     axes parallel to the telescope `local' axes, but with the the x-axis
*     in all cases increasing to the left of the map. The offsets are
*     converted to radians.
*
*     For RA/Dec local systems, the RA, Dec of the pixel is calculated by
*     the SLA_DTP2S routine and these values precessed to the epoch of the
*     observation. These are then used to calculate the zenith
*     distance of the pixel.
*
*     For alt-az local offsets, the alt,az of the map centre at this
*     LST is calculated and SLA_DTP2S used to calculate the alt,az of the
*     point, from which the zenith distance is derived.
*
*     Finally, the airmass is calculated from the zenith distance. Sec (z) is
*     used up to an airmass of 2 , beyond that JCMT_HIGH_AIRMASS is
*     called.

*  Arguments:
*     CENTRE_CRD               = CHARACTER*(*) (Given)
*        The coordinate system of the telescope map centre, RB, RD, or RJ
*     EPOCH                    = DOUBLE PRECISION (Given)
*        The Besselian epoch of the map centre coords
*     RACEN                    = DOUBLE PRECISION (Given)
*        The RA (Besselian, equinox=1950) of the map centre (radians)
*     DECCEN                   = DOUBLE PRECISION (Given)
*        The Dec (Besselian, equinox=1950) of the map centre (radians)
*     LOCAL_CRD                = CHARACTER*(*) (Given)
*        The coordinate system of the local offsets, RB, RJ, or AZ
*     V2Y                      = REAL (Given)
*        The angle between the `local' vertical and the y offset axis,
*        anti-clockwise (radians)
*     X2Y                      = REAL (Given)
*        The angle between the x and y offset axes, anti-clockwise (radians)
*     MJDSTART                 = DOUBLE PRECISION (Given)
*        The modified Julian day at which the observation occured
*     LAT                      = DOUBLE PRECISION (Given)
*        The mean geodetic latitude of the observer (Radians)
*     NX                       = INTEGER (Given)
*        The number of input map pixels in the X direction
*     NY                       = INTEGER (Given)
*        The number of input map pixels in the Y direction
*     XOFF (NX)                = REAL (Given)
*        The x-offset from the map centre of each pixel
*     YOFF (NX)                = REAL (Given)
*        The y-offset from the map centre of each pixel
*     LST (NX, NY)             = DOUBLE PRECISION (Given)
*        The Local apparent sidereal time of observation of each of the
*        map pixels (radians)
*     FBAD                     = REAL (Given)
*        Bad value to which airmass will be set if the zenith distance of
*        the observation exceeds 90 degrees
*     AIRMASS (NX, NY)         = REAL (Returned)
*        The airmass of the map pixels
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: John Lightfoot (ROE)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1992 (REVAD::JFL): Original version.
*      5-JUL-1993 (REVAD::JFL): Modified to assume that input RA, Decs are
*                               always B1950. Release 1.0-2 of JCMTDR
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  External functions:
      INTEGER ICH_LEN            ! Figaro string length function
      DOUBLE PRECISION SLA_EPB   ! MJD to Besselian epoch
      DOUBLE PRECISION SLA_EPJ   ! MJF to Julian epoch
      DOUBLE PRECISION SLA_EPB2D ! Besselian epoch to MJD

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'        ! standard astronomical constants

*  Arguments Given:
      CHARACTER*(*) CENTRE_CRD
      DOUBLE PRECISION EPOCH
      DOUBLE PRECISION RACEN
      DOUBLE PRECISION DECCEN
      CHARACTER*(*) LOCAL_CRD
      REAL V2Y
      REAL X2Y
      DOUBLE PRECISION MJDSTART
      DOUBLE PRECISION LAT
      INTEGER NX
      INTEGER NY
      REAL XOFF (NX)
      REAL YOFF (NY)
      DOUBLE PRECISION LST (NX,NY)
      REAL FBAD

*  Arguments Returned:
      REAL AIRMASS (NX, NY)

*  Status:
      INTEGER STATUS                ! Global status

*  Local Variables:
      DOUBLE PRECISION RACEN_LOC    ! RA of date of map centre
      DOUBLE PRECISION DECCEN_LOC   ! Dec of date of map centre
      DOUBLE PRECISION DIGNORE      ! dummy argument
      DOUBLE PRECISION BCURRENT     ! Besselian epoch of observation
      DOUBLE PRECISION JCURRENT     ! Julian epoch of observation
      DOUBLE PRECISION BEPOCH       ! Besselian epoch of map centre coords
      DOUBLE PRECISION MJDTEMP      !
      DOUBLE PRECISION JEPOCH       ! Julian epoch of map centre coords
      DOUBLE PRECISION X, Y         ! x, y offset of pixel in local coords
      DOUBLE PRECISION RAPOINT      ! current RA of pixel
      DOUBLE PRECISION DECPOINT     ! current Dec of pixel
      DOUBLE PRECISION HA           ! hour angle of pixel
      DOUBLE PRECISION SINHA        ! sin (HA)
      DOUBLE PRECISION Z            ! zenith distance
      DOUBLE PRECISION COSZ         ! cos (Z)
      DOUBLE PRECISION SECZ         ! sec (Z)
      DOUBLE PRECISION ALT          ! altitude of map centre
      DOUBLE PRECISION AZ           ! azimuth of map centre
      DOUBLE PRECISION SINAZ        ! sin (AZ)
      DOUBLE PRECISION COSAZ        ! cos (AZ)
      DOUBLE PRECISION AZOFF        ! azimuth of offset pixel
      DOUBLE PRECISION ALTOFF       ! altitude of offset pixel
      DOUBLE PRECISION DAIRMASS     ! airmass
      INTEGER IX                    ! current pixel x index
      INTEGER IY                    ! current pixel y index
      INTEGER IGNORE                !

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

*  make sure CENTRE_CRD and LOCAL_CRD don't have any pesky trailing chars

      IF (ICH_LEN(CENTRE_CRD) .NE. 0) THEN
         CENTRE_CRD = CENTRE_CRD (:ICH_LEN(CENTRE_CRD))
      ELSE
         CENTRE_CRD = ' '
      END IF
      IF (ICH_LEN(LOCAL_CRD) .NE. 0) THEN
         LOCAL_CRD = LOCAL_CRD (:ICH_LEN(CENTRE_CRD))
      ELSE
         LOCAL_CRD = ' '
      END IF

*  calculate the current epoch, and the epoch of the centre coords in
*  Julian and Besselian form

      BCURRENT = SLA_EPB (MJDSTART)
      JCURRENT = SLA_EPJ (MJDSTART)
      BEPOCH = EPOCH
      MJDTEMP = SLA_EPB2D (BEPOCH)
      JEPOCH = SLA_EPJ (MJDTEMP)

*  The map centre coordinates are always B1950 coordinates. Convert them
*  to the same system as the local offsets. If the local offsets are in AZ
*  then precess the centre coordinates to the epoch of the observation.

      IF (LOCAL_CRD .EQ. 'RB') THEN

*  FK4 B1950.0 coords

         RACEN_LOC = RACEN
         DECCEN_LOC = DECCEN

      ELSE IF (LOCAL_CRD .EQ. 'RJ') THEN

*  FK5 J2000.0 coords

         RACEN_LOC = RACEN
         DECCEN_LOC = DECCEN
         CALL SLA_PRECES ('FK4', BEPOCH, 1950.0D0, RACEN_LOC,
     :     DECCEN_LOC)
         CALL SLA_FK45Z (RACEN_LOC, DECCEN_LOC, 1950.0D0,
     :     RACEN_LOC, DECCEN_LOC)

      ELSE IF (LOCAL_CRD .EQ. 'RD') THEN

*  RA, Dec of date

         RACEN_LOC = RACEN
         DECCEN_LOC = DECCEN
         CALL SLA_PRECES ('FK4', BEPOCH, BCURRENT, RACEN_LOC,
     :     DECCEN_LOC)

      ELSE IF (LOCAL_CRD .EQ. 'AZ') THEN

*  local azimuth

         RACEN_LOC = RACEN
         DECCEN_LOC = DECCEN
         CALL SLA_PRECES ('FK4', BEPOCH, BCURRENT, RACEN_LOC,
     :     DECCEN_LOC)

      ELSE

         IF (STATUS .EQ. SAI__OK) THEN
            IGNORE = 0
            CALL PAR_WRUSER ('JCMT_GET_AIRMASS - unable to '//
     :         'deal with this local coordinate system', IGNORE)
            STATUS = SAI__ERROR
         END IF

      END IF



      IF (STATUS .EQ. SAI__OK) THEN

*  loop over the map pixels

         DO IY = 1, NY
            DO IX = 1, NX

*  the offsets of the pixels are measured along axes whose orientation
*  relative to local north are described by the variables V2Y (angle between
*  north and y-axis, anti-clockwise) and X2Y (angle between x and y-axes
*  measured anticlockwise). Transform the pixel offsets to the coordinate
*  system where y is local north and x lies at right angles to it, increasing
*  to the left.
*                                i y
*                                i
*                                i
*                                i
*                      x---------O


               X = XOFF(IX) * SIN(V2Y-X2Y) + YOFF(IY) * SIN(V2Y)
               Y = XOFF(IX) * COS(V2Y-X2Y) + YOFF(IY) * COS(V2Y)
               X = X * DAS2R
               Y = Y * DAS2R

               IF (LOCAL_CRD.EQ.'RB') THEN

*  use tangent plane equations to calculate RA, Dec of point
*  precess this to current epoch, calculate z

                  CALL SLA_DTP2S (X, Y, RACEN_LOC, DECCEN_LOC,
     :               RAPOINT, DECPOINT)
                  CALL SLA_PRECES ('FK4', 1950.0D0, BCURRENT,
     :               RAPOINT, DECPOINT)
                  HA = LST (IX,IY) - RAPOINT
                  COSZ = SIN(DECPOINT) * SIN(LAT) +
     :               COS(DECPOINT) * COS (LAT) * COS (HA)
                  Z = ACOS (COSZ)

               ELSE IF (LOCAL_CRD .EQ. 'RJ') THEN

                  CALL SLA_DTP2S (X, Y, RACEN_LOC, DECCEN_LOC,
     :               RAPOINT, DECPOINT)
                  CALL SLA_PRECES ('FK5', 2000.0D0, JCURRENT,
     :               RAPOINT, DECPOINT)
                  HA = LST (IX,IY) - RAPOINT
                  COSZ = SIN(DECPOINT) * SIN(LAT) +
     :               COS(DECPOINT) * COS (LAT) * COS (HA)
                  Z = ACOS (COSZ)

               ELSE IF (LOCAL_CRD .EQ. 'RD') THEN

                  CALL SLA_DTP2S (X, Y, RACEN_LOC, DECCEN_LOC,
     :               RAPOINT, DECPOINT)
                  HA = LST (IX,IY) - RAPOINT
                  COSZ = SIN(DECPOINT) * SIN(LAT) +
     :               COS(DECPOINT) * COS (LAT) * COS (HA)
                  Z = ACOS (COSZ)

               ELSE IF (LOCAL_CRD .EQ. 'AZ') THEN

*  calculate az, el of map centre at this LST, az measured from N increasing
*  to east

                  HA = LST (IX,IY) - RACEN_LOC
                  COSZ = SIN(DECCEN_LOC) * SIN(LAT) +
     :               COS(DECCEN_LOC) * COS (LAT) * COS (HA)
                  Z = ACOS (COSZ)
                  ALT = DPI/2.0D0 - Z
                  IF ((SIN(Z)*COS(LAT)) .GT. 0.0D0) THEN
                     SINAZ = SIN (HA) * COS (DECCEN_LOC) / SIN (Z)
                     COSAZ = (SIN(DECCEN_LOC) - COS(Z) * SIN(LAT)) /
     :                  (SIN(Z) * COS(LAT))
                     AZ = ATAN2 (SINAZ, COSAZ)
                  ELSE
                     AZ = 0.0D0
                  END IF

*  use tangent plane coords to calculate az, el of offset

                  CALL SLA_DTP2S (X, Y, AZ, ALT, AZOFF, ALTOFF)
                  Z = DPI/2.0D0 - ALTOFF

               END IF

*  calculate airmass from z

               IF (COS(Z) .GT. 0.0D0) THEN

                  AIRMASS (IX,IY) = 1.0 / COS(Z)

*  if at large z do a correction to the airmass

                  IF (AIRMASS(IX,IY) .GT. 2.0) THEN
                     SECZ = DBLE(AIRMASS(IX,IY))
                     CALL JCMT_HIGH_AIRMASS (SECZ, 1, DAIRMASS)
                     AIRMASS (IX,IY) = REAL (DAIRMASS)
                  END IF

               ELSE

                  AIRMASS (IX,IY) = FBAD

               END IF

            END DO
         END DO

      END IF

      END

