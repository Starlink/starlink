      SUBROUTINE JCMT_OFFSET2RADEC (CENTRE_CRD, EPOCH, RACEN,
     :   DECCEN, LOCAL_CRD, V2Y, X2Y, MJDSTART, LAT, NX, NY,
     :   XOFF, YOFF, LST, B1950, RAOUT, DECOUT, STATUS)
*+
*  Name:
*     JCMT_OFFSET2RADEC

*  Purpose:
*     Produce a list of RA,DEC from a list of XOFF, YOFF, LST

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE JCMT_OFFSET2RADEC (CENTRE_CRD, EPOCH, RACEN,
*    :   DECCEN, LOCAL_CRD, V2Y, X2Y, MJDSTART, LAT, NX, NY,
*    :   XOFF, YOFF, LST, B1950, RAOUT, DECOUT, STATUS)

*  Description:
*   The purpose of the routine is to convert the map centre coords, and the
*   LST, x, y offset of each pixel into an RA,Dec for each pixel.
*
*   The first step is to convert the centre coords into the same system
*   as the local offsets. The exception is for the case where local
*   coordinates are in AZ, in which case the centre coords are just
*   precessed to the current epoch.
*
*   The second step is to cycle through the pixels, calculating the required
*   values:-
*     In each case the offset of the pixel is transformed to be relative to
*     axes parallel to the `local' axes, but with the the x-axis in all
*     cases increasing to the left of the map. The offsets are converted to
*     radians.
*
*     For RA/Dec local systems, the RA, Dec of the pixel is calculated by
*     the SLA_DTP2S routine which converts tangent plane offsets to RA, Dec.
*     For alt-az local offsets, the alt,az of the map centre at this
*     LST is calculated and SLA_DTP2S used to calculate the alt,az of the
*     point, which is then converted back into an RA,Dec (note that for the
*     SLA routine to work properly, the az is calculated increasing in the
*     same direction as RA).
*
*     Finally, the RA, Dec of each point is precessed and/or converted to
*     B1950 FK4 or J2000 FK5 coordinates as required.
*
*  Arguments:
*     CENTRE_CRD               = CHARACTER*(*) (Given)
*        The coordinate system of the telescope map centre, RB or RJ
*     EPOCH                    = DOUBLE PRECISION (Given)
*        The Besselian epoch of the map centre coords
*     RACEN                    = DOUBLE PRECISION (Given)
*        The RA of the map centre (radians)
*     DECCEN                   = DOUBLE PRECISION (Given)
*        The Dec of the map centre (radians)
*     LOCAL_CRD                = CHARACTER*(*) (Given)
*        The coordinate system of the local offsets, RB, RJ, or AZ
*     V2Y                      = REAL (Given)
*        The angle between the `local' vertical and the y offset axis,
*        anti-clockwise (radians)
*     X2Y                      = REAL (Given)
*        The angle between the x offset axis and the y, anticlockwise (radians)
*     MJDSTART                 = DOUBLE PRECISION (Given)
*        The modified Julian day at which the observation occured
*     LAT                      = DOUBLE PRECISION (Given)
*        The mean geodetic latitude of the observer (Radians)
*     NX                       = INTEGER (Given)
*        The number of input map pixels in the X direction
*     NY                       = INTEGER (Given)
*        The number of input map pixels in the Y direction
*     XOFF (NX)                = REAL (Given)
*        The x-offset from the map centre of each pixel, in cell coords
*        multiplied by the cell size
*     YOFF (NX)                = REAL (Given)
*        The y-offset from the map centre of each pixel, in cell coords
*        multiplied by the cell size
*     LST (NX, NY)             = DOUBLE PRECISION (Given)
*        The Local apparent sidereal time of observation of each of the
*        map pixels (radians)
*     B1950                    = LOGICAL (Given)
*        T if want output RAs and Decs in FK4 1950 coords, F if
*        want FK5 2000 coords
*     RAOUT (NX, NY)           = DOUBLE PRECISION (Returned)
*        The mean right acensions of the map pixels
*     DECOUT (NX, NY)          = DOUBLE PRECISION (Returned)
*        The mean declinations of the map pixels
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: John Lightfoot (ROE)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1992 (REVAD::JFL):
*        Original version.
*      7-JUL-1993 (REVAD::JFL): Modified to assume that input RA, Dec of map
*                               centre are always B1950. At release 1.0-2 of
*                               JCMTDR.
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
      DOUBLE PRECISION LST( NX,NY )
      LOGICAL B1950

*  Arguments Returned:
      DOUBLE PRECISION RAOUT( NX,NY )
      DOUBLE PRECISION DECOUT( NX,NY )

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
      DOUBLE PRECISION HA           ! hour angle of pixel
      DOUBLE PRECISION SINHA        ! sin (HA)
      DOUBLE PRECISION COSHA        ! cos (HA)
      DOUBLE PRECISION Z            ! zenith distance
      DOUBLE PRECISION COSZ         ! cos (Z)
      DOUBLE PRECISION ALT          ! altitude of map centre
      DOUBLE PRECISION AZ           ! azimuth of map centre
      DOUBLE PRECISION SINAZ        ! sin (AZ)
      DOUBLE PRECISION COSAZ        ! cos (AZ)
      DOUBLE PRECISION AZOFF        ! azimuth of offset pixel
      DOUBLE PRECISION ALTOFF       ! altitude of offset pixel
      DOUBLE PRECISION SIND         ! sin (declination of offset pixel)
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

*  The map centre coordinates are always B1950. Convert them to the same system
*  as the local offsets. If the local offsets are in AZ then just precess the
*  coordinates to the current epoch.

      IF (LOCAL_CRD .EQ. 'RB') THEN

*  FK4 B1950.0 coords

         RACEN_LOC = RACEN
         DECCEN_LOC = DECCEN
         CALL SLA_PRECES ('FK4', BEPOCH, 1950.0D0, RACEN_LOC,
     :     DECCEN_LOC)

      ELSE IF (LOCAL_CRD .EQ. 'RJ') THEN

*  FK5 J2000.0 coords

         CALL SLA_PRECES ('FK4', BEPOCH, 1950.0D0, RACEN_LOC,
     :     DECCEN_LOC)
         CALL SLA_FK45Z (RACEN_LOC, DECCEN_LOC, 1950.0D0,
     :     RACEN_LOC, DECCEN_LOC)

      ELSE IF (LOCAL_CRD .EQ. 'RD') THEN

*  RA,Dec of date

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
            CALL PAR_WRUSER ('JCMT_OFFSET2RADEC - unable to '//
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

                  CALL SLA_DTP2S (X, Y, RACEN_LOC, DECCEN_LOC,
     :               RAOUT(IX,IY), DECOUT(IX,IY))

*  precess or convert the output coords as required

                  IF (.NOT.B1950) THEN
                     CALL SLA_FK45Z (RAOUT(IX,IY), DECOUT(IX,IY),
     :                  1950.0D0, RAOUT(IX,IY), DECOUT(IX,IY))
                  END IF

               ELSE IF (LOCAL_CRD .EQ. 'RJ') THEN

                  CALL SLA_DTP2S (X, Y, RACEN_LOC, DECCEN_LOC,
     :               RAOUT(IX,IY), DECOUT(IX,IY))

                  IF (B1950) THEN
                     CALL SLA_FK54Z (RAOUT(IX,IY), DECOUT(IX,IY),
     :                  1950.0D0, RAOUT(IX,IY), DECOUT(IX,IY),
     :                  DIGNORE, DIGNORE)
                  END IF

               ELSE IF (LOCAL_CRD .EQ. 'RD') THEN

                  CALL SLA_DTP2S (X, Y, RACEN_LOC, DECCEN_LOC,
     :               RAOUT(IX,IY), DECOUT(IX,IY))

                  IF (B1950) THEN
                     CALL SLA_PRECES ('FK4', BCURRENT, 1950.0D0,
     :                 RAOUT(IX,IY), DECOUT(IX,IY))
                  ELSE
                     CALL SLA_PRECES ('FK4', BCURRENT, 1950.0D0,
     :                 RAOUT(IX,IY), DECOUT(IX,IY))
                     CALL SLA_FK45Z (RAOUT(IX,IY), DECOUT(IX,IY),
     :                  1950.0D0, RAOUT(IX,IY), DECOUT(IX,IY))
                  END IF


               ELSE IF (LOCAL_CRD .EQ. 'AZ') THEN

*  calculate az, el of map centre at this LST, az measured from N to W so
*  that it increases to left of map, like RA, as expected by SLA_DTP2S

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

*  calculate RA and dec of offset

                  Z = DPI/2.0D0 - ALTOFF
                  SIND = COS (Z) * SIN (LAT) + SIN (Z) * COS (LAT) *
     :               COS (AZOFF)
                  DECOUT (IX,IY) = ASIN (SIND)
                  IF ((COS(DECOUT(IX,IY))*COS(LAT)) .NE. 0.0D0) THEN
                     SINHA = SIN (AZOFF) * SIN (Z) / COS (DECOUT(IX,IY))
                     COSHA = (COS(Z) - SIN(DECOUT(IX,IY)) * SIN(LAT)) /
     :                  (COS(DECOUT(IX,IY)) * COS(LAT))
                     HA = ATAN2 (SINHA, COSHA)
                     RAOUT (IX,IY) = LST (IX,IY) - HA
                  ELSE
                     RAOUT (IX,IY) = 0.0D0
                  END IF

*  precess and convert as required

                  IF (B1950) THEN
                     CALL SLA_PRECES ('FK4', BCURRENT, 1950.0D0,
     :                 RAOUT(IX,IY), DECOUT(IX,IY))
                  ELSE
                     CALL SLA_PRECES ('FK4', BCURRENT, 1950.0D0,
     :                 RAOUT(IX,IY), DECOUT(IX,IY))
                     CALL SLA_FK45Z (RAOUT(IX,IY), DECOUT(IX,IY),
     :                 1950.0D0, RAOUT(IX,IY), DECOUT(IX,IY))
                  END IF

               END IF

            END DO
         END DO

      END IF

      END

