      SUBROUTINE JCMT_BEAM2RADEC (CENTRE_CRD, EPOCH, RACEN,
     :   DECCEN, LOCAL_CRD, V2Y, X2Y, MJDSTART, LAT, NX, NY,
     :   XOFF, YOFF, LST, CHOP_CRD, CHOP_X, CHOP_Y, RA, DEC,
     :   COORD_SYSTEM, STATUS)
*+
*  Name:
*     JCMT_BEAM2RADEC

*  Purpose:
*     Produce a list of RA,DEC from a list of XOFF, YOFF, LST, CHOP_X,
*     CHOP_Y etc.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_BEAM2RADEC (CENTRE_CRD, EPOCH, RACEN,
*    :   DECCEN, LOCAL_CRD, V2Y, X2Y, MJDSTART, LAT, NX, NY,
*    :   XOFF, YOFF, LST, CHOP_CRD, CHOP_X, CHOP_Y, RA, DEC,
*    :   COORD_SYSTEM, STATUS)

*  Description:
*   The purpose of the routine is to calculate the apparent RA,Dec of one
*   of the beam positions associated with each pixel.
*
*   The first step is to convert the centre coords into the same system
*   as the local offsets. If the local coordinates are in AZ, then
*   the centre coords are just precessed to the current epoch.
*
*   The second step is to cycle through the pixels, calculating the required
*   values:-
*     In each case the offset of the pixel is transformed to be relative to
*     axes parallel to the `local' axes, but with the the x-axis in all
*     cases increasing to the left of the map. The offsets are converted to
*     radians.
*
*     For RA/Dec local systems:-
*       If the chopper coordinate system is the same as the local, then
*       the beam offset is added to the pixel offset and the RA, dec of the
*       beam calculated by the SLA_DTP2S routine which converts tangent plane
*       offsets to RA, Dec. The RA, dec is precessed to the epoch of the
*       observation.
*       If the chopper coord system is in AZ then the RA, dec of the pixel
*       offset are calculated and precessed to the epoch of the observation.
*       The az,alt of the pixel are calculated and the az,alt of the beam
*       position calculated by another call to SLA_DTP2S. Finally the
*       az,alt of the beam are transformed back to RA, dec.
*
*     For alt-az local systems:-
*       The alt,az of the map centre at this LST is calculated and SLA_DTP2S
*       used to calculate the alt,az of the beam. This is then converted back
*       into an RA,Dec.
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
*        multiplied by the cell size (arcsec)
*     YOFF (NX)                = REAL (Given)
*        The y-offset from the map centre of each pixel, in cell coords
*        multiplied by the cell size (arcsec)
*     LST (NX, NY)             = DOUBLE PRECISION (Given)
*        The Local apparent sidereal time of observation of each of the
*        map pixels (radians)
*     CHOP_CRD                 = CHARACTER*(*) (Given)
*        The coordinate system used by the chopper (LO or AZ)
*     CHOP_X                   = REAL (Given)
*        The x-offset of the beam position from the nominal telescope position
*        (arcsec)
*     CHOP_Y                   = REAL (Given)
*        The corresponding y-offset (arcsec)

*     RA (NX, NY)              = DOUBLE PRECISION (Returned)
*        The RA at the epoch of observation of the beam position for each pixel
*     DEC (NX, NY)             = DOUBLE PRECISION (Returned)
*        The dec.
*     COORD_SYSTEM             = CHARACTER*(*) (Returned)
*        The coordinate system of the output RA, dec, FK4 or FK5
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: John Lightfoot (ROE)
*     {enter_new_authors_here}

*  History:
*     1-JUN-1992: Original version.
*     7-JUL-1993: Modified to expect map centre coords always in B1950
*                 (1.0-2 of JCMTDR)
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
      DOUBLE PRECISION LST(NX,NY)
      CHARACTER*(*) CHOP_CRD
      REAL CHOP_X
      REAL CHOP_Y

*  Arguments Returned:
      DOUBLE PRECISION RA (NX,NY)
      DOUBLE PRECISION DEC (NX,NY)
      CHARACTER*(*) COORD_SYSTEM

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
      DOUBLE PRECISION RA_TEL       ! RA of telescope beam position
      DOUBLE PRECISION DEC_TEL      ! Dec of telescope beam position
      DOUBLE PRECISION AZ_BEAM      ! azimuth of beam
      DOUBLE PRECISION ALT_BEAM     ! altitude of beam
      DOUBLE PRECISION SIND         ! sin (declination of offset pixel)
      INTEGER IX                    ! current pixel x index
      INTEGER IY                    ! current pixel y index
      INTEGER IGNORE                !

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

*  calculate the current epoch, and the epoch of the centre coords in
*  Julian and Besselian form

      BCURRENT = SLA_EPB (MJDSTART)
      JCURRENT = SLA_EPJ (MJDSTART)
      BEPOCH = EPOCH
      MJDTEMP = SLA_EPB2D (BEPOCH)
      JEPOCH = SLA_EPJ (MJDTEMP)

*  The map centre coords are always B1950. Convert them to the same system
*  as the local offsets. If the local offsets are in AZ then just precess
*  the coordinates to the current epoch.

      IF (LOCAL_CRD .EQ. 'RB') THEN

*  FK4 B1950.0 coords

         RACEN_LOC = RACEN
         DECCEN_LOC = DECCEN
         CALL SLA_PRECES ('FK4', BEPOCH, 1950.0D0, RACEN_LOC,
     :     DECCEN_LOC)

      ELSE IF (LOCAL_CRD .EQ. 'RJ') THEN

*  FK5 J2000.0 coords

         CALL SLA_PRECES ('FK4', BEPOCH, 1950.0D0, RACEN_LOC,
     :      DECCEN_LOC)
         CALL SLA_FK45Z (RACEN_LOC, DECCEN_LOC, 1950.0D0,
     :      RACEN_LOC, DECCEN_LOC)

      ELSE IF (LOCAL_CRD .EQ. 'RD') THEN

*  RA, dec of date

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

      END IF


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

               COORD_SYSTEM = 'FK4'
               IF (CHOP_CRD .EQ. 'LO') THEN

*  add beam offsets to telescope offset (remembering that chop offsets have
*  x increasing to right, while local offsets increase to left). Then
*  use tangent plane equations to calculate RA, Dec of beam positions

                  X = X - CHOP_X * DAS2R
                  Y = Y + CHOP_Y * DAS2R
                  CALL SLA_DTP2S (X, Y, RACEN_LOC,
     :               DECCEN_LOC, RA(IX,IY), DEC(IX,IY))

*  precess to current epoch

                  CALL SLA_PRECES ('FK4', 1950.0D0, BCURRENT,
     :               RA(IX,IY), DEC(IX,IY))

               ELSE IF (CHOP_CRD .EQ. 'AZ') THEN

*  calculate RA, Dec of telescope position and precess it to the observation
*  epoch

                  CALL SLA_DTP2S (X, Y, RACEN_LOC, DECCEN_LOC,
     :               RA_TEL, DEC_TEL)
                  CALL SLA_PRECES ('FK4', 1950.0D0, BCURRENT, RA_TEL,
     :               DEC_TEL)

*  now calculate az, el of telescope at this LST, az measured from N to
*  W (increasing to left) as expected by SLA_DTP2S

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

*  now calculate the apparent az, el of the beam

                  CALL SLA_DTP2S (-CHOP_X*DAS2R, CHOP_Y*DAS2R,
     :               AZ, ALT, AZ_BEAM, ALT_BEAM)

*  convert this back to RA, dec

                  Z = DPI/2.0D0 - ALT_BEAM
                  SIND = COS (Z) * SIN (LAT) + SIN (Z) * COS (LAT) *
     :               COS (AZ_BEAM)
                  DEC (IX,IY) = ASIN (SIND)
                  IF ((COS(DEC(IX,IY))*COS(LAT)) .NE. 0.0D0) THEN
                     SINHA = SIN(AZ_BEAM) * SIN(Z) / COS(DEC(IX,IY))
                     COSHA = (COS(Z) - SIN(DEC(IX,IY)) * SIN(LAT)) /
     :                  (COS(DEC(IX,IY)) * COS(LAT))
                     HA = ATAN2 (SINHA, COSHA)
                     RA (IX,IY) = LST (IX,IY) - HA
                  ELSE
                     RA (IX,IY) = 0.0D0
                  END IF
               END IF


            ELSE IF (LOCAL_CRD .EQ. 'RJ') THEN

               COORD_SYSTEM = 'FK5'
               IF (CHOP_CRD .EQ. 'LO') THEN

                  X = X - CHOP_X * DAS2R
                  Y = Y + CHOP_Y * DAS2R
                  CALL SLA_DTP2S (X, Y, RACEN_LOC,
     :               DECCEN_LOC, RA(IX,IY), DEC(IX,IY))
                  CALL SLA_PRECES ('FK5', 2000.0D0, JCURRENT,
     :               RA(IX,IY), DEC(IX,IY))

               ELSE IF (CHOP_CRD .EQ. 'AZ') THEN

*  similar process to RB case

                  CALL SLA_DTP2S (X, Y, RACEN_LOC, DECCEN_LOC,
     :               RA_TEL, DEC_TEL)
                  CALL SLA_PRECES ('FK5', 2000.0D0, JCURRENT, RA_TEL,
     :               DEC_TEL)

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

*  now calculate the apparent az, el of the beam

                  CALL SLA_DTP2S (-CHOP_X*DAS2R, CHOP_Y*DAS2R,
     :               AZ, ALT, AZ_BEAM, ALT_BEAM)

*  convert this back to RA, dec

                  Z = DPI/2.0D0 - ALT_BEAM
                  SIND = COS (Z) * SIN (LAT) + SIN (Z) * COS (LAT) *
     :               COS (AZ_BEAM)
                  DEC (IX,IY) = ASIN (SIND)
                  IF ((COS(DEC(IX,IY))*COS(LAT)) .NE. 0.0D0) THEN
                     SINHA = SIN(AZ_BEAM) * SIN(Z) / COS(DEC(IX,IY))
                     COSHA = (COS(Z) - SIN(DEC(IX,IY)) * SIN(LAT)) /
     :                  (COS(DEC(IX,IY)) * COS(LAT))
                     HA = ATAN2 (SINHA, COSHA)
                     RA (IX,IY) = LST (IX,IY) - HA
                  ELSE
                     RA (IX,IY) = 0.0D0
                  END IF
               END IF


            ELSE IF (LOCAL_CRD .EQ. 'AZ') THEN

               IF (CENTRE_CRD .EQ. 'RB') THEN
                  COORD_SYSTEM = 'FK4'
               ELSE
                  COORD_SYSTEM = 'FK5'
               END IF

*  calculate az, el of map centre at this LST, az measured from N to W so
*  that it increases to left of map, like RA, as expected by SLA_DTP2S

               HA = LST (IX,IY) - RACEN_LOC
               COSZ = SIN(DECCEN_LOC) * SIN(LAT) +
     :            COS(DECCEN_LOC) * COS (LAT) * COS (HA)
               Z = ACOS (COSZ)
               ALT = DPI/2.0D0 - Z
               IF ((SIN(Z)*COS(LAT)) .GT. 0.0D0) THEN
                  SINAZ = SIN (HA) * COS (DECCEN_LOC) / SIN (Z)
                  COSAZ = (SIN(DECCEN_LOC) - COS(Z) * SIN(LAT)) /
     :               (SIN(Z) * COS(LAT))
                  AZ = ATAN2 (SINAZ, COSAZ)
               ELSE
                  AZ = 0.0D0
               END IF

*  use tangent plane coords to calculate az, el of beam (remembering
*  that x-axis of chop offset coords is reversed compared to azimuth)


               X = X - CHOP_X * DAS2R
               Y = Y + CHOP_Y * DAS2R
               CALL SLA_DTP2S (X, Y, AZ, ALT, AZ_BEAM, ALT_BEAM)

*  calculate RA and dec of offsets

               Z = DPI/2.0D0 - ALT_BEAM
               SIND = COS (Z) * SIN (LAT) + SIN (Z) * COS (LAT) *
     :            COS (AZ_BEAM)
               DEC (IX,IY) = ASIN (SIND)
               IF ((COS(DEC(IX,IY))*COS(LAT)) .NE. 0.0D0) THEN
                  SINHA = SIN(AZ_BEAM) * SIN(Z) / COS(DEC(IX,IY))
                  COSHA = (COS(Z) - SIN(DEC(IX,IY)) * SIN(LAT)) /
     :               (COS(DEC(IX,IY)) * COS(LAT))
                  HA = ATAN2 (SINHA, COSHA)
                  RA (IX,IY) = LST (IX,IY) - HA
               ELSE
                  RA (IX,IY) = 0.0D0
               END IF

            ELSE

               IF (STATUS .EQ. SAI__OK) THEN
                  IGNORE = 0
                  CALL PAR_WRUSER ('JCMT_BEAM2RADEC - unable to deal '//
     :              'with this local coordinate system', IGNORE)
                  STATUS = SAI__ERROR
               END IF

            END IF

         END DO
      END DO

      END
