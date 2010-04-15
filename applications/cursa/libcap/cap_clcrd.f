      SUBROUTINE CAP_CLCRD (FULL, CRDTYP, MCRDCL, CRDI,
     :  EQINT, EQIN, EPINT, EPIN, EQOUTT, EQOUT, EPOUTT, EPOUT, MO,
     :  NWLNG, NWLNGN, NWLAT, NWLATN, STATUS)
*+
*  Name:
*     CAP_CLCRD
*  Purpose:
*     Compute new celestial coords. for the current row.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CLCRD (FULL, CRDTYP, MCRDCL, CRDI,
*       EQINT, EQIN, EPINT, EPIN, EQOUTT, EQOUT, EPOUTT, EPOUT MO;
*       NWLNG, NWLNGN, NWLAT, NWLATN; STATUS)
*  Description:
*     Compute new celestial coordinates for the current row of a
*     catalogue.
*  Arguments:
*     FULL  =  LOGICAL (Given)
*        Flag; are full input coordinates to be used?  Coded as follows:
*        .TRUE.  -  Full input coordinates are to be used,
*        .FALSE. -  Only use input Right Ascension and Declination.
*     CRDTYP  =  INTEGER (Given)
*        Code indicating the type of coordinates which are to be
*        calculated.
*     MCRDCL  =  INTEGER (Given)
*        Number of parameters definined a full set of coordinates (=6).
*     CRDI(MCRDCL)  =  INTEGER (Given)
*        Identifiers for the columns containing the coordinates in the
*        input catalogue.
*     EQINT  =  CHARACTER*1 (Given)
*        Time system for input equinox.
*     EQIN  =  DOUBLE PRECISION (Given)
*        Input equinox.
*     EPINT  =  CHARACTER*1 (Given)
*        Time system for input epoch.
*     EPIN  =  DOUBLE PRECISION (Given)
*        Input epoch.
*     EQOUTT  =  CHARACTER*1 (Given)
*        Time system for output equinox.
*     EQOUT  =  DOUBLE PRECISION (Given)
*        Output equinox.
*     EPOUTT  =  CHARACTER*1 (Given)
*        Time system of output epoch.
*     EPOUT  =  DOUBLE PRECISION (Given)
*        Output epoch.
*     MO(35)  =  DOUBLE PRECISION (Given)
*        Array for converting mean to observed coordinates.
*     NWLNG  =  DOUBLE PRECISION (Returned)
*        New Right Ascension or Galactic longitude (radians).
*     NWLNGN  =  LOGICAL (Returned)
*        Null value flag for NWLNG.
*     NWLAT  =  DOUBLE PRECISION (Returned)
*        New Declination or Galactic latitude (radians).
*     NWLATN  =  LOGICAL (Returned)
*        Null value flag for NWLAT.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Get the values for the input Right Ascension and Declination.
*     If neither value is null then
*       Initialise the remaining celestial coordinate quatities to
*       zero.
*       If full coordinates are being used then
*         For each remaining celestial coordinate quatity
*           If the coordinate's identifier is not null then
*             Attempt to get a value for the quantity
*             If the value is not null then
*               Set then value for the quantity.
*             end if
*           end if
*         end for
*       end if
*       Compute the appropriate type of coordinates.
*     else
*       Set the return values to null.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     29/5/97 (ACD): Original version.
*     12/6/97 (ACD): First stable version.
*     21/5/98 (ACD): Modified for additional types of coordinates:
*        observed equatorial, local equatorial, horizon, and
*        supergalactic.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.
      INCLUDE 'CAP_PAR'     ! CAP symbolic constants.
*  Arguments Given:
      LOGICAL
     :  FULL
      INTEGER
     :  CRDTYP,
     :  MCRDCL,
     :  CRDI(MCRDCL)
      CHARACTER
     :  EQINT*1,
     :  EPINT*1,
     :  EQOUTT*1,
     :  EPOUTT*1
      DOUBLE PRECISION
     :  EQIN,
     :  EPIN,
     :  EQOUT,
     :  EPOUT,
     :  MO(35)
*  Arguments Returned:
      DOUBLE PRECISION
     :  NWLNG,
     :  NWLAT
      LOGICAL
     :  NWLNGN,
     :  NWLATN
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Variables:
      DOUBLE PRECISION
     :  RAIN,      ! Input Right Ascension.
     :  DEIN,      !   "   Declination.
     :  CRDVAL(6), ! Full input coordinates.
     :  VALUE,     ! Value for current coordinate.
     :  RA2000,    ! R.A. in J2000 (for conversion to Galactic coords).
     :  DE2000,    ! Dec. "    "   ( "      "      "     "       "   ).
     :  RAWORK,    ! Work variable for current R.A.
     :  DEWORK     !  "      "      "     "    Dec.
      DOUBLE PRECISION
     :  RAOBS,     ! Observed Right Ascension.
     :  DEOBS,     !    "     Declination.
     :  HAOBS,     !    "     Hour Angle.
     :  AZOBS,     !    "     Azimuth.
     :  ZDOBS,     !    "     Zenith Distance.
     :  L,         ! Galactic longitude.
     :  B          !    "     latitude.
      LOGICAL
     :  RAINNL,    ! Null value flag for RAIN.
     :  DEINNL,    !  "     "    "    "  DEIN.
     :  NULFLG     !  "     "    "    "  VALUE.
      INTEGER
     :  LOOP,      ! Loop index.
     :  PROPRF     ! Integer flag; proper motion available?
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Get the values for the input Right Ascension and Declination
*       and proceed if neither value is null.

         CALL CAT_EGT0D (CRDI(1), RAIN, RAINNL, STATUS)
         CALL CAT_EGT0D (CRDI(2), DEIN, DEINNL, STATUS)

         IF (.NOT. RAINNL  .AND.  .NOT. DEINNL) THEN
            CRDVAL(1) = RAIN
            CRDVAL(2) = DEIN

*
*          Initialise the remaining celestial coordinate quantities to
*          zero.

            DO LOOP = 3, 6
               CRDVAL(LOOP) = 0.0D0
            END DO

*
*          If full coordinates are being used then attempt to get a
*          value for each quantity.  Each quantity is also checked
*          to see whether a valid value has been returned.

            IF (FULL) THEN
               DO LOOP = 3, 6
                  IF (CRDI(LOOP) .NE. CAT__NOID) THEN
                     CALL CAT_EGT0D (CRDI(LOOP), VALUE, NULFLG, STATUS)

                     IF (.NOT. NULFLG) THEN
                        CRDVAL(LOOP) = VALUE
                     END IF
                  END IF
               END DO
            END IF

*
*          Compute the appropriate type of coordinates.

            IF (FULL) THEN
               PROPRF = 0
            ELSE
               PROPRF = 1
            END IF

            IF (CRDTYP .EQ. CAP__CDEQM) THEN

*             ... mean equatorial coordinates.

C              print3000, crdval(1), crdval(2),
C    :           crdval(3), crdval(4), crdval(5), crdval(6), proprf,
C    :           eqint, eqin, epint, epin,
C    :           eqoutt, eqout, epoutt, epout
C3000          format(1x, 'crdval: ',
C    :           0PD12.4, 0PD12.4, 0PD12.4, 0PD12.4, 0PD12.4, 0PD12.4 /
C    :           1x, 'proprf: ', i6 /
C    :           1x, 'eqint, eqin: ', a, 1x, F8.1 /
C    :           1x, 'epint, epin: ', a, 1x, F8.1 /
C    :           1x, 'eqoutt, eqout: ', a, 1x, F8.1 /
C    :           1x, 'epoutt, epout: ', a, 1x, F8.1 / )

               CALL CAP_CCTMM (CRDVAL(1), CRDVAL(2), PROPRF,
     :           CRDVAL(3), CRDVAL(4), CRDVAL(5), CRDVAL(6),
     :           EQINT, EQIN, EPINT, EPIN,
     :           EQOUTT, EQOUT, EPOUTT, EPOUT,
     :           NWLNG, NWLAT, STATUS)

            ELSE IF (CRDTYP .EQ. CAP__CDEQO  .OR.
     :               CRDTYP .EQ. CAP__CDEQL  .OR.
     :               CRDTYP .EQ. CAP__CDHOR) THEN

*             ... observed equatorial coordinates, local equatorial
*                 coordinates or horizon coordinates.
*
*             ... First convert to mean J2000 coordinates for the required
*                 epoch,

               CALL CAP_CCTMM (CRDVAL(1), CRDVAL(2), PROPRF,
     :           CRDVAL(3), CRDVAL(4), CRDVAL(5), CRDVAL(6),
     :           EQINT, EQIN, EPINT, EPIN,
     :           'J', 2.0D3, EPOUTT, EPOUT,
     :           RA2000, DE2000,  STATUS)

*
*             ... J2000 to apparent,

               CALL SLA_MAPQK (RA2000, DE2000, CRDVAL(3), CRDVAL(4),
     :           CRDVAL(5), CRDVAL(6), MO, RAWORK, DEWORK)

*
*             ... Apparent to observed.

               CALL SLA_AOPQK (RAWORK, DEWORK, MO(22),
     :           AZOBS, ZDOBS, HAOBS, DEOBS, RAOBS)

               IF (CRDTYP .EQ. CAP__CDEQO) THEN
                  NWLNG = RAOBS
                  NWLAT = DEOBS

               ELSE IF (CRDTYP .EQ. CAP__CDEQL) THEN
                  NWLNG = HAOBS
                  NWLAT = DEOBS

               ELSE IF (CRDTYP .EQ. CAP__CDHOR) THEN
                  NWLNG = AZOBS
                  NWLAT = ZDOBS

               END IF

            ELSE IF (CRDTYP .EQ. CAP__CDGAL  .OR.
     :               CRDTYP .EQ. CAP__CDSPG) THEN

*             ... Galactic of supergalactic coordinates.
*
*             First convert to J2000 equinox equatorial coordinates
*             and then convert to Galactic coordinates.  If required
*             convert the Galactic coordinates to supergalactic
*             coordinates.

               CALL CAP_CCTMM (CRDVAL(1), CRDVAL(2), PROPRF,
     :           CRDVAL(3), CRDVAL(4), CRDVAL(5), CRDVAL(6),
     :           EQINT, EQIN, EPINT, EPIN,
     :           'J', 2.0D3, EPOUTT, EPOUT,
     :           RA2000, DE2000, STATUS)

               CALL SLA_EQGAL (RA2000, DE2000, L, B)

               IF (CRDTYP .EQ. CAP__CDSPG) THEN
                  CALL SLA_GALSUP (L, B, NWLNG, NWLAT)
               ELSE
                  NWLNG = L
                  NWLAT = B
               END IF

            END IF

            NWLNGN = .FALSE.
            NWLATN = .FALSE.

         ELSE
            NWLNGN = .TRUE.
            NWLATN = .TRUE.

            NWLNG = 0.0D0
            NWLAT = 0.0D0

         END IF

      END IF

      END
