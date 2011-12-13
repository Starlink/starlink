      SUBROUTINE IRA1_EQECL( NVAL, AIN, BIN, BEPOCH, BJI, EQUI, BJO,
     :                       EQUO, FORWRD, AOUT, BOUT, STATUS )
*+
*  Name:
*     IRA1_EQECL

*  Purpose:
*     Convert coordinates from equatorial to ecliptic or vice-versa.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_EQECL( NVAL, AIN, BIN, BEPOCH, BJI, EQUI, BJO, EQUO,
*                      FORWRD, AOUT, BOUT, STATUS )

*  Description:
*     If FORWRD is true, the input coordinates specified by AIN and BIN
*     should be Equatorial coordinates, referred to the mean equinox of
*     the epoch given by EQUI (BJI specifies whether this is a
*     Besselian or Julian epoch). These coordinates are then converted
*     to Ecliptic coordinates referred to the mean equinox of the epoch
*     given by EQUO (BJO specifies whether this is a Besselian or
*     Julian epoch) and returned in AOUT and BOUT.  If FORWRD is false,
*     the input coordinates specified by AIN and BIN should be Ecliptic
*     coordinates, referred to the mean equinox of the epoch given by
*     EQUI (BJI specifies whether this is a Besselian or Julian epoch).
*     These coordinates are then converted to Equatorial coordinates
*     referred to the mean equinox of the epoch given by EQUO (BJO
*     specifies whether this is a Besselian or Julian epoch) and
*     returned in AOUT and BOUT.  Equatorial coordinates (either input
*     or output) for which BJ indicates a Besselian epoch are presumed
*     to be in the old FK4 system, otherwise they are presumed to be in
*     the new FK5 system.  If either of the input coordinates are BAD
*     (equal to VAL__BADD), then both the corresponding output values
*     are set BAD.  EQUI and EQUO are considered equal if they differ
*     by less than 0.0001 of a year.

*  Arguments:
*     NVAL = INTEGER (Given)
*        The no. of points to precess.
*     AIN( NVAL ) = DOUBLE PRECISION (Given)
*        Input longitude values, in radians. If FORWRD is true, these
*        should be RA values, otherwise they should be ecliptic
*        longitude values. In both cases, they should be referred to
*        the mean equinox of epoch EQUI.
*     BIN( NVAL ) = DOUBLE PRECISION (Given)
*        Input latitude values, in radians. If FORWRD is true, these
*        should be DEC values, otherwise they should be ecliptic
*        latitude values. In both cases, they should be referred to
*        the mean equinox of epoch EQUI.
*     BEPOCH = DOUBLE PRECISION (Given)
*        The Besselian epoch at which the observations were made.
*     BJI = CHARACTER * ( * ) (Given)
*        If BJI has the value "B" then EQUI is assumed to be a
*        Besselian epoch, and if the input coordinates are equatorial,
*        they are assumed to be in the old FK4 system. Otherwise EQUI
*        is assumed to be a Julian epoch, and if the input coordinates
*        are equatorial, they are assumed to be in the new FK5 system.
*     EQUI = DOUBLE PRECISION (Given)
*        The epoch of the input reference equinox.
*     BJO = CHARACTER * ( * ) (Given)
*        If BJO has the value "B" then EQUO is assumed to be a
*        Besselian epoch, and if the output coordinates are equatorial,
*        they are assumed to be in the old FK4 system. Otherwise EQUO
*        is assumed to be a Julian epoch, and if the output coordinates
*        are equatorial, they are assumed to be in the new FK5 system.
*     EQUO = DOUBLE PRECISION (Given)
*        The epoch of the output reference equinox.
*     AOUT( NVAL ) = DOUBLE PRECISION (Given)
*        Output longitude values, in radians. If FORWRD is true, these
*        will be ecliptic longitude values, otherwise they will be RA
*        values. In both cases, they will be referred to the mean
*        equinox of epoch EQUO.
*     BOUT( NVAL ) = DOUBLE PRECISION (Given)
*        Output latitude values, in radians. If FORWRD is true, these
*        will be ecliptic latitude values, otherwise they will be DEC
*        values. In both cases, they will be referred to the mean
*        equinox of epoch EQUO.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-MAY-1991 (DSB):
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
      INCLUDE 'PRM_PAR'          ! BAD data values.

*  Arguments Given:
      INTEGER          NVAL
      DOUBLE PRECISION AIN( NVAL )
      DOUBLE PRECISION BIN( NVAL )
      DOUBLE PRECISION BEPOCH
      CHARACTER        BJI*(*)
      DOUBLE PRECISION EQUI
      CHARACTER        BJO*(*)
      DOUBLE PRECISION EQUO
      LOGICAL          FORWRD

*  Arguments Returned:
      DOUBLE PRECISION AOUT( NVAL )
      DOUBLE PRECISION BOUT( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION SLA_EPCO  ! SLALIB function.
      DOUBLE PRECISION SLA_EPJ2D ! SLALIB function.
      DOUBLE PRECISION JIN       ! Julian equivalent of EQUI.
      DOUBLE PRECISION JOUT      ! Julian equivalent of EQUO.
      DOUBLE PRECISION MAT(3,3)  ! Equatorial to ecliptic rotation
                                 ! matrix.
      DOUBLE PRECISION VIN(3)    ! Input Cartesian position.
      DOUBLE PRECISION VOUT(3)   ! Input Cartesian position.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If going from Equatorial to Ecliptic...
      IF( FORWRD ) THEN

*  Convert the output epoch to a Julian epoch (if it is not already a
*  Julian epoch).
         JOUT  = SLA_EPCO( 'J', BJO, EQUO )

*  Convert the input coordinates to the FK5 system, referred to the
*  mean equinox of the Julian equivalent of the epoch EQUO.
         CALL IRA1_EQEQ( NVAL, AIN, BIN, BEPOCH, BJI, EQUI, 'J', JOUT,
     :                   AOUT, BOUT, STATUS )

*  Create the matrix for conversion from equatorial (refered to mean
*  equator and equinox of the output epoch) to ecliptic (referred to
*  the mean ecliptic and equinox of the output epoch).
         CALL SLA_ECMAT( SLA_EPJ2D( JOUT ), MAT )

*  Loop round each good input position.
         DO I = 1, NVAL
            IF( AOUT(I) .NE. VAL__BADD ) THEN

*  Convert Equatorial coordinates from Spherical to Cartesian form.
               CALL SLA_DCS2C( AOUT(I), BOUT(I), VIN )

*  Use the matrix to convert these coordinates to Cartesian ecliptic
*  coordinates.
               CALL SLA_DMXV( MAT, VIN, VOUT )

*  Convert Ecliptic coordinates from Cartesian to Spherical form.
               CALL SLA_DCC2S( VOUT, AOUT(I), BOUT(I) )

            END IF

         END DO

*  If going from Ecliptic to Equatorial...
      ELSE

*  Convert the input epoch to a Julian epoch (if it is not already a
*  Julian epoch).
         JIN  = SLA_EPCO( 'J', BJI, EQUI )

*  Create the matrix for conversion from equatorial (refered to mean
*  equator and equinox of the input epoch) to ecliptic (referred to
*  the mean ecliptic and equinox of the input epoch).
         CALL SLA_ECMAT( SLA_EPJ2D( JIN ), MAT )

*  Loop round each good input position.
         DO I = 1, NVAL
            IF( AIN(I) .NE. VAL__BADD .AND.
     :          BIN(I) .NE. VAL__BADD ) THEN

*  Convert Ecliptic coordinates from Spherical to Cartesian form.
               CALL SLA_DCS2C( AIN(I), BIN(I), VIN )

*  Use the inverse matrix to convert these coordinates to Cartesian
*  Equatorial coordinates, in the FK5 system, referred to the mean
*  equinox of Julian epoch JIN.
               CALL SLA_DIMXV( MAT, VIN, VOUT )

*  Convert Equatorial coordinates from Cartesian to Spherical form.
               CALL SLA_DCC2S( VOUT, AOUT(I), BOUT(I) )

*  If either input coordinate was bad, set both output coordinates bad.
            ELSE
               AOUT(I) = VAL__BADD
               BOUT(I) = VAL__BADD

            END IF

         END DO

*  Convert the output equatorial coordinates so that they refer to the
*  mean equinox of the epoch EQUO.
         CALL IRA1_EQEQ( NVAL, AOUT, BOUT, BEPOCH, 'J', JIN, BJO, EQUO,
     :                   AOUT, BOUT, STATUS )

      END IF

      END
