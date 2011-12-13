      SUBROUTINE IRA1_GALEC( NVAL, AIN, BIN, BJ, EQU, FORWRD, AOUT,
     :                       BOUT, STATUS )
*+
*  Name:
*     IRA1_GALEC

*  Purpose:
*     Convert coordinates from galactic to ecliptic or vice-versa.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_GALEC( NVAL, AIN, BIN, BJ, EQU, FORWRD, AOUT, BOUT,
*                      STATUS )

*  Description:
*     If FORWRD is true, the input coordinates specified by AIN and BIN
*     should be galactic coordinates. These are then converted to
*     ecliptic coordinates referred to the mean equinox of the epoch
*     given by EQU (BJ specifies whether this is a Besselian or Julian
*     epoch) and returned in AOUT and BOUT. If FORWRD is false, then
*     the input coordinates are ecliptic and the output coordinates are
*     galactic.  If either of the input coordinates are BAD (equal to
*     VAL__BADD), then both the corresponding output values are set
*     BAD.

*  Arguments:
*     NVAL = INTEGER (Given)
*        The no. of points to precess.
*     AIN( NVAL ) = DOUBLE PRECISION (Given)
*        The input longitude values, in radians. If FORWRD is true,
*        these should be galactic values. Otherwise they should be
*        ecliptic values referred to the mean equinox of epoch EQU.
*     BIN( NVAL ) = DOUBLE PRECISION (Given)
*        The input latitude values, in radians. If FORWRD is true,
*        these should be galactic values. Otherwise they should be
*        ecliptic values referred to the mean equinox of epoch EQU.
*     BJ = CHARACTER * ( * ) (Given)
*        If BJ has the value "B" then EQU is assumed to be a Besselian
*        epoch.  Otherwise EQU is assumed to be a Julian epoch.
*     EQU = DOUBLE PRECISION (Given)
*        The epoch of the reference equinox for the ecliptic
*        coordinates (either input or output).
*     AOUT( NVAL ) = DOUBLE PRECISION (Given)
*        The output longitude values, in radians. If FORWRD is true,
*        these will be ecliptic values referred to the mean equinox of
*        epoch EQU. Otherwise, they will be galactic values,
*     BOUT( NVAL ) = DOUBLE PRECISION (Given)
*        The output latitude values, in radians. If FORWRD is true,
*        these will be ecliptic values referred to the mean equinox of
*        epoch EQU. Otherwise, they will be galactic values,
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
      INTEGER  NVAL
      DOUBLE PRECISION AIN( NVAL )
      DOUBLE PRECISION BIN( NVAL )
      CHARACTER BJ*(*)
      DOUBLE PRECISION EQU
      LOGICAL FORWRD

*  Arguments Returned:
      DOUBLE PRECISION AOUT( NVAL )
      DOUBLE PRECISION BOUT( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION JEC       ! Julian equivalent of required epoch.
      DOUBLE PRECISION MAT1(3,3) ! Galactic to equatorial (J2000)
                                 ! rotation matrix. Obtained by
                                 ! transposing the equatorial(J2000) to
                                 ! galactic matrix from the source for
                                 ! SLALIB routine SLA_GALEQ.
      DOUBLE PRECISION MAT2(3,3) ! Equatorial (J2000) to equatorial
                                 ! (required epoch) rotation matrix.
      DOUBLE PRECISION MAT3(3,3) ! Equatorial (required epoch) to
                                 ! ecliptic (required epoch) rotation
                                 ! matrix.
      DOUBLE PRECISION MAT4(3,3) ! Galactic to equatorial (required
                                 ! epoch) rotation matrix.
      DOUBLE PRECISION MAT5(3,3) ! Galactic to ecliptic (required epoch)
                                 ! rotation matrix.
      DOUBLE PRECISION SLA_EPCO  ! SLALIB function.
      DOUBLE PRECISION SLA_EPJ2D ! SLALIB function.
      DOUBLE PRECISION VIN(3)    ! Input position in Cartesian form.
      DOUBLE PRECISION VOUT(3)   ! Output position in Cartesian form.

*  Local Data:
      DATA MAT1(1,1),MAT1(1,2),MAT1(1,3),
     :     MAT1(2,1),MAT1(2,2),MAT1(2,3),
     :     MAT1(3,1),MAT1(3,2),MAT1(3,3)/
     : -0.054875539726D0,+0.494109453312D0,-0.867666135858D0,
     : -0.873437108010D0,-0.444829589425D0,-0.198076386122D0,
     : -0.483834985808D0,+0.746982251810D0,+0.455983795705D0/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the epoch of the reference equinox for the ecliptic
*  coordinates to a Julian epoch.
      JEC = SLA_EPCO( 'J', BJ, EQU )

*  Get the equatorial precession matrix from Julian epoch 2000.0 to
*  the required Julian epoch, JEC.
      CALL SLA_PREC( 2000.0D0, JEC, MAT2 )

*  Get the equatorial to ecliptic rotation matrix for the required
*  Julian epoch, JEC.
      CALL SLA_ECMAT( SLA_EPJ2D( JEC ), MAT3 )

*  Multiply the galactic to equatorial(J2000) matrix, by the
*  equatorial(J2000) to equatorial(JEC) matrix, to get the galactic to
*  equatorial(JEC) matrix.
      CALL SLA_DMXM( MAT2, MAT1, MAT4 )

*  Multiply the galactic to equatorial(JEC) matrix by the
*  equatorial(JEC) to ecliptic(JEC) matrix to get the final galactic to
*  ecliptic rotation matrix (MAT5).
      CALL SLA_DMXM( MAT3, MAT4, MAT5 )

*  If going from Galactic to Ecliptic...
      IF( FORWRD ) THEN

*  Loop round each good coordinate pair.
         DO I = 1, NVAL
            IF( AIN(I) .NE. VAL__BADD .AND.
     :          BIN(I) .NE. VAL__BADD ) THEN

*  Convert the input coordinates to Cartesian form.
               CALL SLA_DCS2C( AIN(I), BIN(I), VIN )

*  Use the matrix formed above to convert from galactic to ecliptic
*  coordinates.
               CALL SLA_DMXV( MAT5, VIN, VOUT )

*  Convert back to spherical coordinates.
               CALL SLA_DCC2S( VOUT, AOUT(I), BOUT(I) )

*  If either of the input coordinates was bad, set both output
*  coordinates bad.
            ELSE
               AOUT(I) = VAL__BADD
               BOUT(I) = VAL__BADD

            END IF

         END DO

*  If going from Ecliptic to Galactic...
      ELSE

*  Loop round each good coordinate pair.
         DO I = 1, NVAL
            IF( AIN(I) .NE. VAL__BADD .AND.
     :          BIN(I) .NE. VAL__BADD ) THEN

*  Convert the input coordinates to Cartesian form.
               CALL SLA_DCS2C( AIN(I), BIN(I), VIN )

*  Use the inverse of the matrix formed above to convert from ecliptic
*  to galactic coordinates.
               CALL SLA_DIMXV( MAT5, VIN, VOUT )

*  Convert back to spherical coordinates.
               CALL SLA_DCC2S( VOUT, AOUT(I), BOUT(I) )

*  If either of the input coordinates was bad, set both output
*  coordinates bad.
            ELSE
               AOUT(I) = VAL__BADD
               BOUT(I) = VAL__BADD

            END IF

         END DO

      END IF

      END
