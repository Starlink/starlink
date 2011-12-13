      SUBROUTINE IRA1_PREC( NVAL, AIN, BIN, BJ, EQUI, EQUO, AOUT, BOUT,
     :                      STATUS )
*+
*  Name:
*     IRA1_PREC

*  Purpose:
*     Precess equatorial coordinates using either Besselian or Julain
*     epochs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_PREC( NVAL, AIN, BIN, BJ, EQUI, EQUO, AOUT, BOUT,
*                     STATUS )

*  Description:
*     The input equatorial coordinates specified by AIN and BIN should
*     be referred to the mean equinox of the epoch given by EQUI. These
*     coordinates are precessed so that they refer to the mean equinox
*     of the epoch given by EQUO. If BJ has the value B, it is assumed
*     that EQUI and EQUO are both Besselian epochs, and the precession
*     is performed using the old FK4 system.  If BJ has any other
*     value, it is assumed that EQUI and EQUO are both Julian epochs,
*     and the precession is performed using the new FK5 system.
*     If either of the input coordinates are BAD (equal to VAL__BADD),
*     then both the corresponding output values are set BAD. If EQUI and
*     EQUO differ by less than 0.0001 of a year, the input values are
*     just copied to the output arrays.

*  Arguments:
*     NVAL = INTEGER (Given)
*        The no. of points to precess.
*     AIN( NVAL ) = DOUBLE PRECISION (Given)
*        The input RA values, in radians, referred to the mean equinox
*        of epoch EQUI.
*     BIN( NVAL ) = DOUBLE PRECISION (Given)
*        The input DEC values, in radians, referred to the mean equinox
*        of epoch EQUI.
*     BJ = CHARACTER * ( * ) (Given)
*        If BJ has the value "B" then EQUI and EQUO are assumed to be
*        Besselian epochs, and the precession is performed using the
*        old FK4 system. Otherwise EQUI and EQUO are assumed to be
*        Julian epochs, and the precession is performed using the new
*        FK5 system.
*     EQUI = DOUBLE PRECISION (Given)
*        The epoch of the input reference equinox.
*     EQUO = DOUBLE PRECISION (Given)
*        The epoch of the output reference equinox.
*     AOUT( NVAL ) = DOUBLE PRECISION (Given)
*        The output RA values, in radians, referred to the mean equinox
*        of epoch EQUO.
*     BOUT( NVAL ) = DOUBLE PRECISION (Given)
*        The output DEC values, in radians, referred to the mean equinox
*        of epoch EQUO.
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
*     30-APR-1991 (DSB):
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
      INCLUDE 'PRM_PAR'          ! Bad data values.

*  Arguments Given:
      INTEGER NVAL
      DOUBLE PRECISION AIN( NVAL )
      DOUBLE PRECISION BIN( NVAL )
      CHARACTER BJ*(*)
      DOUBLE PRECISION EQUI
      DOUBLE PRECISION EQUO

*  Arguments Returned:
      DOUBLE PRECISION AOUT( NVAL )
      DOUBLE PRECISION BOUT( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter.
      DOUBLE PRECISION PMAT( 3, 3 ) ! Precession matrix.
      DOUBLE PRECISION VIN( 3 )  ! Input coordinates in Cartesian form.
      DOUBLE PRECISION VOUT( 3 ) ! Output coordinates in Cartesian form.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If input and output coordinates are referenced to the same equinox,
*  (to within 0.0001 of a year) just copy input to output.
      IF( ABS( EQUI -  EQUO ) .LT. 1.0D-4 ) THEN

         DO I = 1, NVAL

            IF( AIN(I) .NE. VAL__BADD .AND.
     :          BIN(I) .NE. VAL__BADD ) THEN

               AOUT(I) = AIN(I)
               BOUT(I) = BIN(I)

            ELSE
               AOUT(I) = VAL__BADD
               BOUT(I) = VAL__BADD

            END IF

         END DO

*  Otherwise...
      ELSE

*  If Besselian epochs are specified, set up the precession matrix
*  using the old FK4 system.
         IF( BJ .EQ. 'B' ) THEN
            CALL SLA_PREBN( EQUI, EQUO, PMAT )

*  If Julian epochs are specified, set up the precession matrix
*  using the new FK5 system.
         ELSE
            CALL SLA_PREC( EQUI, EQUO, PMAT )

         END IF

*  Use the precession matrix to transform each good input value.
         DO I = 1, NVAL
            IF( AIN(I) .NE. VAL__BADD .AND.
     :          BIN(I) .NE. VAL__BADD ) THEN

*  Convert the input coordinates to Cartesian form.
               CALL SLA_DCS2C( AIN(I), BIN(I), VIN )

*  Apply the precession matrix.
               CALL SLA_DMXV( PMAT, VIN, VOUT )

*  Convert back to spherical coordinates.
               CALL SLA_DCC2S( VOUT, AOUT(I), BOUT(I) )

*  If either of the input values was bad, set both output values bad.
            ELSE
               AOUT(I) = VAL__BADD
               BOUT(I) = VAL__BADD
            END IF

         END DO

      END IF

      END
