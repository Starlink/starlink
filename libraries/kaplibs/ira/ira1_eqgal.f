      SUBROUTINE IRA1_EQGAL( NVAL, AIN, BIN, BJ, EQU, FORWRD, AOUT,
     :                       BOUT, STATUS )
*+
*  Name:
*     IRA1_EQGAL

*  Purpose:
*     Convert coordinates from equatorial to galactic or vice-versa.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_EQGAL( NVAL, AIN, BIN, BJ, EQU, FORWRD, AOUT, BOUT,
*                      STATUS )

*  Description:
*     If FORWRD is true, the input coordinates specified by AIN and BIN
*     should be Equatorial coordinates, referred to the mean equinox of
*     the epoch given by EQU (BJ specifies whether this is a Besselian
*     or Julian epoch). These coordinates are then converted to
*     Galactic and returned in AOUT and BOUT. If FORWRD is false, then
*     the input coordinates are Galactic and the output coordinates are
*     equatorial.  Equatorial coordinates for which BJ indicates a
*     Besselian epoch are presumed to be in the old FK4 system,
*     otherwise they are presumed to be in the new FK5 system.  If
*     either of the input coordinates are BAD (equal to VAL__BADD),
*     then both the corresponding output values are set BAD.

*  Arguments:
*     NVAL = INTEGER (Given)
*        The no. of points to precess.
*     AIN( NVAL ) = DOUBLE PRECISION (Given)
*        The input longitude values, in radians. If FORWRD is true,
*        these should be RA values, referred to the mean equinox of
*        epoch EQU. Otherwise they shoudl be Galactic longitudes.
*     BIN( NVAL ) = DOUBLE PRECISION (Given)
*        The input latitude values, in radians. If FORWRD is true,
*        these should be DEC values, referred to the mean equinox of
*        epoch EQU. Otherwise, they should be Galactic latitudes.
*     BJ = CHARACTER * ( * ) (Given)
*        If BJ has the value "B" then EQU is assumed to be a Besselian
*        epoch, and the equatorial coordinates (either input or output
*        depending on FORWRD) are assumed to be in the old FK4 system.
*        Otherwise EQU is assumed to be a Julian epoch, and the
*        equatorial coordinates are assumed to be in the new FK5
*        system.
*     EQU = DOUBLE PRECISION (Given)
*        The epoch of the reference equinox for the equatorial
*        coordinates (either input or output).
*     AOUT( NVAL ) = DOUBLE PRECISION (Given)
*        The output longitude values, in radians. If FORWRD is true,
*        these will be Galactic longitude values. Otherwise, they will
*        be RA values, referred to the mean equinox of epoch EQU.
*     BOUT( NVAL ) = DOUBLE PRECISION (Given)
*        The output latitude values, in radians. If FORWRD is true,
*        these will be Galactic latitude values. Otherwise, they will
*        be DEC values, referred to the mean equinox of epoch EQU.
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
      DOUBLE PRECISION ATEMP     ! Temporary longitude storage.
      DOUBLE PRECISION BTEMP     ! Temporary latitude storage.
      INTEGER          I         ! Loop count.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If going from Equatorial to Galactic...
      IF( FORWRD ) THEN

*  If the reference equinox is given by a Besselian epoch...
         IF( BJ .EQ. 'B' ) THEN

*  Precess the input equatorial coordinates to B1950.
            CALL IRA1_PREC( NVAL, AIN, BIN, 'B', EQU, 1950.0D0, AOUT,
     :                      BOUT, STATUS )

*  Convert each good coordinate pair to Galactic.
            DO I = 1, NVAL
               IF( AOUT(I) .NE. VAL__BADD ) THEN
                  CALL SLA_EG50( AOUT(I), BOUT(I), ATEMP, BTEMP )
                  AOUT(I) = ATEMP
                  BOUT(I) = BTEMP
               END IF
            END DO

*  If the reference equinox is given by a Julian epoch...
         ELSE

*  Precess the input equatorial coordinates to J2000.
            CALL IRA1_PREC( NVAL, AIN, BIN, 'J', EQU, 2000.0D0, AOUT,
     :                      BOUT, STATUS )

*  Convert each good coordinate pair to Galactic.
            DO I = 1, NVAL
               IF( AOUT(I) .NE. VAL__BADD ) THEN
                  CALL SLA_EQGAL( AOUT(I), BOUT(I), ATEMP, BTEMP )
                  AOUT(I) = ATEMP
                  BOUT(I) = BTEMP
               END IF
            END DO

         END IF

*  If going from Galactic to Equatorial...
      ELSE

*  If the output reference equinox is given by a Besselian epoch...
         IF( BJ .EQ. 'B' ) THEN

*  Convert each good input coordinate pair from Galactic to
*  Equatorial(B1950.0)
            DO I = 1, NVAL

               IF( AIN(I) .NE. VAL__BADD .AND.
     :             BIN(I) .NE. VAL__BADD ) THEN
                  CALL SLA_GE50( AIN(I), BIN(I), AOUT(I), BOUT(I) )

               ELSE
                  AOUT(I) = VAL__BADD
                  BOUT(I) = VAL__BADD

               END IF

            END DO

*  Precess the output equatorial coordinates from B1950 to the required
*  equinox.
            CALL IRA1_PREC( NVAL, AOUT, BOUT, 'B', 1950.0D0, EQU, AOUT,
     :                      BOUT, STATUS )


*  If the output reference equinox is given by a Julian epoch...
         ELSE

*  Convert each good input coordinate pair from Galactic to
*  Equatorial(J2000.0)
            DO I = 1, NVAL

               IF( AIN(I) .NE. VAL__BADD .AND.
     :             BIN(I) .NE. VAL__BADD ) THEN
                  CALL SLA_GALEQ( AIN(I), BIN(I), AOUT(I), BOUT(I) )

               ELSE
                  AOUT(I) = VAL__BADD
                  BOUT(I) = VAL__BADD

               END IF

            END DO

*  Precess the output equatorial coordinates from J2000 to the required
*  equinox.
            CALL IRA1_PREC( NVAL, AOUT, BOUT, 'J', 2000.0D0, EQU, AOUT,
     :                      BOUT, STATUS )

         END IF

      END IF

      END
