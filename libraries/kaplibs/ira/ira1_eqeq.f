      SUBROUTINE IRA1_EQEQ( NVAL, AIN, BIN, BEPOCH, BJI, EQUI, BJO,
     :                       EQUO, AOUT, BOUT, STATUS )
*+
*  Name:
*     IRA1_EQEQ

*  Purpose:
*     Precess equatorial coordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_EQEQ( NVAL, AIN, BIN, BEPOCH, BJI, EQUI, BJO, EQUO,
*                      AOUT, BOUT, STATUS )

*  Description:
*     The input equatorial coordinates specified by AIN and BIN should
*     be referred to the mean equinox of the epoch given by EQUI. These
*     coordinates are precessed so that they refer to the mean equinox
*     of the epoch given by EQUO. BJI and BJO specify what type of
*     epochs are held by EQUI and EQUO. A value of B is used to
*     indicate a Besselian epoch, any other value indicates a Julian
*     epoch.  BJI need not be the same as BJO. Coordinates for which BJ
*     indicates a Besselian epoch are presumed to be in the old FK4
*     system, otherwise coordinates are presumed to be in the new FK5
*     system.  If either of the input coordinates are BAD (equal to
*     VAL__BADD), then both the corresponding output values are set
*     BAD. EQUI and EQUO are considered equal if they differ by less
*     than 0.0001 of a year.

*  Arguments:
*     NVAL = INTEGER (Given)
*        The no. of points to precess.
*     AIN( NVAL ) = DOUBLE PRECISION (Given)
*        The input RA values, in radians, referred to the mean equinox
*        of epoch EQUI.
*     BIN( NVAL ) = DOUBLE PRECISION (Given)
*        The input DEC values, in radians, referred to the mean equinox
*        of epoch EQUI.
*     BEPOCH = DOUBLE PRECISION (Given)
*        The Besselian epoch at which the observations were made.
*     BJI = CHARACTER * ( * ) (Given)
*        If BJI has the value "B" then EQUI is assumed to be a
*        Besselian epoch, and the input coordinates are assumed to be
*        in the old FK4 system. Otherwise EQUI is assumed to be a
*        Julian epoch, and the input coordinates are assumed to be in
*        the new FK5 system.
*     EQUI = DOUBLE PRECISION (Given)
*        The epoch of the input reference equinox.
*     BJO = CHARACTER * ( * ) (Given)
*        If BJO has the value "B" then EQUO is assumed to be a
*        Besselian epoch, and the output coordinates are assumed to be
*        in the old FK4 system. Otherwise EQUO is assumed to be a
*        Julian epoch, and the output coordinates are assumed to be in
*        the new FK5 system.
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
      DOUBLE PRECISION BEPOCH
      CHARACTER BJI*(*)
      DOUBLE PRECISION EQUI
      CHARACTER BJO*(*)
      DOUBLE PRECISION EQUO

*  Arguments Returned:
      DOUBLE PRECISION AOUT( NVAL )
      DOUBLE PRECISION BOUT( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION ATEMP     ! Temporary RA value.
      DOUBLE PRECISION BTEMP     ! Temporary DEC value.
      DOUBLE PRECISION DR        ! RA proper motion.
      DOUBLE PRECISION DD        ! DEC proper motion.
      INTEGER          I         ! Loop count.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the input reference equinox is specified by a Besselian epoch...
      IF( BJI .EQ. 'B' ) THEN

*  and the output reference equinox is specified by a Besselian epoch...
         IF( BJO .EQ. 'B' ) THEN

*  Precess the input coordinates to get the output coordinates.
            CALL IRA1_PREC( NVAL, AIN, BIN, 'B', EQUI, EQUO, AOUT,
     :                      BOUT, STATUS )

*  If the output reference equinox is specified by a Julian epoch...
         ELSE

*  Precess the input data to B1950.0
            CALL IRA1_PREC( NVAL, AIN, BIN, 'B', EQUI, 1950.0D0,
     :                      AOUT, BOUT, STATUS )

*  Convert from B1950 (FK4) to J2000.0 (FK5)
            DO I = 1, NVAL
               IF( AOUT(I) .NE. VAL__BADD ) THEN
                  CALL SLA_FK45Z( AOUT(I), BOUT(I), BEPOCH,
     :                            ATEMP, BTEMP )
                  AOUT(I) = ATEMP
                  BOUT(I) = BTEMP
               END IF
            END DO

*  Precess the output data to the required Julian epoch, EQUO.
            CALL IRA1_PREC( NVAL, AOUT, BOUT, 'J', 2000.0D0, EQUO,
     :                      AOUT, BOUT, STATUS )
         END IF

*  If the input reference equinox is specified by a Julian epoch...
      ELSE

*  and the output reference equinox is specified by a Besselian epoch...
         IF( BJO .EQ. 'B' ) THEN

*  Precess the input data to J2000.0
            CALL IRA1_PREC( NVAL, AIN, BIN, 'J', EQUI, 2000.0D0,
     :                      AOUT, BOUT, STATUS )

*  Convert from J2000 (FK5) to B1950 (FK4)
            DO I = 1, NVAL
               IF( AOUT(I) .NE. VAL__BADD ) THEN
                  CALL SLA_FK54Z( AOUT(I), BOUT(I), BEPOCH,
     :                            ATEMP, BTEMP, DR, DD )
                  AOUT(I) = ATEMP
                  BOUT(I) = BTEMP
               END IF
            END DO

*  Precess the output data to the required Besselian epoch, EQUO.
            CALL IRA1_PREC( NVAL, AOUT, BOUT, 'B', 1950.0D0, EQUO,
     :                      AOUT, BOUT, STATUS )

*  If the output reference equinox is specified by a Julian epoch...
         ELSE

*  Precess the input coordinates to get the output coordinates.
            CALL IRA1_PREC( NVAL, AIN, BIN, 'J', EQUI, EQUO, AOUT,
     :                      BOUT, STATUS )

         END IF

      END IF


      END
