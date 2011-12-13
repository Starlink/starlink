      SUBROUTINE IRA1_ECEC( NVAL, AIN, BIN, BJI, EQUI, BJO, EQUO, AOUT,
     :                      BOUT, STATUS )
*+
*  Name:
*     IRA_ECEC

*  Purpose:
*     Convert ecliptic coordinates from one reference equinox to
*     another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_ECEC( NVAL, AIN, BIN, BJI, EQUI, BJO, EQUO, AOUT, BOUT,
*                     STATUS )

*  Description:
*     The input coordinates should be ecliptic coordinates referred to
*     the mean equinox of the epoch given by EQUI (BJI specifies
*     whether this is a Besselian or Julian epoch). The output
*     coordinates are ecliptic coordinates referred to the mean equinox
*     of the epoch given by EQUO (epoch type specified by BJO).  If
*     either of the input coordinates are BAD (equal to VAL__BADD),
*     then both the corresponding output values are set BAD. Epoch
*     values that differ by less than 0.0001 of a year are considered
*     equal.

*  Arguments:
*     NVAL = INTEGER (Given)
*        The no. of points to precess.
*     AIN( NVAL ) = DOUBLE PRECISION (Given)
*        The input ecliptic longitude values, in radians, referred to
*        the mean equinox of epoch EQUI.
*     BIN( NVAL ) = DOUBLE PRECISION (Given)
*        The input ecliptic latitude values, in radians, referred to
*        the mean equinox of epoch EQUI.
*     BJI = CHARACTER * ( * ) (Given)
*        If BJI has the value "B" then EQUI is assumed to be a Besselian
*        epoch.  Otherwise EQUI is assumed to be a Julian epoch.
*     EQUI = DOUBLE PRECISION (Given)
*        The epoch of the reference equinox for the input coordinates.
*     BJO = CHARACTER * ( * ) (Given)
*        If BJO has the value "B" then EQUO is assumed to be a Besselian
*        epoch.  Otherwise EQUO is assumed to be a Julian epoch.
*     EQUO = DOUBLE PRECISION (Given)
*        The epoch of the reference equinox for the output coordinates.
*     AOUT( NVAL ) = DOUBLE PRECISION (Given)
*        The output ecliptic longitude values, in radians, referred to
*        the mean equinox of epoch EQUO.
*     BOUT( NVAL ) = DOUBLE PRECISION (Given)
*        The output ecliptic latitude values, in radians, referred to
*        the mean equinox of epoch EQUO.
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
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION JIN       ! Julian equivalent of input epoch.
      DOUBLE PRECISION JOUT      ! Julian equivalent of output epoch.
      DOUBLE PRECISION MAT1(3,3) ! Ecliptic (input epoch) to equatorial
                                 ! (input epoch) rotation matrix.
      DOUBLE PRECISION MAT2(3,3) ! Equatorial (input epoch) to
                                 ! equatorial (output epoch) rotation
                                 ! matrix.
      DOUBLE PRECISION MAT3(3,3) ! Equatorial (output epoch) to
                                 ! ecliptic (output epoch) rotation
                                 ! matrix.
      DOUBLE PRECISION MAT4(3,3) ! Ecliptic (input epoch) to equatorial
                                 ! (output epoch) rotation matrix.
      DOUBLE PRECISION MAT5(3,3) ! Ecliptic (input epoch) to ecliptic
                                 ! (output epoch) rotation matrix.
      DOUBLE PRECISION SLA_EPCO  ! SLALIB function.
      DOUBLE PRECISION SLA_EPJ2D ! SLALIB function.
      DOUBLE PRECISION TEMP      ! Temporary storage.
      DOUBLE PRECISION VIN(3)    ! Input position in Cartesian form.
      DOUBLE PRECISION VOUT(3)   ! Output position in Cartesian form.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the input and output reference equinox is the same, just copy good
*  input data to the output.
      IF( BJI .EQ. BJO .AND. ABS( EQUI - EQUO ) .LT. 1.0D-4 ) THEN

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

*  Get Julian equivalents for the two epochs.
         JIN =  SLA_EPCO( 'J', BJI, EQUI )
         JOUT = SLA_EPCO( 'J', BJO, EQUO )

*  Get the equatorial to ecliptic rotation matrix for the input epoch.
         CALL SLA_ECMAT( SLA_EPJ2D( JIN ), MAT1 )

*  Transpose it to get the ecliptic to equatorial rotation matrix for
*  the input date.
         TEMP = MAT1( 1, 2 )
         MAT1( 1, 2 ) = MAT1( 2, 1 )
         MAT1( 2, 1 ) = TEMP

         TEMP = MAT1( 1, 3 )
         MAT1( 1, 3 ) = MAT1( 3, 1 )
         MAT1( 3, 1 ) = TEMP

         TEMP = MAT1( 2, 3 )
         MAT1( 2, 3 ) = MAT1( 3, 2 )
         MAT1( 3, 2 ) = TEMP

*  Get the equatorial (input epoch) to equatorial (output epoch)
*  rotation matrix.
         CALL SLA_PREC( JIN, JOUT, MAT2 )

*  Get the equatorial to ecliptic rotation matrix for the output date.
         CALL SLA_ECMAT( SLA_EPJ2D( JOUT ), MAT3 )

*  Multiply the ecliptic (input epoch) to equatorial (input epoch)
*  matrix, by the equatorial (input epoch) to equatorial (output epoch)
*  matrix, to get the ecliptic (input epoch) to equatorial (output
*  epoch) matrix.
         CALL SLA_DMXM( MAT2, MAT1, MAT4 )

*  Multiply the ecliptic (input epoch) to equatorial (output epoch)
*  matrix, by the equatorial (output epoch) to ecliptic (output epoch)
*  matrix, to get the final ecliptic (input epoch) to ecliptic (output
*  epoch) rotation matrix (MAT5).
         CALL SLA_DMXM( MAT3, MAT4, MAT5 )

*  Loop round each good coordinate pair.
         DO I = 1, NVAL
            IF( AIN(I) .NE. VAL__BADD .AND.
     :          BIN(I) .NE. VAL__BADD ) THEN

*  Convert the input coordinates to Cartesian form.
               CALL SLA_DCS2C( AIN(I), BIN(I), VIN )

*  Use the matrix formed above to convert from ecliptic (input epoch)
*  to ecliptic (output epoch) coordinates.
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

      END IF

      END
