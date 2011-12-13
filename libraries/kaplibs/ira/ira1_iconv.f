      SUBROUTINE IRA1_ICONV( NVAL, AIN, BIN, NAMEI, EQUI, BJI, NAMEO,
     :                       EQUO, BJO, EPOCH, AOUT, BOUT, STATUS )
*+
*  Name:
*     IRA1_ICONV

*  Purpose:
*     Convert sky coordinates from one system to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_ICONV( NVAL, AIN, BIN, NAMEI, EQUI, BJI, NAMEO,
*                      EQUO, BJO, EPOCH, AOUT, BOUT, STATUS )

*  Description:
*     This routine provideds the functionality of IRA_CONVT
*     without argument verification or context messages.

*  Arguments:
*     NVAL = INTEGER (Given)
*        The number of sky coordinate pairs to be converted.
*     AIN( NVAL ) = DOUBLE PRECISION (Given)
*        A list of sky longitude values to be converted, in radians.
*     BIN( NVAL ) = DOUBLE PRECISION (Given)
*        A list of sky latitude values to be converted, in radians.
*     NAMEI = CHARACTER * ( * ) (Given)
*        A string holding the full name of the sky coordinate system of
*        the input list with no equinox specifier.
*     EQUI = DOUBLE PRECISION (Given)
*        The epoch of the reference equinox for the input Sky Coordinate
*        System. Set to VAL__BADD if the input coordinates are not
*        referenced to the equinox.
*     BJI = CHARACTER * ( * ) (Given)
*        Either B or J; specifies if the epoch given in argument EQUI is
*        a Besselian or Julian epoch.
*     NAMEO = CHARACTER * ( * ) (Given)
*        A string holding the full name of the sky coordinate system
*        required for the output list with no equinox specifier.
*     EQUO = DOUBLE PRECISION (Given)
*        The epoch of the reference equinox for the output Sky
*        Coordinate System. Set to VAL__BADD if the output coordinates
*        are not referenced to the equinox.
*     BJO = CHARACTER * ( * ) (Given)
*        Either B or J; specifies if the epoch given in argument EQUO
*        is a Besselian or Julian epoch.
*     EPOCH = DOUBLE PRECISION (Given)
*        The Julian epoch at which the observations were made. When
*        dealing with IRAS data, the global constant IRA__IRJEP should
*        be specified. This constant is a Julian epoch suitable for all
*        IRAS data.
*     AOUT( NVAL ) = DOUBLE PRECISION (Returned)
*        The list of converted sky longitude values, in radians.
*     BOUT( NVAL ) = DOUBLE PRECISION (Returned)
*        The list of converted sky latitude values, in radians.
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
*     14-JAN-1991 (DSB):
*        Original version.
*     26-APR-1991 (DSB):
*        Modified for second version of IRA
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      INTEGER          NVAL
      DOUBLE PRECISION AIN( NVAL )
      DOUBLE PRECISION BIN( NVAL )
      CHARACTER        NAMEI*(*)
      DOUBLE PRECISION EQUI
      CHARACTER        BJI*(*)
      CHARACTER        NAMEO*(*)
      DOUBLE PRECISION EQUO
      CHARACTER        BJO*(*)
      DOUBLE PRECISION EPOCH

*  Arguments Returned:
      DOUBLE PRECISION AOUT( NVAL )
      DOUBLE PRECISION BOUT( NVAL )

*  Status:
      INTEGER          STATUS    ! Global status

*  Local Variables:
      CHARACTER BADSCS*(IRA__SZSCS) ! An unsupported SCS value.
      DOUBLE PRECISION BEPOCH    ! Besselian epoch corresponding to the
                                 ! given Julian epoch.
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION SLA_EPB   ! SLALIB function.
      DOUBLE PRECISION SLA_EPJ2D ! SLALIB function.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the Besselian Epoch corresponding to the given Julian
*  epoch.
      BEPOCH = SLA_EPB( SLA_EPJ2D( EPOCH ) )

*  Input SCS = EQUATORIAL
      IF( NAMEI .EQ. 'EQUATORIAL') THEN

*  ...Output = EQUATORIAL.
         IF( NAMEO .EQ. 'EQUATORIAL') THEN
            CALL IRA1_EQEQ( NVAL, AIN, BIN, BEPOCH, BJI, EQUI, BJO,
     :                      EQUO, AOUT, BOUT, STATUS )

*  ...Output = GALACTIC.
         ELSE IF( NAMEO .EQ. 'GALACTIC') THEN
            CALL IRA1_EQGAL( NVAL, AIN, BIN, BJI, EQUI, .TRUE., AOUT,
     :                       BOUT, STATUS )

*  ...Output = ECLIPTIC.
         ELSE IF( NAMEO .EQ. 'ECLIPTIC') THEN
            CALL IRA1_EQECL( NVAL, AIN, BIN, BEPOCH, BJI, EQUI, BJO,
     :                      EQUO, .TRUE., AOUT, BOUT, STATUS )

*  ...Unrecognised output sky coordinate system.
         ELSE
            STATUS = IRA__BADSC
            BADSCS = NAMEO
            GO TO 999

         END IF

*  Input SCS = GALACTIC.
      ELSE IF( NAMEI .EQ. 'GALACTIC') THEN

*  ...Output = EQUATORIAL.
         IF( NAMEO .EQ. 'EQUATORIAL') THEN
            CALL IRA1_EQGAL( NVAL, AIN, BIN, BJO, EQUO, .FALSE., AOUT,
     :                       BOUT, STATUS )

*  ...Output = GALACTIC. Just copy input to output.
         ELSE IF( NAMEO .EQ. 'GALACTIC') THEN

            DO I = 1, NVAL

               IF( AIN(I) .NE. VAL__BADD .AND.
     :             BIN(I) .NE. VAL__BADD ) THEN
                  AOUT( I ) = AIN( I )
                  BOUT( I ) = BIN( I )

               ELSE
                  AOUT( I ) = VAL__BADD
                  BOUT( I ) = VAL__BADD

               END IF

            END DO

*  ...Output = ECLIPTIC.
         ELSE IF( NAMEO .EQ. 'ECLIPTIC') THEN
            CALL IRA1_GALEC( NVAL, AIN, BIN, BJO, EQUO, .TRUE., AOUT,
     :                       BOUT, STATUS )

*  ...Unrecognised output sky coordinate system.
         ELSE
            STATUS = IRA__BADSC
            BADSCS = NAMEO
            GO TO 999

         END IF

*  Input SCS = ECLIPTIC.
      ELSE IF( NAMEI .EQ. 'ECLIPTIC') THEN

*  ...Output = EQUATORIAL.
         IF( NAMEO .EQ. 'EQUATORIAL') THEN
            CALL IRA1_EQECL( NVAL, AIN, BIN, BEPOCH, BJI, EQUI, BJO,
     :                      EQUO, .FALSE., AOUT, BOUT, STATUS )

*  ...Output = GALACTIC.
         ELSE IF( NAMEO .EQ. 'GALACTIC') THEN
            CALL IRA1_GALEC( NVAL, AIN, BIN, BJI, EQUI, .FALSE., AOUT,
     :                       BOUT, STATUS )

*  ...Output = ECLIPTIC.
         ELSE IF( NAMEO .EQ. 'ECLIPTIC') THEN
            CALL IRA1_ECEC( NVAL, AIN, BIN, BJI, EQUI, BJO, EQUO, AOUT,
     :                      BOUT, STATUS )

*  ...Unrecognised output sky coordinate system.
         ELSE
            STATUS = IRA__BADSC
            BADSCS = NAMEO
            GO TO 999

         END IF

*  Unrecognised input sky coordinate system.
      ELSE
         STATUS = IRA__BADSC
         BADSCS = NAMEI

      END IF

*  If an unsupported SCS was given, give a message.
 999  CONTINUE

      IF( STATUS .EQ. IRA__BADSC ) THEN
         CALL MSG_SETC( 'SCS', BADSCS )
         CALL ERR_REP( 'IRA1_ICONV_ERR1',
     :   'IRA1_ICONV: Sky coordinate system ^SCS not yet support',
     :                 STATUS )
      END IF


      END
