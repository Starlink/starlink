      SUBROUTINE IRA1_IPACO( NVAL, AIN, BIN, PAIN, NAMEI, EQUI, BJI,
     :                       NAMEO, EQUO, BJO, EPOCH, AOUT, BOUT,
     :                       PAOUT, STATUS )
*+
*  Name:
*     IRA1_IPACO

*  Purpose:
*     Convert sky coordinates and position angles from one system to
*     another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_IPACO( NVAL, AIN, BIN, PAIN, NAMEI, EQUI, BJI, NAMEO,
*                      EQUO, BJO, EPOCH, AOUT, BOUT, PAOUT, STATUS )

*  Description:
*     This routine provides the functionality of IRA_PACON
*     without argument verification or context messages. The output
*     position angle is taken as the difference between the input
*     position angle and the position angle of the north pole of the
*     output SCS (measured in the input SCS).

*  Arguments:
*     NVAL = INTEGER (Given)
*        The number of sky coordinate pairs to be converted.
*     AIN( NVAL ) = DOUBLE PRECISION (Given)
*        A list of sky longitude values to be converted, in radians.
*     BIN( NVAL ) = DOUBLE PRECISION (Given)
*        A list of sky latitude values to be converted, in radians.
*     PAIN( NVAL ) = DOUBLE PRECISION (Given)
*        A list of position angles to be converted, in radians. A
*        position angle is an angle measured from north, through east.
*        The definition of north and east depend on the current sky
*        coordinate system and so position angles will change when the
*        current SCS changes. Conversion of position angle depends on
*        the point on the celestial sphere at which the angle is
*        measured. Each position angle is assumed to be measured at the
*        corresponding position given by AIN and BIN.
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
*     PAOUT( NVAL ) = DOUBLE PRECISION (Returned)
*        The list of converted position angles, in radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1991 (DSB):
*        Original version.
*     24-APR-1991 (DSB):
*        Modified for IRA version 2.
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
      INTEGER NVAL
      DOUBLE PRECISION AIN( NVAL )
      DOUBLE PRECISION BIN( NVAL )
      DOUBLE PRECISION PAIN( NVAL )
      CHARACTER NAMEI*(*)
      DOUBLE PRECISION EQUI
      CHARACTER BJI*(*)
      CHARACTER NAMEO*(*)
      DOUBLE PRECISION EQUO
      CHARACTER BJO*(*)
      DOUBLE PRECISION EPOCH

*  Arguments Returned:
      DOUBLE PRECISION AOUT( NVAL )
      DOUBLE PRECISION BOUT( NVAL )
      DOUBLE PRECISION PAOUT( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER        BADSCS*(IRA__SZSCS) ! An unsupported SCS value.
      DOUBLE PRECISION BEPOCH    ! Besselian epoch corresponding to the
                                 ! given Julian epoch.
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION NPA       ! The longitude (in the input SCS) of
                                 ! the north pole of the output SCS.
      DOUBLE PRECISION NPB       ! The latitude (in the input SCS) of
                                 ! the north pole of the output SCS.
      DOUBLE PRECISION SLA_DBEAR ! SLALIB function.
      DOUBLE PRECISION SLA_EPB   ! SLALIB function.
      DOUBLE PRECISION SLA_EPJ2D ! SLALIB function.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the Besselian Epoch corresponding to the given Julian
*  epoch.
      BEPOCH = SLA_EPB( SLA_EPJ2D( EPOCH ) )

*  Input SCS = EQUATORIAL.
      IF( NAMEI .EQ. 'EQUATORIAL') THEN

*  ...Output = EQUATORIAL.
         IF( NAMEO .EQ. 'EQUATORIAL') THEN

*  Convert all the coordinates (positions, not angles).
            CALL IRA1_EQEQ( NVAL, AIN, BIN, BEPOCH, BJI, EQUI,
     :                      BJO, EQUO, AOUT, BOUT, STATUS )

*  Find the coordinates of the north pole of the output system, in the
*  input system.
            CALL IRA1_EQEQ( 1, 0.0D0, IRA__PIBY2, BEPOCH, BJO, EQUO,
     :                      BJI, EQUI, NPA, NPB, STATUS )

*  ...Output = GALACTIC.
         ELSE IF( NAMEO .EQ. 'GALACTIC') THEN

*  Convert all the coordinates (positions, not angles).
            CALL IRA1_EQGAL( NVAL, AIN, BIN, BJI, EQUI, .TRUE., AOUT,
     :                       BOUT, STATUS )

*  Find the coordinates of the north pole of the output system, in the
*  input system.
            CALL IRA1_EQGAL( 1, 0.0D0, IRA__PIBY2, BJI, EQUI,
     :                       .FALSE., NPA, NPB, STATUS )

*  ...Output = ECLIPTIC.
         ELSE IF( NAMEO .EQ. 'ECLIPTIC') THEN

*  Convert all the coordinates (positions, not angles).
            CALL IRA1_EQECL( NVAL, AIN, BIN, BEPOCH, BJI, EQUI,
     :                      BJO, EQUO, .TRUE., AOUT, BOUT, STATUS )

*  Find the coordinates of the north pole of the output system, in the
*  input system.
            CALL IRA1_EQECL( 1, 0.0D0, IRA__PIBY2, BEPOCH, BJO, EQUO,
     :                      BJI, EQUI, .FALSE., NPA, NPB, STATUS )

*  ...Unrecognised output sky coordinate system.
         ELSE
            STATUS = IRA__BADSC
            BADSCS = NAMEO
            GO TO 999

         END IF

*  Input SCS = GALACTIC
      ELSE IF( NAMEI .EQ. 'GALACTIC') THEN

*  ...Output = EQUATORIAL.
         IF( NAMEO .EQ. 'EQUATORIAL') THEN

*  Convert all the coordinates (positions, not angles).
            CALL IRA1_EQGAL( NVAL, AIN, BIN, BJO, EQUO, .FALSE., AOUT,
     :                       BOUT, STATUS )

*  Find the coordinates of the north pole of the output system, in the
*  input system.
            CALL IRA1_EQGAL( 1, 0.0D0, IRA__PIBY2, BJO, EQUO,
     :                       .TRUE., NPA, NPB, STATUS )

*  ...Output = GALACTIC. Just copy good input position date to the
*  output.
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

*  Convert all the coordinates (positions, not angles).
            CALL IRA1_GALEC( NVAL, AIN, BIN, BJO, EQUO, .TRUE., AOUT,
     :                       BOUT, STATUS )

*  Find the coordinates of the north pole of the output system, in the
*  input system.
            CALL IRA1_GALEC( 1, 0.0D0, IRA__PIBY2, BJO, EQUO, .FALSE.,
     :                       NPA, NPB, STATUS )

*  ...Unrecognised output sky coordinate system.
         ELSE
            STATUS = IRA__BADSC
            BADSCS = NAMEO
            GO TO 999

         END IF

*  Input SCS = ECLIPTIC
      ELSE IF( NAMEI .EQ. 'ECLIPTIC') THEN

*  ...Output = EQUATORIAL.
         IF( NAMEO .EQ. 'EQUATORIAL') THEN

*  Convert all the coordinates (positions, not angles).
            CALL IRA1_EQECL( NVAL, AIN, BIN, BEPOCH, BJI, EQUI,
     :                      BJO, EQUO, .FALSE., AOUT, BOUT, STATUS )

*  Find the coordinates of the north pole of the output system, in the
*  input system.
            CALL IRA1_EQECL( 1, 0.0D0, IRA__PIBY2, BEPOCH, BJO, EQUO,
     :                      BJI, EQUI, .TRUE., NPA, NPB, STATUS )

*  ...Output = GALACTIC.
         ELSE IF( NAMEO .EQ. 'GALACTIC') THEN

*  Convert all the coordinates (positions, not angles).
            CALL IRA1_GALEC( NVAL, AIN, BIN, BJI, EQUI, .FALSE., AOUT,
     :                       BOUT, STATUS )

*  Find the coordinates of the north pole of the output system, in the
*  input system.
            CALL IRA1_GALEC( 1, 0.0D0, IRA__PIBY2, BJI, EQUI, .TRUE.,
     :                       NPA, NPB, STATUS )

*  ...Output = ECLIPTIC.
         ELSE IF( NAMEO .EQ. 'ECLIPTIC') THEN

*  Convert all the coordinates (positions, not angles).
            CALL IRA1_ECEC( NVAL, AIN, BIN, BJI, EQUI, BJO, EQUO, AOUT,
     :                      BOUT, STATUS )

*  Find the coordinates of the north pole of the output system, in the
*  input system.
            CALL IRA1_ECEC( 1, 0.0D0, IRA__PIBY2, BJO, EQUO, BJI, EQUI,
     :                      NPA, NPB, STATUS )

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

*  If all has gone OK, convert the position angles.
      IF( STATUS .EQ. SAI__OK ) THEN

*  If the input and output SCSs are the same just copy the input angles
*  to the output angles.
         IF( NAMEI .EQ. NAMEO. AND. BJI .EQ. BJO. AND.
     :       ABS( EQUI - EQUO ) .LT. 1.0D-4 ) THEN

            DO I = 1, NVAL
               PAOUT(I) = PAIN(I)
            END DO

*  Otherwise...
         ELSE

*  Loop round each good position.
            DO I = 1, NVAL
               IF( AOUT(I) .NE. VAL__BADD ) THEN

*  Subtract the position angle of the output pole from the input
*  position angle, to get the output position angle.
                  PAOUT(I) = PAIN(I) - SLA_DBEAR( AIN(I), BIN(I), NPA,
     :                                            NPB )

*  If this is a bad position, set the angle BAD.
               ELSE
                  PAOUT(I) = VAL__BADD

               END IF
            END DO

         END IF

      END IF

*  If an unsupported SCS was given, give a message.
 999  CONTINUE

      IF( STATUS .EQ. IRA__BADSC ) THEN
         CALL MSG_SETC( 'SCS', BADSCS )
         CALL ERR_REP( 'IRA1_IPACO_ERR1',
     :       'IRA1_IPACO: Sky coordinate system ^SCS not yet support ',
     :                 STATUS )
      END IF

      END
