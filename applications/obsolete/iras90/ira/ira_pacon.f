      SUBROUTINE IRA_PACON( NVAL, AIN, BIN, PAIN, SCSIN, SCSOUT, EPOCH,
     :                      AOUT, BOUT, PAOUT, STATUS )
*+
*  Name:
*     IRA_PACON

*  Purpose:
*     Convert sky coordinates and position angles from one SCS to
*     another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_PACON( NVAL, AIN, BIN, PAIN, SCSIN, SCSOUT, EPOCH, AOUT,
*                     BOUT, PAOUT, STATUS )

*  Description:
*     This routine use SLALIB to convert a list of sky coordinates from
*     one supported sky coordinate system to any other supported
*     system, and also converts a position angle at each position
*     (position angles are relative to "north", but north changes from
*     SCS to SCS, and therefore position angles also change). It is
*     assumed that the observations were made at the date given by the
*     Julian epoch supplied.  If any of the AIN, BIN or PAIN values are
*     equal to the Starlink "BAD" value (VAL__BADD) then the
*     corresponding output AOUT, BOUT and PAOUT values will all be set
*     to the bad value.

*  Arguments:
*     NVAL = INTEGER (Given)
*        The number of sky coordinate pairs to be converted.
*     AIN( NVAL ) = DOUBLE PRECISION (Given)
*        A list of first sky coordinate values to be converted, in
*        radians.
*     BIN( NVAL ) = DOUBLE PRECISION (Given)
*        A list of second sky coordinate values to be converted, in
*        radians.
*     PAIN( NVAL ) = DOUBLE PRECISION (Given)
*        A list of position angles to be converted, in radians. A
*        position angle is an angle from north, measured positive in
*        the sense of rotation from north to east.  Conversion of
*        position angle depends on the point on the celestial sphere at
*        which the position angle is measured. Each position angle is
*        assumed to be measured at the corresponding position given by
*        AIN and BIN.
*     SCSIN = CHARACTER * ( * ) (Given)
*        A string holding the name of the sky coordinate system of the
*        input list  (see ID2 section "Sky Coordinates"). Any
*        unambiguous abbreviation will do.
*     SCSOUT = CHARACTER * ( * ) (Given)
*        A string holding the name of the sky coordinate system required
*        for the output list. Any unambiguous abbreviation will do.
*     EPOCH = DOUBLE PRECISION (Given)
*        The Julian epoch at which the observations were made. When
*        dealing with IRAS data, the global constant IRA__IRJEP should
*        be specified. This constant is a Julian epoch suitable for all
*        IRAS data.
*     AOUT( NVAL ) = DOUBLE PRECISION (Returned)
*        The list of converted sky longitude values, in radians.
*     BOUT( NVAL ) = DOUBLE PRECISION (Returned)
*        The list of converted sky latitude values, in radians.
*     PAOUT( NVAL ) = DOUBLE PRECISION (Given)
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
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Arguments Given:
      INTEGER NVAL
      DOUBLE PRECISION AIN( NVAL )
      DOUBLE PRECISION BIN( NVAL )
      DOUBLE PRECISION PAIN( NVAL )
      CHARACTER        SCSIN*(*)
      CHARACTER        SCSOUT*(*)
      DOUBLE PRECISION EPOCH

*  Arguments Returned:
      DOUBLE PRECISION AOUT( NVAL )
      DOUBLE PRECISION BOUT( NVAL )
      DOUBLE PRECISION PAOUT( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BJI*1            ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQUI.
      CHARACTER BJO*1            ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQUO.
      DOUBLE PRECISION EQUI      ! The epoch of the reference equinox
                                 ! specified in argument SCSIN.
      DOUBLE PRECISION EQUO      ! The epoch of the reference equinox
                                 ! specified in argument SCSOUT.
      CHARACTER NAMEI*(IRA__SZSCS) ! Full input SCS value (with no
                                    ! equinox specifier).
      CHARACTER NAMEO*(IRA__SZSCS) ! Full output SCS value (with no
                                    ! equinox specifier).
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Identify the input SCS.
      CALL IRA1_CHSCS( SCSIN, NAMEI, EQUI, BJI, STATUS )

*  Identify the output SCS.
      CALL IRA1_CHSCS( SCSOUT, NAMEO, EQUO, BJO, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Call IRA1_IPACO to do the work.
      CALL IRA1_IPACO( NVAL, AIN, BIN, PAIN, NAMEI, EQUI, BJI, NAMEO,
     :                 EQUO, BJO, EPOCH, AOUT, BOUT, PAOUT, STATUS )

*  If an error occurred, give appropriate messages.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_PACON_ERR1',
     :     'IRA_PACON: Unable to convert a position and position '//'
     :      angle from one sky coordinate system to another.',
     :                 STATUS )
      END IF

      END
