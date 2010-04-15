      SUBROUTINE PRIM( STATUS )
*+
*  Name:
*     PRIM

*  Purpose:
*     Perform primitive plotting functions.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Perform the primitive plotting functions: move to, draw line to,
*     and mark.

*  Usage:
*     prim action x y [symbol]

*  ADAM Parameters:
*     ACTION = _CHAR (Read and Write)
*        Type of primitive function. This may be one of the following:
*
*        - "M" -- move to,
*        - "D" -- draw line to,
*        - "K" -- mark.
*
*        [The value is prompted for.]
*     X = _REAL (Read and Write)
*        X coordinate of point.
*
*        [The value is prompted for.]
*     Y = _REAL (Read and Write)
*        Y coordinate of point.
*
*        [The value is prompted for.]
*     SYMBOL = _INTEGER (Read and Write)
*        PGPLOT symbol number for drawing the point mark.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 1.
*     PROJECTION = _CHAR (Read)
*        The geometry that is to be used for plotting. This is
*        explained in more detail in the section on projections.
*        Allowed values: "NONE", "TAN", "SIN", "ARC", "GLS", "AITOFF",
*        "MERCATOR" and "STG". This parameter is only relevant when
*        marking points.
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_PROJECTN is used. If
*        PONGO_PROJECTN is not defined, the default value "NONE" is
*        used.
*     RACENTRE = _CHAR (Read)
*        The centre of the projection in RA (i.e. the angle must be
*        specified as hh:mm:ss.sss). This parameter is only required for
*        PROJECTION values other than "NONE".
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_RACENTRE is used. If
*        PONGO_RACENTRE is not defined, the default value "0" is used.
*     DECCENTRE = _CHAR (Read)
*        The centre of the projection in declination (i.e. the angle
*        must be specified as dd:mm:ss.sss). This parameter is only
*        required for PROJECTION values other than "NONE".
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_DECENTRE is used. If
*        PONGO_DECCENTRE is not defined, the default value "0" is used.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - When using non-planar coordinates, the coordinates should be
*       given in degrees. Lines drawn between points are straight.
*       Use GT_CIRCLE for drawing lines that follow great circles.
*

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-JUN-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     20-JUN-1994 (PDRAPER):
*        Added check for device is open.
*     2-MAY-1997 (PDRAPER):
*        Now uses projections for all actions.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants


*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! Device is open

*  Local Variables:
      INTEGER PROJECTION         ! Projection type
      INTEGER SYMBOL             ! PGPLOT symbol number
      INTEGER LSTAT              ! Local status

      DOUBLE PRECISION RA0       ! Projection centre (RA)
      DOUBLE PRECISION DEC0      ! Projection centre (dec)
      DOUBLE PRECISION X         ! X coordinate
      DOUBLE PRECISION Y         ! Y coordinate
      DOUBLE PRECISION L         ! Projective coordinate
      DOUBLE PRECISION M         ! Projective coordinate

      CHARACTER * ( 1 ) ACTION   ! Action

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if device is open.
      IF ( .NOT. PON_DEVOP( .TRUE., STATUS ) ) GO TO 99

*  Get the "action" type.
      CALL PAR_GET0C( 'ACTION', ACTION, STATUS )
      CALL CHR_UCASE( ACTION )

*  Get the projection parameters.
      CALL PON_GETPROJ( 'PROJECTION', 'RACENTRE', 'DECCENTRE',
     :                  PROJECTION, RA0, DEC0, STATUS )

*  Get the point.
      CALL PAR_GET0D( 'X', X, STATUS )
      CALL PAR_GET0D( 'Y', Y, STATUS )

*  Project it if required.
      IF ( PROJECTION .EQ. 1 ) THEN
         L = X
         M = Y
      ELSE
         LSTAT = SAI__OK
         CALL PROJ_CONVPTLM( PROJECTION - 1, RA0, DEC0, X * DDEG2R,
     :                       Y * DDEG2R, L, M, LSTAT )
         IF ( LSTAT .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PRIM_OUTSIDE',
     :'The position you have given cannot be projected', STATUS )
            GO TO 99
         END IF
      END IF

      IF ( ACTION .EQ. 'M' ) THEN
         CALL PGMOVE( REAL( L ), REAL( M ) )
      ELSE IF ( ACTION .EQ. 'D' ) THEN
         CALL PGDRAW( REAL( L ), REAL( M ) )
      ELSE IF ( ACTION .EQ. 'K' ) THEN
         CALL PAR_GET0I( 'SYMBOL', SYMBOL, STATUS )
         CALL PGPOINT( 1, REAL( L ), REAL( M ), SYMBOL )
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
 99   CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PRIM_END',
     :               'PRIM: Unable to perform plotting function.',
     :               STATUS )
      END IF
      END
* $Id$
