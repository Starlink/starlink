      SUBROUTINE PVECT( STATUS )
*+
*  Name:
*     PVECT

*  Purpose:
*     Draw proper motion vectors.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Draw proper motion vectors on a projection of the celestial
*     sphere. The XCOL and YCOL data areas are assumed to contain
*     positions in radians, the EXCOL and EYCOL data areas are assumed
*     to contain the proper motions in radians per year. It is possible
*     to use the ERSCALE parameter to multiply the the proper motion so
*     that it is correct for a given number of years. (The proper
*     motion in RA is assumed to be $\dot{\alpha}\cos\delta$.)

*  Usage:
*     pvect [erscale]

*  ADAM Parameters:
*     ERSCALE = _REAL (Read and Write)
*        The scale factor for multiplying the vectors.
*
*        [The value of the global parameter PONGO_ERSCALE is used. If
*        PONGO_ERSCALE is not defined, the default value 1.0 is used.]
*     ZMULT = _LOGICAL (Read)
*        If TRUE, the ZCOL values are additionally used to multiply the
*        vectors.
*        [FALSE]
*     PROJECTION = _CHAR (Read)
*        The geometry used to plot the data.  This is explained in more
*        detail in the section on projections.  Allowed values: "NONE",
*        "TAN", "SIN", "ARC", "GLS", "AITOFF", "MERCATOR" and "STG".
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
*        of the global parameter PONGO_DECCENTRE is used. If
*        PONGO_DECCENTRE is not defined, the default value "0" is used.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     3-JUN-1994 (PDRAPER):
*        Added explicit type casts.
*     21-JUN-1994 (PDRAPER):
*        Added checks for device open.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PONGO_PAR'       ! PONGO global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'       ! PONGO global variables

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP         ! PGPLOT device is open
      EXTERNAL SLA_DRANRM
      DOUBLE PRECISION SLA_DRANRM ! Put angle into range 0 to 2pi

*  Local Variables:
      LOGICAL ZMULT             ! TRUE if the vectors are to be
                                ! multiplied by the Z data

      INTEGER I                 ! Loop index
      INTEGER IDAT              ! Counter
      INTEGER PROJECTION        ! Projection type
      INTEGER LSTAT             ! Local status
      INTEGER NERR              ! Number of vectors not plotted

      REAL SCALE                ! Scale factor
      REAL ZFACT                ! Z factor

      DOUBLE PRECISION RAT      ! Temporary RA
      DOUBLE PRECISION DECT     ! Temporary dec
      DOUBLE PRECISION POS( 3 ) ! Cartesian position of the points
      DOUBLE PRECISION PM( 3 )  ! Proper motion vector
      DOUBLE PRECISION RA0      ! Projection centre (RA)
      DOUBLE PRECISION DEC0     ! Projection centre (dec)

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL PAR_GET0R( 'ERSCALE', SCALE, STATUS )
      CALL PAR_GET0L( 'ZMULT', ZMULT, STATUS )

*  Get the projection parameters.
      CALL PON_GETPROJ( 'PROJECTION', 'RACENTRE', 'DECCENTRE',
     :                  PROJECTION, RA0, DEC0, STATUS )
      NERR = 0
      IF ( STATUS .EQ. SAI__OK ) THEN
         DO 20 IDAT = 1, NDAT
            IF ( ZMULT ) THEN
               ZFACT = ZDATA( IDAT ) * SCALE
            ELSE
               ZFACT = SCALE
            END IF

            RAT = XDATA( IDAT )
            DECT = YDATA( IDAT )
            CALL SLA_DCS2C( RAT, DECT, POS )
            PM( 1 ) = -DBLE( ERRX( IDAT ) ) * SIN( RAT )
     :           -DBLE( ERRY( IDAT ) ) * COS( RAT ) * SIN( DECT )
            PM( 2 ) = DBLE( ERRX( IDAT ) ) * COS( RAT )
     :           -DBLE( ERRY( IDAT ) ) * SIN( RAT ) * SIN( DECT )
            PM( 3 ) = -DBLE( ERRY( IDAT ) ) * COS( DECT )

            DO 10 I = 1, 3
               POS( I ) = POS( I ) + PM( I ) * DBLE( ZFACT )
 10         CONTINUE

            CALL SLA_DCC2S( POS, RAT, DECT )
            RAT = SLA_DRANRM( RAT )
            LSTAT = SAI__OK
            CALL PON_GT_CIRCLE( PROJECTION-1, RA0, DEC0, XDATA( IDAT ),
     :                          YDATA( IDAT ), RAT, DECT, .TRUE.,
     :                          LSTAT )
            IF ( LSTAT .NE. SAI__OK ) THEN
               NERR = NERR + 1
            END IF
 20      CONTINUE
         IF ( NERR .NE. 0 ) THEN
            STATUS = SAI__ERROR
            IF ( NERR .EQ. NDAT ) THEN
               CALL ERR_REP( 'PVECTPROB',
     :              'Failed to plot any proper motion vectors', STATUS )
            ELSE
               CALL MSG_SETI( 'NERR', NERR )
               CALL ERR_REP( 'PVECTPROB',
     :              'Failed to plot ^NERR vectors.', STATUS )
            END IF
         END IF
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PVECT_END',
     :        'PVECT: Cannot draw proper motion vectors.', STATUS )
      END IF
      END
* $Id$
