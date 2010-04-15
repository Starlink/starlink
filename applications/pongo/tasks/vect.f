      SUBROUTINE VECT( STATUS )
*+
*  Name:
*     VECT

*  Purpose:
*     Draw vectors from each data point.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Use the values in the EXCOL and EYCOL data areas as signed
*     offsets in X and Y to plot vectors from the data points. These
*     vectors are scaled using the error scale parameter ERSCALE.
*     Individual vectors may also be scaled using the contents of the
*     ZCOL data area.

*  Usage:
*     vect [erscale] [zmult]

*  ADAM Parameters:
*     ERSCALE = _REAL (Read and Write)
*        Factor for scaling all vectors.
*
*        [The value of the global parameter PONGO_ERSCALE is used. If
*        PONGO_ERSCALE is not defined, the default value 1.0 is used.]
*     ZMULT = _LOGICAL (Read)
*        If TRUE, the values of the ZCOL data area are used as
*        multipliers for the vectors. This parameter may only be
*        specified on the command line.
*        [FALSE]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Deficiencies:
*     -  Does not draw arrow heads.
*     -  Does not allow the specification of vectors as length and
*     angle.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     10-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     3-JUN-1994 (PDRAPER):
*        Removed unused variables.
*     22-JUN-1994 (PDRAPER):
*        Added check for device already open.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

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
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      INTEGER IDAT               ! Loop index

      REAL SCALE                 ! ERSCALE value
      REAL ZFACT                 ! ZCOL scaling factor

      LOGICAL ZMULT              ! ZMULT flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Is the plotting device open?
      IF ( PON_DEVOP ( .TRUE., STATUS ) ) THEN
         CALL PAR_GET0R( 'ERSCALE', SCALE, STATUS )
         CALL PAR_GET0L( 'ZMULT', ZMULT, STATUS )

*  Check the returned status and act.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO 10 IDAT = 1, NDAT
               IF ( ZMULT ) THEN
                  ZFACT = ZDATA( IDAT ) * SCALE
               ELSE
                  ZFACT = SCALE
               END IF
               CALL PGMOVE( REAL( XDATA( IDAT ) ),
     :                      REAL( YDATA( IDAT ) ) )
               CALL PGDRAW( REAL( XDATA( IDAT ) )+ERRX( IDAT )*ZFACT,
     :                      REAL( YDATA( IDAT ) )+ERRY( IDAT )*ZFACT )
 10         CONTINUE
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'VECT_END',
     :                 'VECT: Cannot draw vectors for each data point.',
     :                 STATUS )
      END IF
      END
* $Id$
