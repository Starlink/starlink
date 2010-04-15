      SUBROUTINE PLOTFUN( STATUS )
*+
*  Name:
*     PLOTFUN

*  Purpose:
*     Plot a given function.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Plot a function specified on the command line by a Fortran-like
*     expression, or through the parameters resulting from a previous
*     fit (using a polynomial or spline).

*  Usage:
*     plotfun action expression [xmin] [xmax]

*  ADAM Parameters:
*     ACTION = _CHAR (Read)
*        The type of function to be plotted. This must be one of the
*        following:
*           - "FUNC" -- Use a Fortran-like expression to define the
*           function.
*           - "POLY" -- Use a set of polynomial coefficients to define
*           the function.
*           - "SPLINE" -- Use a set of spline coefficients from the file
*           SPLINEFILE to define the function.
*
*        [The value is prompted for.]
*     EXPRESSION = _CHAR (Read)
*        The Fortran-like expression to be plotted, in terms of X.
*
*        [The value is prompted for.]
*     XMIN = _REAL (Read)
*        The value of X from which the function is plotted.
*
*        [The value of the global parameter PONGO_XMIN is used.]
*     XMAX = _REAL (Read)
*        The value of X to which the function is plotted.
*
*        [The value of the global parameter PONGO_XMAX is used.]
*     INFILE = _LOGICAL (Read)
*        Used when ACTION is "POLY". If TRUE then the polynomial
*        coefficients are stored in a file (the first line of which is
*        the order of the polynomial), otherwise the coefficients
*        are given using the POLYCOEF parameter.
*
*        [FALSE]
*     NPOLY = _INTEGER (Read and Write)
*        The order of the polynomial: used when ACTION is "POLY".
*
*        [The value of the global parameter PONGO_NPOLY is used.]
*     POLYCOEF = _DOUBLE (Read)
*        A list of polynomial coefficients: used when ACTION is "POLY".
*        and INFILE is FALSE.
*
*        [The value of the global parameter PONGO_POLYCOEF is used.]
*     POLYFILE = _CHAR (Read)
*        The name of a file containing the polynomial coefficients to
*        be plotted. Only used when ACTION is "POLY" and INFILE is
*        TRUE.
*
*        [POLYFILE.dat]
*     SPLINEFILE = FILENAME (Read)
*        The name of the file containing the coefficients and knot
*        positions from a previous spline fit -- used when ACTION is
*        "SPLINE".
*
*        [The value of the global parameter PONGO_SPLINEF is used. If
*        PONGO_SPLINEF is not defined, the value is prompted for.]

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
*     12-OCT-1993 (PCTR):
*        Fixed divide by zero on ACTION=FUNC.
*     2-JUN-1994 (PDRAPER):
*        Added DAT_PAR include. Fixed string assignments.
*     16-JUN-1994 (PDRAPER):
*        Added check for device open.
*     30-AUG-1996 (PDRAPER):
*        Now uses the new spline file format.
*     6-MAY-1997 (PDRAPER):
*        Added ability to read in polynomial coefficients through
*        a file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL PON_DEVOP
      EXTERNAL PON_DEVOP         ! PGPLOT is open

*  Local Variables:
      CHARACTER * ( 1 ) INVERSE( 1 ) ! Always given as X
      CHARACTER * ( 10 ) ACTION ! Type of function to be plotted
      CHARACTER * ( 132 ) FORWARD( 1 ) ! Function expression (Y)
      CHARACTER * ( 80 ) INBUFF
      CHARACTER * ( DAT__SZLOC ) LOCTR ! HDS locator used for TRN calls
      DOUBLE PRECISION COEFF( MAXPOLY + 1 ) ! Polynomial coefficients
      INTEGER I                 ! Loop index
      INTEGER IAT               ! Position in string
      INTEGER ID
      INTEGER IKNOTS
      INTEGER NCHAR
      INTEGER NDATTEMP          ! Number of plotted points
      INTEGER NPOLY             ! Given number of polynomial coefs.
      INTEGER NPTEMP            ! Number of polynomial coefs. provided
      INTEGER SFD               ! Spline parameters file descriptor
      INTEGER PFD               ! Polynomial cofficients file descriptor
      REAL KNOTS( PON__NEST )   ! Knot positions
      REAL SPLINE( PON__NEST )  ! Spline polynomial coefficients
      REAL XMAXP
      REAL XMINP
      REAL XSTEP                ! Plotting step size in X
      LOGICAL OPEN              ! Spline file is open
      LOGICAL INFILE            ! Polynomial coefficients are in file

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Is device open?
      IF ( .NOT. PON_DEVOP( .TRUE., STATUS ) ) GO TO 999

*  Get the action.
      CALL PAR_GET0C( 'ACTION', ACTION, STATUS )
      CALL CHR_UCASE( ACTION )
      IF ( ACTION( : 4 ) .EQ. 'FUNC' ) THEN

*     Get the expression to be plotted.
         CALL PAR_GET0C( 'EXPRESSION', FORWARD( 1 ), STATUS )

*     Get the X limits over which the function is to be plotted.
         CALL PAR_GET0R( 'XMIN', XMINP, STATUS )
         CALL PAR_GET0R( 'XMAX', XMAXP, STATUS )
         CALL CHR_UCASE( FORWARD( 1 ) )
         CALL CHR_PREFX( 'Y=', FORWARD( 1 ), IAT )
         INVERSE( 1 ) = 'X'

*     Create the transformation.
         CALL TRN_NEW( 1, 1, FORWARD, INVERSE, '_REAL:', 'X -->f(X)',
     :                 ' ', ' ', LOCTR, STATUS )

*     Compile it.
         CALL TRN_COMP( LOCTR, .TRUE., ID, STATUS )

*     Set up a data array containing equally spaced input values.
         NDATTEMP = MAX( MIN( 500, ( NDATMAX/2 - 1 ) ), 2 )
         XSTEP = ( XMAXP - XMINP ) / REAL( NDATTEMP - 1 )

         DO 10 I = 1, NDATTEMP
            WORK( I ) = XMINP + XSTEP * REAL( I - 1 )
 10      CONTINUE

*     Transform these to the Y values.
         CALL TRN_TR1R( .FALSE., NDATTEMP, WORK, ID, WORK( NDATTEMP+1 ),
     :                  STATUS )

*     Draw the function.
         CALL PGLINE( NDATTEMP, WORK, WORK( NDATTEMP+1 ) )

*     Clean up.
         CALL DAT_ANNUL( LOCTR, STATUS )
         CALL TRN_ANNUL( ID, STATUS )
         CALL TRN_CLOSE( STATUS )
      ELSE IF ( ACTION( : 4 ) .EQ. 'POLY' ) THEN

*     See if the coefficients are to be given in a file or by the
*     usual parameters.
         CALL PAR_GET0L( 'INFILE', INFILE, STATUS )
         IF ( INFILE ) THEN
            CALL PON_ASFIO( 'POLYFILE', 'READ', 'LIST', 0, PFD, OPEN,
     :                      STATUS )
            CALL FIO_READ( PFD, INBUFF, NCHAR, STATUS )
            READ( INBUFF, * ) NPOLY
            NPOLY = NPOLY + 1
            DO 21 I = 1, NPOLY
               CALL FIO_READ( PFD, INBUFF, NCHAR, STATUS )
               READ( INBUFF, * ) COEFF( I )
 21         CONTINUE
            IF ( OPEN ) CALL FIO_CLOSE( PFD, STATUS )
         ELSE
            CALL PAR_GET0I( 'NPOLY', NPOLY, STATUS )
            NPOLY = NPOLY + 1
            CALL PAR_GET1D( 'POLYCOEF', MAXPOLY, COEFF, NPTEMP, STATUS )
            IF ( NPTEMP .NE. NPOLY ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'ORDER', NPOLY - 1 )
               CALL MSG_SETI( 'NCOEFF', NPOLY )
               CALL ERR_REP( 'PLOTFUN_BADORD', 'Error: an order ^ORDER '
     :                     //'polynomial requires ^NCOEFF coefficients.'
     :                     ,STATUS )
               GO TO 999
            END IF
         END IF

*        Get the X limits over which the function is to be plotted.
         CALL PAR_GET0R( 'XMIN', XMINP, STATUS )
         CALL PAR_GET0R( 'XMAX', XMAXP, STATUS )
         CALL PON_PLOTPOLY( XMINP, XMAXP, NPOLY, COEFF )
      ELSE IF ( ACTION( : 6 ) .EQ. 'SPLINE' ) THEN
         CALL PON_ASFIO( 'SPLINEFILE', 'READ', 'LIST', 0, SFD, OPEN,
     :                   STATUS )

         IF ( STATUS .NE. SAI__OK ) GO TO 999

         CALL FIO_READ( SFD, INBUFF, NCHAR, STATUS )
         READ( INBUFF, * ) IKNOTS, NPOLY

         DO 20 I = 1, IKNOTS
            CALL FIO_READ( SFD, INBUFF, NCHAR, STATUS )
            READ( INBUFF, * ) KNOTS(I), SPLINE(I)
 20      CONTINUE

*     Get the x limits over which the function is to be plotted (NOT
*     used at the moment).
         CALL PON_PLOTSPLINE( IKNOTS, KNOTS, SPLINE, NPOLY, STATUS )
         CALL FIO_CLOSE( SFD, STATUS )
         CALL PAR_CANCL( 'SPLINEFILE', STATUS )
      END IF

*  Abort.
 999  CONTINUE

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'PLOTFUN_END',
     :                              'PLOTFUN: Cannot plot the ' //
     :                              'given function.', STATUS )

      END
* $Id$
