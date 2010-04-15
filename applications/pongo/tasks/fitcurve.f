      SUBROUTINE FITCURVE( STATUS )
*+
*  Name:
*     FITCURVE

*  Purpose:
*     Fit a curve to the data.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     This command fits data using either a polynomial or a smooth
*     spline. If weights are available these can be used when
*     determining the fit.
*
*     The resultant fit parameters are displayed and written to the
*     environment for use by the PLOTFUN application, which can replot the
*     polynomial or spline with other datasets etc.

*  Usage:
*     fitcurve action npoly [weight]

*  ADAM Parameters:
*     ACTION = _CHAR (Read and Write)
*        Type of curve to be fitted. Currently this action must be
*        "POLY" or "SPLINE"
*
*        If the parameter is not specified on the command line, it will
*        be prompted for.
*     NPOLY = _INTEGER (Read and Write)
*        The order of the polynomial or spline. For polynomials this can
*        range from 1 to 9 and for splines from 2 to 6.
*
*        If the parameter is not specified on the command line, it
*        will be prompted for.
*     COLOUR = _INTEGER (Read and Write)
*        The colour index used when plotting the fitted curve.
*
*        If no value is specified on the command line, the current value
*        is used. If there is no current value, a default of 2 (i.e.
*        red) will be used.
*     WEIGHT = _LOGICAL (Read and Write)
*        Whether the fit is to use the y-axis error data in the EYCOL
*        data area, if available. If no error data are available, the
*        fit will always be unweighted.
*
*        If the value is not specified on the command line, the current
*        value is used. If there is no current value, a default value of
*        TRUE is used.
*     POLYCOEF[10] = _DOUBLE (Write)
*        If ACTION is "POLY", the polynomial coefficients resulting
*        from the fit are written to this parameter.
*
*        The value of this parameter is written to the global parameter
*        PONGO_POLYCOEF.
*     POLYFILE = _CHAR (Read)
*        The name of a file to contain the polynomials coefficients
*        of the fit (only used ACTION is "POLY"). This file can be
*        used to store the fit permanently and can be used by the
*        PLOTFUN command to redraw the fit.
*
*        [POLYFILE.dat]
*     SMOOTH = _REAL (Read)
*        Only used if ACTION is "SPLINE". This factor determines the
*        tradeoff between the closeness and smoothness of the spline
*        fit. It should be a real number greater than 0.0. Normally this
*        is dynamically defaulted to a number equal to the number of
*        points to be fitted. Decreasing this value to 0.0 produces an
*        interpolating spline fit.
*     SPLINEFILE = _CHAR (Read)
*        The name of a file to contain the coefficients and knot
*        positions from the spline fit -- used when ACTION is
*        "SPLINE". This file can be used by the PLOTFUN command to
*        redraw the fit.
*
*        [The value of the global parameter PONGO_SPLINEF is used. If
*        PONGO_SPLINEF is not defined, the value is prompted for.]
*     XMIN = _REAL (Read)
*        The minimum X value to be used in the fit.
*
*        The value of the global parameter PONGO_XMIN is used. If
*        PONGO_XMIN is not defined, the default value 0.0 is used.
*     XMAX = _REAL (Read)
*        The maximum X value to be used in the fit.
*
*        The value of the global parameter PONGO_XMAX is used. If
*        PONGO_XMAX is not defined, the default value 1.0 is used.

*  Notes:
*     - This routine fits a general polynomial of the form:
*
*         y = a1 + a2*x + a3*x^2 + a4*x^3 ..... an*x^n
*
*     which has order n-1, using a least squares approach.
*
*     - The spline fit is characterised by the positions of the knots and
*     the spline coefficients (and the order used for the splines) all
*     of which are stored in the spline file. The number of knots used
*     in the fit can only be influenced by using the SMOOTH
*     parameter. Higher values of this give larger smoothing factors. A
*     value of 0 gives an interpolating spline fit.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     2-JAN-1994 (PDRAPER):
*        Made LOGE10 double precision as used in this type anyway.
*     20-JUN-1994 (PDRAPER):
*        Added check for device already open.
*     21-OCT-1994 (PDRAPER):
*        Changed error report about number of parameters when fitting a
*        polynomial reference the polynomial order rather than the
*        number of fit parameters.
*     30-AUG-1996 (PDRAPER):
*        Removed NAG calls, changed to use a general polynomial rather
*        than a Chebyshev polynomial. Recreated the spline fitting. Now
*        uses a routine that determines the positions of the knots itself.
*     14-APR-1997 (PDRAPER):
*        Made some workspace dynamic. This saves on static space in
*        compiled program (most compilers do not treat local arrays
*        as automatic!).
*     30-APR-1997 (PDRAPER):
*        Corrected usage of XMIN, YMIN, XMAX and YMAX parameters.
*        In actual fact only XMIN and XMAX were every used and these
*        inconsistently. XMIN and XMAX are the only parameters left.
*     6-MAY-1997 (PDRAPER):
*        Added POLYFILE parameter to allow permanent storage of the
*        polynomial coefficients and so that string truncation
*        problems for complex fits can be avoided (mainly in IRAF).
*     1-OCT-2004 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS            ! Global status

*  Local Constants:
      DOUBLE PRECISION  LOG10E  ! Log_10( e )
      PARAMETER ( LOG10E = 0.434294481903 )
      INTEGER NINTER            ! Number of points along fit
      PARAMETER ( NINTER = 500 )

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      CHARACTER * ( 10 ) ACTION ! Type of averaging
      CHARACTER * ( 80 ) OUTBUF
      DOUBLE PRECISION COEFF( MAXPOLY + 1 ) ! Polynomial coefficients
      DOUBLE PRECISION EPS      ! Error/tolerance in fit
      DOUBLE PRECISION NEWSIG   ! New transformed error vaue
      DOUBLE PRECISION THEFIT( NDATMAX ) ! Fit to X data points
      DOUBLE PRECISION WD( NDATMAX ) ! Statistical weights
      DOUBLE PRECISION XD( NDATMAX ) ! X-axis data
      DOUBLE PRECISION YD( NDATMAX ) ! Y-axis data
      INTEGER COLIDX            ! Colour for fitted curve
      INTEGER COLSAV            ! Current colour index
      INTEGER I                 ! Loop index
      INTEGER IFAIL1, IFAIL2    ! Fit error status
      INTEGER IKNOTS            ! Number of knots used
      INTEGER IOPT              ! Spline fitting option
      INTEGER IPWRK             ! Pointer to workspace
      INTEGER IPWRK2            ! Spline work array
      INTEGER IPWRK3            ! Spline work array
      INTEGER IRANK( NDATMAX )  ! Rank vector for XD
      INTEGER LWRK              ! Length of work array
      INTEGER NDATFIT           ! Number of data used in the fit
      INTEGER NDEG              ! Order of actual fit
      INTEGER NEST              ! Maximum number of knots in fit
      INTEGER NPOLY             ! Number of knots
      INTEGER SFD               ! Spline file descriptor
      INTEGER PFD               ! Polynomial coefficients file descriptor
      LOGICAL OPEN              ! True when spline file is open
      LOGICAL USEWEI            ! True when using weights
      REAL EPSR                 ! Spline fit estimate
      REAL KNOTS( PON__NEST )   ! Position of knots
      REAL SFACT                ! Smoothing factor for spline fit
      REAL SPLINE( PON__NEST )  ! Coefficients of splines
      REAL WR( NDATMAX )        ! Weights
      REAL XC                   ! X check position
      REAL XMAXP                ! Maximum X value in plot
      REAL XMINP                ! Minimum X value in plot
      REAL XR( NDATMAX )        ! X-axis data
      REAL YR( NDATMAX )        ! Y-axis data
*.

*  Check inherited global status.
      IF ( STATUS.NE.SAI__OK ) RETURN

*  Get the action value.
      CALL PAR_GET0C( 'ACTION', ACTION, STATUS )
      CALL CHR_UCASE( ACTION )
      CALL CHR_LDBLK( ACTION )

*  The polynomial order or number of knots.
      CALL PAR_GET0I( 'NPOLY', NPOLY, STATUS )
      CALL PAR_GET0L( 'WEIGHT', USEWEI, STATUS )
      CALL PAR_GET0R( 'XMIN', XMINP, STATUS )
      CALL PAR_GET0R( 'XMAX', XMAXP, STATUS )

*  Check the value of NPOLY to make sure that it is both non-zero and
*  positive.
      IF ( NPOLY .LT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NPOLY', NPOLY )
         CALL ERR_REP( 'FITCURVE_NPOLYV',
     :                 'Order of polynomial knots is invalid: ^NPOLY.',
     :                 STATUS )
         GO TO 999
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*  Check whether a weighted fit is both required and possible, and
*  inform the user if it is required but no weights are available.
      IF ( USEWEI .AND. ( ERYCOL.EQ.0 ) ) THEN
         CALL MSG_OUT( ' ',
     :        'No error data available, using uniform weight.', STATUS )
      END IF

*  Load the fit arrays. Only use X data within the bounds we're given.
      NDATFIT = 0
      DO 100 I = 1, NDAT
         IF ( XDATA( I ) .GE. XMINP .AND. XDATA( I ) .LE. XMAXP )
     :   THEN
            NDATFIT = NDATFIT + 1
            XD( NDATFIT ) = XDATA( I )
            YD( NDATFIT ) = YDATA( I )

            IF ( ERYCOL .EQ. 0 ) THEN
               WD( NDATFIT ) = 1.0D+00
            ELSE

*     Check whether statistical weights are required.
               IF ( USEWEI ) THEN

*     Check if the Y-axis data have been converted to logarithms
*     and convert their associated errors, if necessary.
                  IF ( LYLOG ) THEN
                     NEWSIG = ABS( LOG10E * DBLE( ERRY( I ) ) /
     :                             REAL( YD( NDATFIT ) ) )
                  ELSE
                     NEWSIG = DBLE( ERRY( I ) )
                  END IF

                  WD( NDATFIT ) = 1.0D+00 / NEWSIG**2
               ELSE
                  WD( NDATFIT ) = 1.0D+00
               END IF
            END IF
         END IF
 100  CONTINUE

*  Check we have some points.
      IF ( NDATFIT .LE. 1 ) THEN
         STATUS = SAI__ERROR
         IF ( NDATFIT .EQ. 1 ) THEN
            CALL ERR_REP( 'FITCURVE_ONEPNT',
     :      'There is only one selected point.', STATUS )
         ELSE
            CALL ERR_REP( 'FITCURVE_NOPNTS',
     :      'There are no selected points.', STATUS )
         END IF
         GO TO 999
      END IF

*  Sort the fit arrays into ascending order of XD.
      CALL PON_ISORT( NDATFIT, XD, IRANK, STATUS )
      CALL PON_SORTV( NDATFIT, IRANK, DWORK, XD, STATUS )
      CALL PON_SORTV( NDATFIT, IRANK, DWORK, YD, STATUS )
      CALL PON_SORTV( NDATFIT, IRANK, DWORK, WD, STATUS )

*  Fit a polynomial to the data.
      IF ( ACTION .EQ. 'POLY' ) THEN

*     Get the number of parameters used in the fit.
         NPOLY = NPOLY + 1

*     Check whether the nubmer of fit parameters is too high. If it is,
*     too high, report an error and abort. Make a report about this
*     and set it to a usable value.
         IF ( NPOLY .GE. NDATFIT ) THEN
            CALL MSG_SETI( 'MAXPOL', NDATFIT - 1 )
            CALL MSG_OUT( 'FITCURVE_NHIGH',
     :           'The order of the polynomial is too high - it is now'//
     :           ' set to the maximum of ^MAXPOL.', STATUS )
            NPOLY = NDATFIT
         ELSE IF ( NPOLY .GT. MAXPOLY .OR. NPOLY .EQ. NDATFIT ) THEN
            CALL MSG_SETI( 'MAXPOL', MAXPOLY - 1 )
            CALL MSG_OUT( 'FITCURVE_PHIGH',
     :           'The order of the polynomial is too high - it is now'//
     :           ' set to the maximum of ^MAXPOL.', STATUS )
            NPOLY = MAXPOLY
         END IF

*     Perform the polynomial fit.
         CALL PSX_CALLOC( 3 * NDATFIT + 3 * NPOLY + 3, '_DOUBLE',
     :                    IPWRK, STATUS )
         EPS = 0.0D0
         NPOLY = NPOLY - 1
         IFAIL2 = 0
         CALL ERR_MARK
         CALL PDA_DPOLFT( NDATFIT, XD, YD, WD, NPOLY, NDEG,
     :                    EPS, THEFIT, IFAIL1, %VAL( CNF_PVAL( IPWRK )),
     :                    IFAIL2 )

*     Check if the fit has been done.
         IF ( NDEG .NE. NPOLY .OR. IFAIL1 .NE. 1 )THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FITCURVE_POLY',
     :           'Polynomial fitting routine failed', STATUS )
            GO TO 999
         ELSE IF ( IFAIL2 .NE. 0 ) THEN

*     Fit may have problems, but has been done anyway. Accept this and
*     continue.
            CALL ERR_ANNUL( IFAIL2 )
         END IF
         CALL ERR_RLSE

*     Print the fit results (extract the coefficients for polynomial
*     first).
         CALL PDA_DPCOEF( NPOLY, 0.0D0, COEFF,
     :                  %VAL( CNF_PVAL( IPWRK ) ), STATUS )
         CALL PON_SPOL( XMINP, XMAXP, NDATFIT, USEWEI, NPOLY, COEFF,
     :                  EPS, STATUS )
         CALL PSX_FREE( IPWRK, STATUS )

*     Plot the fit results. Note we trap the X range to be at most
*     the size of the display area to make sure the step size is
*     likely to be good.
         IF ( PON_DEVOP( .FALSE., STATUS ) ) THEN
            CALL PAR_GET0I( 'COLOUR', COLIDX, STATUS )
            CALL PGQCI( COLSAV )
            CALL PGSCI( COLIDX )
            XMINP = MAX( XMINP, XMIN )
            XMAXP = MIN( XMAXP, XMAX )
            CALL PON_PLOTPOLY( XMINP, XMAXP, NPOLY + 1, COEFF )
            CALL PGSCI( COLSAV )
         ELSE
            CALL MSG_OUT( ' ',
     :           'Unable to plot results -- no device open', STATUS )
         END IF

*     Put the polynomial coefficients back into the parameters.
         CALL PAR_PUT1D( 'POLYCOEF', NPOLY + 1, COEFF, STATUS )
         CALL PAR_PUT0I( 'NPOLY', NPOLY, STATUS )

*     And write them to a file also (IRAF cannot handle long strings).
         CALL PON_ASFIO( 'POLYFILE', 'WRITE', 'LIST', 0, PFD, OPEN,
     :                   STATUS )
         WRITE( OUTBUF, '( X, I6 )' ) NPOLY
         CALL FIO_WRITE( PFD, OUTBUF, STATUS )
         DO 181 I = 1, NPOLY + 1
            WRITE( OUTBUF, '( X, G25.16 )' ) COEFF( I )
            CALL FIO_WRITE( PFD, OUTBUF, STATUS )
 181     CONTINUE
         IF ( OPEN ) CALL FIO_CLOSE( PFD, STATUS )

*  Spline fitting.
      ELSE IF ( ACTION .EQ. 'SPLINE' ) THEN

*     Use an automatic spline placing routine, just allow the user to
*     select the number of knots used.

*     Get the "smoothing" factor. Offer a dynamic default mid-range of
*     that suggested in the PDA_CURFIT documentation. Note SFACT=0.0 is
*     equivalent to interpolation.
         SFACT = REAL( NDATFIT )
         CALL PAR_DEF0R( 'SMOOTH', SFACT, STATUS )
         CALL PAR_GET0R( 'SMOOTH', SFACT, STATUS )
         SFACT = MAX( 0.0, SFACT )

*     Check whether the degree of the splines isn't too high. If it is,
*     too high, make a report and truncate.
         IF ( NPOLY .GT. 6 ) THEN
            CALL MSG_OUT( 'FITCURVE_OHIGH',
     :           'The requested order of the splines is too ' //
     :           'high -  now set to 6', STATUS )
            NPOLY = 6
         END IF
         IF ( NPOLY .LT. 2 ) THEN
            CALL MSG_OUT( 'FITCURVE_OLOW',
     :           'The requested order of the splines is too ' //
     :           'low - now set to 2', STATUS )
            NPOLY = 2
         END IF
         NPOLY = NPOLY - 1

*     Data are of type REAL so transform existing positions and weights.
*     Also check that no adjacent X positions are the same this is not
*     allowed.
         XC = XD( NDATFIT )
         DO 11 I = 1, NDATFIT
            XR( I ) = REAL( XD( I ) )
            YR( I ) = REAL( YD( I ) )
            WR( I ) = REAL( WD( I ) )
            IF ( XC .EQ. XR( I ) ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'FITCURVE_NOMULT', 'You cannot fit'//
     :              ' splines to data that have more than one point '//
     :              'at an X position', STATUS )
               GO TO 999
            END IF
            XC = XR( I )
 11      CONTINUE

*     Want routine to choose it's own knots.
         IOPT = 0

*     Allocate all necessary workspace and fit curve.
         NEST = NDATFIT + NPOLY + 1
         CALL PSX_CALLOC( NEST, '_INTEGER', IPWRK2, STATUS )
         LWRK = NDATFIT * ( NPOLY + 1 ) + NEST * ( 7 + 3 * NPOLY )
         CALL PSX_CALLOC( LWRK, '_REAL', IPWRK3, STATUS )

         CALL PDA_CURFIT( IOPT, NDATFIT, XR, YR, WR, XMINP, XMAXP,
     :                    NPOLY, SFACT, NEST, IKNOTS, KNOTS,
     :                    SPLINE, EPSR, %VAL( CNF_PVAL ( IPWRK3 ) ),
     :                    LWRK, %VAL( CNF_PVAL( IPWRK2 ) ), IFAIL1 )

         CALL PSX_FREE( IPWRK2, STATUS )
         CALL PSX_FREE( IPWRK3, STATUS )

*     Check the success or otherwise of the routine.
         IF ( IFAIL1 .GT. 0 ) THEN
            CALL MSG_SETI( 'IFAIL', IFAIL1 )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FITCURVE_SPLINE',
     :           'Spline fitting routine failed (^IFAIL), try another'//
     :           ' value for smoothing parameter ', STATUS )
            GO TO 999
         END IF

*     Now plot the spline.
         CALL PAR_GET0I( 'COLOUR', COLIDX, STATUS )
         CALL PGQCI( COLSAV )
         CALL PGSCI( COLIDX )
         CALL PON_PLOTSPLINE( IKNOTS, KNOTS, SPLINE, NPOLY, STATUS )
         CALL PGSCI( COLSAV )

*     And save the description to a "spline" file.
         CALL PON_ASFIO( 'SPLINEFILE', 'WRITE', 'LIST', 0, SFD, OPEN,
     :                   STATUS )
         WRITE( OUTBUF, '( X, I6, I6 )' ) IKNOTS, NPOLY
         CALL FIO_WRITE( SFD, OUTBUF, STATUS )
         DO 180 I = 1, IKNOTS
            WRITE( OUTBUF, '( X, 2( G25.16, X ) )' ) KNOTS(I), SPLINE(I)
            CALL FIO_WRITE( SFD, OUTBUF, STATUS )
 180     CONTINUE

*     Finally make a report about the fit "statistics".
         CALL PON_SSPL( XMINP, XMAXP, NDATFIT, USEWEI, NPOLY, EPSR,
     :                  IKNOTS, SFD, SFACT, STATUS )
         IF ( OPEN ) CALL FIO_CLOSE( SFD, STATUS )
      ELSE

*     Action not known.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FITCURVE_NOACT',
     :        'The action given is not known.', STATUS )
      END IF

*  Abort.
 999  CONTINUE

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'FITCURVE_END',
     :                              'FITCURVE: Unable to fit a ' //
     :                              'curve to the data.', STATUS )

      END
* $Id$
