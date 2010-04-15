      SUBROUTINE FITLINE( STATUS )
*+
*  Name:
*     FITLINE

*  Purpose:
*     Fit a straight line to the data.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     If the y-axis error data are available and the WEIGHT paramter is
*     TRUE, this application will perform a weighted least squares fit
*     to a straight line for the data over the range delimited by the
*     XMIN and XMAX parameters. If the y-axis error data are not
*     available or WEIGHT is FALSE, an unweighted fit is performed. The
*     best fit straight line is plotted. The resultant fit parameters
*     are displayed along with some simple statistics for the data
*     (these statistics are also weighted in the case of a weighted
*     fit).

*  Usage:
*     fitline [colour]

*  ADAM Parameters:
*     COLOUR = _INTEGER (Read and Write)
*        The colour index used when plotting the fitted line.
*
*        If the value is not specified on the command line, the current
*        value is used. If there is no current value, a default value of
*        2 (i.e. red) is used.
*     WEIGHT =_LOGICAL (Read and Write)
*        Whether the fit is to use the y-axis error data in the EYCOL
*        data area, if available. If no error data are available, the
*        fit will always be unweighted.
*
*        If the value is not specified on the command line, the current
*        value is used. If there is no current value, a default value of
*        TRUE is used.
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

*  Authors:
*     JBVAD::PAH: Paul Harrison(STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     27-NOV-1992 (PCTR):
*        Some changes for a new version of PON_STAT.
*     1-DEC-1992 (PCTR):
*        Added COLOUR parameter for continuity with FITCURVE.
*     7-DEC-1992 (PCTR):
*        Changed PON_SHOSTAT to PON_SHOLINE and added the arguments
*        WEIGHT and STATUS.
*     6-JUN-1994 (PDRAPER):
*        Removed unused arguments to PON_SHOLINE.
*     20-JUN-1994 (PDRAPER):
*        Added tests for device already open.
*     20-MAR-1995 (PDRAPER):
*        Now uses XMINP and XMAXP as fit range rather than XMIN and
*        YMIN. This behavior is as per the help description.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global parameters

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      LOGICAL WEIGHT             ! Whether weighted fit is required

      INTEGER WTMODE             ! Weight mode: > 0 is weighted
      INTEGER COLIDX             ! Colour index for fitted line
      INTEGER COLSAV             ! Default colour index

      REAL CHI2                  ! Chi-squared
      REAL CORRPROB              ! Correlation coefficient probability
      REAL INTCPT                ! Y-axis intercept
      REAL INTCPTSIG             ! Y-axis intercept error
      REAL FITPROB               ! Fit confidence
      REAL PCORR                 ! Pearson correlation coefficient
      REAL SLOPE                 ! Slope
      REAL SLOPESIG              ! Slope error
      REAL XMAXP                 ! X-axis plotting limit
      REAL XMEAN                 ! Mean of the X-axis data
      REAL XMINP                 ! X-axis plotting limit
      REAL XSIG                  ! Standard deviation of the X-axis data
      REAL YMEAN                 ! Mean of the Y-axis data
      REAL YSIG                  ! Standard deviation of the Y-axis data

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL PAR_GET0R( 'XMIN', XMINP, STATUS )
      CALL PAR_GET0R( 'XMAX', XMAXP, STATUS )
      CALL PAR_GET0I( 'COLOUR', COLIDX, STATUS )
      CALL PAR_GET0L( 'WEIGHT', WEIGHT, STATUS )

*  Check the inherited status and act.
      IF ( STATUS .EQ. SAI__OK ) THEN
         WTMODE = 0
         IF ( WEIGHT ) THEN
            IF ( ERYCOL .NE. 0 ) THEN

*           Use weighted fit mode.
               WTMODE = 1
            ELSE

*  Cannot use wieghted fit mode, inform the user that no error
*  data exist.
               CALL MSG_OUT( ' ',
     :                       'No error data available, using ' //
     :                       'uniform weight.',
     :                       STATUS)
            END IF
         END IF

         CALL PON_STAT( XDATA, YDATA, NDAT, ERRY, WTMODE, XMINP, XMAXP,
     :                  YMIN, YMAX, WORK, LYLOG, XMEAN, YMEAN,
     :                  XSIG, YSIG, INTCPT, SLOPE, INTCPTSIG, SLOPESIG,
     :                  CHI2, FITPROB, PCORR, CORRPROB, STATUS )

*  Check the inherited status.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Plot the best fit line.
            IF ( PON_DEVOP( .FALSE., STATUS ) ) THEN
               CALL PGMOVE( XMINP, INTCPT + SLOPE * XMINP )
               CALL PGQCI( COLSAV )
               CALL PGSCI( COLIDX )
               CALL PGDRAW( XMAXP, INTCPT + SLOPE * XMAXP )
               CALL PGSCI( COLSAV )
            ELSE
               CALL MSG_OUT( ' ', 'Unable to plot results -- device ' //
     :                            'not opened', STATUS )
            END IF

*  Report the statistics.
            CALL PON_SHOLINE( XMINP, XMAXP, YMIN, YMAX, NDAT, WEIGHT,
     :                        XMEAN, YMEAN, XSIG, YSIG, INTCPT, SLOPE,
     :                        INTCPTSIG, SLOPESIG, PCORR, STATUS )
         END IF
      END IF

*  Check the returned status and report an error, if necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'FITLINE_END',
     :                 'FITLINE: Unable to fit a straight line to ' //
     :                 'the data.', STATUS )

      END
* $Id$
