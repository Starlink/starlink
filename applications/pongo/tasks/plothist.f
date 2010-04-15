      SUBROUTINE PLOTHIST( STATUS )
*+
*  Name:
*     PLOTHIST

*  Purpose:
*     Plot a histogram of the data.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PLOTHIST( STATUS )

*  Description:
*     This application has two modes:
*     - Bin the data in the XCOL data area and plot the result.
*     - Plot data that have already been binned and provided in the XCOL
*     and YCOL data areas.

*  Usage:
*     plothist action [binmin] [binmax] [nbin]

*  ADAM Parameters:
*     ACTION = _CHAR (Read)
*        The mode of PLOTHIST as described above:
*
*        - "H" -- If the data in the XCOL data area are not binned, they
*        can be binned and then plotted.  It is possible to plot
*        several histograms with different bin sizes from the same data
*        in XCOL because the data are unaffected by PLOTHIST.
*        - "B" -- If the data have already been binned, this mode will
*        plot a histogram using the XCOL and YCOL data areas. The XCOL
*        data area should specify the bin edges and the YCOL data area
*        should contain their respective frequencies.
*
*        [The value is prompted for.]
*     BINMIN = _REAL (Read and Write)
*        When ACTION is "H", this parameter specifies the lower limit
*        of the binning.
*
*        If no value is specified on the command line, the current
*        value is used. If there is no current value, the value of the
*        global parameter PONGO_XMIN is used.
*     BINMAX = _REAL (Read and Write)
*        When ACTION is "H", this parameter specifies the upper limit of
*        the binning.
*
*        If no value is specified on the command line, the current
*        value is used. If there is no current value, the value of the
*        global parameter PONGO_XMAX.
*     FILL = _LOGICAL (Read)
*        When ACTION is "H" this parameter controls whether the
*        histogram will be drawn filled with the current fill, colour
*        and hatching styles.
*        [FALSE]
*     NBIN = _INTEGER (Read and Write)
*        When ACTION is "H", this parameter specifies the number of
*        equally sized bins to be drawn between the limits of the
*        histogram.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 10.
*     AUTOSCALE = _LOGICAL (Read and Write)
*        When ACTION is "H" this parameter specifies whether PGPLOT
*        auto-scaling is used to determine the plotting limits. If
*        FALSE, the limits defined by the bins of the histogram
*        determine the plotting limits. Here, the plotting limits must
*        previously have been set using the LIMITS application and the
*        plot frame drawn using BOXFRAME. Setting NOAUTOSCALE can be
*        used to draw more than one histogram on the same plot.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to TRUE.
*     CENTRE = _LOGICAL (Read)
*        When ACTION is "B", this parameter specifies whether the
*        values in the XCOL data area denote the centre of each bin
*        (when TRUE) or its lower edge (when FALSE).
*        [FALSE]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     21-JUN-1994 (PDRAPER):
*        Added check for device is open.
*     31-MAY-1996 (PDRAPER):
*        Added option to fill histogram.
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
      INCLUDE 'PONGO_CMN'        ! PONGO global varaibles

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      CHARACTER * ( 1 ) ACTION   ! Action to be taken

      INTEGER FLAG               ! Flag value
      INTEGER NBIN               ! Number of bins

      LOGICAL AUTOSCALE          ! Autoscale flag
      LOGICAL CENTRE             ! Whether Y values are bin centres
      LOGICAL FILL               ! Whether to fill histograms

      REAL XHIGH                 ! Limit of the histogram
      REAL XLOW                  ! Limit of the histogram

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that device is ready for plotting.
      IF ( .NOT. PON_DEVOP( .TRUE., STATUS ) ) GO TO 99

*  Find which type of histogram is required.
      CALL PAR_GET0C( 'ACTION', ACTION, STATUS )

*  Check the returned status and act.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_UCASE( ACTION )

*     Automatic binning of X data.
         IF ( ACTION .EQ. 'H' ) THEN

*        Get the limits of binning and the number of bins.
            CALL PAR_GET0R( 'BINMIN', XLOW, STATUS )
            CALL PAR_GET0R( 'BINMAX', XHIGH, STATUS )
            CALL PAR_GET0I( 'NBIN', NBIN, STATUS )

*        Get FLAG to determine whether axes are drawn automatically.
            CALL PAR_GET0L( 'AUTOSCALE', AUTOSCALE, STATUS )

*        See if we want to fill the histograms using the current styles?
            CALL PAR_GET0L( 'FILL', FILL, STATUS )

*        Check the returned status and act.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( AUTOSCALE .AND. FILL ) THEN
                  FLAG = 2
               ELSE IF ( FILL ) THEN
                  FLAG = 3
               ELSE IF ( AUTOSCALE ) THEN
                  FLAG = 0
               ELSE
                  FLAG = 1
               END IF

*           Convert the data to REAL to pass to PGHIST.
               CALL PON_CONVREAL( NDAT, XDATA, XDATATEMP )
               CALL PGHIST( NDAT, XDATATEMP, XLOW, XHIGH, NBIN, FLAG )
            END IF
         ELSE IF ( ACTION .EQ. 'B' ) THEN

*        Data already binned in X and Y arrays.
            CALL PAR_GET0L( 'CENTRE', CENTRE, STATUS )

*        Check the returned status and act.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL PON_CONVREAL( NDAT, XDATA, XDATATEMP )
               CALL PON_CONVREAL( NDAT, YDATA, YDATATEMP )
               CALL PGBIN( NDAT, XDATATEMP, YDATATEMP, CENTRE )
            END IF
         END IF
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
 99   CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'PLOTHIST_END',
     :                              'PLOTHIST: Cannot plot a ' //
     :                              'histogram of the data.', STATUS )

      END
* $Id$
