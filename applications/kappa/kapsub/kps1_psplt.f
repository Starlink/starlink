      SUBROUTINE KPS1_PSPLT( NBIN, SIGMA, AXISR, AMP, GAMMA, BACK,
     :                       SCALE, RUNITS, PNMIN, PROFIL, 
     :                       PROFR, PROFWT, WORK, STATUS )
*+
*  Name:
*     KPS1_PSPLT

*  Purpose:
*     Plots a point-spread function radial profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PSPLT( NBIN, SIGMA, AXISR, AMP, GAMMA, BACK, SCALE,
*                      RUNITS, PNMIN, PROFIL, PROFR, PROFWT, WORK,
*                      STATUS )

*  Description:
*     This routine plots the mean point-spread-function profile along
*     the minor or major axis.  The graph comprises the mean profile at
*     each bin and the smooth fit to those data.

*  Arguments:
*     NBIN = INTEGER (Given)
*        The number of radial-profile data points.
*     SIGMA = REAL (Given)
*        The Gaussian width (sigma) of the fitted point-spread function.
*     AXISR = REAL (Given)
*        The axis ratio of the fitted point-spread function.
*     AMP = REAL (Given)
*        The Gaussian amplitude of the fitted point-spread function.
*     GAMMA = REAL (Given)
*        Star radial-profile parameter, gamma, of the fitted profile.
*     BACK = REAL (Given)
*        The background level of the mean point-spread function.
*     SCALE = REAL (Given)
*        The scale factor to convert pixels to the physical units given
*        by argument RUNITS.  This factor is applied to the radial
*        distances in the plotted profile.
*     RUNITS = CHARACTER * ( * ) (Given)
*        The units of the radial profile after applying argument SCALE
*        to the pixel steps.  It gets used to make the default X axis
*        label in the plot.
*     PNMIN = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to decide
*        whether to plot the profile along the minor or major axis.
*     PROFIL( NBIN ) = REAL (Given and Returned)
*        On input these are the mean-profile values in each bin.  On
*        exit they become the fitted profile values.
*     PROFR( NBIN ) = REAL (Given and Returned)
*        On input these are the mean-profile radii in each bin.  On
*        exit they become the radii of the fitted profile.
*     PROFWT( NBIN ) = REAL (Given and Returned)
*        On input these are the weights of the mean profile.  On exit
*        they are zero.
*     WORK( NBIN, 2 ) = DOUBLE PRECISION (Given and Returned)
*        Work array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This is a server routine for PSF via KPS1_SPARx.

*  Authors:
*     RFWS: R.F. Warren-Smith (Durham Univ.)
*     MJC: Malcolm J. Currie (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 6 (MJC):
*        Original version based on earlier version of KPS1_RPRFx.
*     1991 August 20 (MJC):
*        Added PNFONT argument to specify plot fount.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1993 August 27 (MJC):
*        Added additional plotting parameters, and allowed the X axis
*        to be scaled to non-pixel units.
*     15-JUL-1999 (TDCA):
*        Converted graphics to AST/PGPLOT.
*     17-SEP-1999 (DSB):
*        Re-formated and generally tidied up.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ definitions
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER NBIN
      REAL SIGMA
      REAL AXISR
      REAL AMP
      REAL GAMMA
      REAL BACK
      REAL SCALE
      CHARACTER RUNITS * ( * )
      CHARACTER PNMIN * ( * )

*  Arguments Given and Returned:
      REAL PROFIL( 0:NBIN - 1 )
      REAL PROFR( 0:NBIN - 1 )
      REAL PROFWT( 0:NBIN - 1 )
      DOUBLE PRECISION WORK( 0:NBIN - 1 , 2 ) 

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DEFLAB*72        ! Default X axis label
      INTEGER BIN                ! Bin counter for a star
      INTEGER IAT                ! Length of X axis label
      INTEGER IPLOT              ! AST Plot for plotting
      INTEGER NDATA              ! Number points to plot in the mean profile
      LOGICAL MINOR              ! Plot profile along the minor axis?
      REAL AXSIG                 ! Sigma along an axis
      REAL DMAX                  ! Maximum data value to plot
      REAL DMIN                  ! Minimum data value to plot
      REAL DRANGE                ! Range of data values to plot
      REAL RADIUS                ! Current mean profile radius
      REAL RAXIS                 ! Profile axis-ratio normalisation
      REAL RMAX                  ! Maximum radius mean profile
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the X axis type.
      CALL PAR_GTD0L( PNMIN, .TRUE., .TRUE., MINOR, STATUS )

*  Get the plot X axis label and set the axis sigma.
      DEFLAB = ' '
      IAT = 0

      IF ( MINOR ) THEN
         CALL CHR_APPND( 'Minor-axis Distance', DEFLAB, IAT )
         AXSIG = SIGMA
         RAXIS = 1.0
      ELSE
         CALL CHR_APPND( 'Major-axis Distance', DEFLAB, IAT )
         AXSIG = SIGMA * AXISR
         RAXIS = AXISR
      END IF

      IF( RUNITS .NE. ' ' ) THEN
         CALL CHR_APPND( ' (', DEFLAB, IAT )
         CALL CHR_APPND( RUNITS, DEFLAB, IAT )
         CALL CHR_APPND( ')', DEFLAB, IAT )
      END IF

*  Initialise counter of good bins.
      NDATA = -1

*  Initialise max and min data values to be included in plot.
      DMAX = VAL__MINR
      DMIN = VAL__MAXR

*  Loop through all the bins in the mean profile.
      DO BIN = 0, NBIN - 1

*  Compress the data arrays to remove empty bins. Scale the data to the units.
         IF ( PROFWT( BIN ) .GT. 0.0 ) THEN
            NDATA = NDATA + 1
            PROFIL( NDATA ) = PROFIL( BIN ) - BACK
            PROFR( NDATA ) = PROFR( BIN ) * SCALE * RAXIS
            PROFWT( NDATA ) = 0.0

            DMAX = MAX( DMAX, PROFIL( NDATA ) )
            DMIN = MIN( DMIN, PROFIL( NDATA ) )

         END IF

      END DO

*  Calculate the fitted profile over the data range for each of the points 
*  where the mean profile is known.  Apply the scaling to the radius.
*  Update the extreme data values to be plotted.
      RMAX = PROFR( NDATA ) / SCALE
      DO BIN = 0, NBIN - 1
         RADIUS = ( RMAX * BIN ) / REAL( NBIN - 1 )
         WORK( BIN, 1 ) = RADIUS * SCALE
         WORK( BIN, 2 ) = AMP * EXP( - 0.5 * ( ( RADIUS /
     :                          MAX( 0.001, AXSIG ) ) *  * GAMMA ) )

         DMAX = MAX( DMAX, REAL( WORK( BIN, 2 ) ) )
         DMIN = MIN( DMIN, REAL( WORK( BIN, 2 ) ) )

      END DO

*  Extend the default Y axis limits slightly.
      DRANGE = DMAX - DMIN
      DMAX = DMAX + 0.05*DRANGE
      DMIN = DMIN - 0.05*DRANGE

*  Plot the binned data.
      CALL KPG1_GRAPH( NDATA + 1, PROFR, PROFIL, 0.0, 0.0, 
     :                 DEFLAB( : IAT ), 'Intensity', 
     :                 'Mean Star Profile', 'XDATA', 'YDATA', 3, 
     :                 .TRUE., 0.0, VAL__BADR, DMIN, DMAX, 
     :                 'KAPPA_PSF', .TRUE., IPLOT, STATUS ) 

*  Only proceed if a plot was produced.
      IF( IPLOT .NE. AST__NULL ) THEN

*  Set up the plotting characteristics to use when drawing the line.
         CALL KPG1_ASPSY( '(LIN*ES)', '(CURVES)', STATUS )
         CALL KPG1_ASSET( 'KAPPA_PSF', 'STYLE', IPLOT, STATUS )

*  Plot the fitted function.
         CALL AST_POLYCURVE( IPLOT, NBIN, 2, NBIN, WORK, STATUS )

*  Annul the Plot, and shut down the graphics workstation, and database.
         CALL AST_ANNUL( IPLOT, STATUS )
         CALL AGP_DEASS( 'DEVICE', .FALSE., STATUS )

      END IF

*  Copy the profile to the returned arrays.
      DO BIN = 0, NBIN - 1
         PROFR( BIN ) = WORK( BIN, 1 ) 
         PROFIL( BIN ) = WORK( BIN, 2 )
      END DO

      END
