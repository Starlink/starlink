      SUBROUTINE KPS1_PSPLT( NBIN, SIGMA, AXISR, AMP, GAMMA, BACK,
     :                       SCALE, RUNITS, PNCLER, PNDEV, PNMIN,
     :                       PNXSIZ, PNYSIZ, PNTIT, PNABSL, PNORDL,
     :                       PNMINT, PNMAJT, PNOUTT, PNFONT, COMMNT,
     :                       PROFIL, PROFR, PROFWT, STATUS )
*+
*  Name:
*     KPS1_PSPLT

*  Purpose:
*     Plots a point-spread function radial profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PSPLT( NBIN, SIGMA, AXISR, AMP, GAMMA, BACK, SCALE,
*                      RUNITS, PNCLER, PNDEV, PNMIN, PNXSIZ, PNYSIZ,
*                      PNTIT, PNABSL, PNORDL, PNMINT, PNMAJT, PNOUTT,
*                      PNFONT, COMMNT, PROFIL, PROFR, PROFWT, STATUS )

*  Description:
*     This routine plots the mean point-spread-function profile along
*     the minor or major axis.  The graph comprises the mean profile at
*     each bin with its associated error bar, and the smooth fit to
*     those data.
*
*     There a number of ADAM parameters which can be set to tailor the
*     style of the plot.  The default title is 'Mean Star Profile'; and
*     the axis labels default to  'Minor-axis Distance (^RUNITS)'
*     where ^RUNITS is replaced by the value of argument RUNITS, and
*     'Intensity'. 'Minor' is replaced by 'Major' when plotting the
*     major-axis profile.

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
*        to the pixel steps.  It gets used to make the default abscissa
*        label in the plot.
*     PNCLER = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to see if the
*        user wishes the display to be cleared.
*     PNDEV = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be associated with
*        a graphics workstation.
*     PNMIN = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to decide
*        whether to plot the profile along the minor or major axis.
*     PNXSIZ = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the 
*        required x extent of the FRAME picture in metres.
*     PNYSIZ = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the 
*        required y extent of the FRAME picture in metres.
*     PNTIT = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the
*        plot title.
*     PNABSL = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the
*        plot abscissa axis label.
*     PNORDL = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the
*        plot ordinate axis label.
*     PNMINT = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the 
*        numbers of minor tick marks per major tick. 
*     PNMAJT = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the 
*        numbers of major tick marks.
*     PNOUTT = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to determine
*        whether the ticks should appear inside or outside the grid
*        region of the plot.
*     PNFONT = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to determine
*        the fount to be used in the plot.
*     COMMNT = CHARACTER * ( * ) (Given)
*        A comment to store in the AGI database with the FRAME and
*        DATA pictures.
*     PROFIL( NBIN ) = REAL (Given and Returned)
*        On input these are the mean-profile values in each bin.  On
*        exit they become the fitted profile values.
*     PROFR( NBIN ) = REAL (Given and Returned)
*        On input these are the mean-profile radii in each bin.  On
*        exit they become the radii of the fitted profile.
*     PROFWT( NBIN ) = REAL (Given and Returned)
*        On input these are the weights of the mean profile.  On exit
*        they are zero.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If graphics are selected two pictures are stored in the 
*     graphics database: a FRAME picture of the requested size, and a
*     DATA picture for the grid, with radius and profile world
*     co-ordinates.
*     -  This is a server routine for PSF via KPS1_SPARx.

*  Algorithm:
*     -  Open the workstation and graphics database.  Create a frame
*     zone of the required size, and save it as a FRAME picture in the
*     database.  Exit if the workstation was null, cancelling its
*     associated parameter.
*     -  Obtain the plot-style functions.
*     -  Compress the mean profile to remove empty bins.  Remove the
*     background from each profile value.  Find the the range of values
*     and radii.
*     -  Plot the mean profile in the bins with error bars.  Save the
*     current graph zone as a DATA picture in the database.
*     -  Calculate the fitted profile over the data range.  Plot it.
*     -  Close down the database.

*  Authors:
*     RFWS: R.F. Warren-Smith (Durham Univ.)
*     MJC: Malcolm J. Currie (STARLINK)
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
*        Added additional plotting parameters, and allowed the abscissa
*        to be scaled to non-pixel units.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions

*  Arguments Given:
      INTEGER
     :  NBIN

      REAL
     :  SIGMA,
     :  AXISR,
     :  AMP,
     :  GAMMA,
     :  BACK,
     :  SCALE

      CHARACTER * ( * )
     :  COMMNT,
     :  RUNITS,
     :  PNCLER,
     :  PNDEV,
     :  PNMIN,
     :  PNXSIZ,
     :  PNYSIZ,
     :  PNTIT,
     :  PNABSL,
     :  PNORDL,
     :  PNMINT,
     :  PNMAJT,
     :  PNOUTT,
     :  PNFONT

*  Arguments Given and Returned:
      REAL
     :  PROFIL( 0:NBIN - 1 ),
     :  PROFR( 0:NBIN - 1 ),
     :  PROFWT( 0:NBIN - 1 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string
                                 ! excluding trailing blanks

*  Local Variables:
      REAL
     :  AXSIG,                   ! Sigma along an axis
     :  MAJTIC( 2 ),             ! Parameters controlling the numbers of
                                 ! major tick marks along x and y axes
                                 ! respectively
     :  MINTIC( 2 ),             ! Numbers of minor tick marks along x
                                 ! and y axes respectively
     :  RADIUS,                  ! Current mean profile radius
     :  RAXIS,                   ! Profile axis-ratio normalisation
     :  RMAX,                    ! Maximum radius mean profile
     :  TICDEF( 2 ),             ! Suggested default axis-tick values
     :  XMAX, YMAX,              ! Maximum x-y values for the plot
     :  XMIN, YMIN               ! Minimum x-y values for the plot

      INTEGER
     :  BIN,                     ! Bin counter for a star
     :  NDATA,                   ! Number points to plot in the mean
                                 ! profile
     :  PICID,                   ! Input picture identifier
     :  PICIDD,                  ! Data-picture identifier
     :  ZONE                     ! SGS zone associated with the frame
                                 ! picture

      LOGICAL                    ! True if:
     :  DEVCAN,                  ! Device is to be cancelled
     :  DEVOPN,                  ! Device is opened, i.e. plotting
                                 ! required
     :  MINOR,                   ! Plot profile along the minor axis
     :  OUTTIC                   ! Axis tick marks are to be placed
                                 ! outside the box instead of inside

      CHARACTER
     :  ABSLAB * ( 72 ),         ! Abscissa label
     :  DEFLAB * ( 72 ),         ! Default abscissa label
     :  FOUNT * 4,               ! Fount type
     :  ORDLAB * ( 72 ),         ! Ordinate label
     :  TITLE * ( 72 )           ! Plot title

*.

*     Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Start a new error context.

      CALL ERR_MARK

*    Initialise the flag to indicate that the device should not be
*    cancelled.

      DEVCAN = .FALSE.

*    Plot an error-bar graph of the data using the NCAR graphics
*    package.

      CALL NCROPN( PNCLER, PNDEV, PNXSIZ, PNYSIZ, COMMNT,
     :             DEVOPN, PICID, PICIDD, ZONE, STATUS )

*    Set the device cancel flag if something went wrong.

      IF ( STATUS .NE. SAI__OK .OR. .NOT. DEVOPN ) DEVCAN = .TRUE.

*    Get the abscissa type.

      CALL PAR_GTD0L( PNMIN, .TRUE., .TRUE., MINOR, STATUS )

*    Get the plot title.

      CALL PAR_GET0C( PNTIT, TITLE, STATUS )

*    Get the plot abscissa label and set the axis sigma.

      IF ( MINOR ) THEN
         DEFLAB = 'Minor-axis Distance ('/
     :            /RUNITS( :CHR_LEN( RUNITS ) )//')'
         AXSIG = SIGMA
         RAXIS = 1.0
      ELSE
         DEFLAB = 'Major-axis Distance ('/
     :            /RUNITS( :CHR_LEN( RUNITS ) )//')'
         AXSIG = SIGMA * AXISR
         RAXIS = AXISR
      END IF
      CALL PAR_DEF0C( PNABSL, DEFLAB, STATUS )
      CALL PAR_GET0C( PNABSL, ABSLAB, STATUS )

*    Get the plot ordinate label.

      CALL PAR_GET0C( PNORDL, ORDLAB, STATUS )

*    Get the number of minor ticks, assigning the dynamic defaults.

      TICDEF( 1 ) = -1.
      TICDEF( 2 ) = -1.
      CALL PAR_GDR1R( PNMINT, 2, TICDEF, -1., VAL__MAXR, .FALSE.,
     :                MINTIC, STATUS )

*    Get the parameter controlling the number of major ticks per
*    axis, assigning the dynamic defaults.

      TICDEF( 1 ) = 3.
      TICDEF( 2 ) = 3.
      CALL PAR_GDR1R( PNMAJT, 2, TICDEF, -1., VAL__MAXR, .FALSE.,
     :                MAJTIC, STATUS )

*    Are the tick marks on the outside of the axes?

      CALL PAR_GTD0L( PNOUTT, .TRUE., .TRUE., OUTTIC, STATUS )

*    Get the fount.  Although NCAR is the default, either must be
*    selected to prevent persistence from earlier invocations.

      CALL PAR_CHOIC( PNFONT, 'GKS', 'GKS,NCAR', .TRUE., FOUNT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( FOUNT .EQ. 'GKS ' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, -100 )
         ELSE IF ( FOUNT .EQ. 'NCAR' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, 100 )
         END IF
      END IF

*    A null return can prevent graphical output.  So save unnecessary
*    processing.

      IF ( STATUS .EQ. SAI__OK .AND. DEVOPN ) THEN

*       Initialise counter of good bins.

         NDATA = -1

*       Initialise minimum ans maximum plot co-ordinates.

         XMAX = VAL__MINR
         XMIN = VAL__MAXR
         YMAX = 1.1
         YMIN = -0.2

*       Loop through all the bins in the mean profile.

         DO BIN = 0, NBIN - 1

*          Compress the data arrays to remove empty bins and also find
*          maximum and minimum values to be plotted in x and y.  Scale
*          the data to the units.

            IF ( PROFWT( BIN ) .GT. 0.0 ) THEN
               NDATA = NDATA + 1
               PROFIL( NDATA ) = PROFIL( BIN ) - BACK
               PROFR( NDATA ) = PROFR( BIN ) * SCALE * RAXIS
               PROFWT( NDATA ) = 0.0

               XMAX = MAX( XMAX, PROFR( NDATA ) )
               XMIN = MIN( XMIN, PROFR( NDATA ) )
               YMAX = MAX( YMAX, PROFIL( NDATA ) + PROFWT( NDATA ) )
               YMIN = MIN( YMIN, PROFIL( NDATA ) - PROFWT( NDATA ) )
            END IF

         END DO

*       Plot the results.
*       =================

*       First the annotated axes and records the DATA picture.  There
*       will not be major tick marks at the ends of each axis.

         CALL NCRBCK( XMIN, XMAX, YMIN, YMAX, TITLE, ABSLAB, ORDLAB,
     :                MINTIC, MAJTIC, OUTTIC, .FALSE., COMMNT, PICID,
     :                PICIDD, STATUS )

*       Plot the profile points and error bars.

         CALL DREBAR( PROFR, PROFIL, PROFWT, NDATA + 1, STATUS )

*       Calculate the fitted profile over the data range for each of
*       the points where the mean profile is known.  Apply the scaling
*       to the radius.

         RMAX = PROFR( NDATA ) / SCALE

         DO BIN = 0, NBIN - 1
            RADIUS = ( RMAX * BIN ) / REAL( NBIN - 1 )
            PROFR( BIN ) = RADIUS * SCALE
            PROFIL( BIN ) = AMP * EXP( - 0.5 * ( ( RADIUS /
     :                      MAX( 0.001, AXSIG ) ) *  * GAMMA ) )
         END DO

*       Plot the fitted function.

         CALL AGCURV( PROFR, 1, PROFIL, 1, NBIN, 1 )
      END IF

*    Close down the database.

      CALL AGS_DEASS( PNDEV, DEVCAN, STATUS )

*    Release the error context.

      CALL ERR_RLSE

      END
