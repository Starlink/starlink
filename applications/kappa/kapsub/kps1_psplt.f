      SUBROUTINE KPS1_PSPLT( NBIN, SIGMA, AXISR, AMP, GAMMA, BACK,
     :                       SCALE, YSCALE, RUNITS, YUNITS, PNMIN,
     :                       PROFIL, PROFR, PROFWT, WORK, STATUS )
*+
*  Name:
*     KPS1_PSPLT

*  Purpose:
*     Plots a point-spread function radial profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PSPLT( NBIN, SIGMA, AXISR, AMP, GAMMA, BACK, SCALE,
*                      YSCALE, RUNITS, YUNITS, PNMIN, PROFIL, PROFR,
*                      PROFWT, WORK, STATUS )

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
*     YSCALE = REAL (Given)
*        The scale factor to convert profile values into displayed Y axis
*        values.
*     RUNITS = CHARACTER * ( * ) (Given)
*        The units of the radial profile after applying argument SCALE
*        to the pixel steps.  It gets used to make the default X axis
*        label in the plot.
*     YUNITS = CHARACTER * ( * ) (Given)
*        The units of the data value axis after applying argument YSCALE.
*        It gets used to make the default Y axis label in the plot.
*     PNMIN = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to decide
*        whether to plot the profile along the minor or major axis.
*     PROFIL( NBIN ) = REAL (Given and Returned)
*        On input these are the mean-profile values in each bin.  On
*        exit they become the scaled, fitted profile values.
*     PROFR( NBIN ) = REAL (Given and Returned)
*        On input these are the mean-profile radii in each bin.  On
*        exit they become the radii of the fitted profile.
*     PROFWT( NBIN ) = REAL (Given and Returned)
*        On input these are the weights of the mean profile.  On exit
*        they are zero.
*     WORK( 0:NBIN-1, 2 ) = DOUBLE PRECISION (Given and Returned)
*        Work array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This is a server routine for PSF via KPS1_SPARx.

*  Environment Parameters:
*     The following environment parameter names are used by this
*     routine:
*
*     PROFOUT = NDF (Write)
*        The name of a 1-dimensional NDF to be created holding the profile
*        data. The DATA component holds the fitted profile values, and the
*        VARIANCE component holds the square of the residuals. If a null (!)
*        value is supplied, no NDF is created.

*  Copyright:
*     Copyright (C) 1991-1993 Science & Engineering Research Council.
*     Copyright (C) 1999-2001, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008, 2010 Science and Technology Faciities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (Durham Univ.)
*     MJC: Malcolm J. Currie (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
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
*     2-MAY-2000 (DSB):
*        Added argument YSCALE.
*     17-MAY-2000 (DSB):
*        Added YUNITS argument.
*     10-JUL-2001 (DSB):
*        Added facilities for creating an 1D output NDF holding the profile.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     25-OCT-2005 (DSB):
*        Free resources allocated by KPG1_ASPSY.
*     2008 June 17 (MJC):
*        Trim trailing blanks from output NDF character components.
*     2010 October 14 (MJC):
*        Permit temporary style attributes.
*     16-JUN-2011 (DSB):
*        Added BSCALE argument to KPG1_GRAPH. BSCALE is currently unused
*        by NORMALIZE. At some point NORMALIZE should be changed to handle
*        data that exceeds the range of a REAL (as has been done to
*        HISTOGRAM).
*     20-MAR-2020 (DSB):
*        Changed KPG1_GRAPH API.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ definitions
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NBIN
      REAL SIGMA
      REAL AXISR
      REAL AMP
      REAL GAMMA
      REAL BACK
      REAL SCALE
      REAL YSCALE
      CHARACTER RUNITS * ( * )
      CHARACTER YUNITS * ( * )
      CHARACTER PNMIN * ( * )

*  Arguments Given and Returned:
      REAL PROFIL( 0:NBIN - 1 )
      REAL PROFR( 0:NBIN - 1 )
      REAL PROFWT( 0:NBIN - 1 )
      DOUBLE PRECISION WORK( 0:NBIN - 1 , 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER DEFLBX*72        ! Default X axis label
      CHARACTER DEFLBY*72        ! Default Y axis label
      CHARACTER DEFTTL*72        ! Default title
      CHARACTER NDFLBX*72        ! X axis label for output NDF
      CHARACTER NDFLBY*72        ! Y axis label for output NDF
      DOUBLE PRECISION BSCALE( 2 )! Scaling for plot labels
      INTEGER BIN                ! Bin counter for a star
      INTEGER EL                 ! The number of mapped values
      INTEGER IATTTL             ! Length of title
      INTEGER IATX               ! Length of X axis label
      INTEGER IATY               ! Length of Y axis label
      INTEGER INDF               ! Identifier for output NDF
      INTEGER IPAX               ! Pointer to NDF AXIS CENTRE array
      INTEGER IPDAT              ! Pointer to NDF data array
      INTEGER IPVAR              ! Pointer to NDF variance array
      INTEGER IPLOT              ! AST Plot for plotting
      INTEGER NC                 ! Number of used characters in string
      INTEGER NDATA              ! Number points to plot in mean profile
      LOGICAL MINOR              ! Plot profile along the minor axis?
      REAL AXSIG                 ! Sigma along an axis
      REAL DMAX                  ! Maximum data value to plot
      REAL DMIN                  ! Minimum data value to plot
      REAL DRANGE                ! Range of data values to plot
      REAL RADIUS                ! Current mean profile radius
      REAL RAXIS                 ! Profile axis-ratio normalisation
      REAL RMAX                  ! Maximum radius mean profile
      REAL XL                    ! Left X limit
      REAL XR                    ! Right X limit
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the X axis type.
      CALL PAR_GTD0L( PNMIN, .TRUE., .TRUE., MINOR, STATUS )

*  Get the plot X axis label and set the axis sigma.
      DEFLBX = ' '
      IATX = 0

      IF ( MINOR ) THEN
         CALL CHR_APPND( 'Minor-axis Distance', DEFLBX, IATX )
         AXSIG = SIGMA
         RAXIS = 1.0
      ELSE
         CALL CHR_APPND( 'Major-axis Distance', DEFLBX, IATX )
         AXSIG = SIGMA * AXISR
         RAXIS = AXISR
      END IF

      NDFLBX = DEFLBX( : IATX )

      IF ( RUNITS .NE. ' ' ) THEN
         CALL CHR_APPND( ' (', DEFLBX, IATX )
         CALL CHR_APPND( RUNITS, DEFLBX, IATX )
         CALL CHR_APPND( ')', DEFLBX, IATX )
      END IF

*  Get the plot Y axis label.
      DEFLBY = 'Intensity'
      IATY = 9

      NDFLBY = DEFLBY( : IATY )

      IF ( YUNITS .NE. ' ' ) THEN
         CALL CHR_APPND( ' (', DEFLBY, IATY )
         CALL CHR_APPND( YUNITS, DEFLBY, IATY )
         CALL CHR_APPND( ')', DEFLBY, IATY )
      END IF

*  Initialise max and min data values to be included in plot.
      DMAX = VAL__MINR
      DMIN = VAL__MAXR

*  Initialise counter of good bins.
      NDATA = -1

*  Loop through all the bins in the mean profile, scaling the data to the
*  units. Also compress the data arrays to remove empty bins.
      DO BIN = 0, NBIN - 1
         IF ( PROFWT( BIN ) .GT. 0.0 ) THEN
            NDATA = NDATA + 1
            PROFIL( NDATA ) = YSCALE*( PROFIL( BIN ) - BACK )
            PROFR( NDATA ) = PROFR( BIN ) * SCALE * RAXIS
            PROFWT( NDATA ) = 0.0

            DMAX = MAX( DMAX, PROFIL( NDATA ) )
            DMIN = MIN( DMIN, PROFIL( NDATA ) )
         END IF
      END DO

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an output NDF to hold the profile.
      CALL LPG_CREAT( 'PROFOUT', '_REAL', 1, 1, NDATA + 1, INDF,
     :                STATUS )

*  If a null was supplied, annul the error.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Otherwise, produce the NDF.
      ELSE

*  Store the fit values corresponding to each bin in work(,1) and the
*  squared residual between the fit value and the data in work(,2)
         DO BIN = 0, NDATA
            RADIUS= PROFR( BIN )/SCALE
            WORK( BIN, 1 ) = YSCALE*AMP * EXP( - 0.5 * ( ( RADIUS /
     :                       MAX( 0.001, AXSIG ) ) *  * GAMMA ) )
            WORK( BIN, 2 ) = ( WORK( BIN, 1 ) - PROFIL( BIN ) )**2
         END DO

*  Map the DATA array, and copy the fit values into it.
         CALL NDF_MAP( INDF, 'DATA', '_DOUBLE', 'WRITE', IPDAT, EL,
     :                 STATUS )
         CALL KPG1_CPNDD( 1, 0, NDATA, WORK( 0, 1 ), 0, NDATA,
     :                    %VAL( CNF_PVAL( IPDAT ) ), EL, STATUS )

*  Map the VARIANCE array, and copy the squared residuals into it.
         CALL NDF_MAP( INDF, 'VARIANCE', '_DOUBLE', 'WRITE', IPVAR, EL,
     :                 STATUS )
         CALL KPG1_CPNDD( 1, 0, NDATA, WORK( 0, 2 ), 0, NDATA,
     :                    %VAL( CNF_PVAL( IPVAR ) ), EL, STATUS )

*  Map the AXIS CENTRE array, and copy the radii values to it.
         CALL NDF_AMAP( INDF, 'CENTRE', 1, '_REAL', 'WRITE', IPAX, EL,
     :                  STATUS )
         CALL KPG1_CPNDR( 1, 0, NDATA, PROFR, 0, NDATA,
     :                    %VAL( CNF_PVAL( IPAX ) ),
     :                    EL, STATUS )

      END IF

*  Calculate the fitted profile over the data range for each of the points
*  where the mean profile is known.  Apply the scaling to the radius.
*  Update the extreme data values to be plotted.
      RMAX = PROFR( NDATA ) / SCALE
      DO BIN = 0, NBIN - 1
         RADIUS = ( RMAX * BIN ) / REAL( NBIN - 1 )
         WORK( BIN, 1 ) = RADIUS * SCALE
         WORK( BIN, 2 ) = YSCALE*AMP * EXP( - 0.5 * ( ( RADIUS /
     :                          MAX( 0.001, AXSIG ) ) *  * GAMMA ) )

         DMAX = MAX( DMAX, REAL( WORK( BIN, 2 ) ) )
         DMIN = MIN( DMIN, REAL( WORK( BIN, 2 ) ) )

      END DO

*  Extend the default Y axis limits slightly.
      DRANGE = DMAX - DMIN
      DMAX = DMAX + 0.05*DRANGE
      DMIN = DMIN - 0.05*DRANGE

*  Store the default plot title.
      DEFTTL = 'Mean Star Profile'
      IATTTL = 17

*  Plot the binned data.
      IPLOT = AST__NULL
      BSCALE( 1 ) = 1.0D0
      BSCALE( 2 ) = 1.0D0
      XL = 0.0
      XR = VAL__BADR
      CALL KPG1_GRAPH( NDATA + 1, PROFR, PROFIL, 0.0, 0.0,
     :                 DEFLBX( : IATX ), DEFLBY( : IATY ),
     :                 DEFTTL( : IATTTL ), 'XDATA', 'YDATA', 3,
     :                 .TRUE., 'KAPPA_PSF', .TRUE., .FALSE.,
     :                 BSCALE, IPLOT, XL, XR, DMIN, DMAX, STATUS )

*  Only proceed if a plot was produced.
      IF ( IPLOT .NE. AST__NULL ) THEN

*  Save the title actually used.
         DEFTTL = AST_GETC( IPLOT, 'TITLE', STATUS )

*  If the default X axis label has not been overridden, use the version
*  without the units.
         IF ( DEFLBX .NE. AST_GETC( IPLOT, 'LABEL(1)', STATUS ) ) THEN
            NDFLBX = AST_GETC( IPLOT, 'LABEL(1)', STATUS )
         END IF

*  If the default Y axis label has not been overridden, use the version
*  without the units.
         IF ( DEFLBY .NE. AST_GETC( IPLOT, 'LABEL(2)', STATUS ) ) THEN
            NDFLBY = AST_GETC( IPLOT, 'LABEL(2)', STATUS )
         END IF

*  Set up the plotting characteristics to use when drawing the line.
         CALL KPG1_ASPSY( '(LIN*ES)', '(CURVES)', STATUS )
         CALL KPG1_ASSET( 'KAPPA_PSF', '+STYLE', IPLOT, STATUS )

*  Plot the fitted function.
         CALL AST_POLYCURVE( IPLOT, NBIN, 2, NBIN, WORK, STATUS )

*  Annul the Plot, and shut down the graphics workstation, and database.
         CALL AST_ANNUL( IPLOT, STATUS )
         CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  Free resources.
         CALL KPG1_ASPSY( ' ', ' ', STATUS )

      END IF

*  If an output NDF is being created...
      IF ( INDF .NE. NDF__NOID ) THEN

*  Store labels, etc.  Note that NDF_CPUT and NDF_ACPUT do not truncate
*  trailing blanks.
         NC = CHR_LEN( DEFTTL )
         CALL NDF_CPUT( DEFTTL( :NC ), INDF, 'TITLE', STATUS )

         NC = CHR_LEN( NDFLBY )
         CALL NDF_CPUT( NDFLBY( :NC ), INDF, 'LABEL', STATUS )

         IF ( YUNITS .NE. ' ' ) THEN
            NC = CHR_LEN( YUNITS )
            CALL NDF_CPUT( YUNITS( :NC ), INDF, 'UNITS', STATUS )
         END IF

         NC = CHR_LEN( NDFLBX )
         CALL NDF_ACPUT( NDFLBX( :NC ), INDF, 'LABEL', 1, STATUS )

         IF ( RUNITS .NE. ' ' ) THEN
            NC = CHR_LEN( RUNITS )
            CALL NDF_ACPUT( RUNITS( :NC ), INDF, 'UNITS', 1, STATUS )
         END IF

*  Annul the NDF identifier.
         CALL NDF_ANNUL( INDF, STATUS )

      END IF

*  Copy the profile to the returned arrays.
      DO BIN = 0, NBIN - 1
         PROFR( BIN ) = REAL( WORK( BIN, 1 ) )
         PROFIL( BIN ) = REAL( WORK( BIN, 2 ) )
      END DO

 999  CONTINUE

      END
