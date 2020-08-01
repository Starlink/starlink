      SUBROUTINE ELPROF( STATUS )
*+
*  Name:
*     ELPROF

*  Purpose:
*     Creates a radial or azimuthal profile of a 2-dimensional image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ELPROF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application will bin the input image into elliptical annuli,
*     or into a `fan' of adjacent sectors, centred on a specified
*     position.  The typical data values in each bin are found (see
*     Parameter ESTIMATOR), and stored in a one-dimensional NDF which
*     can be examined using LINPLOT, INSPECT, etc.  A two-dimensional
*     mask image can optionally be produced indicating which bin each
*     input pixel was placed in.
*
*     The area of the input image which is to be binned is the annulus
*     enclosed between the two concentric ellipses defined by Parameters
*     RATIO, ANGMAJ, RMIN, and RMAX. The binned area can be restricted to
*     an azimuthal section of this annulus using Parameter ANGLIM. Input
*     data outside the area selected by these parameters is ignored. The
*     selected area can be binned in two ways, specified by Parameter
*     RADIAL:
*
*     - If radial binning is selected (the default), then each bin is
*     an elliptical annulus concentric with the ellipses bounding the
*     binned area. The number of bins is specified by Parameter NBIN
*     and the radial thickness of each bin is specified by WIDTH.
*
*     - If azimuthal binning is selected, then each bin is a sector
*     (i.e. a wedge-shape), with its vertex given by parameters XC and
*     YC, and its opening angle given by Parameter WIDTH.  The number of
*     bins is specified by NBIN.

*  Usage:
*     elprof in out nbin xc yc

*  ADAM Parameters:
*     ANGLIM( 2 ) = _REAL (Read)
*        Defines the wedge-shaped sector within which binning is to be
*        performed.  The first value should be the azimuthal angle of
*        the clockwise boundary of the sector, and the second should be
*        the azimuthal angle of the anti-clockwise boundary.  The angles
*        are measured in degrees from the x-axis, and rotation from the
*        x-axis to the y-axis is positive.  If only a single value is
*        supplied, or if both values are equal, the sector starts at
*        the given angle and extends for 360 degrees. [0.0]
*     ANGMAJ = _REAL (Read)
*        The angle between the x-axis and the major axis of the
*        ellipse, in degrees.  Rotation from the x-axis to the y-axis is
*        positive.  [0.0]
*     ESTIMATOR = LITERAL (Read)
*        The method to use for estimating the output pixel values.  It
*        can be either "Mean" or "Weighted Mean". If the weighted mean
*        option is selected but no variances are available in the input
*        data, the unweighted mean will be used instead. ["Mean"]
*     IN = NDF (Read)
*        The input NDF containing the 2-dimensional image from which a
*        profile is to be generated.
*     MASK = NDF (Write)
*        An output NDF of the same shape and size as the input NDF
*        indicating the bin into which each input pixel was placed.
*        For radial profiles, the bins are identified by a mask value
*        equal to the radius (in pixels) measured on the major axis, at
*        the centre of the annular bin.  For azimuthal profiles, the
*        bins are identified by a mask value equal to the angle from
*        the x-axis to the centre of the sector-shaped bin (in
*        degrees).  If a null value is supplied, then no mask NDF is
*        produced. [!]
*     MTITLE = LITERAL (Read)
*        A title for the mask NDF.  If a null value is given, the title
*        is propagated from the input NDF.  This is only prompted for
*        if MASK is given a non-null value. ["Mask created by KAPPA -
*        Elprof"]
*     NBIN = _INTEGER (Read)
*        The number of radial or azimuthal bins required.
*     OUT = NDF (Write)
*        The output 1-dimensional NDF containing the required profile.
*        For radial profiles, it has associated axis information
*        describing the radius, in pixels, at the centre of each
*        annular bin (the radius is measured on the major axis).  For
*        azimuthal profiles, the axis information describes the
*        azimuthal angle, in degrees, at the centre of each
*        sector-shaped bin.  It will contain associated variance
*        information if the input NDF has associated variance
*        information.
*     RADIAL = _LOGICAL (Read)
*        Specifies the sort of profile required.  If RADIAL is TRUE,
*        then a radial profile is produced in which each bin is an
*        elliptical annulus.  Otherwise, an azimuthal profile is
*        produced in which each bin is a wedge-shaped sector. [TRUE]
*     RATIO = _REAL (Read)
*        The ratio of the length of the minor axis of the ellipse to
*        the length of the major axis.  It must be in the range 0.0 to
*        1.0. [1.0]
*     RMAX = _REAL (Read)
*        The radius in pixels, measured on the major axis, at the outer edge
*        of the elliptical annulus to be binned.  If a null value (!) is
*        supplied the value used is the distance from the ellipse centre
*        (specified by XC and YC) to the furthest corner of the image. This
*        will cause the entire image to fall within the outer edge of the
*        binning area. [!]
*     RMIN = _REAL (Read)
*        The radius in pixels, measured on the major axis, at the inner edge
*        of the elliptical region to be binned. [0.0]
*     TITLE = LITERAL (Read)
*        A title for the output profile NDF.  If a null value is
*        supplied the title is propagated from the input NDF. ["KAPPA -
*        Elprof"]
*     WIDTH = _REAL (Read)
*        The width of each bin.  If a radial profile is being created
*        (see Parameter RADIAL) this is the width of each annulus in
*        pixels (measured on the major axis).  If an azimuthal profile
*        is being created, it is the opening angle of each sector, in
*        degrees.  If a null (!) value is supplied, the value used is chosen
*        so that there are no gaps between adjacent bins.  Smaller values
*        will result in gaps appearing between adjacent bins.  The supplied
*        value must be small enough to ensure that adjacent bins do not
*        overlap. The supplied value must be at least 1.0. [!]
*     XC = _REAL (Read)
*        The x pixel co-ordinate of the centre of the ellipse, and the
*        vertex of the sectors.
*     YC = _REAL (Read)
*        The y pixel co-ordinate of the centre of the ellipse, and the
*        vertex of the sectors.

*  Examples:
*     elprof galaxy galprof 20 113 210 angmaj=49 rmin=10 rmax=210 ratio=0.5
*        This example will create a 1-dimensional NDF called galprof
*        containing a radial profile of the 2-dimensional NDF called
*        galaxy.  The profile will contain 20 bins and it will be
*        centred on the pixel co-ordinates (113,210).  Each bin will be
*        an annulus of an ellipse with axis ratio of 0.5 and
*        inclination of 49 degrees to the x-axis.  The image will be
*        binned between radii of 10 pixels, and 210 pixels (measured on
*        the major axis), and there will be no gaps between adjacent
*        bins (i.e. each bin will have a width on the major axis of
*        about 10 pixels).
*     elprof galaxy galprof 10 113 210 radial=f anglim=20 rmin=50
*     rmax=60
*        This example also creates a 1-dimensional NDF called galprof,
*        this time containing an azimuthal profile of the 2-dimensional
*        NDF called "galaxy", containing 10 bins.  Each bin will be a
*        wedge-shaped sector with vertex at pixel co-ordinates
*        (113,210).  The clockwise edge of the first bin will be at an
*        angle of 20 degrees to the x-axis, and each bin will have a
*        width (opening angle) of 36 degrees (so that 360 degrees are
*        covered in total).  Only the section of each sector bounded by
*        radii of 50 and 60 pixels is included in the profile.  In this
*        case the default value of 1.0 is accepted for Parameter RATIO
*        and so the bins will form a circular annulus of width 10
*        pixels.

*  Related Applications:
*     KAPPA: INSPECT; ESP: ELLFOU, ELLPRO, SECTOR.

*  Implementation Status:
*     -  This routine correctly processes the DATA, VARIANCE, TITLE,
*     UNITS, WCS (if RADIAL is true) and HISTORY components of the input NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single-precision floating point.

*  Copyright:
*     Copyright (C) 1995, 1999, 2001, 2004 Central Laboratory of the
*     Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2-FEB-1995 (DSB):
*        Original version.
*     1995 March 27 (MJC):
*        Some improvements to the documentation, removed tabs, used
*        modern variable-declaration style and other stylish changes,
*        removed long lines, mapped the input arrays together for
*        efficiency.
*     6-SEP-1999 (DSB):
*        Changed the defaulting for ANGMAJ, RATIO, RMAX, RMIN and WIDTH
*        to avoid vpath=dynamic parameters.
*     13-JUN-2001 (DSB):
*        Added Parameter ESTIMATOR.
*     24-FEB-2004 (DSB):
*        Added propagation of WCS to output and mask.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL DTOR                  ! Factor to convert degrees to radians
      PARAMETER ( DTOR = 1.7453293E-2 )

      REAL RTOD                  ! Factor to convert radians to degrees
      PARAMETER ( RTOD = 57.29578 )

      REAL PI                    ! Pi
      PARAMETER ( PI = 3.1415927 )

*  Local Variables:
      CHARACTER ESTIM*15         ! Method to use to estimate output values
      INTEGER ACTVAL             ! Actual no. of values obtained
      INTEGER EL                 ! No. of elements in mapped array
      INTEGER I                  ! Loop count
      INTEGER IGRP               ! GRP ID. for group holding ARD desc.
      INTEGER INDF1              ! NDF ID. for input image
      INTEGER INDF2              ! NDF ID. for output profile
      INTEGER INDF3              ! NDF ID. for output mask
      INTEGER IPAX( 2 )          ! Pointers to mapped output AXIS arrays
      INTEGER IPDO               ! Pointer to mapped output DATA array
      INTEGER IPI( 2 )           ! Pointer to mapped input DATA and VARIANCE arrays
      INTEGER IPMASK             ! Pointer to ARD mask array
      INTEGER IPMOUT             ! Pointer to mapped output mask
      INTEGER IPVO               ! Pointer to mapped output VARIANCE array
      INTEGER IPW1               ! Pointer to mapped work array
      INTEGER IPW2               ! Pointer to mapped work array
      INTEGER NBIN               ! No. of bins
      INTEGER NX                 ! Size of first dimension
      INTEGER NY                 ! Size of second dimension
      INTEGER REGVAL             ! Index for first ARD region
      INTEGER SDIM( 2 )          ! Indices of significant axes
      INTEGER SLBND( 2 )         ! Lower bounds of significant axes
      INTEGER SUBND( 2 )         ! Upper bounds of significant axes
      LOGICAL RADIAL             ! Perform radial binning?
      LOGICAL USEANN             ! Is only part of input image to be binned?
      LOGICAL USESEC             ! Restrict binning to a sector?
      LOGICAL VAR                ! Does input NDF have a VARIANCE array?
      LOGICAL WMEAN              ! Use weighted mean?
      REAL ANGLIM( 2 )           ! Angular bounds of the binning sector
      REAL ANGMAJ                ! Position angle of ellipse major axis
      REAL MAXWID                ! Maximum bin width
      REAL PASTEP                ! Step between adjacent sector bins
      REAL RATIO                 ! Ratio of ellipse axes
      REAL RLIM                  ! Radius to furthest corner
      REAL RMAX                  ! Outer radius to be binned
      REAL RMIN                  ! Inner radius to be binned
      REAL RSTEP                 ! Step between adjacent annular bins
      REAL WIDTH                 ! Bin width
      REAL XC                    ! X centre of ellipse
      REAL YC                    ! Y centre of ellipse
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin the NDF context.
      CALL NDF_BEGIN

*  Get an input NDF with exactly two significant dimensions.
      CALL KPG1_GTNDF( 'IN', 2, .TRUE., 'READ', INDF1, SDIM, SLBND,
     :                 SUBND, STATUS )

*  Create the output NDF to hold the mean data values.  It will be
*  changed later to a suitable 1-dimensional shape.
      CALL LPG_PROP( INDF1, 'UNITS', 'OUT', INDF2, STATUS )

*  Find the dimensions of the input image.
      NX = SUBND( 1 ) - SLBND( 1 ) + 1
      NY = SUBND( 2 ) - SLBND( 2 ) + 1

*  See if it has a VARIANCE array and map it if it has.  Also map the
*  DATA array.
      CALL NDF_STATE( INDF1, 'VARIANCE', VAR, STATUS )
      IF ( VAR ) THEN
         CALL KPG1_MAP( INDF1, 'Data,Variance', '_REAL', 'READ', IPI,
     :                        EL, STATUS )
      ELSE

*  Map the only DATA array of the input NDF.
         CALL KPG1_MAP( INDF1, 'Data', '_REAL', 'READ', IPI, EL,
     :                  STATUS )
      END IF

*  Obtain the method to use for estimating the smoothed pixel values.
      CALL PAR_CHOIC( 'ESTIMATOR', 'Mean', 'Mean,Weighted Mean',
     :                .FALSE., ESTIM, STATUS )
      IF( ESTIM .EQ. 'MEAN' ) THEN
         WMEAN = .FALSE.
      ELSE IF( VAR ) THEN
         WMEAN = .TRUE.
      ELSE
         WMEAN = .FALSE.
      END IF

*  Get the number of bins required. Use a minimum of one.
      CALL PAR_GDR0I( 'NBIN', 0, 1, VAL__MAXI, .FALSE., NBIN, STATUS )

*  Make the output NDF one dimensional with one element per bin.
      CALL NDF_SBND( 1, 1, NBIN, INDF2, STATUS )

*  Map the AXIS CENTRE and WIDTH array of the output NDF.
      CALL NDF_AMAP( INDF2, 'CENTRE,WIDTH', 1, '_REAL', 'WRITE', IPAX,
     :               NBIN, STATUS )

*  Map its DATA array, and, if needed, its VARIANCE array.
      CALL KPG1_MAP( INDF2, 'DATA', '_REAL', 'WRITE', IPDO, NBIN,
     :              STATUS )
      IF ( VAR ) CALL KPG1_MAP( INDF2, 'VARIANCE', '_REAL', 'WRITE',
     :                         IPVO, NBIN, STATUS )

*  See if radial or tangential binning is to be used.
      CALL PAR_GET0L( 'RADIAL', RADIAL, STATUS )

*  Get the x and y pixel co-ordinates of the centre of the ellipse.
*  Use the centre of the NDF as the dynamic default.
      CALL PAR_DEF0R( 'XC', 0.5 * REAL( SUBND( 1 ) + SLBND( 1 ) ),
     :                STATUS )
      CALL PAR_GET0R( 'XC', XC, STATUS )

      CALL PAR_DEF0R( 'YC', 0.5 * REAL( SUBND( 2 ) + SLBND( 2 ) ),
     :                STATUS )
      CALL PAR_GET0R( 'YC', YC, STATUS )

*  Find the distance from the ellipse centre to the furthest corner.
      RLIM = SQRT( MAX( XC - REAL ( SLBND( 1 ) - 1 ),
     :                  REAL( SUBND( 1 ) ) - XC )**2 +
     :             MAX( YC - REAL ( SLBND( 2 ) - 1 ),
     :                  REAL( SUBND( 2 ) ) - YC )**2 )

*  Store the value which will be used to represent the first region
*  specified in the ARD description describing the bins to be used.
      REGVAL = 2

*  If radial binning is being used....
      IF ( RADIAL ) THEN

*  Store the output NDF's LABEL component.
         CALL NDF_CPUT( 'Radially binned values', INDF2, 'LABEL',
     :                   STATUS )

*  Store information about the output NDF's AXIS component.
         CALL NDF_ACPUT( 'Radius', INDF2, 'LABEL', 1, STATUS )
         CALL NDF_ACPUT( 'Pixels', INDF2, 'UNITS', 1, STATUS )

*  Get the orientation of the major axis, in degrees.  Ensure it is in
*  the range 0--360, and then convert to radians.
         CALL PAR_GDR0R( 'ANGMAJ', 0.0, 0.0, 360.0, .FALSE., ANGMAJ,
     :                   STATUS )
         ANGMAJ = ANGMAJ * DTOR

*  Get the ratio of the minor- to major-axis lengths.  This must be in
*  the range 0.0 to 1.0.
         CALL PAR_GET0R( 'RATIO', RATIO, STATUS )

         IF ( RATIO .LT. 0.0 ) THEN
            CALL MSG_OUT( 'ELPROF_MSG', '  Using %RATIO = 0.0', STATUS )
            RATIO = 0.0

         ELSE IF ( RATIO .GT. 1.0 ) THEN
            CALL MSG_OUT( 'ELPROF_MSG', '  Using %RATIO = 1.0', STATUS )
            RATIO = 1.0

         END IF

*  Get the major-axis radius of the inner edge of the inner-most
*  elliptical annulus.  Limit it to be positive (greater than 0.0) so
*  that ARD will not try to create an ellipse of zero area.
         CALL PAR_GET0R( 'RMIN', RMIN, STATUS )

         IF ( RMIN .LT. 0.0 ) THEN
            CALL MSG_OUT( 'ELPROF_MSG', '  Using %RMIN = 0.0', STATUS )
            RMIN = 0.0
         END IF

         RMIN = MAX( 0.1, RMIN )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the major-axis radius of the outer edge of the outer-most
*  elliptical annulus.
         CALL PAR_GET0R( 'RMAX', RMAX, STATUS )

*  Use the distance to the furthest corner if a null value was supplied.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            RMAX = RLIM
         END IF

*  Constrain the used value to be at least RMIN + NBIN.
         IF ( RMAX .LT. RMIN + REAL( NBIN ) ) THEN
            RMAX = RMIN + REAL( NBIN )
            CALL MSG_SETR( 'R', RMAX )
            CALL MSG_OUT( 'ELPROF_MSG', '  Using %RMAX = ^R', STATUS )
         END IF

*  Get the orientations of the two radii defining the sector in which
*  the integrations are to be done, in degrees, in the range 0--360.
*  Convert to radians.
         CALL PAR_GET1R( 'ANGLIM', 2, ANGLIM, ACTVAL, STATUS )
         IF ( ACTVAL .EQ. 1 ) ANGLIM( 2 ) = ANGLIM( 1 )

         DO I = 1, 2

            ANGLIM( I ) = MOD( ANGLIM( I ), 360.0 )

            DO WHILE( ANGLIM( I ) .LT. 0.0 )
               ANGLIM( I ) = ANGLIM( I ) + 360.0
            END DO

         END DO

         ANGLIM( 1 ) = ANGLIM( 1 ) * DTOR
         ANGLIM( 2 ) = ANGLIM( 2 ) * DTOR

*  If equal values were given for the two boundaries, do not use a
*  sector (i.e. assume the sector covers 360 degrees).  Set a flag to
*  indicate this.
         IF ( ABS( ANGLIM( 1 ) - ANGLIM( 2 ) ) .LT. VAL__EPSR ) THEN
            USESEC = .FALSE.
         ELSE
            USESEC = .TRUE.
         END IF

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the width of each annulus, constraining it to be at least 1
*  pixel.  The default value is such that there are no gaps between the
*  annuli.  The supplied value is not allowed to be larger than this
*  default.
         MAXWID = ( RMAX - RMIN ) / REAL( NBIN )
         CALL PAR_GDR0R( 'WIDTH', MAXWID, 1.0, MAXWID, .FALSE., WIDTH,
     :                   STATUS )

*  If a null value was supplied, use the dynamic default.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            WIDTH = MAXWID
         END IF

*  Store the radial step between each pair of adjacent annuli.
         IF ( NBIN .GT. 1 ) THEN
            RSTEP = ( RMAX - RMIN - WIDTH ) / REAL( NBIN - 1 )
         ELSE
            RSTEP = 0.0
         END IF

*  If the step size is close to the annulus width, set the annulus
*  width to the exact step size.  This allows for some rounding errors
*  in the conversions between RMAX, RMIN, WIDTH, NBIN and RSTEP, and
*  prevents the following error report from being produced when
*  RSTEP and WIDTH are only different due to rounding errors.
         IF ( ABS( RSTEP - WIDTH ) .LT.
     :        2.0 * VAL__EPSR * ( RSTEP + WIDTH ) ) THEN
            RSTEP = WIDTH
         END IF

*  Get some temporary workspace.
         CALL PSX_CALLOC( 2 + 2 * NBIN, '_INTEGER', IPW1, STATUS )

*  Create the ARD description, storing the axis value for each bin in
*  the AXIS component of the output NDF.
         CALL KPS1_ELPR1( NX, NY, ANGMAJ, RATIO, NBIN, RSTEP, RMIN,
     :                    WIDTH, XC, YC, ANGLIM( 1 ), ANGLIM( 2 ),
     :                    USESEC, REGVAL, IGRP,
     :                    %VAL( CNF_PVAL( IPW1 ) ),
     :                    %VAL( CNF_PVAL( IPAX( 1 ) ) ),
     :                    %VAL( CNF_PVAL( IPAX( 2 ) ) ), STATUS )

*  Propagate WCS from input to output.
         CALL KPS1_ELPR5( INDF1, INDF2, ANGMAJ, RATIO, NBIN, RMIN,
     :                    RMAX, XC, YC, STATUS )

*  If tangetial (sector) bins are being used...
      ELSE

*  Store the output NDF's LABEL component.
         CALL NDF_CPUT( 'Azimuthally binned values', INDF2, 'LABEL',
     :                   STATUS )

*  Store information about the output NDF's AXIS component.
         CALL NDF_ACPUT( 'Azimuthal angle', INDF2, 'LABEL', 1, STATUS )
         CALL NDF_ACPUT( 'Degrees', INDF2, 'UNITS', 1, STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the orientation of the major axis, in degrees, in the range
*  0--360.  Convert to radians.
         CALL PAR_GDR0R( 'ANGMAJ', 0.0, 0.0, 360.0, .FALSE., ANGMAJ,
     :                   STATUS )
         ANGMAJ = ANGMAJ * DTOR

*  Get the ratio of the minor- to major-axis lengths.  This must be in
*  the range 0.0 to 1.0.
         CALL PAR_GET0R( 'RATIO', RATIO, STATUS )

*  Get the major-axis radius of the inner edge of the elliptical
*  annulus.  Limit it to be positive (greater than 0.0) so that ARD
*  will not try to create an ellipse of zero area.
         CALL PAR_GDR0R( 'RMIN', 0.0, 0.0, VAL__MAXR, .FALSE., RMIN,
     :                   STATUS )
         RMIN = MAX( 0.1, RMIN )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the major-axis radius of the outer edge of the elliptical
*  annulus.  Limit it to be greater than RMIN+1.
         CALL PAR_GDR0R( 'RMAX', REAL( MAX( NX, NY ) ), RMIN + 1.0,
     :                    VAL__MAXR, .FALSE., RMAX, STATUS )

*  Use the distance to the furthest corner if a null value was supplied.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            RMAX = RLIM
         END IF

*  Get the orientations of the two radii defining the sector to be
*  binned, in degrees, in the range 0--360.  Convert to radians.
         CALL PAR_GDRVR( 'ANGLIM', 2, 0.0, 360.0, ANGLIM, ACTVAL,
     :                   STATUS )
         IF ( ACTVAL .EQ. 1 ) ANGLIM( 2 ) = ANGLIM( 1 )
         ANGLIM( 1 ) = ANGLIM( 1 ) * DTOR
         ANGLIM( 2 ) = ANGLIM( 2 ) * DTOR

*  Store the default width (opening angle) of each sector, so that
*  adjacent sectors would have no gaps between them.  This is also the
*  maximum allowed sector width.
         MAXWID = ANGLIM( 2 ) - ANGLIM( 1 )
         IF ( MAXWID .LE. 0.0 ) MAXWID = MAXWID + 2 * PI
         MAXWID = RTOD * MAXWID / REAL( NBIN )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the width of each annulus, constraining it to be at least 0.1
*  degree.
         CALL PAR_GDR0R( 'WIDTH', MAXWID, 0.1, MAXWID, .FALSE., WIDTH,
     :                   STATUS )
         WIDTH = DTOR * WIDTH

*  If a null value was supplied, use the dynamic default.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            WIDTH = DTOR*MAXWID
         END IF

*  Calculate the step size between the centre of adjacent sectors.
         IF ( ANGLIM( 2 ) .LE. ANGLIM( 1 ) ) THEN
            PASTEP  = ( ANGLIM( 2 ) - ANGLIM( 1 ) + 2 * PI - WIDTH )
     :                / REAL( NBIN - 1 )
         ELSE
            PASTEP  = ( ANGLIM( 2 ) - ANGLIM( 1 ) - WIDTH )
     :                / REAL( NBIN - 1 )
         END IF

*  If the step size is close to the sector width, set the sector width
*  to the exact step size.  This prevents the following error report
*  from being produced when PASTEP and WIDTH are only different due to
*  rounding errors.
         IF ( ABS( PASTEP - WIDTH ) .LT.
     :       2.0 * VAL__EPSR * ( PASTEP + WIDTH ) ) THEN
            PASTEP = WIDTH
         END IF

*  Get some temporary workspace.
         CALL PSX_CALLOC( 3 + NBIN, '_INTEGER', IPW1, STATUS )

*  See if the binning area covers the entire image. We can speed up the
*  binning process if it does.
         USEANN = ( RMIN .GT. 0.0 .OR.
     :              RMAX .LT. MAX( NX, NY ) .OR.
     :              MAXWID .LT. 2*PI )

*  Create the ARD description, storing the axis value for each bin in
*  the AXIS component of the output NDF.
         CALL KPS1_ELPR2( NX, NY, USEANN, ANGMAJ, RATIO, RMIN, RMAX, XC,
     :                    YC , NBIN, ANGLIM( 1 ), WIDTH, PASTEP, REGVAL,
     :                    IGRP, %VAL( CNF_PVAL( IPW1 ) ),
     :                    %VAL( CNF_PVAL( IPAX( 1 ) ) ),
     :                    %VAL( CNF_PVAL( IPAX( 2 ) ) ), STATUS )
      END IF

*  Get workspace.
      CALL PSX_CALLOC( EL, '_INTEGER', IPMASK, STATUS )
      CALL PSX_CALLOC( NBIN, '_INTEGER', IPW2, STATUS )

*  Find the mean and variance of the data values in each bin.
      CALL KPS1_ELPR3( VAR, SLBND( 1 ), SUBND( 1 ), SLBND( 2 ),
     :                 SUBND( 2 ), %VAL( CNF_PVAL( IPI( 1 ) ) ),
     :                 %VAL( CNF_PVAL( IPI( 2 ) ) ),
     :                 IGRP, %VAL( CNF_PVAL( IPW1 ) ),
     :                 NBIN, WMEAN, REGVAL,
     :                 %VAL( CNF_PVAL( IPDO ) ),
     :                 %VAL( CNF_PVAL( IPVO ) ),
     :                 %VAL( CNF_PVAL( IPW2 ) ),
     :                 %VAL( CNF_PVAL( IPMASK ) ), STATUS )

*  Store a new title in the output NDF.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', INDF1, INDF2, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get an NDF in which to store the mask.
      CALL LPG_PROP( INDF1, 'WCS,AXIS', 'MASK', INDF3, STATUS )

*  If a null value was given, annul the error and continue.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Otherwise, map the DATA array.
      ELSE
         CALL KPG1_MAP( INDF3, 'DATA', '_REAL', 'WRITE', IPMOUT, EL,
     :                 STATUS )

*  Copy the mask to the output NDF, transforming the mask values from
*  integer bin indices, into bin AXIS CENTRE values.
         CALL KPS1_ELPR4( EL, %VAL( CNF_PVAL( IPMASK ) ), NBIN,
     :                    %VAL( CNF_PVAL( IPAX( 1 ) ) ),
     :                    %VAL( CNF_PVAL( IPMOUT ) ), STATUS )

*  Store a new title in the output mask NDF.
         CALL KPG1_CCPRO( 'MTITLE', 'TITLE', INDF1, INDF3, STATUS )

      END IF

*  Jump to here if an error occurs.
 999  CONTINUE

*  Free the workspace.
      CALL PSX_FREE( IPMASK, STATUS )
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )

*  Delete the group containing the ARD description.
      CALL GRP_DELET( IGRP, STATUS )

*  If an error has occurred, attempt to delete the output NDF.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF2, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
           CALL ERR_REP( 'ELPROF_ERR1', 'ELPROF: Unable to create an '/
     :                   /'elliptical profile of an NDF.', STATUS )
      END IF

      END
