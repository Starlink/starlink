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
*     position.  The mean data values in each bin are found, and stored 
*     in a 1-dimensional NDF which can be examined using LINPLOT,
*     INSPECT, etc.  Options exist to restrict the area binned to a
*     given range of radial distance and/or azimuthal angle.  A
*     2-dimensional mask image can optionally be produced indicating
*     which bin each input pixel was placed in.
*
*     If radial binning is selected (the default), then each bin is an
*     elliptical annulus of shape and size determined by parameters
*     RMIN, RMAX, ANGMAJ, RATIO, XC, YC, and WIDTH.  The bins can be
*     restricted to a specified sector of these annuli using parameter
*     ANGLIM.
*
*     If azimuthal binning is selected, then each bin is a sector
*     (i.e. a wedge-shape), with its vertex given by parameters XC and
*     YC, and its opening angle given by parameters WIDTH.  The range of
*     azimuthal angles to be binned can be specified by parameter
*     ANGLIM.  The bins can be restricted to the intersection of these
*     sectors with an elliptical annulus by specified values for
*     parameters RMIN, RMAX, ANGMAJ and RATIO. 

*  Usage:
*     elprof in out nbin xc yc { angmaj=?
*                              { ratio=?
*                              { rmin=?
*                              { rmax=?
*                              radial

*  ADAM Parameters:
*     ANGLIM( 2 ) = _REAL (Read)
*        Defines the wedge-shaped sector within which binning is to be
*        performed.  The first value should be the azimuthal angle of
*        the clockwise boundary of the sector, and the second should be
*        the azimuthal angle of the anti-clockwise boundary.  The angles
*        are measured in degrees from the x-axis, and rotation from the
*        x-axis to the y-axis is positive.  If only a single value is
*        supplied, or if both values are equal, the sector starts at
*        the given angle and extends for 360 degrees. [0]
*     ANGMAJ = _REAL (Read)
*        The angle between the x-axis and the major axis of the
*        ellipse, in degrees.  Rotation from the x-axis to the y-axis is
*        positive.  If an azimuthal profile (see parameter RADIAL) is
*        being produced a run-time default of zero is used, otherwise
*        you will be prompted for a value. []
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
*        1.0.  If an azimuthal profile (see parameter RADIAL) is being
*        produced a run-time default of 1.0 is used, otherwise you are
*        prompted for a value. []
*     RMAX = _REAL (Read)
*        The radius in pixels, measured on the major axis, at the outer edge
*        of the elliptical region to be binned.  If an azimuthal profile
*        (see parameter RADIAL) is being produced, a large run-time
*        default is used which results in the entire image being
*        binned, otherwise you are prompted for a value. []
*     RMIN = _REAL (Read)
*        The radius in pixels, measured on the major axis, at the inner edge
*        of the elliptical region to be binned.  If an azimuthal profile
*        (see parameter RADIAL) is being produced a run-time default of
*        0.0 is used, otherwise you are prompted a value. []
*     TITLE = LITERAL (Read)
*        A title for the output profile NDF.  If a null value is
*        supplied the title is propagated from the input NDF. ["KAPPA -
*        Elprof"]
*     WIDTH = _REAL (Read)
*        The width of each bin.  If a radial profile is being created
*        (see parameter RADIAL) this is the width of each annulus in
*        pixels (measured on the major axis).  If an azimuthal profile
*        is being created, it is the opening angle of each sector, in
*        degrees.  The run-time default is chosen so that there are no
*        gaps between adjacent bins.  Smaller values will result in gaps
*        appearing between adjacent bins.  The supplied value must be
*        small enough to ensure that adjacent bins do not overlap. []
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
*        case the default value of 1.0 is accepted for parameter RATIO
*        and so the bins will form a circular annulus of width 10
*        pixels.

*  Related Applications:
*     KAPPA: INSPECT; ESP: ELLFOU, ELLPRO, SECTOR.

*  Implementation Status:
*     -  This routine correctly processes the DATA, VARIANCE, TITLE,
*     UNITS, and HISTORY components of the input NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single-precision floating point.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-FEB-1995 (DSB):
*        Original version.
*     1995 March 27 (MJC):
*        Some improvements to the documentation, removed tabs, used
*        modern variable-declaration style and other stylish changes,
*        removed long lines, mapped the input arrays together for
*        efficiency.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ error constants

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
      INTEGER ACTVAL             ! Actual no. of values obtained
      REAL ANGLIM( 2 )           ! Angular bounds of the binning sector
      REAL ANGMAJ                ! Position angle of ellipse major axis
      INTEGER EL                 ! No. of elements in mapped array
      INTEGER I                  ! Loop count
      INTEGER IGRP               ! GRP ID. for group holding ARD desc.
      INTEGER INDF1              ! NDF ID. for input image
      INTEGER INDF2              ! NDF ID. for output profile
      INTEGER INDF3              ! NDF ID. for output mask
      INTEGER IPAX( 2 )          ! Pointers to mapped output AXIS arrays
      INTEGER IPDO               ! Pointer to mapped output DATA array
      INTEGER IPMASK             ! Pointer to ARD mask array
      INTEGER IPMOUT             ! Pointer to mapped output mask 
      INTEGER IPI( 2 )           ! Pointer to mapped input DATA and
                                 ! VARIANCE arrays
      INTEGER IPVO               ! Pointer to mapped output VARIANCE
                                 ! array
      INTEGER IPW1               ! Pointer to mapped work array
      INTEGER IPW2               ! Pointer to mapped work array
      REAL MAXWID                ! Maximum bin width
      INTEGER NBIN               ! No. of bins
      INTEGER NX                 ! Size of first dimension
      INTEGER NY                 ! Size of second dimension
      REAL PASTEP                ! Step between adjacent sector bins
      LOGICAL RADIAL             ! Perform radial binning?
      REAL RATIO                 ! Ratio of ellipse axes
      INTEGER REGVAL             ! Index for first ARD region
      REAL RMAX                  ! Outer radius to be binned
      REAL RMIN                  ! Inner radius to be binned
      REAL RSTEP                 ! Step between adjacent annular bins
      INTEGER SDIM( 2 )          ! Indices of significant axes
      INTEGER SLBND( 2 )         ! Lower bounds of significant axes
      INTEGER SUBND( 2 )         ! Upper bounds of significant axes
      LOGICAL USEANN             ! Restrict binning to an annulus?
      LOGICAL USESEC             ! Restrict binning to a sector?
      LOGICAL VAR                ! Does input NDF have a VARIANCE array?
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
      CALL NDG_PROPL( INDF1, 'UNITS', 'OUT', INDF2, STATUS )

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
      
*  Get the major-axis radius of the outer edge of the outer-most
*  elliptical annulus.  Constrain the used value to be at least RMIN +
*  NBIN.
         CALL PAR_GET0R( 'RMAX', RMAX, STATUS )

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

*  Get the width of each annulus, constraining it to be at least 1
*  pixel.  The default value is such that there are no gaps between the
*  annuli.  The supplied value is not allowed to be larger than this
*  default.
         MAXWID = ( RMAX - RMIN ) / REAL( NBIN )
         CALL PAR_GDR0R( 'WIDTH', MAXWID, 1.0, MAXWID, .FALSE., WIDTH, 
     :                   STATUS )

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
     :                    USESEC, REGVAL, IGRP, %VAL( IPW1 ), 
     :                    %VAL( IPAX( 1 ) ), %VAL( IPAX( 2 ) ), STATUS )

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
         CALL PAR_GDR0R( 'RATIO', 1.0, 0.0, 1.0, .FALSE., RATIO, 
     :                   STATUS )
      
*  Get the major-axis radius of the inner edge of the elliptical
*  annulus.  Limit it to be positive (greater than 0.0) so that ARD
*  will not try to create an ellipse of zero area.
         CALL PAR_GDR0R( 'RMIN', 0.0, 0.0, VAL__MAXR, .FALSE., RMIN, 
     :                   STATUS )
         RMIN = MAX( 0.1, RMIN )
      
*  Get the major-axis radius of the outer edge of the elliptical
*  annulus.  Limit it to be greater than RMIN+1.
         CALL PAR_GDR0R( 'RMAX', REAL( MAX( NX, NY ) ), RMIN + 1.0,
     :                    VAL__MAXR, .FALSE., RMAX, STATUS )

*  If a null value was supplied for ANGMAJ, RATIO, RMIN or RMAX, do not
*  use an annulus (i.e. assume the annulus extends from the centre to
*  infinity).  Set a flag and annul the error.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            USEANN = .FALSE.
            CALL ERR_ANNUL( STATUS )
         ELSE
            USEANN = .TRUE.
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

*  Get the width of each annulus, constraining it to be at least 0.1
*  degree. 
         CALL PAR_GDR0R( 'WIDTH', MAXWID, 0.1, MAXWID, .FALSE., WIDTH, 
     :                   STATUS )
         WIDTH = DTOR * WIDTH

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

*  Create the ARD description, storing the axis value for each bin in
*  the AXIS component of the output NDF.
         CALL KPS1_ELPR2( NX, NY, USEANN, ANGMAJ, RATIO, RMIN, RMAX, XC,
     :                    YC , NBIN, ANGLIM( 1 ), WIDTH, PASTEP, REGVAL, 
     :                    IGRP, %VAL( IPW1 ), %VAL( IPAX( 1 ) ),
     :                    %VAL( IPAX( 2 ) ), STATUS )
      END IF

*  Get workspace.
      CALL PSX_CALLOC( EL, '_INTEGER', IPMASK, STATUS )
      CALL PSX_CALLOC( NBIN, '_INTEGER', IPW2, STATUS )

*  Find the mean and variance of the data values in each bin.
      CALL KPS1_ELPR3( VAR, SLBND( 1 ), SUBND( 1 ), SLBND( 2 ), 
     :                 SUBND( 2 ), %VAL( IPI( 1 ) ), %VAL( IPI( 2 ) ),
     :                 IGRP, %VAL( IPW1 ), NBIN, REGVAL, %VAL( IPDO ), 
     :                 %VAL( IPVO ), %VAL( IPW2 ), %VAL( IPMASK ),
     :                 STATUS )

*  Store a new title in the output NDF.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', INDF1, INDF2, STATUS )
      
*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      
*  Get an NDF in which to store the mask.
      CALL NDG_PROPL( INDF1, ' ', 'MASK', INDF3, STATUS )

*  If a null value was given, annul the error and continue.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Otherwise, map the DATA array.
      ELSE
         CALL KPG1_MAP( INDF3, 'DATA', '_REAL', 'WRITE', IPMOUT, EL,
     :                 STATUS )

*  Copy the mask to the output NDF, transforming the mask values from
*  integer bin indices, into bin AXIS CENTRE values.
         CALL KPS1_ELPR4( EL, %VAL( IPMASK ), NBIN, %VAL( IPAX( 1 ) ),
     :                    %VAL( IPMOUT ), STATUS )

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
