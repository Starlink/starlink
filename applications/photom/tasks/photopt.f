      SUBROUTINE PHOTOPT ( STATUS )

*+
*  Name:
*     PHOTOPT

*  Purpose:
*     Perform sampling experiments with different sky estimators.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PHOTOPT( STATUS )

*  Description:
*     PHOTOPT examines the performance of the three different sky
*     estimators used by PHOTOM on a particular frame. It does this by
*     performing aperture photometry on random parts of the frame,
*     subtracting the estimated sky level from a concentric aperture
*     from the level in the central aperture. If the estimator is good
*     then the expected result is zero, as long as there are no objects
*     in the central aperture. This is repeated a number of times over
*     the frame to ensure that a fair representation of the frames
*     characteristics is obtained. The results are shown in graphical
*     form as a set of difference graphs. The histogram of differences
*     will indicate which is the best suited estimator for the frame.

*  ADAM Parameters:
*     ANGLE = _REAL (Read)
*        The orientation of the ellipse defining the aperture. This is
*        defined in degrees going anti-clockwise from the positive
*        y-axis. This is equivalent to a position angle.
*
*     DEVICE = DEVICE (Read)
*        The name of the device to receive the graphical output.
*
*     ECCEN = _REAL (Read)
*        The eccentricity of the ellipse defining the aperture. For a
*        circular aperture this should be set to 0.0.
*
*     IN = NDF (Read)
*        An NDF data structure containing the 2-dimensional image on
*        which the sampling test will be conducted.
*
*     INNER = _REAL (Read)
*        The radius of the inner edge of the annular sky aperture in
*        units of the object aperture size. The actual dimension in
*        pixels is obtained by multiplying this factor by the object
*        aperture semi-major axis in pixels.
*
*     NP = _INTEGER (Read)
*        The number of points to sample in the image up to a maximum of
*        100. This number is factorized so that a regular grid of
*        samples is taken. The actual number of samples is restricted to
*        ensure that the central apertures do not overlap.
*
*     OUTER = _REAL (Read)
*        The radius of the outer edge of the annular sky aperture in
*        units of the object aperture size. The actual dimension in
*        pixels is obtained by multiplying this factor by the object
*        aperture semi-major axis in pixels.
*
*     PADU = _REAL (Read)
*        The number of photons for each interval of the data. If the
*        true value is unknown use a value of 1.
*
*     RANGE = _REAL (Read)
*        The limit of the plot in photon units ( data units * PADU ).
*        This can be used to limit the range of the plot to the
*        interesting region.
*
*     SATURE = _REAL (Read)
*        The saturation level in data units for the image.
*
*     SEMIM = _REAL (Read)
*        The semi-major axis of the ellipse defining the aperture in
*        pixel units. For a circular aperture this corresponds to the
*        radius in pixel units.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     NE: Nick Eaton (University of Durham)
*     PWD: Peter W. Draper (Starlink, University of Durham)
*     {enter_new_authors_here}

*  History:
*     31-OCT-1989 (NE):
*        Original version.
*     15-AUG-1990 (NE):
*        Convert to NDF's.
*     11-FEB-1992 (NE):
*        Remove request for grid array workspace. Now obtained in OPTOP.
*     07-SEP-2004 (PWD):
*        Converted to use CNF pointers. Change dummy MASK pointer to
*        actual array of size 1.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'

*    Status :
      INTEGER STATUS

*  Local Constants :
      INTEGER NE
      PARAMETER ( NE = 36 )

*  Local Variables :
      LOGICAL USEMSK

      CHARACTER FILE*64, PATH*64, TEXT*80
      CHARACTER *(DAT__SZLOC) ILOC

      INTEGER IDIMS( 2 ), INDF, IPIN, LBND( 2 ), NDIM, NEL, NLEV,
     :        UBND( 2 )

      REAL ELLIPS( 2, NE )
      REAL L( 2, NE ), R( 2, NE ), YLIST( NE + 6 ), LYLIST( NE + 4 )
      REAL RYLIST( NE + 4 ), INSL( 2, NE + 4 ), INSR( 2, NE + 4 )
      REAL POLY( 2, 2 * NE + 8 )
      REAL MASK( 1 )
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Start up NDF and get a NDF indentifier for the data
      CALL NDF_BEGIN
      CALL NDF_ASSOC( 'IN', 'READ', INDF, STATUS )

*   Get a locator to the NDF to obtain the name
      CALL DAT_ASSOC( 'IN', 'READ', ILOC, STATUS )

*   Check the bounds of the input data
      CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( NDIM .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PHOTOPT_BOUNDS', 'Data array not 2-dimensional',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Put the pixel dimensions of the data array into the IDIMS array
      IDIMS( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      IDIMS( 2 ) = UBND( 2 ) - LBND( 2 ) + 1

*   Get a mapped pointer to the data array
      CALL NDF_MAP( INDF, 'DATA', '_REAL', 'READ', IPIN, NEL, STATUS )

      WRITE( TEXT, '(''Dimensions = '', 2I6 )' ) IDIMS( 1 ), IDIMS( 2 )
      CALL MSG_OUT( ' ', TEXT, STATUS )

*   Get the file name into a character string
      CALL HDS_TRACE( ILOC, NLEV, PATH, FILE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Indicate that there is no mask file
      USEMSK = .FALSE.

*   Call the work routine
      CALL OPTOP ( IDIMS( 1 ), IDIMS( 2 ), %VAL( CNF_PVAL( IPIN ) ),
     :             MASK, USEMSK, NE, ELLIPS, L, R, YLIST, LYLIST,
     :             RYLIST, INSL, INSR, POLY, FILE, STATUS )

*   Unmap the arrays and annul the locators
  99  CONTINUE
      CALL DAT_ANNUL( ILOC, STATUS )
      CALL NDF_END( STATUS )

      END

************************************************************************

      SUBROUTINE OPTOP ( NX, NY, IMAGE, MASK, USEMSK, NE, ELLIPS, L, R,
     :                   YLIST, LYLIST, RYLIST, INSL, INSR, POLY, FILE,
     :                   STATUS )

*+
*  Name :
*     OPTOP
*
*  Purpose :
*     Main routine for PHOTOPT.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL OPTOP( NX, NY, IMAGE, MASK, USEMSK, NE, ELLIPS, L, R, YLIST,
*    :            LYLIST, RYLIST, INSL, INSR, POLY, FILE, STATUS )
*
*  Description :
*     Main routine for PHOTOPT.
*
*  Arguments :
*     NX = INTEGER (Given)
*        Size of data array in X direction
*     NY = INTEGER (Given)
*        Size of data array in Y direction
*     IMAGE( NX, NY ) = REAL (Given)
*        Data array
*     MASK( NX, NY ) = REAL (Given)
*        Work space array for sky mask
*     USEMSK = LOGICAL (Given)
*        Flag for use of sky mask
*     NE = INTEGER (Given)
*        Number of straight line segments making up elliptical aperture
*     ELLIPS( 2, NE ) = REAL (Given)
*        Array of vertices defining the elliptical aperture
*     L( 2, NE ) = REAL (Given)
*        Work space array for left-hand monotone-polygonal-sector
*     R( 2, NE ) = REAL (Given)
*        Work space array for right-hand monotone-polygonal-sector
*     YLIST( NE + 6 ) = REAL (Given)
*        Work space array for Y-sorted list of intersection vertices
*     LYLIST( NE + 4 ) = REAL (Given)
*        Work space array for left-hand Y-list of intersection vertices
*     RYLIST( NE + 4 ) = REAL (Given)
*        Work space array for right-hand Y-list of intersection vertices
*     INSL( 2, NE + 4 ) = REAL (Given)
*        Work space array for left-hand MPS of intersection vertices
*     INSR( 2, NE + 4 ) = REAL (Given)
*        Work space array for right-hand MPS of intersection vertices
*     POLY( 2, 2 * NE + 8 ) = REAL (Given)
*        Work space array for intersection polygon
*     FILE =  CHARACTER * ( * ) (Given)
*        Name of data file
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm :
*     Open the results file.
*     Initialise the work space array to zero.
*     Indicate that SGS has not been initialised.
*     Get a lot of the parameters from the interface.
*     Loop infinitely
*        Respond to the menu options.
*        If command is 'Exit' then jump out of loop.
*     Enddo.
*     Tidy up.
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-OCT-1989 (NE):
*        Original version.
*     10-JAN-1992
*        Replaced 'BAD_PAR' with 'PRM_PAR'.
*        Added use of local grid array.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'CNF_PAR'

*  Arguments Given :
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      REAL MASK( * )
      LOGICAL USEMSK
      INTEGER NE
      REAL ELLIPS( 2, NE )
      REAL L( 2, NE )
      REAL R( 2, NE )
      REAL YLIST( NE + 6 )
      REAL LYLIST( NE + 4 )
      REAL RYLIST( NE + 4 )
      REAL INSL( 2, NE + 4 )
      REAL INSR( 2, NE + 4 )
      REAL POLY( 2, 2 * NE + 8 )
      CHARACTER * ( * ) FILE

*  Status :
      INTEGER STATUS

*  Local Constants :
      INTEGER GS, MXNP
      PARAMETER ( GS = 64 )
      PARAMETER ( MXNP = 100 )

*  Local Variables :
      LOGICAL SGSOFF

      INTEGER BZONE, GSIZE, I, IDIMS( 2 ), IG, IWKID,
     :        J, N, NBIN, NN, NP, NPX, NPY, SKYEST

      REAL A, A2, A3, DIFF( MXNP ), DIFF1( MXNP ), DIFF2( MXNP ),
     :     DIFF3( MXNP ), E, GRID( GS, GS ), HMAX, HMIN, INNER,
     :     OUTER, PADU, RANGE, SATURE, THETA, XP( MXNP ), YP( MXNP )

      CHARACTER CTYPE( 3 ) * 6, DEVNAM * 10, TEXT * 80
      CHARACTER *(DAT__SZLOC) GLOC

*  Local Data :
      DATA CTYPE / 'Mean  ', 'Mean2s', 'Mode  ' /
*.

*   Initialise the grid array to zero
      DO J = 1, GS
         DO I = 1, GS
            GRID( I, J ) = 0.0
         ENDDO
      ENDDO

*   Flag that SGS has not been initialised
      SGSOFF = .TRUE.

*   Get initial values of the important variables from the parameter system
*   a      = semi-major axis of elliptical aperture
*   e      = eccentricity of aperture
*   theta  = orientaion of elliptical aperture anti-clockwise from x-axis
*   padu   = scaling factor between photons and data units
*   sature = user supplied saturation level
      CALL PAR_GET0R( 'SEMIM', A, STATUS )
      CALL CHPARR( 'SEMIM', A, 0.0, VAL__MAXR, STATUS )
      CALL PAR_GET0R( 'ECCEN', E, STATUS )
      CALL CHPARR( 'ECCEN', E, 0.0, 1.0, STATUS )
      CALL PAR_GET0R( 'ANGLE', THETA, STATUS )
      CALL PAR_GET0R( 'PADU', PADU, STATUS )
      CALL CHPARR( 'PADU', PADU, 0.0, VAL__MAXR, STATUS )
      CALL PAR_GET0R( 'SATURE', SATURE, STATUS )

*   Get the inner and outer radii of the concentric sky aperture
      CALL PAR_GET0R( 'INNER', INNER, STATUS )
      CALL CHPARR( 'INNER', INNER, 0.0, VAL__MAXR, STATUS )
      CALL PAR_GET0R( 'OUTER', OUTER, STATUS )
      CALL CHPARR( 'OUTER', OUTER, 0.0, VAL__MAXR, STATUS )

*   Write out the current values
      WRITE( TEXT, '(''     A = '', F7.1 )' ) A
      CALL MSG_OUT( ' ', TEXT, STATUS )
      WRITE( TEXT, '(''     E = '', F9.3 )' ) E
      CALL MSG_OUT( ' ', TEXT, STATUS )
      WRITE( TEXT, '('' THETA = '', F7.1 )' ) THETA
      CALL MSG_OUT( ' ', TEXT, STATUS )
      WRITE( TEXT, '('' INNER = '', F7.1 )' ) INNER
      CALL MSG_OUT( ' ', TEXT, STATUS )
      WRITE( TEXT, '('' OUTER = '', F7.1 )' ) OUTER
      CALL MSG_OUT( ' ', TEXT, STATUS )
      WRITE( TEXT, '(''SATURE = '', E10.3 )' ) SATURE
      CALL MSG_OUT( ' ', TEXT, STATUS )

*   Get the number of points and ensure it is less than the maximum
      CALL PAR_GET0I( 'NP', NP, STATUS )
      IF ( NP .LT. 0 ) THEN
         NP = 1
      ENDIF
      IF ( NP .GT. MXNP ) THEN
         NP = MXNP
      ENDIF

*   Get the range to plot and ensure it is positive
      CALL PAR_GET0R( 'RANGE', RANGE, STATUS )
      IF ( RANGE .LT. 0.0 ) THEN
         RANGE = - RANGE
      ENDIF

*   Calculate the positions of up to NP regularly spaced samples.
*   Estimate how many squares of side 2a can fit in the array
      N = MIN( NP, NINT( REAL( NX * NY ) / ( 4.0 * A ** 2 ) ) )
      NPX = INT( SQRT( REAL( N * NX ) / REAL( NY ) ) )
      NPY = N / NPX
      N = NPX * NPY

*   Define the positions on a regular grid
      NN = 1
      DO J = 1, NPY
         DO I = 1, NPX
            XP( NN ) = REAL( NX * I ) / REAL( NPX + 1 )
            NN = NN + 1
         ENDDO
      ENDDO

      NN = 1
      DO J = 1, NPY
         DO I = 1, NPX
            YP( NN ) = REAL( NY * J ) / REAL( NPY + 1 )
            NN = NN + 1
         ENDDO
      ENDDO

*   Set up the inner and outer radii for a concentric sky aperture
      A2 = A * OUTER
      A3 = A * INNER

*   Check that the workspace is big enough to take the aperture
      GSIZE = INT( 2.0 * A + 2.0 ) + 1
      IF ( GSIZE .LE. GS ) THEN

*   Do the automatic photometry
         CALL AUTOPT( NE, ELLIPS, A, A2, A3, E, THETA, NX, NY, IMAGE,
     :                GRID, GS, MASK, USEMSK, L, R, YLIST, LYLIST,
     :                RYLIST, INSL, INSR, POLY, PADU, SATURE, N, XP, YP,
     :                DIFF1, DIFF2, DIFF3, STATUS )


*   Otherwise get some temporary work space for an aperture of this size
      ELSE
         IDIMS( 1 ) = GSIZE
         IDIMS( 2 ) = GSIZE
         CALL AIF_TEMP( '_REAL', 2, IDIMS, GLOC, STATUS )
         CALL DAT_MAPR( GLOC, 'WRITE', 2, IDIMS, IG, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Clear the workspace array and pass it to the photometry routine
         CALL CLGRID( GSIZE, GSIZE, %VAL( CNF_PVAL( IG ) ), 1, GSIZE,
     :                1, GSIZE )
         CALL AUTOPT( NE, ELLIPS, A, A2, A3, E, THETA, NX, NY, IMAGE,
     :                %VAL( CNF_PVAL( IG ) ), GSIZE, MASK, USEMSK, L,
     :                R, YLIST, LYLIST, RYLIST, INSL, INSR, POLY, PADU,
     :                SATURE, N, XP, YP, DIFF1, DIFF2, DIFF3, STATUS )

*   Annul the workspace
         CALL DAT_UNMAP( GLOC, STATUS )
         CALL DAT_ANNUL( GLOC, STATUS )
      ENDIF

*   Start up SGS and divide up the space
      CALL SGS_ASSOC( 'DEVICE', 'WRITE', BZONE, STATUS )
      CALL SGS_ICURW( IWKID )
      WRITE( DEVNAM, '(I10)' ) IWKID
      CALL PGBEGIN( 0, DEVNAM, 2, 3 )
      CALL PGSCH( 2.0 )

*   Step through the different sky estimators
      DO SKYEST = 1, 3

*   Copy the results to a local array
         DO J = 1, N
            IF ( SKYEST .EQ. 1 ) THEN
               DIFF( J ) = DIFF1( J )
            ELSEIF ( SKYEST .EQ. 2 ) THEN
               DIFF( J ) = DIFF2( J )
            ELSEIF ( SKYEST .EQ. 3 ) THEN
               DIFF( J ) = DIFF3( J )
            ENDIF
         ENDDO

*   Advance the page and set the world coordinates
         CALL PGADVANCE
         CALL PGVSTAND
         CALL PGWINDOW( 0.0, REAL( NP + 1 ), -RANGE, RANGE )

*   Write the file name on the plot
         IF ( SKYEST .EQ. 1 ) THEN
            CALL PGTEXT( REAL( NP / 100.0 ), RANGE * 1.05, FILE )
         ENDIF

*   Annotate the axes
         CALL PGBOX( 'BCNT', 0.0, 0, 'BCNT', 0.0, 0 )
         CALL PGLABEL( 'Sample', 'Object-Sky', CTYPE( SKYEST ) )
         CALL PGMOVE( 1.0, 0.0 )
         CALL PGDRAW( REAL( NP ), 0.0 )

*   Plot the points and join them to the zero line
         DO J = 1, N
            IF ( DIFF( J ) .GT. RANGE ) THEN
               CALL PGPOINT( 1, REAL( J ), RANGE, 2 )
               CALL PGMOVE( REAL( J ), 0.0 )
               CALL PGDRAW( REAL( J ), RANGE )
            ELSEIF ( DIFF( J ) .LT. -RANGE ) THEN
               CALL PGPOINT( 1, REAL( J ), -RANGE, 2 )
               CALL PGMOVE( REAL( J ), 0.0 )
               CALL PGDRAW( REAL( J ), -RANGE )
            ELSE
               CALL PGPOINT( 1, REAL( J ), DIFF( J ), 3 )
               CALL PGMOVE( REAL( J ), 0.0 )
               CALL PGDRAW( REAL( J ), DIFF( J ) )
            ENDIF
         ENDDO

*   Draw a histogram of values ( NBIN must be less than 200 )
*   A new page is begun by having 0 as the last argument to PGHIST
         HMAX = RANGE
         HMIN = -RANGE
         NBIN = INT( 2 * RANGE + 1 )
         IF ( NBIN .GT. 200 ) THEN
            NBIN = 101
            HMIN = -50.0
            HMAX = 50.0
         ENDIF
         CALL PGHIST( N, DIFF, HMIN, HMAX, NBIN, 0 )
         CALL PGLABEL( 'Object-Sky', 'N', CTYPE( SKYEST ) )

      ENDDO

  99  CONTINUE

*   Switch off SGS
      CALL PGEND
      CALL SGS_CANCL( 'DEVICE', STATUS )

      END

************************************************************************

      SUBROUTINE AUTOPT ( NE, ELLIPS, A, A2, A3, E, THETA, NX, NY,
     :                   IMAGE, GRID, GS, MASK, USEMSK, L, R, YLIST,
     :                   LYLIST, RYLIST, INSL, INSR, POLY, PADU, SATURE,
     :                   NP, XP, YP, DIFF1, DIFF2, DIFF3, STATUS )

*+
*  Name :
*     AUTOPT
*
*  Purpose :
*     This performs the non-interactive measurement of the image
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL AUTOPT( NE, ELLIPS, A, A2, A3, E, THETA, NX, NY, IMAGE, GRID,
*    :             GS, MASK, USEMSK, L, R, YLIST, LYLIST, RYLIST, INSL,
*    :             INSR, POLY, PADU, SATURE, NP, XP, YP, DIFF1, DIFF2,
*    :             DIFF3, STATUS )
*
*  Description :
*     This performs the non-interactive measurement of the image
*
*  Arguments :
*     NE = INTEGER (Given)
*        Number of vertices in ellipse
*     ELLIPS( 2, NE ) = REAL (Given)
*        Array of vertices of ellipse
*     A = REAL (Given)
*        Semi-major axis of aperture ellipse
*     A2 = REAL (Given)
*        Outer radius of concentric sky aperture
*     A3 = REAL (Given)
*        Inner radius of concentric sky aperture
*     E = REAL (Given)
*        Eccentricity of aperture ellipse
*     THETA = REAL (Given)
*        Orientation of aperture ellipse
*     NX = INTEGER (Given)
*        X dimension of image array
*     NY = INTEGER (Given)
*        Y dimension of image array
*     IMAGE( NX, NY ) = REAL (Given)
*        Array containing image
*     GRID( GS, GS ) = REAL (Given)
*        Work space array
*     GS = INTEGER (Given)
*        Size of grid array
*     MASK( * ) = REAL (Given)
*        Array for sky mask
*     USEMSK = LOGICAL (Given)
*        Flag to indicate use of mask array
*     L( 2, NE ) = REAL (Given)
*        Work space array for left-hand monotone-polygonal-sector
*     R( 2, NE ) = REAL (Given)
*        Work space array for right-hand monotone-polygonal-sector
*     YLIST( NE + 6 ) = REAL (Given)
*        Work space array for Y-sorted list of intersection vertices
*     LYLIST( NE + 4 ) = REAL (Given)
*        Work space array for left-hand Y-list of intersection vertices
*     RYLIST( NE + 4 ) = REAL (Given)
*        Work space array for right-hand Y-list of intersection vertices
*     INSL( 2, NE + 4 ) = REAL (Given)
*        Work space array for left-hand MPS of intersection vertices
*     INSR( 2, NE + 4 ) = REAL (Given)
*        Work space array for right-hand MPS of intersection vertices
*     POLY( 2, 2 * NE + 8 ) = REAL (Given)
*        Work space array for intersection polygon
*     PADU = REAL (Given)
*        Photons per data unit
*     SATURE = REAL (Given)
*        User supplied saturation level
*     NP = INTEGER (Given)
*        Number of points
*     XP( NP ) = REAL (Given)
*        X positions
*     YP( NP ) = REAL (Given)
*        Y positions
*     DIFF1( NP ) = REAL (Returned)
*        Object minus sky for first sky estimator
*     DIFF2( NP ) = REAL (Returned)
*        Object minus sky for second sky estimator
*     DIFF3( NP ) = REAL (Returned)
*        Object minus sky for third sky estimator
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-OCT-1989 (NE):
*        Original version.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIO_ERR'
      INCLUDE 'CNF_PAR'

*  Arguments Given :
      INTEGER NE
      REAL ELLIPS( 2, NE )
      REAL A
      REAL A2
      REAL A3
      REAL E
      REAL THETA
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      INTEGER GS
      REAL GRID( GS, GS )
      REAL MASK( * )
      LOGICAL USEMSK
      REAL L( 2, NE )
      REAL R( 2, NE )
      REAL YLIST( NE + 6 )
      REAL LYLIST( NE + 4 )
      REAL RYLIST( NE + 4 )
      REAL INSL( 2, NE + 4 )
      REAL INSR( 2, NE + 4 )
      REAL POLY( 2, 2 * NE + 8 )
      REAL PADU
      REAL SATURE
      INTEGER NP
      REAL XP( NP )
      REAL YP( NP )

*  Arguments Returned :
      REAL DIFF1( NP )
      REAL DIFF2( NP )
      REAL DIFF3( NP )

*  Status :
      INTEGER STATUS

*  Local Constants :
      INTEGER MAXSKY
      PARAMETER ( MAXSKY = 1000 )

*  Local Variables :
      LOGICAL CUTOFF, USEVAR

      INTEGER ASKY, INDEX, IV, J, NV, NXH, NXL, NYH, NYL

      REAL AREA, EFACT, LSKY1, LSKY2, LSKY3, SIGMA, STAR,
     :     VALUES( MAXSKY ), VSKY, XCEN, YCEN

      CHARACTER * ( DAT__SZLOC ) VLOC
      CHARACTER * ( 2 ) CODE
*.

*   Calculate the ellipticity factor
      EFACT = 3.14159265 * SQRT( 1.0 - E**2 )

*   Have the local sky value initially zero
      LSKY1 = 0.0
      LSKY2 = 0.0
      LSKY3 = 0.0
      INDEX = 0

*   Do not use the variance component
      USEVAR = .FALSE.

*   Loop through the positions file
      DO J = 1, NP

*   Assign the points
         XCEN = XP( J )
         YCEN = YP( J )

*   Calculate the useful area of the grid array to be a box of size 2a
         NXL = INT( XCEN - A - 0.5 )
         NXH = INT( XCEN + A + 1.5 )
         NYL = INT( YCEN - A - 0.5 )
         NYH = INT( YCEN + A + 1.5 )
         IF ( NXL .LT. 1 ) NXL = 1
         IF ( NXH .GT. NX ) NXH = NX
         IF ( NYL .LT. 1 ) NYL = 1
         IF ( NYH .GT. NY ) NYH = NY

*   Create an ellipse centered on the cursor
         CALL MAKELL( XCEN, YCEN, A, E, THETA, NE, ELLIPS )

*   Integrate the ellipse over an empty grid
         CALL BOXELL( NE, ELLIPS, NXL, NXH, NYL, NYH, NX, NY, 1.0,
     :                GRID, GS, AREA, CUTOFF, L, R, YLIST, LYLIST,
     :                RYLIST, INSL, INSR, POLY )
         CALL INTELL( NX, NY, IMAGE, GRID, GS, NXL, NXH, NYL, NYH,
     :                SATURE, CODE, STAR, AREA )

*   Clear the active area of the grid array
         CALL CLGRID( GS, GS, GRID, 1, NXH - NXL + 1, 1, NYH - NYL + 1 )

*   Have to use concentric sky aperture in non-interactive mode
*   Calculate the useful area of the grid array to be a box of size
*   twice the largest radius
         NXL = INT( XCEN - A2 - 0.5 )
         NXH = INT( XCEN + A2 + 1.5 )
         NYL = INT( YCEN - A2 - 0.5 )
         NYH = INT( YCEN + A2 + 1.5 )
         IF ( NXL .LT. 1 ) NXL = 1
         IF ( NXH .GT. NX ) NXH = NX
         IF ( NYL .LT. 1 ) NYL = 1
         IF ( NYH .GT. NY ) NYH = NY

*   Estimate the number of pixels that will be used in the sky annulus
*   Add on a few for luck
         ASKY = NINT( EFACT * ( A2 ** 2 - A3 ** 2 ) )
         ASKY = ASKY + 13

*   Find the modal sky value in this annulus
*   If the sky area is less than MAXSKY then pass the VALUES array
         IF ( ASKY .LT. MAXSKY ) THEN
            NV = MAXSKY
            CALL RAGGED ( 1, NX, NY, IMAGE, 0, USEVAR, MASK, USEMSK,
     :                    NXL, NXH, NYL, NYH, XCEN, YCEN, A2, A3, E,
     :                    THETA, VALUES, NV, LSKY1, SIGMA, VSKY )
            NV = MAXSKY
            CALL RAGGED ( 2, NX, NY, IMAGE, 0, USEVAR, MASK, USEMSK,
     :                    NXL, NXH, NYL, NYH, XCEN, YCEN, A2, A3, E,
     :                    THETA, VALUES, NV, LSKY2, SIGMA, VSKY )
            NV = MAXSKY
            CALL RAGGED ( 3, NX, NY, IMAGE, 0, USEVAR, MASK, USEMSK,
     :                    NXL, NXH, NYL, NYH, XCEN, YCEN, A2, A3, E,
     :                    THETA, VALUES, NV, LSKY3, SIGMA, VSKY )

*   Otherwise get some temporary workspace
         ELSE
            NV = ASKY
            CALL DAT_TEMP ( '_REAL', 1, NV, VLOC, STATUS )
            CALL DAT_MAPR ( VLOC, 'WRITE', 1, NV, IV, STATUS )
            CALL RAGGED ( 1, NX, NY, IMAGE, 0, USEVAR, MASK, USEMSK,
     :                    NXL, NXH, NYL, NYH, XCEN, YCEN, A2, A3, E,
     :                    THETA, %VAL( CNF_PVAL( IV ) ), NV, LSKY1,
     :                    SIGMA, VSKY )
            NV = ASKY
            CALL RAGGED ( 2, NX, NY, IMAGE, 0, USEVAR, MASK, USEMSK,
     :                    NXL, NXH, NYL, NYH, XCEN, YCEN, A2, A3, E,
     :                    THETA, %VAL( CNF_PVAL( IV ) ), NV, LSKY2,
     :                    SIGMA, VSKY )
            NV = ASKY
            CALL RAGGED ( 3, NX, NY, IMAGE, 0, USEVAR, MASK, USEMSK,
     :                    NXL, NXH, NYL, NYH, XCEN, YCEN, A2, A3, E,
     :                    THETA, %VAL( CNF_PVAL( IV ) ), NV, LSKY3,
     :                    SIGMA, VSKY )
            CALL DAT_UNMAP( VLOC, STATUS )
            CALL DAT_ANNUL( VLOC, STATUS )
         ENDIF

*   Calculate the sky signal
         DIFF1( J ) = PADU * ( STAR / AREA - LSKY1 )
         DIFF2( J ) = PADU * ( STAR / AREA - LSKY2 )
         DIFF3( J ) = PADU * ( STAR / AREA - LSKY3 )

      ENDDO

      END

* $Id$
