      SUBROUTINE POLIMAGE( STATUS )
*+
*  Name:
*     POLIMAGE

*  Purpose:
*     Converts a catalogue into a 1 or 2-dimensional NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLIMAGE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a 1D or 2D NDF from a supplied catalogue
*     (see parameter SHAPE). The columns containing data value and 
*     (optionally) variance are specified using parameters COLDAT and COLVAR.
*
*     If a 2D NDF is created, the spatial position of each data value is 
*     read from the catalogue columns specified using parameters COLX and
*     COLY. The NDF is formed by bining the catalogue values into a grid 
*     of equally sized rectangular cells, the dimensions of each cell being 
*     given by parameter BOX. Each pixel in the output NDF corresponds 
*     to one of these cells. The  data values for the cell are formed by 
*     combining together the COLDAT values of all input positions which 
*     fall within the cell, using the method specified by the parameter 
*     METHOD. 
*
*     If a 1D NDF is created, the spatial position of each data value is
*     ignored. The data values are just copied into the 1D NDF in the
*     same order that they appear in the input catalogue. That is, the
*     first value in the catalogue becomes pixel 1, the seconds catalogue 
*     value becomes pixel 2, etc. This avoids any binning of the data
*     values, and is useful if applications which do not use spatial 
*     information (such as the KAPPA applications STATS, HISTOGRAM, etc)
*     are to be used.

*  Usage:
*     polimage in out coldat [colvar] [colx] [coly] [method]

*  ADAM Parameters:
*     BOX( 2 ) = _REAL (Read)
*        The x and y bin sizes. These values refer to the co-ordinate Frame
*        given by parameters COLX and COLY. Only accessed if parameter SHAPE 
*        is TRUE.
*     COLDAT = LITERAL (Read)
*        The name of the catalogue column holding the values to be stored
*        in the DATA component of the output NDF. A list of available 
*        column names is displayed if a non-existent column name is given. 
*        An arbitrary algebraic combination of columns may be used by
*        supplying a CURSA expression instead of a single column name. See
*        SUN/190 for details of the syntax of these expressions.
*     COLVAR = LITERAL (Read)
*        The name of the catalogue column holding the values to be stored 
*        in the VARIANCE component of the output NDF. A list of available 
*        column names is displayed if a non-existent column name is given. 
*        If a null (!) value is supplied, no VARIANCE component is created. 
*        An arbitrary algebraic combination of columns may be used by
*        supplying a CURSA expression instead of a single column name.
*        For instance, supplying the string "DP**2" causes the square of
*        the values in column DP to be used. See SUN/190 for details of the 
*        syntax of these expressions. [!]
*     COLX = LITERAL (Read)
*        The name of the catalogue column which gives the coordinate 
*        of each data value along the first axis. A list of available column 
*        names is displayed if a non-existent column name is given. An 
*        arbitrary algebraic combination of columns may be used by supplying 
*        a CURSA expression instead of a single column name. See SUN/190 for 
*        details of the syntax of these expressions. Only accessed if
*        parameter SHAPE is TRUE. [X]
*     COLY = LITERAL (Read)
*        The name of the catalogue column which gives the coordinate 
*        of each data value along the second axis. A list of available column 
*        names is displayed if a non-existent column name is given. An 
*        arbitrary algebraic combination of columns may be used by supplying 
*        a CURSA expression instead of a single column name. See SUN/190 for 
*        details of the syntax of these expressions. Only accessed if
*        parameter SHAPE is TRUE. [Y]
*     IN = LITERAL (Read)
*        The name of the input catalogue. This may be in any format
*        supported by the CAT library (see SUN/181). A file type of .FIT
*        is assumed if no file type is supplied.
*     METHOD = LITERAL (Read)
*        The method to be used when binning data values. This may be 
*        set to any unique abbreviation of the following:
*
*           -  MEAN      -- Mean of the input data values
*           -  MEDIAN    -- Median of the input data values
*           -  SIGMA     -- A sigma clipped mean
*
*        Only accessed if parameter SHAPE is TRUE. [MEAN]
*     MINVAL = _INTEGER (Read)
*        The minimum number of good input values which must be present in
*        a cell to create a good output value. Only accessed if parameter 
*        SHAPE is TRUE. [1]
*     OUT = NDF (Read)
*        The name of the output NDF. 
*     SHAPE = _LOGICAL (Read)
*        If a TRUE value is supplied for parameter SHAPE, then the output
*        NDF is 2-dimensional and inherits the spatial positions given in
*        the columns specified by COLX and COLY. If a FALSE value is 
*        supplied, the output NDF is 1-dimensional and the spatial position
*        of each data value is ignored. In this case, the number of pixels 
*        in the output NDF will equal the number of rows in the input
*        catalogue. The data values are stored in the NDF in the same order 
*        as in the input catalogue. [TRUE]
*     SIGMAS = _REAL (Read)
*        Number of standard deviations to reject data at. Only used if
*        METHOD is set to "SIGMA". Only accessed if parameter SHAPE is
*        TRUE. [4.0]

*  Examples:
*     polimage incat outimg p
*        Creates a 2-D NDF called "outimg" containing the values of column
*        P in the catalogue "incat.FIT". The catalogue values are binned
*        into a 2-D grid of pixels using the spatial positions given in
*        the columns "X" and "Y".
*     polimage incat outimg p noshape
*        Creates a 1-D NDF called "outimg" containing the values of column
*        P in the catalogue "incat.FIT". The number of pixels in the output
*        is equal to the number of rows in the catalogue, and the catalogue 
*        values are copied into the output in the order in which they occur 
*        in the catalogue.

*  Notes:
*     - If the output NDF is 2-dimensional, it will have an AXIS component
*     representing the COLX and COLY values. It will also inherit any WCS
*     information from the catalogue.
*     - If the output NDF is 1-dimensional, it will contain no AXIS or
*     WCS components.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-APR-1999 (DSB):
*        Original version.
*     12-APR-1999 (DSB):
*        Added parameter SHAPE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'AST_PAR'          ! AST_ constants and function declarations
      INCLUDE 'CAT_PAR'          ! CAT_ constants 
      INCLUDE 'PAR_ERR'          ! PAR error constants 

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER X_ID               
      PARAMETER ( X_ID = 1 )

      INTEGER Y_ID
      PARAMETER ( Y_ID = 2 )

      INTEGER D_ID
      PARAMETER ( D_ID = 3 )

      INTEGER V_ID
      PARAMETER ( V_ID = 4 )

*  Local Variables:
      CHARACTER METH*6           ! Binning method
      CHARACTER EXPR*(CAT__SZEXP)! CAT expression
      CHARACTER NAME*(CAT__SZCMP)! CAT column name
      CHARACTER LABEL*80         ! Label string
      CHARACTER TITLE*80         ! Title string
      CHARACTER UNITS*( CAT__SZUNI )! Units string
      DOUBLE PRECISION AIN( 2 )  ! GRID co-ords at point A
      DOUBLE PRECISION BIN( 2 )  ! GRID co-ords at point B
      DOUBLE PRECISION AOUT( 2 ) ! COLX,COLY co-ords at point A
      DOUBLE PRECISION BOUT( 2 ) ! COLX,COLY co-ords at point B
      INTEGER WINMAP             ! Pointer to an AST WinMap
      INTEGER CMAP               ! Pointer to an AST Mapping
      INTEGER TMAP               ! Pointer to an AST Mapping
      INTEGER NEL                ! Number of mapped elements
      INTEGER CLEN               ! Used length of a string
      INTEGER CI                 ! CAT identifier for input catalogue
      INTEGER GI( 4 )            ! CAT identifiers for columns to be read
      INTEGER GTTL               ! CAT identifier for TITLE parameter
      INTEGER IPAX1              ! Pointer to AXIS 1 centre array
      INTEGER IPAX2              ! Pointer to AXIS 2 centre array
      INTEGER IDTYPD             ! CAT component type for COLDAT
      INTEGER IDTYPX             ! CAT component type for COLX
      INTEGER IDTYPY             ! CAT component type for COLY
      INTEGER IP                 ! Pointers to arrays to be filled
      INTEGER IPBIN              ! Pointer to binned Stokes parameters
      INTEGER IPCOV              ! Pointer to workspace
      INTEGER IPD                ! Pointer to i/p data values
      INTEGER IPDST              ! Pointer to stacked i/p data values
      INTEGER IPNCON             ! Pointer to workspace
      INTEGER IPPNT              ! Pointer to workspace
      INTEGER IPPP               ! Pointer to workspace
      INTEGER IPUSED             ! Pointer to workspace
      INTEGER IPV                ! Pointer to i/p variance values
      INTEGER IPVAR              ! Pointer to dummy line variances
      INTEGER IPVBIN             ! Pointer to binned variances
      INTEGER IPVST              ! Pointer to stacked i/p variances
      INTEGER IPW1               ! Pointer to workspace
      INTEGER IPWRK1             ! Pointer to workspace
      INTEGER IPWRK2             ! Pointer to workspace
      INTEGER IPX                ! Pointer to i/p X values
      INTEGER IPY                ! Pointer to i/p Y values
      INTEGER IROW               ! Row index
      INTEGER IWCS               ! Pointer to AST FrameSet read from catalogue
      INTEGER IWCSO              ! Pointer to AST FrameSet for o/p NDF
      INTEGER MAXPOS             ! Position of maximum value
      INTEGER MINPOS             ! Position of minimum value
      INTEGER MINVAL             ! Min. no. of good i/p positions per cell
      INTEGER MXCNT              ! Max. no. of i/p positions in any o/p cell
      INTEGER NBAD               ! No. of bad values
      INTEGER NBIN               ! Total no. of output bins
      INTEGER NCIN               ! No. of vectors in catalogue
      INTEGER NCOL               ! No. of columns to be read
      INTEGER NMAT               ! Size of workspace 
      INTEGER NVAL               ! No. of BOX values supplied
      INTEGER NXBIN              ! No. of output bins along X axis
      INTEGER NYBIN              ! No. of output bins along Y axis
      INTEGER LBND( 2 )          ! O/p NDF lower pixel bounds
      INTEGER UBND( 2 )          ! O/p NDF upper pixel bounds
      LOGICAL VAR                ! Producing variances?
      LOGICAL XYPIX              ! Are (COLX,COLY) pixel co-ordinates?
      REAL BOX( 2 )              ! Bin size
      REAL NSIGMA                ! No. of sigmas to clip at
      REAL SXHI                  ! Upper bound of used region of X axis 
      REAL SXLO                  ! Lower bound of used region of X axis 
      REAL SYHI                  ! Upper bound of used region of Y axis 
      REAL SYLO                  ! Lower bound of used region of Y axis 
      REAL TR( 4 )               ! Coeff.s of (X,Y) -> cell indices mapping
      REAL TR2( 4 )              ! Coeff.s of cell indices -> (X,Y) mapping
      REAL X0                    ! X at bottom left of bottom left cell
      REAL Y0                    ! Y at bottom left of bottom left cell
      INTEGER INDF               ! NDF identifier for output NDF
      LOGICAL SHAPE              ! Does output NDF have shape?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Open the input catalogue, and get its name.
      CALL CAT_ASSOC( 'IN', 'READ', CI, STATUS )

*  See if the output NDF is to be 2-dimensional.
      CALL PAR_GET0L( 'SHAPE', SHAPE, STATUS )

*  If so, attempt to read an AST FrameSet from the input catalogue.
      IF( SHAPE ) THEN
         CALL POL1_GTCTW( CI, IWCS, STATUS )
      ELSE
         IWCS = AST__NULL
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the TITLE parameter (if any) from the input catalogue.
      CALL CAT_TIDNT( CI, 'TITLE', GTTL, STATUS )       
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL CAT_TIQAC( GTTL, 'VALUE', TITLE, STATUS )
         CALL CAT_TRLSE( GTTL, STATUS )
      ELSE
         TITLE = ' '
         CALL ERR_ANNUL( STATUS )
      END IF

*  Find the number of rows in the catalogue. This is the number of input
*  cells.
      CALL CAT_TROWS( CI, NCIN, STATUS )

*  Get information about the required catalogue columns.
*  =====================================================
*  Get CAT identifiers for the required columns. First get the data value
*  column.
      CALL POL1_GTCTC( 'COLDAT', CI, CAT__FITYP, ' ', GI( D_ID ), 
     :                 STATUS )
      NCOL = 1

*  If the output NDF has shape, get CAT identifiers for the position columns. 
      IF( SHAPE ) THEN 
         CALL POL1_GTCTC( 'COLX', CI, CAT__FITYP, ' ', GI( X_ID ), 
     :                     STATUS )
         CALL POL1_GTCTC( 'COLY', CI, CAT__FITYP, ' ', GI( Y_ID ), 
     :                     STATUS )
         NCOL = NCOL + 2
      END IF

*  Now get the variance column. If a null value is supplied, annul the error
*  and set a flag indicating variances are not being handled.
      IF( STATUS .NE. SAI__OK ) GO TO 999

      CALL POL1_GTCTC( 'COLVAR', CI, CAT__FITYP, ' ', GI( V_ID ), 
     :                  STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         VAR = .FALSE.
      ELSE
         VAR = .TRUE.
         NCOL = NCOL + 1
      END IF

*  Obtain the units of the data column.
      UNITS = ' '
      CALL CAT_TIQAC( GI( D_ID ), 'UNITS', UNITS, STATUS )

*  Obtain a label for the data value column.
      CALL CAT_TIDTP( GI( D_ID ), IDTYPD, STATUS )
      IF( IDTYPD .EQ. CAT__EITYP ) THEN
         CALL CAT_TIQAC( GI( D_ID ), 'EXPR', LABEL, STATUS )
      ELSE
         CALL CAT_TIQAC( GI( D_ID ), 'NAME', LABEL, STATUS )
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Now do cases where spatial position information is required.
*  ------------------------------------------------------------
      IF( SHAPE ) THEN 

*  Store the values of the required catalogue columns in work arrays.
*  ==================================================================
*  Allocate work space to hold the data from the required columns.
         CALL PSX_CALLOC( NCIN*NCOL, '_REAL', IP, STATUS )

*  Store pointers to the start of the data from each individual column.
         IPX = IP + NCIN*VAL__NBR*( X_ID - 1 )
         IPY = IP + NCIN*VAL__NBR*( Y_ID - 1 )
         IPD = IP + NCIN*VAL__NBR*( D_ID - 1 )
         IF( VAR ) IPV = IP + NCIN*VAL__NBR*( V_ID - 1 )

*  Check the pointers can be used.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Read the required columns from the catalogue into the work arrays
*  allocated above. This is done in a single pass through the catalogue
*  in order to speed it up a bit. 
         CALL POL1_CTCLM( CI, NCIN, NCOL, GI, %VAL( IP ), STATUS )

*  Get the coefficients of the linear transformation from (X,Y) position
*  to bin indices.
*  =====================================================================
*  Obtain the sizes of each bin.
         CALL PAR_GDRVR( 'BOX', 2, 1.0E-20, VAL__MAXR, BOX, NVAL, 
     :                   STATUS )

*  Duplicate the value if only a single value was given.  
         IF ( NVAL .LT. 2 ) BOX( 2 ) = BOX( 1 )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the maximum and minimum COLX value.
         CALL KPG1_MXMNR( .TRUE., NCIN, %VAL( IPX ), NBAD, SXHI,
     :                     SXLO, MAXPOS, MINPOS, STATUS )

*  Find the maximum and minimum COLY value.
         CALL KPG1_MXMNR( .TRUE., NCIN, %VAL( IPY ), NBAD, SYHI,
     :                     SYLO, MAXPOS, MINPOS, STATUS )

*  Find the number of bins along each axis.
         NXBIN = INT( ( SXHI - SXLO ) / BOX( 1 ) ) + 1
         NYBIN = INT( ( SYHI - SYLO ) / BOX( 2 ) ) + 1

*  Find the total number of bins.
         NBIN = NXBIN*NYBIN

*  Find the X and Y values corresponding to the bottom left corner of the 
*  bottom left bin.
         X0 = SXLO - 0.5*( NXBIN*BOX( 1 ) - SXHI + SXLO ) 
         Y0 = SYLO - 0.5*( NYBIN*BOX( 2 ) - SYHI + SYLO ) 

*  Find the coefficients of the transformation. The X cell index for a
*  position (X,Y) is given by INT( TR( 1 ) + TR( 2 )*X ), the Y cell
*  index is given by INT( TR( 3 ) + TR( 4 )*Y ).
         TR( 1 ) = 1.0 - X0/BOX( 1 )
         TR( 2 ) = 1.0/BOX( 1 )
         TR( 3 ) = 1.0 - Y0/BOX( 2 )
         TR( 4 ) = 1.0/BOX( 2 )

*  Decide on the pixel origin of the output NDF. If the catalogue has
*  a WCS FrameSet in which the base Frame is a PIXEL Frame, set the origin
*  from the input data. Otherwise, assume an origin of (1,1).
         LBND( 1 ) = 1
         LBND( 2 ) = 1
         UBND( 1 ) = NXBIN
         UBND( 2 ) = NYBIN
         XYPIX = .FALSE.
   
         IF( IWCS .NE. AST__NULL ) THEN
            IF( AST_GETC( AST_GETFRAME( IWCS, AST__BASE, STATUS ), 
     :                    'DOMAIN', STATUS ) .EQ. 'PIXEL' ) THEN
               XYPIX = .TRUE.
               LBND( 1 ) = INT( SXLO ) + 1
               LBND( 2 ) = INT( SYLO ) + 1
               UBND( 1 ) = LBND( 1 ) + NXBIN - 1
               UBND( 2 ) = LBND( 2 ) + NYBIN - 1
            END IF
         END IF

*  Create the output NDF.
         CALL NDF_CREAT( 'OUT', '_REAL', 2, LBND, UBND, INDF, STATUS )

*  Get the default WCS FrameSet from the output NDF.
         CALL NDF_GTWCS( INDF, IWCSO, STATUS )

*  If the input catalogue has a WCS FrameSet, merge it into the WCS
*  component of the output NDF. This can only be done if the X and Y
*  columns are not CAT expressions. This is because the COLX and COLY
*  values must correspond to the Base Frame of the WCS  FrameSet.
         CALL CAT_TIDTP( GI( X_ID ), IDTYPX, STATUS )
         CALL CAT_TIDTP( GI( Y_ID ), IDTYPY, STATUS )
   
         IF( IDTYPX .EQ. CAT__FITYP .AND. IDTYPY .EQ. CAT__FITYP .AND.
     :       IWCS .NE. AST__NULL ) THEN

*  Store co-ords of 2 points (A & B) in the GRID Frame of the output NDF.
            AIN( 1 ) = 0.0D0
            AIN( 2 ) = 0.0D0
            BIN( 1 ) = 1.0D0
            BIN( 2 ) = 1.0D0

*  Store the co-ords of the same 2 points in the COLX,COLY Frame of the
*  input catalogue.
            AOUT( 1 ) = DBLE( 0.5 - TR( 1 ) ) / DBLE( TR( 2 ) )
            AOUT( 2 ) = DBLE( 0.5 - TR( 3 ) ) / DBLE( TR( 4 ) )
            BOUT( 1 ) = DBLE( 1.5 - TR( 1 ) ) / DBLE( TR( 2 ) )
            BOUT( 2 ) = DBLE( 1.5 - TR( 3 ) ) / DBLE( TR( 4 ) )

*  Form a WinMap which maps output GRID positions into (COLX,COLY)
*  positions.
            WINMAP = AST_WINMAP( 2, AIN, BIN, AOUT, BOUT, ' ', STATUS )

*  Get the Mapping from (COLX,COLY) Frame to the Current Frame in the
*  input catalogues WCS FrameSet.
            CMAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, 
     :                             STATUS )

*  Merge these Mappings to get the Mapping from the NDF GRID Frame to the
*  catalogues Current Frame, and simplify.
            TMAP = AST_SIMPLIFY( AST_CMPMAP( WINMAP, CMAP, .TRUE., ' ', 
     :                                       STATUS ), STATUS )

*  Add the input catalogues WCS FrameSet into the default FrameSet
*  obtained from the output NDF.  
            CALL AST_ADDFRAME( IWCSO, AST__BASE, TMAP, IWCS, STATUS )

         END IF

*  Now find the largest number of input positions in any bin.
*  ===========================================================
*  Allocate an integer work array with one element per bin to hold the 
*  number of input positions in each bin.
         CALL PSX_CALLOC( NBIN, '_INTEGER', IPW1, STATUS )

*  Check the pointer can be used.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Count the number of input catalogue positions contained in each output
*  cell. The largest number in any one cell is returned.
         CALL POL1_CLCNT( NCIN, %VAL( IPX ), %VAL( IPY ), TR, NXBIN, 
     :                    NYBIN, %VAL( IPW1 ), MXCNT, STATUS )

*  Now copy the input catalogue values into arrays suitable for binning
*  using the vector routines of CCDPACK.
*  =====================================================================
*  Each value to be binned requires a 2D array in which each column
*  corresponds to one output cell. The CCDPACK routines combine the values 
*  in each column to form a combined column value which is stored in the
*  output catalogue. First, allocate an array for the total intensity.
         CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPDST, STATUS )

*  Check the pointer can be used.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Copy the data values from the input catalogue to the work array.
         CALL POL1_STK2( NCIN, %VAL( IPD ), %VAL( IPX ), %VAL( IPY ), 
     :                   NXBIN, NYBIN, MXCNT, TR, %VAL( IPDST ), 
     :                   %VAL( IPW1 ), STATUS )

*  If required, do the same for the variances.
         IF( VAR ) THEN
            CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPVST, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999
   
            CALL POL1_STK2( NCIN, %VAL( IPV ), %VAL( IPX ), %VAL( IPY ), 
     :                      NXBIN, NYBIN, MXCNT, TR, %VAL( IPVST ), 
     :                      %VAL( IPW1 ), STATUS )

         END IF

*  Now find the binned values using vector routines copied from CCDPACK.
*  ==================================================================
*  Get the binning method to use.
         CALL PAR_CHOIC( 'METHOD', ' ', 'MEDIAN,MEAN,SIGMA', .FALSE.,
     :                   METH, STATUS )

*  If using sigma clipping, get the number of sigmas to clip at.
         IF ( METH .EQ. 'SIGMA' ) THEN
            CALL PAR_GDR0R( 'SIGMAS', 4.0, 0.1, 100.0, .FALSE., NSIGMA, 
     :                      STATUS )
         END IF

*  Get the absolute number of good input values required to create a good 
*  output value.
         CALL PAR_GET0I( 'MINVAL', MINVAL, STATUS )
         MINVAL = MIN( 1, MINVAL )

*  Allocate work arrays needed by the binning routines.
         CALL PSX_CALLOC( MXCNT, '_REAL', IPWRK1, STATUS )
         CALL PSX_CALLOC( MXCNT, '_REAL', IPWRK2, STATUS )
         CALL PSX_CALLOC( MXCNT, '_DOUBLE', IPNCON, STATUS )
         CALL PSX_CALLOC( MXCNT, '_INTEGER', IPPNT, STATUS )
         CALL PSX_CALLOC( MXCNT, '_LOGICAL', IPUSED, STATUS )

         IF( VAR ) THEN
            CALL PSX_CALLOC( MXCNT, '_DOUBLE', IPPP, STATUS )
            NMAT = MXCNT*( MXCNT + 1 )/2 
            CALL PSX_CALLOC( MXCNT*NMAT, '_DOUBLE', IPCOV, STATUS )
         END IF

*  Map the DATA array of the output NDF. This will hold the binned data 
*  values. 
         CALL NDF_MAP( INDF, 'DATA', '_REAL', 'WRITE', IPBIN, NEL, 
     :                 STATUS )

*  Check the pointers can be used.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Do the binning. First deal with cases where variances are available.
         IF( VAR ) THEN

*  Map the output VARIANCE array of the output NDF. This will hold the 
*  binned variance values. 
            CALL NDF_MAP( INDF, 'VARIANCE', '_REAL', 'WRITE', IPVBIN, 
     :                    NEL, STATUS )

*  Check the pointers can be used.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Bin the data values and variances...
            CALL POL1_CM1RR( %VAL( IPDST ), NBIN, MXCNT, %VAL( IPVST ),
     :                      METH, MINVAL, NSIGMA, %VAL( IPBIN ),
     :                      %VAL( IPVBIN ), %VAL( IPWRK1 ), 
     :                      %VAL( IPWRK2 ), %VAL( IPPP ), %VAL( IPCOV ), 
     :                      NMAT, %VAL( IPNCON ), %VAL( IPPNT ), 
     :                      %VAL( IPUSED ), STATUS )

*  Now do the binning if there are no variances.
         ELSE

*  Allocate an array to hold the variance to use for each line of data, 
*  and set each element to 1.0 (i.e. give all input values equal weight).
            CALL PSX_CALLOC( MXCNT, '_DOUBLE', IPVAR, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999
            CALL POL1_SETD( MXCNT, 1.0D0, %VAL( IPVAR ), STATUS )

*  Bin the data values.
            CALL POL1_CM3RR( %VAL( IPDST ), NBIN, MXCNT, %VAL( IPVAR ),
     :                       METH, MINVAL, NSIGMA, %VAL( IPBIN ),
     :                       %VAL( IPWRK1 ), %VAL( IPWRK2 ),
     :                       %VAL( IPNCON ), %VAL( IPPNT ), 
     :                       %VAL( IPUSED ), STATUS )

         END IF

*  Now do the cases where no spatial information is required.
*  ----------------------------------------------------------
      ELSE

*  Create the 1-d output NDF.
         CALL NDF_CREAT( 'OUT', '_REAL', 1, 1, NCIN, INDF, STATUS )

*  Map the DATA array.
         CALL NDF_MAP( INDF, 'DATA', '_REAL', 'WRITE', IPBIN, NEL, 
     :                 STATUS )

*  Check the pointers can be used.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Read the data values from the catalogue into the NDF DATA array.
         CALL POL1_CTCLM( CI, NCIN, 1, GI( D_ID ), %VAL( IPBIN ), 
     :                    STATUS )

*  If variances are required, do the same for the VARIANCE array.
         IF( VAR ) THEN

            CALL NDF_MAP( INDF, 'VARIANCE', '_REAL', 'WRITE', IPVBIN, 
     :                    NEL, STATUS )

            IF( STATUS .NE. SAI__OK ) GO TO 999

            CALL POL1_CTCLM( CI, NCIN, 1, GI( V_ID ), %VAL( IPVBIN ), 
     :                       STATUS )

         END IF

      END IF

*  Store ancillary information in the output NDF.
*  ==============================================

*  Store the WCS FrameSet in the output NDF.
      IF( SHAPE ) CALL NDF_PTWCS( IWCSO, INDF, STATUS )

*  Set the UNITS component of the output NDF.   
      CLEN = CHR_LEN( UNITS )
      IF( CLEN .GT. 0 ) CALL NDF_CPUT( UNITS( : CLEN ), INDF, 
     :                                 'UNITS', STATUS )

*  Set the TITLE component of the output NDF.   
      CLEN = CHR_LEN( TITLE )
      IF( CLEN .GT. 0 ) CALL NDF_CPUT( TITLE( : CLEN ), INDF, 
     :                                 'TITLE', STATUS )

*  Set the LABEL component of the output NDF.   
      CLEN = CHR_LEN( LABEL )
      IF( CLEN .GT. 0 ) CALL NDF_CPUT( LABEL( : CLEN ), INDF, 'LABEL', 
     :                                 STATUS )

*  If the COLX and COLY values are not known to be pixel co-ordinates,
*  create AXIS structures in the output NDF holding the COLX and COLY
*  values at the centre of each pixel.
      IF( SHAPE .AND. .NOT. XYPIX ) THEN 

*  Store the coefficients of the transformation from cell indices to
*  (X,Y) coordinates at the cell centre.
         TR2( 1 ) = X0 - 0.5*BOX( 1 )
         TR2( 2 ) = BOX( 1 )
         TR2( 3 ) = Y0 - 0.5*BOX( 2 )
         TR2( 4 ) = BOX( 2 )

*  Map the AXIS Centre arrays.
         CALL NDF_AMAP( INDF, 'CENTRE', 1, '_REAL', 'WRITE', IPAX1, NEL,
     :                  STATUS )
         CALL NDF_AMAP( INDF, 'CENTRE', 2, '_REAL', 'WRITE', IPAX2, NEL,
     :                  STATUS )

*  Store the Axis values.
         CALL POL1_AXSET( TR2, NXBIN, NYBIN, %VAL( IPAX1 ), 
     :                    %VAL( IPAX2 ), STATUS )

*  Store the Axes Label and Units strings.  
         CALL CAT_TIQAC( GI( X_ID ), 'NAME', LABEL, STATUS )
         CLEN = CHR_LEN( LABEL )
         IF( CLEN .GT. 0 ) CALL NDF_ACPUT( LABEL( : CLEN ), INDF,
     :                                     'LABEL', 1, STATUS )

         CALL CAT_TIQAC( GI( X_ID ), 'UNITS', UNITS, STATUS )
         CLEN = CHR_LEN( UNITS )
         IF( CLEN .GT. 0 ) CALL NDF_ACPUT( UNITS( : CLEN ), INDF,
     :                                     'UNITS', 1, STATUS )

         CALL CAT_TIQAC( GI( Y_ID ), 'NAME', LABEL, STATUS )
         CLEN = CHR_LEN( LABEL )
         IF( CLEN .GT. 0 ) CALL NDF_ACPUT( LABEL( : CLEN ), INDF,
     :                                     'LABEL', 2, STATUS )

         CALL CAT_TIQAC( GI( Y_ID ), 'UNITS', UNITS, STATUS )
         CLEN = CHR_LEN( UNITS )
         IF( CLEN .GT. 0 ) CALL NDF_ACPUT( UNITS( : CLEN ), INDF,
     :                                     'UNITS', 2, STATUS )

      END IF

*  Closedown sequence.
*  ===================

*  Arrive here if an error occurs.
 999  CONTINUE

*  Release work space.
      IF( SHAPE ) THEN 

         CALL PSX_FREE( IP, STATUS )
         CALL PSX_FREE( IPW1, STATUS )
         CALL PSX_FREE( IPDST, STATUS )
         CALL PSX_FREE( IPWRK1, STATUS )
         CALL PSX_FREE( IPWRK2, STATUS )
         CALL PSX_FREE( IPNCON, STATUS )
         CALL PSX_FREE( IPPNT, STATUS )
         CALL PSX_FREE( IPUSED, STATUS )
   
         IF( VAR ) THEN
            CALL PSX_FREE( IPVST, STATUS )
            CALL PSX_FREE( IPPP, STATUS )
            CALL PSX_FREE( IPCOV, STATUS )
         ELSE
            CALL PSX_FREE( IPVAR, STATUS )
         END IF

      END IF

*  Release the input catalogue identifier.
      CALL CAT_TRLSE( CI, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLIMAGE_ERR', 'POLIMAGE: Error converting a '//
     :                 'catalogue to a 1 or 2-D NDF.', STATUS )
      END IF

      END
