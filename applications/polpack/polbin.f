      SUBROUTINE POLBIN( STATUS )
*+
*  Name:
*     POLBIN

*  Purpose:
*     Bins a catalogue containing Stokes parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLBIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new catalogue of polarization vectors by
*     binning the Stokes parameters in the supplied catalogue. The columns in
*     the supplied catalogue should correspond to those created by POLVEC.
*
*     The bins used form a grid of equally sized rectangular cells, the
*     dimensions of each cell being specified by the parameter BOX in
*     terms of the X and Y columns in the catalogue. The grid contains 
*     sufficient cells to span the entire range of X and Y covered by the
*     input catalogue. Each position in the output catalogue corresponds
*     to one of these cells. The Stokes parameters for the cell are formed
*     by combining together the Stokes parameters of all input positions
*     which fall within the cell, using the method specified by the
*     parameter METHOD. The degree of polarization, angle of polarization, 
*     and polarized intensity are then derived from these combined Stokes 
*     parameters. The (X,Y) positions in the output catalogue are the 
*     positions at the centre of each of the cells.

*  Usage:
*     polbin in out box [method]

*  ADAM Parameters:
*     BOX( 2 ) = _REAL (Read)
*        The x and y bin sizes. These values refer to the coordinate Frame 
*        defined by columns "X" and "Y" and will usually be in units of pixels.
*     DEBIAS = _LOGICAL (Read)
*        TRUE if a correction for statistical bias is to be made to
*        percentage polarization and polarized intensity. The returned 
*        variance values are unchanged. This correction only applies to 
*        calculations of linear polarization, and cannot be used if the 
*        input catalogue does not contain variance values. If a null value 
*        (!) is supplied, then the correction is applied if output variances
*        are being created, and not otherwise.           [!]
*     IN = LITERAL (Read)
*        The name of the input catalogue. A file type of .FIT is assumed
*        if none is provided.
*     METHOD = LITERAL (Read)
*        The method to be used when binning Stokes parameters. This may be 
*        set to any unique abbreviation of the following:
*           -  MEAN      -- Mean of the input data values
*           -  MEDIAN    -- Median of the input data values
*           -  SIGMA     -- A sigma clipped mean
*        [MEDIAN]
*     MINVAL = _INTEGER (Read)
*        The minimum number of good input values which must be present in
*        a cell to create a good output value. [1]
*     OUT = LITERAL (Read)
*        The name of the output catalogue. A file type of .FIT is assumed
*        if none is provided.
*     SIGMAS = _REAL (Read)
*        Number of standard deviations to reject data at. Only used if
*        METHOD is set to "SIGMA". [4.0]

*  Examples:
*     polbin intab outtab 4
*        Bins the Stokes parameters in catalogue "intab.FIT" and produces
*        catalogue "outtab.FIT" containing binned Stokes parameters and
*        corresponding polarization parameters. Each bin measures 4 pixels
*        along both the X and Y axes, and has a value based on the median 
*        of the corresponding input Stokes values.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-MAR-1998 (DSB):
*        Original version, derived from kappa:vecplot.
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

*  Local Constants:
      INTEGER X_ID               
      PARAMETER ( X_ID = 1 )

      INTEGER Y_ID
      PARAMETER ( Y_ID = 2 )

      INTEGER I_ID
      PARAMETER ( I_ID = 3 )

      INTEGER V_ID
      PARAMETER ( V_ID = 4 )

      INTEGER Q_ID
      PARAMETER ( Q_ID = 4 )

      INTEGER U_ID
      PARAMETER ( U_ID = 5 )

      INTEGER DI_ID
      PARAMETER ( DI_ID = 6 )

      INTEGER DV_ID
      PARAMETER ( DV_ID = 5 )

      INTEGER DQ_ID
      PARAMETER ( DQ_ID = 7 )

      INTEGER DU_ID
      PARAMETER ( DU_ID = 8 )

*  Local Variables:
      CHARACTER METH*6           ! Binning method
      CHARACTER NAME*(CAT__SZCMP)! CAT column name
      CHARACTER STOKES*3         ! Identifiers for cube planes
      CHARACTER UNITS*( CAT__SZUNI )! Units of the total intensity column
      INTEGER CIIN               ! CAT identifier for input catalogue
      INTEGER CIOUT              ! CAT identifier for output catalogue
      INTEGER GI( 8 )            ! CAT identifiers for columns to be read
      INTEGER GANG               ! CAT identifiers for ANGROT parameter
      INTEGER IP                 ! Pointers to arrays to be filled
      INTEGER IPBIN              ! Pointer to binned Stokes parameters
      INTEGER IPCOV              ! Pointer to workspace
      INTEGER IPI                ! Pointer to i/p I values
      INTEGER IPIBN              ! Pointer to binned I values
      INTEGER IPIST              ! Pointer to stacked i/p I values
      INTEGER IPNCON             ! Pointer to workspace
      INTEGER IPPNT              ! Pointer to workspace
      INTEGER IPPP               ! Pointer to workspace
      INTEGER IPQ                ! Pointer to i/p Q values
      INTEGER IPQBN              ! Pointer to binned Q values
      INTEGER IPQST              ! Pointer to stacked i/p Q values
      INTEGER IPU                ! Pointer to i/p U values
      INTEGER IPUBN              ! Pointer to binned U values
      INTEGER IPUSED             ! Pointer to workspace
      INTEGER IPUST              ! Pointer to stacked i/p U values
      INTEGER IPV                ! Pointer to i/p V values
      INTEGER IPVAR              ! Pointer to dummy line variances
      INTEGER IPVBIN             ! Pointer to binned variances
      INTEGER IPVBN              ! Pointer to binned V values
      INTEGER IPVI               ! Pointer to i/p I variances
      INTEGER IPVIBN             ! Pointer to binned I variances
      INTEGER IPVIST             ! Pointer to stacked i/p I variances
      INTEGER IPVQ               ! Pointer to i/p Q variances
      INTEGER IPVQBN             ! Pointer to binned Q variances
      INTEGER IPVQST             ! Pointer to stacked i/p Q variances
      INTEGER IPVST              ! Pointer to i/p V values
      INTEGER IPVU               ! Pointer to i/p U variances
      INTEGER IPVUBN             ! Pointer to binned U variances
      INTEGER IPVUST             ! Pointer to stacked i/p U variances
      INTEGER IPVV               ! Pointer to i/p V variances
      INTEGER IPVVBN             ! Pointer to binned V variances
      INTEGER IPVVST             ! Pointer to stacked i/p V variances
      INTEGER IPW1               ! Pointer to workspace
      INTEGER IPWRK1             ! Pointer to workspace
      INTEGER IPWRK2             ! Pointer to workspace
      INTEGER IPX                ! Pointer to i/p X values
      INTEGER IPY                ! Pointer to i/p Y values
      INTEGER IROW               ! Row index
      INTEGER IWCS               ! Pointer to AST FrameSet read from catalogue
      INTEGER MAXPOS             ! Position of maximum value
      INTEGER MINPOS             ! Position of minimum value
      INTEGER MINVAL             ! Min. no. of good i/p positions per cell
      INTEGER MXCNT              ! Max. no. of i/p positions in any o/p cell
      INTEGER NBAD               ! No. of bad values
      INTEGER NBIN               ! Total no. of output bins
      INTEGER NCIN               ! No. of vectors in catalogue
      INTEGER NCOL               ! No. of columns to be read
      INTEGER NMAT               ! Size of workspace 
      INTEGER NSTOKE             ! No. of planes in cube
      INTEGER NVAL               ! No. of BOX values supplied
      INTEGER NXBIN              ! No. of output bins along X axis
      INTEGER NYBIN              ! No. of output bins along Y axis
      LOGICAL CIRC               ! Doing circular polarimetry?
      LOGICAL DEBIAS             ! Statistical de-biassing required?
      LOGICAL GOTWCS             ! Was a FrameSet read from the input catalogue?
      LOGICAL NULL1              ! Null value flag
      LOGICAL NULL2              ! Null value flag
      LOGICAL NULL3              ! Null value flag
      LOGICAL VAR                ! Producing variances?
      REAL ANGROT                ! ACW angle from X pixel axis to analyser axis (degs)
      REAL BOX( 2 )              ! Bin size
      REAL NSIGMA                ! No. of sigmas to clip at
      REAL RTOD                  ! Conversion factor; radians to degrees
      REAL SXHI                  ! Upper bound of used region of X axis 
      REAL SXLO                  ! Lower bound of used region of X axis 
      REAL SYHI                  ! Upper bound of used region of Y axis 
      REAL SYLO                  ! Lower bound of used region of Y axis 
      REAL Q                     ! Stored Q in input catalogue
      REAL U                     ! Stored U in input catalogue
      REAL ANG                 ! Stored angle in input catalogue
      REAL TR( 4 )               ! Coeff.s of (X,Y) -> cell indices mapping
      REAL TR2( 4 )              ! Coeff.s of cell indices -> (X,Y) mapping
      REAL X0                    ! X at bottom left of bottom left cell
      REAL Y0                    ! Y at bottom left of bottom left cell
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the input catalogue, and get its name.
      CALL CAT_ASSOC( 'IN', 'READ', CIIN, STATUS )

*  Store the values of the required catalogue columns in work arrays.
*  ==================================================================
*  Find the number of rows in the catalogue. This is the number of input
*  cells.
      CALL CAT_TROWS( CIIN, NCIN, STATUS )

*  Get CAT identifiers for the required columns. First get position columns
*  (X and Y) and total intensity (I).
      CALL CAT_TIDNT( CIIN, 'X', GI( X_ID ), STATUS )       
      CALL CAT_TIDNT( CIIN, 'Y', GI( Y_ID ), STATUS )       
      CALL CAT_TIDNT( CIIN, 'I', GI( I_ID ), STATUS )       

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If there is a column named "V" we are dealing with circular polarimetry.
*  Attempt to get a CAT identifier for the "V" column. If an error occurs,
*  annul it and assume we are dealing with linear polarimetry. In this
*  case get columns for Q and U instead. 
      CALL CAT_TIDNT( CIIN, 'V', GI( V_ID ), STATUS )       
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         CIRC = .FALSE.
         CALL CAT_TIDNT( CIIN, 'Q', GI( Q_ID ), STATUS )       
         CALL CAT_TIDNT( CIIN, 'U', GI( U_ID ), STATUS )       
      ELSE
         CIRC = .TRUE.
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Try to find corresponding error columns for these columns (except X
*  and Y). 
      CALL CAT_TIDNT( CIIN, 'DI', GI( DI_ID ), STATUS )       

      IF( CIRC ) THEN
         CALL CAT_TIDNT( CIIN, 'DV', GI( DV_ID ), STATUS )       
      ELSE
         CALL CAT_TIDNT( CIIN, 'DQ', GI( DQ_ID ), STATUS )       
         CALL CAT_TIDNT( CIIN, 'DU', GI( DU_ID ), STATUS )       
      END IF

*  If any of these error columns could not be found, do not use any of
*  them. Annul the error and set a flag indicating error calculations are
*  not required. Store the number of columns to be read from the catalogue.
      IF( STATUS .NE. SAI__OK ) THEN
         VAR = .FALSE.
         CALL ERR_BEGIN( STATUS )

         CALL CAT_TRLSE( GI( DI_ID ), STATUS )       

         IF( CIRC ) THEN
            CALL CAT_TRLSE( GI( DV_ID ), STATUS )       
            NCOL = 4
         ELSE
            CALL CAT_TRLSE( GI( DQ_ID ), STATUS )       
            CALL CAT_TRLSE( GI( DU_ID ), STATUS )       
            NCOL = 5
         END IF

         CALL ERR_END( STATUS )
         CALL ERR_ANNUL( STATUS )

      ELSE
         VAR = .TRUE.
         IF( CIRC ) THEN
            NCOL = 6
         ELSE
            NCOL = 8
         END IF
      END IF

*  Allocate work space to hold the data from the required columns.
      CALL PSX_CALLOC( NCIN*NCOL, '_REAL', IP, STATUS )

*  Store pointers to the start of the data from each individual column.
      IPX = IP + NCIN*VAL__NBR*( X_ID - 1 )
      IPY = IP + NCIN*VAL__NBR*( Y_ID - 1 )
      IPI = IP + NCIN*VAL__NBR*( I_ID - 1 )

      IF( CIRC ) THEN
         IPV = IP + NCIN*VAL__NBR*( V_ID - 1 )
      ELSE
         IPQ = IP + NCIN*VAL__NBR*( Q_ID - 1 )
         IPU = IP + NCIN*VAL__NBR*( U_ID - 1 )
      END IF

      IF( VAR ) THEN
         IPVI = IP + NCIN*VAL__NBR*( DI_ID - 1 )
         IF( CIRC ) THEN
            IPVV = IP + NCIN*VAL__NBR*( DV_ID - 1 )
         ELSE
            IPVQ = IP + NCIN*VAL__NBR*( DQ_ID - 1 )
            IPVU = IP + NCIN*VAL__NBR*( DU_ID - 1 )
         END IF
      END IF

*  Check the pointers can be used.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Read the required columns from the catalogue into the work arrays
*  allocated above. This is done in a single pass through the catalogue
*  in order to speed it up a bit. 
      CALL POL1_CTCLM( CIIN, NCIN, NCOL, GI, %VAL( IP ), STATUS )

*  Square the standard deviation columns so that they become variance values.
      IF( VAR ) THEN
         CALL POL1_SQUAR( NCIN, %VAL( IPVI ), STATUS )
         IF( CIRC ) THEN
            CALL POL1_SQUAR( NCIN, %VAL( IPVV ), STATUS )
         ELSE
            CALL POL1_SQUAR( NCIN, %VAL( IPVQ ), STATUS )
            CALL POL1_SQUAR( NCIN, %VAL( IPVU ), STATUS )
         END IF          
      END IF

*  If we are dealing with linear polarization, get the ACW angle from the
*  X axis to the reference direction (ANGROT). 
      IF( .NOT. CIRC ) THEN
         CALL CAT_TIDNT( CIIN, 'ANGROT', GANG, STATUS )       
         CALL CAT_TIQAR( GANG, 'VALUE', ANGROT, STATUS )
         CALL CAT_TRLSE( GANG, STATUS )
      END IF

*  Decide whether or not a bias correction is needed and possible.
*  ===============================================================
*  Check the global status.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  See if a correction is to be made to the percentage polarization to
*  correct for bias introduced as a result of the noise distribution not
*  being symmetric.
      CALL PAR_GET0L( 'DEBIAS', DEBIAS, STATUS )

*  If a null value is supplied, annull the error, and debias if the
*  variances required are available.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         DEBIAS = VAR

*  Otherwise issue a warning if the user wants to debias the results and there 
*  are no variances available.
      ELSE IF ( DEBIAS .AND. ( .NOT. VAR ) .AND. 
     :          STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_OUT( 'POLBIN_ERR3', 'Vectors will not be '/
     :                 /'corrected for statistical bias because no '/
     :                 /'variance values are available.', STATUS )
         CALL ERR_FLUSH( STATUS )
         DEBIAS = .FALSE.
      END IF

*  Get the coefficients of the linear transformation from (X,Y) position
*  to bin indices.
*  =====================================================================
*  Obtain the sizes of each bin.
      CALL PAR_GDRVR( 'BOX', 2, 1.0E-6, VAL__MAXR, BOX, NVAL, STATUS )

*  Duplicate the value if only a single value was given.  
      IF ( NVAL .LT. 2 ) BOX( 2 ) = BOX( 1 )

*  Find the maximum and minimum X value.
      CALL KPG1_MXMNR( .TRUE., NCIN, %VAL( IPX ), NBAD, SXHI,
     :                  SXLO, MAXPOS, MINPOS, STATUS )

*  Find the maximum and minimum Y value.
      CALL KPG1_MXMNR( .TRUE., NCIN, %VAL( IPY ), NBAD, SYHI,
     :                  SYLO, MAXPOS, MINPOS, STATUS )

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

*  Now find the largest number of input positions inn any bin.
*  ===========================================================
*  Allocate an integer work array with one element per bin to hold the 
*  number of input positions in each bin.
      CALL PSX_CALLOC( NBIN, '_INTEGER', IPW1, STATUS )

*  Check the pointer can be used.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Count the number of input catalogue positions contained in each output
*  cell. The largest number in any one cell is returned.
      CALL POL1_CLCNT( NCIN, %VAL( IPX ), %VAL( IPY ), TR, NXBIN, NYBIN,
     :                 %VAL( IPW1 ), MXCNT, STATUS )

*  Now copy the input Stokes parameters into arrays suitable for binning
*  using the vector routines of CCDPACK.
*  =====================================================================
*  Each value to be binned requires a 2D array in which each column
*  corresponds to one output cell. The CCDPACK routines combine the values 
*  in each column to form a combined column value which is stored in the
*  output catalogue. First, allocate an array for the total intensity.
      CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPIST, STATUS )

*  Check the pointer can be used.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Copy the total intensity values from the input catalogue to the work array.
      CALL POL1_STK2( NCIN, %VAL( IPI ), %VAL( IPX ), %VAL( IPY ), 
     :                NXBIN, NYBIN, MXCNT, TR, %VAL( IPIST ), 
     :                %VAL( IPW1 ), STATUS )

*  Do the same for the other required Stokes vectors (V, or Q and U).
      IF( CIRC ) THEN
         CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPVST, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 999

         CALL POL1_STK2( NCIN, %VAL( IPV ), %VAL( IPX ), %VAL( IPY ),
     :                   NXBIN, NYBIN, MXCNT, TR, %VAL( IPVST ), 
     :                   %VAL( IPW1 ), STATUS )

      ELSE
         CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPQST, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 999

         CALL POL1_STK2( NCIN, %VAL( IPQ ), %VAL( IPX ), %VAL( IPY ), 
     :                   NXBIN, NYBIN, MXCNT, TR, %VAL( IPQST ), 
     :                   %VAL( IPW1 ), STATUS )

         CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPUST, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 999

         CALL POL1_STK2( NCIN, %VAL( IPU ), %VAL( IPX ), %VAL( IPY ), 
     :                   NXBIN, NYBIN, MXCNT, TR, %VAL( IPUST ), 
     :                   %VAL( IPW1 ), STATUS )
      END IF

*  If required, do the same for the variances.
      IF( VAR ) THEN
         CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPVIST, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 999

         CALL POL1_STK2( NCIN, %VAL( IPVI ), %VAL( IPX ), %VAL( IPY ), 
     :                   NXBIN, NYBIN, MXCNT, TR, %VAL( IPVIST ), 
     :                   %VAL( IPW1 ), STATUS )

         IF( CIRC ) THEN
            CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPVVST, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999

            CALL POL1_STK2( NCIN, %VAL( IPVV ), %VAL( IPX ), 
     :                      %VAL( IPY ), NXBIN, NYBIN, MXCNT, 
     :                      TR, %VAL( IPVVST ), %VAL( IPW1 ), STATUS )

         ELSE
            CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPVQST, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999

            CALL POL1_STK2( NCIN, %VAL( IPVQ ), %VAL( IPX ), 
     :                      %VAL( IPY ), NXBIN, NYBIN, MXCNT, 
     :                      TR, %VAL( IPVQST ), %VAL( IPW1 ), STATUS )

            CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPVUST, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999

            CALL POL1_STK2( NCIN, %VAL( IPVU ), %VAL( IPX ), 
     :                      %VAL( IPY ), NXBIN, NYBIN, MXCNT, 
     :                      TR, %VAL( IPVUST ), %VAL( IPW1 ), STATUS )
         END IF

      END IF

*  Now find the binned Stokes parameters using vector routines copied
*  from CCDPACK.
*  ==================================================================
*  Get the binning method to use.
      CALL PAR_CHOIC( 'METHOD', ' ', 'MEDIAN,MEAN,SIGMA', .FALSE.,
     :                METH, STATUS )

*  If using sigma clipping, get the number of sigmas to clip at.
      IF ( METH .EQ. 'SIGMA' ) THEN
         CALL PAR_GDR0R( 'SIGMAS', 4.0, 0.1, 100.0, .FALSE., NSIGMA, 
     :                   STATUS )
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

*  Allocate an array to hold the binned values. This array is accessed as 
*  a cube by POL1_PLVEC. The cube consists of 2 or 3 planes (depending on
*  whether or not we are doing circular polarimetry), each spanned by the X
*  and Y axes (having NXBIN and NYBIN elements respectively). The first
*  plane contains total intensity, the second contains V or Q, and the
*  third (if used) contains U. Get pointers to the start of each plane.
      IF( CIRC ) THEN
         CALL PSX_CALLOC( NBIN*2, '_REAL', IPBIN, STATUS )
         IPIBN = IPBIN
         IPVBN = IPBIN + NBIN*VAL__NBR
         NSTOKE = 2
         STOKES = 'IV'
      ELSE
         CALL PSX_CALLOC( NBIN*3, '_REAL', IPBIN, STATUS )
         IPIBN = IPBIN
         IPQBN = IPBIN + NBIN*VAL__NBR
         IPUBN = IPBIN + 2*NBIN*VAL__NBR
         NSTOKE = 3
         STOKES = 'IQU'
      END IF

*  Check the pointers can be used.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Do the binning. First deal with cases where variances are available.
      IF( VAR ) THEN

*  Allocate arrays to hold the variances for the binned values.
         IF( CIRC ) THEN
            CALL PSX_CALLOC( NBIN*2, '_REAL', IPVBIN, STATUS )
            IPVIBN = IPVBIN
            IPVVBN = IPVBIN + NBIN*VAL__NBR
         ELSE
            CALL PSX_CALLOC( NBIN*3, '_REAL', IPVBIN, STATUS )
            IPVIBN = IPVBIN
            IPVQBN = IPVBIN + NBIN*VAL__NBR
            IPVUBN = IPVBIN + 2*NBIN*VAL__NBR
         END IF

*  Check the pointers can be used.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Bin the total intensity...
         CALL POL1_CM1RR( %VAL( IPIST ), NBIN, MXCNT, %VAL( IPVIST ),
     :                    METH, MINVAL, NSIGMA, %VAL( IPIBN ),
     :                    %VAL( IPVIBN ), %VAL( IPWRK1 ), 
     :                    %VAL( IPWRK2 ), %VAL( IPPP ), %VAL( IPCOV ), 
     :                    NMAT, %VAL( IPNCON ), %VAL( IPPNT ), 
     :                    %VAL( IPUSED ), STATUS )

*  Bin V (if needed)...
         IF( CIRC ) THEN
            CALL POL1_CM1RR( %VAL( IPVST ), NBIN, MXCNT, %VAL( IPVVST ),
     :                       METH, MINVAL, NSIGMA, %VAL( IPVBN ),
     :                       %VAL( IPVVBN ), %VAL( IPWRK1 ), 
     :                       %VAL( IPWRK2 ), %VAL( IPPP ), 
     :                       %VAL( IPCOV ), NMAT, %VAL( IPNCON ), 
     :                       %VAL( IPPNT ), %VAL( IPUSED ), STATUS )


*  Bin Q and U (if needed)...
         ELSE 
            CALL POL1_CM1RR( %VAL( IPQST ), NBIN, MXCNT, %VAL( IPVQST ),
     :                       METH, MINVAL, NSIGMA, %VAL( IPQBN ),
     :                       %VAL( IPVQBN ), %VAL( IPWRK1 ), 
     :                       %VAL( IPWRK2 ), %VAL( IPPP ), 
     :                       %VAL( IPCOV ), NMAT, %VAL( IPNCON ), 
     :                       %VAL( IPPNT ), %VAL( IPUSED ), STATUS )

            CALL POL1_CM1RR( %VAL( IPUST ), NBIN, MXCNT, %VAL( IPVUST ),
     :                       METH, MINVAL, NSIGMA, %VAL( IPUBN ),
     :                       %VAL( IPVUBN ), %VAL( IPWRK1 ), 
     :                       %VAL( IPWRK2 ), %VAL( IPPP ), 
     :                       %VAL( IPCOV ), NMAT, %VAL( IPNCON ), 
     :                       %VAL( IPPNT ), %VAL( IPUSED ), STATUS )
         END IF

*  Now do the binning if there are no variances.
      ELSE

*  Allocate an array to hold the variance to use for each line of data, 
*  and set each element to 1.0 (i.e. give all input values equal weight).
         CALL PSX_CALLOC( MXCNT, '_DOUBLE', IPVAR, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 999
         CALL POL1_SETD( MXCNT, 1.0D0, %VAL( IPVAR ), STATUS )

*  Total intensity...
         CALL POL1_CM3RR( %VAL( IPIST ), NBIN, MXCNT, %VAL( IPVAR ),
     :                    METH, MINVAL, NSIGMA, %VAL( IPIBN ),
     :                    %VAL( IPWRK1 ), %VAL( IPWRK2 ),
     :                    %VAL( IPNCON ), %VAL( IPPNT ), 
     :                    %VAL( IPUSED ), STATUS )

*  V (if needed)...
         IF( CIRC ) THEN
            CALL POL1_CM3RR( %VAL( IPVST ), NBIN, MXCNT, %VAL( IPVAR ),
     :                       METH, MINVAL, NSIGMA, %VAL( IPVBN ),
     :                       %VAL( IPWRK1 ), %VAL( IPWRK2 ),
     :                       %VAL( IPNCON ), %VAL( IPPNT ), 
     :                       %VAL( IPUSED ), STATUS )

*  Q and U (if needed)...
         ELSE 
            CALL POL1_CM3RR( %VAL( IPQST ), NBIN, MXCNT, %VAL( IPVAR ),
     :                       METH, MINVAL, NSIGMA, %VAL( IPQBN ),
     :                       %VAL( IPWRK1 ), %VAL( IPWRK2 ),
     :                       %VAL( IPNCON ), %VAL( IPPNT ), 
     :                       %VAL( IPUSED ), STATUS )

            CALL POL1_CM3RR( %VAL( IPUST ), NBIN, MXCNT, %VAL( IPVAR ),
     :                       METH, MINVAL, NSIGMA, %VAL( IPUBN ),
     :                       %VAL( IPWRK1 ), %VAL( IPWRK2 ),
     :                       %VAL( IPNCON ), %VAL( IPPNT ), 
     :                       %VAL( IPUSED ), STATUS )
         END IF

      END IF

*  Now create the output catalogue to receive the binned Stokes parameters
*  and corresponding Stokes parameters found.
*  =======================================================================
*  Attempt to read an AST FrameSet from the input catalogue. This WCS
*  information will be copied unchanged to the output catalogue when the
*  output catalogue is closed. Report an error if no WCS information is
*  available in the input catalogue.
      CALL KPG1_GTCTA( ' ', CIIN, 0, GI, GOTWCS, IWCS, STATUS )
      IF( IWCS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLBIN_1', 'No usable WCS coordinate system '//
     :                 'information is available in the input '//
     :                 'catalogue.', STATUS )
         GO TO 999
      END IF

*  Get the units string from total intensity column of the input catalogue.
      UNITS = ' '
      CALL CAT_TIQAC( GI( I_ID ), 'UNITS', UNITS, STATUS) 

*  Create the output catalogue.
      CALL POL1_MKCAT( 'OUT', IWCS, CIRC, UNITS, VAR, ANGROT, CIOUT, 
     :                 STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Now calculate the polarization parameters based on the binned Stokes
*  parameters, storing them in the catalogue created above.
*  ====================================================================
*  Store the coefficients of the transformation from cell indices to
*  (X,Y) coordinates at the cell centre.
      TR2( 1 ) = X0 - 0.5*BOX( 1 )
      TR2( 2 ) = BOX( 1 )
      TR2( 3 ) = Y0 - 0.5*BOX( 2 )
      TR2( 4 ) = BOX( 2 )

*  Calculate the polarization vectors. POL1_PLVEC has the cabability to
*  produce output images containing the polarization parameters, These
*  are not wanted here, but pointers still have to be given even though
*  they are ignored. Use IPI as a safe pointer.
      CALL POL1_PLVEC( TR2, NXBIN, NYBIN, NSTOKE, %VAL( IPBIN ), 
     :                 %VAL( IPVBIN ), ANGROT, STOKES, DEBIAS, VAR, 
     :                 .FALSE., 
     :                 .FALSE., .FALSE., .FALSE., .TRUE., CIOUT, 
     :                 %VAL( IPI ), %VAL( IPI ), %VAL( IPI ), 
     :                 %VAL( IPI ), %VAL( IPI ), %VAL( IPI ), 
     :                 %VAL( IPI ), %VAL( IPI ), STATUS )

*  Closedown sequence.
*  ===================

*  Arrive here if an error occurs.
 999  CONTINUE

*  Close the output catalogue, storing a copy of the WCS information from 
*  the input catalogue.
      CALL POL1_CLCAT( IWCS, CIOUT, STATUS )

*  Release work space.
      CALL PSX_FREE( IP, STATUS )
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPIST, STATUS )
      CALL PSX_FREE( IPWRK1, STATUS )
      CALL PSX_FREE( IPWRK2, STATUS )
      CALL PSX_FREE( IPNCON, STATUS )
      CALL PSX_FREE( IPPNT, STATUS )
      CALL PSX_FREE( IPUSED, STATUS )
      CALL PSX_FREE( IPBIN, STATUS )

      IF( .NOT. CIRC ) THEN
         CALL PSX_FREE( IPQST, STATUS )
         CALL PSX_FREE( IPUST, STATUS )
      ELSE
         CALL PSX_FREE( IPVST, STATUS )
      END IF

      IF( VAR ) THEN
         CALL PSX_FREE( IPVIST, STATUS )
         CALL PSX_FREE( IPPP, STATUS )
         CALL PSX_FREE( IPCOV, STATUS )
         CALL PSX_FREE( IPVBIN, STATUS )

         IF( CIRC ) THEN
            CALL PSX_FREE( IPVVST, STATUS )
         ELSE
            CALL PSX_FREE( IPVQST, STATUS )
            CALL PSX_FREE( IPVUST, STATUS )
         END IF

      ELSE
         CALL PSX_FREE( IPVAR, STATUS )
      END IF

*  Release the input catalogue identifier.
      CALL CAT_TRLSE( CIIN, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLBIN_ERR', 'POLBIN: Error binning a '//
     :                 'polarization catalogue.', STATUS )
      END IF

      END
