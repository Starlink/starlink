*+  ASMOOTH - Smooths n-D dataset
      SUBROUTINE ASMOOTH( STATUS )
*
*    Description :
*
*     The input n-D dataset is smoothed by a mask whose width varies as a
*     function of data value in order to attempt to achieve constant
*     signal to noise at defined contour levels. The profile can be
*     selected from a number of alternatives.
*
*     The number of dimensions of the input selected for smoothing
*     define the dimensionality of the mask - at the moment only 1
*     and 2-d are supported.
*
*    Environment parameters :
*
*     INP = UNIV(R)
*       The input dataset
*     OUT = UNIV(W)
*       The output dataset
*     SDIMS = INTEGER[] (R)
*       Dimensions to be smoothed
*     SNR = REAL(R)
*       Target signal to noise in smoothed output
*     FILTER = INTEGER(R)
*       Filter variety, 1=gaussian, 2=tophat, 3=cosine
*     WSTART = INTEGER(R)
*       Starting guess for mask width
*     WMAXSZ = INTEGER(R)
*       Maximum size of filter mask
*     BIAS = UNIV(R)
*       Bias if minimum data value is zero
*     OPT = INTEGER(R)
*       Option for setting data levels, 1=linear & regular, 2=logarithmic
*       and regular, 3=user supplied
*     NLEV = INTEGER(R)
*       Number of smoothing levels for OPT=1 or 2.
*     LEVS = REAL[] (R)
*       Array of data levels, used when OPT=3.
*
*    Method :
*
*     Having selected the dimensions over which smoothing is to take place,
*     ASMOOTH reorders the input axes of the input to place these dimensions
*     at the beginning of the list of an intermediate dataset of the same
*     dimensionality. This intermediate is then simply a stack of slices to
*     be smoothed by the ASMOOTH_INT routine.
*
*     FOR each slice with dimensionality SDIMS in the dataset
*       FOR each user defined data layer
*         Copy all data points d(i) in slice into work layer w(i)
*         IF d(i) < layer_min OR d(i) > layer_max THEN
*           w(i) = 0
*         ELSE IF d(i) in (layer_min,layer_max) THEN
*           w(i) = d(i)
*         END IF
*         IF lower bound of layer < 0 THEN
*           Find average data value in layer and redistribute this over
*           all good quality input pixels.
*         ELSE
*           REPEAT
*             New guess at mask width, hence new mask(j) values
*           UNTIL w_mean / sqrt(w_var * sum(mask(j)^2) ~= SNR
*           Smooth data layer by chosen mask and add to output
*         END IF
*       NEXT data layer
*     NEXT dataset slice
*
*    Deficiencies :
*
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     20 Oct 92 : V1.7-0  Original, adapted from SMOOTH (DJA)
*     30 Jun 93 : V1.7-1  Normalisation corrected (DJA)
*     17 Nov 93 : V1.7-2  Made width convergence more reliable (DJA)
*     25 Feb 94 : V1.7-3  Use BIT_ routines to do bit manipulations (DJA)
*     10 Mar 94 : V1.7-4  Revised quality handling, correct treatment of
*                         negative data slices. (DJA)
*     29 Jun 94 : V1.8-0  Put bias to make LOG spacing more useful (DJA)
*     24 Nov 94 : V1.8-1  Now use USI for user interface (DJA)
*     26 Mar 95 : V1.8-2  Use new data interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER                	CHR_LEN
*
*    Local constants :
*
      INTEGER                	ASM__MXLEV            	! Maximum data levels
        PARAMETER            	( ASM__MXLEV = 100 )
      INTEGER                	MXLINES               	! Max amount of history
        PARAMETER            	( MXLINES = 10 )
*
*    Local variables :
*
      CHARACTER*80           	HTXT(MXLINES)         	! History text
      CHARACTER*30           	STR                   	! Format string

      REAL			BIAS			! Data bias
      REAL                   	DMAX, DMIN            	! Max & min of data
      REAL                   	LEVEL(ASM__MXLEV+2)   	! Data levels for smooth
      REAL                   	SNR                   	! Requested signal to noise

      INTEGER                	A_X, A_Y, A_T         	! Axis numbers
      INTEGER                	DIMS(ADI__MXDIM)      	! Input dimensions
      INTEGER                	FILTER                	! Filter choice
      INTEGER                	I                     	! General loop variable
      INTEGER                	IDPTR                 	! Input data ptr
      INTEGER			IFID			! Input dataset id
      INTEGER                	INORDER(ADI__MXDIM)   	! Inverted transform
      INTEGER                	IQPTR                 	! Input quality ptr
      INTEGER                	IVPTR                 	! Input variance ptr
      INTEGER                	NDIM                  	! Input dimensionality
      INTEGER                	NELM                  	! Input # elements
      INTEGER                	NLEVEL                	! # smooth levels
      INTEGER                	NLINES                	! # lines of history used
      INTEGER                	NSDIM                 	! # smoothed dimensions
      INTEGER                	NSELEM                	! # elements in slice
      INTEGER                	NSLICE                	! # slices
      INTEGER                	ODPTR                 	! Output data ptr
      INTEGER			OFID			! Output dataset id
      INTEGER                	OPTION                	! Level selection option
      INTEGER                	SDIMS(ADI__MXDIM)     	! Dimensions to smooth
      INTEGER                	TDIMS(ADI__MXDIM)     	! Input variance dims
      INTEGER                	TLEN                  	! Length of char string
      INTEGER                	TNDIM                 	! Input variance dim'ality
      INTEGER                	TRDIMS(ADI__MXDIM)    	! Transformed dimensions
      INTEGER                	TRORD(ADI__MXDIM)     	! Transformed axis numbers
      INTEGER                	TDPTR, TQPTR, TVPTR   	! Tranformed input data
      INTEGER                	WDPTR, WQPTR          	! Workspace for layers
      INTEGER                	WSTART                	! Width starting value
      INTEGER                	WMAXSZ                	! Max width dimension

      LOGICAL                	IPRIM                 	! Input primitive?
      LOGICAL                	OK                    	! Input data ok?
      LOGICAL                	POISSON               	! Assume Poisson statistics
      LOGICAL                	REORDER               	! Reordered axes?
      LOGICAL                	QOK                   	! Input quality ok?
      LOGICAL                	VOK                   	! Input variance ok?
*
*    Version :
*
      CHARACTER*30           	VERSION
        PARAMETER            	( VERSION = 'ASMOOTH Version 1.8-1')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version
      CALL MSG_PRNT( VERSION )

*    Initialise
      CALL AST_INIT()

*    Get input and output files
      CALL USI_TASSOC2( 'INP', 'OUT', 'READ', IFID, OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Copy input to output
      CALL ADI_FCOPY( IFID, OFID, STATUS )

*    Check input data
      CALL BDI_CHKDATA( IFID, OK, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid numeric data', STATUS )
        GOTO 99
      ELSE IF ( NDIM .LT. 1 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Data must be at least 1-dimensional',
     :                                                   STATUS )
        GOTO 99
      END IF
      CALL BDI_MAPDATA( IFID, 'READ', IDPTR, STATUS )

*    Count input elements
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*    Input variance present?
      CALL BDI_CHKVAR( IFID, VOK, TNDIM, TDIMS, STATUS )
      IF ( VOK ) THEN
        CALL BDI_MAPVAR( IFID, 'READ', IVPTR, STATUS )
        CALL BDI_DELETE( OFID, 'Variance', STATUS )
        POISSON = .FALSE.
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
        CALL MSG_OUT( ' ', '! Warning, no variance present, assuming'/
     :                                /' Poisson statistics', STATUS )
        POISSON = .TRUE.
      END IF

*    Input quality present?
      CALL BDI_CHKQUAL( IFID, QOK, TNDIM, TDIMS, STATUS )
      IF ( QOK ) THEN
        CALL BDI_MAPMQUAL( IFID, 'READ', IQPTR, STATUS )
      END IF

*    Map output array
      CALL BDI_MAPDATA( OFID, 'WRITE', ODPTR, STATUS )

*    Display list of axes
      CALL BDI_PRIM( IFID, IPRIM, STATUS )
      IF ( .NOT. IPRIM ) THEN
        CALL AXIS_TLIST( IFID, NDIM, STATUS )
      END IF

*    Try to find X,Y axes and set default for SDIMS
      CALL AXIS_TFINDXYT( IFID, NDIM, A_X, A_Y, A_T, STATUS )
      IF ( (A_X .GT. 0) .AND. (A_Y.GT.0) ) THEN
        SDIMS(1) = A_X
        SDIMS(2) = A_Y
        NSDIM = 2
        CALL USI_DEF1I( 'SDIMS', NSDIM, SDIMS, STATUS )
      ELSE IF ( NDIM .EQ. 1 ) THEN
        CALL USI_DEF0I( 'SDIMS', 1, STATUS )
      END IF

*    Get dimensions to be smoothed
      CALL USI_GET1I( 'SDIMS', ADI__MXDIM, SDIMS, NSDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( NSDIM .GT. 2 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Must be 1 or 2 dimensions at present',
     :                                                    STATUS )
      END IF

*    Get mode for selecting data levels
      CALL USI_GET0I( 'OPT', OPTION, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get min & max in data
      IF ( QOK ) THEN
        CALL ARR_RANG1RQ( NELM, %VAL(IDPTR), %VAL(IQPTR), QUAL__MASK,
     :                                           DMIN, DMAX, STATUS )
      ELSE
        CALL ARR_RANG1R( NELM, %VAL(IDPTR), DMIN, DMAX, STATUS )
      END IF
      CALL MSG_SETR( 'MIN', DMIN )
      CALL MSG_SETR( 'MAX', DMAX )
      CALL MSG_PRNT( 'Data range is from ^MIN to ^MAX' )

*    Choose levels from option
      BIAS = 0.0
      IF ( (OPTION.EQ.1) .OR. (OPTION.EQ.2) ) THEN

*      Get number of levels
        CALL USI_GET0I( 'NLEV', NLEVEL, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        IF ( (NLEVEL.LT.1) .OR. (NLEVEL.GT.ASM__MXLEV) ) THEN
          STATUS = SAI__ERROR
          CALL MSG_SETI( 'MAX', ASM__MXLEV )
          CALL ERR_REP( ' ', 'Number of levels must lie between 1'/
     :                                       /' and ^MAX', STATUS )
          GOTO 99
        END IF

*      Check for silly data range
        IF ( (OPTION .EQ. 2) .AND. (DMIN.LE.0.0) ) THEN
          CALL MSG_PRNT( 'The data contains values <= zero - supply'/
     :  /' a bias value to make the minimum positive, or ! to quit' )
          CALL USI_DEF0R( 'BIAS', 0.01*DMAX-DMIN, STATUS )
          CALL USI_GET0R( 'BIAS', BIAS, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Check bias big enough
          IF ( (DMIN+BIAS) .LE. 0.0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETR( 'BIAS', DMIN )
            CALL ERR_REP( ' ', 'Bias value must be greater than ^BIAS',
     :                    STATUS )
            GOTO 99
          END IF

*        Adjust data min and max
          DMIN = DMIN + BIAS
          DMAX = DMAX + BIAS

        END IF

*      Linearly spaced levels
        IF ( OPTION .EQ. 1 ) THEN
          DO I = 1, NLEVEL
            LEVEL(I) = DMIN + (REAL(I)-0.5)*(DMAX-DMIN)/REAL(NLEVEL)
          END DO

*      Logarithmically spaced levels
        ELSE

          DO I = 1, NLEVEL
            LEVEL(I) = LOG10(DMIN) + (REAL(I)-0.5)*
     :               (LOG10(DMAX)-LOG10(DMIN))/REAL(NLEVEL)
            LEVEL(I) = 10.0**LEVEL(I) - BIAS
          END DO

*        RESTORE data min and max
          DMIN = DMIN - BIAS
          DMAX = DMAX - BIAS

        END IF

      ELSE IF ( OPTION .EQ. 3 ) THEN

*      Read levels directly
        CALL USI_GET1R( 'LEVS', ASM__MXLEV, LEVEL, NLEVEL, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

      ELSE
        CALL MSG_SETI( 'OPT', OPTION )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid selection option /^OPT/', STATUS )
        GOTO 99
      END IF

*    Add the data maximum as the last level, and the minimum as the lower
*    bound on the first
      NLEVEL = NLEVEL + 1
      LEVEL(NLEVEL) = DMAX
      DO I = NLEVEL, 1, -1
        LEVEL(I+1) = LEVEL(I)
      END DO
      LEVEL(1) = DMIN
      NLEVEL = NLEVEL + 1

*    Filter choice
      CALL USI_GET0I( 'FILTER', FILTER, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get smooth signal to noise
      CALL USI_GET0R( 'SNR', SNR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get smooth box max width
      CALL USI_GET0I( 'WSTART', WSTART, STATUS )
      CALL USI_GET0I( 'WMAXSZ', WMAXSZ, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Pad dimensions to 7D
      DO I = NDIM+1, ADI__MXDIM
        DIMS(I) = 1
      END DO

*    Get list of axis numbers into string
      CALL STR_DIMTOC( NSDIM, SDIMS, STR )

*    Construct transformation matrix
      CALL AXIS_TGETORD( IFID, STR(2:CHR_LEN(STR)-1), REORDER, TRORD,
     :                                        NSDIM, TRDIMS, STATUS )

*    Transformation of axes required? If so create temporary space
*    and unmap originals to save space
      IF ( REORDER ) THEN
        CALL DYN_MAPR( NDIM, DIMS, TDPTR, STATUS )
        CALL AR7_AXSWAP_R( DIMS, %VAL(IDPTR), TRORD, TRDIMS,
     :                                 %VAL(TDPTR), STATUS )
        CALL BDI_UNMAPDATA( IFID, STATUS )
        IDPTR = TDPTR
        IF ( QOK ) THEN
          CALL DYN_MAPB( NDIM, DIMS, TQPTR, STATUS )
          CALL AR7_AXSWAP_B( DIMS, %VAL(IQPTR), TRORD, TRDIMS,
     :                                   %VAL(TQPTR), STATUS )
          CALL BDI_UNMAPQUAL( IFID, STATUS )
          IQPTR = TQPTR
        END IF
        IF ( VOK ) THEN
          CALL DYN_MAPR( NDIM, DIMS, TVPTR, STATUS )
          CALL AR7_AXSWAP_R( DIMS, %VAL(IVPTR), TRORD, TRDIMS,
     :                                   %VAL(TVPTR), STATUS )
          CALL BDI_UNMAPVAR( IFID, STATUS )
          IVPTR = TVPTR
        END IF
      END IF

*    Number of elements in each slice
      CALL ARR_SUMDIM( NSDIM, TRDIMS, NSELEM )

*    Find the number of slices
      NSLICE = NELM / NSELEM

*    Allocate workspace
      CALL DYN_MAPR( 1, NSELEM*NSLICE, WDPTR, STATUS )
      CALL DYN_MAPB( 1, NSELEM*NSLICE, WQPTR, STATUS )

*    Perform the smooth
      CALL ASMOOTH_INT( NDIM, DIMS, NSDIM, NSLICE, NSELEM,
     :                  NLEVEL, LEVEL, %VAL(IDPTR), QOK,
     :                  %VAL(IQPTR), VOK, %VAL(IVPTR),
     :                  %VAL(WDPTR), %VAL(WQPTR),
     :                  SNR, FILTER, WSTART, WMAXSZ, %VAL(ODPTR),
     :                  STATUS )

*    Need to transform back the output data? If so, have to make a copy
*    first.
      IF ( REORDER ) THEN

*      Invert the axis transformation
        CALL AXIS_ORDINV( NDIM, TRORD, INORDER )

*      Swap the data
        CALL ARR_COP1R( NELM, %VAL(ODPTR), %VAL(TDPTR), STATUS )
        CALL AR7_AXSWAP_R( TRDIMS, %VAL(TDPTR), INORDER, DIMS,
     :                                   %VAL(ODPTR), STATUS )
        CALL DYN_UNMAP( TDPTR, STATUS )
        CALL BDI_UNMAPDATA( OFID, STATUS )

      END IF

*    Update history
      CALL HSI_ADD( OFID, VERSION, STATUS )

*    Create history text
      HTXT(1) = 'Input {INP}'
      CALL MSG_SETI( 'FILT', FILTER )
      CALL MSG_MAKE( 'Filter choice ^FILT', HTXT(2), TLEN )
      CALL MSG_SETR( 'SNR', SNR )
      CALL MSG_MAKE( 'Target SNR in output ^SNR', HTXT(3), TLEN )
      CALL MSG_SETI( 'NLEV', NLEVEL )
      IF ( OPTION .EQ. 1 ) THEN
        CALL MSG_SETR( 'MIN', DMIN )
        CALL MSG_SETR( 'MAX', DMAX )
        CALL MSG_MAKE( '^NLEV levels linearly spaced between ^MIN '/
     :                                  /'and ^MAX', HTXT(4), TLEN )
      ELSE IF ( OPTION .EQ. 2 ) THEN
        CALL MSG_SETR( 'MIN', DMIN )
        CALL MSG_SETR( 'MAX', DMAX )
        CALL MSG_SETR( 'BIAS', BIAS )
        CALL MSG_MAKE( '^NLEV levels log spaced between '/
     :           /'^MIN and ^MAX, with BIAS = ^BIAS', HTXT(4), TLEN )
      ELSE
        CALL STR_DIMTOC( NLEVEL, LEVEL, HTXT(4) )
        CALL MSG_SETC( 'LEVS', HTXT(4) )
        CALL MSG_MAKE( '^NLEV levels ^LEVS', HTXT(4), TLEN )
      END IF

*    Process text
      NLINES = MXLINES
      CALL USI_TEXT( 4, HTXT, NLINES, STATUS )
      CALL HSI_PTXT( OFID, NLINES, HTXT, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  ASMOOTH_INT - Adaptive smooth of data in first 2 dimensions of a 7D object
      SUBROUTINE ASMOOTH_INT( NDIM, DIMS, NSDIM, NSLICE, NSELEM,
     :                        NLEVEL, LEVEL, IN, QOK, INQ, VOK,
     :                        INV, D_LAYER, Q_LAYER, SNR,
     :                        FILTER, WSTART, WMAXSZ, OUT, STATUS )
*
*    Description :
*
*     The input data are presented as a stack of NSLICE objects of
*     dimensionality NSDIM, which are to be smoothed in all NSDIM
*     dimensions.
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Nov 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      INTEGER                NDIM                  ! # real dimensions
      INTEGER                DIMS(ADI__MXDIM)      ! Dataset dimensions
      INTEGER                NSDIM                 ! Number of dims to smooth
      INTEGER                NSLICE                ! # slices
      INTEGER                NSELEM                ! # elements per slice
      INTEGER                NLEVEL                ! # smoothing levels
      REAL                   LEVEL(NLEVEL+2)       ! Smoothing levels
      REAL                   IN(NSELEM,NSLICE)     ! Data to smooth
      LOGICAL                QOK, VOK              ! Quality & variance ok?
      REAL                   INV(NSELEM,NSLICE)    ! Data variance to smooth
      BYTE                   INQ(NSELEM,NSLICE)    ! Data quality
      REAL                   SNR                   ! Requested signal to noise
      INTEGER                FILTER                ! Filter choice
      INTEGER                WSTART                ! Filter width starting val
      INTEGER                WMAXSZ                ! Estimated max filter width
*
*    Workspace :
*
      REAL                   D_LAYER(NSELEM)       ! Layer to be smoothed
      BYTE                   Q_LAYER(NSELEM)       ! Quality of layer
*
*    Export :
*
      REAL                   OUT(NSELEM,NSLICE)    ! Output data
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                   DBAR                  ! Average data value
      REAL                   VBAR                  ! Average data variance
      REAL                   WIDTHS(3)             ! Filter width data

      INTEGER                I                     ! Loop over data
      INTEGER                ILEVEL                ! Loop over smooth levels
      INTEGER                ISLICE                ! Loop over slices
      INTEGER                WDIMS(ADI__MXDIM)     ! Weights dimensions
      INTEGER                WPTR                  ! Weights array
      INTEGER                WTRUNC                ! Truncation radius
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise the output
      CALL ARR_INIT1R( 0.0, NSELEM*NSLICE, OUT, STATUS )

*    Construct weights array dimensions
      DO I = 1, ADI__MXDIM
        IF ( I .LE. NSDIM ) THEN
          WDIMS(I) = WMAXSZ*2+1
        ELSE
          WDIMS(I) = 1
        END IF
      END DO

*    Allocate memory for weights
      CALL DYN_MAPR( NSDIM, WDIMS, WPTR, STATUS )

*    Loop over slices
      DO ISLICE = 1, NSLICE

*      Loop over smooth levels
        DO ILEVEL = 1, NLEVEL

*        Load data into smooth layer
          CALL ASMOOTH_LOAD( LEVEL(ILEVEL), LEVEL(ILEVEL+1), NSELEM,
     :                       IN(1,ISLICE), QOK, INQ(1,ISLICE),
     :                       VOK, INV(1,ISLICE),
     :                       D_LAYER, Q_LAYER, DBAR, VBAR, STATUS )

*        Bad status means all bad quality points in this slice and no
*        contribution to the sum.
          IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__OK

*        If layer is negative, simply add in the mean of the values in
*        the layer into the good pixels
          ELSE IF ( LEVEL(ILEVEL) .LT. 0.0 ) THEN

*          User interest
            CALL MSG_SETI( 'LAYER', ILEVEL )
            CALL MSG_PRNT( 'Smoothing layer ^LAYER, negative so '/
     :                     /'redistributing flux' )

*          Perform the add
            CALL ASMOOTH_ADD( NSELEM, D_LAYER, Q_LAYER,
     :                        OUT(1,ISLICE), STATUS )

          ELSE

*          Determine width of mask for this level
            CALL ASMOOTH_WIDTH( NSDIM, SNR, FILTER, WSTART,
     :                        WDIMS(1), WDIMS(2), WDIMS(3), WDIMS(4),
     :                        WDIMS(5), WDIMS(6), WDIMS(7),
     :                        DBAR, VBAR, %VAL(WPTR), WIDTHS, WTRUNC,
     :                        STATUS )

*          User interest
            CALL MSG_SETI( 'LAYER', ILEVEL )
            CALL MSG_SETR( 'WID', WIDTHS(1) )
            IF ( FILTER .EQ. 1 ) THEN
              CALL MSG_SETC( 'UNIT', 'FWHM' )
            ELSE IF ( FILTER .EQ. 2 ) THEN
              CALL MSG_SETC( 'UNIT', 'full-width' )
            ELSE IF ( FILTER .EQ. 3 ) THEN
              CALL MSG_SETC( 'UNIT', 'full-width' )
            END IF
            CALL MSG_PRNT( 'Smoothing layer ^LAYER, mask ^UNIT ^WID'/
     :                     /' pixels' )

*          Perform the smooth, adding smoothed slice into OUT
            CALL ASMOOTH_SMOOTH( NSDIM, DIMS(1), DIMS(2), DIMS(3),
     :                 DIMS(4), DIMS(5), DIMS(6), DIMS(7), D_LAYER,
     :                 QOK, Q_LAYER, INQ(1,ISLICE), WDIMS(1),
     :                 WDIMS(2), WDIMS(3),
     :                 WDIMS(4), WDIMS(5), WDIMS(6), WDIMS(7),
     :                 %VAL(WPTR), WTRUNC, OUT, STATUS )

          END IF
        END DO

      END DO

      END



*+  ASMOOTH_LOAD - Load data from a slice into work arrays applying cuts
      SUBROUTINE ASMOOTH_LOAD( LEVEL, NLEVEL, NELM, IN, QOK, INQ,
     :                         VOK, INV, D_LAYER, Q_LAYER,
     :                         DBAR, VBAR, STATUS )
*
*    Description :
*
*     The data in IN/Q/V are moved into their respective <x>_LAYER arrays
*     after applying a cutoff LEVEL to the data array.
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Nov 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      REAL                   LEVEL, NLEVEL         ! This & next level
      INTEGER                NELM                  ! # data values
      REAL                   IN(NELM)              ! Data slice
      LOGICAL                QOK, VOK              ! Quality & variance ok?
      BYTE                   INQ(NELM)             ! Data slice quality
      REAL                   INV(NELM)             ! Data slice variance
*
*    Export :
*
      REAL                   D_LAYER(NELM)         ! Layer to be smoothed
      BYTE                   Q_LAYER(NELM)         ! Quality of layer
      REAL                   DBAR                  ! Average data value
      REAL                   VBAR                  ! Average data variance
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER                I                     ! Loop over data
      INTEGER                NOK                   ! # points above the cut
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise to good quality
      CALL ARR_INIT1B( QUAL__GOOD, NELM, Q_LAYER, STATUS )

*    Initialise
      DBAR = 0.0
      VBAR = 0.0
      NOK = 0

*    Quality present
      IF ( QOK ) THEN

*      Variance present?
        IF ( VOK ) THEN
          DO I = 1, NELM
            IF ( INQ(I) .EQ. QUAL__GOOD ) THEN
              IF ( IN(I) .LE. LEVEL ) THEN
                D_LAYER(I) = 0.0
                Q_LAYER(I) = QUAL__BAD
              ELSE IF ( IN(I) .GT. NLEVEL ) THEN
                D_LAYER(I) = 0.0
                Q_LAYER(I) = QUAL__BAD
              ELSE
                D_LAYER(I) = IN(I)
                DBAR = DBAR + IN(I)
                VBAR = VBAR + INV(I)
                NOK = NOK + 1
              END IF
            ELSE
              D_LAYER(I) = 0.0
              Q_LAYER(I) = QUAL__MISSING
            END IF
          END DO
        ELSE
          DO I = 1, NELM
            IF ( INQ(I) .EQ. QUAL__GOOD ) THEN
              IF ( IN(I) .LE. LEVEL ) THEN
                D_LAYER(I) = 0.0
                Q_LAYER(I) = QUAL__BAD
              ELSE IF ( IN(I) .GT. NLEVEL ) THEN
                D_LAYER(I) = 0.0
                Q_LAYER(I) = QUAL__BAD
              ELSE
                D_LAYER(I) = IN(I)
                DBAR = DBAR + IN(I)
                NOK = NOK + 1
              END IF
            ELSE
              D_LAYER(I) = 0.0
              Q_LAYER(I) = QUAL__MISSING
            END IF
          END DO
          VBAR = DBAR
        END IF

*    No quality
      ELSE

*      Variance present?
        IF ( VOK ) THEN
          DO I = 1, NELM
            IF ( IN(I) .LE. LEVEL ) THEN
              D_LAYER(I) = 0.0
              Q_LAYER(I) = QUAL__BAD
            ELSE IF ( IN(I) .GT. NLEVEL ) THEN
              D_LAYER(I) = 0.0
              Q_LAYER(I) = QUAL__BAD
            ELSE
              D_LAYER(I) = IN(I)
              DBAR = DBAR + IN(I)
              VBAR = VBAR + INV(I)
              NOK = NOK + 1
            END IF
          END DO
        ELSE
          DO I = 1, NELM
            IF ( IN(I) .LE. LEVEL ) THEN
              D_LAYER(I) = 0.0
              Q_LAYER(I) = QUAL__BAD
            ELSE IF ( IN(I) .GT. NLEVEL ) THEN
              D_LAYER(I) = 0.0
              Q_LAYER(I) = QUAL__BAD
            ELSE
              D_LAYER(I) = IN(I)
              DBAR = DBAR + IN(I)
              NOK = NOK + 1
            END IF
          END DO
          VBAR = DBAR
        END IF
      END IF

      IF ( NOK .GT. 0 ) THEN
        DBAR = DBAR / NOK
        VBAR = VBAR / NOK
      ELSE
        STATUS = SAI__ERROR
      END IF

      END



*+  ASMOOTH_ADD - Add averaged data layer into output
      SUBROUTINE ASMOOTH_ADD( NELM, D_LAYER, Q_LAYER, OUT, STATUS )
*
*    Description :
*
*     The data in IN/Q/V are moved into their respective <x>_LAYER arrays
*     after applying a cutoff LEVEL to the data array.
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Nov 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      INTEGER                NELM                  ! # data values
      REAL                   D_LAYER(NELM)         ! Layer to be add in
      BYTE                   Q_LAYER(NELM)         ! Quality of layer
*
*    Export :
*
      REAL                   OUT(NELM)    	   ! Output data
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL		     MEAN, SUM

      INTEGER                I                     ! Loop over data
      INTEGER                NOK                   ! # points above the cut
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find average good value
      SUM = 0.0
      DO I = 1, NELM
        IF ( Q_LAYER(I) .EQ. QUAL__GOOD ) THEN
          SUM = SUM + D_LAYER(I)
        END IF
      END DO

*    Count good pixels input, ie. those for which Q_LAYER isn't QUAL_MISSING
      NOK = 0
      DO I = 1, NELM
        IF ( Q_LAYER(I) .NE. QUAL__MISSING ) THEN
          NOK = NOK + 1
        END IF
      END DO
      MEAN = SUM / REAL(NOK)

*    Add MEAN to all good output pixels
      DO I = 1, NELM
        IF ( Q_LAYER(I) .NE. QUAL__MISSING ) OUT(I) = OUT(I) + MEAN
      END DO

      END



*+  ASMOOTH_WIDTH - Find optimum width setting to achieve SNR
      SUBROUTINE ASMOOTH_WIDTH( NSDIM, SNR, FILTER, WSTART,
     :                          L1, L2, L3, L4, L5, L6, L7,
     :                          DBAR, VBAR, WGT, WIDTH, WTRUNC, STATUS )
*
*    Description :
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Nov 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Import :
*
      INTEGER                NSDIM                 ! Mask dimensionality
      INTEGER                L1,L2,L3,L4,L5,L6,L7  ! Filter dimensions
      REAL                   SNR                   ! Target signal to noise
      INTEGER                FILTER                ! Filter mask type
      INTEGER                WSTART                ! Filter mask start size
      REAL                   DBAR                  ! Average data value
      REAL                   VBAR                  ! Average data variance
*
*    Export :
*
      REAL                   WGT(-L1/2:L1/2, -L2/2:L2/2,   ! Smoothing mask
     :                           -L3/2:L3/2, -L4/2:L4/2,
     :                           -L5/2:L5/2, -L6/2:L6/2,
     :                           -L7/2:L7/2 )
      REAL                   WIDTH(*)              ! Width data
      INTEGER                WTRUNC                ! Truncation radius
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      REAL                   WSCALE                ! Allowable range for scale
        PARAMETER            ( WSCALE = 30.0 )
*
*    Local variables :
*
      REAL                   CV
      REAL                   TWSUM                 ! Target sum of weights^2
      REAL                   WSUM2                 ! Actual sum of weights^2
      REAL                   LB, UB                !

      INTEGER                I                     ! Loop over data
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Total target sum of weights^2
      TWSUM = ((DBAR/SNR)**2)/VBAR

*    Initialise widths
      DO I = 1, 3
        WIDTH(I) = REAL(WSTART)
      END DO
      LB = 1.0 / WSCALE
      UB = WSCALE

*    Create a weights array with user supplied width
 10   CV = 10.0**((LOG10(UB)+LOG10(LB))/2.0)
      DO I = 1, 3
        WIDTH(I) = REAL(WSTART)*CV
      END DO
      CALL ASMOOTH_GENWGT( FILTER, NSDIM, L1, L2, L3, L4, L5,
     :                    L6, L7, WIDTH, WGT, WSUM2, WTRUNC, STATUS )

*    Adjust sum of weights
      IF ( ABS((WSUM2-TWSUM)/TWSUM) .GT. 0.005 ) THEN
        IF ( WSUM2 .GT. TWSUM ) THEN
          LB = CV
        ELSE
          UB = CV
        END IF
        IF ( ABS((LB-UB)/LB) .GT. 0.01 ) GOTO 10
      END IF

*    Abort point
 99   CONTINUE

      END



*+  ASMOOTH_GENWGT - Generate weights given mask type and width in pixels
      SUBROUTINE ASMOOTH_GENWGT( FILTER, NDIM, L1, L2, L3, L4, L5,
     :                 L6, L7, WIDTH, WGT, WSUM2, WTRUNC, STATUS )
*
*    Description :
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Nov 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      INTEGER                FILTER                ! Filter mask type
      INTEGER                NDIM                  ! Mask dimensionality
      INTEGER                L1,L2,L3,L4,L5,L6,L7  ! Filter dimensions
      REAL                   WIDTH(*)              ! Width data
*
*    Export :
*
      REAL                   WGT(-L1/2:L1/2, -L2/2:L2/2,
     :                           -L3/2:L3/2, -L4/2:L4/2,
     :                           -L5/2:L5/2, -L6/2:L6/2,
     :                           -L7/2:L7/2 )
      REAL                   WSUM2                 ! Sum of weights^2
      INTEGER                WTRUNC                ! Truncation radius
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      INTEGER                ASM__GAUSS, ASM__BOX,
     :                       ASM__COSINE
        PARAMETER            ( ASM__GAUSS = 1,
     :                         ASM__BOX = 2,
     :                         ASM__COSINE = 3 )
*
*    Local variables :
*
      REAL                   D1,D2,D3,D4,D5,D6,D7  ! Distance from centre in pix
      REAL                   NORM                  ! Normalisation per pixel
      REAL                   S1,S2,S3,S4,S5,S6,S7  ! Distance from centre in pix
                                                   ! squared

      REAL                   BOXWID                ! Box width
      REAL                   FNV                   ! Function value
      REAL                   SIGMA, SIGMA2         ! Gaussian sigma and ^2
      REAL                   WSUM                  ! Renormalisation

      INTEGER                I,J,K,L,M,N,O         ! Loop over data
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialse weights array
      CALL ARR_INIT1R( 0.0, L1*L2*L3*L4*L5*L6*L7, WGT, STATUS )

*    Pre-calculate stuff for filters
      IF ( FILTER .EQ. ASM__BOX ) THEN

*      Convert full width to radius
        BOXWID = WIDTH(1)/2.0
        WTRUNC = NINT(BOXWID + 1.0)
        IF ( NDIM .EQ. 1 ) THEN
          NORM = 1.0 / (BOXWID*2.0)
        ELSE IF ( NDIM .EQ. 2 ) THEN
          NORM = 1.0 / (MATH__PI*BOXWID**2)
        END IF

      ELSE IF ( FILTER .EQ. ASM__GAUSS ) THEN
        SIGMA = WIDTH(1) / (2.0*SQRT(2.0*LOG(2.0)))
        SIGMA2 = SIGMA**2
        IF ( NDIM .EQ. 1 ) THEN
          NORM = 1.0/SQRT(2.0*MATH__PI*SIGMA)
          WTRUNC = NINT(2.5 * SIGMA+1.0)
        ELSE
          NORM = 1.0/(2.0*MATH__PI*SIGMA2)
          WTRUNC = NINT(3.0 * SIGMA+1.0)
        END IF

      ELSE IF ( FILTER .EQ. ASM__COSINE ) THEN

        NORM = 1.0
        WTRUNC = WIDTH(1)*1.1

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unknown filter code, programmer error!',
     :                                                      STATUS )
        GOTO 99
      END IF

*    Initialise
      WTRUNC = MIN(L1,WTRUNC)

*    Construct weights from width data
      DO O = -L7/2, L7/2
        D7 = REAL(O)
        S7 = D7*D7
        DO N = -L6/2, L6/2
          D6 = REAL(N)
          S6 = D6*D6
          DO M = -L5/2, L5/2
            D5 = REAL(M)
            S5 = D5*D5
            DO L = -L4/2, L4/2
              D4 = REAL(L)
              S4 = D4*D4
              DO K = -L3/2, L3/2
                D3 = REAL(K)
                S3 = D3*D3
                  DO J = -L2/2, L2/2
                  D2 = REAL(J)
                  S2 = D2*D2

*                Switch on dimensionality
                  IF ( NDIM .EQ. 1 ) THEN
                    DO I = -L1/2, L1/2
                      D1 = REAL(I)
                      IF ( FILTER .EQ. ASM__BOX ) THEN
                        IF ( D1 .LE. BOXWID ) THEN
                          FNV = 1.0
                        ELSE
                          FNV = 0.0
                        END IF
                      ELSE IF ( FILTER .EQ. ASM__GAUSS ) THEN
                        S1 = D1*D1
                        FNV = DEXP(DBLE(-S1/SIGMA2/2.0))
                      ELSE IF ( FILTER .EQ. ASM__COSINE ) THEN
                        FNV = COS(D1 * MATH__PI * 2.0 / WIDTH(1))+1.0
                      END IF
                      WGT(I,J,K,L,M,N,O) = FNV*NORM
                    END DO
                  ELSE IF ( NDIM .EQ. 2 ) THEN
                    DO I = -L1/2, L1/2
                      S1 = REAL(I)**2
                      IF ( FILTER .EQ. ASM__BOX ) THEN
                        IF ( SQRT(S1+S2) .LE. BOXWID ) THEN
                          FNV = 1.0
                        ELSE
                          FNV = 0.0
                        END IF
                      ELSE IF ( FILTER .EQ. ASM__GAUSS ) THEN
                        FNV = DEXP(DBLE(-(S1+S2)/SIGMA2/2.0))
                      ELSE IF ( FILTER .EQ. ASM__COSINE ) THEN
                        FNV = COS(SQRT(S1) * MATH__PI * 2.0
     :                                           / WIDTH(1))+1.0
                      END IF
                      WGT(I,J,K,L,M,N,O) = FNV*NORM
                    END DO
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

*    Normalise weights array and find sum of squares
      CALL ARR_SUM1R( L1*L2*L3*L4*L5*L6*L7, WGT, WSUM, STATUS )
      WSUM2 = 0.0
      DO O = -L7/2, L7/2
        DO N = -L6/2, L6/2
          DO M = -L5/2, L5/2
            DO L = -L4/2, L4/2
              DO K = -L3/2, L3/2
                DO J = -L2/2, L2/2
                  DO I = -L1/2, L1/2
                    WGT(I,J,K,L,M,N,O) = WGT(I,J,K,L,M,N,O) / WSUM
                    WSUM2 = WSUM2 + WGT(I,J,K,L,M,N,O)**2
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

*    Abort point
 99   CONTINUE

      END



*+  ASMOOTH_SMOOTH - Smooth an n-dimensional array with a mask
      SUBROUTINE ASMOOTH_SMOOTH( NSDIM, L1, L2, L3, L4, L5,
     :                 L6, L7, D_LAYER, QOK, Q_LAYER, INQ,
     :                 W1, W2, W3, W4, W5, W6, W7, WGT, MW,
     :                 OUT, STATUS )
*
*    Description :
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*
*    Bugs :
*
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Nov 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      INTEGER                NSDIM                 ! Mask dimensionality
      INTEGER                L1,L2,L3,L4,L5,L6,L7  ! Filter dimensions
      REAL                   D_LAYER(L1,L2,L3,L4,  ! Input data
     :                             L5,L6,L7)
      LOGICAL                QOK                   ! Quality present
      BYTE                   Q_LAYER(L1,L2,L3,L4,  ! Input data quality
     :                              L5,L6,L7)
      INTEGER                W1,W2,W3,W4,W5,W6,W7  ! Filter dimensions
      REAL                   WGT(-W1/2:W1/2, -W2/2:W2/2,
     :                           -W3/2:W3/2, -W4/2:W4/2,
     :                           -W5/2:W5/2, -W6/2:W6/2,
     :                           -W7/2:W7/2 )
      INTEGER                MW                    ! Truncation radius
      BYTE                   INQ(L1,L2,L3,L4,      ! Output data quality
     :                              L5,L6,L7)
*
*    Import / Export :
*
      REAL                   OUT(L1,L2,L3,L4,      ! Output data
     :                              L5,L6,L7)
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER                I,J         		! Loop over data
      INTEGER                II,JJ			! Loop over mask
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Switch on dimensionality
      IF ( NSDIM .EQ. 1 ) THEN

        IF ( QOK ) THEN
          DO I = 1, L1
            IF ( Q_LAYER(I,1,1,1,1,1,1) .EQ. QUAL__GOOD ) THEN
              DO II = MAX(1,I-MW), MIN(L1,I+MW)
                IF ( INQ(II,1,1,1,1,1,1) .EQ. QUAL__GOOD ) THEN
                  OUT(II,1,1,1,1,1,1) = OUT(II,1,1,1,1,1,1) +
     :                D_LAYER(I,1,1,1,1,1,1)*WGT(II-I,0,0,0,0,0,0)
                END IF
              END DO
            END IF
          END DO
        ELSE
          DO I = 1, L1
            DO II = MAX(1,I-MW), MIN(L1,I+MW)
              OUT(II,1,1,1,1,1,1) = OUT(II,1,1,1,1,1,1) +
     :                D_LAYER(I,1,1,1,1,1,1)*WGT(II-I,0,0,0,0,0,0)
            END DO
          END DO
        END IF

      ELSE IF ( NSDIM .EQ. 2 ) THEN

        IF ( QOK ) THEN
          DO J = 1, L2
            DO I = 1, L1
              IF ( Q_LAYER(I,J,1,1,1,1,1) .EQ. QUAL__GOOD ) THEN
                DO JJ = MAX(1,J-MW), MIN(L2,J+MW)
                  DO II = MAX(1,I-MW), MIN(L1,I+MW)
                    IF ( INQ(II,JJ,1,1,1,1,1) .EQ. QUAL__GOOD ) THEN
                      OUT(II,JJ,1,1,1,1,1) = OUT(II,JJ,1,1,1,1,1) +
     :                  D_LAYER(I,J,1,1,1,1,1)*WGT(II-I,JJ-J,0,0,0,0,0)
                    END IF
                  END DO
                END DO
              END IF
            END DO
          END DO
        ELSE
          DO J = 1, L2
            DO I = 1, L1
              DO JJ = MAX(1,J-MW), MIN(L2,J+MW)
                DO II = MAX(1,I-MW), MIN(L1,I+MW)
                  OUT(II,JJ,1,1,1,1,1) = OUT(II,JJ,1,1,1,1,1) +
     :                D_LAYER(I,J,1,1,1,1,1)*WGT(II-I,JJ-J,0,0,0,0,0)
                END DO
              END DO
            END DO
          END DO
        END IF

      END IF

      END
