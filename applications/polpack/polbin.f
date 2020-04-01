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
*     terms of the X and Y columns in the catalogue. Spectropolarimetry
*     data can also be binned in the frequency axis (see parameter ZBOX).
*     The grid contains sufficient cells to include all the vector
*     positions included in the input catalogue. Each position in the output
*     catalogue corresponds to one of these cells. The Stokes parameters for
*     the cell are formed by combining together the Stokes parameters of all
*     input positions which fall within the cell, using the method specified
*     by the parameter METHOD. The degree of polarization, angle of
*     polarization, and polarized intensity are then derived from these
*     combined Stokes parameters. The vector position in the output catalogue
*     is the position at the centre of the corresponding cell.

*  Usage:
*     polbin in out box [method]

*  ADAM Parameters:
*     BOX( 2 ) = _REAL (Read)
*        The x and y bin sizes. These values refer to the coordinate Frame
*        defined by columns "X" and "Y" and will usually be in units of pixels.
*        This parameter is not accessed if parameter INTEGRATE is TRUE.
*        Parameter ZBOX specifies binning along the frequency axis when
*        dealing with spectropolarimeter data.
*     DEBIAS = _LOGICAL (Read)
*        TRUE if a correction for statistical bias is to be made to
*        percentage polarization and polarized intensity. The returned
*        variance values are unchanged. This correction only applies to
*        calculations of linear polarization, and cannot be used if the
*        input cube does not contain variance values. If a null value
*        (!) is supplied, then the correction is applied if output variances
*        are being created, and not otherwise. The type of de-biasing to
*        use is specified by parameter DEBIASTYPE. [!]
*     DEBIASTYPE = LITERAL (Read)
*        Only used if DEBIAS is TRUE. It gives the type of bias estimator
*        to use, using the nomeclature of Montier at al "Polarization
*        measurements analysis II. Best estimators of polarization
*        fraction and angle" (A&A, 2018):
*          - "AS": The asymptotic estimator. See section 2.3 of Montier
*             et al. This estimator produces bad P and PI values if the
*             squared PI value is less than the variance in PI.
*          - "MAS": The modified asymptotic estimator. See section 2.5 of
*             Montier et al. This estimator does not produces bad P and PI
*             values, even if the squared PI value is less than the
*             variance in PI.
*        This parameter was introduced at version 3.6.2 of POLPACK.
*        Earlier versions always used the "AS" estimator if de-biasing
*        was requested. The dynamic default is the current value, which
*        is initially "AS". []
*     IN = LITERAL (Read)
*        The name of the input catalogue. A file type of .FIT is assumed
*        if none is provided.
*     INTEGRATE = LOGICAL_ (Read)
*        If TRUE, then all the input vectors are placed in a single bin.
*        In this case, parameter BOX is not used and the output catalogue
*        will contain only a single vector. [FALSE]
*     METHOD = LITERAL (Read)
*        The method to be used when binning Stokes parameters. This may be
*        set to any unique abbreviation of the following:
*
*           -  MEAN      -- Mean of the input data values
*           -  MEDIAN    -- Median of the input data values
*           -  SIGMA     -- A sigma clipped mean
*
*        In all cases, if variances are available for the input Stokes
*        parameters, then the reciprocals of these variances will be used
*        to weight the input Stokes parameters when forming the output
*        Stokes parameters using the selected method. [MEDIAN]
*     MINVAL = _INTEGER (Read)
*        The minimum number of good input values which must be present in
*        a cell to create a good output value. [1]
*     OUT = LITERAL (Read)
*        The name of the output catalogue. A file type of .FIT is assumed
*        if none is provided.
*     RADEC = _LOGICAL (Read)
*        If TRUE, columns holding the RA and DEC (FK5, J2000) are added
*        to the output catalogue, if the input catalogue contains the
*        necessary WCS information. If FALSE, no RA and DEC columns are
*        written. For large catalogues, creating RA and DEC columns can
*        cause a significant delay. [current value]
*     SIGMAS = _REAL (Read)
*        Number of standard deviations to reject data at. Only used if
*        METHOD is set to "SIGMA". [4.0]
*     ZBOX = _REAL (Read)
*        The bin size along the third (Z) axis in the input catalogue.
*        a Z column. The supplied value should usually be in units of pixels.
*        This parameter is not accessed if parameter INTEGRATE is TRUE, or
*        if the input catalogue does not contain a Z column.

*  Notes:
*     -  The reference direction for the Stokes vectors and polarization
*     vectors in the output catalogue will be north if the input catalogue
*     has a celestial co-ordinate Frame within its WCS information. Otherwise,
*     the reference direction will be the second pixel axis. The POLANAL
*     Frame in the WCS information of the output catalogue is updated to
*     describe the new reference direction.
*     -  The bottom left corner of each bin is chosen so that the origin
*     of the (X,Y) Frame (or (X,Y,Z) Frame if the data is 3D) would
*     correspond to a bin corner.

*  Examples:
*     polbin intab outtab 4
*        Bins the Stokes parameters in catalogue "intab.FIT" and produces
*        catalogue "outtab.FIT" containing binned Stokes parameters and
*        corresponding polarization parameters. Each bin measures 4 pixels
*        along both the X and Y axes, and has a value based on the median
*        of the corresponding input Stokes values.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26-MAR-1998 (DSB):
*        Original version.
*     6-AUG-1998 (DSB):
*        Use KPG1_GTCTW instead of KPG1_GTCTA (which has changed its
*        behaviour).
*     10-NOV-1998 (DSB):
*        Rename KPG1_GTCTW as POL1_GTCTW.
*     6-APR-1999 (DSB):
*        Changed reference direction scheme.
*     14-MAR-2000 (DSB):
*        Change the choice of bottom left corner of each bin so that the
*        origin of the (X,Y) Frame would correspond to a bin corner.
*     17-DEC-2000 (DSB):
*        Added parameter INTEGRATE.
*     5-FEB-2001 (DSB):
*        Added support for 3D data.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     7-MAR-2005 (DSB):
*        Correct use of CNF_PVAL
*     13-JUL-2009 (DSB):
*        Changed IPVAR array from DOUBLE PRECISION to REAL.
*     1-APR-2020 (DSB):
*        Added parameter DEBIASTYPE.
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
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER KPG1_CEIL          ! Returns smallest integer >= X
      INTEGER KPG1_FLOOR         ! Returns largest integer <= X
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MAX_ID
      PARAMETER ( MAX_ID = 11 )

*  Local Variables:
      CHARACTER CVAL*6           ! Character value
      CHARACTER FIELDS( 5 )*50   ! Individual fields of catalogue specification
      CHARACTER METH*6           ! Binning method
      CHARACTER NAME*(CAT__SZCMP)! CAT column name
      CHARACTER ONAME*256        ! Full file spec for output catalogue
      CHARACTER STOKES*3         ! Identifiers for cube planes
      CHARACTER TITLE*80         ! Title from input catalogue
      CHARACTER UNITS*( CAT__SZUNI )! Units of the total intensity column
      INTEGER CIIN               ! CAT identifier for input catalogue
      INTEGER CIOUT              ! CAT identifier for output catalogue
      INTEGER DI_ID              ! Index of the DI column in the GI array
      INTEGER DQ_ID              ! Index of the DQ column in the GI array
      INTEGER DU_ID              ! Index of the DU column in the GI array
      INTEGER DV_ID              ! Index of the DV column in the GI array
      INTEGER EQMAP              ! (X,Y)->(RA,DEC) Mapping
      INTEGER FRM                ! Default Frame
      INTEGER GI( MAX_ID )       ! CAT identifiers for columns to be read
      INTEGER GTTL               ! CAT identifier for TITLE parameter
      INTEGER IBNOFF             ! Offset to binned I values
      INTEGER IDEBIAS            ! De-biasing estimator identifier
      INTEGER IOFF               ! Offset to i/p I values
      INTEGER IP                 ! Pointers to arrays to be filled
      INTEGER IPBIN              ! Pointer to binned Stokes parameters
      INTEGER IPCOV              ! Pointer to workspace
      INTEGER IPIST              ! Pointer to stacked i/p I values
      INTEGER IPNCON             ! Pointer to workspace
      INTEGER IPPNT              ! Pointer to workspace
      INTEGER IPPP               ! Pointer to workspace
      INTEGER IPQST              ! Pointer to stacked i/p Q values
      INTEGER IPUSED             ! Pointer to workspace
      INTEGER IPUST              ! Pointer to stacked i/p U values
      INTEGER IPVAR              ! Pointer to dummy line variances
      INTEGER IPVBIN             ! Pointer to binned variances
      INTEGER IPVIST             ! Pointer to stacked i/p I variances
      INTEGER IPVQST             ! Pointer to stacked i/p Q variances
      INTEGER IPVST              ! Pointer to i/p V values
      INTEGER IPVUST             ! Pointer to stacked i/p U variances
      INTEGER IPVVST             ! Pointer to stacked i/p V variances
      INTEGER IPW1               ! Pointer to workspace
      INTEGER IPW2               ! Pointer to second work array
      INTEGER IPW3               ! Pointer to third work array
      INTEGER IPWRK1             ! Pointer to workspace
      INTEGER IPWRK2             ! Pointer to workspace
      INTEGER IROW               ! Row index
      INTEGER IWCS               ! Pointer to AST FrameSet read from catalogue
      INTEGER IXHI               ! Upper pixel index on X axis
      INTEGER IXLO               ! Lower pixel index on X axis
      INTEGER IYHI               ! Upper pixel index on Y axis
      INTEGER IYLO               ! Lower pixel index on Y axis
      INTEGER IZHI               ! Upper pixel index on Z axis
      INTEGER IZLO               ! Lower pixel index on Z axis
      INTEGER I_ID               ! Index of the I column in the GI array
      INTEGER MAXPOS             ! Position of maximum value
      INTEGER MINPOS             ! Position of minimum value
      INTEGER MINVAL             ! Min. no. of good i/p positions per cell
      INTEGER MXCNT              ! Max. no. of i/p positions in any o/p cell
      INTEGER NBAD               ! No. of bad values
      INTEGER NBIN               ! Total no. of output bins
      INTEGER NCIN               ! No. of vectors in catalogue
      INTEGER NCOL               ! No. of columns to be read
      INTEGER NDIMO              ! No. of dimensions in output NDFs
      INTEGER NEXTID             ! Next Id to use
      INTEGER NMAT               ! Size of workspace
      INTEGER NSTOKE             ! No. of planes in cube
      INTEGER NVAL               ! No. of BOX values supplied
      INTEGER NXBIN              ! No. of output bins along X axis
      INTEGER NYBIN              ! No. of output bins along Y axis
      INTEGER NZBIN              ! No. of output bins along Z axis
      INTEGER QBNOFF             ! Offset to binned Q values
      INTEGER QOFF               ! Offset to i/p Q values
      INTEGER Q_ID               ! Index of the Q column in the GI array
      INTEGER UBNOFF             ! Offset to binned U values
      INTEGER UOFF               ! Offset to i/p U values
      INTEGER U_ID               ! Index of the U column in the GI array
      INTEGER VBNOFF             ! Offset to binned V values
      INTEGER VIBNOFF            ! Offset to binned I variances
      INTEGER VIOFF              ! Offset to i/p I variances
      INTEGER VOFF               ! Offset to i/p V values
      INTEGER VQBNOFF            ! Offset to binned Q variances
      INTEGER VQOFF              ! Offset to i/p Q variances
      INTEGER VUBNOFF            ! Offset to binned U variances
      INTEGER VUOFF              ! Offset to i/p U variances
      INTEGER VVBNOFF            ! Offset to binned V variances
      INTEGER VVOFF              ! Offset to i/p V variances
      INTEGER V_ID               ! Index of the V column in the GI array
      INTEGER XOFF               ! Offset to i/p X values
      INTEGER X_ID               ! Index of the X column in the GI array
      INTEGER YOFF               ! Offset to i/p Y values
      INTEGER Y_ID               ! Index of the Y column in the GI array
      INTEGER ZOFF               ! Offset to i/p Z values
      INTEGER Z_ID               ! Index of the Z column in the GI array
      LOGICAL CIRC               ! Doing circular polarimetry?
      LOGICAL DEBIAS             ! Statistical de-biassing required?
      LOGICAL INTGRT             ! Integrate all vectors?
      LOGICAL NULL1              ! Null value flag
      LOGICAL NULL2              ! Null value flag
      LOGICAL NULL3              ! Null value flag
      LOGICAL RADEC              ! Are RA/DEC columns required?
      LOGICAL SPEC               ! Is there a Z axis?
      LOGICAL VAR                ! Producing variances?
      LOGICAL VERB               ! Verose errors required?
      REAL ANG                   ! Stored angle in input catalogue
      REAL ANGROT                ! ACW angle from X axis to i/p ref dirn (degs)
      REAL ANGRT                 ! ACW angle from X axis to o/p ref dirn (degs)
      REAL BOX( 3 )              ! Bin size
      REAL NSIGMA                ! No. of sigmas to clip at
      REAL Q                     ! Stored Q in input catalogue
      REAL RTOD                  ! Conversion factor; radians to degrees
      REAL SXHI                  ! Upper bound of used region of X axis
      REAL SXLO                  ! Lower bound of used region of X axis
      REAL SYHI                  ! Upper bound of used region of Y axis
      REAL SYLO                  ! Lower bound of used region of Y axis
      REAL SZHI                  ! Upper bound of used region of Z axis
      REAL SZLO                  ! Lower bound of used region of Z axis
      REAL TR( 6 )               ! Coeff.s of (X,Y,Z) -> cell indices mapping
      REAL TR2( 6 )              ! Coeff.s of cell indices -> (X,Y,Z) mapping
      REAL U                     ! Stored U in input catalogue
      REAL X0                    ! X at bottom left of bottom left cell
      REAL XC                    ! X at centre of input catalogue
      REAL Y0                    ! Y at bottom left of bottom left cell
      REAL YC                    ! Y at centre of input catalogue
      REAL Z0                    ! Z at bottom left of bottom left cell
      REAL ZC                    ! Z at centre of input catalogue




*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  See if the user wants verbose error messages.
      CALL KPG1_VERB( VERB, 'POLPACK', STATUS )

*  Open the input catalogue, and get its name.
      CALL CTG_ASSO1( 'IN', VERB, 'READ', CIIN, FIELDS, STATUS )

*  Store the values of the required catalogue columns in work arrays.
*  ==================================================================
*  Find the number of rows in the catalogue. This is the number of input
*  cells.
      CALL CAT_TROWS( CIIN, NCIN, STATUS )

*  Get CAT identifiers for the required columns. First get position columns
*  (X and Y) and total intensity (I).
      X_ID = 1
      CALL POL1_GTCOL( CIIN, 'X', .TRUE., GI( X_ID ), STATUS )
      Y_ID = 2
      CALL POL1_GTCOL( CIIN, 'Y', .TRUE., GI( Y_ID ), STATUS )
      I_ID = 3
      CALL POL1_GTCOL( CIIN, 'I', .TRUE., GI( I_ID ), STATUS )

      NEXTID = 4

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If there is a column named "Z" we are dealing with spectropolarimetry.
*  Attempt to get a CAT identifier for the "Z" column.
      CALL POL1_GTCOL( CIIN, 'Z', .FALSE., GI( NEXTID ), STATUS )
      IF( GI( NEXTID ) .EQ. CAT__NOID ) THEN
         SPEC = .FALSE.
         NDIMO = 2
      ELSE
         SPEC = .TRUE.
         NDIMO = 3
         Z_ID = NEXTID
         NEXTID = NEXTID + 1
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If there is a column named "V" we are dealing with circular polarimetry.
*  Attempt to get a CAT identifier for the "V" column. Otherwise,
*  assume we are dealing with linear polarimetry. In this case get columns
*  for Q and U instead.
      CALL POL1_GTCOL( CIIN, 'V', .FALSE., GI( NEXTID ), STATUS )
      IF( GI( NEXTID ) .EQ. CAT__NOID ) THEN
         CIRC = .FALSE.
         Q_ID = NEXTID
         U_ID = NEXTID + 1
         CALL POL1_GTCOL( CIIN, 'Q', .TRUE., GI( Q_ID ), STATUS )
         CALL POL1_GTCOL( CIIN, 'U', .TRUE., GI( U_ID ), STATUS )
         NEXTID = NEXTID + 2
      ELSE
         CIRC = .TRUE.
         V_ID = NEXTID
         NEXTID = NEXTID + 1
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Try to find corresponding error columns for these columns (except X
*  and Y (and Z) ).
      CALL POL1_GTCOL( CIIN, 'DI', .TRUE., GI( NEXTID ), STATUS )

      IF( CIRC ) THEN
         CALL POL1_GTCOL( CIIN, 'DV', .TRUE., GI( NEXTID + 1 ), STATUS )
      ELSE
         CALL POL1_GTCOL( CIIN, 'DQ', .TRUE., GI( NEXTID + 1 ), STATUS )
         CALL POL1_GTCOL( CIIN, 'DU', .TRUE., GI( NEXTID + 2 ), STATUS )
      END IF

*  If any of these error columns could not be found, do not use any of
*  them. Annul the error and set a flag indicating error calculations are
*  not required. Store the number of columns to be read from the catalogue.
      IF( STATUS .NE. SAI__OK ) THEN
         VAR = .FALSE.
         CALL ERR_BEGIN( STATUS )

         CALL CAT_TRLSE( GI( NEXTID ), STATUS )

         IF( CIRC ) THEN
            CALL CAT_TRLSE( GI( NEXTID + 1 ), STATUS )
            NCOL = 4
         ELSE
            CALL CAT_TRLSE( GI( NEXTID + 1 ), STATUS )
            CALL CAT_TRLSE( GI( NEXTID + 2 ), STATUS )
            NCOL = 5
         END IF

         CALL ERR_END( STATUS )
         CALL ERR_ANNUL( STATUS )

      ELSE
         DI_ID = NEXTID

         VAR = .TRUE.
         IF( CIRC ) THEN
            DV_ID = NEXTID + 1
            NEXTID = NEXTID + 2
            NCOL = 6
         ELSE
            DQ_ID = NEXTID + 1
            DU_ID = NEXTID + 2
            NEXTID = NEXTID + 3
            NCOL = 8
         END IF
      END IF

      IF( SPEC ) NCOL = NCOL + 1

*  Allocate work space to hold the data from the required columns.
      CALL PSX_CALLOC( NCIN*NCOL, '_REAL', IP, STATUS )

*  Store byte offsets to the start of the data from each individual column.
      XOFF = NCIN*VAL__NBR*( X_ID - 1 )
      YOFF = NCIN*VAL__NBR*( Y_ID - 1 )
      IOFF = NCIN*VAL__NBR*( I_ID - 1 )

      IF( SPEC ) THEN
         ZOFF = NCIN*VAL__NBR*( Z_ID - 1 )
      ELSE
         ZOFF = XOFF
      END IF

      IF( CIRC ) THEN
         VOFF = NCIN*VAL__NBR*( V_ID - 1 )
      ELSE
         QOFF = NCIN*VAL__NBR*( Q_ID - 1 )
         UOFF = NCIN*VAL__NBR*( U_ID - 1 )
      END IF

      IF( VAR ) THEN
         VIOFF = NCIN*VAL__NBR*( DI_ID - 1 )
         IF( CIRC ) THEN
            VVOFF = NCIN*VAL__NBR*( DV_ID - 1 )
         ELSE
            VQOFF = NCIN*VAL__NBR*( DQ_ID - 1 )
            VUOFF = NCIN*VAL__NBR*( DU_ID - 1 )
         END IF
      END IF

*  Check the pointers can be used.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Read the required columns from the catalogue into the work arrays
*  allocated above. This is done in a single pass through the catalogue
*  in order to speed it up a bit.
      CALL POL1_CTCLM( CIIN, NCIN, NCOL, GI, %VAL( CNF_PVAL( IP ) ),
     :                 STATUS )

*  Square the standard deviation columns so that they become variance values.
      IF( VAR ) THEN
         CALL POL1_SQUAR( NCIN, %VAL( CNF_PVAL( IP ) + VIOFF ), STATUS )
         IF( CIRC ) THEN
            CALL POL1_SQUAR( NCIN, %VAL( CNF_PVAL( IP ) + VVOFF ),
     :                       STATUS )
         ELSE
            CALL POL1_SQUAR( NCIN, %VAL( CNF_PVAL( IP ) + VQOFF ),
     :                       STATUS )
            CALL POL1_SQUAR( NCIN, %VAL( CNF_PVAL( IP ) + VUOFF ),
     :                       STATUS )
         END IF
      END IF

*  Attempt to read an AST FrameSet from the input catalogue. This WCS
*  information will be copied to the output catalogue when the
*  output catalogue is closed.
      CALL POL1_GTCTW( CIIN, IWCS, STATUS )

*  Get the ACW angle from the X axis to the input reference direction
*  (ANGROT). This is defined by the POLANAL Frame in the WCS FrameSet.
      IF( IWCS .NE. AST__NULL ) THEN
         ANGROT = 0.0
         CALL POL1_GTANG( NDF__NOID, CIIN, IWCS, ANGROT, STATUS )

*  If no WCS is available...
      ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  Warn the user that an ANGROT value of 90 is being assumed.
         ANGROT = 90.0
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'POLBIN_1', '  No usable WCS information is '//
     :                 'available in the input catalogue.', STATUS )
         CALL MSG_OUT( 'POLBIN_2', '  Assuming the reference '//
     :                 'direction is parallel to the second pixel '//
     :                 'axis.', STATUS )
         CALL MSG_BLANK( STATUS )

*  Create a default FrameSet containing a single Frame describing the X and
*  Y columns. The Axis Symbols are assigned the column names.
         FRM = AST_FRAME( NDIMO, ' ', STATUS )

         CALL CAT_TIQAC( GI( X_ID ), 'NAME', NAME, STATUS )
         CALL AST_SETC( FRM, 'Symbol(1)', NAME( : CHR_LEN( NAME ) ),
     :                  STATUS )
         CALL AST_SETC( FRM, 'Label(1)', NAME( : CHR_LEN( NAME ) ),
     :                  STATUS )

         CALL CAT_TIQAC( GI( Y_ID ), 'NAME', NAME, STATUS )
         CALL AST_SETC( FRM, 'Symbol(2)', NAME( : CHR_LEN( NAME ) ),
     :                  STATUS )
         CALL AST_SETC( FRM, 'Label(2)', NAME( : CHR_LEN( NAME ) ),
     :                  STATUS )

         IF( SPEC ) THEN
            CALL CAT_TIQAC( GI( Z_ID ), 'NAME', NAME, STATUS )
            CALL AST_SETC( FRM, 'Symbol(3)', NAME( : CHR_LEN( NAME ) ),
     :                  STATUS )
            CALL AST_SETC( FRM, 'Label(3)', NAME( : CHR_LEN( NAME ) ),
     :                  STATUS )
         END IF

         IWCS =AST_FRAMESET( FRM, ' ', STATUS )

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
         CALL ERR_REP( 'POLBIN_2', 'Vectors will not be '/
     :                 /'corrected for statistical bias because no '/
     :                 /'variance values are available.', STATUS )
         CALL ERR_FLUSH( STATUS )
         DEBIAS = .FALSE.
      END IF

*  Get the coefficients of the linear transformation from (X,Y(,Z))
*  position to bin indices.
*  =================================================================
*  Find the maximum and minimum X value. Ensure that at least one pixel
*  is spanned by the recorded max and min values.
      CALL KPG1_MXMNR( .TRUE., NCIN, %VAL( CNF_PVAL( IP ) + XOFF ),
     :                 NBAD, SXHI,
     :                 SXLO, MAXPOS, MINPOS, STATUS )
      IF( SXHI - SXLO .LT. 1.0  ) THEN
         SXHI = 0.5*( SXHI + SXLO + 1.0 )
         SXLO = SXHI - 1.0
      END IF

*  Find the maximum and minimum Y value.
      CALL KPG1_MXMNR( .TRUE., NCIN, %VAL( CNF_PVAL( IP ) + YOFF ),
     :                 NBAD, SYHI,
     :                 SYLO, MAXPOS, MINPOS, STATUS )
      IF( SYHI - SYLO .LT. 1.0  ) THEN
         SYHI = 0.5*( SYHI + SYLO + 1.0 )
         SYLO = SYHI - 1.0
      END IF

*  If required find min and max Z values.
      IF( SPEC ) THEN
         CALL KPG1_MXMNR( .TRUE., NCIN, %VAL( CNF_PVAL( IP ) + ZOFF ),
     :                    NBAD, SZHI,
     :                    SZLO, MAXPOS, MINPOS, STATUS )
         IF( SZHI - SZLO .LT. 1.0  ) THEN
            SZHI = 0.5*( SZHI + SZLO + 1.0 )
            SZLO = SZHI - 1.0
         END IF
      ELSE
         SZHI = 1.0
         SZLO = 0.0
      END IF

*  Find the coords of the centre.
      XC = 0.5*( SXLO + SXHI )
      YC = 0.5*( SYLO + SYHI )
      ZC = 0.5*( SZLO + SZHI )

*  See if we are integrating all vectors into a single vector, or into a
*  grid of vectors.
      CALL PAR_GET0L( 'INTEGRATE', INTGRT, STATUS )

*  If we are binning into a grid of boxes...
      IF( .NOT. INTGRT ) THEN

*  Obtain the sizes of each bin.
         CALL PAR_GDRVR( 'BOX', 2, 1.0E-6, VAL__MAXR, BOX, NVAL,
     :                   STATUS )

*  Duplicate the value if only a single value was given.
         IF ( NVAL .LT. 2 ) BOX( 2 ) = BOX( 1 )

*  If required, get the spectral bin size.
         IF( SPEC ) THEN
            CALL PAR_GET0R( 'ZBOX', BOX( 3 ), STATUS )
            BOX( 3 ) = MAX( 1.0, BOX( 3 ) )
         ELSE
            BOX( 3 ) = 1.0
         END IF

*  Limit the box dimensions to be no bigger than the span of the data,
*  plus 5%.
         BOX( 1 ) = MIN( BOX( 1 ), REAL( INT( 1.05*( SXHI - SXLO ) ) ) )
         BOX( 2 ) = MIN( BOX( 2 ), REAL( INT( 1.05*( SYHI - SYLO ) ) ) )
         BOX( 3 ) = MIN( BOX( 3 ), REAL( INT( 1.05*( SZHI - SZLO ) ) ) )

*  Find the indices of the first and last bins on each axis. This
*  assumes that the origin on each axis is at a bin edge.
         IXLO = KPG1_FLOOR( SXLO / BOX( 1 ) )
         IXHI = KPG1_CEIL( SXHI / BOX( 1 ) )
         IYLO = KPG1_FLOOR( SYLO / BOX( 2 ) )
         IYHI = KPG1_CEIL( SYHI / BOX( 2 ) )
         IZLO = KPG1_FLOOR( SZLO / BOX( 3 ) )
         IZHI = KPG1_CEIL( SZHI / BOX( 3 ) )

*  Find the number of bins along each axis.
         NXBIN = IXHI - IXLO + 1
         NYBIN = IYHI - IYLO + 1
         NZBIN = IZHI - IZLO + 1

*  Find the total number of bins.
         NBIN = NXBIN*NYBIN*NZBIN

*  Find the X, Y and Z values corresponding to the bottom left corner of the
*  bottom left bin. Again, this assumes that the origin on each axis is at
*  a bin edge.
         X0 = IXLO*BOX( 1 )
         Y0 = IYLO*BOX( 2 )
         Z0 = IZLO*BOX( 3 )

*  Find the coefficients of the transformation. The X cell index for a
*  position (X,Y,Z) is given by INT( TR( 1 ) + TR( 2 )*X ), the Y cell
*  index is given by INT( TR( 3 ) + TR( 4 )*Y ), the Z cell index is given
*  by INT( TR( 5 ) + TR( 6 )*Z ).
         TR( 1 ) = 1.0 - X0/BOX( 1 )
         TR( 2 ) = 1.0/BOX( 1 )
         TR( 3 ) = 1.0 - Y0/BOX( 2 )
         TR( 4 ) = 1.0/BOX( 2 )
         TR( 5 ) = 1.0 - Z0/BOX( 3 )
         TR( 6 ) = 1.0/BOX( 3 )

*  Store the coefficients of the transformation from cell indices to
*  (X,Y,Z) coordinates at the cell centre.
         TR2( 1 ) = X0 - 0.5*BOX( 1 )
         TR2( 2 ) = BOX( 1 )
         TR2( 3 ) = Y0 - 0.5*BOX( 2 )
         TR2( 4 ) = BOX( 2 )
         TR2( 5 ) = Z0 - 0.5*BOX( 3 )
         TR2( 6 ) = BOX( 3 )

*  If we are integrating all vectors in the catalogue, set up values which
*  result in all vectors being placed in a single bin.
      ELSE
         NXBIN = 1
         NYBIN = 1
         NZBIN = 1
         NBIN = 1
         TR( 1 ) = 1.0
         TR( 2 ) = 0.0
         TR( 3 ) = 1.0
         TR( 4 ) = 0.0
         TR( 5 ) = 1.0
         TR( 6 ) = 0.0
         TR2( 1 ) = XC
         TR2( 2 ) = 0.0
         TR2( 3 ) = YC
         TR2( 4 ) = 0.0
         TR2( 5 ) = ZC
         TR2( 6 ) = 0.0
      END IF

*  Now find the largest number of input positions inn any bin.
*  ===========================================================
*  Allocate an integer work array with one element per bin to hold the
*  number of input positions in each bin.
      CALL PSX_CALLOC( NBIN, '_INTEGER', IPW1, STATUS )

*  Check the pointer can be used.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Count the number of input catalogue positions contained in each output
*  cell. The largest number in any one cell is returned.
      CALL POL1_CLCNT( NCIN, SPEC, %VAL( CNF_PVAL( IP ) + XOFF ),
     :                 %VAL( CNF_PVAL( IP ) + YOFF ),
     :                 %VAL( CNF_PVAL( IP ) + ZOFF ), TR, NXBIN, NYBIN,
     :                 NZBIN, %VAL( CNF_PVAL( IPW1 ) ), MXCNT, STATUS )

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
      CALL POL1_STK2( NCIN, SPEC, %VAL( CNF_PVAL( IP ) + IOFF ),
     :                %VAL( CNF_PVAL( IP ) + XOFF ),
     :                %VAL( CNF_PVAL( IP ) + YOFF ),
     :                %VAL( CNF_PVAL( IP ) + ZOFF ),
     :                NXBIN, NYBIN, NZBIN, MXCNT, TR,
     :                %VAL( CNF_PVAL( IPIST ) ),
     :                %VAL( CNF_PVAL( IPW1 ) ), STATUS )

*  Do the same for the other required Stokes vectors (V, or Q and U).
      IF( CIRC ) THEN
         CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPVST, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 999

         CALL POL1_STK2( NCIN, SPEC, %VAL( CNF_PVAL( IP ) + VOFF ),
     :                   %VAL( CNF_PVAL( IP ) + XOFF ),
     :                   %VAL( CNF_PVAL( IP ) + YOFF ),
     :                   %VAL( CNF_PVAL( IP ) + ZOFF ), NXBIN, NYBIN,
     :                   NZBIN, MXCNT, TR, %VAL( CNF_PVAL( IPVST ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   STATUS )

      ELSE
         CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPQST, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 999

         CALL POL1_STK2( NCIN, SPEC, %VAL( CNF_PVAL( IP ) + QOFF ),
     :                   %VAL( CNF_PVAL( IP ) + XOFF ),
     :                   %VAL( CNF_PVAL( IP ) + YOFF ),
     :                   %VAL( CNF_PVAL( IP ) + ZOFF ), NXBIN, NYBIN,
     :                   NZBIN, MXCNT, TR, %VAL( CNF_PVAL( IPQST ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   STATUS )

         CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPUST, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 999

         CALL POL1_STK2( NCIN, SPEC, %VAL( CNF_PVAL( IP ) + UOFF ),
     :                   %VAL( CNF_PVAL( IP ) + XOFF ),
     :                   %VAL( CNF_PVAL( IP ) + YOFF ),
     :                   %VAL( CNF_PVAL( IP ) + ZOFF ), NXBIN, NYBIN,
     :                   NZBIN, MXCNT, TR, %VAL( CNF_PVAL( IPUST ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   STATUS )
      END IF

*  If required, do the same for the variances.
      IF( VAR ) THEN
         CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPVIST, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 999

         CALL POL1_STK2( NCIN, SPEC, %VAL( CNF_PVAL( IP ) + VIOFF ),
     :                   %VAL( CNF_PVAL( IP ) + XOFF ),
     :                   %VAL( CNF_PVAL( IP ) + YOFF ),
     :                   %VAL( CNF_PVAL( IP ) + ZOFF ), NXBIN, NYBIN,
     :                   NZBIN, MXCNT, TR, %VAL( CNF_PVAL( IPVIST ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   STATUS )

         IF( CIRC ) THEN
            CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPVVST, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999

            CALL POL1_STK2( NCIN, SPEC, %VAL( CNF_PVAL( IP ) + VVOFF ),
     :                      %VAL( CNF_PVAL( IP ) + XOFF ),
     :                      %VAL( CNF_PVAL( IP ) + YOFF ),
     :                      %VAL( CNF_PVAL( IP ) + ZOFF ), NXBIN, NYBIN,
     :                      NZBIN, MXCNT, TR,
     :                      %VAL( CNF_PVAL( IPVVST ) ),
     :                      %VAL( CNF_PVAL( IPW1 ) ), STATUS )

         ELSE
            CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPVQST, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999

            CALL POL1_STK2( NCIN, SPEC, %VAL( CNF_PVAL( IP ) + VQOFF ),
     :                      %VAL( CNF_PVAL( IP ) + XOFF ),
     :                      %VAL( CNF_PVAL( IP ) + YOFF ),
     :                      %VAL( CNF_PVAL( IP ) + ZOFF ), NXBIN, NYBIN,
     :                      NZBIN, MXCNT, TR,
     :                      %VAL( CNF_PVAL( IPVQST ) ),
     :                      %VAL( CNF_PVAL( IPW1 ) ), STATUS )

            CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPVUST, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999

            CALL POL1_STK2( NCIN, SPEC, %VAL( CNF_PVAL( IP ) + VUOFF ),
     :                      %VAL( CNF_PVAL( IP ) + XOFF ),
     :                      %VAL( CNF_PVAL( IP ) + YOFF ),
     :                      %VAL( CNF_PVAL( IP ) + ZOFF ), NXBIN, NYBIN,
     :                      NZBIN, MXCNT, TR,
     :                      %VAL( CNF_PVAL( IPVUST ) ),
     :                      %VAL( CNF_PVAL( IPW1 ) ), STATUS )

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

      IF( VAR .AND. METH .NE. 'MEAN' .AND. STATUS .EQ. SAI__OK ) THEN
         CALL PSX_CALLOC( MXCNT, '_DOUBLE', IPPP, STATUS )
         NMAT = MXCNT*( MXCNT + 1 )/2
         CALL PSX_CALLOC( MXCNT*NMAT, '_DOUBLE', IPCOV, STATUS )
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'POLBIN_3', 'Try using METHOD=MEAN or '//
     :                    'reducing the number of vectors', STATUS )
         END IF
      ELSE
         IPPP = IPNCON
         IPCOV = IPNCON
      END IF

*  Allocate an array to hold the binned values. This array is accessed
*  as a 4D hyper-cube by POL1_PLVEC. The hyper-cube consists of 2 or 3
*  cubes (depending on whether or not we are doing circular polarimetry),
*  each spanned by the X, Y and Z axes (having NXBIN, NYBIN and NZBIN
*  elements respectively). The first cube contains total intensity, the
*  second contains V or Q, and the third (if used) contains U. Get pointers
*  to the start of each cube.
      IF( CIRC ) THEN
         CALL PSX_CALLOC( NBIN*2, '_REAL', IPBIN, STATUS )
         IBNOFF = 0
         VBNOFF = NBIN*VAL__NBR
         NSTOKE = 2
         STOKES = 'IV'
      ELSE
         CALL PSX_CALLOC( NBIN*3, '_REAL', IPBIN, STATUS )
         IBNOFF = 0
         QBNOFF = NBIN*VAL__NBR
         UBNOFF = 2*NBIN*VAL__NBR
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
            VIBNOFF = 0
            VVBNOFF = NBIN*VAL__NBR
         ELSE
            CALL PSX_CALLOC( NBIN*3, '_REAL', IPVBIN, STATUS )
            VIBNOFF = 0
            VQBNOFF = NBIN*VAL__NBR
            VUBNOFF = 2*NBIN*VAL__NBR
         END IF

*  Check the pointers can be used.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Bin the total intensity...
         CALL POL1_CM1RR( %VAL( CNF_PVAL( IPIST ) ), NBIN, MXCNT,
     :                    %VAL( CNF_PVAL( IPVIST ) ),
     :                    METH, MINVAL, NSIGMA,
     :                    %VAL( CNF_PVAL( IPBIN ) + IBNOFF ),
     :                    %VAL( CNF_PVAL( IPVBIN ) + VIBNOFF ),
     :                    %VAL( CNF_PVAL( IPWRK1 ) ),
     :                    %VAL( CNF_PVAL( IPWRK2 ) ),
     :                    %VAL( CNF_PVAL( IPPP ) ),
     :                    %VAL( CNF_PVAL( IPCOV ) ),
     :                    NMAT, %VAL( CNF_PVAL( IPNCON ) ),
     :                    %VAL( CNF_PVAL( IPPNT ) ),
     :                    %VAL( CNF_PVAL( IPUSED ) ), STATUS )

*  Bin V (if needed)...
         IF( CIRC ) THEN
            CALL POL1_CM1RR( %VAL( CNF_PVAL( IPVST ) ), NBIN, MXCNT,
     :                       %VAL( CNF_PVAL( IPVVST ) ),
     :                       METH, MINVAL, NSIGMA,
     :                       %VAL( CNF_PVAL( IPBIN ) + VBNOFF ),
     :                       %VAL( CNF_PVAL( IPVBIN ) + VVBNOFF ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       %VAL( CNF_PVAL( IPPP ) ),
     :                       %VAL( CNF_PVAL( IPCOV ) ), NMAT,
     :                       %VAL( CNF_PVAL( IPNCON ) ),
     :                       %VAL( CNF_PVAL( IPPNT ) ),
     :                       %VAL( CNF_PVAL( IPUSED ) ), STATUS )


*  Bin Q and U (if needed)...
         ELSE
            CALL POL1_CM1RR( %VAL( CNF_PVAL( IPQST ) ), NBIN, MXCNT,
     :                       %VAL( CNF_PVAL( IPVQST ) ),
     :                       METH, MINVAL, NSIGMA,
     :                       %VAL( CNF_PVAL( IPBIN ) + QBNOFF ),
     :                       %VAL( CNF_PVAL( IPVBIN ) + VQBNOFF ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       %VAL( CNF_PVAL( IPPP ) ),
     :                       %VAL( CNF_PVAL( IPCOV ) ), NMAT,
     :                       %VAL( CNF_PVAL( IPNCON ) ),
     :                       %VAL( CNF_PVAL( IPPNT ) ),
     :                       %VAL( CNF_PVAL( IPUSED ) ), STATUS )

            CALL POL1_CM1RR( %VAL( CNF_PVAL( IPUST ) ), NBIN, MXCNT,
     :                       %VAL( CNF_PVAL( IPVUST ) ),
     :                       METH, MINVAL, NSIGMA,
     :                       %VAL( CNF_PVAL( IPBIN ) + UBNOFF ),
     :                       %VAL( CNF_PVAL( IPVBIN ) + VUBNOFF ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       %VAL( CNF_PVAL( IPPP ) ),
     :                       %VAL( CNF_PVAL( IPCOV ) ), NMAT,
     :                       %VAL( CNF_PVAL( IPNCON ) ),
     :                       %VAL( CNF_PVAL( IPPNT ) ),
     :                       %VAL( CNF_PVAL( IPUSED ) ), STATUS )
         END IF

*  Now do the binning if there are no variances.
      ELSE

*  Allocate an array to hold the variance to use for each line of data,
*  and set each element to 1.0 (i.e. give all input values equal weight).
         CALL PSX_CALLOC( MXCNT, '_REAL', IPVAR, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 999
         CALL POL1_SETR( MXCNT, 1.0, %VAL( CNF_PVAL( IPVAR ) ),
     :                   STATUS )

*  Total intensity...
         CALL POL1_CM3RR( %VAL( CNF_PVAL( IPIST ) ), NBIN, MXCNT,
     :                    %VAL( CNF_PVAL( IPVAR ) ),
     :                    METH, MINVAL, NSIGMA,
     :                    %VAL( CNF_PVAL( IPBIN ) + IBNOFF ),
     :                    %VAL( CNF_PVAL( IPWRK1 ) ),
     :                    %VAL( CNF_PVAL( IPWRK2 ) ),
     :                    %VAL( CNF_PVAL( IPNCON ) ),
     :                    %VAL( CNF_PVAL( IPPNT ) ),
     :                    %VAL( CNF_PVAL( IPUSED ) ), STATUS )

*  V (if needed)...
         IF( CIRC ) THEN
            CALL POL1_CM3RR( %VAL( CNF_PVAL( IPVST ) ), NBIN, MXCNT,
     :                       %VAL( CNF_PVAL( IPVAR ) ),
     :                       METH, MINVAL, NSIGMA,
     :                       %VAL( CNF_PVAL( IPBIN ) + VBNOFF ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       %VAL( CNF_PVAL( IPNCON ) ),
     :                       %VAL( CNF_PVAL( IPPNT ) ),
     :                       %VAL( CNF_PVAL( IPUSED ) ), STATUS )

*  Q and U (if needed)...
         ELSE
            CALL POL1_CM3RR( %VAL( CNF_PVAL( IPQST ) ), NBIN, MXCNT,
     :                       %VAL( CNF_PVAL( IPVAR ) ),
     :                       METH, MINVAL, NSIGMA,
     :                       %VAL( CNF_PVAL( IPBIN ) + QBNOFF ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       %VAL( CNF_PVAL( IPNCON ) ),
     :                       %VAL( CNF_PVAL( IPPNT ) ),
     :                       %VAL( CNF_PVAL( IPUSED ) ), STATUS )

            CALL POL1_CM3RR( %VAL( CNF_PVAL( IPUST ) ), NBIN, MXCNT,
     :                       %VAL( CNF_PVAL( IPVAR ) ),
     :                       METH, MINVAL, NSIGMA,
     :                       %VAL( CNF_PVAL( IPBIN ) + UBNOFF ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       %VAL( CNF_PVAL( IPNCON ) ),
     :                       %VAL( CNF_PVAL( IPPNT ) ),
     :                       %VAL( CNF_PVAL( IPUSED ) ), STATUS )
         END IF

      END IF

*  Now create the output catalogue to receive the binned Stokes parameters
*  and corresponding Stokes parameters found.
*  =======================================================================

*  Get the reference direction for the output catalogue. This may be
*  different to the reference direction for the input cube. Use the +ve Y
*  axis as the reference direction if no WCS information is available.
      CALL POL1_ANGRT( IWCS, XC, YC, ANGRT, STATUS )

*  Get the units string from total intensity column of the input catalogue.
      UNITS = ' '
      CALL CAT_TIQAC( GI( I_ID ), 'UNITS', UNITS, STATUS)

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the TITLE parameter (if any) from the input catalogue.
      CALL CAT_TIDNT( CIIN, 'TITLE', GTTL, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL CAT_TIQAC( GTTL, 'VALUE', TITLE, STATUS )
         CALL CAT_TRLSE( GTTL, STATUS )
      ELSE
         TITLE = ' '
         CALL ERR_ANNUL( STATUS )
      END IF

*  See if RA/DEC columns are to be created if possible.
      CALL PAR_GET0L( 'RADEC', RADEC, STATUS )

*  Create the output catalogue.
      CALL POL1_MKCAT( 'OUT', IWCS, CIRC, UNITS, VAR, ANGRT, TITLE,
     :                 RADEC, CIOUT, EQMAP, ONAME, STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Now calculate the polarization parameters based on the binned Stokes
*  parameters, storing them in the catalogue created above.
*  ====================================================================
*  Allocate work arrays.
      IF( EQMAP .NE. AST__NULL ) THEN
         CALL PSX_CALLOC( NXBIN*NYBIN*NDIMO, '_DOUBLE', IPW2, STATUS )
      ELSE
         IPW2 = IP
      END IF

*  If de-biasing is required, see what bias estimator is to be used.
      IF( DEBIAS ) THEN
         CALL PAR_CHOIC( 'DEBIASTYPE', 'AS', 'AS,MAS', .FALSE.,
     :                   CVAL, STATUS )
         IF( CVAL .EQ. 'AS' ) THEN
            IDEBIAS = 1
         ELSE
            IDEBIAS = 2
         END IF
      ELSE
         IDEBIAS = 0
      END IF

*  Calculate the polarization vectors. POL1_PLVEC has the cabability to
*  produce output images containing the polarization parameters, These
*  are not wanted here, but pointers still have to be given even though
*  they are ignored. Use IP as a safe pointer.
      CALL POL1_PLVEC( TR2, EQMAP, NXBIN, NYBIN, NZBIN, NSTOKE,
     :                 NXBIN*NYBIN, %VAL( CNF_PVAL( IPBIN ) ),
     :                 %VAL( CNF_PVAL( IPVBIN ) ),
     :                 STOKES, IDEBIAS, VAR, ANGROT, ANGRT, NDIMO,
     :                 .FALSE., .FALSE., .FALSE., .FALSE.,
     :                 .FALSE., .FALSE., .FALSE., .TRUE., CIOUT,
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IP ) ),
     :                 %VAL( CNF_PVAL( IPW2 ) ),
     :                 STATUS )

*  Free the work space.
      IF( EQMAP .NE. AST__NULL ) CALL PSX_FREE( IPW2, STATUS )

*  Closedown sequence.
*  ===================

*  Arrive here if an error occurs.
 999  CONTINUE

*  Close the output catalogue, storing a copy of the WCS information from
*  the input catalogue.
      CALL POL1_CLCAT( IWCS, CIOUT, STATUS )

*  If an error has occurred, delete the output catalogue.
      IF( STATUS .NE. SAI__OK ) CALL POL1_RM( ONAME )

*  Release work space.
      IF( .NOT. CIRC ) THEN
         CALL PSX_FREE( IPQST, STATUS )
         CALL PSX_FREE( IPUST, STATUS )
      ELSE
         CALL PSX_FREE( IPVST, STATUS )
      END IF

      IF( VAR ) THEN
         CALL PSX_FREE( IPVIST, STATUS )
         IF( IPPP .NE. IPNCON ) THEN
            CALL PSX_FREE( IPPP, STATUS )
            CALL PSX_FREE( IPCOV, STATUS )
         END IF
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

      CALL PSX_FREE( IP, STATUS )
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPIST, STATUS )
      CALL PSX_FREE( IPWRK1, STATUS )
      CALL PSX_FREE( IPWRK2, STATUS )
      CALL PSX_FREE( IPNCON, STATUS )
      CALL PSX_FREE( IPPNT, STATUS )
      CALL PSX_FREE( IPUSED, STATUS )
      CALL PSX_FREE( IPBIN, STATUS )

*  Release the input catalogue identifier.
      CALL CAT_TRLSE( CIIN, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLBIN_ERR', 'POLBIN: Error binning a '//
     :                 'polarization catalogue.', STATUS )
      END IF

      END
