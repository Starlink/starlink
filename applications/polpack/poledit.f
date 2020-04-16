      SUBROUTINE POLEDIT ( STATUS )
*+
*  Name:
*     POLEDIT

*  Purpose:
*     Change the values in a vector catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLEDIT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates an output vector catalogue containing a
*     copy of the input catalogue supplied by parameter IN. The changes
*     specified by parameter MODE are made to the output catalogue.

*  Usage:
*     poledit in out mode

*  ADAM Parameters:
*     COL = LITERAL (Read)
*        The name of the catalogue column to use. If parameter MODE is
*        "ChangeColVals" the name of an existing column must be supplied
*        (a list of available column names is displayed if a non-existent
*        column name is given). If parameter MODE is "AddColumn" the name
*        of the new column must be supplied (an error will be reported if
*        the column already exists).
*     COL1 = LITERAL (Read)
*        The name of the catalogue column which gives the first coordinate
*        of each data value. A list of available column names is displayed
*        if a non-existent column name is given. ["RA"]
*     COL2 = LITERAL (Read)
*        The name of the catalogue column which gives the first coordinate
*        of each data value. A list of available column names is displayed
*        if a non-existent column name is given. ["DEC"]
*     COMMENT = LITERAL (Read)
*        A descriptive comment to store with the  new column if parameter
*        MODE is "AddColumn". The suggested default is the Label value from
*        the NDF specified by parameter NDF.
*     DEBIASTYPE = LITERAL (Read)
*        Only used if parameter MODE is "Debias". It gives the type of bias
*        estimator to use when creating the new P and PI values, using the
*        nomeclature of Montier at al "Polarization measurements analysis II.
*        Best estimators of polarization fraction and angle" (A&A, 2018):
*          - "AS": The asymptotic estimator. See section 2.3 of Montier
*             et al. This estimator produces bad P and PI values if the
*             squared PI value is less than the variance in PI.
*          - "MAS": The modified asymptotic estimator. See section 2.5 of
*             Montier et al. This estimator does not produces bad P and PI
*             values, even if the squared PI value is less than the
*             variance in PI.
*          - "None": No de-bising is applied.
*     IN = LITERAL (Read)
*        The input catalogue.
*     MASKCOL = _LOGICAL (Read)
*        Determines how the NDF specified by parameter NDF is used to
*        generate the new column values (only used if parameter MODE is
*        "AddColumn"). If parameter MASKCOL is FALSE (the default), the
*        NDF's pixel array is sampled at the positions given in the
*        catalogue columns specified by parameters COL1 and COL2, using
*        the interpolation method specified by parameter METHOD, and the
*        sampled values are stored in the new column. The data type of
*        the new column is set to the data type of the NDF. If parameter
*        MASKCOL is TRUE, the new column holds integer values - 1 if the
*        corresponding pixel in the NDF array is good and 0 otherwise.
*        Parameters COL1 and COL2 again specify the catalogue columns
*        that hold the position for each row. The nearest pixel to
*        the resulting position is tested by comparing it with the
*        appropriate Starlink "bad" value. [FALSE]
*     METHOD = LITERAL (Read)
*        Only used if parameter MODE is set to "ChangeColVals" or "AddColumn".
*        It gives the method to use when sampling the NDF pixel array to
*        generate the new catalogue column values. For details on these
*        schemes, see the descriptions of routines AST_RESAMPLEx in SUN/210.
*        METHOD can take the following values.
*
*        - "Bilinear" -- The new column values are calculated by bi-linear
*        interpolation among the four nearest pixels values in the input NDF.
*        Produces smoother output NDFs than the nearest-neighbour scheme, but
*        is marginally slower.
*
*        - "Nearest" -- The new column values are assigned the value  of the
*        single nearest input pixel.
*
*        - "Sinc" -- The new column values are calculated using the sinc(pi*x)
*        kernel, where x is the pixel offset from the interpolation point and
*        sinc(z)=sin(z)/z. Use of this scheme is not recommended.
*
*        - "SincSinc" -- Uses the sinc(pi*x)sinc(k*pi*x) kernel. A
*        valuable general-purpose scheme, intermediate in its visual
*        effect between the bilinear and nearest-neighbour schemes.
*
*        - "SincCos" -- Uses the sinc(pi*x)cos(k*pi*x) kernel. Gives
*        similar results to the "Sincsinc" scheme.
*
*        - "SincGauss" -- Uses the sinc(pi*x)exp(-k*x*x) kernel. Good
*        results can be obtained by matching the FWHM of the envelope
*        function to the point-spread function of the input data (see
*        Parameter PARAMS).
*
*        - "Somb" -- Uses the somb(pi*x) kernel, where x is the pixel
*        offset from the interpolation point and somb(z)=2*J1(z)/z (J1 is
*        the first-order Bessel function of the first kind. This scheme
*        is similar to the "Sinc" scheme.
*
*        - "SombCos" -- Uses the somb(pi*x)cos(k*pi*x) kernel. This
*        scheme is similar to the "SincCos" scheme.
*
*        - "Gauss" -- Uses the exp(-k*x*x) kernel. The FWHM of the Gaussian
*        is given by Parameter PARAMS(2), and the point at which to truncate
*        the Gaussian to zero is given by Parameter PARAMS(1).
*
*        The initial default is "Nearest".  [current value]
*     MODE = LITERAL (Read)
*        The type of edit to apply. Must be one of the following values:
*
*        - "AddColumn": A new column is added to the catalogue. The name
*        of the new column is given by parameter COL. The values to store
*        in the column are obtained from the NDF specified by parameter
*        NDF in a manner determined by parameter MASKCOL.
*
*        - "Debias": New debiased values are written to the P (percentage
*        polarisation) and PI (polarised intensity) columns based on the
*        values in the Q, U, I and DPI columns. The form of de-biasing to
*        use is specified by parameter DEBIASTYPE. This option is the same
*        as the "Recalc" option except that only the P and PI columns are
*        recalculated.
*
*        - "ChangeColVals": The values in the column specified by parameter
*        COL are changed to the values read from the NDF specified by
*        parameter NDF. The NDF is sampled at the positions stored in the
*        catalogue columns specified by parameters COL1 and COL2, using the
*        interpolation method specified by parameter METHOD.
*
*        - "Recalc": All columns that are derived from the Stokes parameter
*        columns are re-calculated. In other words, the P, PI, ANG, DP, DPI
*        and DANG columns are recalculated on the basis of the curent values
*        in the I, Q, U, DI, DQ and DU columns. The form of de-biasing to
*        use is specified by parameter DEBIASTYPE.
*
*     NDF = NDF (Read)
*        The input NDF to use when parameter MODE is "ChangeColVals" or
*        "AddColumn".
*     OUT = LITERAL (Read)
*        The output catalogue.
*     PARAMS( 2 ) = _DOUBLE (Read)
*        An optional array which consists of additional parameters
*        required by the Sinc, SincSinc, SincCos, SincGauss, Somb,
*        SombCos and Gauss methods (see parameter METHOD).
*
*        PARAMS( 1 ) is required by all the above schemes. It is used to
*        specify how many pixels are to contribute to the interpolated
*        result on either side of the interpolation in each dimension.
*        Typically, a value of 2 is appropriate and the minimum allowed
*        value is 1 (i.e. one pixel on each side). A value of zero or
*        fewer indicates that a suitable number of pixels should be
*        calculated automatically. [0]
*
*        PARAMS( 2 ) is required only by the Gauss, SombCos, SincSinc,
*        SincCos, and SincGauss schemes. For the SombCos, SincSinc and
*        SincCos schemes, it specifies the number of pixels at which the
*        envelope of the function goes to zero. The minimum value is
*        1.0, and the run-time default value is 2.0. For the Gauss and
*        SincGauss scheme, it specifies the full-width at half-maximum
*        (FWHM) of the Gaussian envelope measured in output pixels.  The
*        minimum value is 0.1, and the run-time default is 1.0.  On
*        astronomical NDFs and spectra, good results are often obtained
*        by approximately matching the FWHM of the envelope function,
*        given by PARAMS(2), to the point-spread function of the input
*        data. []
*     UNITS = LITERAL (Read)
*        A string giving the units used by the  new column if parameter
*        MODE is "AddColumn". The suggested default is the Units value from
*        the NDF specified by parameter NDF.
*  Notes:
*     - The input catalogue must contain WCS information (i.e. it must
*     have been created using polpack).

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory.
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
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     9-APR-2020 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'CNF_PAR'          ! CNF_ functions
      INCLUDE 'CAT_PAR'          ! CAT_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER COMM*80          ! Column descriptive comment
      CHARACTER CTYPE*( DAT__SZTYP ) ! NDF data type
      CHARACTER COLNM1*30        ! First external column name
      CHARACTER COLNM2*30        ! Second external column name
      CHARACTER DBTYPE*5         ! Type of de-biasing to use
      CHARACTER DEFNM1*30        ! First default external column name
      CHARACTER DEFNM2*30        ! Second default external column name
      CHARACTER FIELDS( 5 )*50   ! Individual fields of catalogue specification
      CHARACTER METHOD*13        ! Interpolation method to use.
      CHARACTER MODE*30          ! Edit mode
      CHARACTER ONAME*256        ! Full file spec for output
      CHARACTER UNITS*30         ! Units string
      DOUBLE PRECISION PARAMS( 2 )! Param. values passed to AST_RESAMPLE
      DOUBLE PRECISION SHIFTS( 2 )! Shifts to apply
      INTEGER CIIN               ! CAT identifier for input catalogue
      INTEGER CIOUT              ! CAT identifier for output catalogue
      INTEGER CWCS               ! WCS FrameSet from input catalogue
      INTEGER DTYPE              ! The column data type
      INTEGER EL                 ! Number of pixels mapped
      INTEGER FS                 ! FrameSet pointer
      INTEGER GI( 2 )            ! CAT identifiers for COL1 and COL2
      INTEGER GI0                ! CAT identifiers for COL
      INTEGER INDF               ! Identifier for input NDF
      INTEGER IPCOL              ! Pointer to array of new column values
      INTEGER IPDATA             ! Pointer to NDF array
      INTEGER IPLUT              ! Pointer to Look-up table arrays
      INTEGER IWCS               ! WCS FrameSet from input NDF
      INTEGER LUTMAP1            ! Pointer to LutMap for axis 1
      INTEGER LUTMAP2            ! Pointer to LutMap for axis 2
      INTEGER LUTMAPS            ! Pointer to LutMap for axes 1 and 2
      INTEGER MAP                ! Mapping from COL1/COL2 frame to PIXEL
      INTEGER METHOD_CODE        ! Integer corresponding to interp. method
      INTEGER NBAD               ! No. of bad output values
      INTEGER NC                 ! Used length of string
      INTEGER NPAR               ! No. of required interpolation parameters
      INTEGER NROW               ! No. of rows in catalogue
      INTEGER OUTPERM( 2 )       ! Output permutation
      INTEGER PMAP               ! PermMap to duplicate row number
      INTEGER ROWMAP             ! Mapping from row number to COL1/COL2 frame
      INTEGER SDIM( 2 )          ! Indices of significant pixel axes
      INTEGER SLBND( 2 )         ! Lower bounds of significant pixel axes
      INTEGER SUBND( 2 )         ! Upper bounds of significant pixel axes
      INTEGER TEMPLT             ! Template Frame for searching IWCS
      INTEGER TOTMAP             ! Mapping from row number to PIXEL frame
      LOGICAL ISRADEC            ! Do COL1/COL2 hold RA and DEC?
      LOGICAL MASKCOL            ! Should column values form a mask?
      LOGICAL VERB               ! Verbose errors required?
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  See if the user wants verbose error messages.
      CALL KPG1_VERB( VERB, 'POLPACK', STATUS )

*  Open the input catalogue, and get its name.
      CALL CTG_ASSO1( 'IN', VERB, 'READ', CIIN, FIELDS, STATUS )

*  Attempt to read an AST FrameSet from the input catalogue. This WCS
*  information will be copied to the output catalogue when the output
*  catalogue is closed.
      CALL POL1_GTCTW( CIIN, CWCS, STATUS )
      IF( CWCS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'C', FIELDS( 5 ) )
         CALL ERR_REP( ' ', 'No WCS information found in input '//
     :                 'catalogue "^C".', STATUS )
      END IF

*  Open the output catalogue.
      CALL CTG_CREA1( 'OUT', CIOUT, ONAME, STATUS )

*  Find the number of rows in the catalogue.
      CALL CAT_TROWS( CIIN, NROW, STATUS )

*  Copy the polpack version number from input to output.
      CALL POL1_CPVRC( CIIN, CIOUT, STATUS )

*  Get the edit mode.
      CALL PAR_CHOIC( 'MODE', 'CHANGECOLVALS', 'CHANGECOLVALS,'//
     :                'ADDCOLUMN,DEBIAS,RECALC', .FALSE., MODE, STATUS )

*  Replace column values or add a new column with values read from an
*  input NDF.
*  ------------------------------------------------------------------
      IF( MODE .EQ. 'CHANGECOLVALS' .OR. MODE .EQ. 'ADDCOLUMN' ) THEN

*  Get the NDF, its WCS info and pixel bounds.
         CALL NDF_ASSOC( 'NDF', 'Read', INDF, STATUS )
         CALL KPG1_ASGET( INDF, 2, .TRUE., .TRUE., .TRUE., SDIM, SLBND,
     :                    SUBND, IWCS, STATUS )

*  Get identifiers for the catalogue columns holding sample positions.
*  Default to "RA" and "DEC".
         CALL POL1_COLNM( 'RA', .FALSE., DEFNM1, STATUS )
         CALL POL1_GTCTC( 'COL1', CIIN, CAT__FITYP, DEFNM1, GI( 1 ),
     :                     STATUS )
         CALL POL1_COLNM( 'DEC', .FALSE., DEFNM2, STATUS )
         CALL POL1_GTCTC( 'COL2', CIIN, CAT__FITYP, DEFNM2, GI( 2 ),
     :                     STATUS )

*  Are COL1/COL2 "RA" and "DEC"?
         CALL CAT_TIQAC( GI( 1 ), 'NAME', COLNM1, STATUS )
         CALL CAT_TIQAC( GI( 2 ), 'NAME', COLNM2, STATUS )
         IF( COLNM1 .EQ. DEFNM1 .AND. COLNM2 .EQ. DEFNM2 ) THEN
            ISRADEC = .TRUE.

*  If not, they must be "X" and "Y".
         ELSE
            CALL POL1_COLNM( 'X', .FALSE., DEFNM1, STATUS )
            CALL POL1_COLNM( 'Y', .FALSE., DEFNM2, STATUS )
            IF( COLNM1 .EQ. DEFNM1 .AND. COLNM2 .EQ. DEFNM2 ) THEN
               ISRADEC = .FALSE.
            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               CALL MSG_SETC( 'C1', COLNM1 )
               CALL MSG_SETC( 'C2', COLNM2 )
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'Parameters COL1 (^C1) and COL2 '//
     :                       '(^C2) do not specified a pair of '//
     :                       'spatial coordinate columns.', STATUS )
            END IF
         END IF

*  If COL1/COL2 are "X" and "Y", then the Mapping from NDF PIXEL to
*  catalogue column COL1/COL2 values (i.e. X and Y) is a UnitMap.
         IF( .NOT. ISRADEC ) THEN
            MAP = AST_UNITMAP( 2, ' ', STATUS )

*  If COL1/COL2 are "RA" and "DEC", then the Mapping connecting COL1/COL2 to
*  the NDF PIXEL system must be obtained from the NDF's WCS FrameSet.
         ELSE

*  The RA/DEC columns are always FK5,J2000 (Epoch may vary). Create a
*  SkyFrame with these properties that can be used as a template when
*  searching the NDF's FrameSet.
            TEMPLT = AST_SKYFRAME( 'System=FK5,Equinox=J2000', STATUS )

*  Find a Mapping from NDF PIXEL coordinates (always frame 2 in IWCS) to
*  the catalogue's SKY coordinate system (defined by the above template).
*  Since AST_FINDFRAME find the mapping from the Base Frame of the target
*  to the template Frame, we need to make frame 2 (PIXEL) the Base Frame
*  in IWCS first.
            CALL AST_SETI( IWCS, 'Base', 2, STATUS )
            FS = AST_FINDFRAME( IWCS, TEMPLT, 'SKY', STATUS )
            IF( FS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'No celestial coordinates '//
     :                      'found in the NDF''s WCS information.',
     :                      STATUS )
            ELSE

*  Get the Mapping from catalogue column value (i.e. RA and DEC) to NDF
*  PIXEL.
               MAP = AST_GETMAPPING( FS, AST__CURRENT, AST__BASE,
     :                               STATUS )
            END IF
         END IF

*  Now we have the Mapping from the NDF PIXEL system to the catalogue
*  columns represented by COL1/COL2. We will use AST_RESAMPLE to get an
*  interpolated data value from the NDF for each catalogue row. So we
*  need the Mapping from row number to PIXEL position. For this we need
*  to contruct a pair of LutMaps to represent the COL1 and COL2 values as
*  a pair of look-up tables. First allocate room for the two tables.
         CALL PSX_CALLOC( 2*NROW, '_DOUBLE', IPLUT, STATUS )

*  Read the values from the two columns into the above array.
         CALL POL1_CTCLD( CIIN, NROW, 2, GI, %VAL( CNF_PVAL(IPLUT) ),
     :                    STATUS )

*  Create the two LutMaps. The input to each LutMap is one-based row
*  number, and the output is the column value. Ensure nearest neighbour
*  interpolation is used.
         LUTMAP1 = AST_LUTMAP( NROW,  %VAL( CNF_PVAL(IPLUT) ), 1.0D0,
     :                         1.0D0, 'LutInterp=1', STATUS )
         LUTMAP2 = AST_LUTMAP( NROW,  %VAL( CNF_PVAL(IPLUT) +
     :                                      NROW*VAL__NBD ), 1.0D0,
     :                         1.0D0, 'LutInterp=1', STATUS )

*  Combine them in parallel.
         LUTMAPS = AST_CMPMAP( LUTMAP1, LUTMAP2, .FALSE., ' ', STATUS )

*  Create a PermMap which duplicates its one input. This is used to
*  create two copies of row number that can be used to feed the two
*  inputs of LUTMAPS.
         OUTPERM( 1 ) = 1
         OUTPERM( 2 ) = 1
         PMAP = AST_PERMMAP( 1, 1, 2, OUTPERM, 0.0D0, ' ', STATUS )

*  Combine the above mapping to get a Mapping from row number to the
*  COL1/COL2 frame.
         ROWMAP = AST_CMPMAP( PMAP, LUTMAPS, .TRUE., ' ', STATUS )

*  Combine the above mapping (from row number to the COL1/COL2 frame) with
*  the mapping from the COL1/COL2 frame to NDF PIXEL position, to get a
*  Mapping from catalogue row number to NDF PIXEL position.
         TOTMAP = AST_CMPMAP( ROWMAP, MAP, .TRUE., ' ', STATUS )

*  AST_RESAMPLE uses a pixel coordinate scheme where the *centre* of each
*  array pixel corresponds to an integer value, unlike the usual Starlink
*  pixel coordinate system where the start of a pixel is an integer value.
*  So we need to include an extra shift of half a pixel on each axis.
         SHIFTS( 1 ) = 0.5D0
         SHIFTS( 2 ) = 0.5D0
         TOTMAP = AST_CMPMAP( TOTMAP, AST_SHIFTMAP( 2, SHIFTS, ' ',
     :                                              STATUS ),
     :                        .TRUE., ' ', STATUS )

*  The AST_RESAMPLE routine requires the forward transformation of the
*  supplied mapping to go the other way (from NDF PIXEL to catalogue row
*  number), so invert the above Mapping.
         CALL AST_INVERT( TOTMAP, STATUS )

*  Get the data type in which to map the NDF. If we are modifying an
*  existing column, use the data type of the existing column. If we are
*  adding a new column, use the native data type of the NDF.
         IF( MODE .EQ. 'CHANGECOLVALS' ) THEN

*  Copy the full contents of the input catalogue (parameters, etc, as well
*  as table values) to the output catalogue. We can copy the table values
*  now since we will not be adding any new columns to the output catalogue.
            CALL POL1_CPCAT( CIIN, CIOUT, AST__NULL, .TRUE., STATUS )

*  Get an identifier for the column to change, restricted to one of the
*  existing columns.
            CALL POL1_GTCTC( 'COL', CIOUT, CAT__FITYP, ' ', GI0,
     :                       STATUS )

*  Get the data type of the column, as a integer CAT identifier in DTYPE.
            CALL CAT_TIQAI( GI0, 'DTYPE', DTYPE, STATUS )

*  Get the corresponding HDS type.
            IF( DTYPE .EQ. CAT__TYPEB ) THEN
               CTYPE = '_BYTE'
            ELSE IF( DTYPE .EQ. CAT__TYPEW ) THEN
               CTYPE = '_WORD'
            ELSE IF( DTYPE .EQ. CAT__TYPEI ) THEN
               CTYPE = '_INTEGER'
            ELSE IF( DTYPE .EQ. CAT__TYPER ) THEN
               CTYPE = '_REAL'
            ELSE IF( DTYPE .EQ. CAT__TYPED ) THEN
               CTYPE = '_DOUBLE'
            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL CAT_TIQAC( GI0, 'NAME', COLNM1, STATUS )
               CALL MSG_SETC( 'N', COLNM1 )
               CALL MSG_SETC( 'T', CTYPE )
               CALL ERR_REP( ' ', 'Column ''^N'' has unsupported data'//
     :                       ' type ''^T''.', STATUS )
            END IF

*  The column will store the actual NDF pixel values, not a mask of
*  boolean values.
            MASKCOL = .FALSE.

*  If adding a new column....
         ELSE

*  Copy the column and parameter definitions from the input catalogue
*  to the output catalogue. Do not copy the row data since we need to add
*  the new column definition first.
            CALL POL1_CPCAT( CIIN, CIOUT, AST__NULL, .FALSE., STATUS )

*  See if the new column should be a mask holding boolean values.
            CALL PAR_GET0L( 'MASKCOL', MASKCOL, STATUS )

*  Get the data type of the NDF (as an HDS type string).
            CALL NDF_TYPE( INDF, 'Data', CTYPE, STATUS )

*  Get the corresponding CAT data type constant. Promote unsigned data
*  types to the next larger signed type since CAT does not support
*  unsigned data types.
            IF( MASKCOL ) THEN
               DTYPE = CAT__TYPEI
            ELSE IF( CTYPE .EQ. '_BYTE' ) THEN
               DTYPE = CAT__TYPEB
            ELSE IF( CTYPE .EQ. '_WORD' .OR.
     :               CTYPE .EQ. '_UBYTE' ) THEN
               DTYPE = CAT__TYPEW
               CTYPE = '_WORD'
            ELSE IF( CTYPE .EQ. '_INTEGER' .OR.
     :               CTYPE .EQ. '_UWORD' ) THEN
               DTYPE = CAT__TYPEI
               CTYPE = '_INTEGER'
            ELSE IF( CTYPE .EQ. '_REAL' ) THEN
               DTYPE = CAT__TYPER
            ELSE IF( CTYPE .EQ. '_DOUBLE' ) THEN
               DTYPE = CAT__TYPED
            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL NDF_MSG( 'N', INDF )
               CALL MSG_SETC( 'T', CTYPE )
               CALL ERR_REP( ' ', 'NDF ''^N'' has unsupported data '//
     :                       'type ''^T''.', STATUS )
            END IF

*  Get an identifier for a new column using parameter COL.
            CALL POL1_NWCTC( 'COL', CIOUT, CAT__FITYP, ' ', DTYPE, GI0,
     :                       STATUS )

*  Get the descriptive commment for the new column.
            COMM = ' '
            CALL NDF_CGET( INDF, 'Label', COMM, STATUS )
            IF( MASKCOL ) THEN
               NC = CHR_LEN( COMM )
               IF( NC .EQ. 0 ) THEN
                  COMM = 'Mask'
               ELSE
                  COMM( NC + 2 : ) = 'mask'
               END IF
            END IF
            CALL PAR_DEF0C( 'COMMENT', COMM, STATUS )
            CALL PAR_GET0C( 'COMMENT', COMM, STATUS )

*  Store it in the catalogue.
            IF( COMM .NE. ' ' ) CALL CAT_TATTC( GI0, 'COMM', COMM,
     :                                          STATUS )

*  Get the units string for the new column (no units needed if the
*  column is a mask).
            IF( .NOT. MASKCOL ) THEN
               UNITS = ' '
               CALL NDF_CGET( INDF, 'Units', UNITS, STATUS )
               CALL PAR_DEF0C( 'UNITS', UNITS, STATUS )
               CALL PAR_GET0C( 'UNITS', UNITS, STATUS )

*  Store it in the catalogue.
               IF( UNITS .NE. ' ' ) CALL CAT_TATTC( GI0, 'UNITS', UNITS,
     :                                              STATUS )
            END IF

*  We can copy the table values from input to output now as the extra
*  column definition has been stored in the output catalogue.
            CALL POL1_CPTAB( CIIN, CIOUT, 0, 0, 0, AST__NULL, STATUS )
         END IF

*  Map the required NDF array component.
         CALL NDF_MAP( INDF, 'Data', CTYPE, 'Read', IPDATA, EL,
     :                 STATUS )

*  Allocate room for the new column values.
         CALL PSX_CALLOC( NROW, CTYPE, IPCOL, STATUS )

*  Get the interpolation method to be used.
         IF( MASKCOL ) THEN
            METHOD = 'Nearest'
         ELSE
            CALL PAR_CHOIC( 'METHOD', 'Nearest', 'Nearest,Bilinear,'//
     :                      'Sinc,Gauss,SincSinc,SincCos,SincGauss,'//
     :                      'Somb,SombCos', .TRUE., METHOD, STATUS )
         END IF

*  Tell the user which kernel is being used and set suitable defaults for
*  PARAMS for each type of kernel. Also get the integer code for the
*  method, as required by AST_RESAMPLE.
         NPAR = 0
         IF( METHOD( 1 : 1 ) .EQ. 'N' ) THEN
            METHOD_CODE = AST__NEAREST
            CALL MSG_OUT( ' ', '  Using nearest neighbour '//
     :                    'interpolation.', STATUS )

         ELSE IF( METHOD( 1 : 2 ) .EQ. 'BI' ) THEN
            METHOD_CODE = AST__LINEAR
            CALL MSG_OUT( ' ', '  Using bi-linear interpolation.',
     :                    STATUS )

         ELSE IF( METHOD( 1 : 1 ) .EQ. 'G' ) THEN
            NPAR = 2
            PARAMS( 1 ) = 0.0
            PARAMS( 2 ) = 2.0
            METHOD_CODE = AST__GAUSS
            CALL MSG_OUT( ' ', '  Using a Gaussian interpolation '//
     :                    'kernel.', STATUS )

         ELSE IF ( METHOD( 1 : 4 ) .EQ. 'SINC' ) THEN
            NPAR = 2
            PARAMS( 1 ) = 0.0
            PARAMS( 2 ) = 2.0

            IF ( METHOD( 5 : 5 ) .EQ. 'S' ) THEN
               METHOD_CODE = AST__SINCSINC
               CALL MSG_OUT( ' ', '  Using sincsinc interpolation '//
     :                       'kernel.', STATUS )

            ELSE IF( METHOD( 5 : 5 ) .EQ. 'C' ) THEN
               METHOD_CODE = AST__SINCCOS
               CALL MSG_OUT( ' ', '  Using sinccos interpolation '//
     :                       'kernel.', STATUS )

            ELSE IF( METHOD( 5 : 5 ) .EQ. 'G' ) THEN
               METHOD_CODE = AST__SINCGAUSS
               PARAMS( 2 ) = 1.0
               CALL MSG_OUT( ' ', '  Using sincgauss interpolation '//
     :                       'kernel.', STATUS )

           ELSE
               NPAR = 1
               METHOD_CODE = AST__SINC
               CALL MSG_OUT( ' ', '  Using sinc interpolation kernel.',
     :                       STATUS )
            END IF

         ELSE IF ( METHOD( 1 : 4 ) .EQ. 'SOMB' ) THEN
            NPAR = 2
            PARAMS( 1 ) = 0.0
            PARAMS( 2 ) = 2.0

            IF( METHOD( 5 : 5 ) .EQ. 'C' ) THEN
               METHOD_CODE = AST__SOMBCOS
               CALL MSG_OUT( ' ', '  Using sombcos interpolation '//
     :                       'kernel.', STATUS )

            ELSE
               NPAR = 1
               METHOD_CODE = AST__SOMB
               CALL MSG_OUT( ' ', '  Using somb interpolation kernel.',
     :                       STATUS )

            END IF

         END IF

*  If required, set the dynamic defaults for PARAMS and then get the
*  required number of interpolation parameters.
         IF( NPAR .GT. 0 ) THEN
            CALL PAR_DEF1D( 'PARAMS', NPAR, PARAMS, STATUS )
            CALL PAR_EXACD( 'PARAMS', NPAR, PARAMS, STATUS )
         END IF

*  Use AST_RESAMPLE to sample the NDF data array at the position of each
*  catalogue row, and place the new values in the above allocated array.
*  Then modify the output catalogue by replacing the required column
*  values with the values produced by AST_RESAMPLE.
         IF( CTYPE .EQ. '_BYTE' ) THEN
            NBAD = AST_RESAMPLEB( TOTMAP, 2,  SLBND, SUBND,
     :                            %VAL( CNF_PVAL(IPDATA) ), 0,
     :                            METHOD_CODE, AST_NULL, PARAMS,
     :                            AST__USEBAD, 0.0D0, 0, VAL__BADB,
     :                            1, 1, NROW, 1, NROW,
     :                            %VAL( CNF_PVAL(IPCOL) ), 0, STATUS )
            CALL POL1_PTCLMB( CIOUT, NROW, 1, GI0, MASKCOL,
     :                        %VAL( CNF_PVAL(IPCOL) ), STATUS )

         ELSE IF( CTYPE .EQ. '_WORD' ) THEN
            NBAD = AST_RESAMPLEW( TOTMAP, 2,  SLBND, SUBND,
     :                            %VAL( CNF_PVAL(IPDATA) ), 0,
     :                            METHOD_CODE, AST_NULL, PARAMS,
     :                            AST__USEBAD, 0.0D0, 0, VAL__BADW,
     :                            1, 1, NROW, 1, NROW,
     :                            %VAL( CNF_PVAL(IPCOL) ), 0, STATUS )
            CALL POL1_PTCLMW( CIOUT, NROW, 1, GI0, MASKCOL,
     :                        %VAL( CNF_PVAL(IPCOL) ), STATUS )

         ELSE IF( CTYPE .EQ. '_INTEGER' ) THEN
            NBAD = AST_RESAMPLEI( TOTMAP, 2,  SLBND, SUBND,
     :                            %VAL( CNF_PVAL(IPDATA) ), 0,
     :                            METHOD_CODE, AST_NULL, PARAMS,
     :                            AST__USEBAD, 0.0D0, 0, VAL__BADI,
     :                            1, 1, NROW, 1, NROW,
     :                            %VAL( CNF_PVAL(IPCOL) ), 0, STATUS )
            CALL POL1_PTCLMI( CIOUT, NROW, 1, GI0, MASKCOL,
     :                        %VAL( CNF_PVAL(IPCOL) ), STATUS )

         ELSE IF( CTYPE .EQ. '_REAL' ) THEN
            NBAD = AST_RESAMPLER( TOTMAP, 2,  SLBND, SUBND,
     :                            %VAL( CNF_PVAL(IPDATA) ), 0.0,
     :                            METHOD_CODE, AST_NULL, PARAMS,
     :                            AST__USEBAD, 0.0D0, 0, VAL__BADR,
     :                            1, 1, NROW, 1, NROW,
     :                            %VAL( CNF_PVAL(IPCOL) ), 0.0, STATUS )
            CALL POL1_PTCLMR( CIOUT, NROW, 1, GI0, MASKCOL,
     :                        %VAL( CNF_PVAL(IPCOL) ), STATUS )

         ELSE
            NBAD = AST_RESAMPLED( TOTMAP, 2,  SLBND, SUBND,
     :                            %VAL( CNF_PVAL(IPDATA) ), 0.0D0,
     :                            METHOD_CODE, AST_NULL, PARAMS,
     :                            AST__USEBAD, 0.0D0, 0, VAL__BADD,
     :                            1, 1, NROW, 1, NROW,
     :                            %VAL( CNF_PVAL(IPCOL) ), 0.0D0,
     :                            STATUS )
            CALL POL1_PTCLMD( CIOUT, NROW, 1, GI0, MASKCOL,
     :                        %VAL( CNF_PVAL(IPCOL) ), STATUS )

         END IF

*  Report an error if there are no good values to store.
         IF( NBAD .GE. NROW .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'No good values to store. Is there '//
     :                    'any overlap between the NDF and the '//
     :                    'catalogue?', STATUS )
         END IF

*  Free the dynamic arrays allocated above.
         CALL PSX_FREE( IPLUT, STATUS )
         CALL PSX_FREE( IPCOL, STATUS )

*  Re-calculate derived columns using specified de-biasing.
*  --------------------------------------------------------
      ELSE IF( MODE .EQ. 'DEBIAS' .OR. MODE .EQ. 'RECALC' ) THEN

*  Get the type of de-biasing to use.
         CALL PAR_CHOIC( 'DEBIASTYPE', 'NONE', 'None,AS,MAS', .TRUE.,
     :                   DBTYPE, STATUS )

*  Copy the full contents of the input catalogue (parameters, etc, as well
*  as table values) to the output catalogue. We can copy the table values
*  now since we will not be adding any new columns to the output catalogue.
         CALL POL1_CPCAT( CIIN, CIOUT, AST__NULL, .TRUE., STATUS )

*  Modify the output values.
         CALL POL1_RECALC( CIOUT, DBTYPE, (MODE .EQ. 'RECALC'), STATUS )

*  Report an error if the edit mode has not yet been implemented.
*  --------------------------------------------------------------
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'A', MODE )
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Edit mode "^A" has not yet been ' //
     :                 'implemented.', STATUS )
      END IF

*  Close the output catalogue, storing a copy of the WCS information from
*  the input catalogue.
      CALL POL1_CLCAT( CWCS, CIOUT, STATUS )

*  If an error has occurred, delete the output catalogue.
      IF( STATUS .NE. SAI__OK ) CALL POL1_RM( ONAME )

*  Release the input catalogue identifier.
      CALL CAT_TRLSE( CIIN, STATUS )

*  Release NDF and AST resources.
      CALL NDF_END( STATUS )
      CALL AST_END( STATUS )

*  Add a context message if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'POLEDIT: Failed to edit the values in a '//
     :                 'vector catalogue.', STATUS )
      END IF

      END
