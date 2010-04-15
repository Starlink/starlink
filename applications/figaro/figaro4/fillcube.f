      SUBROUTINE FILLCUBE( STATUS )
*+
*  Name:
*     FILLCUBE

*  Purpose:
*     Copy one NDF into part of another.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FILLCUBE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine copies data, variance etc. from one NDF into another
*     existing NDF. By successive calls the output NDF can be filled
*     with data from a number of input NDFs. The target area in the
*     output is identified by matching axis data (not pixel indices).
*     Data are copied from input to output only if the input data value
*     is not bad, apart from that existing data in the output are
*     overwritten.
*
*     This application is more akin to ASCIN than to GROW. The main
*     differences to ASCIN are that FILLCUBE updates an existing output
*     and that its input is an NDF rather than an ASCII table.
*     Its main advantage over GROW is that input and output may
*     (actually must) have the same dimensionality, but any dimensions
*     or axis data can differ. Also it is not necessary that target
*     pixels form a contiguous subset in the output: The input pixels
*     could match, say, every second or third output pixel.
*     The disadvantages are that results and spectroscopic values in the
*     Specdre Extension are not handled, and that the coordinates along
*     each axis in input and output must be linear.
*
*     For each input pixel, FILLCUBE looks for the output pixel that is
*     nearest in the space of axis data coordinates. Data are copied
*     only if the output pixel is hit close to its centre. However, if
*     an axis is degenerate (has only one pixel) in both input and
*     output, then the coordinates are assumed to match.
*
*     No indication is given as to how many input pixels did not match
*     any output pixel.

*  Usage:
*     fillcube in out

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        True if informational messages are to be issued.
*     TOL = _REAL (Read)
*        The tolerated fraction of the pixel size by which the input
*        coordinates may deviate from the output coordinates. If any one
*        of the axis values deviates more than TOL times the coordinate
*        step, then the input data are ignored and the output data left
*        unchanged. [0.2]
*     IN = NDF (Read)
*        The input NDF.
*     OUT = NDF (Read)
*        The output NDF. This must already exist, update access is
*        required.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7, although
*     it is largely ignored.
*
*     This routine works in situ on an existing output file.
*
*     Spectroscopic values must not exist in the Extension of either
*     the input or the output NDF: A unique coordinate axis is
*     required for all axes, including the spectroscopic one, in
*     order to locate the target pixels by matching coordinates
*     between input and output. If this is inconvenient, GROW may be
*     a more suitable application for your purpose.
*
*     Spectroscopic widths must not exist in the Extension of the
*     output NDF and are ignored in the input NDF: This information
*     is likely to be present only when spectroscopic values are
*     present as well.
*
*     Covariance row sums must not exist in the Extension of the
*     output NDF: The validity of this information is difficult to
*     assess when only parts of spectra might be copied from one cube
*     to another, and when these parts are contiguous in the input
*     but might not be in the output. Input covariance row sums are
*     ignored.
*
*     The results in the input Extension are ignored, and results
*     must not exist in the output Extension.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30 May 1993 (hme):
*        Original version.
*     25 Nov 1994 (hme):
*        Use new libraries.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL INFO
      REAL TOL
      LOGICAL XTHER1             ! True if IN Extension exists
      LOGICAL XTHER2             ! True if OUT Extension exists
      LOGICAL VTHER1             ! True if IN variances exist
      LOGICAL VTHER2             ! True if OUT variances exist
      LOGICAL THERE              ! True if an HDS component exists
      LOGICAL LINEAR             ! True if an array is linear
      INTEGER I                  ! Loop index
      INTEGER NDF( 2 )           ! NDF identifiers
      INTEGER PNTR( 4 )          ! Array pointers
      INTEGER NELM1              ! Array size
      INTEGER NELM2              ! Array size
      INTEGER NDIM1              ! Input NDF dimensionality
      INTEGER NDIM2              ! Output NDF dimensionality
      INTEGER DIM1( NDF__MXDIM ) ! Input NDF dimensions
      INTEGER DIM2( NDF__MXDIM ) ! Output NDF dimensions
      INTEGER LBND1( NDF__MXDIM ) ! Input NDF lower bounds
      INTEGER LBND2( NDF__MXDIM ) ! Output NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds, unused
      INTEGER SPAX1              ! IN SPECAXIS
      INTEGER SPAX2              ! OUT SPECAXIS
      REAL COEFF0( NDF__MXDIM )  ! Pixel transform coefficients
      REAL COEFF1( NDF__MXDIM )  ! Pixel transform coefficients
      REAL LVAL1R, UVAL1R        ! IN axis range
      REAL LVAL2R, UVAL2R        ! OUT axis range
      DOUBLE PRECISION LVAL1D, UVAL1D ! IN axis range
      DOUBLE PRECISION LVAL2D, UVAL2D ! OUT axis range
      CHARACTER * ( DAT__SZTYP ) TYPE1 ! Array data type
      CHARACTER * ( DAT__SZTYP ) TYPE2 ! Array data type
      CHARACTER * ( DAT__SZLOC ) XLOC1 ! IN Extension locator
      CHARACTER * ( DAT__SZLOC ) XLOC2 ! OUT Extension locator

*  Internal References:
      REAL SPD_UAAGR             ! Get array element
      DOUBLE PRECISION SPD_UAAGD ! dto.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN
      XTHER1 = .FALSE.
      XTHER2 = .FALSE.

*  Modal parameters.
      CALL PAR_GET0L( 'INFO', INFO, STATUS )
      CALL PAR_GET0R( 'TOL',  TOL,  STATUS )

*  Access input and output NDFs.
      CALL NDF_ASSOC( 'IN',  'READ',   NDF(1), STATUS )
      CALL NDF_ASSOC( 'OUT', 'UPDATE', NDF(2), STATUS )

*  Find out NDF shapes.
*  The lower bounds are needed only for the INFO messages.
      CALL NDF_DIM( NDF(1), NDF__MXDIM, DIM1, NDIM1, STATUS )
      CALL NDF_DIM( NDF(2), NDF__MXDIM, DIM2, NDIM2, STATUS )
      IF ( INFO ) THEN
         CALL NDF_BOUND( NDF(1), NDF__MXDIM, LBND1, UBND, I, STATUS )
         CALL NDF_BOUND( NDF(2), NDF__MXDIM, LBND2, UBND, I, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Check that IN and OUT have same dimensionality.
      IF ( NDIM1 .NE. NDIM2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FILLCUBE_E01', 'FILLCUBE: Error: Input and ' //
     :      'output dimensionality do not match.', STATUS )
         GO TO 500
      END IF

*  Locate Extensions.
      CALL SPD_EAAA( NDF(1), 'READ', XTHER1, XLOC1, STATUS )
      CALL SPD_EAAA( NDF(2), 'READ', XTHER2, XLOC2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  SPECAXIS must match.
      CALL SPD_EABA( NDF(1), XTHER1, SPAX1, STATUS )
      CALL SPD_EABA( NDF(2), XTHER2, SPAX2, STATUS )
      IF ( SPAX1 .NE. SPAX2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FILLCUBE_E02', 'FILLCUBE: Error: Input and ' //
     :      'output spectroscopic axis do not match.', STATUS )
         GO TO 500
      END IF

*  SPECVALS must not exist, neither in IN nor in OUT,
      IF ( XTHER1 ) THEN
         CALL DAT_THERE( XLOC1, XCMP6, THERE, STATUS )
         IF ( THERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FILLCUBE_E03', 'FILLCUBE: Error: Input ' //
     :         'has spectroscopic values in Specdre Extension.',
     :         STATUS )
            GO TO 500
         END IF
      END IF
      IF ( XTHER2 ) THEN
         CALL DAT_THERE( XLOC2, XCMP6, THERE, STATUS )
         IF ( THERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FILLCUBE_E04', 'FILLCUBE: Error: Output ' //
     :         'has spectroscopic values in Specdre Extension.',
     :         STATUS )
            GO TO 500
         END IF
      END IF

*  SPECWIDS, COVRS, RESULTS are ignored in IN, must not exist in OUT.
      IF ( XTHER2 ) THEN
         CALL DAT_THERE( XLOC2, XCMP7, THERE, STATUS )
         IF ( THERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FILLCUBE_E05', 'FILLCUBE: Error: Output ' //
     :         'has spectroscopic widths in Specdre Extension.',
     :         STATUS )
            GO TO 500
         END IF
         CALL DAT_THERE( XLOC2, XCMP8, THERE, STATUS )
         IF ( THERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FILLCUBE_E06', 'FILLCUBE: Error: Output ' //
     :         'has covariance row sums in Specdre Extension.', STATUS )
            GO TO 500
         END IF
         CALL DAT_THERE( XLOC2, XCMP9, THERE, STATUS )
         IF ( THERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FILLCUBE_E07', 'FILLCUBE: Error: Output ' //
     :         'has results in Specdre Extension.', STATUS )
            GO TO 500
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Start the informational output.
      IF ( INFO ) CALL MSG_OUT( 'FILLCUBE_M01',
     :      'The NDF pixel index transformations are:', STATUS )

*  Check and inter-relate axes.
      DO 1 I = 1, NDIM1

*     Find out data type necessary.
         CALL NDF_ATYPE( NDF(1), 'CENTRE', I, TYPE1, STATUS )
         CALL NDF_ATYPE( NDF(2), 'CENTRE', I, TYPE2, STATUS )

*     If we need to use _DOUBLE.
         IF ( TYPE1 .EQ. '_DOUBLE' .OR. TYPE2 .EQ. '_DOUBLE' ) THEN

*        Map input and output axis data.
            CALL NDF_AMAP( NDF(1), 'CENTRE', I, '_DOUBLE', 'READ',
     :         PNTR(1), NELM1, STATUS )
            CALL NDF_AMAP( NDF(2), 'CENTRE', I, '_DOUBLE', 'READ',
     :         PNTR(2), NELM2, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500

*        Check that either axis data array is linear.
            IF ( NELM1 .GT. 1 ) THEN
               CALL SPD_UAAHD( NELM1, %VAL( CNF_PVAL(PNTR(1)) ), 1D-5,
     :            LVAL1D, UVAL1D, LINEAR, STATUS )
               IF ( .NOT. LINEAR ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'FILLCUBE_T01', I )
                  CALL ERR_REP(  'FILLCUBE_E08', 'FILLCUBE: Error: ' //
     :               'Input has non-linear data for axis ' //
     :               '# ^FILLCUBE_T01.', STATUS )
                  GO TO 500
               END IF
            ELSE
               LVAL1D = SPD_UAAGD( %VAL( CNF_PVAL(PNTR(1)) ), 1,
     :                             STATUS )
               UVAL1D = LVAL1D
            END IF
            IF ( NELM2 .GT. 1 ) THEN
               CALL SPD_UAAHD( NELM2, %VAL( CNF_PVAL(PNTR(2)) ), 1D-5,
     :            LVAL2D, UVAL2D, LINEAR, STATUS )
               IF ( .NOT. LINEAR ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'FILLCUBE_T01', I )
                  CALL ERR_REP(  'FILLCUBE_E09', 'FILLCUBE: Error: ' //
     :               'Output has non-linear data for axis ' //
     :               '# ^FILLCUBE_T01.', STATUS )
                  GO TO 500
               END IF
            ELSE
               LVAL2D = SPD_UAAGD( %VAL( CNF_PVAL(PNTR(2)) ), 1,
     :                             STATUS )
               UVAL2D = LVAL2D
            END IF
            IF ( STATUS .NE. SAI__OK ) GO TO 500

*        Work out transform for Fortran array indices:
*        (OUTPIX - 1) = (LVAL1 - LVAL2) * (NELM2 - 1) / (UVAL2 - LVAL2)
*                     + (INPIX - 1)
*                     * (UVAL1 - LVAL1) / (UVAL2 - LVAL2)
*                     * (NELM2 - 1) / (NELM1 - 1)
*        (OUTPIX-1) = COEFF0 + COEFF1 * (INPIX-1).
*        If input or output has only one pixel the same grid spacing is
*        assumed for both. If neither has more than one pixel, they are
*        assumed to match.
            IF ( NELM1 .GT. 1 .AND. NELM2 .GT. 1 ) THEN
               COEFF0(I) = ( LVAL1D - LVAL2D ) / ( UVAL2D - LVAL2D )
     :                   * FLOAT( NELM2 - 1 )
               COEFF1(I) = ( UVAL1D - LVAL1D ) / ( UVAL2D - LVAL2D )
     :                   * FLOAT( NELM2 - 1 ) / FLOAT( NELM1 - 1 )
            ELSE IF ( NELM1 .GT. 1 ) THEN
               COEFF0(I) = ( LVAL1D - LVAL2D ) / ( UVAL1D - LVAL1D )
     :                   * FLOAT( NELM1 - 1 )
               COEFF1(I) = 1.
            ELSE IF ( NELM2 .GT. 1 ) THEN
               COEFF0(I) = ( LVAL1D - LVAL2D ) / ( UVAL2D - LVAL2D )
     :                   * FLOAT( NELM2 - 1 )
               COEFF1(I) = 1.
            ELSE
               COEFF0(I) = 0.
               COEFF1(I) = 1.
            END IF

*     Else (we use _REAL).
         ELSE

*        Map input and output axis data.
            CALL NDF_AMAP( NDF(1), 'CENTRE', I, '_REAL', 'READ',
     :         PNTR(1), NELM1, STATUS )
            CALL NDF_AMAP( NDF(2), 'CENTRE', I, '_REAL', 'READ',
     :         PNTR(2), NELM2, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500

*        Check that either axis data array is linear.
            IF ( NELM1 .GT. 1 ) THEN
               CALL SPD_UAAHR( NELM1, %VAL( CNF_PVAL(PNTR(1)) ), 1E-5,
     :            LVAL1R, UVAL1R, LINEAR, STATUS )
               IF ( .NOT. LINEAR ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'FILLCUBE_T01', I )
                  CALL ERR_REP(  'FILLCUBE_E08', 'FILLCUBE: Error: ' //
     :               'Input has non-linear data for axis ' //
     :               '# ^FILLCUBE_T01.', STATUS )
                  GO TO 500
               END IF
            ELSE
               LVAL1R = SPD_UAAGR( %VAL( CNF_PVAL(PNTR(1)) ), 1,
     :                             STATUS )
               UVAL1R = LVAL1R
            END IF
            IF ( NELM2 .GT. 1 ) THEN
               CALL SPD_UAAHR( NELM2, %VAL( CNF_PVAL(PNTR(2)) ), 1E-5,
     :            LVAL2R, UVAL2R, LINEAR, STATUS )
               IF ( .NOT. LINEAR ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'FILLCUBE_T01', I )
                  CALL ERR_REP(  'FILLCUBE_E09', 'FILLCUBE: Error: ' //
     :               'Output has non-linear data for axis ' //
     :               '# ^FILLCUBE_T01.', STATUS )
                  GO TO 500
               END IF
            ELSE
               LVAL2R = SPD_UAAGR( %VAL( CNF_PVAL(PNTR(2)) ), 1,
     :                             STATUS )
               UVAL2R = LVAL2R
            END IF
            IF ( STATUS .NE. SAI__OK ) GO TO 500

*        Work out transform for Fortran array indices.
            IF ( NELM1 .GT. 1 .AND. NELM2 .GT. 1 ) THEN
               COEFF0(I) = ( LVAL1R - LVAL2R ) / ( UVAL2R - LVAL2R )
     :                   * FLOAT( NELM2 - 1 )
               COEFF1(I) = ( UVAL1R - LVAL1R ) / ( UVAL2R - LVAL2R )
     :                   * FLOAT( NELM2 - 1 ) / FLOAT( NELM1 - 1 )
            ELSE IF ( NELM1 .GT. 1 ) THEN
               COEFF0(I) = ( LVAL1R - LVAL2R ) / ( UVAL1R - LVAL1R )
     :                   * FLOAT( NELM1 - 1 )
               COEFF1(I) = 1.
            ELSE IF ( NELM2 .GT. 1 ) THEN
               COEFF0(I) = ( LVAL1R - LVAL2R ) / ( UVAL2R - LVAL2R )
     :                   * FLOAT( NELM2 - 1 )
               COEFF1(I) = 1.
            ELSE
               COEFF0(I) = 0.
               COEFF1(I) = 1.
            END IF
         END IF

*     Inform the user about the pixel transforms.
         IF ( INFO ) THEN
            CALL MSG_SETI( 'AXIS', I )
            CALL MSG_SETR( 'COEFF0', COEFF0(I) )
            CALL MSG_SETR( 'COEFF1', COEFF1(I) )
            CALL MSG_SETI( 'LBND1', LBND1(I) )
            CALL MSG_SETI( 'LBND2', LBND2(I) )
            CALL MSG_OUT( 'FILLCUBE_M02', 'Axis # ^AXIS:  (Outpix ' //
     :         '- ^LBND2) = ^COEFF0 + ^COEFF1 * (Inpix - ^LBND1)',
     :         STATUS )
         END IF

*     Release axis data.
         CALL NDF_AUNMP( NDF(1), 'CENTRE', I, STATUS )
         CALL NDF_AUNMP( NDF(2), 'CENTRE', I, STATUS )
 1    CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Formal pixel transforms for the remaining axes.
      DO 2 I = NDIM1+1, NDF__MXDIM
         COEFF0(I) = 0.
         COEFF1(I) = 1.
 2    CONTINUE

*  Find out type necessary for main components.
      CALL NDF_TYPE( NDF(2), 'DATA,VARIANCE', TYPE2, STATUS )
      IF ( TYPE2 .NE. '_DOUBLE' ) TYPE2 = '_REAL'

*  Map data and variances.
      CALL NDF_STATE( NDF(1), 'VARIANCE', VTHER1, STATUS )
      CALL NDF_STATE( NDF(2), 'VARIANCE', VTHER2, STATUS )
      IF ( VTHER1 ) THEN
         CALL NDF_MAP( NDF(1), 'DATA,VARIANCE', TYPE2, 'READ',
     :      PNTR(1), NELM1, STATUS )
      ELSE
         CALL NDF_MAP( NDF(1), 'DATA', TYPE2, 'READ',
     :      PNTR(1), NELM1, STATUS )
      END IF
      IF ( VTHER2 ) THEN
         CALL NDF_MAP( NDF(2), 'DATA,VARIANCE', TYPE2, 'UPDATE',
     :      PNTR(3), NELM2, STATUS )
      ELSE
         CALL NDF_MAP( NDF(2), 'DATA', TYPE2, 'UPDATE',
     :      PNTR(3), NELM2, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Copy data and variances.
*  Anything is copied only if pixels match within TOL and input data are
*  non-bad in that pixel.
*  Variances are copied only if they exist in OUT. Should they exist in
*  OUT but not in IN, then bad values are copied into OUT's variance.
      IF ( TYPE2 .EQ. '_DOUBLE' ) THEN
         CALL SPD_WZQAD( VTHER1, VTHER2, NELM1, NELM2, DIM1, DIM2,
     :                   COEFF0, COEFF1, TOL, %VAL( CNF_PVAL(PNTR(1)) ),
     :                   %VAL( CNF_PVAL(PNTR(2)) ),
     :                   %VAL( CNF_PVAL(PNTR(3)) ),
     :                   %VAL( CNF_PVAL(PNTR(4)) ), STATUS )
      ELSE
         CALL SPD_WZQAR( VTHER1, VTHER2, NELM1, NELM2, DIM1, DIM2,
     :                   COEFF0, COEFF1, TOL, %VAL( CNF_PVAL(PNTR(1)) ),
     :                   %VAL( CNF_PVAL(PNTR(2)) ),
     :                   %VAL( CNF_PVAL(PNTR(3)) ),
     :                   %VAL( CNF_PVAL(PNTR(4)) ), STATUS )
      END IF

*  Tidy up.
 500  CONTINUE
      IF ( XTHER2 ) CALL DAT_ANNUL( XLOC2, STATUS )
      IF ( XTHER1 ) CALL DAT_ANNUL( XLOC1, STATUS )
      CALL NDF_ANNUL( NDF(2), STATUS )
      CALL NDF_ANNUL( NDF(1), STATUS )
      CALL NDF_END( STATUS )
      END
