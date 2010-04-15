      SUBROUTINE SPD_CAAC( NDFCUB, GRAPHI, VARUSE, SPVXST, COVRSX,
     :   PTRCX, PTRCD, PTRCV, PTRCC, PTRMSK,
     :   PTRW1, PTRW2, PTRW3, PTRW4, CUBDIM, STATUS )
*+
*  Name:
*     SPD_CAAC

*  Purpose:
*     Cube and workspace access for line fit routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CAAC( NDFCUB, GRAPHI, VARUSE, SPVXST, COVRSX,
*        PTRCX, PTRCD, PTRCV, PTRCC, PTRMSK,
*        PTRW1, PTRW2, PTRW3, PTRW4, CUBDIM, STATUS )

*  Description:
*     This routine accesses the input cube for FITGAUSS and other line
*     fit routines. This comprises mapping its data, variance,
*     spectroscopic values, and covariance row sums. The cube arrays are
*     mapped read-only. This routine also provides all the work spaces
*     needed by FITGAUSS etc.
*
*     If graphical interaction may occur later on, then this routine
*     will also work out the finder image (for actual cube
*     dimensionality 2 and 3 only) and register it in SPLOOP. The finder
*     image is stored in a work space. No pointer to this work space is
*     returned to the calling routine. Registering the finder image
*     means that it is available to SPLOOP routines anyway.
*
*     This routine accesses a number of NDFs - existing and temporary
*     ones. Since their identifiers are not returned - and sometimes not
*     even their pointers, the only way to get rid of these is to close
*     the NDF context they are in (by a call to NDF_END).

*  Arguments:
*     NDFCUB = INTEGER (Given)
*        The NDF identifier of the cube. The first non-degenerate axis
*        must be the spectroscopic axis.
*     GRAPHI = INTEGER (Given)
*        Non-zero if finder image to be worked out and registered.
*     VARUSE = INTEGER (Given and Returned)
*        If given zero any existing variance information is ignored. The
*        returned value is non-zero if variance was found and will be
*        used.
*     SPVXST = INTEGER (Returned)
*        Non-zero if and only if spectroscopic values are mapped from
*        the Specdre Extension and thus are an N-D array instead of a
*        vector of the length of the spectroscopic axis.
*     COVRSX = INTEGER (Returned)
*        Non-zero if covariance row sums are found and mapped. If VARUSE is
*        returned zero, covariance row sums are not looked for and
*        COVRSX is returned zero as well.
*     PTRC{XDVC} = INTEGER (Returned)
*        These are pointers to the mapped cube arrays. {XDVC} stands for
*        spectroscopic values, data, variances, and covariance row sums.
*        The cube arrays are input and read-only.
*     PTRMSK = INTEGER (Returned)
*        The pointer to the packed array of masked x, data, variances,
*        and covariance row sums. This is an uninitialised work space
*        with four times the length of the cube's spectroscopic axis.
*     PTRW{1234} = INTEGER (Returned)
*        The pointers to four further work spaces. Each work space has
*        the length of the cube's spectroscopic axis.
*     CUBDIM = INTEGER (Returned)
*        The actual dimensionality - ignoring degentate axes - of the
*        cube.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21 Apr 1994 (hme):
*        Original version.
*     04 May 1994 (hme):
*        Add the finder image handling.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDFCUB

*  Arguments Given and Returned:
      INTEGER GRAPHI
      INTEGER VARUSE

*  Arguments Returned:
      INTEGER SPVXST
      INTEGER COVRSX
      INTEGER PTRCX
      INTEGER PTRCD
      INTEGER PTRCV
      INTEGER PTRCC
      INTEGER PTRMSK
      INTEGER PTRW1
      INTEGER PTRW2
      INTEGER PTRW3
      INTEGER PTRW4
      INTEGER CUBDIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
*  NDF(1): covariance row sums
*  NDF(2): spectroscopic values, if from Specdre Extension
*  NDF(3): work space for packed masked arrays
*  NDF(4): four further work spaces
*  NDF(5): work space for finder image
      LOGICAL EXTXST             ! True if Extension exists
      LOGICAL VEXIST             ! True if variances available
      LOGICAL BADDAT, BADVAR     ! Unused
      INTEGER I, J               ! Temporary integers
      INTEGER PLACE              ! NDF placeholder
      INTEGER NDF( 5 )           ! NDF identifiers
      INTEGER NELM               ! Unused
      INTEGER DIM(  NDF__MXDIM ) ! NDF dimensions
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      INTEGER NDIM               ! NDF dimensionality
      INTEGER ADIM(  NDF__MXDIM ) ! Actual NDF dimensions
      INTEGER ALBND( NDF__MXDIM ) ! Actual NDF lower bounds
      INTEGER IMDIM( 3 )         ! {1,ADIM(2),ADIM(3)}
      INTEGER SPAXIS             ! Number of spectroscopic axis
      INTEGER PTRFI              ! Pointer to finder image
      INTEGER WORKR, WORKI       ! Pointer to work spaces
      REAL MINVAL, MAXVAL        ! Finder image extrema
      CHARACTER * ( 64 ) ALABEL  ! Unused
      CHARACTER * ( 64 ) AUNITS  ! Unused
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to Extension

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out cube's dimensions and bounds.
*  See if the Extension exists.
*  Find out the spectroscopic axis.
      CALL NDF_DIM(   NDFCUB, NDF__MXDIM, DIM,        NDIM, STATUS )
      CALL NDF_BOUND( NDFCUB, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      CALL SPD_EAAA(  NDFCUB, 'READ', EXTXST, XLOC, STATUS )
      CALL SPD_EABA(  NDFCUB, EXTXST, SPAXIS, STATUS )

*  Check that first non-degenerate axis is the spectroscopic axis.
      IF ( SPAXIS .NE. 1 ) THEN
         DO 1 I = SPAXIS, 1, -1
            IF ( DIM(I) .NE. 1 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_CAAC_E01', 'SPD_CAAC: Error: ' //
     :            'Spectroscopic axis is not first non-degenerate ' //
     :            'axis.', STATUS )
               GO TO 500
            END IF
 1       CONTINUE
      ELSE IF ( DIM(SPAXIS) .EQ. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_CAAC_E02', 'SPD_CAAC: Error: ' //
     :      'Spectroscopic axis is degenerate.', STATUS )
         GO TO 500
      END IF

*  Access and map pixel centres, wherever they are (Extension or axis
*  structure).
      CALL SPD_EAEA( NDFCUB, XLOC, SPAXIS, 'READ', '_REAL',
     :   ALABEL, AUNITS, PTRCX, NDF(2), NELM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( XLOC, STATUS )
         GO TO 500
      END IF
      IF ( NDF(2) .EQ. NDF__NOID ) THEN
         SPVXST = 0
      ELSE
         SPVXST = 1
      END IF

*  See if variances exist.
*  If variance absent although we want to use it, change our VARUSE flag.
      CALL NDF_STATE( NDFCUB, 'VARIANCE', VEXIST, STATUS )
      IF ( VARUSE .NE. 0 .AND. .NOT. VEXIST ) VARUSE = 0

*  Try to access the covariance row sums. But recover from failure.
      IF ( EXTXST .AND. VARUSE .NE. 0 ) THEN
         CALL ERR_MARK
            CALL SPD_EAGD( NDFCUB, XLOC, 'READ', '_REAL',
     :         PTRCC, NDF(1), NELM, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               COVRSX = 1
            ELSE
               COVRSX = 0
               CALL ERR_ANNUL( STATUS )
            END IF
         CALL ERR_RLSE
      ELSE
         COVRSX = 0
      END IF

*  Release the Extension locator.
      IF ( EXTXST ) CALL DAT_ANNUL( XLOC, STATUS )

*  Map cube data and variances.
      CALL NDF_MAP( NDFCUB, 'DATA', '_REAL', 'READ',
     :   PTRCD, NELM, STATUS )
      IF ( VARUSE .NE. 0 )
     :   CALL NDF_MAP( NDFCUB, 'VARIANCE', '_REAL', 'READ',
     :      PTRCV, NELM, STATUS )

*  Check status.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get a work space for masking.
*  The data component is used for masked centres, data, weights.
*  which are packed into one array. This array is _REAL.
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( '_REAL', 1, 1, 4*DIM(SPAXIS), PLACE,
     :              NDF(3), STATUS )
      CALL NDF_MAP( NDF(3), 'DATA', '_REAL', 'WRITE',
     :              PTRMSK, I, STATUS )

*  Get four more work spaces. All components will be usurped for
*  different things.
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( '_REAL', 1, 1, DIM(SPAXIS), PLACE, NDF(4), STATUS )
      CALL NDF_AMAP( NDF(4), 'CENTRE', 1, '_REAL', 'WRITE',
     :               PTRW1, NELM, STATUS )
      CALL NDF_AMAP( NDF(4), 'WIDTH',  1, '_REAL', 'WRITE',
     :              PTRW2, NELM, STATUS )
      CALL NDF_MAP( NDF(4), 'DATA',      '_REAL', 'WRITE',
     :              PTRW3, NELM, STATUS )
      CALL NDF_MAP( NDF(4), 'VARIANCE',  '_REAL', 'WRITE',
     :              PTRW4, NELM, STATUS )

*  Work out the actual dimensionality and dimensions.
      CUBDIM = 0
      DO 2 I = 1, NDIM
         IF ( DIM(I) .NE. 1 ) THEN
            CUBDIM = CUBDIM + 1
            ADIM(CUBDIM)  = DIM(I)
            ALBND(CUBDIM) = LBND(I)
         END IF
 2    CONTINUE

*  If graphics interaction expected.
      IF ( GRAPHI .NE. 0 ) THEN

*     If dimensionality is 2.
         IF ( CUBDIM .EQ. 2 ) THEN

*        Get work space.
            CALL NDF_TEMP( PLACE, STATUS )
            CALL NDF_NEW( '_REAL', 1, 1, ADIM(1)*ADIM(2), PLACE,
     :                    NDF(5), STATUS )
            CALL NDF_MAP( NDF(5), 'DATA', '_REAL', 'WRITE',
     :                    PTRFI, I, STATUS )

*        Copy data to work space.
            CALL VEC_RTOR( .FALSE., ADIM(1)*ADIM(2),
     :                     %VAL( CNF_PVAL( PTRCD ) ),
     :                     %VAL( CNF_PVAL( PTRFI ) ), I, J, STATUS )

*        Work out extrema of work space.
            CALL SPD_UAAAR( .TRUE., ADIM(1)*ADIM(2),
     :                      %VAL( CNF_PVAL( PTRFI ) ), MINVAL, MAXVAL,
     :                      STATUS )

*        Replace bad values with 2*min-max in work space.
            CALL SPD_UAABR( ADIM(1)*ADIM(2),
     :                      2*MINVAL-MAXVAL, %VAL( CNF_PVAL( PTRFI ) ),
     :                      STATUS )

*        Register work space as finder image.
            CALL SPD_PDAA( PTRFI, ADIM(1), ADIM(2), ALBND(1), ALBND(2),
     :                     STATUS )

*     Else if dimensionality is 3.
         ELSE IF ( CUBDIM .EQ. 3 ) THEN

*        Get work spaces.
            CALL NDF_TEMP( PLACE, STATUS )
            CALL NDF_NEW( '_REAL', 1, 1, ADIM(2)*ADIM(3), PLACE,
     :                    NDF(5), STATUS )
            CALL NDF_ASTYP( '_INTEGER', NDF(5), 'CENTRE', 1, STATUS )
            CALL NDF_MAP( NDF(5), 'DATA', '_REAL', 'WRITE',
     :                    PTRFI, I, STATUS )
            CALL NDF_MAP( NDF(5), 'VARIANCE', '_REAL', 'WRITE',
     :                     WORKR, I, STATUS )
            CALL NDF_AMAP( NDF(5), 'CENTRE', 1, '_INTEGER', 'WRITE',
     :                     WORKI, I, STATUS )

*        Average cube along first axis, result into work space.
            IMDIM(1) = 1
            IMDIM(2) = ADIM(2)
            IMDIM(3) = ADIM(3)
            CALL SPD_UAACR( .FALSE., 3, ADIM(1)*ADIM(2)*ADIM(3),
     :                      ADIM(2)*ADIM(3), VAL__BADR, ADIM, IMDIM,
     :                      %VAL( CNF_PVAL( PTRCD ) ), 0., BADDAT,
     :                      BADVAR, %VAL( CNF_PVAL( PTRFI ) ),
     :                      %VAL( CNF_PVAL( WORKR ) ),
     :                      %VAL( CNF_PVAL( WORKI ) ), STATUS )

*        Release work spaces other than the finder image.
            CALL NDF_AUNMP( NDF(5), 'CENTRE', 1, STATUS )
            CALL NDF_UNMAP( NDF(5), 'VARIANCE',  STATUS )

*        Work out extrema of work space.
            CALL SPD_UAAAR( .TRUE., ADIM(2)*ADIM(3),
     :                      %VAL( CNF_PVAL( PTRFI ) ), MINVAL,
     :                      MAXVAL, STATUS )

*        Replace bad values with 2*min-max in work space.
            CALL SPD_UAABR( ADIM(2)*ADIM(3), 2*MINVAL-MAXVAL,
     :                      %VAL( CNF_PVAL( PTRFI ) ), STATUS )

*        Register work space as finder image.
            CALL SPD_PDAA( PTRFI, ADIM(2), ADIM(3), ALBND(2), ALBND(3),
     :                     STATUS )

         END IF

      END IF

*  Return.
 500  CONTINUE
      END
