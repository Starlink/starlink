      SUBROUTINE ARCIDENT( STATUS )
*+
*  Name:
*     ARCIDENT

*  Purpose:
*     Auto-identify located features.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ARCIDENT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine identifies located features in a set of spectra.
*     Auto-identification is done from scratch (without prior
*     identification of any features) with the algorithm by Mills
*     (1992).
*
*     The input data must be a base NDF. They can be a single spectrum
*     or a set of spectra. Examples for the latter are a long slit
*     spectrum, a set of extracted fibre spectra, or a collapsed
*     echellogram (a set of extracted orders from an echelle
*     spectrograph). It is necessary that the spectroscopic axis be the
*     first axis in the data set. It does not matter how many further
*     axes there are, the data will be treated as a linear set of rows
*     with each row a spectrum.
*
*     The features for which an identification should be attempted must
*     have been located. That is, they must be components of type
*     'Gauss', 'triangle', 'Gauss feature' or 'triangle feature' in the
*     results structure of the Specdre Extension. Each of these
*     components must have at least a 'centre' and 'peak' parameter. The
*     centres (feature locations) must be a strictly monotonically
*     increasing list. Their variances must be available. The locations
*     (centre parameters) must be in terms of NDF pixel coordinates. The
*     peaks must be positive. They are used as a measure of the
*     importance of a feature.
*
*     The coverage in spectroscopic values of all spectra (rows) should
*     either be similar (long slit or fibres) or roughly adjacent
*     (echellogram). There must not yet be any spectroscopic value
*     information: There must be no array of spectroscopic values or
*     widths in the Specdre Extension. The pixel centre array for the
*     spectroscopic axis (i.e. the first axis) must be NDF pixel
*     coordinates (usually 0.5, 1.5, ...). The data must be arranged
*     such that spectroscopic values increase left to right. In the case
*     of rows with adjacent coverage spectroscopic values must also
*     increase with row number. In a collapsed echellogram this usually
*     means that for wavelength calibration the order number must
*     decrease with increasing row number. If this is not the case then
*     it is still possible to work on a collapsed echellogram: You can
*     set ECHELLE false and thus use the full WRANGE for each row, but
*     you must adjust DRANGE to be a more reasonable guess of the
*     dispersion.
*
*     Identification is done by comparison with a feature data base
*     according to Mills (1992). The feature data base should to some
*     degree match the observation. Its spectral extent should be wider
*     than that of the observation. But it should not contain a
*     significant number of features that are not located. This is
*     because the automatic identification algorithm uses relative
*     distances between neighbouring features. If most neighbours in the
*     list of laboratory values are not detected in the actual arc
*     observation, then the algorithm may fail to find a solution or may
*     return the wrong solution.
*
*     This routine works on each row (spectrum) in turn. It establishes
*     information about relative distances between neighbouring located
*     features and compares this with a feature data base. This serves
*     to identify at least a specified number of features. An
*     auto-identification should always be checked in the process of
*     fitting a polynomial dispersion curve. All located features are
*     retained by this routine, so that further identifications can be
*     added or some identifications can be cancelled.
*
*     The result of this routine is a list of feature identifications.
*     All located features are retained, though some will have not been
*     identified. The locations and identifications (pixel coordinates
*     and laboratory values) are stored in the results structure of the
*     Specdre Extension of the input data. This replaces the
*     pre-existing results extension. The locations are strictly
*     monotonically increasing, as are in all probability the
*     identifications.
*
*     The new results structure provides for as many component as the
*     old one had components of any recognised type. Each component has
*     on output the type 'arc feature'. It has two parameters 'centre'
*     and 'laboratory value'. Located but unidentified features will
*     have bad values as laboratory values. The variances of laboratory
*     values are set to zero.
*
*     Mills's (1992) algorithm performs only an initial line
*     identification. It is important to verify the returned values by
*     fitting a wavelength or frequency scale (e.g. polynomial or spline
*     fit), and to reject any out-liers. The algorithm should be given
*     the positions of all conceivable features in the spectra. It does
*     not use the fainter ones unless it is unable to identify using
*     only the brightest, but you will get more robust behaviour if you
*     always provide all possible candidate lines for potential
*     identification. The algorithm should not be fed severely blended
*     line positions as the chance of incorrect identifications will be
*     significantly higher (this is the exception to the rule above).
*
*     The speed of the algorithm varies approximately linearly with
*     wavelength/frequency range and also with dispersion range so the
*     better constraints you provide the faster it will run. The
*     algorithm takes your constraints as hard limits and it is usually
*     more robust to accept a slightly longer runtime by relaxing the
*     ranges a little.
*
*     If the algorithm runs and keeps looping increasing its set of
*     neighbours, then the most likely causes are as follows:
*     -  wavelength/frequency scale does not increase with increasing x
*        (set the CHKRVS parameter true and try again).
*     -  WRANGE or DRANGE are too small (increase them both by
*        a factor of 2 and try again).

*  Usage:
*     arcident in out fdb wrange=?

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        If false, the routine will issue only error messages and no
*        informational messages. [YES]
*     ECHELLE = _LOGICAL (Read)
*        If false, the given WRANGE is used for each row, assuming the
*        rows are similar spectra (long slit or fibre). If true, a
*        collapsed echellogram is assumed. In that case each row is an
*        extracted order with different wavelength/frequency range. This
*        routine will divide the given WRANGE into as many sub-ranges as
*        there are rows (orders) in the given input. [NO]
*     IN = NDF (Read)
*        The spectrum or set of spectra in which located features are to
*        be identified. This must be a base NDF, the spectroscopic axis
*        must be the first axis. No spectroscopic values or widths must
*        exist in the Specdre Extension. The pixel centres along the
*        first axis must be NDF pixel coordinates. The input NDF must
*        have a results structure in its Specdre Extension, and the
*        results must contain a number of line components with strictly
*        monotonically increasing position (centre).
*     OUT = NDF (Read)
*        The output NDF is a copy of the input, except that the results
*        structure holds feature identifications rather than locations
*        ('peak' parameters will have been replaced with 'laboratory
*        value' parameters).
*     FDB = NDF (Read)
*        The feature data base. The actual data base is a set of
*        primitive arrays in an extension to this NDF called ECHELLE.
*        A feature data base can be generated from a list of wavelengths
*        or frequencies with ARCGENDB.
*     WRANGE( 2 ) = _REAL (Read)
*        The approximate range of wavelengths or frequencies. The
*        narrower this range the faster is the identification algorithm.
*        But if in doubt give a wider range.
*     DRANGE( 2 ) = _REAL (Read)
*        The range into which the dispersion in pixels per wavelength or
*        per frequency falls. The narrower this range the faster is the
*        identification algorithm. But if in doubt give a wider range.
*     STRENGTH = _REAL (Read)
*        This specifies the maximum ratio between the strength of
*        features that are to be used initially for identification. If
*        the strongest feature has peak 1000, then the weakest
*        feature used initially has peak greater than 1000/STRENGTH.
*        [50.0]
*     THRESH = _REAL (Read)
*        This specifies the maximum difference between the ratios of
*        neighbour distances as observed and as found in the feature
*        data base. The difference is evaluated as
*           ABS(1 - ABS(obs/ref)) <? THRESH.
*        Values much larger than 0.1 are likely to generate a lot of
*        coincidence matches; values less than 0.01 may well miss 'good'
*        matches in less-than-ideal data. You may need to relax this
*        parameter if your arc spectra are very distorted (non-linear
*        dispersion). [0.03]
*     MAXLOC = _INTEGER (Read)
*        This specifies the maximum number of features to be used when
*        generating ratios for initial identification. In general, a
*        good solution can be found using only the strongest 8 to 16
*        features. The program slowly increases the number of features
*        it uses until an adequate solution if found. However, there may
*        be a large numbers of weak features present which are not in
*        the reference database. This parameter allows the setting of an
*        absolute maximum on the number of features (per row) which
*        are to be considered. If less than MAXLOC features are located
*        in a given row, then the number of identified features is used
*        instead for that row. [30]
*     MINIDS = _INTEGER (Read)
*        The minimum number of features that must be identified for the
*        algorithm to be successful. If fewer than MINIDS features are
*        located in a given row, then a smaller number is used instead
*        for that row. [9]
*     NEIGHB( 2 ) = _INTEGER (Read)
*        NEIGHB(1) specifies the starting number of neighbouring
*        features (on each side) to examine when generating ratios for
*        matching. (These are neighbours in the observed spectra, not in
*        the feature data base.) Increasing this will lead to
*        exponential increases in CPU time, so it should be used with
*        caution when all else fails. The default value is 3. Higher
*        values are tried automatically by the program if no solution
*        can be found. The number of neighbours considered is increased
*        until it reaches the maximum of NEIGHB(2), when the program
*        gives up. [3,6]

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  References:
*     Mills, D., 1992, Automatic ARC wavelength calibration, in P.J.
*     Grosbol, R.C.E. de Ruijsscher (eds), 4th ESO/ST-ECF Data
*     Analysis Workshop, Garching, 13 - 14 May 1992, ESO Conference and
*     Workshop Proceedings No. 41, Garching bei Muenchen, 1992

*  Authors:
*     djm: Dave Mills (UCL)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11 Jun 1993 (hme):
*        Original version.
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

*  Local Constants:
      INTEGER XCLEN              ! Specdre Extension string length
      PARAMETER ( XCLEN = 32 )

*  Local Variables:
*  NDF(1): Input NDF.
*  NDF(2): Output NDF.
*  NDF(3): Input results NDF.
*  NDF(4): Output results NDF.
*  NDF(5): Feature data base.
*  NDF(6): 1st workspace.
*  NDF(7): 2nd to 6th workspace.
*  NELM(1): Size of input NDF.
*  NELM(2...4): Size of input result NDF, input result vectors.
*  NELM(5...7): Size of output result NDF, output result vectors.
*  LOC(1): Input Specdre Extension.
*  LOC(2...7): Input component related vectors.
*  LOC(8): Input parameter related vector.
*  LOC(9): Output Specdre Extension.
*  LOC(10...15): Output component related vectors.
*  LOC(16): Output parameter related vector.
*  LOC(17...24): FDB echelle extension and vectors therein.
*  IPNTR(1): Input results data.
*  IPNTR(2): Input results variance.
*  IPNTR(3...8): Component related vectors.
*  IPNTR(9): Parameter related vector.
*  IPNTR(10): Input axis array.
*  OPNTR(1): Output results data.
*  OPNTR(2): Output results variance.
*  OPNTR(3...8): Component related vectors.
*  OPNTR(9): Parameter related vector.
*  FPNTR(1...7): FTR_WAVE, FTR_DB, FTR_LEFT, FTR_RIGHT, WAVE_INDEX,
*     QUICK_INDEX, QENTRIES.
*  WPNTR(1): I-th located centre is %VAL(IPNTR(1))(%VAL(WPNTR(1))(I)).
*  WPNTR(2): located non-bad centres for a row.
*  WPNTR(3): located non-bad peaks for a row.
*  WPNTR(4): located non-bad centre variances for a row.
*  WPTNR(5): identified locations.
*  WPNTR(6): identifications (laboratory values).
      LOGICAL INFO
      LOGICAL ECHELL
      REAL WRANGE( 2 )
      REAL DRANGE( 2 )
      REAL STREN
      REAL THRESH
      INTEGER MAXLOC
      INTEGER MINIDS
      INTEGER NEIGHB( 2 )
      LOGICAL ISBAS              ! True if NDF is base NDF
      LOGICAL THERE              ! True if an HDS component exists
      LOGICAL LINEAR             ! True if array is linear
      INTEGER I                  ! Counter of spectra (rows)
      INTEGER MONO, FIRST        ! Used for monotonicity check
      INTEGER FDBSTA             ! FDB status
      INTEGER MAXPOS             ! Local max_positions_to_use
      INTEGER MINPOS             ! Local min_solution_size
      INTEGER PLACE( 2 )         ! NDF place holder
      INTEGER NDF( 7 )           ! NDF identifiers
      INTEGER NELM( 7 )          ! Array sizes
      INTEGER NDIM               ! NDF dimensionality
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER LBND( NDF__MXDIM ) ! Input NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! Input NDF upper bounds
      INTEGER NROWS              ! How many rows in input NDF
      INTEGER INDXSZ             ! Size of FDB index
      INTEGER DBDIM( 3 )         ! Dimensions of FDB
      INTEGER IPNTR( 10 )        ! Input array pointers
      INTEGER OPNTR( 9 )         ! Output array pointers
      INTEGER FPNTR( 7 )         ! FDB array pointers
      INTEGER WPNTR( 6 )         ! Work array pointers
      INTEGER NCOMP1             ! Number of components in input results
      INTEGER TNPAR1             ! Number of parameters in input results
      INTEGER NCOMP2             ! Number of comps in output results
      INTEGER TNPAR2             ! Number of params in output results
      INTEGER NCOMP3             ! Number of non-bad line components
      INTEGER NCOMP4             ! Number of identified components
      INTEGER COMP( 2 )          ! Component range to be mapped
      REAL TEMP                  ! Buffer variable
      REAL XSTART, XEND          ! Axis array range
      REAL STARTW                ! Lab value range start (per row)
      REAL ENDW                  ! Lab value range end (per row)
      CHARACTER * ( DAT__SZTYP ) TYPE3( 3 ) ! Types used in results
      CHARACTER * ( DAT__SZLOC ) LOC( 24 ) ! HDS locators

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN

*  Modal parameters.
      CALL PAR_GET0L( 'INFO',    INFO,   STATUS )
      CALL PAR_GET0L( 'ECHELLE', ECHELL, STATUS )

*  Get input.
      CALL NDF_ASSOC( 'IN', 'READ', NDF(1), STATUS )
      CALL NDF_SIZE( NDF(1), NELM(1), STATUS )
      CALL NDF_DIM( NDF(1), NDF__MXDIM, DIM, NDIM, STATUS )
      CALL NDF_BOUND( NDF(1), NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      NROWS = NELM(1) / DIM(1)

*  Check input.
*  As a side effect the Specdre Extension is accessed for read.
      CONTINUE

*     Must be base NDF.
         CALL NDF_ISBAS( NDF(1), ISBAS, STATUS )
         IF ( .NOT. ISBAS ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCIDENT_E01', 'ARCIDENT: Error ' //
     :         'accessing input: The NDF is not a base NDF.', STATUS )
            GO TO 500
         END IF

*     Spectroscopic axis must be first.
         CALL SPD_EAAA( NDF(1), 'READ', THERE, LOC(1), STATUS )
         CALL SPD_EABA( NDF(1), THERE, I, STATUS )
         IF ( I .NE. 1 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCIDENT_E02', 'ARCIDENT: Error ' //
     :         'accessing input: The spectroscopic axis is not the ' //
     :         'first axis.', STATUS )
            GO TO 500
         END IF

*     Must have Extension.
         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCIDENT_E03', 'ARCIDENT: Error ' //
     :         'accessing input: There is no Specdre Extension.',
     :         STATUS )
            GO TO 500
         END IF

*     Must have neither SPECVALS nor SPECWIDS.
         IF ( THERE ) THEN
            CALL DAT_THERE( LOC(1), XCMP6, THERE, STATUS )
            IF ( THERE ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ARCIDENT_E04', 'ARCIDENT: Error ' //
     :            'accessing input: There are spectroscopic values ' //
     :            'in the Specdre Extension.', STATUS )
               GO TO 500
            END IF
            CALL DAT_THERE( LOC(1), XCMP7, THERE, STATUS )
            IF ( THERE ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ARCIDENT_E05', 'ARCIDENT: Error ' //
     :            'accessing input: There are spectroscopic widths ' //
     :            'in the Specdre Extension.', STATUS )
               GO TO 500
            END IF
         END IF

*     Axis data must be NDF pixel coordinates.
         CALL NDF_AMAP( NDF(1), 'CENTRE', 1, '_REAL', 'READ',
     :      IPNTR(10), I, STATUS )
         CALL SPD_UAAHR( I, %VAL( CNF_PVAL(IPNTR(10)) ), 1E-5, XSTART,
     :                   XEND, LINEAR, STATUS )
         IF ( .NOT. LINEAR .OR.
     :         XSTART .NE. LBND(1)-0.5 .OR. XEND .NE. UBND(1)-0.5 ) THEN
            CALL MSG_SETR( 'ARCIDENT_T01', LBND(1)-0.5 )
            CALL MSG_SETR( 'ARCIDENT_T02', UBND(1)-0.5 )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCIDENT_E06', 'ARCLOCAT: Error ' //
     :         'accessing input: The first axis'' coordinates are ' //
     :         'not default NDF pixel coordinates. They should run ' //
     :         'linearly from ^ARCIDENT_T01 to ^ARCIDENT_T02.', STATUS )
            GO TO 500
         END IF
         CALL NDF_AUNMP( NDF(1), 'CENTRE', 1, STATUS )

*  Investigate the input results.
*  This includes getting all the workspaces we need.
      CONTINUE

*     Access the input results.
         CALL SPD_FDHA( NDF(1), LOC(1), NCOMP1, TNPAR1, TYPE3, STATUS )

*     Get all the workspaces we need.
         CALL NDF_TEMP( PLACE(1), STATUS )
         CALL NDF_NEW( '_INTEGER', 1, 1, NCOMP1, PLACE(1),
     :      NDF(6), STATUS )
         CALL NDF_MAP( NDF(6), 'DATA', '_INTEGER', 'WRITE',
     :      WPNTR(1), I, STATUS )
         CALL NDF_TEMP( PLACE(2), STATUS )
         CALL NDF_NEW( '_REAL', 1, 1, NCOMP1, PLACE(2),
     :      NDF(7), STATUS )
         CALL NDF_MAP( NDF(7), 'DATA,VARIANCE', '_REAL', 'WRITE',
     :      WPNTR(2), I, STATUS )
         CALL NDF_AMAP( NDF(7), 'CENTRE,WIDTH,VARIANCE', 1, '_REAL',
     :      'WRITE', WPNTR(4), I, STATUS )

*     Map input results.
         COMP(1) = 1
         COMP(2) = NCOMP1
         TYPE3(1) = '_REAL'
         TYPE3(2) = '_REAL'
         TYPE3(3) = '_REAL'
         CALL SPD_FDHE( NDF(1), LOC(1), 'READ', TYPE3, COMP, NDF(3),
     :      LOC(2), LOC(8), IPNTR(1), IPNTR(3), IPNTR(9), NELM(2),
     :      STATUS )

*     Work out which input components are line features.
*     This sets a vector LOCAT so that the I-th location is the
*     DATA(LOCAT(I)) in the input result data. The I-th peak is the next
*     element. It also finds out how many located components there are,
*     which is NCOMP2 and used to create the output results.
*     This does not check whether component's parameters are bad,
*     because the action here is an action for all spectra.
         CALL SPD_WZKH( NCOMP1, TNPAR1, %VAL( CNF_PVAL(IPNTR(6)) ),
     :                  %VAL( CNF_PVAL(IPNTR(5)) ),
     :                  %VAL( CNF_PVAL(IPNTR(9)) ), NCOMP2,
     :                  %VAL( CNF_PVAL(WPNTR(1)) ), STATUS,
     :                  %VAL(CNF_CVAL(XCLEN)), %VAL(CNF_CVAL(XCLEN)) )

*     Release input results extension vectors.
         DO 1 I = 2, 8
            CALL DAT_ANNUL( LOC(I), STATUS )
 1       CONTINUE

*  Create the output NDF by propagation, copying everything.
      CALL NDF_PROP( NDF(1), 'UNITS,DATA,VARIANCE,AXIS', 'OUT', NDF(2),
     :   STATUS )

*  Re-create output results, keep data and variance mapped.
*  SPD_WZKJ sets all comptypes to 'arc feature' and the paratypes to
*  'centre' and 'laboratory value'.
      CALL SPD_EAAA( NDF(2), 'UPDATE', THERE, LOC(9), STATUS )
      CALL DAT_ERASE( LOC(9), XCMP9, STATUS )
      TNPAR2 = 2 * NCOMP2
      TYPE3(1) = '_REAL'
      TYPE3(2) = '_REAL'
      TYPE3(3) = '_REAL'
      COMP(1) = 1
      COMP(2) = NCOMP2
      CALL SPD_FDHF( NDF(2), LOC(9), NCOMP2, TNPAR2, TYPE3, STATUS )
      CALL SPD_FDHE( NDF(2), LOC(9), 'UPDATE', TYPE3, COMP, NDF(4),
     :   LOC(10), LOC(16), OPNTR(1), OPNTR(3), OPNTR(9), NELM(5),
     :   STATUS )
      CALL SPD_WZKJ( NELM(6), NELM(7), %VAL( CNF_PVAL(OPNTR(5)) ),
     :               %VAL( CNF_PVAL(OPNTR(9)) ),
     :               STATUS, %VAL(CNF_CVAL(XCLEN)),
     :               %VAL(CNF_CVAL(XCLEN)) )
      DO 2 I = 10, 16
         CALL DAT_ANNUL( LOC(I), STATUS )
 2    CONTINUE

*  Access and map the feature data base.
      CALL NDF_ASSOC( 'FDB', 'READ', NDF(5), STATUS )
      CALL NDF_XLOC( NDF(5), 'ECHELLE', 'READ', LOC(17), STATUS )
      CALL DAT_FIND( LOC(17), 'FTR_WAVE',    LOC(18), STATUS )
      CALL DAT_FIND( LOC(17), 'FTR_DB',      LOC(19), STATUS )
      CALL DAT_FIND( LOC(17), 'FTR_LEFT',    LOC(20), STATUS )
      CALL DAT_FIND( LOC(17), 'FTR_RIGHT',   LOC(21), STATUS )
      CALL DAT_FIND( LOC(17), 'WAVE_INDEX',  LOC(22), STATUS )
      CALL DAT_FIND( LOC(17), 'QUICK_INDEX', LOC(23), STATUS )
      CALL DAT_FIND( LOC(17), 'QENTRIES',    LOC(24), STATUS )
      CALL DAT_SHAPE( LOC(19), 3, DBDIM,  NDIM, STATUS )
      CALL DAT_SHAPE( LOC(23), 1, INDXSZ, NDIM, STATUS )
      IF ( DBDIM(1) .NE. DBDIM(2) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCIDENT_E07', 'ARCIDENT: Error accessing ' //
     :      'feature data base. Scope is not unique.', STATUS )
      END IF
      CALL DAT_MAP( LOC(18),'_REAL', 'READ',1,DBDIM(3),FPNTR(1),STATUS )
      CALL DAT_MAP( LOC(19),'_REAL', 'READ',3,DBDIM,  FPNTR(2), STATUS )
      CALL DAT_MAP( LOC(20),'_BYTE', 'READ',3,DBDIM,  FPNTR(3), STATUS )
      CALL DAT_MAP( LOC(21),'_BYTE', 'READ',3,DBDIM,  FPNTR(4), STATUS )
      CALL DAT_MAP( LOC(22),'_UWORD','READ',3,DBDIM,  FPNTR(5), STATUS )
      CALL DAT_MAP( LOC(23),'_INTEGER','READ',1,INDXSZ,FPNTR(6),STATUS )
      CALL DAT_MAP( LOC(24),'_REAL', 'READ',1,INDXSZ, FPNTR(7), STATUS )

*  Ask what wavelength range.
      CALL PAR_GET1R( 'WRANGE', 2, WRANGE, I, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( I .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCIDENT_E08', 'ARCIDENT: Error getting ' //
     :      'expected range of laboratory values. 2 values must be ' //
     :      'given.', STATUS )
         GO TO 500
      ELSE IF ( WRANGE(1) .EQ. WRANGE(2) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCIDENT_E09', 'ARCIDENT: Error getting ' //
     :      'expected range of laboratory values. The 2 values must ' //
     :      'not be equal.', STATUS )
         GO TO 500
      END IF
      TEMP      = MIN( WRANGE(1), WRANGE(2) )
      WRANGE(2) = MAX( WRANGE(1), WRANGE(2) )
      WRANGE(1) = TEMP

*  Default dispersion range.
      IF ( ECHELL ) THEN
         DRANGE(1) = ( XEND - XSTART ) / ( WRANGE(2) - WRANGE(1) )
     :             * FLOAT(NROWS+1) / 4.
         DRANGE(2) = ( XEND - XSTART ) / ( WRANGE(2) - WRANGE(1) )
     :             * FLOAT(2*NROWS)
      ELSE
         TEMP = ( XEND - XSTART ) / ( WRANGE(2) - WRANGE(1) )
         DRANGE(1) = TEMP / 2.
         DRANGE(2) = TEMP * 2.
      END IF
      CALL PAR_DEF1R( 'DRANGE', 2, DRANGE, STATUS )

*  Ask for the expected dispersion.
      CALL PAR_GET1R( 'DRANGE', 2, DRANGE, I, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( I .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCIDENT_E10', 'ARCIDENT: Error getting ' //
     :      'range of expected dispersion. 2 values must be ' //
     :      'given.', STATUS )
         GO TO 500
      ELSE IF ( DRANGE(1) .EQ. DRANGE(2) .OR.
     :          DRANGE(1) * DRANGE(2) .EQ. 0. ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCIDENT_E11', 'ARCIDENT: Error getting ' //
     :      'expected range of laboratory values. The 2 values must ' //
     :      'not be equal, nor equal to zero.', STATUS )
         GO TO 500
      END IF

*  Further parameters.
      CALL PAR_GET0R( 'STRENGTH',  STREN,  STATUS )
      CALL PAR_GET0R( 'THRESH',    THRESH, STATUS )
      CALL PAR_GET0I( 'MAXLOC',    MAXLOC, STATUS )
      CALL PAR_GET0I( 'MINIDS',    MINIDS, STATUS )
      CALL PAR_GET1I( 'NEIGHB', 2, NEIGHB, I, STATUS )
      IF ( I .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCIDENT_E12', 'ARCIDENT: Error getting ' //
     :      'min/max no. of neighbours. 2 values must be ' //
     :      'given.', STATUS )
         GO TO 500
      ELSE IF ( NEIGHB(1) .LT. 1 .OR. NEIGHB(2) .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCIDENT_E13', 'ARCIDENT: Error getting ' //
     :      'min/max no. of neighbours. Values must be ' //
     :      'positive.', STATUS )
         GO TO 500
      END IF
      I         = MIN( NEIGHB(1), NEIGHB(2) )
      NEIGHB(2) = MAX( NEIGHB(1), NEIGHB(2) )
      NEIGHB(1) = I

*  For each spectrum.
      DO 3 I = 1, NROWS

*     Sort from the input results to the workspaces.
*     Only Gauss and triangle components make it, only their centre and
*     peak are copied, and the centre variance.
*     Take away features with bad-valued parameters, so only NCOMP3
*     remain.
*     The locations go into one array, the peaks into another, the
*     centre variances into a third.
         CALL SPD_WZKK( I, TNPAR1, NCOMP2, %VAL( CNF_PVAL(WPNTR(1)) ),
     :                  %VAL( CNF_PVAL(IPNTR(1)) ),
     :                  %VAL( CNF_PVAL(IPNTR(2)) ), NCOMP3,
     :                  %VAL( CNF_PVAL(WPNTR(2)) ),
     :                  %VAL( CNF_PVAL(WPNTR(3)) ),
     :                  %VAL( CNF_PVAL(WPNTR(4)) ), STATUS )

*     Check that the locations are strictly monotonically increasing.
         MONO = 2
         CALL SPD_UAASR( NCOMP3, %VAL( CNF_PVAL(WPNTR(2)) ), MONO,
     :                   FIRST, STATUS )
         IF ( MONO .NE. 2 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ARCIDENT_T03', I )
            CALL ERR_REP( 'ARCIDENT_E14', 'ARCIDENT: Error ' //
     :         'identifying features in row #^ARCIDENT_T03. ' //
     :         'Line component locations are not strictly ' //
     :         'monotonically increasing. ' //
     :         'Aborting and deleting output file.', STATUS )
            GO TO 500
         END IF

*     Set up for auto-identification in this row.
         FDBSTA = 0
         MAXPOS = MIN( MAXLOC, NCOMP3 )
         MINPOS = MAX( MIN( MINIDS, MAXPOS - 2 ), 6 )
         NCOMP4 = 0

*     If something is wrong with some of the parameters, warn the user.
         IF ( MINPOS .GT. MAXPOS ) THEN
            CALL MSG_SETI( 'ARCIDENT_T03', I )
            CALL MSG_OUT( 'ARCIDENT_M01', 'ARCIDENT: Warning: ' //
     :         'Not enough located features in row #^ARCIDENT_T03. ' //
     :         'Skipping to next row.', STATUS )

*     Else (all is well so far).
         ELSE

*        Set up the lab value range for this row.
*        In echelle mode we assume that orders may overlap by half their
*        extent.
            IF ( ECHELL ) THEN
               STARTW = WRANGE(1) + FLOAT(I-1)
     :                * (WRANGE(2)-WRANGE(1)) / FLOAT(NROWS+1)
               ENDW   = WRANGE(1) + FLOAT(I+1)
     :                * (WRANGE(2)-WRANGE(1)) / FLOAT(NROWS+1)
            ELSE
               STARTW = WRANGE(1)
               ENDW   = WRANGE(2)
            END IF

*        Identify the located features.
*        The locations and peaks are turned into identified locations
*        and their identifications. Not all located features may have
*        been identified.
            CALL SPD_WZKA( INFO, DBDIM(1), DBDIM(3), INDXSZ,
     :                     %VAL( CNF_PVAL(FPNTR(1)) ),
     :                     %VAL( CNF_PVAL(FPNTR(2)) ),
     :                     %VAL( CNF_PVAL(FPNTR(3)) ),
     :                     %VAL( CNF_PVAL(FPNTR(4)) ),
     :                     %VAL( CNF_PVAL(FPNTR(5)) ),
     :                     %VAL( CNF_PVAL(FPNTR(6)) ),
     :                     %VAL( CNF_PVAL(FPNTR(7)) ), XSTART, XEND,
     :                     STARTW, ENDW, .FALSE., DRANGE(1), DRANGE(2),
     :                     THRESH, MINPOS, MAXPOS, NEIGHB(1),
     :                     NEIGHB(2), STREN, NCOMP3,
     :                     %VAL( CNF_PVAL(WPNTR(2)) ),
     :                     %VAL( CNF_PVAL(WPNTR(3)) ), NCOMP4,
     :                     %VAL( CNF_PVAL(WPNTR(5)) ),
     :                     %VAL( CNF_PVAL(WPNTR(6)) ), FDBSTA )

*        If something went wrong for this row, give a warning.
            IF ( FDBSTA .NE. 0 .OR. NCOMP4 .LE. 0 ) THEN
               CALL MSG_SETI( 'ARCIDENT_T03', I )
               CALL MSG_OUT( 'ARCIDENT_M02', 'ARCIDENT: Warning: ' //
     :            'Failed to identify features in row ' //
     :            '#^ARCIDENT_T03. Skipping to next row.', STATUS )

*        Else (this row was fine), sort into output results the locations,
*        location variances and identifications.
            ELSE
               CALL SPD_WZKL( NCOMP3, NCOMP4, I, TNPAR2,
     :                        %VAL( CNF_PVAL(WPNTR(2)) ),
     :                        %VAL( CNF_PVAL(WPNTR(4)) ),
     :                        %VAL( CNF_PVAL(WPNTR(5)) ),
     :                        %VAL( CNF_PVAL(WPNTR(6)) ),
     :                        %VAL( CNF_PVAL(OPNTR(1)) ),
     :                        %VAL( CNF_PVAL(OPNTR(2)) ), STATUS )
            END IF
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 500
 3    CONTINUE

*  Tidy up.
 500  CONTINUE

*  Release FDB.
      DO 4 I = 24, 17, -1
         CALL DAT_ANNUL( LOC(I), STATUS )
 4    CONTINUE
      CALL NDF_ANNUL( NDF(5), STATUS )

*  Release output.
      CALL NDF_ANNUL( NDF(4), STATUS )
      CALL DAT_ANNUL( LOC(9), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF_DELET( NDF(2), STATUS )
      ELSE
         CALL NDF_ANNUL( NDF(2), STATUS )
      END IF

*  Release input.
      CALL NDF_ANNUL( NDF(3), STATUS )
      CALL DAT_ANNUL( LOC(1), STATUS )
      CALL NDF_ANNUL( NDF(1), STATUS )

*  Close NDF.
      CALL NDF_END( STATUS )

*  Return.
      END
