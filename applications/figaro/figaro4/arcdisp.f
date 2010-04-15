      SUBROUTINE ARCDISP( STATUS )
*+
*  Name:
*     ARCDISP

*  Purpose:
*     Fit polynomial dispersion curve.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ARCDISP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine fits a polynomial dispersion curve to a list of
*     identified arc features and transforms the NDF pixel coordinates
*     to spectroscopic values. Optionally you can use a graphical
*     dialogue to improve on the previous feature identification, until
*     you like the appearance of the dispersion curve.
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
*     The actual input is the results structure in the Specdre
*     Extension. This must be a set of components of type 'arc
*     feature'. Each must have two parameters 'centre' and 'laboratory
*     value'. These must be corresponding locations one expressed in
*     NDF pixel coordinates, the other in spectroscopic values
*     (wavelength, frequency etc.). The centres must be strictly
*     monotonically increasing, their variances must be available.
*     Laboratory values may be bad values to signify unidentified
*     features.
*
*     In the graphical dialogue the results structure may be updated.
*     The locations remain unchanged; all located features form a fixed
*     list of potentially identified features. Identifications may be
*     added, deleted or modified. The user has to work on each row in
*     turn (unless Quit is chosen). When the user switches from one row
*     to the next, the dispersion curve for the finished row is applied
*     and its spectroscopic values in the Specdre Extension are set.
*     When the last row is finished, the application exits; the output
*     of this routine is (i) an updated list of identifications in the
*     results structure of the Specdre Extension and (ii) an array of
*     spectroscopic values according to the dispersion curves for each
*     row, also in the Specdre Extension. At any point the user can
*     quit. In this case the array of spectroscopic values is
*     discarded, but the updated identifications are retained. If run
*     without dialogue, this routine simply performs the polynomial fit
*     of the dispersion curve for each row in turn and works out the
*     array of spectroscopic values. The list of identifications is
*     input only and remains unchanged. If for any row the fit cannot
*     be performed, then the spectroscopic values calculated so far are
*     discarded and the routine aborts.
*
*     There must not yet be any spectroscopic value information: There
*     must be no array of spectroscopic values or widths in the Specdre
*     Extension. The pixel centre array for the spectroscopic axis
*     (i.e. the first axis) must be NDF pixel coordinates (usually 0.5,
*     1.5, ...).
*
*     This routine works on each row (spectrum) in turn. It fits a
*     polynomial to the existing identifications. In the optional
*     graphical dialogue two plots are displayed and updated as
*     necessary. The lower panel is a plot of laboratory values
*     (wavelength, frequency etc.) versus pixel coordinate shows
*     -  all possible identifications from the feature data base as
*        horizontal lines,
*     -  all unidentified located features as vertical lines,
*     -  all identified located features as diagonal crosses,
*     -  the dispersion curve.
*
*     In the upper panel, a linear function is subtracted so that it
*     displays the higher-order components of the dispersion curve.
*     Crosses indicate the identified located features. Since the scale
*     of this upper panel is bigger, it can be used to spot outlying
*     feature identifications. In the dialogue you can
*        R - Switch to next row, accepting the current fit for this row
*        X - X-zoom 2x on cursor
*        Y - Y-zoom 2x on cursor
*        W - Unzoom to show whole row
*        N - Pan by 75% of current plot range
*        A - Add ID for location nearest to cursor (from FDB)
*        S - Set ID for location nearest to cursor (from cursor y pos.)
*        D - Delete ID for feature nearest to cursor
*        Q - Quit (preserves updated IDs, discards applied fits for all
*            rows)
*        ? - Help
*
*     Whenever the list of identifications is changed, the dispersion
*     curve is fitted again and re-displayed. If there are too few
*     identifications for the order chosen, then the dialogue will
*     display the maximum order possible. But such an under-order fit
*     cannot be accepted, the R option will result in an error.
*
*     The Q option will always result in an error report, formally the
*     routine aborts. After all, it does not achieve the main goal of
*     applying individual dispersion curves to all rows.
*
*     On one hand the output of this routine may be an updated list of
*     identifications, which could in principle be used in a future run
*     of this routine. On the other hand this routine will always
*     result in an array of spectroscopic values. The existence of
*     these spectroscopic values prevents using this routine again.
*     Before using this routine again on the same input NDF you have to
*     delete the SPECVALS component in the Specdre Extension.
*
*     In order to facilitate repeated use of this routine on the same
*     data, it always uses the Specdre Extension to store spectroscopic
*     values, even if the data are one-dimensional and the first axis
*     centre array would suffice to hold that information. This leaves
*     the first axis centre array at NDF pixel coordinates, as
*     necessary for re-use of this routine.

*  Usage:
*     arcdisp in order

*  ADAM Parameters:
*     DIALOG = _CHAR (Read)
*        If this is 'Y', 'T' or 'G', then the graphical dialogue is
*        entered before the polynomial dispersion curve for any row is
*        accepted and applied. If this is 'N' or 'F' then the dialogue
*        is not entered and separate dispersion curves are applied to
*        all rows. The string is case-insensitive. ['G']
*     IN = NDF (Read)
*        The spectrum or set of spectra in which emission features are
*        to be located. This must be a base NDF, the spectroscopic axis
*        must be the first axis. No spectroscopic values or widths must
*        exist in the Specdre Extension. The pixel centres along the
*        first axis must be NDF pixel coordinates. Update access is
*        necessary, the results structure in the Specdre Extension may
*        be modified, an array of spectroscopic values will be created
*        in the Specdre Extensions.
*     ORDER = _INTEGER (Read)
*        The polynomial order of dispersion curves. This cannot be changed
*        during the graphical dialogue. Neither can it differ between
*        rows.  [2]
*     FDB = NDF (Read)
*        The feature data base. Only the simple list of values FTR_WAVE is
*        used and only in graphics dialogue. It serves to find the
*        identification for an as yet unidentified - but located
*        feature.
*     DEVICE = GRAPHICS (Read)
*        The graphics device to be used. This is unused if DIALOG is
*        false.
*     WRANGE( 2 ) = _REAL (Read)
*        In graphical dialogue this parameter is used repeatedly to get
*        a range of laboratory values. This is used for plotting as well
*        as for finding identifications in the feature data base.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.
*
*     This routine works in situ and modifies the input file.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22 Jun 1993 (hme):
*        Original version.
*     29 Jun 1993 (hme):
*        Quit in SPADZ no longer returns bad status.
*     16 Mar 1994 (hme):
*        Change argument list for SPAEA.
*     30 Jan 1995 (hme):
*        Insert call to NDF_ASTYP between creating and mapping the
*        _DOUBLE temporary NDF. This should avoid a problematic type
*        conversion in NDF_END.
*     20 Nov 1995 (hme):
*        Use PDA instead of NAG for polynomial fitting.
*     2005 June 1 (MJC):
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
      INCLUDE 'SPD_EPAR'         ! Standard Extension constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER XCLEN              ! Length of strings in string arrays
      PARAMETER ( XCLEN = 32 )

*  Local Variables:
*  NDF(1): input NDF.
*  NDF(2): output SPECVALS.
*  NDF(3): RESULTS.
*  NDF(4): FDB.
*  NDF(5): workspaces.
*  NDF(6): workspace.
*  NELM(1): input NDF size.
*  NELM(2...4): result array sizes.
*  NELM(5): FDB line list size.
*  LOC(1): input Specdre Extension.
*  LOC(2...7): component-related vectors.
*  LOC(8): parameter-related vector.
*  LOC(9): FDB extension.
*  LOC(10): FTR_WAVE.
*  PNTR(1): output SPECVALS.
*  PNTR(3...4): result data and variance.
*  PNTR(5...10): component-related vectors.
*  PNTR(11): parameter-related vector.
*  PNTR(12): FDB line list.
*  PNTR(13...16): workspaces NCOMP long.
*  PNTR(17): workspace 3*NCOMP+3*(ORDER+1) long.
      CHARACTER * ( 1 ) DIALOG
      INTEGER ORDER
      LOGICAL THERE              ! True if an HDS component exists
      LOGICAL ISBAS              ! True if NDF is base
      LOGICAL LINEAR             ! True if array is linear
      LOGICAL FINSHD             ! True if all rows transformed
      INTEGER I                  ! Temporary integer
      INTEGER PLACE              ! NDF place holder
      INTEGER NDF( 6 )           ! NDF identifiers
      INTEGER NELM( 5 )          ! Array sizes
      INTEGER NROWS              ! Input number of rows
      INTEGER NDIM               ! Input NDF dimensionality
      INTEGER DIM(  NDF__MXDIM ) ! Input NDF dimensions
      INTEGER LBND( NDF__MXDIM ) ! Input NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! Input NDF upper bounds
      INTEGER PNTR( 17 )         ! Array pointers
      INTEGER COMP( 2 )          ! Results component range
      INTEGER NCOMP              ! Number of result components
      INTEGER TNPAR              ! Total number of result parameters
      REAL XSTART, XEND          ! Range of linear array
      CHARACTER * ( DAT__SZLOC ) LOC( 10 ) ! HDS locators
      CHARACTER * ( DAT__SZTYP ) TYPE( 3 ) ! Results data types
*
*  Initialise PNTR and NELM arrays
      DATA PNTR,NELM /17*0, 5*0/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up.
      CALL NDF_BEGIN

*  Modal parameters.
      CALL PAR_GET0C( 'DIALOG', DIALOG, STATUS )
      CALL CHR_UCASE( DIALOG )
      IF ( DIALOG.EQ.'Y' .OR. DIALOG.EQ.'T' .OR. DIALOG.EQ.'G' ) THEN
         DIALOG = 'G'
      ELSE IF ( DIALOG .EQ. 'N' .OR. DIALOG .EQ. 'F' ) THEN
         DIALOG = 'F'
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCDISP_E01', 'ARCDISP: Error: Invalid ' //
     :      'dialog mode.', STATUS )
         GO TO 500
      END IF

*  Get input.
      CALL NDF_ASSOC( 'IN', 'UPDATE', NDF(1), STATUS )
      CALL NDF_SIZE( NDF(1), NELM(1), STATUS )
      CALL NDF_DIM( NDF(1), NDF__MXDIM, DIM, NDIM, STATUS )
      CALL NDF_BOUND( NDF(1), NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      NROWS = NELM(1) / DIM(1)

*  Check input.
*  As a side effect the Specdre Extension is accessed for update.
      CONTINUE

*     Must be base NDF.
         CALL NDF_ISBAS( NDF(1), ISBAS, STATUS )
         IF ( .NOT. ISBAS ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCDISP_E02', 'ARCDISP: Error ' //
     :         'accessing input: The NDF is not a base NDF.', STATUS )
            GO TO 500
         END IF

*     Spectroscopic axis must be first.
         CALL SPD_EAAA( NDF(1), 'UPDATE', THERE, LOC(1), STATUS )
         CALL SPD_EABA( NDF(1), THERE, I, STATUS )
         IF ( I .NE. 1 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCDISP_E03', 'ARCDISP: Error ' //
     :         'accessing input: The spectroscopic axis is not the ' //
     :         'first axis.', STATUS )
            GO TO 500
         END IF

*     Must have neither SPECVALS nor SPECWIDS.
         IF ( THERE ) THEN
            CALL DAT_THERE( LOC(1), XCMP6, THERE, STATUS )
            IF ( THERE ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ARCDISP_E04', 'ARCDISP: Error ' //
     :            'accessing input: There are spectroscopic values ' //
     :            'in the Specdre Extension.', STATUS )
               GO TO 500
            END IF
            CALL DAT_THERE( LOC(1), XCMP7, THERE, STATUS )
            IF ( THERE ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ARCDISP_E05', 'ARCDISP: Error ' //
     :            'accessing input: There are spectroscopic widths ' //
     :            'in the Specdre Extension.', STATUS )
               GO TO 500
            END IF
         END IF

*  Map pixel centres as SPECVALS for update, check that they are pixel
*  coordinates.
      CALL SPD_EAED( NDF(1), LOC(1), 'UPDATE', '_REAL',
     :   'laboratory values', 'unknown', PNTR(1), NDF(2), I, STATUS )
      CALL SPD_UAAHR( DIM(1), %VAL( CNF_PVAL(PNTR(1)) ), 1E-5, XSTART,
     :                XEND, LINEAR, STATUS )
      IF ( .NOT. LINEAR .OR.
     :      XSTART .NE. LBND(1)-0.5 .OR. XEND .NE. UBND(1)-0.5 ) THEN
         CALL MSG_SETR( 'ARCDISP_T01', LBND(1)-0.5 )
         CALL MSG_SETR( 'ARCDISP_T02', UBND(1)-0.5 )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCDISP_E06', 'ARCDISP: Error ' //
     :      'accessing input: The first axis'' coordinates are not ' //
     :      'default NDF pixel coordinates. They should run ' //
     :      'linearly from ^ARCDISP_T01 to ^ARCDISP_T02.', STATUS )
         GO TO 500
      END IF

*  Access the exisiting results.
      CALL SPD_FDHA( NDF(1), LOC(1), NCOMP, TNPAR, TYPE, STATUS )
      TYPE(1) = '_REAL'
      TYPE(2) = '_REAL'
      TYPE(3) = '_REAL'
      COMP(1) = 1
      COMP(2) = NCOMP
      CALL SPD_FDHE( NDF(1), LOC(1), 'UPDATE', TYPE, COMP, NDF(3),
     :   LOC(2), LOC(8), PNTR(3), PNTR(5), PNTR(11), NELM(2),
     :   STATUS )

*  Check that the results structure conforms to our needs.
      CALL SPD_WZMC( NCOMP, TNPAR, %VAL(CNF_PVAL(PNTR(8))),
     :               %VAL( CNF_PVAL(PNTR(7)) ),
     :               %VAL( CNF_PVAL(PNTR(11)) ), STATUS,
     :               %VAL(CNF_CVAL(XCLEN)), %VAL(CNF_CVAL(XCLEN)) )

*  Release the results' extension vectors.
      DO 1 I = 2, 8
         CALL DAT_ANNUL( LOC(I), STATUS )
 1    CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get polynomial order.
      CALL PAR_GET0I( 'ORDER', ORDER, STATUS )

*  Get workspaces.
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( '_DOUBLE', 1, 1, NCOMP, PLACE, NDF(5), STATUS )
      CALL NDF_ASTYP( '_DOUBLE', NDF(5), 'CENTRE,WIDTH,VARIANCE',
     :   1, STATUS )
      CALL NDF_AMAP( NDF(5), 'CENTRE,WIDTH,VARIANCE', 1, '_DOUBLE',
     :   'WRITE', PNTR(13), I, STATUS )
      CALL NDF_MAP( NDF(5), 'DATA', '_DOUBLE', 'WRITE',
     :   PNTR(16), I, STATUS )
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( '_DOUBLE', 1, 1, 3*(NCOMP+ORDER+1),
     :   PLACE, NDF(6), STATUS )
      CALL NDF_MAP( NDF(6), 'DATA', '_DOUBLE', 'WRITE',
     :   PNTR(17), I, STATUS )

*  Get the feature data base, from it the line list.
*  That is used to add further identifications in the graphics
*  dialogue, or just as lines in the log plot without dialogue.
      CALL NDF_ASSOC( 'FDB', 'READ', NDF(4), STATUS )
      CALL NDF_XLOC( NDF(4), 'ECHELLE', 'READ', LOC(9), STATUS )
      CALL DAT_FIND( LOC(9), 'FTR_WAVE', LOC(10), STATUS )
      CALL DAT_SHAPE( LOC(10), 1, NELM(5), I, STATUS )
      CALL DAT_MAP( LOC(10), '_REAL', 'READ', 1, NELM(5),
     :   PNTR(12), STATUS )

*  If graphical dialogue was chosen.
      IF ( DIALOG .EQ. 'G' ) THEN

*     Enter the graphical dialogue.
         CALL SPD_CZMD( ORDER, NELM(5), NCOMP, DIM(1), NROWS,
     :                  %VAL( CNF_PVAL(PNTR(12)) ),
     :                  %VAL( CNF_PVAL(PNTR(13)) ),
     :                  %VAL( CNF_PVAL(PNTR(14)) ),
     :                  %VAL( CNF_PVAL(PNTR(15)) ),
     :                  %VAL( CNF_PVAL(PNTR(16)) ),
     :                  %VAL( CNF_PVAL(PNTR(17)) ),
     :                  %VAL( CNF_PVAL(PNTR(1)) ),
     :                  %VAL( CNF_PVAL(PNTR(3)) ),
     :                  %VAL( CNF_PVAL(PNTR(4)) ),
     :                  FINSHD, STATUS )

*     If the user Quit before all rows have been worked on, issue a
*     warning and delete the SPECVALS.
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         IF ( .NOT. FINSHD ) THEN
            CALL MSG_OUT( 'ARCDISP_M17', 'ARCDISP: Warning: Quit ' //
     :         'option chosen. Retaining changed identifications, ' //
     :         'but deleting spectroscopic values from Specdre ' //
     :         'Extension.', STATUS )
            CALL NDF_DELET( NDF(2), STATUS )
         END IF

*  Else (just apply existing identifications), apply to each row in
*  turn.
      ELSE
         DO 2 I = 1, NROWS
            CALL SPD_CZME( ORDER, NELM(5), NCOMP, DIM(1), I,
     :                     %VAL( CNF_PVAL(PNTR(12)) ),
     :                     %VAL( CNF_PVAL( PNTR(13)) ),
     :                     %VAL( CNF_PVAL(PNTR(14)) ),
     :                     %VAL( CNF_PVAL(PNTR(15)) ),
     :                     %VAL( CNF_PVAL(PNTR(16)) ),
     :                     %VAL( CNF_PVAL(PNTR(17)) ),
     :                     %VAL( CNF_PVAL(PNTR(1)) ),
     :                     %VAL( CNF_PVAL(PNTR(3)) ),
     :                     %VAL( CNF_PVAL(PNTR(4)) ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500
 2       CONTINUE
      END IF

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( NDF(2), STATUS )
      CALL DAT_ANNUL( LOC(1), STATUS )
      CALL NDF_END( STATUS )

*  Return.
      END
