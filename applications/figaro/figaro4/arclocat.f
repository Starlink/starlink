      SUBROUTINE ARCLOCAT( STATUS )
*+
*  Name:
*     ARCLOCAT

*  Purpose:
*     Locate line features in a set of spectra.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ARCLOCAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine locates narrow features in a set of spectra. Features
*     can be located from scratch automatically. In a different mode,
*     feature locations can be added or deleted in a graphical dialogue.
*     The feature location and peak are determined by a Gauss or
*     triangle line fit.
*
*     The input data must be a base NDF. They can be a single spectrum
*     or a set of spectra. Examples for the latter are a long slit
*     spectrum, a set of extracted fibre spectra, or a collapsed
*     echellogram (a set of extracted orders from an echelle
*     spectrograph). It is necessary that the spectroscopic axis be the
*     first axis in the data set. It does not matter how many further
*     axes there are, the data will be treated as a set of rows with
*     each row a spectrum.
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
*     decrease with increasing row number.
*
*     In automatic mode this routine works on each row (spectrum) in
*     turn. It scans through the spectrum and looks for pixels that
*     exceed the local background level by at least the given threshold
*     value. When such a pixel is spotted, a single-component line fit
*     is tried no the local profile. The local profile is centred on the
*     pixel suspected to be part of an emission feature. It includes 1.5
*     times the guessed FWHM on either side and a further 5 baseline
*     pixels on either side. A local linear baseline is subtracted prior
*     to the line fit. In order for the feature to be entered into the
*     list of located features, the fit must succeed, the fitted peak
*     must exceed the threshold, and the fitted peak must exceed the
*     absolute difference of background levels between the left and
*     right.
*
*     When run with graphics dialogue this routine works on any choice
*     of rows. It uses a pre-existing list of located features to which
*     can be added or from which features can be deleted. Graphics
*     dialogue can also be used to just check the locations. The graph
*     displays the spectrum currently worked on in bin-style. The current
*     list of located features is indicated by dashed vertical lines.
*     The options in the graphical dialogue are:
*        R - Choose different row to work on
*        X - X-zoom 2x on cursor
*        Y - Y-zoom 2x on cursor
*        W - Unzoom to show whole row
*        N - Pan left/right by 75% of current x range
*        A - Add the feature under cursor to list (subject to line fit)
*        S - Add the cursor position as feature to list
*        D - Delete the feature nearest cursor from list
*        Q - Quit, preserving the updated list of located features
*        ? - Help
*
*     The difference between the A and S options is that A tries a line
*     fit to the local profile around the cursor, while S accepts the
*     cursor x position as exact centre and the cursor y position as
*     exact peak of a new feature; (the variance of the centre is set
*     to 0.25, the variance of the peak to the bad value).
*
*     The result of this routine is a list of Gauss or triangle
*     features. Their locations in NDF pixel coordinates and their peak
*     values are stored in the results structure of the Specdre
*     Extension of the input data. If run in automatic mode, this
*     routine will replace any previously existing results structure. If
*     run with graphics dialogue, this routine will try to work with a
*     pre-existing list of located features. But if the pre-existing
*     results structure does not conform to the required format, then a
*     new results structure is created.
*
*     The list of located features (for each row) is always sorted such
*     that the locations are strictly monotonically increasing.
*
*     The results structure provides for a certain number of components.
*     These have component type 'Gauss feature' or 'triangle feature'.
*     Each component has two parameters 'centre' and 'peak'. The number
*     of components is determined when the results structure is created,
*     it is derived from the approximate width of features and the
*     number of pixels in each spectrum.

*  Usage:
*     arclocat in fwhm thresh

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        If true, messages about the progress of auto-locating features
*        are issued. [YES]
*     DIALOG = _CHAR (Read)
*        If this is 'Y', 'T' or 'G', then no auto-locating takes place
*        and the graphics dialogue is entered. If this is 'N' or 'F'
*        then the dialogue is not entered and auto-locating is done
*        instead. The string is case-insensitive.  ['G']
*     MODE = _CHAR (Read)
*        This can be 'Gauss' or 'triangle' and chooses the line profile
*        to be fitted. This string is case-insensitive and can be
*        abbreviated to one character. ['Gauss']
*     IN = NDF (Read)
*        The spectrum or set of spectra in which emission features are
*        to be located. This must be a base NDF, the spectroscopic axis
*        must be the first axis. No spectroscopic values or widths must
*        exist in the Specdre Extension. The pixel centres along the
*        first axis must be NDF pixel coordinates. Update access is
*        necessary, the results structure in the Specdre Extension will
*        be modified, possibly re-created.
*     FWHM = _REAL (Read)
*        The guessed full width at half maximum of the features to be
*        located. This is used to estimate the maximum number of
*        features that might be located, to locate baseline ranges next
*        to suspected features, and as a guess for the line fit.
*     THRESH = _REAL (Read)
*        The threshold. While scanning a pixel must exceed this
*        threshold to initiate a line fit. The fitted peak also must
*        exceed the threshold in order that the feature location be
*        accepted. This parameter is significant only for automatic
*        location of features.
*     DEVICE = GRAPHICS (Read)
*        The graphics device to be used. This is unused if DIALOG is
*        false.
*     ROWNUM = _INTEGER (Read)
*        In graphics dialogue this parameter is used to switch to a
*        different row (spectrum).

*  Examples:
*     arclocat in 4. 20. mode=triangle dialog=f
*        This will scan through (all rows of) the NDF called "in". It
*        looks out for features of 4 pixels full width at half maximum
*        and with a peak value of at least 20 above the local
*        background. The features are fitted as triangles. The search is
*        automatic. Thus a new results structure in the input NDF's
*        Specdre Extension is created with the locations (centres) and
*        peaks of located features.
*     arclocat in 4. mode=Gauss dialog=g rownum=5
*        This will use the graphic dialogue. Starting with the fifth row
*        the user can use the mouse cursor to choose features that are
*        to be deleted from or added to the list of located features.
*        This can be used to improve on an automatic run, or when no
*        features have been located so far. If you try to add a feature
*        to the list, a Gauss fit is tried in the vicinity of the
*        cursor-selected position.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.
*
*     This routine works in situ and modifies the input file.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11 Jun 1993 (hme):
*        Original version.
*     23 Jun 1993 (hme):
*        Fix bug, where workspace for line fit was much too small.
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
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER XCLEN              ! Specdre Extension string length
      PARAMETER ( XCLEN = 32 )

*  Local Variables:
*  NDF(1): input NDF.
*  NDF(2):
*  NDF(3): results NDF.
*  NDF(4): workspace.
*  NELM(1): input NDF size.
*  NELM(2...4): result array sizes.
*  LOC(1): Specdre Extension.
*  LOC(2...7): component related vectors.
*  LOC(8): parameter related vector.
*  PNTR(1): pixel centre array.
*  PNTR(2): data array.
*  PNTR(3): result data array.
*  PNTR(4): result variance array.
*  PNTR(5...10): component related vectors.
*  PNTR(11): parameter related vector.
*  PNTR(12): workspace for packed local profile.
      LOGICAL INFO
      CHARACTER * ( 1 ) DIALOG
      CHARACTER * ( 8 ) MODE
      REAL FWHM
      REAL THRESH
      LOGICAL ISBAS              ! True if NDF is base
      LOGICAL THERE              ! True if an HDS component exists
      LOGICAL LINEAR             ! True if an array is linear
      INTEGER I, J               ! Temporary integers
      INTEGER PLACE              ! NDF place holder
      INTEGER NDF( 4 )           ! NDF identifiers
      INTEGER NELM( 4 )          ! Array sizes
      INTEGER NROWS              ! Number of rows in input
      INTEGER NDIM               ! Input NDF dimensionality
      INTEGER DIM( NDF__MXDIM )  ! Input NDF dimenesions
      INTEGER LBND( NDF__MXDIM ) ! Input NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! Input NDF upper bounds
      INTEGER PNTR( 12 )         ! Array pointers
      INTEGER NCOMP              ! Maximum number of features per row
      INTEGER TNPAR              ! Number of parameters in results
      INTEGER COMP( 2 )          ! Range of components accessed
      REAL XSTART, XEND          ! Range of an array
      CHARACTER * ( DAT__SZLOC ) LOC( 8 ) ! HDS locators
      CHARACTER * ( DAT__SZTYP ) TYPE3( 3 ) ! Results data types

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN

*  Modal parameters.
      CALL PAR_GET0L( 'INFO',   INFO,   STATUS )
      CALL PAR_GET0C( 'DIALOG', DIALOG, STATUS )
      CALL PAR_GET0C( 'MODE',   MODE,   STATUS )
      CALL CHR_UCASE( DIALOG )
      IF ( DIALOG.EQ.'Y' .OR. DIALOG.EQ.'T' .OR. DIALOG.EQ.'G' ) THEN
         DIALOG = 'G'
      ELSE IF ( DIALOG .EQ. 'N' .OR. DIALOG .EQ. 'F' ) THEN
         DIALOG = 'F'
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCLOCAT_E01', 'ARCLOCAT: Error: Invalid ' //
     :      'dialog mode.', STATUS )
         GO TO 500
      END IF
      CALL CHR_UCASE( MODE )
      MODE(2:) = ' '
      IF ( MODE .NE. 'G' .AND. MODE .NE. 'T' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCLOCAT_E02', 'ARCLOCAT: Error: Invalid ' //
     :      'mode. Must be "Gauss" or "triangle".', STATUS )
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
            CALL ERR_REP( 'ARCLOCAT_E03', 'ARCLOCAT: Error ' //
     :         'accessing input: The NDF is not a base NDF.', STATUS )
            GO TO 500
         END IF

*     Spectroscopic axis must be first.
         CALL SPD_EAAA( NDF(1), 'UPDATE', THERE, LOC(1), STATUS )
         CALL SPD_EABA( NDF(1), THERE, I, STATUS )
         IF ( I .NE. 1 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCLOCAT_E04', 'ARCLOCAT: Error ' //
     :         'accessing input: The spectroscopic axis is not the ' //
     :         'first axis.', STATUS )
            GO TO 500
         END IF

*     Must have neither SPECVALS nor SPECWIDS.
         IF ( THERE ) THEN
            CALL DAT_THERE( LOC(1), XCMP6, THERE, STATUS )
            IF ( THERE ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ARCLOCAT_E05', 'ARCLOCAT: Error ' //
     :            'accessing input: There are spectroscopic values ' //
     :            'in the Specdre Extension.', STATUS )
               GO TO 500
            END IF
            CALL DAT_THERE( LOC(1), XCMP7, THERE, STATUS )
            IF ( THERE ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ARCLOCAT_E06', 'ARCLOCAT: Error ' //
     :            'accessing input: There are spectroscopic widths ' //
     :            'in the Specdre Extension.', STATUS )
               GO TO 500
            END IF
         END IF

*  Get width and threshold.
      CALL PAR_GET0R( 'FWHM', FWHM, STATUS )
      IF ( DIALOG .NE. 'G' ) CALL PAR_GET0R( 'THRESH', THRESH, STATUS )

*  Map pixel centres, check that they are pixel coordinates.
      CALL NDF_AMAP( NDF(1), 'CENTRE', 1, '_REAL', 'READ',
     :               PNTR(1), I, STATUS )
      CALL SPD_UAAHR( I, %VAL( CNF_PVAL(PNTR(1)) ), 1E-5, XSTART, XEND,
     :                LINEAR, STATUS )
      IF ( .NOT. LINEAR .OR.
     :      XSTART .NE. LBND(1)-0.5 .OR. XEND .NE. UBND(1)-0.5 ) THEN
         CALL MSG_SETR( 'ARCLOCAT_T01', LBND(1)-0.5 )
         CALL MSG_SETR( 'ARCLOCAT_T02', UBND(1)-0.5 )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCLOCAT_E07', 'ARCLOCAT: Error ' //
     :      'accessing input: The first axis'' coordinates are not ' //
     :      'default NDF pixel coordinates. They should run ' //
     :      'linearly from ^ARCLOCAT_T01 to ^ARCLOCAT_T02.', STATUS )
         GO TO 500
      END IF

*  Map data array.
      CALL NDF_MAP( NDF(1), 'DATA', '_REAL', 'READ',
     :   PNTR(2), I, STATUS )

*  If results exist, check that they are what is needed.
      CALL DAT_THERE( LOC(1), XCMP9, THERE, STATUS )
      IF ( THERE ) THEN

*     For automatic location a pre-existing results structure is not
*     tolerated.
         IF ( DIALOG .NE. 'G' ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCLOCAT_E08', 'ARCLOCAT: Error ' //
     :         'accessing input: For auto-location the input must ' //
     :         'not have a results structure in the Specdre Extension.',
     :         STATUS )
            GO TO 500
         END IF

*     Access the existing results.
         TYPE3(1) = '_REAL'
         TYPE3(2) = '_REAL'
         TYPE3(3) = '_REAL'
         COMP(1) = 0
         COMP(2) = 0
         CALL SPD_FDHD( NDF(1), LOC(1), 'UPDATE', TYPE3, COMP, NDF(3),
     :      LOC(2), LOC(8), PNTR(3), PNTR(5), PNTR(11), NELM(2),
     :      STATUS )
         NCOMP = NELM(3)
         TNPAR = NELM(4)

*     Check that the results structure conforms to our needs.
         CALL SPD_WZJA( MODE, NCOMP, TNPAR, %VAL( CNF_PVAL(PNTR(8)) ),
     :                  %VAL(CNF_PVAL(PNTR(7))),
     :                  %VAL(CNF_PVAL(PNTR(11)) ), STATUS,
     :                  %VAL(CNF_CVAL(LEN(MODE))), 
     :                  %VAL(CNF_CVAL(XCLEN)), %VAL(CNF_CVAL(XCLEN)) )

*     Release the results' extension vectors.
         DO 1003 I = 2, 8
            CALL DAT_ANNUL( LOC(I), STATUS )
 1003    CONTINUE

*  Else (results do not exist), create results.
      ELSE

*     Create the results structure.
*     Two features can be only so close as to share one 5-pixel baseline
*     range. Thus they are at least 3*width+5 apart. With nx pixels in
*     the spectrum, we can have in total (nx-5)/(3*width+5)+1 features.
*     Each needs 2 parameters in the results structure, centre and peak.
         NCOMP = INT( FLOAT( DIM(1) - 5 ) / ( 3. * FWHM + 5. ) ) + 1
         TNPAR = 2 * NCOMP
         TYPE3(1) = '_REAL'
         TYPE3(2) = '_REAL'
         TYPE3(3) = '_REAL'
         COMP(1) = 1
         COMP(2) = NCOMP
         CALL SPD_FDHF( NDF(1), LOC(1), NCOMP, TNPAR, TYPE3, STATUS )

*     Access the new results structure.
         CALL SPD_FDHE( NDF(1), LOC(1), 'UPDATE', TYPE3, COMP, NDF(3),
     :      LOC(2), LOC(8), PNTR(3), PNTR(5), PNTR(11), NELM(2),
     :      STATUS )

*     Set up the results' extension vectors.
         IF ( MODE .EQ. 'G' ) THEN
            CALL SPD_UAAFC( 1, NELM(3), %VAL( CNF_PVAL(PNTR(7)) ),
     :         'Gauss feature', STATUS, %VAL(CNF_CVAL(XCLEN)) )
         ELSE IF ( MODE .EQ. 'T' ) THEN
            CALL SPD_UAAFC( 1, NELM(3), %VAL( CNF_PVAL(PNTR(7)) ),
     :         'triangle feature', STATUS, %VAL(CNF_CVAL(XCLEN)) )
         END IF
         CALL SPD_WZJB( NELM(4), %VAL(CNF_PVAL(PNTR(11))), STATUS,
     :                  %VAL(CNF_CVAL(XCLEN)) )

*     Release the results' extension vectors.
         DO 1001 I = 2, 8
            CALL DAT_ANNUL( LOC(I), STATUS )
 1001    CONTINUE
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get workspace for packed local profile.
      CALL NDF_TEMP( PLACE, STATUS )
      I = 4 * ( 2 * INT(1.5*FWHM) + 9 )
      CALL NDF_NEW( '_REAL', 1, 1, I, PLACE, NDF(4), STATUS )
      CALL NDF_MAP( NDF(4), 'DATA', '_REAL', 'WRITE',
     :   PNTR(12), I, STATUS )

*  If graphical dialogue was chosen, enter the dedicated routine.
      IF ( DIALOG .EQ. 'G' ) THEN
         CALL SPD_CZJE( MODE, DIM(1), INT(1.5*FWHM), NCOMP, NROWS,
     :                  FWHM, %VAL( CNF_PVAL(PNTR(1)) ),
     :                  %VAL( CNF_PVAL(PNTR(2)) ),
     :                  %VAL( CNF_PVAL(PNTR(12)) ),
     :                  %VAL( CNF_PVAL(PNTR(3)) ),
     :                  %VAL( CNF_PVAL(PNTR(4)) ), STATUS )

*  Else (automatic location), scan each row in turn.
      ELSE
         DO 1002 I = 1, NROWS
            CALL SPD_CZJD( INFO, MODE, DIM(1), INT(1.5*FWHM), NCOMP, I,
     :                     FWHM, THRESH, %VAL( CNF_PVAL(PNTR(1)) ),
     :                     %VAL( CNF_PVAL(PNTR(2)) ),
     :                     %VAL( CNF_PVAL(PNTR(12)) ), J,
     :                     %VAL( CNF_PVAL(PNTR(3)) ),
     :                     %VAL( CNF_PVAL(PNTR(4)) ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500
 1002    CONTINUE
      END IF

*  Tidy up.
 500  CONTINUE
      CALL NDF_ANNUL( NDF(4), STATUS )
      CALL NDF_ANNUL( NDF(3), STATUS )
      CALL DAT_ANNUL( LOC(1), STATUS )
      CALL NDF_ANNUL( NDF(1), STATUS )
      CALL NDF_END( STATUS )

*  Return.
      END
