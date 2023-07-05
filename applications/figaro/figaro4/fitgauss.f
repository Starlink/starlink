      SUBROUTINE FITGAUSS( STATUS )
*+
*  Name:
*     FITGAUSS

*  Purpose:
*     Fit Gauss profiles to a spectrum.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FITGAUSS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine fits up to six Gauss profiles at a time to a
*     one-dimensional data set. This can be specified as an NDF section.
*     The data set must extend along the spectroscopic axis.
*
*     After accessing the data and the (optional) plot device, the data
*     will be subjected to a mask that consists of up to six abscissa
*     intervals. These may or may not overlap and need not lie within
*     the range of existing data. The masking will remove data which are
*     bad, have bad variance or have zero variance. The masking will
*     also provide weights for the fit. If the given data have no
*     variances attached, or if the variances are to be ignored, all
*     weights will be equal.
*
*     After the data have been masked, guessed values for the fit are
*     required. These are
*     -  the number of components to be fitted,
*     -  the value of any underlying constant continuum (this must be an
*        a-priori known constant),
*     -  the components' guessed centre positions,
*     -  peak heights and
*     -  full widths at half maxima. Finally,
*     -  fit flags for each of the Gauss parameters are needed.
*
*     The fit flags specify whether any parameter is fixed, fitted, or
*     kept at a constant ratio or offset to another fitted parameter.
*
*     The masked data and parameter guesses are then fed into the fit
*     routine. Single or multiple Gauss fits are made to line features.
*     Gauss fit parameters may be free, fixed, or tied to the
*     corresponding parameter of another Gauss component fitted at the
*     same time. Peak and width are tied by fixing the ratios, the
*     centre is tied by fixing the offset. Up to six Gauss components
*     can be fitted simultaneously.
*
*     The fit is done by minimising chi-squared (or rms if variances are
*     unavailable or are chosen to be ignored). The covariances between
*     fit parameters - and among these the uncertainties of parameters -
*     are estimated from the curvature of psi-squared. psi-squared is
*     usually the same as chi-squared. If, however, the given data are
*     not independent measurements, a slightly modified function
*     psi-squared should be used, because the curvature of chi-squared
*     gives an overoptimistic estimate of the fit parameter uncertainty.
*     In that function the variances of the given measurements are
*     substituted by the sums over each row of the covariance matrix of
*     the given data. If the data have been resampled with a Specdre
*     routine, that routine will have stored the necessary additional
*     information in the Specdre Extension, and this routine will
*     automatically use that information to assess the fit parameter
*     uncertainties. A full account of the psi-squared function is given
*     in Meyerdierks, 1992a/b. But note that these covariance row sums
*     are ignored if the main variance is ignored or unavailable.
*
*     If the fit is successful, then the result is reported to
*     the standard output device and plotted on the graphics device. The
*     final plot viewport is saved in the AGI data base and can be used
*     by further applications.
*
*     The result is stored in the Specdre Extension of the input NDF.
*     Optionally, the complete description (input NDF name, mask used,
*     result, etc.) is written (appended) to an ASCII log file.
*
*     Optionally, the application can interact with the user. In that
*     case, a plot is provided before masking, before guessing and
*     before fitting. After masking, guessing and fitting, a screen
*     report and a plot are provided and the user can improve the
*     parameters. Finally, the result can be accepted or rejected, that
*     is, the user can decide whether to store the result in the Specdre
*     Extension or not.
*
*     The screen plot consists of two viewports. The lower one shows the
*     data values (full-drawn bin-style) overlaid with the guess or fit
*     (dashed line-style). The upper box shows the residuals (cross
*     marks) and error bars. The axis scales are arranged such that
*     all masked data can be displayed. The upper box displays a
*     zero-line for reference, which also indicates the mask.
*
*     The Extension provides space to store fit results for each
*     non-spectroscopic coordinate. Say, if you have a 2-D image each
*     row being a spectrum, then you can store results for each row. The
*     whole set of results can be filled successively by fitting one row
*     at a time and always using the same component number to store the
*     results for that row. (See also the example.)
*
*     The components fitted by this routine are specified as follows:
*     The line names and laboratory frequencies are the default values
*     and are not checked against any existing information in the
*     input's Specdre Extension. The component types are 'Gauss'. The
*     numbers of parameters allocated to each component are 4, the
*     three guessed and fitted parameters and the line integral. The
*     parameter types are in order of appearance: 'centre', 'peak',
*     'FWHM', 'integral'.

*  Usage:
*     fitgauss in device=? mask1=? mask2=?
*        ncomp=? cont=? centre=? peak=? fwhm=? cf=? pf=? wf=?
*        comp=? logfil=?

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        If false, this routine will issue only error messages and no
*        informational message. [YES]
*     VARUSE = _LOGICAL (Read)
*        If false, input variances are ignored. [YES]
*     DIALOG = _CHAR (Read)
*        If 'T', the routine offers in general more options for
*        interaction. The mask or guess can be improved after
*        inspections of a plot. Also, the routine can resolve
*        uncertainties about where to store results by consulting the
*        user. ['T']
*     IN = NDF (Read)
*        The input NDF. This must be a one-dimensional (section of an)
*        NDF. You can specify e.g. an image column as IN(5,) or part of
*        an image row as IN(2.2:3.3,10). Update access is necessary to
*        store the fit result in the NDF's Specdre Extension.
*     REPAIR = _LOGICAL (Read)
*        If DIALOG is true, REPAIR can be set true in order to change
*        the spectroscopic number axis in the Specdre Extension. [NO]
*     DEVICE = DEVICE (Read)
*        The name of the plot device. Enter the null value (!) to
*        disable plotting. [!]
*     MASK1( 6 ) = _REAL (Read)
*        Lower bounds of mask intervals. The mask is the part(s) of the
*        spectrum that is (are) fitted and plotted. The mask is put
*        together from up to six intervals:
*
*           mask = [MASK1(1);MASK2(1)] U [MASK1(2);MASK1(2)]
*                U ...
*                U [MASK1(MSKUSE);MASK2(MSKUSE)].
*
*        The elements of the MASK parameters are not checked for
*        monotony. Thus intervals may be empty or overlapping. The
*        number of intervals to be used is derived from the number of
*        lower/upper bounds entered. Either MASK1 or MASK2 should be
*        entered with not more numbers than mask intervals required.
*     MASK2( 6 ) = _REAL (Read)
*        Upper bounds of mask intervals. See MASK1.
*     NCOMP = _INTEGER (Read)
*        The number of Gauss profiles to be fitted. Must be between 1
*        and 6. [1]
*     CONT = _REAL (Read)
*        This value indicates the level of the continuum. Any constant
*        value for CONT is acceptable. [0]
*     CENTRE( 6 ) = _REAL (Read)
*        Guess centre position for each Gauss component.
*     PEAK( 6 ) = _REAL (Read)
*        Guess peak height for each Gauss component.
*     FWHM( 6 ) = _REAL (Read)
*        Guess full width at half maximum for each Gauss component.
*     CF( 6 ) = _INTEGER (Read)
*        For each Gauss component I, a value CF(I)=0 indicates that
*        CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CF(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CF(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J).
*     PF( 6 ) = _INTEGER (Read)
*        For each Gauss component I, a value PF(I)=0 indicates that
*        PEAK(I) holds a guess which is free to be fitted.
*        A positive value PF(I)=I indicates that PEAK(I) is fixed.
*        A positive value PF(I)=J<I indicates that PEAK(I) has to
*        keep a fixed ratio to PEAK(J).
*     WF( 6 ) = _INTEGER (Read)
*        For each Gauss component I, a value WF(I)=0 indicates that
*        FWHM(I) holds a guess which is free to be fitted.
*        A positive value WF(I)=I indicates that FWHM(I) is fixed.
*        A positive value WF(I)=J<I indicates that FWHM(I) has to
*        keep a fixed ratio to FWHM(J).
*     REMASK = _LOGICAL (Read)
*        Reply YES to have another chance for improving the mask.
*        [NO]
*     REGUESS = _LOGICAL (Read)
*        Reply YES to have another chance for improving the guess and
*        fit. [NO]
*     FITGOOD = _LOGICAL (Read)
*        Reply YES to store the result in the Specdre Extension. [YES]
*     COMP = _INTEGER (Read)
*        The results are stored in the Specdre Extension of the data.
*        This parameter specifies which existing components are being
*        fitted. You should give NCOMP values, which should all be
*        different and which should be between zero and the number of
*        components that are currently stored in the Extension. Give a
*        zero for a hitherto unknown component. If a COMP element is
*        given as zero or if it specifies a component unfit to store the
*        results of this routine, then a new component will be created
*        in the result storage structure. In any case this routine will
*        report which components were actually used and it will deposit
*        the updated values in the parameter system. [1,2,3,4,5,6]
*     LOGFIL = FILENAME (Read)
*        The file name of the log file. Enter the null value (!) to
*        disable logging. The log file is opened for append. [!]
*     FCENTRE( 6 ) = _REAL (Write)
*        Fitted centre position for each Gauss component.
*     FPEAK( 6 ) = _REAL (Write)
*        Fitted peak height for each Gauss component.
*     FFWHM( 6 ) = _REAL (Write)
*        Fitted full width at half maximum for each Gauss component.

*  Examples:
*     fitgauss in device=xw mask1=-1.5 mask2=2.5
*           ncomp=1 cont=1.0 centre=0.5 peak=-0.5 fwhm=1.5 cf=0 pf=0 wf=0
*           comp=1 logfil=line
*        This fits a single Gauss profile to the x range [-1.5,2.5]. The
*        continuum is assumed to be constant at 1.0. The Gauss is
*        guessed to be centred at 0.5 with width 1.5. It is guessed to
*        be an absorption line with an amplitude of -0.5.
*        All Gauss parameters are free to be fitted. The fit result is
*        reported to the text file LINE.DAT and stored as component
*        number 1 in the input file's Specdre Extension.
*        Since DIALOG is not turned off, the user will be prompted for
*        improvements of the mask and guess, and will be asked whether
*        the final fit result is to be accepted (stored in the Extension
*        and written to LINE.DAT).
*        The XWINDOWS graphics device will display the spectrum before
*        masking, guessing, and fitting. Independent of the DIALOG
*        switch, a plot is produced after fitting.
*     fitgauss in(,5) device=! mask1=-1.5 mask2=2.5
*           ncomp=1 cont=0.0 centre=0.5 peak=13.0 fwhm=1.5 cf=0 pf=0 wf=1
*           comp=0 logfil=! dialog=f
*        This fits a single Gauss profile to the x range [-1.5,2.5] of
*        the 5th row in the 2-D image IN. The baseline is assumed to be
*        constant at 0.0. The Gauss is guessed to be centred at 0.5 with
*        width 1.5. It is guessed to be an emission line with an
*        amplitude of 13. Centre position and peak height are free to be
*        fitted, but the width is fixed to 1.5. User interaction
*        (DIALOG) and plotting (DEVICE) are de-selected. There is also no
*        log file where to the results are written. If INFO were also
*        switched off, no report whatsoever would be made. However, the
*        results are stored as a new component (COMP=0) in the Specdre
*        Extension of the input file.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.
*
*     This routine works in situ and modifies the input file.

*  References:
*     Meyerdierks, H., 1992a, Covariance in resampling and model fitting,
*     Starlink, Spectroscopy Special Interest Group
*
*     Meyerdierks, H., 1992b, Fitting resampled spectra, in P.J.
*     Grosbol, R.C.E. de Ruijsscher (eds), 4th ESO/ST-ECF Data Analysis
*     Workshop, Garching, 13 - 14 May 1992, ESO Conference and Workshop
*     Proceedings No. 41, Garching bei Muenchen, 1992

*  Authors:
*     jrw: J.R. Walsh (AAO)
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acd: Clive Davenhall (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     %% Mar 1987 (jrw):
*        Original version of Figaro's GAUSS.
*     %% %%% 198% (jrw, ks):
*        A number of changes for Figaro's GAUSS.
*     24 Mar 1988 (ks):
*        Changes to Figaro's GAUSS.
*     08 May 1991 (hme):
*        First version as SPECFIT. This is based on the structure of
*        Figaro's GAUSS, but provides a completely different user
*        interface, including bad-value handling. The subroutine DOFIT
*        is the interface to CONTFIT, E02ADF, E02AEF for polynomials
*        and to AUTOFIT, DOIT1, DOIT2, E04HBF, E04JBF, FUNCT, MONIT; all
*        these routines were changed only slightly. The subroutine
*        DOPLOT is the interface to GAUS_XZPLOT, which also was changed
*        only slightly.
*     31 May 1991 (hme):
*        Try a different way to deal with array parameters.
*        Reduce number of Gauss components to 6.
*        Polynomial is now calculated for whole spectrum. Correction for
*        polynomial is still done only within mask, but that could be
*        whole spectrum. The point really was, that the fit data were
*        bad outside the (outer) mask as it was at time of fit.
*        Ignore status from VEC_ routines.
*     28 Jun 1991 (hme):
*        Acquire the covariance matrix in case of a Gauss fit. This is
*        stored in A as matrix of size FITPAR x FITPAR. This can however
*        only be used by subroutines, since here, it is declared as 1-d
*        of size (3*MAXGAU)**2.
*        Report parameter errors and correlation matrix.
*        VARUSE, MSKUSE parameters.
*        Accept 10-D data, copy specified row into work space NDF,
*        on eXit copy it back, on Quit don't copy it back.
*        Plot with AGI/SNX/NCAR. CONTFIT oblivious.
*        Sort to increasing x while masking (provided given x are
*        monotone).
*        Remove constraints in AUTOFIT.
*        All subroutine names now SPFxxx (except AUTOFIT and below).
*        Inherited status in SPFREP doesn't work if FITPAR = 0.
*     23 Jul 1991 (hme):
*        Replace AUTOFIT, DOIT1, DOIT2, FUNCT, MONIT by call to E04DGF
*        with objective function SPFGAU. Pack masked x, data, weights
*        into one array.
*        Scrap oblivious parameters in SPFDFT. MENUE -> MENU. Don't use
*        FITPAR for dimensions. Scrap MSKUSE as a parameter.
*        Add ITER parameter. Have SPFDFT put fitted Gauss parameters
*        to ADAM parameters FIT%%%%. If ITER, SPFSET will read these and
*        set as dynamic defaults for CENTRE etc.
*        No code from the original GAUSS has survived now.
*     21 Sep 1991 (hme):
*        Avoid NDF.
*        Ad hoc emergency exit: Always when VXIST and fit copied to
*        data.
*     29 Oct 1991 (hme):
*        Revised the emergency exit. Now happens only when the post-copy
*        update finds an empty mask. (The typical case is that the whole
*        spectrum was replaced by the fit, thus the whole variance array
*        (if it exists) was set to 0, and if VARUSE no data would pass
*        any mask any more.) This finding an empty mask is significant,
*        because the post-correction updates use the whole spectrum as
*        mask.
*        Also, the emergency exit asks the user whether to exit or quit,
*        but has a tendency to quit (by default for any menu option
*        other than X).
*     27 Nov 1991 (hme):
*        Rename to FITGAUSS. Modify messages.
*        Proper handling of status in connection with VEC_ calls.
*     15 Dec 1991 (hme):
*        Suppress Starlink error messages arising from DSA-calls.
*     15 Jul 1992 (hme):
*        Change from DSA to NDF. Use Specdre Extension. Use covariance
*        row sums.
*     08 Jun 1993 (hme):
*        Update call to SPFDFT, returns CHISQR now.
*     10 Jun 1993 (hme):
*        Suppress error conditions from SPFDFT, but not error reports.
*     24 Jun 1993 (hme):
*        Revise error reporting. Revise graphics access and paging.
*     30 Jan 1995 (hme):
*        Initialise CHISQR to zero.
*     21 Nov 1995 (hme):
*        Change ASCII file to type LIST instead of FORTRAN.
*     23 Oct 2001 (acd):
*        Output the fit parameters (central position, peak height and
*        FWHM) as ADAM parameters as well as to the screen and log file.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     2005 Sep 1 (TIMJ):
*        Fix slight problem with CNF_PVAL and pointer arithmetic.
*        Initialise FIT arrays
*     2009 Jan 5 (TIMJ):
*        Use MSG_FLUSHERR instead of ERR_FLUSH to flush fit failures.
*        This is done to help ORAC-DR to prevent many ignorable error
*        from filling the stderr feed. Currently not configurable by parameter
*        but it could be if there are problems with doing it this way.
*     2023 Jul 5 (GSB):
*        Ensure the routine exits with bad status if fitting was
*        not successful.  (ORAC-DR expects this to happen in order
*        to know not to try to read pointing results from the
*        output parameters.)
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Status returned by PAR_
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! Bad values
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      CHARACTER * ( 32 ) COMPTY  ! Type of fitted component
      PARAMETER ( COMPTY = 'Gauss' )
      CHARACTER * ( 32 ) LINENA  ! Name of fitted component
      PARAMETER ( LINENA = 'unidentified component' )
      INTEGER REALSZ             ! No. of bytes per real number
      PARAMETER ( REALSZ = 4 )
      INTEGER MSKDIM             ! Maximum number of mask intervals * 2
      PARAMETER ( MSKDIM = 12 )
      INTEGER MAXGAU             ! Maximum number of Gauss components
      PARAMETER ( MAXGAU = 6 )
      INTEGER PLTRES             ! Number of fit curve points to display
      PARAMETER ( PLTRES = 501 )
      REAL RT8LN2                ! Square root of 8 ln(2)
      PARAMETER ( RT8LN2 = 2.354820 )

*  Local Variables:
*  NDF(1): input data/variance NDF.
*  NDF(2): input centre NDF.
*  NDF(3): masked arrays.
*  NDF(4): residuals.
*  NDF(5): two work spaces for error plotting and residual finding.
*  NDF(6): input covariance row sum NDF.
*  PNTR(1): input centre pointer.
*  PNTR(2): input data value pointer.
*  PNTR(3): input variance pointer.
*  PNTR(4): masked array pointer.
*  PNTR(5): unused.
*  PNTR(6): unused.
*  PNTR(7): residual data pointer.
*  PNTR(8): first workspace from NDF(5).
*  PNTR(9): second workspace from NDF(5).
*  PNTR(10):input covariance row sums.
*  RMIN/RMAX(1): masked centre extrema.
*  RMIN/RMAX(2): masked data value extrema.
*  RMIN/RMAX(3): masked error extrema.
*  RMIN/RMAX(4): residual extrema.
*  PMIN/PMAX(1): abscissa of bottom viewport
*  PMIN/PMAX(2): ordinate of bottom viewport
*  PMIN/PMAX(3): abscissa of top viewport
*  PMIN/PMAX(4): ordinate of top viewport
*  PICID(1): originally active picture.
*  PICID(2): bottom viewport.
*  PICID(3): top viewport.
      LOGICAL INFO
      LOGICAL VARUSE
      CHARACTER * ( 1 ) DIALCH
      LOGICAL DIALOG
      INTEGER MSKUSE
      REAL MASK( MSKDIM )
      LOGICAL PLOT
      INTEGER NCOMP
      REAL CONT
      REAL CENTRE( MAXGAU )
      REAL PEAK(   MAXGAU )
      REAL SIGMA(  MAXGAU )
      INTEGER CFLAGS( MAXGAU )
      INTEGER PFLAGS( MAXGAU )
      INTEGER SFLAGS( MAXGAU )
      LOGICAL REPLY
      INTEGER COMP( MAXGAU )
      LOGICAL FILE
      LOGICAL COVRSX             ! True if covariance row sums found
      LOGICAL MSKINI             ! True if mask to be set to full range
      LOGICAL FITTED             ! True if fit was successful
      INTEGER I, J               ! Loop indices
      INTEGER NDF( 6 )           ! Input NDF identifier
      INTEGER PNTR( 10 )         ! Mapped array pointers
      INTEGER NELM               ! Number of data points
      INTEGER PICID( 3 )         ! AGI picture
      INTEGER ZONID              ! SGS zone identifier
      INTEGER FD                 ! FIO descriptor
      INTEGER FU                 ! Fortran unit number
      INTEGER MSKELM             ! Number of masked data points
      INTEGER IMIN               ! First x-array elem. that passed mask
      INTEGER IMAX               ! Last x-array element that passed mask
      INTEGER FITPAR             ! No. of fit parameters
      INTEGER FITDIM             ! max(1,FITPAR)
      REAL WX1( 3 )              ! AGI world coordinates
      REAL WX2( 3 )              ! AGI world coordinates
      REAL WY1( 3 )              ! AGI world coordinates
      REAL WY2( 3 )              ! AGI world coordinates
      REAL RMIN( 4 )             ! Minima
      REAL RMAX( 4 )             ! Maxima
      REAL PMIN( 4 )             ! Plot minima
      REAL PMAX( 4 )             ! Plot maxima
      REAL CHISQR                ! Masked chi-squared or square of rms
      REAL VARSCL                ! VARSCL*COVAR is covariance
      REAL FWHM( MAXGAU )        ! Full width half maxima
      REAL FFWHM( MAXGAU )       ! Fitted Full width half maxima
      REAL FLUX( MAXGAU )        ! Line integrals
      REAL CVAR( MAXGAU )        ! Centre variances
      REAL PVAR( MAXGAU )        ! Peak variances
      REAL SVAR( MAXGAU )        ! Sigma variances
      REAL WVAR( MAXGAU )        ! FWHM variances
      REAL FVAR( MAXGAU )        ! Line integral variances
      REAL FITX( PLTRES )        ! Fit curve x values
      REAL FITY( PLTRES )        ! Fit curve y values
      REAL DATA( 4 )             ! Parameter values
      REAL VARS( 4 )             ! Parameter variances
      DOUBLE PRECISION COVAR( 9*MAXGAU*MAXGAU ) ! Covariance matrix
      CHARACTER * ( 132 ) IN     ! Input NDF file name
      CHARACTER * ( 32 ) PARATY( 4 ) ! Parameter types
      CHARACTER * ( 64 ) PLABEL( 3 ) ! Plot labels

*  Local data.
      DATA PARATY / 'centre', 'peak', 'FWHM', 'integral' /

*  Initialise data variables
      DATA NDF, PNTR /6*0, 10*0/

*  Initialise fitting flag values
      DATA CFLAGS, PFLAGS, SFLAGS /MAXGAU*0, MAXGAU*0, MAXGAU*0/

*  And all MAXGAU arrays
      DATA FWHM, FFWHM, FLUX, CVAR, PVAR, SVAR, WVAR,
     :     CENTRE, PEAK, SIGMA, COMP
     :     / MAXGAU*0.0, MAXGAU*0.0, MAXGAU*0.0, MAXGAU*0.0,
     :       MAXGAU*0.0, MAXGAU*0.0, MAXGAU*0.0, MAXGAU*0.0,
     :       MAXGAU*0.0, MAXGAU*0.0, MAXGAU*0 /

      DATA RMIN, RMAX / 4*0, 4*0 /

      DO I = 1, 9*MAXGAU*MAXGAU
         COVAR(I) = 0.0D0
      END DO
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN
      PLOT   = .FALSE.
      FILE   = .FALSE.
      FITTED = .FALSE.
      REPLY  = .FALSE.
      FITPAR = 0
      FITDIM = 1
      CHISQR = 0.

*  Get modal parameters.
      CALL PAR_GET0L( 'INFO',   INFO,   STATUS )
      CALL PAR_GET0L( 'VARUSE', VARUSE, STATUS )
      CALL PAR_GET0C( 'DIALOG', DIALCH, STATUS )
      CALL CHR_UCASE( DIALCH )
      DIALOG = ( DIALCH .EQ. 'T' .OR. DIALCH .EQ. 'Y' )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Data and workspace access. This is rather complex and handled by a
*  specific subroutine. It will also return suitable plot labels.
      CALL SPD_CAAF( DIALOG, VARUSE, COVRSX, NELM, NDF, PNTR, IN,
     :   PLABEL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Find out ranges of given data.
      CALL SPD_UAAAR( .FALSE., NELM, %VAL( CNF_PVAL(PNTR(1)) ), RMIN(1),
     :                RMAX(1), STATUS )
      CALL SPD_UAAAR( .TRUE.,  NELM, %VAL( CNF_PVAL(PNTR(2)) ), RMIN(2),
     :                RMAX(2), STATUS )

*  Get plot device. Null value for no plotting.
*  Write access means erase active part of screen.
*  The AGI picture active before this routine started is used for
*  display. The same picture will be active after this routine finishes.
      PLOT = .TRUE.
      CALL SPD_UGAA( 'DEVICE', 'WRITE', ' ', PICID(1), ZONID, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         PLOT = .FALSE.
      END IF

*  Check status.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Plot (data only).
      IF ( PLOT .AND. DIALOG )
     :   CALL SPD_WAAL( .FALSE., .FALSE., .FALSE., ZONID, PLABEL,
     :                  RMIN, RMAX, NELM, %VAL( CNF_PVAL(PNTR(1)) ),
     :                  %VAL( CNF_PVAL(PNTR(2)) ), MSKDIM, MSKUSE, MASK,
     :                  PLTRES, FITX, FITY, NELM,
     :                  %VAL( CNF_PVAL(PNTR(4)) ),
     :                  %VAL( CNF_PVAL(PNTR(7)) ),
     :                  %VAL( CNF_PVAL(PNTR(8)) ),
     :                  %VAL( CNF_PVAL(PNTR(9)) ), PMIN, PMAX, STATUS )

*  Come here to redo mask.
      MSKINI = .TRUE.
 1    CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Find out and apply mask.
      CALL SPD_CAAGR( DIALOG, MSKINI, .TRUE., VARUSE, COVRSX, NELM,
     :                MSKDIM, %VAL( CNF_PVAL(PNTR(1)) ),
     :                %VAL( CNF_PVAL(PNTR(2)) ),
     :                %VAL( CNF_PVAL(PNTR(3)) ),
     :                %VAL( CNF_PVAL(PNTR(10)) ), MSKUSE, MASK, IMIN,
     :                IMAX, RMIN, RMAX, MSKELM,
     :                %VAL( CNF_PVAL(PNTR(4)) ), STATUS )
      RMIN(3) = SQRT(RMIN(3))
      RMAX(3) = SQRT(RMAX(3))
      MSKINI = .FALSE.

*  Check status.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Have a chat with the user.
      IF ( DIALOG ) THEN

*     Plot (data and mask only).
*     This is definitely not the first plot. So we call PGEND before
*     plotting.
         IF ( PLOT ) THEN
            CALL SPD_WAAL( .TRUE., .FALSE., .FALSE., ZONID, PLABEL,
     :                     RMIN, RMAX, NELM, %VAL( CNF_PVAL(PNTR(1)) ),
     :                     %VAL( CNF_PVAL(PNTR(2)) ), MSKDIM, MSKUSE,
     :                     MASK, PLTRES, FITX, FITY, MSKELM,
     :                     %VAL( CNF_PVAL(PNTR(4)) ),
     :                     %VAL( CNF_PVAL(PNTR(7)) ),
     :                     %VAL( CNF_PVAL(PNTR(8)) ),
     :                     %VAL( CNF_PVAL(PNTR(9)) ), PMIN, PMAX,
     :                     STATUS )
         END IF

*     Report to screen.
*     Use number of components zero to suppress spurious Gauss line
*     report.
         CALL SPD_WZEC( .FALSE., 6, MSKDIM, FITPAR, FITDIM,
     :      IN, NELM,
     :      VARUSE, MSKUSE, MSKELM, RMIN(1), RMAX(1), MASK,
     :      0, CONT, CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, SIGMA,
     :      COVAR, CHISQR/(MSKELM-FITPAR), STATUS )

*     Ask if improvement on mask required.
         CALL PAR_GET0L( 'REMASK', REPLY, STATUS )
         CALL PAR_CANCL( 'REMASK', STATUS )
      END IF

*  Try again to mask?
      IF ( REPLY ) GO TO 1

*  Come here to redo guess and fit.
 2    CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Select guess.
      CALL SPD_CZEE( DIALOG, MAXGAU, NCOMP, FITPAR, FITDIM, CONT,
     :   CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, SIGMA, COVAR, STATUS )

*  Have another chat with the user.
      IF ( DIALOG ) THEN

*     Get the guess data corresponding to the masked data.
         CALL SPD_WZED( MAXGAU, 1, MSKELM, NCOMP, CONT,
     :                 CENTRE, PEAK, SIGMA, 0D0, RMIN(1), RMAX(1),
     :                 %VAL( CNF_PVAL(PNTR(4)) ),
     :                 %VAL( CNF_PVAL(PNTR(8)) ), STATUS )

*     Get residuals as difference of masked data minus guess data. Also
*     need the range of residuals for plotting purposes.
         CALL VEC_SUBR( .FALSE., MSKELM,
     :                  %VAL( CNF_PVAL(PNTR(4))+REALSZ*MSKELM ),
     :                  %VAL( CNF_PVAL(PNTR(8)) ),
     :                  %VAL( CNF_PVAL(PNTR(7)) ), I, J, STATUS )
         CALL SPD_UAAAR( .FALSE., MSKELM, %VAL( CNF_PVAL(PNTR(7)) ),
     :                   RMIN(4), RMAX(4), STATUS )

*     Get masked 1/error as square root of weights. Can skip guess data.
         CALL VEC_SQRTR( .FALSE., MSKELM,
     :                   %VAL( CNF_PVAL(PNTR(4))+2*REALSZ*MSKELM ),
     :                   %VAL( CNF_PVAL(PNTR(8)) ), I, J, STATUS )

*     Multiply residuals with 1/error.
         CALL VEC_MULR( .FALSE., MSKELM, %VAL( CNF_PVAL(PNTR(7)) ),
     :                  %VAL( CNF_PVAL(PNTR(8)) ),
     :                  %VAL( CNF_PVAL(PNTR(9)) ), I, J, STATUS )

*     Get CHISQR as the sum of squares over the array hiding behind
*     pointer 9. This is either chi-squared or
*     rms = SQRT( CHISQR / degrees of freedom )
*     Note that the masked 1/error are still behind pointer 8.
*     They are needed below for plotting.
         CALL SPD_UAAWR( .FALSE., MSKELM,
     :                   %VAL( CNF_PVAL( CNF_PVAL(PNTR(9)) ) ),
     :                   I, CHISQR, STATUS )

*     Calculate guess curve and plot.
         IF ( PLOT ) THEN

*        Fill the guess curve x array linearly to match plotted range.
            CALL SPD_UAAJR( RMIN(1), RMAX(1), PLTRES, FITX, STATUS )

*        Calculate the guess curve y array from its x array.
            CALL SPD_WZED( MAXGAU, 1, PLTRES, NCOMP, CONT,
     :         CENTRE, PEAK, SIGMA, 0D0, RMIN(1), RMAX(1),
     :         FITX, FITY, STATUS )

*        Calculate the bottom and top tips of error bars as residual
*        plus or minus 1/(1/error).
            IF ( VARUSE )
     :         CALL SPD_WAAK( .FALSE., MSKELM,
     :                        %VAL( CNF_PVAL(PNTR(7)) ),
     :                        %VAL( CNF_PVAL(PNTR(8)) ),
     :                        %VAL( CNF_PVAL(PNTR(8)) ),
     :                        %VAL( CNF_PVAL(PNTR(9)) ), STATUS )

*        Plot guess (the whole lot).
            CALL SPD_WAAL( .TRUE., .TRUE., VARUSE, ZONID, PLABEL,
     :                      RMIN, RMAX, NELM, %VAL( CNF_PVAL(PNTR(1)) ),
     :                      %VAL( CNF_PVAL(PNTR(2)) ), MSKDIM, MSKUSE,
     :                      MASK, PLTRES, FITX, FITY, MSKELM,
     :                      %VAL( CNF_PVAL(PNTR(4)) ),
     :                      %VAL( CNF_PVAL(PNTR(7)) ),
     :                      %VAL( CNF_PVAL(PNTR(8)) ),
     :                      %VAL( CNF_PVAL(PNTR(9)) ), PMIN, PMAX,
     :                      STATUS )
         END IF

*     Report to screen.
         CALL SPD_WZEC( .FALSE., 6, MSKDIM, FITPAR, FITDIM,
     :      IN, NELM,
     :      VARUSE, MSKUSE, MSKELM, RMIN(1), RMAX(1), MASK,
     :      NCOMP, CONT, CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, SIGMA,
     :      COVAR, CHISQR/(MSKELM-FITPAR), STATUS )

*     Ask if improvement on guess required.
         CALL PAR_GET0L( 'REGUESS', REPLY, STATUS )
         CALL PAR_CANCL( 'REGUESS', STATUS )
      END IF

*  Try again to guess?
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( REPLY ) GO TO 2

*  Do the fit.
      CALL ERR_MARK
      CALL SPD_WFGA( INFO, COVRSX, MSKELM, NCOMP, FITPAR, FITDIM, CONT,
     :               %VAL( CNF_PVAL(PNTR(4)) ), CFLAGS, PFLAGS, SFLAGS,
     :               CENTRE, PEAK, SIGMA, CHISQR, COVAR, FITTED,
     :               STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
*  Report the error using MSG
         CALL MSG_FLUSHERR( STATUS )
      END IF
      CALL ERR_RLSE

*  Report fit.
      IF ( FITTED ) THEN

*     Get the fit data corresponding to the masked data.
         CALL SPD_WZED( MAXGAU, 1, MSKELM, NCOMP, CONT, CENTRE, PEAK,
     :                  SIGMA, 0D0, RMIN(1), RMAX(1),
     :                  %VAL( CNF_PVAL(PNTR(4)) ),
     :                  %VAL( CNF_PVAL(PNTR(8)) ), STATUS )

*     Get residuals as difference of masked data minus fit data. Also
*     need the range of residuals for plotting purposes.
         CALL VEC_SUBR( .FALSE., MSKELM,
     :                  %VAL( CNF_PVAL(PNTR(4))+REALSZ*MSKELM ),
     :                  %VAL( CNF_PVAL(PNTR(8)) ),
     :                  %VAL( CNF_PVAL(PNTR(7)) ), I, J, STATUS )
         CALL SPD_UAAAR( .FALSE., MSKELM, %VAL( CNF_PVAL(PNTR(7)) ),
     :                   RMIN(4), RMAX(4), STATUS )

*     Get masked 1/error as square root of weights. Can skip fit data.
         CALL VEC_SQRTR( .FALSE., MSKELM,
     :                   %VAL( CNF_PVAL(PNTR(4))+2*REALSZ*MSKELM ),
     :                   %VAL( CNF_PVAL(PNTR(8)) ), I, J, STATUS )

*     Multiply residuals with 1/error.
         CALL VEC_MULR( .FALSE., MSKELM, %VAL( CNF_PVAL(PNTR(7)) ),
     :                  %VAL( CNF_PVAL(PNTR(8)) ),
     :                  %VAL( CNF_PVAL(PNTR(9)) ), I, J, STATUS )

*     Get CHISQR as the sum of squares over the array hiding behind
*     pointer 9. This is either chi-squared or
*     rms = SQRT( CHISQR / degrees of freedom )
*     Note that the masked weights (1/error) are still behind pointer 8.
*     They are needed below for plotting.
         CALL SPD_UAAWR( .FALSE., MSKELM, %VAL( CNF_PVAL(PNTR(9)) ),
     :                   I, CHISQR, STATUS )

*     Work out the variances of all parameters (fixed, fitted, tied),
*     including the FWHMa and line integrals. The scaling factor for the
*     COVAR matrix to yield covariance may be 1 or rms-squared.
         IF ( VARUSE ) THEN
            VARSCL = 1.
         ELSE
            VARSCL = CHISQR / ( MSKELM - FITPAR )
         END IF
         CALL SPD_WZEE( NCOMP, FITDIM, CFLAGS, PFLAGS, SFLAGS, PEAK,
     :      SIGMA, VARSCL, COVAR, FWHM, FLUX,
     :      CVAR, PVAR, SVAR, WVAR, FVAR, STATUS )

*     Calculate fit curve and plot.
         IF ( PLOT ) THEN

*        Fill the fit curve x array linearly to match plotted range.
            CALL SPD_UAAJR( RMIN(1), RMAX(1), PLTRES, FITX, STATUS )

*        Calculate the fit curve y array from its x array.
            CALL SPD_WZED( MAXGAU, 1, PLTRES, NCOMP, CONT,
     :         CENTRE, PEAK, SIGMA, 0D0, RMIN(1), RMAX(1),
     :         FITX, FITY, STATUS )

*        Calculate the bottom and top tips of error bars as residual
*        plus or minus 1/weight.
            IF ( VARUSE ) THEN
               CALL SPD_WAAK( .FALSE., MSKELM,
     :                        %VAL( CNF_PVAL(PNTR(7)) ),
     :                        %VAL( CNF_PVAL(PNTR(8)) ),
     :                        %VAL( CNF_PVAL(PNTR(8)) ),
     :                        %VAL( CNF_PVAL(PNTR(9)) ), STATUS )
            END IF

*        Plot fit (the whole lot).
            CALL SPD_WAAL( .TRUE., FITTED, VARUSE, ZONID, PLABEL,
     :                     RMIN, RMAX, NELM, %VAL( CNF_PVAL(PNTR(1)) ),
     :                     %VAL( CNF_PVAL(PNTR(2)) ), MSKDIM, MSKUSE,
     :                     MASK, PLTRES, FITX, FITY, MSKELM,
     :                     %VAL( CNF_PVAL(PNTR(4)) ),
     :                     %VAL( CNF_PVAL(PNTR(7)) ),
     :                     %VAL( CNF_PVAL(PNTR(8)) ),
     :                     %VAL( CNF_PVAL(PNTR(9)) ), PMIN, PMAX,
     :                     STATUS )
         END IF
      END IF

*  Report to screen.
      IF ( INFO .OR. DIALOG )
     :   CALL SPD_WZEC( .FALSE., 6, MSKDIM, FITPAR, FITDIM, IN, NELM,
     :      VARUSE, MSKUSE, MSKELM, RMIN(1), RMAX(1), MASK,
     :      NCOMP, CONT, CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, SIGMA,
     :      COVAR, CHISQR/(MSKELM-FITPAR), STATUS )

*  Ask if improvement on guess required.
      IF ( DIALOG ) THEN
         CALL PAR_GET0L( 'REGUESS', REPLY, STATUS )
         CALL PAR_CANCL( 'REGUESS', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Try again to guess?
      IF ( REPLY ) GO TO 2

*  Ask if improvement on mask required.
      IF ( DIALOG ) THEN
         CALL PAR_GET0L( 'REMASK', REPLY, STATUS )
         CALL PAR_CANCL( 'REMASK', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Try again to guess?
      IF ( REPLY ) GO TO 1

*  So there won't be another plot. Let's save the PGPLOT viewports as
*  AGI pictures.
      IF ( PLOT ) THEN

*     Enquire world coordinates of current picture.
         CALL AGI_SELP( PICID(1), STATUS )
         CALL AGI_IWOCO( WX1(1), WX2(1), WY1(1), WY2(1), STATUS )

*     Work out world coordinates of the two PGPLOT viewports used by
*     the plotting routine. We assume the plot layout here instead of
*     getting the information from the plotting routine.
         WX1(2) = WX1(1) + 0.1 * ( WX2(1) - WX1(1) )
         WX2(2) = WX2(1) - .05 * ( WX2(1) - WX1(1) )
         WY1(2) = WY1(1) + 0.1 * ( WY2(1) - WY1(1) )
         WY2(2) = WY2(1) - 0.3 * ( WY2(1) - WY1(1) )
         WX1(3) = WX1(2)
         WX2(3) = WX2(2)
         WY1(3) = WY1(1) + 0.7 * ( WY2(1) - WY1(1) )
         WY2(3) = WY2(1) - 0.1 * ( WY2(1) - WY1(1) )

*     Save the two viewports as AGI pictures. Their coordinate systems
*     are as reported back by the plotting routine.
         CALL AGI_SELP( PICID(1), STATUS )
         CALL AGI_NUPIC( WX1(2), WX2(2), WY1(2), WY2(2),
     :      'DATA', 'SPECDRE_FITCHEBY',
     :      PMIN(1), PMAX(1), PMIN(2), PMAX(2), PICID(2), STATUS )
         CALL AGI_SELP( PICID(1), STATUS )
         CALL AGI_NUPIC( WX1(3), WX2(3), WY1(3), WY2(3),
     :      'DATA', 'SPECDRE_FITCHEBY',
     :      PMIN(3), PMAX(3), PMIN(4), PMAX(4), PICID(3), STATUS )
      END IF

*  If we technically have a fit, see if we want to accept it.
      IF ( FITTED .AND. DIALOG )
     :   CALL PAR_GET0L( 'FITGOOD', FITTED, STATUS )

*  If we have an accepted fit.
      IF ( FITTED ) THEN

*     Find out in which component to store results.
*     Must do checks on the numbers given:
*     All must be positive and not equal, there must be NCOMP of them.
*     We do not check the number of values given, since we fill with 0.
         CALL PAR_GET1I( 'COMP', MAXGAU, COMP, I, STATUS )

*     Loop through components fitted.
         DO 4 J = 1, NCOMP

*        Check that component number for storage has not been used
*        already during this run.
            IF ( J .GT. 1 ) THEN
               DO 3 I = 1, J-1
                  IF ( COMP(J) .GT. 0 .AND. COMP(J) .EQ. COMP(I) )
     :               COMP(J) = 0
 3             CONTINUE
            END IF

*        Sort the parameters.
            DATA(1) = CENTRE(J)
            DATA(2) = PEAK(J)
            DATA(3) = FWHM(J)
            DATA(4) = FLUX(J)
            VARS(1) = CVAR(J)
            VARS(2) = PVAR(J)
            VARS(3) = WVAR(J)
            VARS(4) = FVAR(J)

*        Store parameters.
*        This may not be terribly effective: In the extreme case, 6
*        components are stored and all are hitherto unknown. Then the
*        result structure is reshaped 6 times to accommodate for one
*        more component each.
            CALL SPD_CAAH( NDF(1), 4, 4, '_REAL', '_REAL', '_REAL',
     :         LINENA, COMPTY, PARATY, VAL__BADR,
     :         DATA, VARS, COMP(J), STATUS )

*        Report back, which component was actually used.
            IF ( INFO ) THEN
               CALL MSG_SETI( 'FITGAUSS_T01', COMP(J) )
               CALL MSG_OUT(  'FITGAUSS_M01', 'Used component # ' //
     :            '^FITGAUSS_T01 to store results.', STATUS )
            END IF
 4       CONTINUE

*     Also feed back into parameter system.
         IF ( NCOMP .GT. 1 ) THEN
            CALL PAR_PUT1I( 'COMP', NCOMP, COMP, STATUS )
         ELSE
            CALL PAR_PUT0I( 'COMP', COMP(1), STATUS )
         END IF

*     Check status.
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Get logfile, null value for no logging.
         FILE = .TRUE.
         CALL FIO_ASSOC( 'LOGFIL', 'APPEND', 'LIST', 0, FD, STATUS )

*     If no log file.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            FILE = .FALSE.

*     Else (log file specified).
         ELSE

*        Report to log file.
            CALL FIO_UNIT( FD, FU, STATUS )
            CALL SPD_WZEC( .TRUE., FU, MSKDIM, FITPAR, FITDIM,
     :         IN, NELM,
     :         VARUSE, MSKUSE, MSKELM, RMIN(1), RMAX(1), MASK,
     :         NCOMP, CONT, CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, SIGMA,
     :         COVAR, CHISQR/(MSKELM-FITPAR), STATUS )
         END IF

*     Check status.
         IF ( STATUS .NE. SAI__OK ) GO TO 500
      END IF

*    Write the fit results as ADAM parameters.  Note that it is necessary
*    to compute the FWHM from the sigmas.
      IF (STATUS .EQ. SAI__OK  .AND.  NCOMP .GT. 0) THEN
         DO I = 1, NCOMP
            FFWHM(I) = SIGMA(I) * RT8LN2
         END DO

         CALL PAR_PUT1R( 'FCENTRE', NCOMP, CENTRE, STATUS )
         CALL PAR_PUT1R( 'FPEAK', NCOMP, PEAK, STATUS )
         CALL PAR_PUT1R( 'FFWHM', NCOMP, FFWHM, STATUS )
      END IF

*  Tidy up.
 500  CONTINUE

*  Close down file I/O.
      IF ( FILE ) THEN
         CALL FIO_CANCL( 'LOGFIL', STATUS )
         CALL FIO_DEACT( STATUS )
      END IF

*  Close down graphics.
      IF ( PLOT ) CALL SPD_UGAB( 'DEVICE', .FALSE., STATUS )

*  Close down NDF.
      DO 5 I = 1, 6
         IF ( NDF(I) .NE. NDF__NOID ) CALL NDF_ANNUL( NDF(I), STATUS )
 5    CONTINUE
      CALL NDF_END( STATUS )

*  Exit with bad status if fitting failed?
      IF ( STATUS .EQ. SAI__OK .AND. .NOT. FITTED ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FITGAUSS_E01', 'FITGAUSS: Error: ' //
     :      'Fitting may not have been successful.', STATUS )
      END IF

*  Return.
      END
