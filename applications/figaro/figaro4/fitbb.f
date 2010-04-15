      SUBROUTINE FITBB( STATUS )
*+
*  Name:
*     FITBB

*  Purpose:
*     Fit diluted Planck curves to a spectrum.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FITBB( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine fits up to six diluted Planck curves to a
*     one-dimensional data set. This can be specified as an NDF section.
*     The data set must extend along the spectroscopic axis. The fit is
*     done on a double logarithmic representation of the data. The axis
*     data must be the common logarithm of frequency in Hertz. The data
*     themselves must be the common logarithm of intensity or flux
*     density in arbitrary units.
*
*     A diluted Planck component is defined as
*                                                            3
*          f_j     Theta_j        alpha_j                  nu
*        10    = 10        (nu/Hz)        (2h/c^2)  -------------------
*                                                     exp(hnu/kT_j) - 1
*
*     This assumes that the optical depth is small and the emissivity is
*     proportional to the frequency to the power of alpha. 10^Theta is
*     the hypothetical optical depth at frequency 1 Hz.
*
*     If the optical depth is large, a single simple Planck function
*     should be fitted, i.e. only one component with alpha = 0. In this
*     case 10^Theta is the conversion factor from the Planck function in
*     Jy/sr to the (linear) data values. If for example the data are the
*     common logarithm of the calibrated flux density of a source in Jy,
*     then Theta is the logarithm of the solid angle (in sr) subtended
*     by the source.
*
*     The fit is performed in double logarithmic representation, i.e.
*     the fitted function is
*
*        f = lg[ sum_j 10^f_j ]
*
*     The choice of Theta, alpha and lg(T) as fit parameters is
*     intuitive, but makes the fit routine ill-behaved. Very often alpha
*     cannot be fitted at all and must be fixed. Theta and alpha usually
*     anti-correlate completely. Even with fixed alpha do Theta and lg(T)
*     anti-correlate strongly.
*
*     Furthermore, Theta is difficult to guess. From any initial guess
*     of Theta one can improve by using Theta plus the average
*     deviation of the data from the guessed spectrum.
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
*     -  the components' guessed scaling constants Theta,
*     -  emissivity exponents alpha and
*     -  common logarithms of colour temperatures in Kelvin. Finally,
*     -  fit flags for each of the parameters are needed.
*
*     The fit flags specify whether any parameter is fixed, fitted, or
*     kept at a constant offset to another fitted parameter.
*
*     The masked data and parameter guesses are then fed into the fit
*     routine. Single or multiple fits are made. Fit parameters may be
*     free, fixed, or tied to the corresponding parameter of another
*     component fitted at the same time. They are tied by fixing the
*     offset, Up to six components can be fitted simultaneously.
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
*     input's Specdre Extension. The component types are 'Planck'. The
*     numbers of parameters allocated to each component are 3, the
*     three guessed and fitted parameters. The parameter types are in
*     order of appearance: 'Theta', 'alpha', 'lg(T)'.

*  Usage:
*     fitbb in device=? mask1=? mask2=?
*        ncomp=? theta=? alpha=? lgtemp=? sf=? af=? tf=?
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
*        The number of Planck curves to be fitted. Must be between 1
*        and 6. [1]
*     THETA( 6 ) = _REAL (Read)
*        Guess scaling constant for each diluted Planck component.
*     ALPHA( 6 ) = _REAL (Read)
*        Guess emissivity exponent for each diluted Planck component.
*     LGTEMP( 6 ) = _REAL (Read)
*        Guess common logarithm of colour temperature in Kelvin for
*        each diluted Planck component.
*     SF( 6 ) = _INTEGER (Read)
*        For each component I, a value SF(I)=0 indicates that
*        THETA(I) holds a guess which is free to be fitted.
*        A positive value SF(I)=I indicates that THETA(I) is fixed.
*        A positive value SF(I)=J<I indicates that THETA(I) has to
*        keep a fixed offset from THETA(J).
*     AF( 6 ) = _INTEGER (Read)
*        For each component I, a value AF(I)=0 indicates that
*        ALPHA(I) holds a guess which is free to be fitted.
*        A positive value AF(I)=I indicates that ALPHA(I) is fixed.
*        A positive value AF(I)=J<I indicates that ALPHA(I) has to
*        keep a fixed offset to ALPHA(J).
*     TF( 6 ) = _INTEGER (Read)
*        For each component I, a value TF(I)=0 indicates that
*        LGTEMP(I) holds a guess which is free to be fitted.
*        A positive value TF(I)=I indicates that LGTEMP(I) is fixed.
*        A positive value TF(I)=J<I indicates that LGTEMP(I) has to
*        keep a fixed ratio to LGTEMP(J).
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

*  Examples:
*     fitbb in device=xw mask1=10.5 mask2=14.5
*           ncomp=1 theta=0.5 alpha=0 lgtemp=3.5 sf=0 af=1 tf=0
*           comp=1 logfil=planck
*        This fits a Planck curve to the range of frequencies between
*        about 30 GHz and 3E14 Hz. The temperature is guessed to be
*        3000 K. The fit result is reported to the text file PLANCK and
*        stored as component number 1 in the input file's Specdre
*        Extension.
*        Since DIALOG is not turned off, the user will be prompted for
*        improvements of the mask and guess, and will be asked whether
*        the final fit result is to be accepted (stored in the Extension
*        and written to PLANCK.DAT).
*        The XWINDOWS graphics device will display the spectrum before
*        masking, guessing, and fitting. Independent of the DIALOG
*        switch, a plot is produced after fitting.

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
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     30 Jan 1993 (hme):
*        Original version. Adapted from FITTRI.
*     25 Jun 1993 (hme):
*        Revised error reporting.
*        Suppress error conditions from SPACS, but not error reports.
*        Revise graphics access and paging.
*     30 Jan 1995 (hme):
*        Initialise CHISQR to zero.
*     21 Nov 1995 (hme):
*        Open ASCII file with type LIST instead of FORTRAN.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     2006 Oct 19 (TIMJ):
*        Fix CNF_PVAL pointer offsetting
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

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
      PARAMETER ( COMPTY = 'Planck' )
      CHARACTER * ( 32 ) LINENA  ! Name of fitted component
      PARAMETER ( LINENA = 'unidentified component' )
      INTEGER REALSZ             ! No. of bytes per real number
      PARAMETER ( REALSZ = 4 )
      INTEGER MSKDIM             ! Maximum number of mask intervals * 2
      PARAMETER ( MSKDIM = 12 )
      INTEGER MAXBBS             ! Maximum number of components
      PARAMETER ( MAXBBS = 6 )
      INTEGER PLTRES             ! Number of fit curve points to display
      PARAMETER ( PLTRES = 501 )

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
      REAL THETA( MAXBBS )
      REAL ALPHA( MAXBBS )
      REAL TEMPE( MAXBBS )
      INTEGER CFLAGS( MAXBBS )
      INTEGER PFLAGS( MAXBBS )
      INTEGER SFLAGS( MAXBBS )
      LOGICAL REPLY
      INTEGER COMP( MAXBBS )
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
      REAL CVAR( MAXBBS )        ! Theta variances
      REAL PVAR( MAXBBS )        ! Alpha variances
      REAL WVAR( MAXBBS )        ! Temperature variances
      REAL FITX( PLTRES )        ! Fit curve x values
      REAL FITY( PLTRES )        ! Fit curve y values
      REAL DATA( 3 )             ! Parameter values
      REAL VARS( 3 )             ! Parameter variances
      DOUBLE PRECISION COVAR( 9*MAXBBS*MAXBBS ) ! Covariance matrix
      CHARACTER * ( 132 ) IN     ! Input NDF file name
      CHARACTER * ( 32 ) PARATY( 3 ) ! Parameter types
      CHARACTER * ( 64 ) PLABEL( 3 ) ! Plot labels

*  Local Data:
      DATA PARATY / 'Theta', 'alpha', 'log10(T)' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN
      PLOT   = .FALSE.
      FILE   = .FALSE.
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

*  For fitting a Planck curve the labels may not be all that suitable.
*  The axis is certainly assumed to be in lg(Hz). The data are assumed
*  to be proportional to intensity or flux density, but we don't impose
*  any label here.
      PLABEL(1) = 'frequency [log10(Hz)]'
      PLABEL(2) = 'intensity [log10(arb.u.)]'

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
     :                  %VAL( CNF_PVAL(PNTR(2)) ), MSKDIM, MSKUSE,
     :                  MASK, PLTRES, FITX, FITY, NELM,
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
*     Use number of components zero to suppress spurious line
*     report.
         CALL SPD_WZGC( .FALSE., 6, MSKDIM, FITPAR, FITDIM,
     :      IN, NELM,
     :      VARUSE, MSKUSE, MSKELM, RMIN(1), RMAX(1), MASK,
     :      0, CFLAGS, PFLAGS, SFLAGS, THETA, ALPHA, TEMPE,
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
      CALL SPD_CZGE( DIALOG, MAXBBS, NCOMP, FITPAR, FITDIM,
     :   CFLAGS, PFLAGS, SFLAGS, THETA, ALPHA, TEMPE, COVAR, STATUS )

*  Have another chat with the user.
      IF ( DIALOG ) THEN

*     Get the guess data corresponding to the masked data.
         CALL SPD_WZGD( MAXBBS, MSKELM, NCOMP, THETA, ALPHA, TEMPE,
     :                  %VAL( CNF_PVAL(PNTR(4)) ),
     :                  %VAL( CNF_PVAL(PNTR(8)) ), STATUS )

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
         CALL SPD_UAAWR( .FALSE., MSKELM, %VAL( CNF_PVAL(PNTR(9)) ),
     :                   I, CHISQR, STATUS )

*     Calculate guess curve and plot.
         IF ( PLOT ) THEN

*        Fill the guess curve x array linearly to match plotted range.
*        (We could do this more precisely and with only 20 points: Since
*        the profiles are just two straight lines, 3 points per
*        profile are enough. Even where profiles blend, are these points
*        the only ones where a corner can occur. We only need two extra
*        points for the left and right end of the frame.)
            CALL SPD_UAAJR( RMIN(1), RMAX(1), PLTRES, FITX, STATUS )

*        Calculate the guess curve y array from its x array.
            CALL SPD_WZGD( MAXBBS, PLTRES, NCOMP,
     :         THETA, ALPHA, TEMPE, FITX, FITY, STATUS )

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
         CALL SPD_WZGC( .FALSE., 6, MSKDIM, FITPAR, FITDIM, IN, NELM,
     :      VARUSE, MSKUSE, MSKELM, RMIN(1), RMAX(1), MASK,
     :      NCOMP, CFLAGS, PFLAGS, SFLAGS, THETA, ALPHA, TEMPE,
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
      CALL SPD_WFPA( INFO, COVRSX, MSKELM, NCOMP, FITPAR, FITDIM,
     :               %VAL( CNF_PVAL(PNTR(4)) ), CFLAGS, PFLAGS, SFLAGS,
     :               THETA, ALPHA, TEMPE, COVAR, FITTED, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL ERR_RLSE

*  Report fit.
      IF ( FITTED ) THEN

*     Get the fit data corresponding to the masked data.
         CALL SPD_WZGD( MAXBBS, MSKELM, NCOMP, THETA, ALPHA, TEMPE,
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
*     including the temperatures and line integrals. The scaling factor
*     for the COVAR matrix to yield covariance may be 1 or rms-squared.
         IF ( VARUSE ) THEN
            VARSCL = 1.
         ELSE
            VARSCL = CHISQR / ( MSKELM - FITPAR )
         END IF
         CALL SPD_WZGE( NCOMP, FITDIM, CFLAGS, PFLAGS, SFLAGS,
     :      VARSCL, COVAR, CVAR, PVAR, WVAR, STATUS )

*     Calculate fit curve and plot.
         IF ( PLOT ) THEN

*        Fill the fit curve x array linearly to match plotted range.
            CALL SPD_UAAJR( RMIN(1), RMAX(1), PLTRES, FITX, STATUS )

*        Calculate the fit curve y array from its x array.
            CALL SPD_WZGD( MAXBBS, PLTRES, NCOMP,
     :         THETA, ALPHA, TEMPE, FITX, FITY, STATUS )

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
     :   CALL SPD_WZGC( .FALSE., 6, MSKDIM, FITPAR, FITDIM, IN, NELM,
     :      VARUSE, MSKUSE, MSKELM, RMIN(1), RMAX(1), MASK,
     :      NCOMP, CFLAGS, PFLAGS, SFLAGS, THETA, ALPHA, TEMPE,
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
     :      'DATA', 'SPECDRE_FITBB',
     :      PMIN(1), PMAX(1), PMIN(2), PMAX(2), PICID(2), STATUS )
         CALL AGI_SELP( PICID(1), STATUS )
         CALL AGI_NUPIC( WX1(3), WX2(3), WY1(3), WY2(3),
     :      'DATA', 'SPECDRE_FITBB',
     :      PMIN(3), PMAX(3), PMIN(4), PMAX(4), PICID(3), STATUS )
      END IF

*  If we technically have a fit, see if we want to accept it.
      IF ( FITTED .AND. DIALOG )
     :   CALL PAR_GET0L( 'FITGOOD', FITTED, STATUS )

*  If we have an accepted fit.
      IF ( FITTED ) THEN

*     Find out which components to store results in.
*     Must do checks on the numbers given:
*     All must be positive and not equal, there must be NCOMP of them.
*     We do not check the number of values given, since we fill with 0.
         CALL PAR_GET1I( 'COMP', MAXBBS, COMP, I, STATUS )

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
            DATA(1) = THETA(J)
            DATA(2) = ALPHA(J)
            DATA(3) = TEMPE(J)
            VARS(1) = CVAR(J)
            VARS(2) = PVAR(J)
            VARS(3) = WVAR(J)

*        Store parameters.
*        This may not be terribly effective: In the extreme case, 6
*        components are stored and all are hitherto unknown. Then the
*        result structure is reshaped 6 times to accommodate for one
*        more component each.
            CALL SPD_CAAH( NDF(1), 3, 3, '_REAL', '_REAL', '_REAL',
     :         LINENA, COMPTY, PARATY, VAL__BADR,
     :         DATA, VARS, COMP(J), STATUS )

*        Report back, which component was actually used.
            IF ( INFO ) THEN
               CALL MSG_SETI( 'FITBB_T01', COMP(J) )
               CALL MSG_OUT(  'FITBB_M01', 'Used component # ' //
     :            '^FITBB_T01 to store results.', STATUS )
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
            CALL SPD_WZGC( .TRUE., FU, MSKDIM, FITPAR, FITDIM,
     :         IN, NELM,
     :         VARUSE, MSKUSE, MSKELM, RMIN(1), RMAX(1), MASK,
     :         NCOMP, CFLAGS, PFLAGS, SFLAGS, THETA, ALPHA, TEMPE,
     :         COVAR, CHISQR/(MSKELM-FITPAR), STATUS )
         END IF

*     Check status.
         IF ( STATUS .NE. SAI__OK ) GO TO 500
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

*  Return.
      END
