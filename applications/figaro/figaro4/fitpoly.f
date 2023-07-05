      SUBROUTINE FITPOLY( STATUS )
*+
*  Name:
*     FITPOLY

*  Purpose:
*     Fit a polynomial to a spectrum.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FITPOLY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine fits a polynomial to a one-dimensional data set. This
*     can be specified as an NDF section. The data set must extend along
*     the spectroscopic axis.
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
*     The masked data are then fed into the fit routine. The highest
*     polynomial order possible is 7. The fit weights data points
*     according to their errors. The coefficients reported are those of
*     an ordinary polynomial. Let (x,y) be the measurements, y(x) be the
*     polynomial of order n fitting the measurements, c_i (i = 1, ...,
*     n+1) be the fitted coefficients. Then y(x) can be calculated as
*
*        y(x) = c_1 + c_2 x + c_3 x**2 + ... + c_{n+1} x**n
*
*     If the fit is successful, then the result is reported to the
*     screen and plotted on the graphics device. The final plot viewport
*     is saved in the AGI data base and can be used by further
*     applications.
*
*     The result is stored in the Specdre Extension of the input NDF.
*     Optionally, the complete description (input NDF name, mask used,
*     result, etc.) is written (appended) to an ASCII log file.
*
*     Optionally, the application can interact with the user. In that
*     case, a plot is provided before masking and before specifying the
*     polynomial order. After masking and fitting, a screen report and a
*     plot (optional) are provided and the user can improve the
*     parameters. Finally, the result can be accepted or rejected, that
*     is the user can decide whether to store the result in the Specdre
*     Extension or not.
*
*     The screen plot consists of two viewports. The lower one shows the
*     data values (full-drawn bin-style) overlaid with the fit (dashed
*     line-style). The upper box shows the residuals (cross marks)
*     and error bars. The axis scales are arranged such that
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
*     The component fitted by this routine is specified as follows: The
*     line name and laboratory frequency are the default values and are
*     not checked against any existing information in the input's
*     Specdre Extension. The component type is 'polynomial'. The
*     number of parameters allocated to the component is 9. The
*     parameter types are in order of appearance: 'order', 'coeff0',
*     ... 'coeff7'. Unused coefficient are stored as zero.

*  Usage:
*     fitpoly in device=? mask1=? mask2=? order=? comp=? logfil=?

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        If false, the routine will issue only error messages and no
*        informational messages. [YES]
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
*     ORDER = _INTEGER (Read)
*        The polynomial order of the fit. Must be between 0 and 7. [1]
*     REMASK = _LOGICAL (Read)
*        Reply YES to have another chance for improving the mask.
*        [NO]
*     REGUESS = _LOGICAL (Read)
*        Reply YES to have another chance for improving the guess and
*        fit. [NO]
*     FITGOOD = _LOGICAL (Read)
*        Reply YES to store the result in the Specdre Extension. [YES]
*     COMP = _INTEGER (Read and Write)
*        The results are stored in the Specdre Extension of the data.
*        This parameter specifies which existing component is being
*        fitted. It should be between zero and the number of components
*        that are currently stored in the Extension. Give zero for a
*        hitherto unknown component. If COMP is given as zero or if it
*        specifies a component unfit to store the results of this
*        routine, then a new component will be created in the result
*        storage structure. In any case this routine will report which
*        component was actually used and it will deposit the updated
*        value in the parameter system. [1]
*     FITCOEFFS( ) = _REAL (Write)
*        The coefficients of the fitted polynomial. The number of
*        coefficients returned depends on the order of the fitted
*        polynomial. No more than eight coefficients will be returned.
*     LOGFIL = FILENAME (Read)
*        The file name of the log file. Enter the null value (!) to
*        disable logging. The log file is opened for append. [!]

*  Examples:
*     fitpoly in device=! mask1=2.2 mask2=3.3 order=3 comp=1 logfil=!
*        IN is a 1-D NDF. A 3rd order fit is made to the abscissa range
*        between 2.2 and 3.3. The result is stored in component number 1
*        of the result structure in the Specdre Extension of IN. The
*        plot device and ASCII log file are de-selected.
*     fitpoly in(,15) device=xw mask1=[2.0,2.3,3.4] mask2=[2.1,3.2,4.0]
*           order=2 comp=0 logfil=myfil
*        Here IN is 2-D and the 15th row is selected as the 1-D input
*        for the fit. The mask consists of three intervals
*        [2.0;2.1] U [2.3;3.2] U [3.4,4.0]. The fit is a parabola. Space
*        for a new component is created for storage in the Specdre
*        Extension. The plot device is xwindows.
*     fitpoly in(,20) device=xw mask1=[2.0,2.3,3.4] mask2=[2.1,3.2,4.0]
*           order=4 comp=2 logfil=myfil
*        In a follow-up from the previous example, now the 20th row is
*        fitted with 4th order. If in the previous run the routine told
*        us that it had used component number 2, then COMP=2 is what we
*        want to use to store a similar fit for a different row.
*        The first time round, the description of component 2 was
*        created, saying that it is a polynomial with order of 7
*        or less etc. And the fit result for the 15th row was stored in
*        an array that has space for all rows in the input file.
*        So the second time round, FITPOLY checks whether component 2
*        is suitable, whether it is a polynomial with maximum
*        order 7. It then stores the new result for the 20th row in the
*        place reserved for this row.
*        Gradually all rows can be fitted and their results stored in
*        the Extension. Possibly this could be automated by writing a
*        looping ICL procedure or shell script.
*        In the end the corresponding results for all rows are stored in
*        one data structure, and could for example be converted into a
*        plot of the n-th parameter value versus row number.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.
*
*     This routine works in situ and modifies the input file.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     BEC: Brad Cavanagh (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10 Jul 1992 (hme):
*        Adapt FITCHEBY from FITGAUSS. Change from DSA to NDF. Use
*        Specdre Extension. Disuse SPF-routines.
*     09 Sep 1992 (hme):
*        Don't set title.
*     25 Jun 1993 (hme):
*        Revise error reporting. Revise graphics access and paging.
*     21 Nov 1995 (hme):
*        Use PDA instead of NAG. Hence fit a Taylor series instead of a
*        Chebyshev series. Actually fit an ordinary polynomial (Taylor
*        expansion about x = 0).
*        Open ASCII file as type LIST instead of FORTRAN.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     2006 Oct 19 (TIMJ):
*        Fix CNF_PVAL pointer offsetting
*     2007 April 2 (BEC):
*        Return fitted polynomial coefficients via parameter system.
*     2023 Jul 5 (GSB):
*        Ensure the routine exits with bad status if fitting was
*        not successful.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INCLUDE 'PAR_ERR'          ! Status returned by PAR_
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status
      INTEGER IFAIL1, IFAIL2     ! PDA stati

*  Local Constants:
      CHARACTER * ( 32 ) COMPTY  ! Type of fitted component
      PARAMETER ( COMPTY = 'polynomial' )
      CHARACTER * ( 32 ) LINENA  ! Type of fitted component
      PARAMETER ( LINENA = 'unidentified component' )
      INTEGER DBLSIZ             ! Number of bytes in a _DOUBLE
      PARAMETER ( DBLSIZ = 8 )
      INTEGER MSKDIM             ! Maximum number of mask intervals * 2
      PARAMETER ( MSKDIM = 12 )
      INTEGER MAXPAR             ! Maximum polynom order plus 1
      PARAMETER ( MAXPAR = 8 )
      INTEGER PLTRES             ! Number of fit curve points to display
      PARAMETER ( PLTRES = 501 )

*  Local Variables:
*  NDF(1): input data/variance NDF.
*  NDF(2): input centre NDF.
*  NDF(3): masked array.
*  NDF(4): residuals.
*  NDF(5): two work spaces for error plotting and residual finding.
*  NDF(6): work space (two really, pointer 5).
*  PNTR(1): input centre pointer.
*  PNTR(2): input data value pointer.
*  PNTR(3): input variance pointer.
*  PNTR(4): masked array pointer.
*  PNTR(5): PDA_DPOLFT first workspace pointer (NDF(6)).
*  PNTR(5)+DBLSIZ*NELM: PDA_DPOLFT returned coefficient matrix (NDF(6)).
*  PNTR(6): residual centre pointer.
*  PNTR(7): residual data pointer.
*  PNTR(8): first workspace from NDF(5).
*  PNTR(9): second workspace from NDF(5).
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
      INTEGER ORDER
      LOGICAL PLOT
      LOGICAL REPLY
      INTEGER COMP
      LOGICAL FILE
      LOGICAL MSKINI             ! True if mask to be set to full range
      LOGICAL FITTED             ! True if fit was successful
      INTEGER I, J               ! Loop indices
      INTEGER NDF( 6 )           ! Input NDF identifier
      INTEGER PNTR( 9 )          ! Mapped array pointers
      INTEGER NELM               ! Number of data points
      INTEGER PICID( 5 )         ! AGI picture
      INTEGER ZONID              ! SGS zone identifier
      INTEGER FD                 ! FIO descriptor
      INTEGER FU                 ! Fortran unit number
      INTEGER MSKELM             ! Number of masked data points
      INTEGER IMIN               ! First x-array elem. that passed mask
      INTEGER IMAX               ! Last x-array element that passed mask
      INTEGER NDEG               ! Actual order fitted
      REAL WX1( 3 )              ! AGI world coordinates
      REAL WX2( 3 )              ! AGI world coordinates
      REAL WY1( 3 )              ! AGI world coordinates
      REAL WY2( 3 )              ! AGI world coordinates
      REAL RMIN( 4 )             ! Minima
      REAL RMAX( 4 )             ! Maxima
      REAL PMIN( 4 )             ! Plot minima
      REAL PMAX( 4 )             ! Plot maxima
      REAL CHISQR                ! Masked chi-squared or square of rms
      REAL FITX( PLTRES )        ! Fit curve x values
      REAL FITY( PLTRES )        ! Fit curve y values
      REAL DATA( MAXPAR+1 )      ! Parameter values
      REAL VARS( MAXPAR+1 )      ! Parameter variances
      CHARACTER * ( 132 ) IN     ! Input NDF file name
      CHARACTER * ( 32 ) PARATY( MAXPAR+1 ) ! Parameter types
      CHARACTER * ( 64 ) PLABEL( 3 ) ! Plot labels
      DOUBLE PRECISION DTEMP1, DTEMP2 ! Temporary numbers
      DOUBLE PRECISION EPS       ! Control and rms for PDA_DPOLFT
      DOUBLE PRECISION COEFF( MAXPAR ) ! Coefficient vector

*  Local data.
      DATA PARATY / 'order',
     :   'coeff0', 'coeff1', 'coeff2', 'coeff3',
     :   'coeff4', 'coeff5', 'coeff6', 'coeff7' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN
      PLOT   = .FALSE.
      FILE   = .FALSE.
      REPLY  = .FALSE.

*  Get switch parameters.
      CALL PAR_GET0L( 'INFO',   INFO,   STATUS )
      CALL PAR_GET0L( 'VARUSE', VARUSE, STATUS )
      CALL PAR_GET0C( 'DIALOG', DIALCH, STATUS )
      CALL CHR_UCASE( DIALCH )
      DIALOG = ( DIALCH .EQ. 'T' .OR. DIALCH .EQ. 'Y' )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Data and workspace access. This is rather complex and handled by a
*  specific subroutine. It will also return suitable plot labels.
      CALL SPD_CZFD( DIALOG, VARUSE, MAXPAR,
     :   NELM, NDF, PNTR, IN, PLABEL, STATUS )
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
     :                  %VAL( CNF_PVAL(PNTR(6)) ),
     :                  %VAL( CNF_PVAL(PNTR(7)) ),
     :                  %VAL( CNF_PVAL(PNTR(8)) ),
     %                  %VAL( CNF_PVAL(PNTR(9)) ), PMIN, PMAX, STATUS )

*  Come here to redo mask.
      MSKINI = .TRUE.
 1    CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Find out and apply mask.
      CALL SPD_CAAGD( DIALOG, MSKINI, .TRUE., VARUSE, .FALSE.,
     :                NELM, MSKDIM, %VAL( CNF_PVAL(PNTR(1)) ),
     :                %VAL( CNF_PVAL(PNTR(2)) ),
     :                %VAL( CNF_PVAL(PNTR(3)) ), 0., MSKUSE, MASK,
     :                IMIN, IMAX, RMIN, RMAX, MSKELM,
     :                %VAL( CNF_PVAL(PNTR(4)) ), STATUS )
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
     :                     %VAL( CNF_PVAL(PNTR(6)) ),
     :                     %VAL( CNF_PVAL(PNTR(7)) ),
     :                     %VAL( CNF_PVAL(PNTR(8)) ),
     :                     %VAL( CNF_PVAL(PNTR(9)) ), PMIN, PMAX,
     :                     STATUS )
         END IF

*     Report to screen.
         CALL SPD_WZFC( .FALSE., .FALSE., 6, IN, VARUSE, NELM,
     :      MSKDIM, MSKUSE, MSKELM, MASK, RMIN(1), RMAX(1),
     :      MAXPAR, ORDER, DATA(2), CHISQR, STATUS )

*     Ask if improvement on mask required.
         CALL PAR_GET0L( 'REMASK', REPLY, STATUS )
         CALL PAR_CANCL( 'REMASK', STATUS )
      END IF

*  Try again to mask?
      IF ( REPLY ) GO TO 1

*  Come here to redo guess and fit.
 2    CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Select guess, i.e. polynomial order.
      CALL PAR_GET0I( 'ORDER', ORDER, STATUS )
      CALL PAR_CANCL( 'ORDER', STATUS )
      CALL PAR_DEF0I( 'ORDER', ORDER, STATUS )

*  Apply fit.
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL ERR_MARK
         FITTED = .FALSE.
         IFAIL2 = 0
         EPS = 0D0
         CALL PDA_DPOLFT( MSKELM,  %VAL( CNF_PVAL(PNTR(4)) ),
     :                    %VAL( CNF_PVAL(PNTR(4))+DBLSIZ*MSKELM ),
     :                    %VAL( CNF_PVAL(PNTR(4))+2*DBLSIZ*MSKELM ),
     :                    ORDER, NDEG, EPS, %VAL( CNF_PVAL(PNTR(5)) ),
     :                    IFAIL1,
     :                    %VAL( CNF_PVAL(PNTR(5))+DBLSIZ*NELM ),
     :                    IFAIL2 )
         STATUS = IFAIL2
         IF ( STATUS .NE. 0 ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL ERR_ANNUL( STATUS )
         END IF
      CALL ERR_RLSE

*  If failed to get a fit, report and move on to improvement
*  dialogue.
      IF ( NDEG .NE. ORDER .OR. IFAIL1 .NE. 1 .OR. IFAIL2 .NE. 0 ) THEN
         CALL MSG_OUT(  'FITPOLY_M01', 'No fit achieved. ' //
     :      'PDA_DPOLFT returned with error status.', STATUS )

*  Else (fit successful), work out coefficients.
      ELSE

*     Get the ordinary polynomial coefficient (as DOUBLE array).
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         CALL ERR_MARK
         CALL PDA_DPCOEF( NDEG, 0D0, COEFF,
     :                    %VAL( CNF_PVAL(PNTR(5))+DBLSIZ*NELM ),
     :                    STATUS )
         IF ( STATUS .NE. 0 ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE

*     If coefficients could not be calculated, report and move on to
*     improvement dialogue.
         IF ( IFAIL2 .NE. 0 ) THEN
            CALL MSG_OUT(  'FITPOLY_M03', 'Coefficients unknown. ' //
     :         'PDA_DPCOEF returned with error status.', STATUS )

*     Else (fit successful, and coefficients known),
*     evaluate, plot, report.
         ELSE

*        Set the success flag.
            FITTED = .TRUE.

*        Here come the REAL vectors with all the information to be put
*        into the result NDF of the Specdre Extension.
            DATA(1) = FLOAT(ORDER)
            VARS(1) = 0.
            DO 3 I = 1, MAXPAR

*           Reorganise COEFF elements so that the result for ORDER is
*           in the first ORDER+1 elements, and that the rest till
*           element no. 8 is zero.
               IF ( I .LE. ORDER+1 ) THEN
                  DATA(I+1) = SNGL(COEFF(I))
                  VARS(I+1) = VAL__BADR
               ELSE
                  DATA(I+1) = 0.
                  VARS(I+1) = 0.
                  COEFF(I)  = 0D0
               END IF
 3          CONTINUE

*        Get residual centres as single precision copy of masked centres.
            CALL VEC_DTOR( .FALSE., MSKELM, %VAL( CNF_PVAL(PNTR(4)) ),
     :                     %VAL( CNF_PVAL(PNTR(6)) ), I, J, STATUS )

*        Get the corresponding fit data.
*        The fit routine returned these in its first work space. We need
*        only copy to single precision.
            CALL VEC_DTOR( .FALSE., MSKELM, %VAL( CNF_PVAL(PNTR(5)) ),
     :                     %VAL( CNF_PVAL(PNTR(8)) ), I, J, STATUS )

*        Get single precision copy of masked data.
            CALL VEC_DTOR( .FALSE., MSKELM,
     :                      %VAL( CNF_PVAL(PNTR(4))+DBLSIZ*MSKELM ),
     :                      %VAL( CNF_PVAL(PNTR(9)) ), I, J, STATUS )

*        Get residuals as difference of masked data minus fit data. Also
*        need the range of residuals for plotting purposes.
            CALL VEC_SUBR( .FALSE., MSKELM, %VAL( CNF_PVAL(PNTR(9)) ),
     :                     %VAL( CNF_PVAL(PNTR(8)) ),
     :                     %VAL( CNF_PVAL(PNTR(7)) ), I, J, STATUS )
            CALL SPD_UAAAR( .FALSE., MSKELM, %VAL( CNF_PVAL(PNTR(7)) ),
     :                      RMIN(4), RMAX(4), STATUS )

*        Get single precision copy of masked weights. Can skip fit data.
            CALL VEC_DTOR( .FALSE., MSKELM,
     :                     %VAL( CNF_PVAL(PNTR(4))+2*DBLSIZ*MSKELM ),
     :                     %VAL( CNF_PVAL(PNTR(8)) ), I, J, STATUS )

*        Multiply residuals with weights. Can skip masked data.
            CALL VEC_MULR( .FALSE., MSKELM, %VAL( CNF_PVAL(PNTR(7)) ),
     :                     %VAL( CNF_PVAL(PNTR(8)) ),
     :                     %VAL( CNF_PVAL(PNTR(9)) ), I, J, STATUS )

*        Get CHISQR as the sum of squares over the array hiding behind
*        pointer 9. This is either chi-squared or
*        rms = SQRT( CHISQR / degrees of freedom )
*        Note that the masked weights (1/error) are still behind pointer 8.
*        They are needed below for plotting.
            CALL SPD_UAAWR( .FALSE., MSKELM, %VAL( CNF_PVAL(PNTR(9)) ),
     :                      I, CHISQR, STATUS )

*        Calculate fit curve and plot.
            IF ( PLOT ) THEN

*           Fill the fit curve x array linearly to match plotted range.
               CALL SPD_UAAJR( RMIN(1), RMAX(1), PLTRES, FITX, STATUS )

*           Calculate the fit curve y array from its x array.
*           If this fails for any element, bail out to the end of this
*           IF block.
               IF ( STATUS .NE. SAI__OK ) GO TO 500
               CALL ERR_MARK
               DO 4 I = 1, PLTRES
                  CALL PDA_DP1VLU( NDEG, 0, DBLE(FITX(I)), DTEMP1,
     :                             DTEMP2, %VAL( CNF_PVAL(PNTR(5))+
     :                             DBLSIZ*NELM ), STATUS )
                  IF ( STATUS .NE. 0 ) THEN
                     CALL ERR_FLUSH( STATUS )
                     CALL ERR_ANNUL( STATUS )
                     CALL ERR_RLSE
                     GO TO 400
                  END IF
                  FITY(I) = SNGL(DTEMP1)
 4             CONTINUE
               CALL ERR_RLSE

*           Calculate the bottom and top tips of error bars as residual
*           plus or minus 1/weight.
               IF ( VARUSE ) THEN
                  CALL SPD_WAAK( .FALSE., MSKELM,
     :                           %VAL( CNF_PVAL(PNTR(7)) ),
     :                           %VAL( CNF_PVAL(PNTR(8)) ),
     :                           %VAL( CNF_PVAL(PNTR(8)) ),
     :                           %VAL( CNF_PVAL(PNTR(9)) ),
     :                           STATUS )
               END IF

*           Plot (the whole lot).
               CALL SPD_WAAL( .TRUE., FITTED, VARUSE, ZONID, PLABEL,
     :                        RMIN, RMAX, NELM,
     :                        %VAL( CNF_PVAL(PNTR(1)) ),
     :                        %VAL( CNF_PVAL(PNTR(2)) ), MSKDIM, MSKUSE,
     :                        MASK, PLTRES, FITX, FITY, MSKELM,
     :                        %VAL( CNF_PVAL(PNTR(6)) ),
     :                        %VAL( CNF_PVAL(PNTR(7)) ),
     :                        %VAL( CNF_PVAL(PNTR(8)) ),
     :                        %VAL( CNF_PVAL(PNTR(9)) ), PMIN, PMAX,
     :                        STATUS )

*           Come here if fit could not be evaluated for plot.
 400           CONTINUE
            END IF

*        Report to screen.
            CALL SPD_WZFC( FITTED, .FALSE., 6, IN, VARUSE, NELM,
     :         MSKDIM, MSKUSE, MSKELM, MASK, RMIN(1), RMAX(1),
     :      MAXPAR, ORDER, DATA(2), CHISQR, STATUS )

         END IF
      END IF

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
     :      'DATA', 'SPECDRE_FITPOLY',
     :      PMIN(1), PMAX(1), PMIN(2), PMAX(2), PICID(2), STATUS )
         CALL AGI_SELP( PICID(1), STATUS )
         CALL AGI_NUPIC( WX1(3), WX2(3), WY1(3), WY2(3),
     :      'DATA', 'SPECDRE_FITPOLY',
     :      PMIN(3), PMAX(3), PMIN(4), PMAX(4), PICID(3), STATUS )
      END IF

*  If we technically have a fit, see if we want to accept it.
      IF ( FITTED .AND. DIALOG )
     :   CALL PAR_GET0L( 'FITGOOD', FITTED, STATUS )

*  If we have an acceptable fit.
      IF ( FITTED ) THEN

*     Find out in which component to store results.
         CALL PAR_GET0I( 'COMP', COMP, STATUS )

*     We store 9 parameters, even if that's bigger than ORDER+2.
         CALL SPD_CAAH( NDF(1), 9, 9, '_REAL', '_REAL', '_REAL',
     :      LINENA, COMPTY, PARATY, VAL__BADR,
     :      DATA, VARS, COMP, STATUS )

*     Report back, which component was actually used.
         IF ( INFO ) THEN
            CALL MSG_SETI( 'FITPOLY_T02', COMP )
            CALL MSG_OUT(  'FITPOLY_M02', 'Used component # ' //
     :         '^FITPOLY_T02 to store results.', STATUS )
         END IF
         CALL PAR_PUT0I( 'COMP', COMP, STATUS )

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
            CALL SPD_WZFC( FITTED, .TRUE., FU, IN, VARUSE, NELM,
     :         MSKDIM, MSKUSE, MSKELM, MASK, RMIN(1), RMAX(1),
     :         MAXPAR, ORDER, DATA(2), CHISQR, STATUS )
         END IF

*     Return the coefficients via parameter.
         CALL PAR_PUT1R( 'FITCOEFFS', ORDER + 1 , DATA(2), STATUS )

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
      CALL NDF_END( STATUS )

*  Exit with bad status if fitting failed?
      IF ( STATUS .EQ. SAI__OK .AND. .NOT. FITTED ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FITPOLY_E01', 'FITPOLY: Error: ' //
     :      'Fitting may not have been successful.', STATUS )
      END IF

*  Return.
      END
