      SUBROUTINE FINDSP
*+
*  Name:
*     FINDSP

*  Purpose:
*     Locate spectra in fibre frame.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FINDSP

*  Description:
*     This routine locates spectra in a large fibre frame and produces a
*     polynomial file. The polynomial file has a version 2 format.
*     Version 1 format uses the coefficients of a Chebyshev series,
*     while version 2 format uses ordinary polynomial coeffients.
*
*     The technique of this routine is to
*
*      1 Compress the data array,
*      2 Follow ridges from start positions by centroiding,
*      3 Fit a polynomial Y(X) to the centroids,
*      4 Write the polynomial coefficients to a text file.
*
*     The text file can be read by the applications OVERPF and POLEXT.
*     Those applications will also be able to read text files in version
*     1 format.

*  ADAM Parameters:
*     IMAGE = LITERAL (Read)
*        The fibre frame - one with distorted fibre spectra equally
*        spaced.
*     BLACK = _REAL (Read)
*        The data value below which the image display is to have the
*        background colour. The display is scaled linearly between the
*        data values specified as BLACK and WHITE.
*     WHITE = _REAL (Read)
*        The data value above which the image display is to have the
*        foreground colour. The display is scaled linearly between the
*        data values specified as BLACK and WHITE.
*     NUMFIB = _INTEGER (Read)
*        The total number of fibres used in the observation, including
*        any dud fibres.
*     NORDER = _INTEGER (Read)
*        The order of the polynomial to be fitted along each spectrum.
*        The default is 6 and the maximum order allowed is 10. An even
*        order is suggested by the presence of 'barrel' distortion.
*     NPTS = _INTEGER (Read)
*        The image is compressed in the X (wavelength) direction before
*        the centroids are determined. This parameter fixes the number
*        X-direction bins in the compressed frame.  This parameter also
*        by definition is the number of points along the spectrum to be
*        used for fitting the polynomial. Choice of this parameter is a
*        trade-off between having enough points along the spectra that
*        the the curved spectra can be reliably followed and having
*        enough S/N in the compressed image to determine a reliable
*        centroid.
*     FWCENT = _INTEGER (Read)
*        The 'full-width' of the centroiding range in the vertical
*        direction.
*     CFW = _REAL (Read)
*        To get the initial centre for the centroiding the program does
*        a linear extrapolation from the last two centroids. The program
*        searches out from the previously determined central centroids.
*        In order to supress large fluctuations that sometimes occur it
*        is necessary to have damping in the extrapolation.  If fact
*        this 'Centroid Weighting Function' parameter is the constant
*        that the true gradient of the linear extrapolation is
*        multiplied by to guess the next centroid. Hence a value of CFW
*        less than 1 damps the extrapolation toward the horizontal.
*     YFIRST = _REAL (Read)
*        The position of the centre of the first spectrum. This is
*        expressed as the number of pixels up from the bottom of the
*        image as viewed on the ARGS.  Note that the default is only a
*        guess from the size of the image.
*     YSEP = _REAL (Read)
*        The average number of pixels separating each spectrum in the
*        input image. Again the default value represents a guess.
*     PFILE = LITERAL (Write)
*        The file to which the results of the spectrum fitting performed
*        by FINDSP is to be written.  If no extension is specified,
*        `.pol' is used.
*     ADJUST = _LOGICAL (Read)
*        Used to ask whether centroid start points need adjustment.
*     CHGPAR = _LOGICAL (Read)
*        Used to ask whether analysis to be repeated with changed
*        parameters.
*     REJECT = _REAL (Read)
*        The number of a fibre to be rejected.
*     CHGREJ = _LOGICAL (Read)
*        Used to ask whether the set of fibres to be rejected should be
*        revised.

*  Authors:
*     jrl: John Lucey (AAO, Durham)
*     ks: Keith Shortridge (AAO)
*     jm: Jo Murray (RAL, Starlink)
*     jms: {author_name} (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     {date} (jrl):
*        Original version.
*     31 Jan 1985 (ks):
*        Originally modified for Figaro.
*     06 Mar 1987 (ks):
*        Finally released as part of Figaro.
*     Summer 1988 (jrl):
*        Modified for GKS 7.2.
*     16 Sep 1988 (ks):
*        Explicit setting of overlay device removed.  PENCOLS and
*        JRL_PEN given FIG_ prefix and added to Figaro standard library.
*     23 Jan 1989 (jm):
*        Modified to use DSA_ routines. Dynamic memory handling changed
*        to use DYN_ routines.
*     22 Feb 1991 (jms):
*        Added PAR_ABORTs and STATUS checks to support user requested
*        aborts. In FINDSP_WORK SGS closes properly now.
*     01 Jul 1991 (hme):
*        Increase string lengths for IMAGEDEV, OVERDEV in FINDSP_WORK to
*        32.
*     30 Jan 1992 (hme):
*        Major cosmetics to improve behaviour in LSE (an editor).
*        Changed STOP to RETURN. Enquire workstation properties rather
*        than assuming 256 colours. Some rearrangement of code. Taken
*        out most informational messages. If there is no independent
*        overlay device, use the device itself, and disuse lowest 5
*        colours for image display.
*     20 Oct 1992 (hme):
*        INCLUDE changed. Remove call to STR$UPCASE, which folded the
*        device name. Open list file with lowercase extension. Make the
*        default file name lowercase too. Replace DSA_WRUSER with
*        PAR_WRUSER. Remove equality tests between logicals. Try to
*        tackle GCA with declaring NX,NY,RNX,RNY. Next try to use
*        variables for GCA(0.5..
*     26 Oct 1992 (hme):
*        It turns out that the Unix systems have GKS 7.4 while VAXs have
*        GKS 7.2. Among the few changes are additional arguments for
*        GCA.
*     11 Mar 1993 (hme):
*        Abort orderly when BLACK and WHITE are equal.
*     27 Jul 1993 (hme):
*        Disuse PAR_Q*. Add parameters ADJUST, CHGPAR, REJECT, CHGREJ.
*     25 Apr 1995 (hme):
*        The time has come. No longer use NAG, and take the opportunity
*        to no longer use GKS or SGS either. This is a re-write of the
*        code, although the structure remains.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Local Variables:
      LOGICAL ABORT              ! Status from work routine
      INTEGER STATUS             ! DSA status
      INTEGER PGSTAT             ! PGBEG status
      INTEGER IGNORE             ! Ignored status
      INTEGER LU                 ! Fortran unit for polynomial file
      INTEGER NDIM               ! Number of image dimensions
      INTEGER NELM               ! Number of elements in image
      INTEGER DIMS( 2 )          ! Image dimensions
      INTEGER IPTR               ! Pointer for image data
      INTEGER SLOT               ! Slot number - ignored
      INTEGER LDATE              ! Length of character string DATE
      INTEGER LDAY               ! Length of character string DAY
      INTEGER LTIME              ! Length of character string TIME
      CHARACTER * ( 32 )  DEVICE ! Graphics device
      CHARACTER * ( 64 )  OBJECT ! Name of object associated with IMAGE
      CHARACTER * ( 256 ) IMAGE  ! Name of IMAGE file
      CHARACTER * ( 256 ) PFILE  ! Name of polynomial file
      CHARACTER * ( 256 ) FILNAM ! Full name of polynomial file
      CHARACTER * ( 9 )  DAY     ! Day
      CHARACTER * ( 20 ) DATE    ! Date
      CHARACTER * ( 20 ) TIME    ! Time

*  Internal References:
      LOGICAL PAR_ABORT
      INTEGER FIG_PGBEG
      INTEGER ICH_LEN

*.

*  Open DSA.
      STATUS = 0
      CALL DSA_OPEN( STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500

*  Open the image, check it is 2-D, map the data.
      CALL DSA_INPUT( 'IMAGE', 'IMAGE', STATUS )
      CALL DSA_DATA_SIZE( 'IMAGE', 2, NDIM, DIMS, NELM, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500
      IF ( NDIM .NE. 2 ) THEN
         CALL PAR_WRUSER(
     :      'FINDSP: Error: Image data is not two-dimensional.',
     :      IGNORE )
         GO TO 500
      END IF
      CALL DSA_MAP_DATA( 'IMAGE', 'READ', 'FLOAT', IPTR, SLOT, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500

*  Get the name of the file to write the coefficients to. Then try to
*  open the file as new. If the user gives no file name extension then
*  .pol is used.
      CALL PAR_RDCHAR( 'PFILE', 'pfile', PFILE )
      IF ( PAR_ABORT() ) GO TO 500
      CALL DSA_OPEN_TEXT_FILE( PFILE, '.pol', 'NEW', .TRUE.,
     :   LU, FILNAM, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500

*  Open the graphics device, set up view port.
*  The view port uses identical scales in x and y, initially no box is
*  drawn.
      CALL VAR_GETCHR( 'SOFT', 0, 0, DEVICE, STATUS )
      IF ( STATUS .NE. 0 ) THEN
         CALL PAR_WRUSER( 'FINDSP: Error: No display specified. ' //
     :      'Use SOFT to select a graphics device.', IGNORE )
         GO TO 500
      END IF
      PGSTAT = FIG_PGBEG( 0, DEVICE, 1, 1 )
      IF ( PGSTAT .NE. 1 ) THEN
         CALL PAR_WRUSER( 'FINDSP: Error opening display.', IGNORE )
         GO TO 500
      END IF
      CALL PGASK( .FALSE. )
      CALL PGENV( 0., FLOAT(DIMS(1)), 0., FLOAT(DIMS(2)), 0, -2 )

*  Call the work routine. This handles all graphics, interaction, and
*  file output.
      CALL FINDSP_WORK( DIMS(1), DIMS(2), %VAL(CNF_PVAL(IPTR)), LU,
     :                  ABORT )
      IF ( ABORT ) GO TO 400

*  Append some comments to the output text file.
      CALL GEN_TIME( 6, DAY, LDAY, DATE, LDATE, TIME, LTIME )
      CALL DSA_GET_ACTUAL_NAME( 'IMAGE', IMAGE, STATUS )
      WRITE( LU, '(/2A)' ) ' Polynomial fits based on data from ',
     :    IMAGE(:ICH_LEN(IMAGE))
      OBJECT = ' '
      CALL DSA_OBJECT_NAME( 'IMAGE', OBJECT, STATUS )
      IF( STATUS .NE. 0 ) GO TO 500
      WRITE( LU, '(1X,A)' ) OBJECT
      WRITE( LU, '(/A/1X,5A/)' )
     :   ' Generated by Program FINDSP, run on ',
     :   DAY(:LDAY), ' ', DATE(:LDATE), ' at ', TIME(:LTIME)

*  Report file name to terminal.
      CALL PAR_WRUSER( 'Polynomial fits written to ' //
     :   FILNAM(:ICH_LEN(FILNAM)), IGNORE )

*  Close graphics device.
 400  CONTINUE
      CALL FIG_PGEND

*  Close DSA.
 500  CONTINUE
      CALL DSA_CLOSE( STATUS )

*  Return.
      END


      SUBROUTINE FINDSP_WORK( NX, NY, RAY, LU, ABORT )
*+
*  Name:
*     FINDSP_WORK

*  Purpose:
*     Do the work for FINDSP.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FINDSP_WORK( NX, NY, RAY, LU, ABORT )

*  Description:
*     MXNPTS  is the max. X dimension of compressed array CMRAY
*     MYSIZE  is the max. Y dimension of input image
*     MXNFIB  is the max. num. of fibres
*     MXORP1  is the max. order of poly. allowed plus 1
*     ABORT   error code OK=0, ERROR=1

*  History:
*     16-AUG-1996 (MJCL):
*       Increased maximum Y-dimension to 4096.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL RAY( NX, NY )
      INTEGER LU

*  Status:
      LOGICAL ABORT

*  Local Constants:
*  (There is a local array with dimension MXNPTS which is used
*  occasionally with fibre number as index. So MXNFIB should not be
*  larger than MXNPTS.)
      INTEGER MXNPTS             ! Maximum x dimension after compression
      PARAMETER ( MXNPTS = 100 )
      INTEGER MYSIZE             ! Maximum y dimension
      PARAMETER ( MYSIZE = 4096 )
      INTEGER MXNFIB             ! Maximum number of fibres
      PARAMETER ( MXNFIB = 100 )
      INTEGER MXORP1             ! Maximum polynomial order plus 1
      PARAMETER ( MXORP1 =  11 )

*  Local Variables:
      LOGICAL PQUEST
      INTEGER NUMFIB
      INTEGER NORDER
      INTEGER NPTS
      INTEGER FWCENT
      INTEGER NOK
      REAL BLACK, WHITE
      REAL CFW
      REAL YS, SHIFT
      LOGICAL ODD                ! True if FWCENT is odd
      LOGICAL FIBOK( MXNFIB )    ! False if fibre rejected
      INTEGER I, J, IX, IY, IFIB ! Loop indices
      INTEGER IXS, IXE           ! Loop index bounds
      INTEGER IYLOW, IYTOP       ! Loop index bounds
      INTEGER IYS                ! Truncated YS
      INTEGER NOFIB              ! Number of rejected fibres
      INTEGER IGNORE             ! Ignored status
      INTEGER NDEG, IFAIL, IFAIL2 ! PDA stati
      INTEGER HNPTS              ! Half the number of compressed points
      INTEGER HYWID              ! Half of FWCENT
      REAL XBIN                  ! Compression factor in X
      REAL SUM, YSUM, FREQ       ! Variables to add up things
      REAL VALUE                 ! Temporary floating point value
      REAL TR( 6 )               ! PGGRAY coordinate transform
      REAL CMRAY( MXNPTS, MYSIZE ) ! Compressed image
      REAL SMRAY( MXNPTS, MYSIZE ) ! Smoothed compressed image
      DOUBLE PRECISION RMS         ! R.m.s. from polynomial fit
      DOUBLE PRECISION X( MXNPTS ) ! X coordinates in fibre trace
      DOUBLE PRECISION Y( MXNPTS ) ! Y coordinates in fibre trace
      DOUBLE PRECISION W( MXNPTS ) ! Y coordinates in fibre trace
      DOUBLE PRECISION YY( MXNPTS, MXNFIB ) ! dto, for all fibres
      DOUBLE PRECISION A2( 3 * MXNPTS + 3 * MXORP1 ) ! PDA_DPOLFT result
      DOUBLE PRECISION COEF( MXORP1 ) ! Ordinary polynomial coeffs
      DOUBLE PRECISION YLEVEL( MXNPTS ) ! Y coordinates in fitted trace
      CHARACTER * ( 79 ) TEXT    ! Text buffer

*  Internal References:
      LOGICAL PAR_ABORT
      INTEGER ICH_LEN

*  Data Statements.
      DATA TR / 0., 1., 0., 0., 0., 1. /

*.


*  Preliminaries.
*  ==============

*  Set abort flag. This will be reset if and only if all goes well in
*  this routine.
      ABORT = .TRUE.

*  Check y size of image. The compressed image is in a local buffer with
*  a fixed maximum size.
      IF ( NY .GT. MYSIZE ) THEN
         CALL PAR_WRUSER(
     :      'FINDSP: Error: Y dimension of image too big.', IGNORE )
         GO TO 500
      END IF
      WRITE( TEXT, 1 ) NX, NY
      CALL PAR_WRUSER( TEXT, IGNORE )
 1    FORMAT( 'Image size is ', I4, ' by ', I4 )

*  Find approximate image range and seek confirmation from user.
      BLACK = +1E20
      WHITE = -1E20
      DO 3 IY = 1, NY
         DO 2 IX = INT(FLOAT(NX)/2.+0.5-FLOAT(NX)/15.),
     :                INT(FLOAT(NX)/2.+0.5+FLOAT(NX)/15.)
            BLACK = MIN( BLACK, RAY(IX,IY) )
            WHITE = MAX( BLACK, RAY(IX,IY) )
 2       CONTINUE
 3    CONTINUE
      CALL PAR_RDVAL( 'BLACK', -1E20, 1E20, BLACK, ' ', VALUE )
      BLACK = VALUE
      CALL PAR_RDVAL( 'WHITE', -1E20, 1E20, WHITE, ' ', VALUE )
      WHITE = VALUE
      IF ( PAR_ABORT() ) GO TO 500
      IF ( BLACK .EQ. WHITE ) THEN
         CALL PAR_WRUSER( 'FINDSP: Error: BLACK and WHITE must not '//
     :      'be equal.', IGNORE )
         GO TO 500
      END IF


*  Initial display of the image.
*  =============================

*  The names of the image range parameters assume that the foreground
*  colour is white and the background colour is black. If the user has
*  changed these to green and pink, they deserve all they get.
*  The display includes a yellow box without ticks or labels.
      CALL PGGRAY( RAY, NX, NY, 1, NX, 1, NY, WHITE, BLACK, TR )
      CALL PGSCI( 7 )
      CALL PGBOX( 'BC', 0., 0, 'BC', 0., 0 )


*  The outer interaction loop.
*  ===========================

 4    CONTINUE


*     Get some parameters.
*     --------------------

*     NUMFIB is the number of fibres in the image.
*     NORDER is the polynomial order to be used to fit the fibre's y(x)
*     trace through the image.
*     NPTS is the number of points to be used in x for the compressed
*     image. Its half value is also used a lot in this routine.
*     FWCENT is the full width of the centroiding range. This routine
*     often needs to know if this is even or odd, and the half width is
*     needed a lot as well.
*     CFW determines the damping in guessing the next centroid, i.e. the
*     reluctance to look for a vastly different centroid in the next
*     bin.
         CALL PAR_RDVAL( 'NUMFIB', 1., FLOAT(MXNFIB), 64.,
     :      'Fibres', VALUE )
         NUMFIB = VALUE
         CALL PAR_RDVAL( 'NORDER', 1., FLOAT(MXORP1-1), 6.,
     :      ' ', VALUE )
         NORDER = VALUE
         CALL PAR_RDVAL( 'NPTS', FLOAT(NORDER+1), FLOAT(MXNPTS), 30.,
     :      ' ', VALUE )
         NPTS  = VALUE
         HNPTS = INT( VALUE / 2. + 0.5 )
         XBIN  = FLOAT(NX) / FLOAT(NPTS)
         CALL PAR_RDVAL( 'FWCENT', 1., 9999., 3., ' ', VALUE )
         FWCENT = VALUE
         IF( MOD(FWCENT,2) .EQ. 1 ) THEN
            ODD = .TRUE.
            HYWID = ( FWCENT - 1 ) / 2
         ELSE
            ODD = .FALSE.
            HYWID = FWCENT / 2
         END IF
         CALL PAR_RDVAL( 'CFW', 0., 1., 1., ' ', CFW )
         IF ( PAR_ABORT() ) GO TO 500


*     Compress the image.
*     -------------------

         CALL PAR_WRUSER( 'Compressing image for centroiding.', IGNORE )
         DO 7 IY = 1, NY
            DO 6 I = 1, NPTS
               IXS = NINT( (I-1)*XBIN+1 )
               IXE = NINT(     I*XBIN   )
               SUM = 0.
               DO 5 IX = IXS, IXE
                  SUM = SUM + RAY(IX,IY)
 5             CONTINUE
               CMRAY(I,IY) = SUM
 6          CONTINUE
 7       CONTINUE


*     Smooth the compressed image.
*     ----------------------------

         IY = 1
         SMRAY(1,IY) = CMRAY(1,IY)
         DO 8 IX = 2, NPTS-1
            SMRAY(IX,IY) =
     :           0.25 * ( CMRAY(IX-1,IY) + CMRAY(IX+1,IY) )
     :         + 0.50 * CMRAY(IX,IY)
 8       CONTINUE
         SMRAY(NPTS,IY) = CMRAY(NPTS,IY)
         DO 10 IY = 2, NY-1
            DO 9 IX = 2, NPTS-1
               SMRAY(IX,IY) =
     :              0.125 * ( CMRAY(IX-1,IY) + CMRAY(IX+1,IY) +
     :                        CMRAY(IX,IY-1) + CMRAY(IX,IY+1) )
     :            + 0.50  * CMRAY(IX,IY)
 9          CONTINUE
 10      CONTINUE
         IY = NY
         SMRAY(1,IY) = CMRAY(1,IY)
         DO 11 IX = 2, NPTS-1
            SMRAY(IX,IY) =
     :           0.25 * ( CMRAY(IX-1,IY) + CMRAY(IX+1,IY) )
     :         + 0.50 * CMRAY(IX,IY)
 11      CONTINUE
         SMRAY(NPTS,IY) = CMRAY(NPTS,IY)
         IX = 1
         DO 12 IY = 2, NY-2
            SMRAY(IX,IY) =
     :           0.25 * ( CMRAY(IX,IY+1) + CMRAY(IX,IY-1) )
     :         + 0.50 * CMRAY(IX,IY)
 12      CONTINUE
         IX = NPTS
         DO 13 IY = 2, NY-2
            SMRAY(IX,IY) =
     :           0.25 * ( CMRAY(IX,IY+1) + CMRAY(IX,IY-1) )
     :         + 0.50 * CMRAY(IX,IY)
 13      CONTINUE


*     The inner interaction loop.
*     ===========================

 14      CONTINUE


*        Get some more parameters.
*        ------------------------

*        YS or YFIRST is the y value of the first fibre.
*        SHIFT or YSEP is the separation in y between successive fibres.
            YS = NINT(FLOAT(NY)/20.0)
            CALL PAR_RDVAL( 'YFIRST', 1., FLOAT(NY), YS, ' ', YS )
            SHIFT = FLOAT(NY) * 0.9 / FLOAT(NUMFIB)
            CALL PAR_RDVAL( 'YSEP', 1., FLOAT(NY), SHIFT, ' ', SHIFT )
            IF ( PAR_ABORT() ) GO TO 500


*        Re-display the image (with trivial yellow box).
*        -----------------------------------------------

            CALL PGPAGE
            CALL PGGRAY( RAY, NX, NY, 1, NX, 1, NY, WHITE, BLACK, TR )
            CALL PGSCI( 7 )
            CALL PGBOX( 'BC', 0., 0, 'BC', 0., 0 )


*        Process and indicate the fibre settings.
*        ----------------------------------------

*        Green pen.
            CALL PGSCI( 3 )

*        For each fibre.
            DO 16 IFIB = 1, NUMFIB

*           Y index range for this fibre.
               IF( ODD ) THEN
                  IYLOW = MAX(  1, NINT(YS)-HYWID )
                  IYTOP = MIN( NY, NINT(YS)+HYWID )
               ELSE
                  IYLOW = MAX(  1, INT(YS)-HYWID+1 )
                  IYTOP = MIN( NY, INT(YS)+HYWID   )
               END IF

*           Calculate centroid as YSUM/FREQ.
               YSUM = 0.0
               FREQ = 0.0
               DO 15 IY = IYLOW, IYTOP
                  YSUM = YSUM + REAL(IY) * SMRAY(I,IY)
                  FREQ = FREQ + SMRAY(I,IY)
 15            CONTINUE

*           Check the sums. Assuming that the image values are by and
*           large positive, the sums are not positive when there was not
*           anything to add up. I.e. the fibre was outside the image.
*           If there is a problem with this fibre, then we return to the
*           start of the inner loop, i.e. the user can change the fibre
*           start positions.
               IF( YSUM .LE. 0. .OR. FREQ .LE. 0. ) THEN
                  CALL PAR_WRUSER( 'Over-stepped image   TRY AGAIN',
     :               IGNORE )
                  CALL PGUPDT
                  CALL PAR_CNPAR( 'YFIRST' )
                  CALL PAR_CNPAR( 'YSEP'   )
                  GO TO 14
               END IF

*           First (central) point of the fibre, X and Y coordinate.
               I = HNPTS
               X(I) = DBLE( FLOAT(I-1)*XBIN + FLOAT(I)*XBIN + 1. ) / 2D0
               Y(I) = DBLE( YSUM / FREQ )
               YY(I,IFIB) = Y(I)

*           If fibre number is even, draw its number on the display.
*           The original routine used an SGS text height of 1.5*NY/200
*           and centre justification in both directions. We try here
*           with the defaults in PGPLOT, but centre-justified.
               IF( MOD( IFIB, 2 ) .EQ. 0 )THEN
                  WRITE( TEXT, '(I3)' ) IFIB
                  CALL PGPTXT( SNGL(X(HNPTS)), SNGL(Y(HNPTS)),
     :               0., 0.5, TEXT(:ICH_LEN(TEXT)) )
               END IF

*           Draw a marker for the fibre start point.
*           The original routine used an SGS marker height of NY/200. We
*           try here the PGPLOT default. Symbol 5 in both sets is the
*           diagonal cross.
               CALL PGPT( 1, SNGL(X(HNPTS)), SNGL(Y(HNPTS)), 5 )

*           Fibre loop increment.
               YS = Y(I) + SHIFT

 16         CONTINUE


*        Check with the user whether settings are ok.
*        --------------------------------------------

            CALL PGUPDT
            CALL PAR_CNPAR( 'ADJUST' )
            CALL PAR_RDKEY( 'ADJUST', .FALSE., PQUEST )
            IF ( PAR_ABORT() ) GO TO 500

*        If the user does not like the fibre start points, return to the
*        start of the inner interactive loop.
            IF ( PQUEST ) THEN
               CALL PAR_CNPAR( 'YFIRST' )
               CALL PAR_CNPAR( 'YSEP' )
               GO TO 14
            END IF


*        End of inner interactive loop.
*        ------------------------------


*     Find centroids.
*     ---------------

*     Green pen.
         CALL PGSCI( 3 )

*     For each fibre.
         CALL PAR_WRUSER( 'Finding centroids.', IGNORE )
         DO 24 IFIB = 1, NUMFIB


*        Right hand side of image.
*        -------------------------

            YS = YY(HNPTS,IFIB)
            DO 19 I = HNPTS, NPTS
               X(I) = DBLE( FLOAT(I-1)*XBIN + FLOAT(I)*XBIN + 1. ) / 2D0
               DO 18 J = 1, 2

*              Determine window edges.
                  IF ( ODD ) THEN
                     IYLOW = MAX(  1, NINT(YS)-HYWID )
                     IYTOP = MIN( NY, NINT(YS)+HYWID )
                     IYS   = NINT(YS)
                  ELSE
                     IYLOW = MAX(  1, INT(YS)-HYWID+1 )
                     IYTOP = MIN( NY, INT(YS)+HYWID   )
                     IYS   = INT(YS)
                  END IF

*              Fine adjustment to centre.
                  IF ( SMRAY(I,IYS) .LT. SMRAY(I,IYS-1) ) THEN
                     IYLOW = IYLOW-1
                     IYTOP = IYTOP-1
                  END IF
                  IF ( SMRAY(I,IYS) .LT. SMRAY(I,IYS+1) ) THEN
                     IYLOW = IYLOW+1
                     IYTOP = IYTOP+1
                  END IF

*              Move ends in if they are too high.
                  IF ( SMRAY(I,IYLOW) .GT. SMRAY(I,IYLOW+1) )
     :               IYLOW = IYLOW+1
                  IF ( SMRAY(I,IYTOP) .GT. SMRAY(I,IYTOP-1) )
     :               IYTOP = IYTOP-1
                  YSUM = 0.
                  FREQ = 0.
                  DO 17 IY = IYLOW, IYTOP
                     YSUM = YSUM + REAL(IY) * SMRAY(I,IY)
                     FREQ = FREQ + SMRAY(I,IY)
 17               CONTINUE
                  IF ( YSUM .LE. 0. .OR. FREQ .LE. 0. ) THEN
                     CALL PAR_WRUSER(
     :                  ' Over-stepped image    will continue', IGNORE )
                     Y(I) = Y(I-1)
                  ELSE
                     Y(I) = DBLE( YSUM / FREQ )
                  END IF
                  YS = Y(I)
 18            CONTINUE
               YY(I,IFIB) = Y(I)

*           Guess where next centroid is.
               IF ( I .EQ. HNPTS )THEN
                  YS = Y(I)
               ELSE IF ( I .EQ. HNPTS+1 ) THEN
                  YS = Y(I) + CFW * ( Y(I) - Y(I-1) )
               ELSE IF ( I .GT. HNPTS+1 ) THEN
                  YS = Y(I) + CFW * 0.6 * ( Y(I)   - Y(I-1) )
     :                      + CFW * 0.4 * ( Y(I-1) - Y(I-2) )
               END IF
 19         CONTINUE


*        Left hand side of image.
*        ------------------------

            YS = YY(HNPTS,IFIB)
            DO 22 I = HNPTS-1, 1, -1
               X(I) = DBLE( FLOAT(I-1)*XBIN + FLOAT(I)*XBIN + 1. ) / 2D0
               DO 21 J = 1, 2
                  IF ( ODD ) THEN
                     IYLOW = MAX(  1, NINT(YS)-HYWID )
                     IYTOP = MIN( NY, NINT(YS)+HYWID )
                     IYS   = NINT(YS)
                  ELSE
                     IYLOW = MAX(  1, INT(YS)-HYWID+1 )
                     IYTOP = MIN( NY, INT(YS)+HYWID   )
                     IYS   = INT(YS)
                  END IF

*              Check highest count is in centre.
                  IF ( SMRAY(I,IYS) .LT. SMRAY(I,IYS-1) ) THEN
                     IYTOP = IYTOP-1
                     IYLOW = IYLOW-1
                  END IF
                  IF ( SMRAY(I,IYS) .LT. SMRAY(I,IYS+1) ) THEN
                     IYTOP = IYTOP+1
                     IYLOW = IYLOW+1
                  END IF

*              Move ends in if they are too high.
                  IF ( SMRAY(I,IYLOW) .GT. SMRAY(I,IYLOW+1) )
     :               IYLOW = IYLOW+1
                  IF ( SMRAY(I,IYTOP) .GT. SMRAY(I,IYTOP-1) )
     :               IYTOP = IYTOP-1
                  YSUM = 0.
                  FREQ = 0.
                  DO 20 IY = IYLOW, IYTOP
                     YSUM = YSUM + REAL(IY) * SMRAY(I,IY)
                     FREQ = FREQ + SMRAY(I,IY)
 20               CONTINUE
                  IF ( YSUM .LE. 0. .OR. FREQ .LE. 0. ) THEN
                     CALL PAR_WRUSER(
     :                  ' Over-stepped image    will continue', IGNORE )
                     Y(I) = Y(I+1)
                  ELSE
                     Y(I) = DBLE( YSUM / FREQ )
                  END IF
                  YS = Y(I)
 21            CONTINUE
               YY(I,IFIB) = Y(I)

*           Guess where next centroid is.
               IF ( I .EQ. HNPTS-1 ) THEN
                  YS = Y(I)
               ELSE IF( I .EQ. HNPTS-2 ) THEN
                  YS = Y(I) + CFW * ( Y(I) - Y(I+1) )
               ELSE IF( I .LT. HNPTS-2 ) THEN
                  YS = Y(I) + CFW * 0.6 * ( Y(I)   - Y(I+1) )
     :                      + CFW * 0.4 * ( Y(I+1) - Y(I+2) )
               END IF
 22         CONTINUE


*        Mark centroids.
*        ---------------

            DO 23 I = 1, NPTS

*           Red cross where centroid comes too close to previous fibre
*           (closer than FWCENT). Green cross otherwise.
               IF ( IFIB .GT. 1 .AND.
     :              ABS( Y(I) - YY(I,IFIB-1) ) .LT. FLOAT(FWCENT) ) THEN
                  CALL PGSCI( 2 )
                  CALL PGPT( 1, SNGL(X(I)), SNGL(Y(I)), 5 )
                  CALL PGSCI( 3 )
               ELSE
                  CALL PGPT( 1, SNGL(X(I)), SNGL(Y(I)), 5 )
               END IF

 23         CONTINUE

 24      CONTINUE


*     Check with user if fibre traces are ok.
*     ---------------------------------------

*     The user can reject the whole fibre settings, including how many
*     there are and what polynomial order to use. This will return to
*     the start of the outer interaction loop.
         CALL PGUPDT
         CALL PAR_CNPAR( 'CHGPAR' )
         CALL PAR_RDKEY( 'CHGPAR', .FALSE., PQUEST )
         IF ( PAR_ABORT() ) GO TO 500
         IF ( PQUEST ) THEN
            CALL PAR_CNPAR( 'NUMFIB' )
            CALL PAR_CNPAR( 'NORDER' )
            CALL PAR_CNPAR( 'NPTS'   )
            CALL PAR_CNPAR( 'CFW'    )
            CALL PAR_CNPAR( 'FWCENT' )
            CALL PAR_CNPAR( 'YSEP'   )
            CALL PAR_CNPAR( 'YFIRST' )
            GO TO 4
         END IF


*     End of the outer interaction loop.
*     ----------------------------------


*  Start recording to output file.
*  -------------------------------

*  In version 1 of the file format, this was a free-format line with NX,
*  NY, NUMFIB, NPTS, and NORDER. In version 2 we add the string '2.0'
*  Any programme reading the file can
*  then try to read 6 numbers. Failure implies version 1, success yields
*  the version number.
      WRITE( LU, 25 ) NX, NY, NUMFIB, NPTS, NORDER, 2.0
 25   FORMAT( 5 ( ' ', I6 ), F4.1 )


*  Fibre rejection loop.
*  ---------------------

         CALL PAR_WRUSER( 'Option to reject selected centroids.',
     :      IGNORE )

*     Initialise all fibres to being good.
 26      CONTINUE
         DO 27 IFIB = 1, NUMFIB
            FIBOK(IFIB) = .TRUE.
 27      CONTINUE
         NOFIB = 0

*     Ask the user for (another) number of a fibre to be rejected, until
*     the user specifies fibre number zero. The rejections are registered
*     in FIBOK and counted in NOFIB.
         NOK = 0
 28      CONTINUE
         CALL PAR_CNPAR( 'REJECT' )
         CALL PAR_RDVAL( 'REJECT', 0., FLOAT(NUMFIB), FLOAT(NOK),
     :      ' ', VALUE )
         IF ( PAR_ABORT() ) GO TO 500
         NOK = VALUE
         IF ( NOK .NE. 0 ) THEN
            NOFIB      = NOFIB + 1
            FIBOK(NOK) = .FALSE.
            GO TO 28
         END IF


*     Fill in the rejected fibre traces.
*     ----------------------------------

*     If any fibres are to be rejected, their trace is now derived as
*     follows: In each columns of the compressed image, fit a polynomial
*     of degree 4 to the y position as a function of the fibre number.
*     In these fits the rejected fibres get negligible weight. After the
*     fit the rejecte fibres are filled in using the fit.
         IF ( NOFIB .GT. 0 ) THEN

*        Re-display the image (with trivial yellow box).
*        Mainly, this clears the overlay of fibre tracing markers.
            CALL PGPAGE
            CALL PGGRAY( RAY, NX, NY, 1, NX, 1, NY, WHITE, BLACK, TR )
            CALL PGSCI( 7 )
            CALL PGBOX( 'BC', 0., 0, 'BC', 0., 0 )
            CALL PGUPDT

*        Fill X and W with fibre number and weight for the fibre.
            DO 29 IFIB = 1, NUMFIB
               X(IFIB) = DBLE(IFIB)
               IF( FIBOK(IFIB) ) THEN
                  W(IFIB) = 1D0
               ELSE
                  W(IFIB) = 1D-8
               END IF
 29         CONTINUE

*        For each compressed X bin.
            DO 31 I = 1, NPTS

*           For each fibre, fill Y with the Y coordinate of the fibre in
*           this compressed bin.
               DO IFIB = 1, NUMFIB
                  Y(IFIB) = YY(I,IFIB)
               END DO

*           Fit a polynomial of degree 4 for y(i) where y is the
*           vertical position of the fibre in the current x bin and
*           where i is the number of the fibre.
*           An error is reported, but the application continues.
               RMS = 0D0
               IFAIL2 = 0
               CALL PDA_DPOLFT( NUMFIB, X, Y, W, 4, NDEG, RMS, YLEVEL,
     :            IFAIL, A2, IFAIL2 )
               IF ( NDEG.NE.4 .OR. IFAIL.NE.1 .OR. IFAIL2.NE.0 ) THEN
                  CALL PAR_WRUSER( 'FINDSP: Error fitting a ' //
     :               'polynomial y(ifib).', IGNORE )
                  GO TO 500
               END IF

*           For each rejected fibre, set the fibre y position to what
*           the fit estimates.
               DO 30 IFIB = 1, NUMFIB
                  IF( .NOT. FIBOK(IFIB) ) YY(I,IFIB) = YLEVEL(IFIB)
 30            CONTINUE

 31         CONTINUE

*        Restore the compressed bin numbers in X.
            DO 32 I = 1, NPTS
               X(I) = DBLE( FLOAT(I-1)*XBIN + FLOAT(I)*XBIN + 1. ) / 2D0
 32         CONTINUE

*        Draw the updated fibre traces, red for the interpolated ones,
*        green for the original traces.
            CALL PGSCI( 2 )
            DO 34 IFIB = 1, NUMFIB
               IF( .NOT. FIBOK(IFIB) ) THEN
                  IF( MOD( IFIB, 2 ) .EQ. 0 )THEN
                     WRITE( TEXT, '(I3)' ) IFIB
                     CALL PGPTXT( SNGL(X(HNPTS)), SNGL(YY(HNPTS,IFIB)),
     :                  0., 0.5, TEXT(:ICH_LEN(TEXT)) )
                  END IF
                  DO 33 I = 1, NPTS
                     CALL PGPT( 1, SNGL(X(I)), SNGL(YY(I,IFIB)), 5 )
 33               CONTINUE
               END IF
 34         CONTINUE
            CALL PGSCI( 3 )
            DO 36 IFIB = 1, NUMFIB
               IF( FIBOK(IFIB) ) THEN
                  IF( MOD( IFIB, 2 ) .EQ. 0 )THEN
                     WRITE( TEXT, '(I3)' ) IFIB
                     CALL PGPTXT( SNGL(X(HNPTS)), SNGL(YY(HNPTS,IFIB)),
     :                  0., 0.5, TEXT(:ICH_LEN(TEXT)) )
                  END IF
                  DO 35 I = 1, NPTS
                     CALL PGPT( 1, SNGL(X(I)), SNGL(YY(I,IFIB)), 5 )
 35               CONTINUE
               END IF
 36         CONTINUE

         END IF

*     Ask user's confirmation. There is now the option to return to the
*     start of the rejection loop. But the rejected and interpolated
*     fibre traces have now become formally as good as the others.
         CALL PGUPDT
         CALL PAR_CNPAR( 'CHGREJ' )
         CALL PAR_RDKEY( 'CHGREJ', .FALSE., PQUEST )
         IF ( PAR_ABORT() ) GO TO 500
         IF ( PQUEST ) GO TO 26


*     End of the fibre rejection loop.
*     --------------------------------


*  Final processing.
*  =================

*  Re-display the image (with trivial yellow box).
*  Mainly, this clears the overlay of fibre tracing markers.
      CALL PGPAGE
      CALL PGGRAY( RAY, NX, NY, 1, NX, 1, NY, WHITE, BLACK, TR )
      CALL PGSCI( 7 )
      CALL PGBOX( 'BC', 0., 0, 'BC', 0., 0 )
      CALL PGUPDT

*  Set equal weights.
      DO 37 I = 1, NPTS
         W(I) = 1D0
 37   CONTINUE

*  For each fibre.
      DO 40 IFIB = 1, NUMFIB

*     Polynomial fit.
         RMS = 0D0
         IFAIL2 = 0
         CALL PDA_DPOLFT( NPTS, X, YY(1,IFIB), W, NORDER, NDEG, RMS,
     :      YLEVEL, IFAIL, A2, IFAIL2 )
         IF ( NDEG.NE.NORDER .OR. IFAIL.NE.1 .OR. IFAIL2.NE.0 ) THEN
            CALL PAR_WRUSER( 'FINDSP: Error fitting a polynomial ' //
     :         'y(x).', IGNORE )
            GO TO 500
         END IF

*     Get coefficients of an ordinary polynomial (a Taylor expansion
*     about x=0).
         IFAIL2 = 0
         CALL PDA_DPCOEF( NORDER, 0D0, COEF, A2, IFAIL2 )
         IF ( IFAIL2 .NE. 0 ) THEN
            CALL PAR_WRUSER( 'FINDSP: Error converting polynomial ' //
     :         'coefficients.', IGNORE )
            GO TO 500
         END IF

*     Write coefficients to file.
         WRITE( LU, * ) ( COEF(J), J=1, NORDER+1 )

*     Write the residuals of the fit to terminal.
         WRITE( TEXT, 38 ) IFIB, RMS
         CALL PAR_WRUSER( TEXT(:ICH_LEN(TEXT)), IGNORE )
 38      FORMAT( 'Fit to spectrum no. ', I3,
     :      ' has an r.m.s. scatter of ', F10.2, ' pixels' )

*     Draw fit (red pen).
         CALL PGSCI( 2 )
         CALL PGMOVE( SNGL(X(1)), SNGL(YLEVEL(1)) )
         DO 39 I=2,NPTS
            CALL PGDRAW( SNGL(X(I)), SNGL(YLEVEL(I)) )
 39      CONTINUE
 40   CONTINUE

*  Reset abort flag.
      ABORT = .FALSE.

*  Return.
 500  CONTINUE
      END
