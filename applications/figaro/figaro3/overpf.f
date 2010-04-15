      SUBROUTINE OVERPF
*+
*  Name:
*     OVERPF

*  Purpose:
*     Overlay polynomial fits on an image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL OVERPF

*  Description:
*     Overlays the Polynominal Fits on  an image.
*     Displaying a zoomed part of the image is possible.
*     To be used with FINDSP and POLEXT.

*  ADAM Parameters:
*     IMAGE = LITERAL (Read)
*        The fibre frame - one with distorted fibre spectra
*        approximately equally spaced.
*     BLACK = _REAL (Read)
*        The data value below which the image display is to have the
*        background colour. The display is scaled linearly between the
*        data values specified as BLACK and WHITE.
*     WHITE = _REAL (Read)
*        The data value above which the image display is to have the
*        foreground colour. The display is scaled linearly between the
*        data values specified as BLACK and WHITE.
*     PFILE = LITERAL (Read)
*        The file containing the polynominal fits. If no extension is
*        specified, `.pol' is used.
*     YSTART = _REAL (Read)
*        First Y value to be displayed.
*     YEND = _REAL (Read)
*        Last Y value to be displayed.
*     XSTART = _REAL (Read)
*        First X value to be displayed.
*     XEND = _REAL (Read)
*        Last X value to be displayed.
*     EXTWID = _REAL (Read)
*        The input may be integer or real. An integer input causes edges
*        of pixels included to be drawn, whereas real input causes trams
*        lines of width EXTWID to be drawn surrounding each polynomial
*        fit.
*     REPEAT = _CHAR (Read)
*        Used to ask whether a display with different parameters should
*        be made.

*  Authors:
*     jrl: John Lucey (AAO, Durham)
*     ks: Keith Shortridge (AAO)
*     jm: Jo Murray (RAL, Starlink)
*     jms: {author_name} (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     mjcl: Martin Clayton (starlink, UCL)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     {date} (jrl):
*        Original version.
*     16 Sep 1988 (ks):
*        PENCOLS and JRL_PEN changed to FIG_PENCOLS and FIG_JRL_PEN.
*        STOPs removed from code.  Explicit determination of overlay
*        device replaced by call to FIG_OVERDEV.
*     03 Feb 1989 (jm):
*        Modified to use DSA_ routines dynamic memory handling changed
*        to use DYN_ routines.
*     22 Jan 1991 (jms):
*        Added PAR_ABORTS to support user requested aborts. Modified to
*        trap errors when reading the coefficient file. Added check to
*        close SGS at the end of OVERPF_WORK.
*     01 Jul 1991 (hme):
*        Increase string lengths for IMAGEDEV, OVERDEV in OVERPF_WORK to
*        32.
*     30 Jan 1992 (hme):
*        Major cosmetics to improve behaviour in LSE (an editor).
*        Enquire workstation properties rather than assuming 256
*        colours. Some rearrangement of code. Taken out most
*        informational messages. If there is no independent overlay
*        device, use the device itself, and disuse lowest 5 colours for
*        image display.
*     10 Sep 1992 (hme):
*        INCLUDE changed. Avoid calls to CTOI, CTOR. Calls CHR_ instead.
*        No longer fold the device name. Open text file with lowercase
*        extension. Call PAR_WRUSER rather than DSA_WRUSER.
*     26 Oct 1992 (hme):
*        The VMS version must keep the call to GKS 7.2 GCA. On Unix
*        there is GKS 7.4 and the call to GCA must be changed.
*     10 Aug 1993 (hme):
*        Eliminate CHR_ calls by doing an internal READ.
*     25 Apr 1995 (hme):
*        The time has come. No longer use GKS or SGS either. This is a
*        re-write of the code, although the structure remains.
*     16 Sep 1996 (mjcl):
*        Changed format for messages to support images of Y-dimension
*        greater than 999 pixels.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Local Constants
      INTEGER MAXFIB             ! Maximum number of fibres
      PARAMETER ( MAXFIB = 256 )
      INTEGER MAXORD             ! Maximum polynomial order
      PARAMETER ( MAXORD =  10 )

*  Local Variables:
      INTEGER I, IFIB            ! Do loop variables
      INTEGER STATUS             ! DSA status
      INTEGER IOSTAT             ! Reading error status code number
      INTEGER PGSTAT             ! PGBEG status
      INTEGER IGNORE             ! Ignored status
      INTEGER LU                 ! Fortran unit for polynomial file
      INTEGER NDIM               ! Number of image dimensions
      INTEGER NELM               ! Number of elements in image
      INTEGER DIMS( 2 )          ! Image dimensions
      INTEGER NX, NY             ! Expected dimensions of image
      INTEGER NUMFIB             ! Number of fibres
      INTEGER NPTS               ! No. of points along spectrum
      INTEGER NORDER             ! Order of polynomial
      INTEGER IPTR               ! Pointer for image data
      INTEGER IYRUN              ! Pointer to workspace for YRUN
      INTEGER IYLEVEL            ! Pointer to workspace for YLEVEL
      INTEGER IXP                ! Pointer to workspace for X
      INTEGER SLOT               ! Slot number - ignored
      REAL VERSION               ! Coefficient file version number
      CHARACTER * ( 32 )  DEVICE ! Graphics device
      CHARACTER * ( 256 ) PFILE  ! Name of polynomial file
      CHARACTER * ( 256 ) FILNAM ! Full name of polynomial file
      CHARACTER * ( 79  ) TEXT   ! Used to format user messages
      CHARACTER * ( 132 ) TEXTL  ! Used to read long line from file
      DOUBLE PRECISION ADPCOF( MAXORD+1, MAXFIB ) ! Polynomial coefficients

*  Internal References:
      LOGICAL PAR_ABORT
      INTEGER FIG_PGBEG

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

*  Get name of coefficient file and open it.
      CALL PAR_RDCHAR( 'PFILE', 'PFILE', PFILE )
      IF ( PAR_ABORT() ) GO TO 500
      CALL DSA_OPEN_TEXT_FILE( PFILE, '.pol', 'OLD', .FALSE., LU,
     :   FILNAM, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500

*  Read the header line from the coefficient file.
*  It contains five integers, but may also contain a 6th, real, number.
*  If the 6th number is missing the file format is version 1, else the
*  number gives the version number.
*  The difference between version 1 and 2 is that the former stores the
*  coefficients of a Chebyshev series, while the latter stores
*  coefficients of an ordinary polynomial. Chebyshev coefficients have
*  to be converted with GEN_CHB2NO.
*  We have to buffer the first line in the file in an internal string.
*  Otherwise a missing VERSION on the first line would be read from the
*  second line.
      READ( LU, '(A)' ) TEXTL
      READ( TEXTL, *, IOSTAT=IOSTAT )
     :   NX, NY, NUMFIB, NPTS, NORDER, VERSION
      IF ( IOSTAT .NE. 0 ) THEN
         VERSION = 1.
         READ( TEXTL, *, IOSTAT=IOSTAT ) NX, NY, NUMFIB, NPTS, NORDER
         IF ( IOSTAT .NE. 0 ) THEN
            CALL PAR_WRUSER(
     :         'OVERPF: Error reading coefficient file.', IGNORE )
            GO TO 500
         END IF
      END IF

*  Report to terminal.
      CALL PAR_WRUSER( ' ', IGNORE )
      WRITE( TEXT, 1 ) VERSION
      CALL PAR_WRUSER( TEXT, IGNORE )
      WRITE( TEXT, 2 ) NX, NY
      CALL PAR_WRUSER( TEXT, IGNORE )
      WRITE( TEXT, 3 ) NUMFIB
      CALL PAR_WRUSER( TEXT, IGNORE )
      WRITE( TEXT, 4 ) NPTS
      CALL PAR_WRUSER( TEXT, IGNORE )
      WRITE( TEXT, 5 ) NORDER
      CALL PAR_WRUSER( TEXT, IGNORE )
 1    FORMAT( 'Coefficient file is format version ', F3.1 )
 2    FORMAT( 'Image size expected is ', I4, ' by ', I4 )
 3    FORMAT( 'No. of fibres is ', I3 )
 4    FORMAT( 'No. of points along spectra was ', I3 )
 5    FORMAT( 'Order of polynomial used was ', I2 )

*  Check capacity of programme sufficient.
      IF ( NUMFIB .GT. MAXFIB ) THEN
         CALL PAR_WRUSER(
     :      'OVERPF: Error: File contains too many fibres.', IGNORE )
         GO TO 500
      END IF
      IF ( NORDER .GT. MAXORD ) THEN
         CALL PAR_WRUSER( 'OVERPF: Error: File contains polynomials ' //
     :      'of too high a degree.', IGNORE )
         GO TO 500
      END IF

*  Check consistent data and abort if inconsistent.
      IF( DIMS(1) .NE. NX ) THEN
         CALL PAR_WRUSER( 'OVERPF: Error: Inconsistent 1st dimension.',
     :      IGNORE )
         GO TO 500
      END IF
      IF( DIMS(2) .NE. NY ) THEN
         CALL PAR_WRUSER( 'OVERPF: Error: Inconsistent 2nd dimension.',
     :      IGNORE )
         GO TO 500
      END IF

*  Read in the file.
      DO 1006 IFIB = 1, NUMFIB
         READ( LU, *, IOSTAT=IOSTAT )
     :      ( ADPCOF(I,IFIB), I = 1, NORDER + 1 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL PAR_WRUSER( 'OVERPF: Error reading coefficient file.',
     :         IGNORE )
            GO TO 500
         END IF
 1006 CONTINUE
      DO 1007 I = 1, 6
         READ( LU, '(A64)', IOSTAT=IOSTAT ) TEXT
         IF ( IOSTAT .NE. 0 ) THEN
            CALL PAR_WRUSER( 'OVERPF: Error reading coefficient file.',
     :         IGNORE )
            GO TO 500
         END IF
         CALL PAR_WRUSER( TEXT, IGNORE )
 1007 CONTINUE
      CALL PAR_WRUSER( ' ', IGNORE )

*  Get workspace for YRUN, one real array NX.
*  Get workspace for YLEVEL, one REAL array NPTS.
*  Get workspace for X, one double precision array NPTS.
      CALL DSA_GET_WORK_ARRAY(   NX, 'FLOAT',  IYRUN,   SLOT, STATUS )
      CALL DSA_GET_WORK_ARRAY( NPTS, 'FLOAT',  IYLEVEL, SLOT, STATUS )
      CALL DSA_GET_WORK_ARRAY( NPTS, 'DOUBLE', IXP,     SLOT, STATUS )
      IF( STATUS .NE. 0 ) GO TO 500

*  Open the graphics device, set up view port.
*  The view port uses identical scales in x and y, initially no box is
*  drawn.
      CALL VAR_GETCHR( 'SOFT', 0, 0, DEVICE, STATUS )
      IF ( STATUS .NE. 0 ) THEN
         CALL PAR_WRUSER( 'OVERPF: Error: No display specified. ' //
     :      'Use SOFT to select a graphics device.', IGNORE )
         GO TO 500
      END IF
      PGSTAT = FIG_PGBEG( 0, DEVICE, 1, 1 )
      IF ( PGSTAT .NE. 1 ) THEN
         CALL PAR_WRUSER( 'OVERPF: Error opening display.', IGNORE )
         GO TO 500
      END IF
      CALL PGASK( .FALSE. )
      CALL PGENV( 0., FLOAT(DIMS(1)), 0., FLOAT(DIMS(2)), 0, -2 )

*  Now do the real work.
      CALL OVERPF_WORK( DIMS(1), DIMS(2),
     :                  %VAL(CNF_PVAL(IPTR)), %VAL(CNF_PVAL(IYRUN)),
     :                  %VAL(CNF_PVAL(IYLEVEL)), %VAL(CNF_PVAL(IXP)),
     :                  ADPCOF, NUMFIB, NPTS, NORDER, VERSION )

*  Close graphics device.
      CALL FIG_PGEND

*  Close DSA.
 500  CONTINUE
      CALL DSA_CLOSE( STATUS )

*  Return.
      END


      SUBROUTINE OVERPF_WORK( NX, NY, RAY, YRUN, YLEVEL, X,
     :   ADPCOF, NUMFIB, NPTS, NORDER, VERSION )
*+
*  Name:
*     OVERPF_WORK

*  Purpose:
*     Do the work for OVERPF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL OVERPF_WORK( NX, NY, RAY, YRUN, YLEVEL, X,
*        ADPCOF, NUMFIB, NPTS, NORDER, VERSION )

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants
      INTEGER MAXFIB
      PARAMETER ( MAXFIB = 256 )
      INTEGER MAXORD
      PARAMETER ( MAXORD =  10 )

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL RAY( NX, NY )
      DOUBLE PRECISION ADPCOF( MAXORD+1, MAXFIB )
      INTEGER NUMFIB
      INTEGER NPTS
      INTEGER NORDER
      REAL VERSION

*  Arguments Returned:
      REAL YRUN( NX )
      REAL YLEVEL( NPTS )
      DOUBLE PRECISION X( NPTS )

*  Local Variables:
      INTEGER NPPF
      REAL BLACK, WHITE
      REAL XSTART, XEND, YSTART, YEND
      REAL EXTWID
      CHARACTER * ( 32 ) CEXT
      CHARACTER * ( 1 ) REPEAT
      LOGICAL ODD                ! True if EXTWID is odd
      INTEGER HEX                ! Half of EXTWID
      INTEGER I, J, IX, IY, IFIB ! Loop indices
      INTEGER HNPTS              ! Half the number of compressed points
      INTEGER IGNORE             ! Ignored status
      INTEGER IXLIM( 2 )         ! Sub-image pixel range
      INTEGER IYLIM( 2 )         ! dto.
      REAL EXHALF                ! Half of EXTWID
      REAL XBIN                  ! Compression factor in X
      REAL XTEM, YTEM            ! Temporary position
      REAL YLOW, YTOP            ! Temporary y values
      REAL VALUE                 ! Temporary floating point value
      REAL TR( 6 )               ! PGGRAY coordinate transform
      DOUBLE PRECISION YY        ! Temporary double precision value
      DOUBLE PRECISION DPCOEF( MAXORD+1 ) ! Ordinary polynomial coeffs.
      DOUBLE PRECISION TPCOEF( MAXORD+1 ) ! Chebyshev series coeffs.
      CHARACTER * ( 1 ) EXTYPE   ! The mask type, P or M
      CHARACTER * ( 79 ) TEXT    ! Text buffer

*  Internal References:
      LOGICAL PAR_ABORT
      INTEGER ICH_LEN

*  Data Statements.
      DATA TR / 0., 1., 0., 0., 0., 1. /

*.


*  Preliminaries.
*  ==============

*  Compression factor.
*  Set X array to number of compressed pixel.
      HNPTS = NPTS / 2
      XBIN  = FLOAT(NX) / FLOAT(NPTS)
      DO 1 I = 1, NPTS
         X(I) = DBLE( FLOAT(I-1)*XBIN + FLOAT(I)*XBIN + 1. ) / 2D0
 1    CONTINUE

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
         CALL PAR_WRUSER( 'OVERPF: Error: BLACK and WHITE must not '//
     :      'be equal.', IGNORE )
         GO TO 500
      END IF

*  Initial sub-image.
      IXLIM(1) = 1
      IXLIM(2) = NX
      IYLIM(1) = 1
      IYLIM(2) = NY


*  Main interaction loop.
*  ======================

 4       CONTINUE

*     Enquire sub-image from user.
*     ----------------------------

*     Get parameters.
         CALL PAR_WRUSER( ' ', IGNORE )
         CALL PAR_WRUSER( 'Which part of the image ?', IGNORE )
         CALL PAR_RDVAL( 'XSTART', 1., FLOAT(NX), 1., ' ', XSTART )
         CALL PAR_RDVAL( 'XEND',   1., FLOAT(NX), 1., ' ', XEND   )
         CALL PAR_RDVAL( 'YSTART', 1., FLOAT(NY), 1., ' ', YSTART )
         CALL PAR_RDVAL( 'YEND',   1., FLOAT(NY), 1., ' ', YEND   )
         IF ( PAR_ABORT() ) GO TO 500
         IXLIM(1) = XSTART
         IXLIM(2) = XEND
         IYLIM(1) = YSTART
         IYLIM(2) = YEND
         IXLIM(1)  = MAX(  1, IXLIM(1) )
         IXLIM(2)  = MIN( NX, IXLIM(2) )
         IYLIM(1)  = MAX(  1, IYLIM(1) )
         IYLIM(2)  = MIN( NY, IYLIM(2) )


*     Display the sub-image.
*     ----------------------

*     The sub-image is chosen simply by changing the window coordinates
*     of the viewport. (Although we also tell PGGRAY about the index
*     range to be displayed.) As in FINDSP, the display is surrounded by
*     a trivial yellow box.
         CALL PGPAGE
         CALL PGSWIN( FLOAT(IXLIM(1)), FLOAT(IXLIM(2)),
     :      FLOAT(IYLIM(1)), FLOAT(IYLIM(2)) )
         CALL PGGRAY( RAY, NX, NY, IXLIM(1), IXLIM(2),
     :      IYLIM(1), IYLIM(2), WHITE, BLACK, TR )
         CALL PGSCI( 7 )
         CALL PGBOX( 'BC', 0., 0, 'BC', 0., 0 )


*     Enquire mask type and extraction width.
*     ---------------------------------------

*     If the user gives EXTWID with a decimal point the mask type is P,
*     if she gives EXTWID without decimal point (and thus an integer)
*     the mask type is 'M'. This loop continues until the user input
*     makes sense.

 5       CONTINUE

         CALL PAR_RDCHAR( 'EXTWID', '7.0', CEXT )
         CALL PAR_CNPAR( 'EXTWID' )
         EXTYPE = 'M'
         IF ( INDEX(CEXT,'.') .NE. 0 ) EXTYPE = 'P'
         READ( CEXT, *, ERR=5 ) EXTWID

         IF( EXTYPE .EQ. 'M' ) THEN
            NPPF = EXTWID
            IF( NPPF .LT. 1 ) GO TO 5
            IF( MOD(NPPF,2) .EQ. 1 ) THEN
               ODD = .TRUE.
               HEX = ( NPPF - 1 ) / 2
            ELSE
               ODD = .FALSE.
               HEX = NPPF / 2
            END IF
         ELSE
            IF( EXTWID .LT. 1. ) GO TO 5
         END IF


*     Loop through the fibres.
*     ========================

         DO 19 IFIB = 1, NUMFIB


*        Acquire the ordinary polynomial coefficients for this fibre.
*        ------------------------------------------------------------

*        If the file format version is 1, then we have to convert a row
*        of ADPCOF from Chebyshev series coefficients - which they are -
*        to ordinary coefficients. If the file format version is later,
*        then we need only copy the row from ADPCOF.
*        A complication arises, because GEN_CHB2NO has the undocumented
*        feature of modifying the given Chebyshev coefficients.
            IF ( VERSION .EQ. 1.0 ) THEN
               DO 6 I = 1, NORDER + 1
                  TPCOEF(I) = ADPCOF(I,IFIB)
 6             CONTINUE
               CALL GEN_CHB2NO( NORDER, X(1), X(NPTS), TPCOEF, DPCOEF )
            ELSE
               DO 7 I = 1, NORDER + 1
                  DPCOEF(I) = ADPCOF(I,IFIB)
 7             CONTINUE
            END IF


*        Evaluate the polynomial at each compressed point.
*        -------------------------------------------------

*        This gives the centroid curve.
*        Most points are never used, but the central point and the two
*        points at either end are, i.e. 1, 2, HNPTS, NPTS-1, NPTS.
            DO 9 I = 1, NPTS
               YY = DPCOEF(NORDER+1)
               DO 8 J = NORDER, 1, -1
                  YY = YY * X(I)
                  YY = YY + DPCOEF(J)
 8             CONTINUE
               YLEVEL(I) = SNGL(YY)
 9          CONTINUE


*        Further processing for visible fibres only.
*        -------------------------------------------

            IF ( YLEVEL(HNPTS) .GE. FLOAT(IYLIM(1)) .AND.
     :           YLEVEL(HNPTS) .LE. FLOAT(IYLIM(2)) ) THEN


*           Evaluate the polynomial at each uncompressed point.
*           ---------------------------------------------------

*           In the olden days when NAG's E02AEF was used and
*           extrapolation was not possible, this was complicated. The
*           polynomial would be evaluated inside the range covered by
*           compressed pixels. (Since compressed pixels are bigger than
*           the original, their centres do not quite reach to the
*           centres of the original edge pixels.) Near the edge the
*           outermost gradient would be used to extrapolate.
*           Since we have ordinary polynomial coefficients here, we can
*           easily extrapolate up to the edge.
               DO 11 IX = 1, NX
                  YY = DPCOEF(NORDER+1)
                  DO 10 J = NORDER, 1, -1
                     YY = YY * DBLE(IX)
                     YY = YY + DPCOEF(J)
 10               CONTINUE
                  YRUN(IX) = SNGL(YY)
 11            CONTINUE


*           Draw the centroid polynomial just evaluated.
*           --------------------------------------------

*           Draw with red pen. Label with fibre number.
               CALL PGSCI( 2 )
               CALL PGMOVE( FLOAT(IXLIM(1)), YRUN(IXLIM(1)) )
               DO 12 IX = IXLIM(1) + 1, IXLIM(2)
                  CALL PGDRAW( FLOAT(IX), YRUN(IX) )
 12            CONTINUE
               WRITE( TEXT, '(I3)' ) IFIB
               XTEM = FLOAT( IXLIM(1) + IXLIM(2) ) / 2.
               YTEM = YRUN( INT( XTEM + 0.5 ) )
               CALL PGPTXT( XTEM, YTEM, 0., 0.5, TEXT(:ICH_LEN(TEXT)) )


*           Plot the mask.
*           ==============

*           Blue pen.
               CALL PGSCI( 4 )


*           M type mask (outline exact pixels).
*           -----------------------------------

               IF ( EXTYPE .EQ. 'M' ) THEN

*              If the width is odd.
                  IF ( ODD ) THEN

*                 Lower boundary.
                     IY     = MAX( IYLIM(1), NINT(YRUN(IXLIM(1)))-HEX )
                     YLOW   = FLOAT(IY) - 0.5
                     XSTART = FLOAT(IXLIM(1)) - 0.5
                     DO 13 IX = IXLIM(1) + 1, IXLIM(2)
                        IY   = MAX( IYLIM(1), NINT(YRUN(IX))-HEX )
                        XEND = FLOAT(IX)+0.5
                        IF ( ABS( YLOW - FLOAT(IY) + 0.5 ) .GT. 0.1 .OR.
     :                       IX .EQ. IXLIM(2) ) THEN
                           CALL PGMOVE( XSTART, YLOW )
                           CALL PGDRAW( XEND,   YLOW )
                           YLOW   = FLOAT(IY) - 0.5
                           XSTART = XEND
                        END IF
 13                  CONTINUE

*                 Upper boundary.
                     IY     = MIN( IYLIM(2), NINT(YRUN(IXLIM(1)))+HEX )
                     YTOP   = FLOAT(IY) + 0.5
                     XSTART = FLOAT(IXLIM(1)) - 0.5
                     DO 14 IX = IXLIM(1) + 1, IXLIM(2)
                        IY   = MIN( IYLIM(2), NINT(YRUN(IX))+HEX )
                        XEND = FLOAT(IX) + 0.5
                        IF ( ABS( YTOP - FLOAT(IY) - 0.5 ) .GT. 0.1 .OR.
     :                      IX .EQ. IXLIM(2) ) THEN
                           CALL PGMOVE( XSTART, YTOP )
                           CALL PGDRAW( XEND,   YTOP )
                           YTOP   = FLOAT(IY) + 0.5
                           XSTART = XEND
                        END IF
 14                  CONTINUE


*              Else (the width is even).
                  ELSE

*                 Lower boundary.
                     IY     = MAX( IYLIM(1), INT(YRUN(IXLIM(1)))+1-HEX )
                     YLOW   = FLOAT(IY) - 0.5
                     XSTART = FLOAT(IXLIM(1)) - 0.5
                     DO 15 IX = IXLIM(1) + 1, IXLIM(2)
                        IY   = MAX( IYLIM(1), INT(YRUN(IX))+1-HEX )
                        XEND = FLOAT(IX) + 0.5
                        IF ( ABS( YLOW - FLOAT(IY) + 0.5 ) .GT. 0.1 .OR.
     :                       IX .EQ. IXLIM(2) ) THEN
                           CALL PGMOVE( XSTART, YLOW )
                           CALL PGDRAW( XEND,   YLOW )
                           YLOW   = FLOAT(IY) - 0.5
                           XSTART = XEND
                        END IF
 15                  CONTINUE

*                 Upper boundary.
                     IY     = MIN( IYLIM(2), INT(YRUN(IXLIM(1)))+HEX )
                     YTOP   = FLOAT(IY) + 0.5
                     XSTART = FLOAT(IXLIM(1)) - 0.5
                     DO 16 IX = IXLIM(1) + 1, IXLIM(2)
                        IY   = MIN( IYLIM(2), INT(YRUN(IX))+HEX )
                        XEND = FLOAT(IX) + 0.5
                        IF ( ABS( YTOP - FLOAT(IY) -0.5 ) .GT. 0.1 .OR.
     :                       IX .EQ. IXLIM(2) ) THEN
                           CALL PGMOVE( XSTART, YTOP )
                           CALL PGDRAW( XEND,   YTOP )
                           YTOP   = FLOAT(IY) + 0.5
                           XSTART = XEND
                        END IF
 16                  CONTINUE

                  END IF


*           P type mask (shifted centroid outline).
*           ---------------------------------------

               ELSE

*              Lower boundary.
                  EXHALF = EXTWID / 2.
                  XTEM   = FLOAT(IXLIM(1)) - 0.5
                  YTEM   = YRUN(IXLIM(1)) - EXHALF
                  CALL PGMOVE( XTEM, YTEM )
                  DO 17 IX = IXLIM(1), IXLIM(2)
                     XTEM = FLOAT(IX) + 0.5
                     YTEM = YRUN(IX) - EXHALF
                     CALL PGDRAW( XTEM, YTEM )
 17               CONTINUE

*              Upper boundary.
                  XTEM   = FLOAT(IXLIM(1)) - 0.5
                  YTEM   = YRUN(IXLIM(1)) + EXHALF
                  CALL PGMOVE( XTEM, YTEM )
                  DO 18 IX = IXLIM(1), IXLIM(2)
                     XTEM = FLOAT(IX) + 0.5
                     YTEM = YRUN(IX) + EXHALF
                     CALL PGDRAW( XTEM, YTEM )
 18               CONTINUE

               END IF

            END IF

 19      CONTINUE


*     Enquire whether to display again.
*     ---------------------------------

         CALL PGUPDT
         REPEAT = 'N'
         CALL PAR_CNPAR( 'REPEAT' )
         CALL PAR_RDCHAR( 'REPEAT', 'N', REPEAT )
         IF ( PAR_ABORT() ) GO TO 500
         IF( REPEAT .EQ. 'Y' .OR. REPEAT .EQ. 'y' ) THEN
            CALL PAR_CNPAR( 'XSTART' )
            CALL PAR_CNPAR( 'XEND'   )
            CALL PAR_CNPAR( 'YSTART' )
            CALL PAR_CNPAR( 'YEND'   )
            CALL PAR_CNPAR( 'EXTWID' )
            GO TO 4
         END IF


*     End of main interaction loop.
*     -----------------------------

*  Return.
 500  CONTINUE
      END
