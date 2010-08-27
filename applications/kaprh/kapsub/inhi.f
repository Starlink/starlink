* +  INHI - calculates, plots and stores the histogram of a subregion
*           of a 2-d array

      SUBROUTINE INHI( GRAPHS, ZONEG, NDFI, LBND, DIM1, DIM2, ARRAY,
     :                 PNNUMB, PNHINA, PNTITL, PNHIRE, NULL,
     :                 PLTITL, ABSLAB, ORDLAB, MINTIC, MAJTIC, XLOG,
     :                 YLOG, OUTTIC, THICK, RNUMB, HSTGRM, STATUS )
*
*    Description :
*
*     This subroutine calculates the histogram of a 2-d array. The
*     histogram may be plotted in the chosen SGS zone, and may
*     optionally be saved in an NDF file.
*
*     The plotting style may be controlled: the axis tick marks can
*     be drawn inside or outside the axes; either or both axes may be
*     logarithmic; the numbers of major and minor tick marks per axis
*     may be adjusted for linear axes, or let the graphics package
*     decide, the number of minor tick marks per major tick is fixed
*     (8) for a logarithmic axis; and finally the whole plot may be
*     drawn with a thicker pen.
*
*    Invocation :
*
*      CALL INHI( GRAPHS, ZONEG, NDFI, LBND, DIM1, DIM2, ARRAY,
*     :           PNNUMB, PNHINA, PNTITL, PNHIRE, NULL, PLTITL,
*     :           ABSLAB, ORDLAB, MINTIC, MAJTIC, XLOG, YLOG, OUTTIC,
*     :           THICK, RNUMB, HSTGRM, STATUS )
*
*    Arguments :
*
*     GRAPHS = LOGICAL ( READ )
*         True if a graph of the image is to be plotted
*     ZONEG = INTEGER ( READ )
*         SGS zone identifier of graphics device if %GRAPHS is true
*     NDFI = INTEGER ( READ )
*         NDF identifier of the input NDF associated with the 2-d array.
*         It is used to propagate the data units and label to the
*         output histogram NDF's axis units and label respectively.
*     LBND( 2 ) = INTEGER ( READ )
*         Lower bounds of the array, thus the first element of the
*         array (1,1) is actually at (LBND(1),LBND(2)).
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     ARRAY( DIM1, DIM2 ) = REAL ( READ )
*         2-d array whose histogram is to be derived.
*     PNNUMB = CHARACTER ( READ )
*         Parameter name of the number of bins in the histogram.
*     PNHINA = CHARACTER ( READ )
*         Parameter name of file to store histogram.
*     PNTITL = CHARACTER ( READ )
*         Parameter name of the title of the stored histogram.
*     PNHIRE = CHARACTER ( READ )
*         Parameter name of flag to determine whether histogram is to be
*         reported to the user.
*     NULL = REAL( READ )
*         Null value used by graphics package
*     PLTITL = CHAR( READ )
*         Title for the plot
*     ABSLAB = CHAR( READ )
*         Label for the x axis
*     ORDLAB = CHAR( READ )
*         Label for the y axis
*     XLOG = LOGICAL( READ )
*         If true the x axis is logarithmic
*     YLOG = LOGICAL( READ )
*         If true the y axis is logarithmic
*     MINTIC( 2 ) = REAL( READ )
*         The number of minor tick marks between each major tick mark
*           for the x and y axes.  A negative value forces the graphics
*           package to compute appropriate values.
*     MAJTIC( 2 ) = REAL( READ )
*         The parameter controlling the numbers of major tick marks
*           for the x and y axes.  (Number used is between MAJTIC+2 and
*           5*MAJTIC/2+4.) A negative value forces the graphics package
*           to compute appropriate values.
*     OUTTIC = LOGICAL( READ )
*         If true the axis tick marks are drawn outside the box
*     THICK = REAL( READ )
*         The line thickness in units of the default
*     RNUMB = INTEGER( READ, WRITE )
*         Default number of bins for the histogram
*     HSTGRM = LOGICAL( WRITE )
*         True if the histogram has been computed.
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Find the maximum and minimum values in the array.
*     If no error then
*        Get the number of bins in the histogram
*        Get workspace for the histogram calculations
*        Abort if requested
*        If status is ok then
*           Compute the histogram
*           If no error returned then
*              Copy the histogram
*              Find the maximum number of entries in the histogram bins
*              If number of bins is sufficient then
*                 Report a summary of the histogram
*                 Inquire whether or not a report of the full histogram
*                   is required
*              Else
*                 Set full-histogram-report flag to true
*              If full histogram is to be reported call routine to do it
*              If no error then
*                 If graphs mode then
*                    If input title is the default then
*                       Create a title using the region limits within it
*                    Else
*                       Use supplied title
*                    Endif
*                    Set Autograph to plot in the graphics zone
*                    Create work space for histogram plotting
*                    If status is ok then
*                       Compute locus of the histogram and store in
*                         workspace
*                       Find plot limits --- for a logarithmic
*                         abscissa with negative or zero limits, set
*                         the limits to be in units of bin numbers
*                       Plot histogram with annotated axes
*                    Else
*                       Report error
*                    Endif
*                    Tidy workspace
*                 Endif
*              Else
*                 Report error context
*              Endif
*           Else
*              Report error context
*           Endif
*        Else
*           Report error context
*        Endif
*        Tidy workspace
*        -  Create a primitive NDF.  Map its data array.  Obtain a
*        title for the NDF.  Copy the histogram to the NDF.  Write a
*        label for the output NDF.  Copy the label and units from the
*        input NDF to the output's axis structure.  Map the axis centre
*        array and fill it with the centres of the bins (in data
*        value). Handle a null entry transparently.  Cancel the
*        parameters.
*     Endif
*     End
*
*    Authors :
*
*     Malcolm Currie  STARLINK (RAL::CUR)
*
*    History :
*
*     1988 Apr 9 : Original extracted from INSPECT (RAL::CUR)
*     1988 Jun 23: More error reporting and tidying (RAL::CUR).
*     1988 Jul 7 : Bug fix in GENHIS call; limit calls to HSTREP; extra
*                  argument in HSTREP; used integer copy for COPY1D;
*                  added extra parameter via argument PNHIRE
*                  (RAL::CUR).
*     1988 Sep 14: Added extra argument to HSTPL and generated plot
*                  title (RAL::CUR).
*     1988 Oct 26: Renamed the DATA_LABEL object in the output IMAGE
*                  structure to be TITLE for consistency
*                  (RAL::CUR).
*     1989 Apr 23: Converted to NCAR graphics with many plot style
*                  options --- argument list has been extended
*                  (RAL::CUR).
*     1989 Jul 13: Control of temporary data via TRN routines to avoid
*                  HDS problem giving an access violation (RAL::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RAL::CUR).
*     1989 Dec 21: Workspace managed by AIF_TEMP (RAL::CUR).
*     1990 Jan 9 : Corrected SGS status (RAL::CUR).
*     1990 Feb 18: Added NINVAL argument for the modified MAXMIN call
*                  (RAL::CUR).
*     1991 Jun 10: Added LBND argument as temporary patch for INSPECT to
*                  use NDF (RAL::CUR).
*     1991 Jun 13: Added NDFI argument so propagate the label and
*                  units to the axis structure of the output NDF, and
*                  create axis centres (RAL::CUR).
*     1991 Jul 5 : Works on a whole array rather than a subregion, thus
*                  X1, X2, Y1, Y2 arguments removed.  Redesigned with
*                  less error checking to reduce indentation.  Calls
*                  modern versions of subroutines to compute the
*                  histogram.
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! no default typing allowed

*    Global Constants :

      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! parameter-system errors

*    Status :

      INTEGER STATUS

*    Import :

      INTEGER
     :  NDFI,
     :  LBND( 2 ),
     :  DIM1, DIM2,
     :  ZONEG

      LOGICAL
     :  GRAPHS,
     :  HSTGRM,
     :  OUTTIC,
     :  XLOG,
     :  YLOG

      REAL
     :  ARRAY( DIM1, DIM2 ),
     :  MINTIC( 2 ),
     :  MAJTIC( 2 ),
     :  NULL,
     :  THICK

      CHARACTER*(*)
     :  ABSLAB, ORDLAB,
     :  PLTITL,
     :  PNNUMB,
     :  PNHINA,
     :  PNHIRE,
     :  PNTITL

*    Import/Export:

      INTEGER
     :  RNUMB

*    Local constants :

      INTEGER
     :  BINFAC,                 ! number of compressed bins for summary
     :  MXHSTB                  ! maximum no. of histogram bins
      PARAMETER ( BINFAC = 16 )
      PARAMETER ( MXHSTB = 5000 )

      REAL
     :  YNULL                   ! bin height for an empty bin in a
                                ! histogram with a log ordinate
      PARAMETER ( YNULL = 0.8 ) ! Must be in range 0.5 to 1.0

*    Local Variables :

      CHARACTER*(DAT__SZLOC)    ! locator to:
     :  HPLOC1,                 ! work space for histogram x locus
     :  HPLOC2                  ! work space for histogram y locus

      CHARACTER
     :  DIMSTR * 20,            ! List of start or end pixels of region
     :  LABEL * 256,            ! Label of the NDF
     :  PTITLE * 73,            ! Plot title (73=33+2*DIMSTR)
     :  UNITS * 256,            ! Units of the NDF
     :  XLAB * 132              ! Abscissa label (usually just a copy
                                ! of ABSLAB)
      DOUBLE PRECISION
     :  BINWID                  ! Bin width

      INTEGER
     :  AXPNTR( 1 ),            ! Pointer to the axis centres
     :  DUMMY,                  ! Dummy dimension
     :  EL,                     ! Number of array elements
     :  HIST( MXHSTB ),         ! Histogram
     :  HPPTR1,                 ! Pointer to work array in HSTPL
     :  HPPTR2,                 ! Pointer to work array in HSTPL
     :  IERR,                   ! Position of first error copying the
                                ! histogram to the output NDF
     :  MAXH,                   ! Maximum number in a histogram bin
     :  MAXPOS,                 ! Vector position of maximum pixel in
                                ! input array
     :  MINH,                   ! Minimum number in a histogram bin
     :  MINPOS                  ! Vector position of minimum pixel in
                                ! input array

      INTEGER
     :  NCHAR,                  ! Running length of plot title
     :  NCHLIM,                 ! No. of characters in a region limit
     :  NDFO,                   ! NDF identifier for output histogram
     :  NERR,                   ! Number of of conversion errors
                                ! copying the histogram to the output
                                ! NDF
     :  NINVAL,                 ! Number of bad values
     :  NPOS,                   ! No. of points in histogram locus
     :  NUMBIN,                 ! Number of bins in the histogram
     :  OUTPTR( 1 ),            ! Pointer to the DATA_ARRAY for storage
     :  UBND( 2 )               ! Upper bounds of current region

      REAL
     :  MAXIM,                  ! Maximum value of the region
     :  MINIM,                  ! Minimum value of the region
     :  XLOW, XHIGH,            ! x limits of the plot
     :  YLOW, YHIGH             ! y limits of the plot

      LOGICAL                   ! True if:
     :  BAD,                    ! Bad pixels may be present
     :  HIREP,                  ! Full histogram is to be reported to
                                ! the user
     :  THERE                   ! Units defined in input NDF

*-

*    Check the inherited status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL ERR_MARK

*    Compute the number of pixels in the array.

      EL = DIM1 * DIM2
      BAD = .TRUE.

*    Find maximum and minimum in region

      CALL KPG1_MXMNR( BAD, EL, ARRAY, NINVAL, MAXIM, MINIM,
     :                 MAXPOS, MINPOS, STATUS )

*    Get the number of bins in the histogram.

      CALL PAR_GDR0I( PNNUMB, RNUMB, 2, MXHSTB, .TRUE., NUMBIN, STATUS )
      CALL PAR_CANCL( PNNUMB, STATUS )

      IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

*    Compute the histogram.

      CALL KPG1_GHSTR( BAD, EL, ARRAY, NUMBIN, MAXIM, MINIM, HIST,
     :                 STATUS )

*    Make the default the current value.

      RNUMB = NUMBIN

*    The histogram has been obtained.

      HSTGRM = .TRUE.

*    Find the range of the numbers in the histogram bins.  Empty bins
*    will be filled with zero, so no need to check for bad values.

      BAD = .FALSE.
      CALL KPG1_MXMNI( BAD, NUMBIN, HIST, NINVAL,
     :                 MAXH, MINH, MAXPOS, MINPOS, STATUS )

      IF ( NUMBIN .GT. BINFAC ) THEN

*       Display the histogram.

         CALL HSTREP( HIST, MINIM, MAXIM, NUMBIN, BINFAC, STATUS )

*       Determine whether full histogram is to be reported to the
*       user, unless it is short, when it is always reported

         CALL PAR_GTD0L( PNHIRE, .FALSE., .TRUE., HIREP, STATUS )
         CALL PAR_CANCL( PNHIRE, STATUS )
      ELSE
         HIREP = .TRUE.
      END IF

*    Report the full histogram.

      IF ( HIREP ) THEN
         CALL HSTDSP( HIST, MINIM, MAXIM, NUMBIN, STATUS )
      END IF

*    Check for an error.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

      IF ( GRAPHS ) THEN

         IF ( PLTITL .EQ. 'Histogram of current region' ) THEN

*          Find the upper bounds of the region.

            UBND( 1 ) = LBND( 1 ) - 1 + DIM1
            UBND( 2 ) = LBND( 2 ) - 1 + DIM2

*          Extend the title, first by converting limits of the current
*          region to strings.

            CALL ERR_MARK
            CALL KPG_DIMLS( 2, LBND, NCHLIM, DIMSTR, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               PTITLE = PLTITL( :27 )//': '//DIMSTR( :NCHLIM )//' to '
               NCHAR = 33 + NCHLIM
               CALL KPG_DIMLS( 2, UBND, NCHLIM, DIMSTR, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL CHR_APPND( DIMSTR( :NCHLIM ), PTITLE, NCHAR )
               END IF
            END IF

*          Ignore any error from KPG_DIMLS.

            IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
            CALL ERR_RLSE
         ELSE

*          Take the title literally.

            PTITLE = PLTITL
         END IF

*       Plot in the line-graph zone.

         CALL SGS_SELZ( ZONEG, STATUS )

*       Get AUTOGRAPH to use the SGS zone.

         CALL SNX_AGWV

*       Create work space for histogram plotting.

         NPOS = 2 * ( NUMBIN + 1 ) + ( NUMBIN - 1 ) * 3
         CALL AIF_GETVM( '_REAL', 1, NPOS, HPPTR1, HPLOC1, STATUS )
         CALL AIF_GETVM( '_REAL', 1, NPOS, HPPTR2, HPLOC2, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*          Form locus of x-y points that is the histogram.

            CALL HSTLO( HIST, NUMBIN, MINIM, MAXIM, NULL,
     :                  XLOG, YLOG, YNULL, NPOS, %VAL(HPPTR1),
     :                  %VAL(HPPTR2), STATUS )

            XLAB = ABSLAB

*          Find the limits of the plot. First x.

            IF ( XLOG ) THEN
               IF ( MINIM .LE. 0.0 ) THEN

*                Note in this case HSTLO uses bin numbers.

                  XLOW = 0.5
                  XHIGH = 0.5 + REAL( NUMBIN )
                  CALL MSG_OUT( 'BINNOS', 'Some of the data used to '/
     :              /'form the histogram is zero or negative. Thus '/
     :              /'the abscissa is now bin numbers.', STATUS )
                  XLAB = 'Bin number'
               ELSE
                  XLOW = MINIM
                  XHIGH = MAXIM
               END IF
            ELSE
               XLOW = MINIM
               XHIGH = MAXIM
            END IF

*          Now y, but first find the maximum value of the histogram.

            IF ( YLOG ) THEN
               YLOW = YNULL
               YHIGH = INT( 1.05 * MAXH ) + 1
            ELSE
               YLOW = 0.0
               YHIGH = INT( 1.05 * MAXH ) + 1
            END IF

*          Plot locus just computed within annotated axes.  Both axes'
*          limits are defined.

            CALL LINPLT( %VAL(HPPTR1), %VAL(HPPTR2), NPOS, .TRUE.,
     :                   .TRUE., XLOW, YLOW, XHIGH, YHIGH, PTITLE,
     :                   XLAB, ORDLAB, MINTIC, MAJTIC, XLOG, YLOG,
     :                   OUTTIC, THICK, STATUS )

            CALL SGS_FLUSH
            PTITLE = ' '
         ELSE
            CALL ERR_REP( 'INHI_WPH',
     :        'INHI: Unable to get workspace for plotting the '/
     :        /'histogram.', STATUS )
            CALL ERR_FLUSH( STATUS )

*       End of check for no error getting workspace to plot histogram.

         END IF

*       Tidy work space structures.

         CALL AIF_ANTMP( HPLOC1, STATUS )
         CALL AIF_ANTMP( HPLOC2, STATUS )

*    End of graphics-output check.

      END IF

*    Create an output NDF.
*    =====================

*    Start a new error context.

      CALL ERR_MARK

*    Give some commentary.

      CALL MSG_OUT( 'NULLOP', 'Type the null character, !, if '/
     :  /'the histogram is not to be saved.', STATUS )

*    Start a new NDF context.

      CALL NDF_BEGIN

*    Create a new NDF.

      CALL LPG_CREP( PNHINA, '_REAL', 1, NUMBIN, NDFO, STATUS )

*    Map the data array.

      CALL KPG1_MAP( NDFO, 'Data', '_INTEGER', 'WRITE', OUTPTR,
     :              DUMMY, STATUS )

*    Get the title for the NDF.

      CALL NDF_CINP( PNTITL, NDFO, 'TITLE', STATUS )

*    Write the slice to the NDF.

      CALL VEC_ITOI( .FALSE., NUMBIN, HIST,
     :               %VAL( OUTPTR( 1 ) ), IERR, NERR, STATUS )

*    Write a label for the NDF.  The histogram is unitless.

      CALL NDF_CPUT( 'Number', NDFO, 'Label', STATUS )

*    Obtain the label from the input NDF.  Use a default value if there
*    is no label.  Put the label into the axis structure.

      LABEL = 'Data values'
      CALL NDF_CGET( NDFI, 'Label', LABEL, STATUS )
      CALL NDF_ACPUT( LABEL, NDFO, 'Label', 1, STATUS )

*    Put the units in the axis structure, which is also created at this
*    point, provided that the object exists in the input NDF.  There is
*    only one axis.

      CALL NDF_STATE( NDFI, 'Units', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( NDFI, 'Units', UNITS, STATUS )
         CALL NDF_ACPUT( UNITS, NDFO, 'Units', 1, STATUS )
      END IF

*    Map the axis centres.

      CALL NDF_AMAP( NDFO, 'CENTRE', 1, '_REAL', 'WRITE', AXPNTR,
     :               NUMBIN, STATUS )

*    Fill the axis array with the centres of the histogram bins by
*    first finding the bin width.  This assumes an even distribution of
*    points within the bin which is probably not the case, but the
*    difference is only likely to be serious when the number of
*    non-empty bins is low.

      BINWID = DBLE( MAXIM - MINIM ) / DBLE( NUMBIN )
      CALL KPG1_SSCOF( NUMBIN, BINWID, DBLE( MINIM ) + 0.5D0 * BINWID,
     :                 %VAL( AXPNTR( 1 ) ), STATUS )

*    Handle the null case invisibly.

      IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*    Close down the NDF system.

      CALL NDF_END( STATUS )

*    Release the new error context.

      CALL ERR_RLSE

*    Cancel the parameters for the INSPECT loop.

      CALL PAR_CANCL( PNHINA, STATUS )
      CALL PAR_CANCL( PNTITL, STATUS )

 999  CONTINUE

      CALL ERR_RLSE
      END
