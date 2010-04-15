      SUBROUTINE CAP_GHSPL (COLOUR, STATUS)
*+
*  Name:
*     CAP_GHSPL
*  Purpose:
*     Plot a histogram.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GHSPL (COLOUR; STATUS)
*  Description:
*     Plot a GHSPL.  A selected expression is histogrammed from the
*     current selection.
*  Arguments:
*     COLOUR  =  CHARACTER*(*) (Given)
*        CURSA plotting colour.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is a catalogue open then
*       If there is a histogram open then
*         Determine the number of rows in the current selection.
*         Release any previously mapped arrays.
*         Map in work arrays to hold the points.
*         Read in the values for the expression.
*         If ok then
*           Determine the X range.
*           If the X range is valid then
*             Map the work arrays.
*             Generate the histogram.
*             If the histogram contains some points then
*               If the axes have not been plotted then
*                 Clear the screen.
*                 Plot the axes.
*                 Update the 'axes plotted' flag.
*               end if
*               Translate the CURSA plotting colour into the corresponding
*               PGPLOT code.
*               Plot the histogram.
*               Report the histogram details.
*               If ok then
*                 Copy details of the points plotted to the common block.
*               end if
*               Report any error.
*             else
*               Report warning: histogram contains no points.
*             end if
*             Release the work arrays.
*           else
*             Report warning: zero or negative X range.
*             Release the array to hold the points.
*           end if
*         else
*           Report error reading the expression to histogram.
*         end if
*       else
*         Report warning: no plot open.
*       end if
*     else
*       Report warning; no open catalogue.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/9/99  (ACD): Original version (from CAP_GSCPL).
*     19/11/99 (ACD): First stable version.
*     1/12/99  (ACD): Changed SNGL to REAL for Linux.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'SGZ_PAR'           ! catview parametric constants.
      INCLUDE 'CNF_PAR'           ! CNF functions
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! catview common block.
      INCLUDE 'SPLOT_CMN'         ! catview scatterplot common block.
*  Arguments Given:
      CHARACTER
     :  COLOUR*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      REAL XBIT  ! Minimum X range for the histogram.
      PARAMETER (XBIT = 1.0E-10)
*  Local Variables:
      INTEGER
     :  SI,      ! Selection identifier.
     :  ROWS,    ! Number of rows in the selection.
     :  XPTR,    ! Pointer to the array of X values.
     :  XID,     ! Identifier for the X axis expression.
     :  PTS,     ! Number of points to be histogrammed.
     :  BINS,    ! Number of bins in the histogram.
     :  WPTS     ! Number of points in line to be plotted.
      INTEGER
     :  HXPTR,   ! Pointer to histogram X values.
     :  HYPTR,   !    "    "      "     Y   "   .
     :  IHYPTR,  !    "    "      "     Y   "    (INTEGER work array).
     :  WXPTR,   !    "    "  X coords. of line to be plotted.
     :  WYPTR,   !    "    "  Y   "   . "   "   "  "     "   .
     :  NUMNUL,  ! Number of null rows in the selection.
     :  NUMEXC,  ! Number of points lying outside the histogram range.
     :  NUMINC   ! Number of points used to construct the histogram.
      INTEGER
     :  LTITLE,  ! Length of TITLE (excl. trail. blanks).
     :  LCRIT,   !   "    "  CRIT  ( "  .   "  .   "   ).
     :  PGCOL    ! PGPLOT colour index.
      CHARACTER
     :  TITLE*50,          ! Title.
     :  CRIT*(CAT__SZEXP), ! Selection expression.
     :  YLABEL*40          ! Y label for the histogram.
      REAL
     :  BINWD,   ! Width of each histogram bin.
     :  XMIN,    ! Minimum of the histogram X range.
     :  XMAX,    ! Maximum "   "      "     X   "  .
     :  YMIN,    ! Minimum "   "      "     Y   "  .
     :  YMAX,    ! Maximum "   "      "     Y   "  .
     :  XRANGE,  ! X range.
     :  YRANGE   ! Y   "  .
      REAL
     :  XMINP,   ! X minimum of the plotting range.
     :  XMAXP,   ! X maximum "   "     "       "  .
     :  YMINP,   ! Y minimum "   "     "       "  .
     :  YMAXP    ! Y maximum "   "     "       "  .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Check that there is a histogram open.

            IF (OPEN__HIST) THEN

*
*             Determine the current selection and the number of
*             rows in it.

               SI = SELID__SGZ(CSEL__SGZ)
               CRIT = CRIT__SGZ(CSEL__SGZ)

               CALL CAT_TROWS (SI, ROWS, STATUS)

*
*             Release any previously mapped array.

               IF (PTS__HIST .GT. 0) THEN
                  CALL CAP_FREAR (XPTR__HIST, STATUS)
               END IF

*
*             Map in work arrays to hold the points to be histogrammed.

               CALL CAP_CRTAR (ROWS, '_REAL', XPTR, STATUS)

*
*             Read in the values for the expression and proceed
*             if all is ok.

               XID = XID__HIST

               CALL CAP_RDCLR (SI, XID, ROWS, PTS,
     :           %VAL(CNF_PVAL(XPTR)), NUMNUL, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN

*
*                Determine the X range of the values to be histogrammed.

                  IF (AUTO__HIST) THEN
                     CALL CAP_PRNG (PTS, %VAL(CNF_PVAL(XPTR)), XMIN,
     :                 XMAX, STATUS)
                  ELSE
                     XMIN = XMIN__HIST
                     XMAX = XMAX__HIST
                  END IF

                  XRANGE = XMAX - XMIN

*
*                Check that there is a valid X range.

                  IF (XRANGE .GT. XBIT) THEN

*
*                   Determine the histogram width and number of bins.


                     IF (BINSP__HIST) THEN
                        BINWD = BINWD__HIST
                        BINS = INT(XRANGE / BINWD) + 1
                     ELSE
                        BINWD = XRANGE / REAL(NBINS__HIST)
                        BINS = NBINS__HIST
                     END IF

*
*                   Map the arrays to hold X and Y values of the histogram
*                   and the X and Y coordinates of the line used to plot
*                   the histogram.

                     CALL CAP_CRTAR (BINS, '_REAL', HXPTR, STATUS)
                     CALL CAP_CRTAR (BINS, '_REAL', HYPTR, STATUS)
                     CALL CAP_CRTAR (BINS, '_INTEGER', IHYPTR, STATUS)

                     WPTS = BINS * 2

                     CALL CAP_CRTAR (WPTS, '_REAL', WXPTR, STATUS)
                     CALL CAP_CRTAR (WPTS, '_REAL', WYPTR, STATUS)

*
*                   Generate the histogram.

                     CALL CAP_GNHST (XMIN, XMAX, BINWD, NORM__HIST,
     :                 PTS, %VAL(CNF_PVAL(XPTR)), BINS,
     :                 %VAL(CNF_PVAL(IHYPTR)), %VAL(CNF_PVAL(HXPTR)),
     :                 %VAL(CNF_PVAL(HYPTR)), YMIN, YMAX, NUMINC,
     :                 NUMEXC, STATUS)

*
*                   Proceed if the histogram includes some points.

                     IF (NUMINC .GT. 0) THEN

*
*                      If the axes have not yet been plotted then
*                      plot them and update the 'axes plotted' flag.

                        IF (.NOT. AXPL__HIST) THEN

*
*                         First assemble the title if one has not been
*                         given.

                           IF (TITLE__HIST .NE. ' ') THEN
                              TITLE = TITLE__HIST

                           ELSE
                              TITLE = ' '
                              LTITLE = 0

                              CALL CHR_PUTC ('Selection ', TITLE,
     :                          LTITLE)

                              CALL CHR_PUTI (CSEL__SGZ, TITLE, LTITLE)
                              CALL CHR_PUTC (': ', TITLE, LTITLE)

                              IF (CRIT .NE. ' ') THEN
                                 LCRIT = CHR_LEN(CRIT)
                                 CALL CHR_PUTC (CRIT(1 : LCRIT), TITLE,
     :                             LTITLE)
                              ELSE
                                 CALL CHR_PUTC ('<none>', TITLE, LTITLE)
                              END IF
                           END IF

*
*                         Next determine the plotting range.
*
*                         In the case where the range is not auto-scaled
*                         note:
*                         - the X range has already been set,
*                         - the Y range is only set from the common
*                           block if there is a valid range in the
*                           common block; otherwise the locally computed
*                           values are retined.  The behaviour ensures that
*                           the Y range is computed correctly the first
*                           time a histogram is plotted.

                           IF (AUTO__HIST) THEN
                              XMIN__HIST = XMIN
                              XMAX__HIST = XMAX
                              YMIN__HIST = YMIN
                              YMAX__HIST = YMAX
                           ELSE
                              IF (YMAX__HIST .GT. YMIN__HIST) THEN
                                 YMIN = YMIN__HIST
                                 YMAX = YMAX__HIST
                              ELSE
                                 YMIN__HIST = YMIN
                                 YMAX__HIST = YMAX
                              END IF
                           END IF

                           XRANGE = XMAX - XMIN
                           YRANGE = YMAX - YMIN

                           XMINP = XMIN - (XRANGE * 2.0E-2)
                           XMAXP = XMAX + (XRANGE * 2.0E-2)
                           YMINP = YMIN - (YRANGE * 2.0E-2)
                           YMAXP = YMAX + (YRANGE * 2.0E-2)

*
*                         Clear the screen.

                           CALL PGPAGE

*
*                         Plot the axes.

                           IF (NORM__HIST) THEN
                              YLABEL = 'Normalised counts'
                           ELSE
                              YLABEL = 'Counts'
                           END IF

                           CALL CAP_SAXES (XMINP, XMAXP, YMINP, YMAXP,
     :                       TITLE, XLABL__HIST, YLABEL, STATUS)

*
*                         Set the flag indicating that the axes have
*                         been plotted.

                           AXPL__HIST = .TRUE.

                        END IF

*
*                      Translate the CURSA plotting colour into the
*                      corresponding PGPLOT code.

                        CALL CAP_TPCOL (COLOUR, PGCOL, STATUS)

*
*                      Plot the histogram.

                        CALL CAP_PHIST (PGCOL, BINS,
     :                    %VAL(CNF_PVAL(HXPTR)),
     :                    %VAL(CNF_PVAL(HYPTR)), WPTS,
     :                    %VAL(CNF_PVAL(WXPTR)),
     :                    %VAL(CNF_PVAL(WYPTR)),
     :                    STATUS)

*
*                      Report the histogram details.

                        CALL CAP_HSDET (CSEL__SGZ, CRIT, BINS, BINWD,
     :                    COLOUR, NUMINC, NUMEXC, NUMNUL, STATUS)

*
*                      If all is ok then copy the details of the points
*                      histogrammed to the histogram common block.

                        IF (STATUS .EQ. SAI__OK) THEN
                           PTS__HIST = PTS
                           XPTR__HIST = XPTR
                        END IF

*
*                      Report any error.

                        IF (STATUS .NE. SAI__OK) THEN
                           CALL ERR_REP ('CAP_GHSPL_ERR', 'Failed to '/
     :                       /'plot the histogram.', STATUS)

                        END IF

                     ELSE
                        CALL CAP_WARN (GUI__SGZ, ' ', 'The histogram '/
     :                    /'range contains no points.', STATUS)

                     END IF

*
*                   Release the work arrays.

                     CALL CAP_FREAR (HXPTR, STATUS)
                     CALL CAP_FREAR (HYPTR, STATUS)
                     CALL CAP_FREAR (IHYPTR, STATUS)

                     CALL CAP_FREAR (WXPTR, STATUS)
                     CALL CAP_FREAR (WYPTR, STATUS)

                  ELSE
                     CALL CAP_WARN (GUI__SGZ, ' ', 'The histogram X '/
     :                 /'range is zero or negative.', STATUS)

                     CALL CAP_FREAR (XPTR, STATUS)

                  END IF

               ELSE
                  CALL ERR_REP ('CAP_GHSPL_RD', 'Error reading the '/
     :              /'column to be histogrammed.', STATUS)

               END IF

            ELSE
               CALL CAP_WARN (GUI__SGZ, ' ', 'There is no plot '/
     :           /'open.', STATUS)

            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
