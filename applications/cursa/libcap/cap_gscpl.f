      SUBROUTINE CAP_GSCPL (PLTSYM, COLOUR, STATUS)
*+
*  Name:
*     CAP_GSCPL
*  Purpose:
*     Plot a scatterplot.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSCPL (PLTSYM, COLOUR; STATUS)
*  Description:
*     Plot a scatterplot.  Two selected expressions are plotted
*     from the current selection.
*  Arguments:
*     PLTSYM  =  CHARACTER*(*) (Given)
*        CURSA plotting symbol.
*     COLOUR  =  CHARACTER*(*) (Given)
*        CURSA plotting colour.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is a catalogue open then
*       If there is a plot open then
*         If the axis expressions have been specified then
*           Determine the number of rows in the current selection.
*           Release any previously mapped arrays.
*           Map in work arrays to hold the points.
*           Read in the values for the expressions.
*           If ok then
*             If the axes have not been plotted then
*               Clear the screen.
*               Plot the axes.
*               Update the 'axes plotted' flag.
*             end if
*             Translate the CURSA plotting symbol and colour into
*             PGPLOT codes.
*             Plot the selection.
*             If ok then
*               Copy details of the points plotted to the common block.
*             end if
*             Report any error.
*           else
*             Report error reading the expressions to plot.
*           end if
*         else
*           Report warning; expressions not specified.
*         end if
*       else
*         Report warning; no plot open.
*       end if
*     else
*       Report warning; no open catalogue.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     7/9/98   (ACD): Original version.
*     19/11/98 (ACD): First stable version.
*     15/9/99  (ACD): Corrected the routine name in the error reporting.
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
     :  PLTSYM*(*),
     :  COLOUR*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  SI,      ! Selection identifier.
     :  ROWS,    ! Number of rows in the selection.
     :  XPTR,    ! Pointer to the array of X values.
     :  YPTR,    !    "    "   "    "   "  Y   "   .
     :  XID,     ! Identifier for the X axis expression.
     :  YID,     !      "      "   "  Y  "       "     .
     :  PTS,     ! Number of points to be plotted.
     :  LTITLE,  ! Length of TITLE (excl. trail. blanks).
     :  LCRIT    !   "    "  CRIT  ( "  .   "  .   "   ).
      INTEGER
     :  PGSYMB,  ! PGPLOT plotting symbol.
     :  PGCOL,   ! PGPLOT colour index.
     :  NUMNUL   ! Number of null rows in the selection.
      CHARACTER
     :  TITLE*50,          ! Title.
     :  CRIT*(CAT__SZEXP)  ! Selection expression.
      REAL
     :  XMIN,    ! Minimum of the X axis.
     :  XMAX,    ! Maximum "   "  "  "  .
     :  YMIN,    ! Minimum "   "  Y  "  .
     :  YMAX,    ! Maximum "   "  "  "  .
     :  XRANGE,  ! X range.
     :  YRANGE   ! Y   "  .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Check that there is a plot open.

            IF (OPEN__SPLOT) THEN

*
*             Check that the expressions for the axes have been
*             defined.

               IF (XPRS__SPLOT) THEN

*
*                Determine the current selection and the number of
*                rows in it.

                  SI = SELID__SGZ(CSEL__SGZ)
                  CALL CAT_TROWS (SI, ROWS, STATUS)

*
*                Release any previously mapped arrays.

                  IF (PTS__SPLOT .GT. 0) THEN
                     CALL CAP_FREAR (XPTR__SPLOT, STATUS)
                     CALL CAP_FREAR (YPTR__SPLOT, STATUS)
                  END IF

*
*                Map in work arrays to hold the points to be plotted.

                  CALL CAP_CRTAR (ROWS, '_REAL', XPTR, STATUS)
                  CALL CAP_CRTAR (ROWS, '_REAL', YPTR, STATUS)

*
*                Read in the values for the expressions and proceed
*                if all is ok.

                  XID = XID__SPLOT
                  YID = YID__SPLOT

                  CALL CAP_RDSCT (SI, XID, YID, ROWS, PTS,
     :              %VAL(CNF_PVAL(XPTR)), %VAL(CNF_PVAL(YPTR)),
     :              NUMNUL, STATUS)

                  IF (NUMNUL .GT. 0) THEN
                     CALL MSG_SETI ('NUMNUL', NUMNUL)
                     CALL CAP_INFO (GUI__SGZ, ' ', '^NUMNUL rows not '/
     :                 /'plotted because they contained null values.',
     :                 STATUS)
                  END IF

                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   If the axes have not yet been plotted then
*                   plot them and update the 'axes plotted' flag.

                     IF (.NOT. AXPL__SPLOT) THEN

*
*                      First assemble the title if one has not been
*                      given.

                        IF (TITLE__SPLOT .NE. ' ') THEN
                           TITLE = TITLE__SPLOT

                        ELSE
                           TITLE = ' '
                           LTITLE = 0

                           CALL CHR_PUTC ('Selection ', TITLE, LTITLE)

                           CALL CHR_PUTI (CSEL__SGZ, TITLE, LTITLE)
                           CALL CHR_PUTC (': ', TITLE, LTITLE)

                           CRIT = CRIT__SGZ(CSEL__SGZ)

                           IF (CRIT .NE. ' ') THEN
                              LCRIT = CHR_LEN(CRIT)
                              CALL CHR_PUTC (CRIT(1 : LCRIT), TITLE,
     :                          LTITLE)
                           ELSE
                              CALL CHR_PUTC ('<none>', TITLE, LTITLE)
                           END IF
                        END IF

*
*                      Next determine the plotting range.

                        IF (AUTO__SPLOT) THEN
                           CALL CAP_PRNG (PTS, %VAL(CNF_PVAL(XPTR)),
     :                       XMIN, XMAX, STATUS)
                           CALL CAP_PRNG (PTS, %VAL(CNF_PVAL(YPTR)),
     :                       YMIN, YMAX, STATUS)

                           XRANGE = XMAX - XMIN
                           YRANGE = YMAX - YMIN

                           XMIN = XMIN - (XRANGE * 2.0E-2)
                           XMAX = XMAX + (XRANGE * 2.0E-2)
                           YMIN = YMIN - (YRANGE * 2.0E-2)
                           YMAX = YMAX + (YRANGE * 2.0E-2)

                           XMIN__SPLOT = XMIN
                           XMAX__SPLOT = XMAX
                           YMIN__SPLOT = YMIN
                           YMAX__SPLOT = YMAX

                        ELSE
                           XMIN = XMIN__SPLOT
                           XMAX = XMAX__SPLOT
                           YMIN = YMIN__SPLOT
                           YMAX = YMAX__SPLOT

                        END IF

*
*                      Clear the screen.

                        CALL PGPAGE

*
*                      Plot the axes.

                        CALL CAP_SAXES (XMIN, XMAX, YMIN, YMAX,
     :                    TITLE, XLABL__SPLOT, YLABL__SPLOT, STATUS)

*
*                      Set the flag indicating that the axes have
*                      been plotted.

                        AXPL__SPLOT = .TRUE.

                     END IF

*
*                   Translate the CURSA plotting symbol and colour
*                   into the corresponding PGPLOT codes.

                     CALL CAP_TPSYM (PLTSYM, PGSYMB, STATUS)
                     CALL CAP_TPCOL (COLOUR, PGCOL, STATUS)

*
*                   Plot the selection.

                     CALL CAP_PSCAT (PGSYMB, PGCOL, PTS,
     :                 %VAL(CNF_PVAL(XPTR)), %VAL(CNF_PVAL(YPTR)),
     :                 STATUS)

*
*                   If all is ok then copy the details of the points
*                   plotted to the scatterplot common block.

                     IF (STATUS .EQ. SAI__OK) THEN
                        PTS__SPLOT = PTS
                        XPTR__SPLOT = XPTR
                        YPTR__SPLOT = YPTR
                     END IF

*
*                   Report any error.

                     IF (STATUS .NE. SAI__OK) THEN
                        CALL ERR_REP ('CAP_GSCPL_ERR', 'Failed to '/
     :                    /'generate scatterplot.', STATUS)

                     END IF

                  ELSE
                     CALL ERR_REP ('CAP_GSCPL_RD', 'Error '/
     :                 /'reading the columns to be plotted.',
     :                 STATUS)

                  END IF

               ELSE
                  CALL CAP_WARN (GUI__SGZ, ' ', 'The columns to '/
     :              /'be plotted have not been defined.', STATUS)

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
