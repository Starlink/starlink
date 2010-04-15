      SUBROUTINE CATVIEW (STATUS)
*+
*  Name:
*     CATVIEW
*  Purpose:
*     Application to browse and generate selections from a catalogue.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATVIEW (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     catview is an application for browsing catalogues and selecting
*     subsets from the command line.  It provides facilities to:
*
*     * list the columns in a catalogue,
*
*     * list the parameters and textual information from a catalogue,
*
*     * list new columns computed on-the-fly using an algebraic
*       expression defined in terms of existing columns and parameters.
*       For example, if the catalogue contained columns V and B_V
*       (corresponding to the V magnitude and B-V colour) then the B
*       magnitude  could be listed by specifying the expression
*       V + B_V.
*
*     * fast creation of a subset within a specified range for a sorted
*       column,
*
*     * creation of subsets defined by algebraic criteria. For example,
*       if the catalogue again contained columns V and B_V then
*       to find the stars in the catalogue fainter than twelfth magnitude
*       and with a B-V of greater than 0.5 the criteria would be
*       V > 12.0 .AND. B_V > 0.5,
*
*     * subsets extracted from the catalogue can be saved as new
*       catalogues. These subsets can include new columns computed from
*       expressions as well as columns present in the original catalogue,
*
*     * subsets extracted from the catalogue can be saved in a text file
*       in a form suitable for printing, or in a form suitable for passing
*       to other applications (that is, unencumbered with extraneous
*       annotation).
*  Usage:
*     catview
*  ADAM Parameters:
*     CNAME  =  CHARACTER (read)
*        Give the name of the catalogue to be reported.
*     ACTION  =  CHARACTER (read)
*        Enter required action; HELP for a list of options.
*     CMPLST  =  CHARACTER (read)
*        Enter list of columns and expressions, separated by semi-colons.
*     SELNO  =  INTEGER (read)
*        Enter the number of the required selection.
*     EXPR  =  CHARACTER (read)
*        Enter an expression defining the required selection.
*     MINRNG  =  CHARACTER (read)
*        Enter minimum value of the required range.
*
*        If the column within which the range is being specified is
*        not an angle then simply enter the required value.
*
*        If the column is an angle then the value can be entered as
*        either a decimal value in radians or a sexagesimal value in
*        hours or degrees, minutes and seconds.  If a sexagesimal value
*        is specified then the hours or degrees, minutes and seconds
*        should be separated by a colon (:).  Optionally fractional
*        seconds can be specified by including a decimal point and the
*        required number of places of decimals.  An unsigned value is
*        assumed to be in hours and a signed value in degrees (a
*        negative angle cannot be specified in hours).  That is,
*        a positive angle in degrees must be preceded by a plus sign.
*
*        Examples: any of the following values could be entered to
*        to specify an angle of 30 degrees:
*
*            2:00:00.0   hours (decimal point included in seconds)
*            2:00:00     hours (integer number of seconds)
*
*          +30:00:00.0   degrees (decimal point included in seconds)
*          +30:00:00     degrees (integer number of seconds)
*
*            0.5235988   radians
*     MAXRNG  =  CHARACTER (read)
*        Enter maximum value of the required range.
*
*        If the column within which the range is being specified is
*        not an angle then simply enter the required value.
*
*        If the column is an angle then the value can be entered as
*        either a decimal value in radians or a sexagesimal value in
*        hours or degrees, minutes and seconds.  If a sexagesimal value
*        is specified then the hours or degrees, minutes and seconds
*        should be separated by a colon (:).  Optionally fractional
*        seconds can be specified by including a decimal point and the
*        required number of places of decimals.  An unsigned value is
*        assumed to be in hours and a signed value in degrees (a
*        negative angle cannot be specified in hours).  That is,
*        a positive angle in degrees must be preceded by a plus sign.
*
*        Examples: any of the following values could be entered to
*        to specify an angle of 30 degrees:
*
*            2:00:00.0   hours (decimal point included in seconds)
*            2:00:00     hours (integer number of seconds)
*
*          +30:00:00.0   degrees (decimal point included in seconds)
*          +30:00:00     degrees (integer number of seconds)
*
*            0.5235988   radians
*     ROWNO  =  INTEGER (read)
*        Enter the required row number in the current selection.
*     FIRSTR  =  INTEGER (read)
*        Enter the first row to be listed in the current selection.
*     LASTR  =  INTEGER (read)
*        Enter the last row to be listed (0 = last in the current selection).
*     FLNAME  =  CHARACTER (read)
*        Enter the name of the output text file.
*     CATOUT  =  CHARACTER (read)
*        Enter the name of the output catalogue.
*     CFLAG  =  LOGICAL (read)
*        Columns to be saved:
*        true - all columns;  false - only currently chosen.
*     TFLAG  =  LOGICAL (read)
*        Save header text from base catalogue?  The permitted
*        responses are:  true - save header;  false - do not save text.
*     COMM  =  CHARACTER (read)
*        Enter comments to annotate the new catalogue.
*     PNAME  =  CHARACTER (read)
*        Enter the name of column or parameter.
*     UNITS  =  CHARACTER (read)
*        Enter the new units for the column or parameter.
*     EXFMT  =  CHARACTER (read)
*        Enter the new external format for the column or parameter.
*     SWID  =  INTEGER (read)
*        Enter the screen width in characters.
*     SHT  =  INTEGER (read)
*        Enter the screen height in number of lines.
*     SEQNO  =  LOGICAL (read)
*        Should a sequence number be listed with each row?
*     NLIST  =  INTEGER (read)
*        Enter the number of lines for LIST to output; -1 for them all
*     ANGRPN  =  CHARACTER (read)
*        Control the way in which angles are displayed.  The permitted
*        responses are:  SEXAGESIMAL - sexagesimal hours or degrees,
*        RADIANS - radians.
*     ANGRF  =  LOGICAL (read)
*        Reformat the UNITS attribute for angles?
*     GUI  =  LOGICAL (read)
*        Is the application being run from a GUI?
*     FPRINT  =  LOGICAL (read)
*        Flag; is output file a print file or a data file, coded as follows:
*        .TRUE.  -  print file,
*        .FALSE. -  data file.
*     FPGSZE  =  INTEGER (read)
*        Enter the number of lines in a page of output.
*     FWID  =  INTEGER (read)
*        Enter the width of line in the output file, in characters.
*     FSUMM  =  CHARACTER (read)
*        Include summary in text file?  The permitted responses are:
*        A = absent, F = include summary.
*     FCOL  =  CHARACTER (read)
*        Include column details in text file?  The permitted responses
*        are:  A = absent, S = summary only, F = full details.
*     FPAR  =  CHARACTER (read)
*        Include parameter details in text file?  The permitted responses
*        are:  A = absent, S = summary only, F = full details.
*     FTXT  =  CHARACTER (read)
*        Include header text in text file?  The permitted responses are:
*        A = absent, F = include full text.
*     FTABL  =  CHARACTER (read)
*        Include data table in text file?  The permitted responses are:
*        A = absent, S = columns only, F = Columns and headings.
*     CMPSTT  =  CHARACTER (read)
*        Enter list of columns separated by semi-colons.
*     DECPL  =  INTEGER (read)
*        Enter the number of decimal places for displaying statistics.
*        Note that this quantity controls only the precision with
*        which the statistics are displayed, not the precision with
*        which they are computed; they are computed as DOUBLE PRECISION
*        numbers.
*     SFNAME  =  CHARACTER (read)
*        Enter the name of the file to hold the column statistics.
*     GRPHDV  =  CHARACTER (read)
*        Give the name of the graphics device.
*     TITLE  =  CHARACTER (read)
*        Enter the title to be displayed on the plot.
*     XEXPR  =  CHARACTER (read)
*        Enter column or expression defining the plot X-axis.
*     YEXPR  =  CHARACTER (read)
*        Enter column or expression defining the plot Y-axis.
*     AUTOSCL  =  LOGICAL (read)
*        Flag; is the scatter-plot to be auto-scaled?
*     CXMIN  =  CHARACTER (read)
*        Minimum value to be plotted on X axis.
*     CXMAX  =  CHARACTER (read)
*        Maximum value to be plotted on X axis.
*     CYMIN  =  CHARACTER (read)
*        Minimum value to be plotted on Y axis.
*     CYMAX  =  CHARACTER (read)
*        Maximum value to be plotted on Y axis.
*     PLTSYM  =  CHARACTER (read)
*        Plotting symbol to be used in scatter-plot.
*     COLOUR  =  CHARACTER (read)
*        Colour of the plotting symbols to be used in scatter-plot.
*     BINSP  =  LOGICAL (read)
*        Histogram bin specification:
*        TRUE  -  the bins are specified by their width,
*        FALSE -  the total number of bins is specified.
*     BINDET  =  REAL (read)
*        The details of the histogram bins.  If BINSP is TRUE then
*        BINDET is the width of each bin.  If BINSP is FALSE then it
*        is the total number of bins.
*     NORML  =  LOGICAL (read)
*        Flag; is the histogram to be normalised?
*     QUIET  =  LOGICAL (read)
*        Operate in quiet mode where warnings are suppressed.  The
*        permitted values are:
*        TRUE  - quiet mode,
*        FALSE - verbose mode.
*  Examples:
*     catselect
*        You will be placed in a command prompt where you enter commands
*        to examine the catalogue and generate subsets of it.  Type
*        HELP to see a list of commands.
*  Pitfalls:
*     catview is not really intended to be used interactively and is
*     somewhat terse and inconvenient.  If possible you should use the
*     GUI-based catalogue browser xcatview instead.  However, xcatview
*     requires an X display and catview may be useful if you do not
*     have one.  It may also be useful for running prepared scripts which
*     perform routine, standard, batch type operations.
*  Algorithm:
*     catview has two modes of operation.  For interactive use it is
*     driven from an internal command loop.  Conversely when being
*     driven from a GUI it executes a single action.
*
*     Obtain and set the quiet mode.
*     Determine whether being driven from a GUI
*     If driven from a GUI then
*       Get the action name
*     end if
*     Do while (more reporting required)
*       If not being driven from a GUI then
*         Obtain the action required
*       end of
*       Convert the action into upper case.
*       If the action is 'OPEN' then
*         If there is no open catalogue then
*           Initialise the catview common block.
*           Attempt to open the new catalogue.
*           Attempt to initialise the catalogue.
*         else
*           Report a warning; there is already a catalogue open.
*         end if
*       else if the action is 'SHOWCOL' then
*         Display a list of the columns in the catalogue.
*       else if the action is 'DETCOL' then
*         Display full details of all the columns.
*       else if the action is 'SHOWPAR' then
*         Display a list of the parameters in the catalogue.
*       else if the action is 'DETPAR' then
*         Display full details of all the parameters.
*       else if the action is 'SHOWTXT' then
*         Display the header text information.
*       else if the action is 'SHOWROWS' then
*         Show the number of rows in the current selection.
*       else if the action is 'SETCMP' then
*         Enter the list of components to be displayed.
*       else if the action is 'SHOWSEL' then
*         Display the selections which currently exist.
*       else if the action is 'CHOSEL' then
*         Choose an existing selection to become the current
*         selection.
*       else if the action is 'SETSEL' then
*         Create a new selection.
*       else if the action is 'SHOWRNG' then
*         Show the columns available for creating a range selection.
C         Currently sorted columns, ultimately also index columns.
*       else if the action is 'SETRNG' then
*         Create a new range selection.
*       else if the action is 'SETROW' then
*         Set the current row number.
*       else if the action is 'LIST' then
*         List the current selection to the screen.
*       else if the action is 'PREV' then
*         List the previous page of the current selection to the
*         screen.
*       else if the action is 'SETSTAT' then
*         Enter the list of columns for which statistics are to be
*         computed.
*       else if the action is 'SETDECPL' then
*         Set the number of number of decimal places for displaying
*         statistics.
*       else if the action is 'STATS' then
*         Compute statistics on the specified columns.
*       else if the action is 'SCOPEN' then
*         Open a new scatter-plot.
*       else if the action is 'SCRANGE' then
*         Set the axis ranges for a scatter-plot.
*       else if the action is 'SCPLOT' then
*         Plot the current selection in the current scatter-plot.
*       else if the action is 'SCSHRNG' then
*         Show the range of the current plot.
*       else if the action is 'SCLOSE' then
*         Close the current scatter-plot.
*       else if the action is 'HSOPEN' then
*         Open a new histogram.
*       else if the action is 'HSRANGE' then
*         Set the axis ranges for a histogram.
*       else if the action is 'HSPLOT' then
*         Plot the current selection in the current scatter-plot.
*       else if the action is 'HSSHRNG' then
*         Show the range of the current histogram.
*       else if the action is 'HSCLOSE' then
*         Close the current histogram.
*       else if the action is 'FILE' then
*         List the current selection to a file.
*       else if the action is 'SAVECAT' then
*         Save the current selection as a catalogue.
*       else if the action is 'SHOWFMT' then
*         Show the data type, units and external display format for
*         the column.
*       else if the action is 'SETFMT' then
*         Set a new external display format for a column or
*         parameter.
*       else if the action is 'SETCONF' then
*         Set the screen configuration.
*       else if the action is 'SETFILE' then
*         Set the text file configuration options.
*       else if the action is 'COLNAME' then
*         Display the names of all the columns in the catalogue.
*       else if the action is 'HELP' then
*         List the various actions.
*       else if the action is 'EXIT' then
*         Set the termination flag.
*         Close any plot.
*         If there is a catalogue open then
*           Attempt to close the catalogue.
*         end if
*       else
*         Report a message; unknown action.
*       end if
*       If the status is not ok then
*         Report the error and annul the status.
*       end if
*       If the application is being driven from a GUI then
*         Set the termination flag.
*       end if
*     end do
*     Report any error.
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/4/94  (ACD): Original version.
*     1/10/95  (ACD): First stable version.
*     19/12/95 (ACD): Added the ability to handle continuation lines
*        for entering long lists of components (columns in practice)
*        to be displayed or long expressions for selections.  Note that
*        this facility is only available if the application is being
*        run without a GUI.
*     20/10/96 (ACD): Modified for Tcl/Tk v4 and expect v5; added a
*        call to MSG_OUT after getting the 'ACTION' parameter if the
*        application is being driven from a GUI.
*     29/11/96 (ACD): Added options for computing statistics on columns.
*     26/11/98 (ACD): Added options for scatter-plots.
*     21/6/99  (ACD): Added parameter FPRINT.
*     28/6/99  (ACD): Started modifications to be driven from STARTCL.
*     13/9/99  (ACD): Completed modifications to be driven from STARTCL.
*     15/9/99  (ACD): Added options for histograms.
*     17/11/99 (ACD): Documented the histogram options.
*     30/11/99 (ACD): Reinstated calling MSG_TUNE to suprress wrapping
*        output lines when catview is being driven from a GUI.
*     5/4/01   (ACD): Added the quiet mode.
*     4/11/01  (ACD): Removed single quote characters from the prologue
*        comments; they were confusing SST.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'CAT_PAR'          ! CAT parametric constants.
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  CNAME*(CAT__SZCNF),  ! Catalogue name.
     :  ACTION*10,           ! Current action.
     :  CMPLST*2000,         ! List of columns and expressions for display.
     :  CMPSTT*2000,         ! List of columns for statistics.
     :  EXPR*(CAT__SZEXP),   ! Expression defining selection.
     :  FLNAME*75,           ! Name of output text file.
     :  SFNAME*75,           ! Name of output statistics text file.
     :  PNAME*(CAT__SZCMP),  ! Name of a column or parameter.
     :  UNITS*(CAT__SZUNI),  ! New units.
     :  EXFMT*(CAT__SZEXF)   ! New external format.
      CHARACTER
     :  CATOUT*(CAT__SZCNF), ! Name of output catalogue.
     :  COMM*70,    ! Comments for output catalogue.
     :  MINRNG*75,  ! Minimum extent of a range selection.
     :  MAXRNG*75,  ! Maximum   "    "  "   "       "    .
     :  FSUMM*1,    ! Flag: include summary    in text file?
     :  FCOL*1,     !  "  :    "    columns    "   "    "  ?
     :  FPAR*1,     !  "  :    "    parameters "   "    "  ?
     :  FTXT*1,     !  "  :    "    text       "   "    "  ?
     :  FTABL*1,    !  "  :    "    data table "   "    "  ?
     :  ANGRPN*75   ! Flag controlling representation of columns of angles.
      INTEGER
     :  CI,      ! Catalogue identifier.
     :  ROWS,    ! Number of rows in the catalogue.
     :  SELNO,   ! Number of required selection.
     :  FIRSTR,  ! First row to be listed in current selection.
     :  LASTR,   ! Last   "  "  "    "    "     "        "    .
     :  SWID,    ! Screen width  in characters.
     :  SHT,     !   "    height "      "     .
     :  ROWNO,   ! Number of the current row in the current selection.
     :  NLIST    ! Number of rows to be listed by LIST.
      INTEGER
     :  FPGSZE,  ! Page size for text file (number of lines).
     :  FWID,    ! Width of lines in the text file (characters).
     :  DECPL    ! Number of decimal places for displaying statistics.
      LOGICAL
     :  QUIET,   ! Flag; operate in quiet or verbose (normal) mode?
     :  MORE,    ! Flag; process more actions?
     :  OPEN,    ! Flag; is a catalogue currently open?
     :  SEQNO,   ! Flag; preceed rows listed with sequence number?
     :  ANGRF,   ! Flag; reformat UNITS attribute for angles?
     :  FPRINT,  ! Flag; is output file a print or data file?
     :  GUI,     ! Flag; is the application being driven from a GUI?
     :  CFLAG,   ! Flag; copy all or chosen columns to new catalogue?
     :  TFLAG,   ! Flag; copy header text to new catalogue?
     :  BINSP,   ! Flag; how are histogram details specified?
     :  NORML    ! Flag; is the histogram to be normalised?
      REAL
     :  BINDET   ! Histogram bin details (width or no. of bins).
      CHARACTER
     :  GRPHDV*75,   ! Graphics device.
     :  TITLE*50,    ! Title for scatter-plot.
     :  XEXPR*(CAT__SZEXP),   ! Expression for X axis of scatter-plot.
     :  YEXPR*(CAT__SZEXP),   !     "       "  Y  "   "      "      .
     :  PLTSYM*15,   ! Scatter-plot plotting symbol.
     :  COLOUR*15    !     "        symbol colour.
      LOGICAL
     :  AUTOSCL  ! Flag; auto-scale the scatter-plot?
      CHARACTER
     :  CXMIN*40,    ! Minimum of the X axis of the scatter-plot.
     :  CXMAX*40,    ! Maximum "   "  "  "   "   "       "      .
     :  CYMIN*40,    ! Minimum "   "  Y  "   "   "       "      .
     :  CYMAX*40     ! Maximum "   "  "  "   "   "       "      .
*  Local Data:
      DATA OPEN /.FALSE./
      SAVE OPEN
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Obtain and set the quiet mode.

         CALL PAR_GET0L ('QUIET', QUIET, STATUS)

         IF (QUIET) THEN
            CALL CAT_TUNES ('QUIET', 'YES', STATUS)
         ELSE
            CALL CAT_TUNES ('QUIET', 'NO', STATUS)
         END IF

*
*       Determine whether the application is being driven from a GUI
*       and if so then get the action name.  Also set the maximum
*       line length from the message system appropriately.

         CALL PAR_GET0L ('GUI', GUI, STATUS)
         CALL PAR_CANCL ('GUI', STATUS)

         IF (GUI) THEN
             CALL TASK_GET_NAME (ACTION, STATUS)
             CALL MSG_TUNE ('SZOUT', 0, STATUS)
          ELSE
             CALL MSG_TUNE ('SZOUT', 79, STATUS)
         END IF

C        print3999, action
C3999    format(1x, '****** action: ', a)

*
*       If not being driven from a GUI then process actions while the user
*       wishes to continue and the status is ok.  Otherwise (ie. being
*       driven from a GUI) process a single Action.

         MORE = .TRUE.
         OPEN = .FALSE.

         DO WHILE (MORE)

*
*          If the application is not being driven from a GUI then get
*          the action name.  Convert the action name into upper case
*          irrespective of whether it is being driven from a GUI or not.

            IF (.NOT. GUI) THEN
               CALL PAR_GET0C ('ACTION', ACTION, STATUS)
               CALL PAR_CANCL ('ACTION', STATUS)
            END IF

            CALL CHR_UCASE (ACTION)

C           print555, action
C 555       format(1x, 'action :', a10)

*
*          Process the various actions.

            IF (ACTION .EQ. 'OPEN') THEN

*
*             Open a catalogue.

               CALL PAR_GET0C ('CNAME', CNAME, STATUS)
               CALL PAR_CANCL ('CNAME', STATUS)

*
*             If there is no catalogue open then attempt to initialise
*             the catview common block, attempt to open and then initialise
*             the new catalogue, otherwise report a warning.

               IF (.NOT. OPEN) THEN
                  CALL CAP_GINIT (STATUS)

                  CALL CAT_TOPEN (CNAME, 'OLD', 'READ', CI, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL CAP_GCINI (CNAME, CI, STATUS)

                     CALL CAT_TROWS (CI, ROWS, STATUS)

                     CALL MSG_SETI ('ROWS', ROWS)
                     CALL CAP_INFO (GUI, ' ', 'The catalogue '/
     :                 /'contains ^ROWS rows.', STATUS)

                     OPEN = .TRUE.

                  END IF

               ELSE
                  CALL CAP_WARN (GUI, ' ', 'There is already an '/
     :              /'open catalogue; new catalogue not opened.',
     :              STATUS)

               END IF

            ELSE IF (ACTION .EQ. 'SHOWCOL') THEN

*
*             Display a list of the columns in the catalogue.

               CALL CAP_GSCOL (STATUS)

            ELSE IF (ACTION .EQ. 'DETCOL') THEN

*
*             Display full details for all the columns in the
*             catalogue.

               CALL CAP_GDCOL (STATUS)

            ELSE IF (ACTION .EQ. 'SHOWPAR') THEN

*
*             Display a list of the parameters in the catalogue.

               CALL CAP_GSPAR (STATUS)

            ELSE IF (ACTION .EQ. 'DETPAR') THEN

*
*             Display full details for all the parameters in the
*             catalogue.

               CALL CAP_GDPAR (STATUS)

            ELSE IF (ACTION .EQ. 'SHOWTXT') THEN

*
*             Display the header text information for the catalogue.

               CALL CAP_GSTXT (STATUS)

            ELSE IF (ACTION .EQ. 'SHOWROWS') THEN

*
*             Display the number of rows in the current selection.

               CALL CAP_GROWS (STATUS)

            ELSE IF (ACTION .EQ. 'SETCMP') THEN

*
*             Enter the list of components (columns, expressions and
*             parameters) which are to be displayed.  Note that
*             CAP_GTSTR, which allows continuation lines to be input,
*             is only called if the application is being run without
*             a GUI.

               IF (GUI) THEN
C                 print4000, 'before PAR_GET0C'
C                 print4000, 'before PAR_GET0C'
C4000             format(1x, a)
                  CALL PAR_GET0C ('CMPLST', CMPLST, STATUS)
C                 print4000, 'before PAR_CANCL'
                  CALL PAR_CANCL ('CMPLST', STATUS)
C                 print4000, 'after PAR_CANCL'
               ELSE
                  CALL CAP_GTSTR ('CMPLST', CMPLST, STATUS)
               END IF

               CALL CAP_GSCMP (CMPLST, 'LIST', STATUS)

            ELSE IF (ACTION .EQ. 'SHOWSEL') THEN

*
*             Show details of all the selections which currently
*             exist.

               CALL CAP_GSSEL (STATUS)

            ELSE IF (ACTION .EQ. 'CHOSEL') THEN

*
*             Choose an existing selection to become the current
*             selection.

               CALL PAR_GET0I ('SELNO', SELNO, STATUS)
               CALL PAR_CANCL ('SELNO', STATUS)

               CALL CAP_GCSEL (SELNO, STATUS)

            ELSE IF (ACTION .EQ. 'SETSEL') THEN

*
*             Create a new selection.  Note that CAP_GTSTR, which
*             allows continuation lines to be input, is only called
*             if the application is being run without a GUI.

               IF (GUI) THEN
                  CALL PAR_GET0C ('EXPR', EXPR, STATUS)
                  CALL PAR_CANCL ('EXPR', STATUS)
               ELSE
                  CALL CAP_GTSTR ('EXPR', EXPR, STATUS)
               END IF

               CALL CAP_GCEXP (EXPR, STATUS)

            ELSE IF (ACTION .EQ. 'SHOWRNG') THEN

*
*             List the columns available for a range selection.
C             Currently only the sorted columns are available;
C             eventually indexed columns will also be added.

               CALL CAP_GSRNG (STATUS)

            ELSE IF (ACTION .EQ. 'SETRNG') THEN

*
*             Create a new range selection.

               CALL PAR_GET0C ('PNAME', PNAME, STATUS)
               CALL PAR_CANCL ('PNAME', STATUS)

               CALL PAR_GET0C ('MINRNG', MINRNG, STATUS)
               CALL PAR_CANCL ('MINRNG', STATUS)

               CALL PAR_GET0C ('MAXRNG', MAXRNG, STATUS)
               CALL PAR_CANCL ('MAXRNG', STATUS)

*
*             If cavtview is being driven from a GUI then a check is
*             made to see if the first characters of the given ranges
*             are colons.  This check is necessary because ranges
*             specified in the GUI are preceded by a colon in order
*             to avoid negative numbers being confused for tcl flags
*             by procedure exp_send (!!!).

               IF (GUI) THEN
                  CALL CHR_LDBLK (MINRNG)

                  IF (MINRNG(1 : 1) .EQ. ':') THEN
                     MINRNG(1 : 1) = ' '
                     CALL CHR_LDBLK (MINRNG)
                  END IF

                  CALL CHR_LDBLK (MAXRNG)

                  IF (MAXRNG(1 : 1) .EQ. ':') THEN
                     MAXRNG(1 : 1) = ' '
                     CALL CHR_LDBLK (MAXRNG)
                  END IF
               END IF

               CALL CAP_GCRNG (PNAME, MINRNG, MAXRNG, STATUS)

            ELSE IF (ACTION .EQ. 'SETROW') THEN

*
*             Set the current row number.

               CALL PAR_GET0I ('ROWNO', ROWNO, STATUS)
               CALL PAR_CANCL ('ROWNO', STATUS)

               CALL CAP_GSTRW (ROWNO, STATUS)

            ELSE IF (ACTION .EQ. 'LIST') THEN

*
*             List the current selection to the screen.

               CALL CAP_GLIST (STATUS)

            ELSE IF (ACTION .EQ. 'PREV') THEN

*
*             List the previous page in the current selection.

               CALL CAP_GPREV (STATUS)

            ELSE IF (ACTION .EQ. 'SETSTAT') THEN

*
*             Enter the list of columns for which statistics are
*             to be computed.  Note that CAP_GTSTR, which allows
*             continuation lines to be input, is only called if the
*             application is being run without a GUI.

               IF (GUI) THEN
                  CALL PAR_GET0C ('CMPSTT', CMPSTT, STATUS)
                  CALL PAR_CANCL ('CMPSTT', STATUS)
               ELSE
                  CALL CAP_GTSTR ('CMPSTT', CMPSTT, STATUS)
               END IF

               CALL CAP_GSCMP (CMPSTT, 'STATS', STATUS)

            ELSE IF (ACTION .EQ. 'SETDECPL') THEN

*
*             Enter the number of decimal places to which statistics
*             are to be computed.

               CALL PAR_GET0I ('DECPL', DECPL, STATUS)
               CALL PAR_CANCL ('DECPL', STATUS)

               CALL CAP_GDCPL (DECPL, STATUS)

            ELSE IF (ACTION .EQ. 'STATS') THEN

*
*             Compute statistics on the specified columns.

               CALL PAR_GET0C ('SFNAME', SFNAME, STATUS)
               CALL PAR_CANCL ('SFNAME', STATUS)

               IF (SFNAME .EQ. 'none') THEN
                  SFNAME = 'NONE'
               END IF

               CALL CAP_GSTAT (SFNAME, STATUS)

            ELSE IF (ACTION .EQ. 'SCOPEN') THEN

*
*             Open a new scatter-plot.

               CALL PAR_GET0C ('GRPHDV', GRPHDV, STATUS)
               CALL PAR_CANCL ('GRPHDV', STATUS)

               CALL PAR_GET0C ('TITLE', TITLE, STATUS)
               CALL PAR_CANCL ('TITLE', STATUS)

               IF (TITLE .EQ. 'NONE'  .OR.  TITLE .EQ. 'none') THEN
                  TITLE = ' '
               END IF

               CALL PAR_GET0C ('XEXPR', XEXPR, STATUS)
               CALL PAR_CANCL ('XEXPR', STATUS)

               CALL PAR_GET0C ('YEXPR', YEXPR, STATUS)
               CALL PAR_CANCL ('YEXPR', STATUS)

               CALL CAP_GSCOP (GRPHDV, TITLE, XEXPR, YEXPR, STATUS)

            ELSE IF (ACTION .EQ. 'SCRANGE') THEN

*
*             Set the axis ranges for a scatter-plot.

               CALL PAR_GET0L ('AUTOSCL', AUTOSCL, STATUS)
               CALL PAR_CANCL ('AUTOSCL', STATUS)

               CALL PAR_GET0C ('CXMIN', CXMIN, STATUS)
               CALL PAR_CANCL ('CXMIN', STATUS)

               CALL PAR_GET0C ('CXMAX', CXMAX, STATUS)
               CALL PAR_CANCL ('CXMAX', STATUS)

               CALL PAR_GET0C ('CYMIN', CYMIN, STATUS)
               CALL PAR_CANCL ('CYMIN', STATUS)

               CALL PAR_GET0C ('CYMAX', CYMAX, STATUS)
               CALL PAR_CANCL ('CYMAX', STATUS)

               CALL CAP_GSCRN (AUTOSCL, CXMIN, CXMAX, CYMIN, CYMAX,
     :           STATUS)

            ELSE IF (ACTION .EQ. 'SCPLOT') THEN

*
*             Plot the current selection in the current scatter-plot.

               CALL PAR_GET0C ('PLTSYM', PLTSYM, STATUS)
               CALL PAR_CANCL ('PLTSYM', STATUS)
               CALL CHR_UCASE (PLTSYM)

               CALL PAR_GET0C ('COLOUR', COLOUR, STATUS)
               CALL PAR_CANCL ('COLOUR', STATUS)
               CALL CHR_UCASE (COLOUR)

               CALL CAP_GSCPL (PLTSYM, COLOUR, STATUS)

            ELSE IF (ACTION .EQ. 'SCSHRNG') THEN

*
*             Show the range of the current plot.

               CALL CAP_GSCSH (STATUS)

            ELSE IF (ACTION .EQ. 'SCLOSE') THEN

*
*             Close the current scatter-plot.

               CALL CAP_GPCLS (STATUS)

            ELSE IF (ACTION .EQ. 'HSOPEN') THEN

*
*             Open a new histogram.

               CALL PAR_GET0C ('GRPHDV', GRPHDV, STATUS)
               CALL PAR_CANCL ('GRPHDV', STATUS)

               CALL PAR_GET0C ('TITLE', TITLE, STATUS)
               CALL PAR_CANCL ('TITLE', STATUS)

               IF (TITLE .EQ. 'NONE'  .OR.  TITLE .EQ. 'none') THEN
                  TITLE = ' '
               END IF

               CALL PAR_GET0C ('XEXPR', XEXPR, STATUS)
               CALL PAR_CANCL ('XEXPR', STATUS)

               CALL CAP_GHSOP (GRPHDV, TITLE, XEXPR, STATUS)

            ELSE IF (ACTION .EQ. 'HSRANGE') THEN

*
*             Set the axis ranges for a histogram.

               CALL PAR_GET0L ('AUTOSCL', AUTOSCL, STATUS)
               CALL PAR_CANCL ('AUTOSCL', STATUS)

               CALL PAR_GET0C ('CXMIN', CXMIN, STATUS)
               CALL PAR_CANCL ('CXMIN', STATUS)

               CALL PAR_GET0C ('CXMAX', CXMAX, STATUS)
               CALL PAR_CANCL ('CXMAX', STATUS)

               CALL PAR_GET0L ('BINSP', BINSP, STATUS)
               CALL PAR_CANCL ('BINSP', STATUS)

               CALL PAR_GET0R ('BINDET', BINDET, STATUS)
               CALL PAR_CANCL ('BINDET', STATUS)

               CALL PAR_GET0L ('NORML', NORML, STATUS)
               CALL PAR_CANCL ('NORML', STATUS)

               CALL CAP_GHSRN (AUTOSCL, CXMIN, CXMAX, BINSP, BINDET,
     :           NORML, STATUS)

            ELSE IF (ACTION .EQ. 'HSPLOT') THEN

*
*             Plot the current selection in the current histogram.

               CALL PAR_GET0C ('COLOUR', COLOUR, STATUS)
               CALL PAR_CANCL ('COLOUR', STATUS)
               CALL CHR_UCASE (COLOUR)

               CALL CAP_GHSPL (COLOUR, STATUS)

            ELSE IF (ACTION .EQ. 'HSSHRNG') THEN

*
*             Show the range of the current histogram.

               CALL CAP_GHSSH (STATUS)

            ELSE IF (ACTION .EQ. 'HSCLOSE') THEN

*
*             Close the current histogram.

               CALL CAP_GPCLS (STATUS)

            ELSE IF (ACTION .EQ. 'FILE') THEN

*
*             List the current selection to a text file.

               CALL PAR_GET0I ('FIRSTR', FIRSTR, STATUS)
               CALL PAR_CANCL ('FIRSTR', STATUS)

               CALL PAR_GET0I ('LASTR', LASTR, STATUS)
               CALL PAR_CANCL ('LASTR', STATUS)

               CALL PAR_GET0C ('FLNAME', FLNAME, STATUS)
               CALL PAR_CANCL ('FLNAME', STATUS)

               CALL CAP_GFILE (FIRSTR, LASTR, FLNAME, STATUS)

            ELSE IF (ACTION .EQ. 'SAVECAT') THEN

*
*             Save the current selection as a catalogue.

               CALL PAR_GET0C ('CATOUT', CATOUT, STATUS)
               CALL PAR_CANCL ('CATOUT', STATUS)

               CALL PAR_GET0L ('CFLAG', CFLAG, STATUS)
               CALL PAR_CANCL ('CFLAG', STATUS)

               CALL PAR_GET0L ('TFLAG', TFLAG, STATUS)
               CALL PAR_CANCL ('TFLAG', STATUS)

               CALL PAR_GET0C ('COMM', COMM, STATUS)
               CALL PAR_CANCL ('COMM', STATUS)

               IF (COMM .EQ. '<none>') THEN
                  COMM = " "
               END IF

               CALL CAP_GSCAT (CATOUT, CFLAG, TFLAG, COMM, STATUS)

            ELSE IF (ACTION .EQ. 'SHOWFMT') THEN

*
*             Show the data type, units and external format for a
*             column.

               CALL PAR_GET0C ('PNAME', PNAME, STATUS)
               CALL PAR_CANCL ('PNAME', STATUS)

               CALL CAP_GCDSP (PNAME, STATUS)

            ELSE IF (ACTION .EQ. 'SETFMT') THEN

*
*             Set a new external display format for a column or
*             parameter.

               CALL PAR_GET0C ('PNAME', PNAME, STATUS)
               CALL PAR_CANCL ('PNAME', STATUS)

               CALL PAR_GET0C ('UNITS', UNITS, STATUS)
               CALL PAR_CANCL ('UNITS', STATUS)

               CALL PAR_GET0C ('EXFMT', EXFMT, STATUS)
               CALL PAR_CANCL ('EXFMT', STATUS)

               IF (UNITS .EQ. '<none>'  .OR.  UNITS .EQ. '{}')
     :           THEN
                  UNITS = " "
               END IF

               IF (EXFMT .EQ. '<none>') THEN
                  EXFMT = " "
               END IF

               CALL CAP_RPFMT (PNAME, UNITS, EXFMT, STATUS)

            ELSE IF (ACTION .EQ.'SETCONF') THEN

*
*             Set the screen configuration.

               CALL PAR_GET0I ('SWID', SWID, STATUS)
               CALL PAR_CANCL ('SWID', STATUS)

               CALL PAR_GET0I ('SHT', SHT, STATUS)
               CALL PAR_CANCL ('SHT', STATUS)

               CALL PAR_GET0L ('SEQNO', SEQNO, STATUS)
               CALL PAR_CANCL ('SEQNO', STATUS)

               CALL PAR_GET0I ('NLIST', NLIST, STATUS)
               CALL PAR_CANCL ('NLIST', STATUS)

               CALL PAR_GET0C ('ANGRPN', ANGRPN, STATUS)
               CALL PAR_CANCL ('ANGRPN', STATUS)

               CALL PAR_GET0L ('ANGRF', ANGRF, STATUS)
               CALL PAR_CANCL ('ANGRF', STATUS)

               CALL CAP_GSCNF (SWID, SHT, SEQNO, NLIST, ANGRPN,
     :           ANGRF, GUI, STATUS)

*
*             Set the maximum permitted line from the message system to
*             match the screen width.

               CALL MSG_TUNE ('SZOUT', SWID, STATUS)

            ELSE IF (ACTION .EQ. 'SETFILE') THEN

*
*             Set the configuration options for the output test file.

               CALL PAR_GET0L ('FPRINT', FPRINT, STATUS)
               CALL PAR_CANCL ('FPRINT', STATUS)

               CALL PAR_GET0I ('FPGSZE', FPGSZE, STATUS)
               CALL PAR_CANCL ('FPGSZE', STATUS)

               CALL PAR_GET0I ('FWID', FWID, STATUS)
               CALL PAR_CANCL ('FWID', STATUS)

               CALL PAR_GET0C ('FSUMM', FSUMM, STATUS)
               CALL PAR_CANCL ('FSUMM', STATUS)

               CALL PAR_GET0C ('FCOL', FCOL, STATUS)
               CALL PAR_CANCL ('FCOL', STATUS)

               CALL PAR_GET0C ('FPAR', FPAR, STATUS)
               CALL PAR_CANCL ('FPAR', STATUS)

               CALL PAR_GET0C ('FTXT', FTXT, STATUS)
               CALL PAR_CANCL ('FTXT', STATUS)

               CALL PAR_GET0C ('FTABL', FTABL, STATUS)
               CALL PAR_CANCL ('FTABL', STATUS)

               CALL CHR_UCASE (FSUMM)
               CALL CHR_UCASE (FCOL)
               CALL CHR_UCASE (FPAR)
               CALL CHR_UCASE (FTXT)
               CALL CHR_UCASE (FTABL)

               CALL CAP_GSFIL (FPRINT, FPGSZE, FWID, FSUMM, FCOL,
     :           FPAR, FTXT, FTABL, STATUS)

            ELSE IF (ACTION .EQ. 'COLNAME') THEN

*
*             Display the names of all the columns in the catalogue.

               CALL CAP_GLCOL (STATUS)

            ELSE IF (ACTION .EQ. 'HELP') THEN

*
*             List the various actions.

               CALL CAP_GHELP (STATUS)

            ELSE IF (ACTION .EQ. 'EXIT'  .OR.  ACTION .EQ. 'STOP'
     :        .OR.  ACTION .EQ. 'QUIT') THEN

*
*             Set the termination flag and tidy up: close any plot
*             attempt to close any open catalogue.

               MORE = .FALSE.

               CALL CAP_GPCLS (STATUS)

               IF (OPEN) THEN
                  CALL CAP_GCLSE (STATUS)
               END IF

               CALL CAP_INFO (GUI, ' ', 'Catalogue closed.', STATUS)

            ELSE

*
*             The action is unknown: report a warning (not an error).

               CALL MSG_SETC ('ACTION', ACTION)
               CALL CAP_WARN (GUI, ' ', 'Unknown action ignored: '/
     :           /'^ACTION', STATUS)
            END IF

*
*          If any error status has been raised then report the
*          error and annul the status.  Note that the termination
*          flag is NOT set.

            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP ('CATVIEW_REP', 'Catview error.',
     :           STATUS)
               CALL ERR_FLUSH (STATUS)
               CALL ERR_ANNUL (STATUS)
            END IF

*
*          If the application is being driven from a GUI then set the
*          termination flag; it does not loop when being driven from a
*          GUI.

            IF (GUI) THEN
               MORE = .FALSE.
            END IF

         END DO

*
*      If any error has occurred then report it.

        IF (STATUS .NE. SAI__OK) THEN
           CALL ERR_REP ('CATVIEW_ERR', 'Catview terminating with '/
     :       /'an error.', STATUS)
        END IF

      END IF

      END
