      SUBROUTINE CAP_GHELP (STATUS)
*+
*  Name:
*     CAP_GHELP
*  Purpose:
*     List the actions available.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GHELP (STATUS)
*  Description:
*     List the actions available.  Note that this routine makes no
*     check that there is a catalogue open.
*  Arguments:
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     List the various actions.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/4/94  (ACD): Original version.
*     13/4/95  (ACD): First stable version.
*     29/11/96 (ACD): Added entries for commands to compute the
*        statistics on a set of columns.
*     26/11/98 (ACD): Added entries for scatter-plot commands.
*     29/11/99 (ACD): Added entries for histogram commands.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants.
*  Status:
      INTEGER STATUS             ! Global status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       List the various actions.

         CALL MSG_OUT (' ', 'OPEN    - Open a new catalogue.',
     :     STATUS)
         CALL MSG_OUT (' ', 'SHOWCOL - List the columns in the '/
     :     /'catalogue.', STATUS)
         CALL MSG_OUT (' ', 'DETCOL  - List details for the columns '/
     :     /'in the catalogue.', STATUS)
         CALL MSG_OUT (' ', 'SHOWPAR - List the parameters in the '/
     :     /'catalogue.', STATUS)
         CALL MSG_OUT (' ', 'DETPAR  - List details for the '/
     :     /'parameters in the catalogue.', STATUS)
         CALL MSG_OUT (' ', 'SHOWTXT - List header text for the '/
     :     /'catalogue.', STATUS)
         CALL MSG_OUT (' ', 'SHOWROWS - Show the number of rows in '/
     :     /'the current selection.', STATUS)
         CALL MSG_OUT (' ', 'SETCMP  - Enter the columns and '/
     :     /'expressions to be listed.', STATUS)
         CALL MSG_OUT (' ', 'SHOWSEL - List details of all the '/
     :     /'selections.', STATUS)
         CALL MSG_OUT (' ', 'CHOSEL  - Choose an existing selection '/
     :     /'to become current.', STATUS)
         CALL MSG_OUT (' ', 'SETSEL  - Create a new selection.', STATUS)
         CALL MSG_OUT (' ', 'SHOWRNG - Show the columns suitable for '/
     :     /'a range selection.', STATUS)
         CALL MSG_OUT (' ', 'SETRNG  - Create a range selection.',
     :     STATUS)
         CALL MSG_OUT (' ', 'SETROW  - Set the current row number.',
     :     STATUS)
         CALL MSG_OUT (' ', 'LIST    - List the current selection to '/
     :     /'the screen.', STATUS)
         CALL MSG_OUT (' ', 'PREV    - List the previous page of the '/
     :     /'current selection to the screen.', STATUS)
         CALL MSG_OUT (' ', 'SETSTAT - Enter the columns for which '/
     :     /'statistics required.', STATUS)
         CALL MSG_OUT (' ', 'SETDECPL - Enter the number of decimal '/
     :     /'places for displaying statistics.', STATUS)
         CALL MSG_OUT (' ', 'STATS   - Compute statistics for the '/
     :     /'specified columns.', STATUS)
         CALL MSG_OUT (' ', 'SCOPEN  - Open a new scatter-plot.',
     :     STATUS)
         CALL MSG_OUT (' ', 'SCRANGE - Set the axis ranges for a '/
     :     /'scatter-plot.', STATUS)
         CALL MSG_OUT (' ', 'SCPLOT  - Plot the current selection as '/
     :     /'a scatter-plot.', STATUS)
         CALL MSG_OUT (' ', 'SCSHRNG - Show the range of the current '/
     :     /'plot.', STATUS)
         CALL MSG_OUT (' ', 'SCLOSE  - Close the current scatter-plot.',
     :     STATUS)
         CALL MSG_OUT (' ', 'HSOPEN  - Open a new histogram.',
     :     STATUS)
         CALL MSG_OUT (' ', 'HSRANGE - Set the X axis range etc. for '/
     :     /'a histogram.', STATUS)
         CALL MSG_OUT (' ', 'HSPLOT  - Plot a histogram from the '/
     :     /'current selection.', STATUS)
         CALL MSG_OUT (' ', 'HSSHRNG - Show the X range of the '/
     :     /'current histogram.', STATUS)
         CALL MSG_OUT (' ', 'HSCLOSE - Close the current histogram.',
     :     STATUS)
         CALL MSG_OUT (' ', 'FILE    - List the current selection to '/
     :     /'a text file.', STATUS)
         CALL MSG_OUT (' ', 'SAVECAT - Save the current selection as '/
     :     /'a catalogue.', STATUS)
         CALL MSG_OUT (' ', 'SHOWFMT - Show the data type, units and '/
     :     /'display format for a column.', STATUS)
         CALL MSG_OUT (' ', 'SETFMT  - Set the external format for a '/
     :     /'column or parameter.', STATUS)
         CALL MSG_OUT (' ', 'SETCONF - Configure the output for the '/
     :     /'terminal.', STATUS)
         CALL MSG_OUT (' ', 'SETFILE - Set the text file '/
     :     /'configuration options.', STATUS)
         CALL MSG_OUT (' ', 'COLNAME - List the names of all the '/
     :     /'columns', STATUS)
         CALL MSG_OUT (' ', 'HELP    - List commands available (this '/
     :     /'list).', STATUS)
         CALL MSG_OUT (' ', 'EXIT    - Terminate catview.', STATUS)

      END IF

      END
