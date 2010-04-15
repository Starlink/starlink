      SUBROUTINE CATSELECT (STATUS)
*+
*  Name:
*     CATSELECT
*  Purpose:
*     Generate a selection from a catalogue.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATSELECT (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     Generate a selction from a catalogue using one of a number of a
*     different type of selections.  Optionally the rejected objects
*     may be written to a separate catalogue.  The following types
*     of selections are available.
*
*     Arbitrary expression: objects which satisfy an algebraic
*       expression which you supply.
*
*     Range within a sorted column: objects which lie within a given
*       range for a specified column.  This option only works on
*       sorted columns.  However, because it is not necessary to read
*       the entire column it works essentially instantaneously,
*       irrespective of the number of rows in the catalogue.
*
*     Rectangular area: objects which lie within a given rectangle.
*       (If the columns are spherical-polar coordinates, such as
*       Right Ascension and Declination, rather than Cartesian
*       coordinates then the sides of the rectangle become parallels
*       and great circles.)
*
*     Circular area: objects which lie within a given angular distance
*       from a specified point.  This type of selection is only likely
*       to be used on columns of celestial coordinates.
*
*     Polygonal area: objects which lie inside (or outside) a given
*       polygon.
*
*     Every Nth entry: every Nth object from the catalogue.  This option
*       is useful for producing a smaller, but representative, sample of
*       a large catalogue.  Such a sample might then be investigated
*       interactively in cases where the original catalogue was too
*       large to be studied interactively.
*  Usage:
*     catselect
*  ADAM Parameters:
*     CATIN  = CHARACTER (read)
*        Give the name of the input catalogue.
*     CATOUT  =  CHARACTER (read)
*        Give the name of the output catalogue of selected objects.
*     CATREJ  =  CHARACTER (read)
*        Give the name of the output catalogue of rejected objects.
*     SELTYP  =  CHARACTER (read)
*        Enter the required type of selection ("H" for a list).
*     TRNFRM  =  LOGICAL (read)
*        Transform criteria to catalogue system before selection?
*     TARGET  =  LOGICAL (read)
*        Output the selection as a target list?
*     REJCAT  =  LOGICAL (read)
*        Produce a second output catalogue containing the rejected objects?
*     EXPR  =  CHARACTER (read)
*        Enter an expression defining the required selection.
*     PNAME  =  CHARACTER (read)
*        Enter the name of column or parameter.
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
*        should be separated by a colon (':').  Optionally fractional
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
*        should be separated by a colon (':').  Optionally fractional
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
*     FREQ  =  INTEGER (read)
*        Every FREQth object will be selected.
*     XCOL  =  CHARACTER (read)
*        Enter X coordinate column from input catalogue.
*     YCOL  =  CHARACTER (read)
*        Enter Y coordinate column from input catalogue.
*     CATPOLY  =  CHARACTER (read)
*        Give the name of the catalogue containing the polygon.
*     XPLCOL  =  CHARACTER (read)
*        Enter X coordinate column from polygon catalogue.
*     YPLCOL  =  CHARACTER (read)
*        Enter Y coordinate column from polygon catalogue.
*     INSIDE  =  LOGICAL (read)
*        The objects either inside or outside the polygon may be selected.
*     XMIN  =  DOUBLE (read)
*        Minimum X value for the required rectangle.
*
*        If the X column within which the minimum is being specified is
*        not an angle then simply enter the required value.
*
*        If the column is an angle then the value can be entered as
*        either a decimal value in radians or a sexagesimal value in
*        hours or degrees, minutes and seconds.  If a sexagesimal value
*        is specified then the hours or degrees, minutes and seconds
*        should be separated by a colon (':').  Optionally fractional
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
*     XMAX  =  DOUBLE (read)
*        Maximum X value for the required rectangle.
*
*        If the X column within which the maximum is being specified is
*        not an angle then simply enter the required value.
*
*        If the column is an angle then the value can be entered as
*        either a decimal value in radians or a sexagesimal value in
*        hours or degrees, minutes and seconds.  If a sexagesimal value
*        is specified then the hours or degrees, minutes and seconds
*        should be separated by a colon (':').  Optionally fractional
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
*     YMIN  =  DOUBLE (read)
*        Minimum Y value for the required rectangle.
*
*        If the Y column within which the minimum is being specified is
*        not an angle then simply enter the required value.
*
*        If the column is an angle then the value can be entered as
*        either a decimal value in radians or a sexagesimal value in
*        hours or degrees, minutes and seconds.  If a sexagesimal value
*        is specified then the hours or degrees, minutes and seconds
*        should be separated by a colon (':').  Optionally fractional
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
*     YMAX  =  DOUBLE (read)
*        Maximum Y value for the required rectangle.
*
*        If the Y column within which the minimum is being specified is
*        not an angle then simply enter the required value.
*
*        If the column is an angle then the value can be entered as
*        either a decimal value in radians or a sexagesimal value in
*        hours or degrees, minutes and seconds.  If a sexagesimal value
*        is specified then the hours or degrees, minutes and seconds
*        should be separated by a colon (':').  Optionally fractional
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
*     RACOL  =  CHARACTER (read)
*        Enter Right Ascension column from input catalogue.
*     DCCOL  =  CHARACTER (read)
*        Enter Declination column from input catalogue.
*     RACEN  =  CHARACTER (read)
*        The central Right Ascension.
*
*        The value may be specified as either a sexagesimal value in
*        hours or a decimal value in radians.  If the value is supplied
*        as sexagesimal hours then the hours, minutes and seconds must
*        be separated by a colon (':').  Optionally fractional
*        seconds can be specified by including a decimal point and the
*        required number of places of decimals.  A negative angle may
*        be specified by preceding the value by a minus sign.
*
*        Examples: an angle of 10 hours, 30 minutes and 15.3 seconds
*        may be specified by entering either of the following two
*        values:
*
*            10:30:15.3     sexagesimal hours
*            2.7500062      radians
*     DCCEN  =  CHARACTER (read)
*        The central Declination.
*
*        The value may be specified as either a sexagesimal value in
*        degrees or a decimal value in radians.  If the value is supplied
*        as sexagesimal degrees then the degrees, minutes and seconds must
*        be separated by a colon (':').  Optionally fractional
*        seconds can be specified by including a decimal point and the
*        required number of places of decimals.  A negative angle may
*        be specified by preceding the value by a minus sign.
*
*        Examples: a negative angle of 33 degrees, 30 minutes and 15.2
*        seconds may be specified by entering either of the following two
*        values:
*
*           -33:30:15.2     sexagesimal degrees
*           -0.584759       radians
*     RADIUS  =  CHARACTER (read)
*        The selection radius.
*
*        The value may be specified as either a sexagesimal value in
*        minutes and seconds of arc or a decimal value in radians.  If
*        a sexagesimal value is supplied then the minutes and seconds
*        of arc must be separated by a colon (':').  Note that a colon
*        must be present if the value is to interpretted as minutes of
*        arc; if no colon is present it will be interpretted as radians.
*        Optionally fractional seconds can be specified by including a
*        decimal point and the required number of places of decimals.  A
*        negative angle may be specified by preceding the value by a
*        minus sign.
*
*        Examples: a radius of two minutes of arc may be specified by
*        entering either of the following two values:
*
*            2:0            sexagesimal minutes of arc
*            5.8178E-4      radians
*     EQUINX  =  CHARACTER (read)
*        The equinox of the catalogue coordinates.
*
*        The equinox is specified as a time system followed by the
*        value in that system in years.  A single alphabetical character
*        is used to identify each of the two time systems supported:
*        B for Bessellian and J for Julian.  Optionally decimal
*        fractions of a year may be specified by including a decimal
*        point followed by the required fraction.
*
*        Examples:
*            J2000          Julian equinox 2000.
*            B1950          Bessellian equinox 1950.
*     EPOCH  =  CHARACTER (read)
*        The epoch of the catalogue coordinates.
*
*        The epoch is specified as a time system followed by the
*        value in that system in years.  A single alphabetical character
*        is used to identify each of the two time systems supported:
*        B for Bessellian and J for Julian.  Optionally decimal
*        fractions of a year may be specified by including a decimal
*        point followed by the required fraction.
*
*        Examples:
*            J1996.894      Julian epoch of 1996.894.
*            B1955.439      Bessellian epoch of 1955.439.
*     RAC  =  CHARACTER (read)
*        Enter the name of the Right Ascension column.
*     DECC  =  CHARACTER (read)
*        Enter the name of the Declination column.
*     PMRAC  =  CHARACTER (read)
*        Name of the proper motion in Right Ascension column (radians/year).
*     PMDEC  =  CHARACTER (read)
*        Name of the proper motion in Declination column (radians/year).
*     PLXC  =  CHARACTER (read)
*        Name of the parallax column (radians).
*     RVC  =  CHARACTER (read)
*        Name of the radial velocity column (Km/sec).
*
*        A positive value corresponds to an object which is red-shifted
*        or receding and a negative value to one which is blue-shifted
*        or approaching.
*     LABELC  =  CHARACTER (read)
*        Name of the column used to label objects on plots.
*     NOROWS  =  LOGICAL (read)
*        Flag indicating whether a selection which contains no rows is
*        to be considered an error or not.  Coded as follows:
*        .TRUE.  -  consider an error,
*        .FALSE. -  do not consider an error (default).
*        If NOROWS is set to .TRUE. then a selection with no rows will
*        raise the status SAI__WARN.
*     TEXT  =  CHARACTER (read)
*        Flag indicating the textual header information to be copied to
*        the output catalogues.  The valid responses are:
*        A - all; the output catalogue will contain a complete copy
*            of the header information for the input catalogue,
*            duplicated as comments,
*        C - (default) copy only the comments from the input catalogue.
*            In the case of a FITS table the COMMENTS and HISTORY
*            keywords will be copied.
*        N - none; no textual header information is copied.
*     QUIET  =  LOGICAL (read)
*        Operate in quiet mode where warnings are suppressed.  The
*        permitted values are:
*        TRUE  - quiet mode,
*        FALSE - verbose mode.
*     NUMSEL  =  INTEGER (write)
*        Number of rows selected.
*  Examples:
*     catselect
*        The input and output catalogues and the type of selection
*        required will be promted for.  Additional prompts specifiy
*        the details of the selection.  Any comments in the input
*        catalogue will be copied.
*     catselect  text=all
*        The input and output catalogues and the type of selection
*        required will be promted for.  Additional prompts specifiy
*        the details of the selection.  All the header information in
*        the input catalogue will be duplicated as comments in the
*        output catalogue.
*     catselect  text=none
*        The input and output catalogues and the type of selection
*        required will be promted for.  Additional prompts specifiy
*        the details of the selection.  Any comments in the input
*        catalogue will not be copied.
*  Algorithm:
*     Obtain and set the quiet mode.
*     Attempt to open the input catalogue.
*     If ok then
*       Determine whether a catalogue of the rejected objects is required.
*       Determine the type of selection required.
*       If the selection is a sky region then
*         Determine whether a transformation is required.
*       end if
*       Determine if the output catalogue is to be formatted as a
*       target list.
*       Determine what header text is to be copied.
*       If all is ok then
*         If an arbitrary selection is required then
*           Enter the expression.
*           Create the selection.
*         else if a range selection is required then
*           Enter the column for the selection.
*           Enter the range.
*           Create the selection.
*         else if a rectangular selection is required then
*           Enter the columns and extrema required.
*           Enter the equinox and epoch.
*           Create the selection.
*         else if a circular selection is required then
*           Enter the columns required.
*           Enter the centre.
*           Enter the equinox and epoch.
*           Enter the radius.
*         else if a polygonal selection is required then
*           Determine the name of the X column for the selection.
*           Determine the name of the Y column for the selection.
*           Attempt to open the polygon catalogue
*           If ok then
*             Determine the name of the polygon X column.
*             Determine the name of the polygon Y column.
*             Determine whether points inside or outside the polygon
*             are to be selected.
*             Generate the selection.
*             Close the polygon catalogue.
*           else
*             Report an error.
*           end if
*         else if an n-sample selection is required then
*           Enter N.
*           Create the selection.
*         else if a littlebig selection is required then
*           Enter the column required.
*           Enter the number of obejects to be selected.
*           Determine whether largest or smallest to be selected.
*         else
*           Report error: unknown type of selection.
*           Set the status.
*         end if
*         Output the number of rows selected as a parameter.
*         If ok and some objects were selected then
*           Report the number of objects selected.
*           Attempt to open the output catalogue.
*           If ok then
*             If a target list is not required then
*               Copy the selection to the output catalogue.
*             else
*               If the input catalogue does not have an equinox and
*               epoch then obtain them from the user.
*               Get the names of the special columns for the target
*               list.
*               Copy the selection to the output catalogue as a target
*               list.
*             end if
*             If a catalogue of rejected objects is required then
*               If there are some rejected objects then
*                 Attempt to open the catalogue of rejected objects.
*                 If ok then
*                   If a target list is not required then
*                     Copy the rejected objects to the rejects catalogue.
*                   else
*                     Copy the rejected objects to the rejects catalogue as
*                     a target list.
*                   end if
*                   Close the rejects catalogue.
*                 else
*                   Report error: failed to open rejects catalogue.
*                 end if
*               else
*                 Report a warning; no rows were rejected.
*               end if
*             end if
*             Close the output catalogue.
*           else
*             Report error: failed to open output catalogue.
*           end if
*         else
*           If the status is ok then
*             Report a warning; no rows were selected.
*           end if
*         end if
*       end if
*       Close the input catalogue.
*     else
*       Report error: failed to open input catalogue.
*     end if
*     If the status is ok then
*       If no rows were selected then
*         Determine whether no rows being selected is to raise a status.
*         If so then
*           Set the status to warning.
*         end if
*       end if
*     end if
*     Report any error.
*  Authors:
*     ACD:  A C Davenhall (Edinburgh)
*  History:
*     11/6/96  (ACD): Original version.
*     22/11/96 (ACD): First stable version.
*     3/6/97   (ACD): Added check for bad status when looping for
*        parameter TEXT.
*     17/9/99  (ACD): Added output of the number of rows selected and
*        optional; raising of a status if no rows were selected.  Also
*        corrected bug whereby the output catalogue was not closed in
*        all circumstances.
*     17/3/00  (ACD): Fixed bug in writing the rejects catalogue if no
*        rows were rejected.
*     26/7/00  (ACD): Corrected the documented access mode for NUMSEL
*        in the prologue and removed the `Bugs' section from the prologue.
*     5/4/01  (ACD): Added the quiet mode.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'CAT_PAR'          ! CAT parametric constants.
      INCLUDE 'CAT_ERR'          ! CAT error codes.
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  CIIN,       ! Identifier for the input  catalogue.
     :  CIOUT,      !     "       "   "  output     "    .
     :  CIREJ,      !     "       "   "  rejects    "     (optional).
     :  EI,         ! Expression identifier.
     :  SI,         ! Identifier to selected objects.
     :  SIR,        ! Identifier to rejected objects.
     :  NUMSEL,     ! Number of selected objects.
     :  NUMREJ,     ! Number of rejected objects.
     :  QI          ! Parameter identifier.
      LOGICAL
     :  QUIET,      ! Flag; operate in quiet or verbose (normal) mode?
     :  TRNFRM,     ! Flag; transform positions before selection?
     :  REJCAT,     ! Flag; create catalogue of rejected objects?
     :  GOTSLT,     ! Flag; got type of selection required?
     :  TARGET,     ! Flag; write catalogues as target lists?
     :  INSIDE,     ! Flag; select points inside polygon?
     :  NOROWS      ! Flag; raise status if no rows selected?
      CHARACTER
     :  SELTYP*20,  ! Type of selection required.
     :  TEXT*10,    ! Flag indicating header text to be copied.
     :  CRIT*(CAT__SZEXP),  ! Description of the selection.
     :  EXPR*(CAT__SZEXP),  ! Expression defining selection.
     :  PNAME*(CAT__SZCMP), ! Name of a column or parameter.
     :  MINRNG*75,  ! Minimum extent of a range selection.
     :  MAXRNG*75,  ! Maximum   "    "  "   "       "    .
     :  XCOL*(CAT__SZCMP),    ! Name of X column.
     :  YCOL*(CAT__SZCMP),    !  "   "  Y   "   .
     :  XPLCOL*(CAT__SZCMP),  ! Name of polygon X column.
     :  YPLCOL*(CAT__SZCMP)   !  "   "     "    Y   "   .
      INTEGER
     :  CIPOLY,     ! Identifier for polygon catalogue.
     :  FREQ        ! Frequency for selecting objects.
      CHARACTER
     :  XMIN*75,    ! X minimum for rectangular selection.
     :  XMAX*75,    ! X maximum  "      "           "    .
     :  YMIN*75,    ! Y minimum  "      "           "    .
     :  YMAX*75     ! Y maximum  "      "           "    .
      DOUBLE PRECISION
     :  XMINR,      ! X min. for rectangular selection (radians if angle).
     :  XMAXR,      ! X max.  "      "           "     (   "    "    "  ).
     :  YMINR,      ! Y min.  "      "           "     (   "    "    "  ).
     :  YMAXR       ! Y max.  "      "           "     (   "    "    "  ).
      CHARACTER
     :  RACOL*(CAT__SZCMP),   ! Name of the Right Ascension column.
     :  DCCOL*(CAT__SZCMP),   !  "   "   "  Declination       "   .
     :  RACEN*75,   ! Right Ascension of the central point.
     :  DCCEN*75,   ! Declination     "   "     "      "  .
     :  RADIUS*75   ! Radius of selection.
      DOUBLE PRECISION
     :  RACENR,     ! Right Ascension of the central point (radians).
     :  DCCENR,     ! Declination     "   "     "      "   (   "   ).
     :  RADR        ! Radius of selection (radians).
      CHARACTER
     :  EQUINX*(CAT__SZCMP),  ! Equinox of the catalogue.
     :  EPOCH*(CAT__SZCMP),   ! Epoch   "   "      "    .
     :  RAC*(CAT__SZCMP),     ! Target list column: Right Ascension.
     :  DECC*(CAT__SZCMP),    !   "     "     "   : Declination.
     :  PMRAC*(CAT__SZCMP),   !   "     "     "   : proper motion in RA.
     :  PMDEC*(CAT__SZCMP),   !   "     "     "   :   "      "    "  Dec.
     :  PLXC*(CAT__SZCMP),    !   "     "     "   : parallax.
     :  RVC*(CAT__SZCMP),     !   "     "     "   : radial velocity.
     :  LABELC*(CAT__SZCMP)   !   "     "     "   : plot label.
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
*       Initialise the names of the Right Ascension and Declination
*       columns for a target list to blank.

         RAC = ' '
         DECC = ' '

*
*       Initialise the number of rows selected.

         NUMSEL = 0

*
*       Attempt to open the input catalogue and proceed if ok.

         CALL CAT_ASSOC ('CATIN', 'READ', CIIN, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN

*
*          Determine whether an additional output catalogue of the rejected
*          objects is required.

            CALL PAR_GET0L ('REJCAT', REJCAT, STATUS)
            CALL PAR_CANCL ('REJCAT', STATUS)

*
*          Determine the type of selection required.

            GOTSLT = .FALSE.

            DO WHILE (.NOT. GOTSLT)
               CALL PAR_GET0C ('SELTYP', SELTYP, STATUS)
               CALL PAR_CANCL ('SELTYP', STATUS)

               CALL CHR_UCASE (SELTYP)

               IF (SELTYP(1 : 1) .EQ. 'H') THEN
                  CALL MSG_OUT (' ', 'E ... arbitrary expression.',
     :              STATUS)
                  CALL MSG_OUT (' ',
     :              'R ... range within a sorted column (fast).',
     :              STATUS)
                  CALL MSG_OUT (' ', 'A ... rectangular area.',
     :              STATUS)
                  CALL MSG_OUT (' ', 'C ... circular area.',
     :              STATUS)
                  CALL MSG_OUT (' ', 'P ... polygonal area.',
     :              STATUS)
                  CALL MSG_OUT (' ', 'N ... every Nth entry.',
     :              STATUS)
C                 CALL MSG_OUT (' ',
C     :              'L ... N largest or smallest entries for '/
C     :              /'some column.', STATUS)
                  CALL MSG_OUT (' ', 'H ... list options.',
     :              STATUS)
               END IF

               IF (SELTYP(1 : 1) .EQ. 'E'  .OR.
     :             SELTYP(1 : 1) .EQ. 'R'  .OR.
     :             SELTYP(1 : 1) .EQ. 'A'  .OR.
     :             SELTYP(1 : 1) .EQ. 'C'  .OR.
     :             SELTYP(1 : 1) .EQ. 'P'  .OR.
     :             SELTYP(1 : 1) .EQ. 'N'  .OR.
     :             SELTYP(1 : 1) .EQ. 'L') THEN
                  GOTSLT = .TRUE.
               END IF
            END DO

*
*          If the selection is potentially a selection of a region
*          on the sky then determine whether a transformation of
*          of the values used to define the selection is required.

            IF (SELTYP(1 : 1) .EQ. 'A'  .OR.
     :          SELTYP(1 : 1) .EQ. 'C'  .OR.
     :          SELTYP(1 : 1) .EQ. 'P') THEN
               CALL PAR_GET0L ('TRNFRM', TRNFRM, STATUS)
               CALL PAR_CANCL ('TRNFRM', STATUS)
            END IF

*
*          Determine whether the output catalogues are to be written
*          as target lists.

            CALL PAR_GET0L ('TARGET', TARGET, STATUS)
            CALL PAR_CANCL ('TARGET', STATUS)

*
*          Determine what header text is to be copied.  The options are:
*          A - all,   C - comments only,   N - none.

            TEXT = ' '

            DO WHILE (TEXT(1 : 1) .NE. 'A'  .AND.
     :                TEXT(1 : 1) .NE. 'C'  .AND.
     :                TEXT(1 : 1) .NE. 'N'  .AND.
     :                STATUS .EQ. SAI__OK)
               CALL PAR_GET0C ('TEXT', TEXT, STATUS)
               CALL PAR_CANCL ('TEXT', STATUS)

               CALL CHR_UCASE (TEXT)
            END DO

*
*          Proceed if all is ok.

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Check for the various types of selections and perform
*             the one required.
*
*             Arbitrary expression.

               IF (SELTYP(1 : 1) .EQ. 'E') THEN
                  CALL CAP_GTSTR ('EXPR', EXPR, STATUS)
                  CALL CAT_EIDNT (CIIN, EXPR, EI, STATUS)

                  CALL CAT_SELCT (CIIN, EI, REJCAT, SI, NUMSEL,
     :              SIR, NUMREJ, STATUS)

*
*             Range selection.

               ELSE IF (SELTYP(1 : 1) .EQ. 'R') THEN
                  CALL PAR_GET0C ('PNAME', PNAME, STATUS)
                  CALL PAR_CANCL ('PNAME', STATUS)

                  CALL PAR_GET0C ('MINRNG', MINRNG, STATUS)
                  CALL PAR_CANCL ('MINRNG', STATUS)

                  CALL PAR_GET0C ('MAXRNG', MAXRNG, STATUS)
                  CALL PAR_CANCL ('MAXRNG', STATUS)

                  CALL CAP_RANGE (CIIN, PNAME, MINRNG, MAXRNG,
     :              REJCAT, SI, NUMSEL, SIR, NUMREJ, CRIT, STATUS)

*
*             Rectangular area.

               ELSE IF (SELTYP(1 : 1) .EQ. 'A') THEN
                  CALL PAR_GET0C ('XCOL', XCOL, STATUS)
                  CALL PAR_CANCL ('XCOL', STATUS)

                  CALL PAR_GET0C ('XMIN', XMIN, STATUS)
                  CALL PAR_CANCL ('XMIN', STATUS)

                  CALL PAR_GET0C ('XMAX', XMAX, STATUS)
                  CALL PAR_CANCL ('XMAX', STATUS)

                  CALL PAR_GET0C ('YCOL', YCOL, STATUS)
                  CALL PAR_CANCL ('YCOL', STATUS)

                  CALL PAR_GET0C ('YMIN', YMIN, STATUS)
                  CALL PAR_CANCL ('YMIN', STATUS)

                  CALL PAR_GET0C ('YMAX', YMAX, STATUS)
                  CALL PAR_CANCL ('YMAX', STATUS)

                  IF (INDEX(XMIN, ':') .NE. 0) THEN
                     CALL CAP_ANGDC ('ANGLE', XMIN, XMINR, STATUS)
                  ELSE
                     CALL CHR_CTOD (XMIN, XMINR, STATUS)
                  END IF

                  IF (INDEX(XMAX, ':') .NE. 0) THEN
                     CALL CAP_ANGDC ('ANGLE', XMAX, XMAXR, STATUS)
                  ELSE
                     CALL CHR_CTOD (XMAX, XMAXR, STATUS)
                  END IF

                  IF (INDEX(YMIN, ':') .NE. 0) THEN
                     CALL CAP_ANGDC ('ANGLE', YMIN, YMINR, STATUS)
                  ELSE
                     CALL CHR_CTOD (YMIN, YMINR, STATUS)
                  END IF

                  IF (INDEX(YMAX, ':') .NE. 0) THEN
                     CALL CAP_ANGDC ('ANGLE', YMAX, YMAXR, STATUS)
                  ELSE
                     CALL CHR_CTOD (YMAX, YMAXR, STATUS)
                  END IF

                  CALL CAP_RECT (CIIN, XCOL, YCOL, XMINR, XMAXR,
     :              YMINR, YMAXR, REJCAT, SI, NUMSEL, SIR, NUMREJ,
     :              CRIT, STATUS)

                  RAC = XCOL
                  DECC = YCOL

*
*             Circular area.

               ELSE IF (SELTYP(1 : 1) .EQ. 'C') THEN
                  CALL PAR_GET0C ('RACOL', RACOL, STATUS)
                  CALL PAR_CANCL ('RACOL', STATUS)

                  CALL PAR_GET0C ('DCCOL', DCCOL, STATUS)
                  CALL PAR_CANCL ('DCCOL', STATUS)

                  CALL PAR_GET0C ('RACEN', RACEN, STATUS)
                  CALL PAR_CANCL ('RACEN', STATUS)

                  CALL PAR_GET0C ('DCCEN', DCCEN, STATUS)
                  CALL PAR_CANCL ('DCCEN', STATUS)

                  CALL PAR_GET0C ('RADIUS', RADIUS, STATUS)
                  CALL PAR_CANCL ('RADIUS', STATUS)

                  IF (INDEX(RACEN, ':') .NE. 0) THEN
                     CALL CAP_ANGDC ('HOURS', RACEN, RACENR, STATUS)
                  ELSE
                     CALL CHR_CTOD (RACEN, RACENR, STATUS)
                  END IF

                  IF (INDEX(DCCEN, ':') .NE. 0) THEN
                     CALL CAP_ANGDC ('DEGREES', DCCEN, DCCENR,
     :                 STATUS)
                  ELSE
                     CALL CHR_CTOD (DCCEN, DCCENR, STATUS)
                  END IF

                  IF (INDEX(RADIUS, ':') .NE. 0) THEN
                     CALL CAP_ANGDC ('ARCMIN', RADIUS, RADR, STATUS)
                  ELSE
                     CALL CHR_CTOD (RADIUS, RADR, STATUS)
                  END IF

                  CALL CAP_CIRC (CIIN, RACOL, DCCOL, RACENR, DCCENR,
     :              RADR, REJCAT, SI, NUMSEL, SIR, NUMREJ, CRIT,
     :              STATUS)

                  RAC = RACOL
                  DECC = DCCOL

*
*             Polygonal area.

               ELSE IF (SELTYP(1 : 1) .EQ. 'P') THEN

*
*                Determine the names of the X and Y columns for the
*                selection.

                  CALL PAR_GET0C ('XCOL', XCOL, STATUS)
                  CALL PAR_CANCL ('XCOL', STATUS)

                  CALL PAR_GET0C ('YCOL', YCOL, STATUS)
                  CALL PAR_CANCL ('YCOL', STATUS)

*
*                Attempt to open the polygon catalogue and proceed
*                if ok.

                  CALL CAT_ASSOC ('CATPOLY', 'READ', CIPOLY, STATUS)
                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   Determine the names of the polygon X and Y columns
*                   for the selection.

                     CALL PAR_DEF0C ('XPLCOL', XCOL, STATUS)
                     CALL PAR_GET0C ('XPLCOL', XPLCOL, STATUS)
                     CALL PAR_CANCL ('XPLCOL', STATUS)

                     CALL PAR_DEF0C ('YPLCOL', YCOL, STATUS)
                     CALL PAR_GET0C ('YPLCOL', YPLCOL, STATUS)
                     CALL PAR_CANCL ('YPLCOL', STATUS)

*
*                   Determine whether points inside or outside the
*                   polygon are to be selected.

                     CALL PAR_GET0L ('INSIDE', INSIDE, STATUS)
                     CALL PAR_CANCL ('INSIDE', STATUS)

*
*                   Generate the selection.

                     CALL CAP_SPOLY (CIIN, XCOL, YCOL, CIPOLY,
     :                 XPLCOL, YPLCOL, INSIDE, TRNFRM, REJCAT,
     :                 SI, NUMSEL, SIR, NUMREJ, STATUS)

*
*                   Attempt to close the polygon catalogue.

                     CALL CAT_TRLSE (CIPOLY, STATUS)

                  ELSE
                     CALL ERR_REP ('CATSELECT_PLYOP',
     :                 'Failed to open polygon catalogue.', STATUS)
                  END IF

                  RAC = XCOL
                  DECC = YCOL

*
*             Every Nth object.

               ELSE IF (SELTYP(1 : 1) .EQ. 'N') THEN

                  CALL PAR_GET0I ('FREQ', FREQ, STATUS)
                  CALL PAR_CANCL ('FREQ', STATUS)

                  CALL CAP_NSAMP (CIIN, FREQ, REJCAT, SI, NUMSEL,
     :              SIR, NUMREJ, STATUS)

*
*             N largest or smallest objects.

               ELSE IF (SELTYP(1 : 1) .EQ. 'L') THEN

*                Enter the column required.
*                Enter the number of obejects to be selected.
*                Determine whether largest or smallest to be selected.

                  CALL MSG_OUT (' ', 'Sorry, not implemented yet.',
     :              STATUS)

               ELSE

*
*                Invalid selection type specified.

                  STATUS = SAI__ERROR
                  CALL MSG_SETC ('SELTYP', SELTYP)
                  CALL ERR_REP ('CATSELECT_INVSTP',
     :              'Unknown type of selection requested: ^SELTYP.',
     :              STATUS)
               END IF

*
*             Output the number of rows selected as a parameter.

               CALL PAR_PUT0I ('NUMSEL', NUMSEL, STATUS)

*
*             Proceed if all is ok and some objects were selected.

               IF (STATUS .EQ. SAI__OK  .AND.  NUMSEL .GT. 0) THEN

*
*                Report the number of objects selected.

                  CALL MSG_SETI ('NUMSEL', NUMSEL)
                  CALL MSG_OUT (' ', '^NUMSEL objects selected.',
     :              STATUS)

*
*                Attempt to open the output catalogue and proceed if ok.

                  CALL CAT_CREAT ('CATOUT', CIOUT, STATUS)
                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   Copy the selection as either a simple selection
*                   or a target list, as required.

                     IF (.NOT. TARGET) THEN
                        CALL CAP_CPCAT (SI, CIOUT, TEXT, STATUS)

                     ELSE

*
*                      Output as a target list; first if the input
*                      catalogue does not have an equinox and epoch
*                      then determine what they are.

                        CALL MSG_OUT (' ', 'Enter the names of the '/
     :                    /'special parameters and columns for the '/
     :                    /'target list.', STATUS)

                        CALL CAT_TIDNT (CIIN, 'EQUINOX', QI, STATUS)
                        IF (STATUS .EQ. SAI__OK) THEN
                           EQUINX = ' '
                        ELSE
                           IF (STATUS .EQ. CAT__NOCMP) THEN
                              CALL ERR_ANNUL (STATUS)

                              CALL PAR_GET0C ('EQUINX', EQUINX, STATUS)
                              CALL PAR_CANCL ('EQUINX', STATUS)
                           END IF
                        END IF

                        CALL CAT_TIDNT (CIIN, 'EPOCH', QI, STATUS)
                        IF (STATUS .EQ. SAI__OK) THEN
                           EPOCH = ' '
                        ELSE
                           IF (STATUS .EQ. CAT__NOCMP) THEN
                              CALL ERR_ANNUL (STATUS)

                              CALL PAR_GET0C ('EPOCH', EPOCH, STATUS)
                              CALL PAR_CANCL ('EPOCH', STATUS)
                           END IF
                        END IF

*
*                      Get the names of the special columns for the
*                      target list.

                        IF (RAC .NE. ' '  .AND.  DECC .NE. ' ') THEN
                           CALL PAR_DEF0C ('RAC', RAC, STATUS)
                           CALL PAR_DEF0C ('DECC', DECC, STATUS)
                        END IF

                        CALL PAR_GET0C ('RAC', RAC, STATUS)
                        CALL PAR_CANCL ('RAC', STATUS)

                        CALL PAR_GET0C ('DECC', DECC, STATUS)
                        CALL PAR_CANCL ('DECC', STATUS)

                        CALL PAR_GET0C ('PMRAC', PMRAC, STATUS)
                        CALL PAR_CANCL ('PMRAC', STATUS)

                        CALL PAR_GET0C ('PMDEC', PMDEC, STATUS)
                        CALL PAR_CANCL ('PMDEC', STATUS)

                        CALL PAR_GET0C ('PLXC', PLXC, STATUS)
                        CALL PAR_CANCL ('PLXC', STATUS)

                        CALL PAR_GET0C ('RVC', RVC, STATUS)
                        CALL PAR_CANCL ('RVC', STATUS)

                        CALL PAR_GET0C ('LABELC', LABELC, STATUS)
                        CALL PAR_CANCL ('LABELC', STATUS)

*
*                      Copy the selection as a target list.

                        CALL CAP_CPTRG (SI, CIOUT, RAC, DECC, PMRAC,
     :                    PMDEC, PLXC, RVC, LABELC, EQUINX, EPOCH,
     :                    TEXT, STATUS)
                     END IF

*
*                   If a catalogue of rejected objects then check that
*                   there are some rejects and attempt to create the
*                   catalogue.

                     IF (REJCAT) THEN
                        IF (NUMREJ .GT. 0) THEN
                           CALL CAT_CREAT ('CATREJ', CIREJ, STATUS)
                           IF (STATUS .EQ. CAT__OK) THEN

*
*                            Attempt to copy the catalogue as a catalogue
*                            or a target list, as required.  Note that if
*                            a target list is being written then all the
*                            details obtained for the selected objects
*                            still apply and there is no need obtain them
*                            again.

                              IF (.NOT. TARGET) THEN
                                 CALL CAP_CPCAT (SIR, CIREJ, TEXT,
     :                             STATUS)

                              ELSE
                                 CALL CAP_CPTRG (SIR, CIREJ, RAC, DECC,
     :                             PMRAC, PMDEC, PLXC, RVC, LABELC,
     :                             EQUINX, EPOCH, TEXT, STATUS)
                              END IF

*
*                            Close the catalogue of rejected objects.

                              CALL CAT_TRLSE (CIREJ, STATUS)
                           ELSE
                              CALL ERR_REP ('CATSELECT_REJOP',
     :                          'Failed to open rejects catalogue.',
     :                          STATUS)
                           END IF
                        ELSE
                           CALL CAP_WARN (.TRUE., ' ', 'no rows '/
     :                       /'were rejected; rejects catalogue not '/
     :                       /'written.', STATUS)
                        END IF
                     END IF

*
*                   Close the output catalogue.

                     CALL CAT_TRLSE (CIOUT, STATUS)
                  ELSE
                     CALL ERR_REP ('CATSELECT_OUTOP',
     :                 'Failed to open output catalogue.', STATUS)
                  END IF
               ELSE

*
*                Report a warning if no objects were found which
*                matched the given criterion.

                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL CAP_WARN (.TRUE., ' ', 'no rows '/
     :                 /'satisfied the criteria; output catalogue '/
     :                 /'not written.', STATUS)
                  END IF
               END IF
            END IF

*
*          Close the input catalogue.

            CALL CAT_TRLSE (CIIN, STATUS)

         ELSE
            CALL ERR_REP ('CATSELECT_INOP',
     :        'Failed to open input catalogue.', STATUS)
         END IF

*
*       If the status is ok and no objects were selected then first
*       check whether this case is required to raise a status.
*       If so then set the status to 'warning'.

         IF (STATUS .EQ. SAI__OK ) THEN
            IF (NUMSEL .LE. 0) THEN
               CALL PAR_GET0L ('NOROWS', NOROWS, STATUS)
               CALL PAR_CANCL ('NOROWS', STATUS)

               IF (NOROWS) THEN
                  STATUS = SAI__WARN
               END IF
            END IF
         END IF

*
*       Report any error.

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'CATSELECT_ERR',
     :        'CATSELECT terminating with an error.', STATUS)
         END IF

      END IF

      END
