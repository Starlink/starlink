      SUBROUTINE HDSTRACE( STATUS )
*+
*  Name:
*     HDSTRACE

*  Purpose:
*     Examines the contents of a data-system object.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL HDSTRACE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Data files in ADAM are stored in an hierarchical format (HDS).
*     This cannot be read by just typing the file at the terminal or
*     spooling it to a printer---a special application is required.
*     Now rather than writing separate code to read a variety of
*     structures, this application is sufficiently general to examine
*     almost all HDS structures or objects.  The examination may also
*     be written to a text file as well as being reported to the user.
*
*     The version number of the HDS data format used by the supplied file
*     may also be displayed. See parameter VERSION.
*
*     For the specified ADAM data-system object (X) there are three
*     cases which are handled:
*
*     1) X is a primitive object. The value, or the first and last few
*        values of X are listed.
*
*     2) X is a structure.  The contents of the structure are
*        listed. If a component is encountered which is itself a
*        structure then its contents are listed down to a level of six
*        nested structures.
*
*     3) X is an array of structures.  All elements will be listed
*        if Parameter FULL is set to TRUE; only the first element will
*        be listed when Parameter FULL is set to FALSE (default).
*        Arrays of structures nested six deep can be listed.
*
*     Listings are in the following order: name; dimensions, if any;
*     type; and value or comment.  Comments are enclosed in braces.
*
*     The values are normally listed at the end of each line, but may
*     start on a new line.  The maximum number of lines of data values
*     may also be set.  For all but the smallest arrays where the values
*     of all elements can be displayed in the space provided, the last
*     few values in the array as well as the first few are presented.
*     The last few values appear on a new line, indented the same as
*     the line above with the ellipsis notation to indicate any missing
*     values.  Note the number of elements shown depends on the number
*     of characters that will fit on the line.  The ellipsis notation
*     is also used for long character values where there is only room
*     to show the first and last few characters.  Bad values appear as
*     asterisks.
*
*     The exact layout may be adjusted and is controlled by four
*     additional parameters: a) the indentation of the type string with
*     respect to the beginning of the name string; b) indentation of the
*     value(s) (if not on a new line) with respect to the beginning of
*     the type string; and c) the width of the output.  If the name
*     and dimensions do not fit within the space given by parameters
*     a) and b), the alignment will be lost because at least two spaces
*     will separate the name from the type, or the type from the
*     value(s).  The fourth parameter defines how character arrays are
*     arranged.  The default is that character-array elements are
*     concatenated to fill the available space delimited by commas.  The
*     alternative is to write the value of each element on a new line.
*     This improves readability for long strings.

*  Usage:
*     hdstrace object [full] [nlines] [typind] [valind] [logfile]
*        [eachline] [newline] [width] [widepage] [version] [sorted]

*  ADAM Parameters:
*     EACHLINE = _LOGICAL (Read)
*        If true, the elements of a character array will each appear on
*        a separate line.  Otherwise elements fill the available space
*        and may span several lines, paragraph style. [FALSE]
*     FULL = _LOGICAL (Read)
*        If true, all the contents of an array of structures will be
*        traced, otherwise only the first element is examined. [FALSE]
*     HDSVERSION = _INTEGER (Write)
*        An output parameter in which is placed the version number of
*        the HDS data format used by the supplied file. See also parameter
*        VERSION.
*     LOGFILE = FILENAME (Read)
*        The name of the ASCII file to contain a log of the examination
*        of the data object.  Null (!) means do not create a log file.
*        [!]
*     NEWLINE = _LOGICAL (Read)
*        True indicates that data values are to start on a new line
*        below the name and type, and indented from the name.
*        Otherwise the values are appended to the same line. [FALSE]
*     NLINES = LITERAL (Read)
*        The maximum number of lines in which data values of each
*        primitive array component may be displayed, but excluding the
*        continuation line used to show the last few values.  Note that
*        there may be several data values per line.  There is no
*        formatting of the values.  If you require the whole of each
*        array use NLINES = "ALL".  Beware this facility can result in
*        a large report, so select just the array or arrays you wish to
*        trace. [1]
*     OBJECT = UNIV (Read)
*        The name of the data-system object to be traced.  This may be
*        a whole structure if the name of the container file is given,
*        or it may be an object within the container file, or even a
*        sub-section of an array component.
*     SORTED = _LOGICAL (Read)
*        If true, list structure components in sorted order.  [FALSE]
*     TYPIND = _INTEGER (Read)
*        Column indentation of the component's type with respect to
*        the current indentation of the component's name.  If the name
*        plus dimensions cannot fit in the space provided alignment
*        will be lost, since HDSTRACE insists that there be a gap of at
*        least two columns.  Note that HDS names can be up to 15
*        characters, and the dimension in the format (dim1,dim2,...) is
*        abutted to the name. [15]
*     VALIND = _INTEGER (Read)
*        Column indentation of the component's value(s) with respect to
*        the current indentation of the component's type provided
*        NEWLINE is false.  If, however, NEWLINE is true, VALIND is
*        ignored and the value is indented by one column with respect
*        to the component's name.  If the type cannot fit in the space
*        provided alignment will be lost, since HDSTRACE insists that
*        there be a gap of at least two columns.  HDS types can be up
*        to 15 characters. [15]
*     VERSION = _LOGICAL (Read)
*        If true, the version number (an integer) of the HDS data format
*        used by the supplied file is appended to the end of the trace.
*        It is also stored in output parameter HDSVERSION. [FALSE]
*     WIDEPAGE = _LOGICAL (Read)
*        If true a 132-character-wide format is used to report the
*        examination.  Otherwise the format is 80 characters wide.
*        It is only accessed if WIDTH is null.  [FALSE]
*     WIDTH = _INTEGER (Read)
*        Maximum width of the the output in characters.  The default is
*        the screen width of a terminal (up to the maximum message
*        buffer, currently 300 characters), and 80 for a file.  []

*  Notes:
*     This application allows far more flexibility in layout than
*     earlier applications like LS and the original TRACE, though the
*     order of the attributes of an object has been fixed and
*     rearranged for standardisation, particularly for documentation
*     purposes.

*  Algorithm:
*     Get a locator to the HDS object
*     If no error then
*        Get the full-trace switch, the new-line switch, the number of
*          lines of values, the default indentation for type and value
*        If no error then
*           Get the necessary information for the HDS object.
*           Put out the HDSTRACE header message.
*           Deal with the HDS object depending on whether it is of
*             PRIMITIVE type, a STRUCTURE or an ARRAY of STRUCTURES.
*           If object is of primitive type then put out the information
*             and values associated with this PRIMITIVE HDS object.
*           Else
*              If object is an ARRAY of STRUCTURES, report its name and
*                 dimensions.
*              Examine the HDS object tracing down the array of
*                structures recursively.
*           Endif
*        Endif
*        Put out the HDSTRACE termination message.
*        Tidy up the HDS object.
*     Endif

*  Copyright:
*     Copyright (C) 1989, 1991-1993 Science & Engineering Research
*     Council.
*     Copyright (C) 2014, 2021 Science and Technology Facilities
*     Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S Berry (EAO)
*     GSB: Graham Bell (EAO)
*     {enter_new_authors_here}

*  History:
*     1989 May 16 (MJC):
*        Converted from Baines's TRACE.
*     1989 Jun 15 (MJC):
*        Extra options added for file output, width of the page and one
*        character-array element per line; revise subroutine calls to
*        use TRA_ library.
*     1991 January 31 (MJC):
*        Converted to the SST prologue and expanded the parameter
*        descriptions.
*     1992 January 13 (MJC):
*        Documented the flagging of bad pixels.  Now applies to all
*        numeric data types.
*     1992 September 24 (MJC):
*        Called a new recursive routine that has virtually unlimited
*        ability to trace through the object, and prevents multiple
*        blank lines.  This routine terminates the output with a blank
*        line, so have done likewise for a primitive object, and removed
*        the blank line before the "End of Trace" message.
*     1992 September 25 (MJC):
*        Removed AIF calls.
*     1993 November 4 (MJC):
*        Renamed to HDSTRACE from TRACE to avoid a name clash on UNIX
*        systems.
*     2014 November 15 (MJC):
*        Added WIDTH parameter to allow more control over the width,
*        which defaults to the terminal width.  Adjust the
*        message-system output size to match.  Only access WIDEPAGE if
*        WIDTH is undefined.  WIDEPAGE is retained only for backwards
*        compatibility.
*     2017 May 16 (DSB):
*        Added VERSION and HDSVERSION parameters.
*     2021 February 18 (GSB):
*        Added SORTED parameter.
*     2021 February 25 (MJC):
*        Scale the maximum number of lines of output correctly to avoid
*        numeric wraparound leading to an attempt to print a negative
*        number of values when NLINES=ALL.  Reset and document the
*        maximum output width to MSG__SZMSG, rather than the previous
*        arbitrary 512 characters.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! Switch off the default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Environment constants
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'PRM_PAR'        ! Maximum-value constant
      INCLUDE 'MSG_PAR'        ! MSG constants

*  Status:
      INTEGER STATUS           ! Global status

*  External References:
      INTEGER CHR_LEN          ! String size ignoring trailing blanks

*  Local Constants:
      INTEGER
     :  LNSIZE,                ! Length of message buffer
     :  STEP,                  ! Indentation step size
     :  WDSIZE                 ! Length of wide message buffer
      PARAMETER ( LNSIZE = 78 )
      PARAMETER ( STEP = 3 )
      PARAMETER ( WDSIZE = 130 )

*  Local Variables:
      CHARACTER * ( DAT__SZLOC )
     :  OBJLOC                 ! Locator to object
      CHARACTER * ( DAT__SZTYP )
     :  TYPE                   ! Type of the object
      CHARACTER * ( DAT__SZNAM )
     :  NAME                   ! Name of the object
      CHARACTER * ( MSG__SZMSG )
     :  DUMMY,                 ! Buffer for output of labels to logfile
     :  LINE                   ! Message buffer

      CHARACTER * 120
     :  FILSPC,                ! File specification (not used)
     :  NLC,                   ! Number of lines (character form)
     :  OBJNAM                 ! Data object's name

      LOGICAL                  ! True if:
     :  FULL,                  ! Full trace of arrays of structures
                               ! required
     :  LOGEXM,                ! A log of the examination is written to
                               ! an ASCII file
     :  NEWLIN,                ! Values start on a new line
     :  ONEPLN,                ! Character arrays will have one element
                               ! reported per line
     :  PRIM,                  ! Object is primitive
     :  QUIET,                 ! Will screen output be suppressed?
     :  VERSIO,                ! Display the HDS version number?
     :  WIDEPG,                ! A wide page (132 character) is produced
                               ! rather than default 80
     :  SORTED                 ! Structure components to be sorted?

      INTEGER
     :  CMNTYP,                ! Indentation for type
     :  CMNVAL,                ! Indentation for value
     :  FD,                    ! File description
     :  FILTER,                ! MSG filtering level
     :  HDSVER,                ! HDS version number
     :  I, J,                  ! Character counters
     :  INDENT,                ! Indentation level for text output
     :  NDIM,                  ! Dimensionality of the object
     :  NLEV,                  ! Number of path levels (not used)
     :  NLINES,                ! Number of lines to be used for values
     :  SIZE,                  ! Size of object if vectorised
     :  THEIGH,                ! Height of the terminal in characters
     :  TWIDTH,                ! Width of the terminal in characters
     :  WIDTH,                 ! Width of the output in characters
     :  DIMS( DAT__MXDIM )     ! Dimensions of the object

*.

*    Check global status for an error.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the conditional filtering level for screen output, and set a flag
*    if no screen output will be displayed.
      CALL MSG_IFLEV( FILTER, ' ', STATUS )
      QUIET = ( FILTER .EQ. MSG__QUIET )

*    Get a locator to the desired object.

      OBJLOC = ' '
      CALL DAT_ASSOC( 'OBJECT', 'READ', OBJLOC, STATUS )

*    Check for an error.  Report and abort if one has occurred.

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'HDSTRACE__OPOBJ',
     :     'HDSTRACE: Error associating with the object.', STATUS )
         GOTO 999
      END IF

*    Get the full-trace switch.

      CALL PAR_GTD0L( 'FULL', .FALSE., .TRUE., FULL, STATUS )

*    Get the switch for values on a new line.

      CALL PAR_GTD0L( 'NEWLINE', .FALSE., .TRUE., NEWLIN, STATUS )

*    Get the maximum number of lines of values for each object.

      CALL PAR_MIX0I( 'NLINES', '1', 1, 10000, 'ALL', .FALSE.,
     :                NLC, STATUS )

*    Convert the string to an integer.  The 'ALL' option converts to the
*    largest integer.  The scaling is needed because the subroutines
*    evaluate an expression involving line length/2 * NLINES to find
*    the maximum number of values.  The factor allows a minimum of
*    ten characters for indentation, name, and dimensions.

      IF ( NLC( 1:3 ) .EQ. 'ALL' ) THEN
         NLINES = VAL__MAXI / ( ( MSG__SZMSG - 10 ) / 2 )
      ELSE
         CALL CHR_CTOI( NLC, NLINES, STATUS )
      END IF

*    Get the screen width if there is a terminal.  80 is returned
*    otherwise.

      CALL ONE_SCRSZ( TWIDTH, THEIGH, STATUS )
      TWIDTH = MIN( MSG__SZMSG, TWIDTH )

*    Obtain the desired width, defaulting to the terminal width.  A null
*    return asks that the old WIDEPAGE parameter be used.

      CALL ERR_MARK
      CALL PAR_GDR0I( 'WIDTH', TWIDTH, MIN( TWIDTH, 60 ), MSG__SZMSG,
     :                 .FALSE., WIDTH, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL ( STATUS )

*    Get the switch for wide page.

         CALL PAR_GTD0L( 'WIDEPAGE', .FALSE., .TRUE., WIDEPG, STATUS )
         IF ( WIDEPG ) THEN
            WIDTH = WDSIZE + 2
         ELSE
            WIDTH = LNSIZE + 2
         END IF
      END IF
      CALL ERR_RLSE

*    Tell the message system the desired width.
      CALL MSG_TUNE( 'SZOUT', WIDTH + 2, STATUS )

*    Get the indentation columns for the type and value.

      CALL PAR_GDR0I( 'TYPIND', 15, 10, 40, .FALSE., CMNTYP, STATUS )
      IF ( .NOT. NEWLIN ) THEN
         CALL PAR_GDR0I( 'VALIND', 15, 10, 40, .FALSE., CMNVAL, STATUS )
      ELSE
         CMNVAL = 15
      ENDIF

*    Get the switch for character array elements to appear on one line.

      CALL PAR_GTD0L( 'EACHLINE', .FALSE., .TRUE., ONEPLN, STATUS )

*    Get the switch for displaying the HDS version number.

      CALL PAR_GET0L( 'VERSION', VERSIO, STATUS )

*    Get the switch for sorting the structure components.

      CALL PAR_GET0L( 'SORTED', SORTED, STATUS )

*    Something has gone wrong obtaining the parameters.  Tidy the
*    locator and exit.

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( OBJLOC, STATUS )
         GOTO 999
      END IF

*    Attempt to obtain and open a log file to record the trace
*    listing.  Handle the null case transparently.

      CALL ERR_MARK

*    Open the output log file of the desired width of page.

      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', WIDTH, FD, STATUS )
      LOGEXM = STATUS .EQ. SAI__OK

*    Annul a null status.

      IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*    Release the new error context.

      CALL ERR_RLSE

*    Exit if an error occurred.

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( OBJLOC, STATUS )
         GOTO 999
      END IF

      IF ( LOGEXM )
     :   CALL MSG_OUT( 'LOG', 'Logging to $LOGFILE', STATUS )

*    Store the HDS version number in an output parameter.

      CALL HDS_INFOI( OBJLOC, 'VERSION', ' ', HDSVER, STATUS )
      CALL PAR_PUT0I( 'HDSVERSION', HDSVER, STATUS )

*    Skip the rest if no output is required either on the screen or in a
*    log file.

      IF ( .NOT. QUIET .OR. LOGEXM ) THEN

*    Set the indentation column.

         INDENT = STEP

*    Get all necessary information on this object.

         CALL TRA_LOCIN( OBJLOC, NAME, PRIM, TYPE, SIZE, NDIM, DIMS,
     :                   STATUS )

*    Report the header.

         CALL MSG_OUT( 'BLANK', ' ', STATUS )
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL MSG_OUT( 'HDSTRACE_START', '$OBJECT  <^TYPE>', STATUS )
         CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    Write the header to the logfile.
*    =================================

         IF ( LOGEXM .AND. STATUS .EQ. SAI__OK ) THEN
            DUMMY = ' '
            CALL FIO_WRITE( FD, DUMMY( 1:1 ), STATUS )
            I = 0

*       Obtain the file name.  This is not ideal.

            CALL HDS_TRACE( OBJLOC, NLEV, OBJNAM, FILSPC, STATUS )

*       Form the output string.

            J = CHR_LEN( OBJNAM )
            CALL CHR_PUTC( OBJNAM( 1:J ), DUMMY, I )
            CALL CHR_PUTC( '  <', DUMMY, I )
            J = CHR_LEN( TYPE )
            CALL CHR_PUTC( TYPE( 1:J ), DUMMY, I )
            CALL CHR_PUTC( '>', DUMMY, I )

*       Write the header line to the log file.

            CALL FIO_WRITE( FD, DUMMY( 1:I ), STATUS )
         END IF

*    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*    Deal with the object depending on whether it is PRIMITIVE,
*    a STRUCTURE or an ARRAY of STRUCTURES.

         IF ( PRIM ) THEN

*       Write out the information and values associated with the
*       primitive object.

            CALL TRA_PRIMI( OBJLOC, NAME, TYPE, SIZE, NDIM, DIMS,
     :                      INDENT, CMNVAL, NEWLIN, NLINES, ONEPLN,
     :                      LOGEXM, FD, LINE( :WIDTH ), STATUS )

*       Insert a blank line to match the output from the Hierarchical
*       trace.

            CALL MSG_BLANK( STATUS )
            IF ( LOGEXM ) CALL FIO_WRITE( FD, ' ', STATUS )

         ELSE

*      Check for a scalar-structure object.

            IF ( NDIM .GT. 0 ) THEN

*         We must have an array of structures; output its name and
*         dimensions.

               CALL TRA1_ARSTR( NAME, NDIM, DIMS, INDENT, LOGEXM, FD,
     :                          LINE( :WIDTH ), STATUS )

*         Increment the indentation by one step so that in the output
*         the "Contents of..." message is aligned with the structure's
*         name, and therefore the names of the objects within the arrays
*         of structures are indented with respect to the structure's
*         name.

               INDENT = INDENT + STEP
            END IF

*       Examine the scalar structure using the recursive routine.

            CALL TRA1_THIER( OBJLOC, INDENT, FULL, STEP, CMNTYP,
     :                       CMNVAL, NEWLIN, NLINES, ONEPLN, SORTED,
     :                       LOGEXM, FD, LINE( : WIDTH ), STATUS )

         END IF

*    Write the termination message.

         CALL MSG_OUT( 'HDSTRACE_END', 'End of Trace.', STATUS )
         CALL MSG_BLANK( STATUS )

*    If required, append the HDS version number.

         IF( VERSIO ) THEN
            CALL MSG_SETI( 'V', HDSVER )
            CALL MSG_OUT( 'HDSTRACE_VER', 'HDS data format: V^V',
     :                    STATUS )
            CALL MSG_BLANK( STATUS )
         END IF

*    Write the terminator to the logfile.

         IF ( LOGEXM .AND. STATUS .EQ. SAI__OK ) THEN
            DUMMY = 'End of Trace.'
            CALL FIO_WRITE( FD, DUMMY( 1:13 ), STATUS )
            DUMMY = ' '
            CALL FIO_WRITE( FD, DUMMY( 1:1 ), STATUS )
         END IF

*    If required, write the HDS version number to the logfile.

         IF ( LOGEXM .AND. STATUS .EQ. SAI__OK ) THEN
            DUMMY = 'HDS data format : V'
            I = CHR_LEN( DUMMY )
            CALL CHR_PUTI( HDSVER, DUMMY, I )
            CALL FIO_WRITE( FD, DUMMY( 1:I ), STATUS )
            DUMMY = ' '
            CALL FIO_WRITE( FD, DUMMY( 1:1 ), STATUS )
         END IF

*    Close the log file.

         IF ( LOGEXM ) CALL FIO_CLOSE( FD, STATUS )

      END IF

*    Tidy the locator.

      CALL DAT_ANNUL( OBJLOC, STATUS )

  999 CONTINUE
      END
