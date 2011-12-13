      SUBROUTINE PLOTLIST( STATUS )
*+
*  Name:
*     PLOTLIST

*  Purpose:
*     Draws position markers on a graphics display.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PLOTLIST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine draws a variety of markers (crosses, circles,
*     squares etc.) on a graphics device at positions specified
*     in a series of position lists.  Before this application can be
*     run an image (or other graphical output such as a contour image)
*     must have been displayed using a suitable routine such as
*     KAPPA's DISPLAY (SUN/95) or CCDPACK's DRAWNDF.
*
*     For a more interactive display of markers on an Xwindows display,
*     you can use the IDICURS program instead.

*  Usage:
*     plotlist inlist [device]

*  ADAM Parameters:
*     CLEAR = _LOGICAL (Read)
*        This parameter controls whether or not the display device
*        is cleared before plotting the markers. Setting this TRUE could
*        be useful if plotting in a device overlay.
*        [FALSE]
*     DEVICE = DEVICE (Write)
*        The name of the device on which to plot the markers.
*        [Current display device]
*     INLIST = LITERAL (Read)
*        This parameter is used to access the names of the lists which
*        contain the positions and, if NDFNAMES is TRUE, the names of
*        the associated NDFs. If NDFNAMES is TRUE the names of the
*        position lists are assumed to be stored in the extension of
*        the NDFs (in the CCDPACK extension item CURRENT_LIST) and the
*        names of the NDFs themselves should be given (and may include
*        wildcards).
*
*        If NDFNAMES is FALSE then the actual names of the position
*        lists should be given. These may not use wildcards but may be
*        specified using indirection (other CCDPACK position list
*        processing routines will write the names of their results
*        files into files suitable for use in this manner) the
*        indirection character is "^".
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     MSIZE = _REAL (Read)
*        The size of the marker which will be drawn as a multiple of
*        the default value. So for instance doubling the value of this
*        parameter will increase the size of the markers by a factor of
*        two. The default marker size is around 1/40 of the lesser of
*        the width or height of the plot.
*        [2.5]
*     MTYPE = _INTEGER (Read)
*        The type of marker to plot at the positions given in the input
*        files. PGPLOT Graph Markers are drawn if the value lies in the
*        range 0-31 (a value of 2 gives a cross, 7 a triangle, 24-27
*        various circles etc. see the PGPLOT manual). If the value of
*        this parameter is less than zero then the identifier values,
*        which are in column one of the input file, will be written over
*        the objects.
*        [2]
*     NDFNAMES = _LOGICAL (Read)
*        If TRUE then the routine will assume that the names of the
*        position lists are stored in the NDF CCDPACK extensions under
*        the item "CURRENT_LIST".
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [TRUE]
*     PALNUM = _INTEGER (Read)
*        The pen number to use when drawing the markers.  The colours
*        associated with these pens are the default PGPLOT pens (see
*        the PGPLOT manual for a complete description). These are:
*           - 0 -- background colour
*           - 1 -- foreground colour
*           - 2 -- red
*           - 3 -- green
*           - 4 -- blue
*           - 5 -- cyan
*           - 6 -- magenta
*           - 7 -- yellow
*           - 8 -- orange
*
*        and so on up to pen 16 (or up to the number available on the
*        current graphics device). After PLOTLIST has been run these
*        colours can be superseded by using the KAPPA palette
*        facilities PALDEF and PALENTRY, but note that any subsequent
*        runs of PLOTLIST will reinstate the PGPLOT default colours.
*        The KAPPA palette pen numbers correspond to PALNUM values
*        (hence the parameter name).
*        [3]
*     THICK = _INTEGER (Read)
*        The thickness of the lines used to draw the markers. This may
*        take any value in the range 1-21.
*        [1]

*  Examples:
*     plotlist inlist='*'
*        In this example all the NDFs in the current directory are
*        accessed and their associated lists of positions are plotted
*        onto the current display device.
*
*     plotlist ndfnames=false inlist=one_list.dat
*        In this example the position list one_list.dat is opened and
*        its position are plotted on the current display device.
*
*     plotlist in='aligned_*' mtype=-1 palnum=4 msize=1 thick=3
*        In this example the NDFs aligned_* have their associated
*        position lists accessed and the positions are plotted on the
*        current display device. The pen colour used is blue. The
*        text is drawn at a relative size of 1 (the normal default  is
*        2.5) with a line thickness of 3.

*  Notes:
*     - Position list formats.
*
*       CCDPACK supports data in two formats.
*
*       CCDPACK format - the first three columns are interpreted as the
*       following.
*
*          - Column 1: an integer identifier
*          - Column 2: the X position
*          - Column 3: the Y position
*
*       The column one value must be an integer and is used to identify
*       positions which are the same but which have different locations
*       on different images. Values in any other (trailing) columns are
*       usually ignored.
*
*       EXTERNAL format - positions are specified using just an X and
*       a Y entry and no other entries.
*
*          - Column 1: the X position
*          - Column 2: the Y position
*
*       This format is used by KAPPA applications such as CURSOR.
*
*       Comments may be included in a file using the characters "#" and
*       "!". Columns may be separated by the use of commas or spaces.
*
*     - NDF extension items.
*
*       If NDFNAMES is TRUE then the item "CURRENT_LIST" of the
*       .MORE.CCDPACK structure of the input NDFs will be located
*       and assumed to contain the names of the lists whose positions
*       are to be plotted.

*  Behaviour of Parameters:
*     All parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application. The intrinsic
*     default behaviour of the application may be restored by using the
*     RESET keyword on the command line.
*
*     Certain parameters (LOGTO, LOGFILE and NDFNAMES) have global
*     values. These global values will always take precedence, except
*     when an assignment is made on the command line. Global values may
*     be set and reset using the CCDSETUP and CCDCLEAR commands.
*
*     The DEVICE parameter also has a global association. This is not
*     controlled by the usual CCDPACK mechanisms, instead it works in
*     co-operation with KAPPA (SUN/95) image display/control routines.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2000-2001 Central Laboratory of the
*     Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1992 (PDRAPER):
*        Original version.
*     6-OCT-1995 (PDRAPER):
*        Updated for CCDPACK version 2.0.
*     29-JUL-1998 (PDRAPER):
*        Added missing CCD1_END call.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     9-JAN-2001 (MBT):
*        Modified to use AST and AGI properly.  This allows it to interact
*        with the AGI graphics database much more intelligently.
*     22-MAY-2001 (MBT):
*        Changed to cope with empty position list files.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
      INCLUDE 'FIO_PAR'          ! FIO parameters
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
*     EXTERNAL FIO_TEST
      LOGICAL FIO_TEST           ! Test FIO error returns.

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for readind data lines
      CHARACTER * ( FIO__SZFNM ) FNAME ! Name of position list
      CHARACTER * ( AST__SZCHR ) STYELS ! Plot style elements
      INTEGER CI1                ! Minimum allowed colour index
      INTEGER CI2                ! Maximum allowed colour index
      INTEGER FDIN               ! FIO system file descriptor
      INTEGER FIOGR              ! Input FIO identifiers
      INTEGER FRM                ! AST identifier for a frame
      INTEGER FSET               ! AST identifier for a frameset
      INTEGER INDEX              ! Loop variable
      INTEGER INDF               ! NDF identifier
      INTEGER IPDAT              ! Pointer to input data
      INTEGER IPEN               ! Pen number
      INTEGER IPID               ! Pointer to data identifiers
      INTEGER JCURF              ! Frame index of Current frame of Frameset
      INTEGER JCURP              ! Frame index of Current frame of Plot
      INTEGER JPIXF              ! Frame index of Pixel frame of Frameset
      INTEGER MTYPE              ! Marker type
      INTEGER NDFGR              ! Input NDF group identifier
      INTEGER NLGR               ! Group of NDFs with no associated lists
      INTEGER NNOLIS             ! Number of NDFs with no associated lists
      INTEGER NOPEN              ! Number of input lists
      INTEGER NREC               ! Number of input data records
      INTEGER NVAL               ! Number of values in input data
      INTEGER PICID              ! AGI picture identifier
      INTEGER PLOT               ! AST identifier for Plot object
      INTEGER STYELL             ! Number of characters in STYELS string
      INTEGER THICK              ! Marker pen width
      LOGICAL CLEAR              ! True if device is to be cleared
      LOGICAL NDFS               ! True if position list names in NDFs
      LOGICAL XYONLY             ! True if only X and Y positions are available
      REAL MSIZE                 ! Marker size (relative to default)

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup CCDPACK.
      CALL CCD1_START( 'PLOTLIST', STATUS )

*  Begin a new AST context.
      CALL AST_BEGIN( STATUS )

*  Begin a new NDF context.
      CALL NDF_BEGIN

*  Find the source of the position list names. Are they associated with
*  NDFs or is an explicit lists of names to be given?
      NDFS = .TRUE.
      CALL PAR_GET0L( 'NDFNAMES', NDFS, STATUS )

*  Now access a group of position list names.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL CCD1_GTLIG( NDFS, 'CURRENT_LIST', 'INLIST', 1, CCD1__MXLIS,
     :                 .FALSE., NOPEN, FIOGR, NDFGR, NNOLIS, NLGR,
     :                 STATUS )
      CALL CCD1_GRDEL( NLGR, STATUS )

*  If not all supplied NDFs have position lists, warn the user of
*  this fact and continue.
      IF ( NNOLIS .GT. 0 ) THEN
         CALL CCD1_MSG( ' ', '  NDFs with no associated '//
     :                  'position lists will be ignored.', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
      END IF

*  Write the names of the input lists out to the user.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Input position lists:', STATUS )
      CALL CCD1_MSG( ' ', '    ---------------------', STATUS )
      DO 6 INDEX = 1, NOPEN
         CALL GRP_GET( FIOGR, INDEX, 1, FNAME, STATUS )
         CALL MSG_SETC( 'FNAME', FNAME )
         CALL MSG_SETI( 'N', INDEX )
         CALL CCD1_MSG( ' ', '  ^N) ^FNAME', STATUS )
 6    CONTINUE

*  Where the position list names originated.
      IF ( NDFS ) THEN
         CALL CCD1_MSG( ' ',
     :'  Position list names extracted from NDF extensions.', STATUS )

*  Write the names of the NDFs out to the user.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '    Associated NDFs:', STATUS )
         CALL CCD1_MSG( ' ', '    ----------------', STATUS )
         DO 7 INDEX = 1, NOPEN
            CALL GRP_GET( NDFGR, INDEX, 1, FNAME, STATUS )
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL MSG_SETI( 'N', INDEX )
            CALL CCD1_MSG( ' ', '  ^N) ^FNAME', STATUS )
 7       CONTINUE
      END IF

*  See if device is to be cleared.
      CLEAR = .FALSE.
      CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )

*  Open the graphics device, setting the current AGI picture to the
*  last DATA frame.
      IF( CLEAR ) THEN
         CALL AGP_ASSOC( 'DEVICE', 'WRITE', 'DATA', .FALSE., PICID,
     :                    STATUS )
      ELSE
         CALL AGP_ASSOC( 'DEVICE', 'READ', 'DATA', .FALSE., PICID,
     :                    STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get the marker size.
      CALL PAR_GET0R( 'MSIZE', MSIZE, STATUS )
      MSIZE = ABS( MSIZE )
      CALL PGSCH( MSIZE )

*  Get the marker type.
      CALL PAR_GET0I( 'MTYPE', MTYPE, STATUS )

*  Get the marker colour.
      CALL PAR_GET0I( 'PALNUM', IPEN, STATUS )

*  Set the pen number to a sensible value for the current device.
      CALL PGQCOL( CI1, CI2 )
      IPEN = MAX( CI1, MIN( CI2, IPEN ) )
      CALL PGSCI( IPEN )

*  Get the marker thickness.
      CALL PAR_GET0I( 'THICK', THICK, STATUS )
      THICK = MAX( 1, MIN( THICK, 21 ) )
      CALL PGSLW( THICK )

*  Construct a list of style elements based on the ADAM parameter values.
      CALL MSG_SETR( 'MSIZE', MSIZE )
      CALL MSG_SETI( 'PALNUM', IPEN )
      CALL MSG_SETI( 'THICK', THICK )
      CALL MSG_LOAD( ' ', 'Size(markers)=^MSIZE,' //
     :               'Colour(markers)=^PALNUM,' //
     :               'Colour(strings)=^PALNUM,' //
     :               'Width(markers)=^THICK', STYELS, STYELL, STATUS )

*  If we are not using NDFs construct a default AST Plot object aligned
*  with the current AGI picture to do the plotting into.  Its Current
*  frame will be in the PIXEL domain.
      IF ( .NOT. NDFS ) THEN
         FRM = AST_FRAME( 2, 'Domain=PIXEL', STATUS )
         FSET = AST_FRAMESET( FRM, ' ', STATUS )
         CALL CCD1_APLOT( FSET, PICID, .TRUE., PLOT, STATUS )
         CALL CCD1_PLSTY( PLOT, ' ', ' ', STATUS )
         CALL AST_SET( PLOT, STYELS( 1:STYELL ), STATUS )
      END IF

*  Now loop opening each position list then plotting its positions.
      DO 9999 INDEX = 1, NOPEN

*  Get the name of the input list and open the file.
         CALL GRP_GET( FIOGR, INDEX, 1, FNAME, STATUS )
         CALL CCD1_OPFIO( FNAME, 'READ', 'LIST', 0, FDIN, STATUS )

*  Report error message if open failed.
         IF ( FIO_TEST( 'OPEN error', STATUS ) ) THEN
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL ERR_REP( 'TRANLIST_PFERR',
     :      '  Failed to input file ^FNAME', STATUS )
            GO TO 98
         END IF

*  Write informational message about it.
         CALL CCD1_MSG( ' ',  ' ', STATUS )
         CALL MSG_SETC( 'CURRENT_LIST', FNAME )
         CALL CCD1_MSG( ' ', '  +++ Plotting file: ^CURRENT_LIST',
     :                  STATUS )

*  Inform user how many files we've processed out of the total number.
         CALL MSG_SETI( 'CURRENT_NUM', INDEX )
         CALL MSG_SETI( 'MAX_NUM', NOPEN )
         CALL CCD1_MSG( ' ', '  (Number ^CURRENT_NUM of ^MAX_NUM)',
     :                  STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )

*  If we are using NDFs construct an AST Plot object from the WCS
*  component of the NDF.
         IF ( NDFS ) THEN

*  Get the NDF identifier.
            CALL NDG_NDFAS( NDFGR, INDEX, 'READ', INDF, STATUS )

*  Get the WCS component of the NDF.
            CALL CCD1_GTWCS( INDF, FSET, STATUS )

*  Record the frame indices of the Current and Pixel domains.
            CALL CCD1_FRDM( FSET, 'PIXEL', JPIXF, STATUS )
            JCURF = AST_GETI( FSET, 'Current', STATUS )

*  Construct a Plot object aligned with the current AGI picture.
            CALL CCD1_APLOT( FSET, PICID, .TRUE., PLOT, STATUS )

*  Now set the Current frame of the Plot to the frame which was in the
*  PIXEL domain of the NDF.  Although the frame indices of PLOT will
*  be different from those of FSET, we can use the fact that the
*  Current frame is the same, and the relative positions are the same.
            JCURP = AST_GETI( PLOT, 'Current', STATUS ) + JPIXF - JCURF
            CALL AST_SETI( PLOT, 'Current', JCURP, STATUS )

*  Apply default and user-selected style settings to the plot.
            CALL CCD1_PLSTY( PLOT, ' ', ' ', STATUS )
            CALL AST_SET( PLOT, STYELS( 1:STYELL ), STATUS )
         END IF

*  Find out how many entries it has.
         CALL CCD1_LTEST( FDIN, LINE, CCD1__BLEN, 2, 0, NVAL, STATUS )

*  Only attempt to plot points if there are any.
         IF ( NVAL .GT. 0 ) THEN

*  Map in the data.
            IF ( NVAL .EQ. 2 ) THEN

*  X and Y positions only.
               CALL CCD1_NLMAP( FDIN, LINE, CCD1__BLEN, IPDAT, NREC,
     :                          NVAL, STATUS )
               XYONLY = .TRUE.
            ELSE
               CALL CCD1_LMAP( FDIN, LINE, CCD1__BLEN, IPID, IPDAT,
     :                         NREC, NVAL, STATUS )
               XYONLY = .FALSE.
            END IF

*  If MTYPE is less then zero then use any identifiers as the marker.
*  Otherwise we will not use identifiers
            IF ( XYONLY ) THEN
               MTYPE = MAX( 0, MIN( 31, ABS( MTYPE ) ) )
            ELSE
               IF ( MTYPE .GE. 0 ) THEN
                  MTYPE = MIN( 31, MTYPE )
               END IF
            END IF

*  Draw the points.
            CALL CCD1_DRAWA( PLOT, %VAL( CNF_PVAL( IPID ) ),
     :                       %VAL( CNF_PVAL( IPDAT ) ), NREC,
     :                       NVAL, MTYPE, STATUS )

*  Free memory used on this pass.
            CALL CCD1_MFREE( IPID, STATUS )
            CALL CCD1_MFREE( IPDAT, STATUS )
         END IF

*  Close the position list.
 98      CALL FIO_CLOSE( FDIN, STATUS )

*  Stop if having problems.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
 9999 CONTINUE

*  Exit label.
 99   CONTINUE

*  Release all the workspace.
      CALL CCD1_MFREE( -1, STATUS )

*  Close the graphics device.
      CALL AGP_DEASS( 'DEVICE', .TRUE., STATUS )

*  Relase group resources.
      CALL CCD1_GRDEL( FIOGR, STATUS )
      CALL CCD1_GRDEL( NDFGR, STATUS )

*  Exit the NDF context.
      CALL NDF_END( STATUS )

*  Exit the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PLOTLIST_ERR',
     :   'PLOTLIST: Error plotting positions.',
     :   STATUS )
      END IF

*  Close CCDPACK logging system.
      CALL CCD1_END( STATUS )

      END
* $Id$
