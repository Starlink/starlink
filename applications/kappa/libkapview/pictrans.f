      SUBROUTINE PICTRANS( STATUS )
*+
*  Name:
*     PICTRANS

*  Purpose:
*     Transforms a graphics position from one picture co-ordinate Frame
*     to another.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PICTRANS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application transforms a position on a graphics device from
*     one co-ordinate Frame to another.  The input and output Frames may
*     be chosen freely from the Frames available in the WCS information
*     stored with the current picture in the AGI graphics database.  The
*     transformed position is formatted for display and written to the
*     screen and also to an output parameter.

*  Usage:
*     pictrans posin framein [frameout] [device]

*  ADAM Parameters:
*     BOUND = _LOGICAL (Write)
*        BOUND is TRUE when the supplied point lies within the bounds of
*        the current picture.
*     DEVICE = DEVICE (Read)
*        The graphics workstation.  [The current graphics device]
*     EPOCHIN = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        parameter FRAMEIN) for a celestial co-ordinate system, then an
*        epoch value is needed to qualify it.  This is the epoch at
*        which the supplied sky position was determined.  It should be
*        given as a decimal years value, with or without decimal places
*        ("1996.8" for example).  Such values are interpreted as a
*        Besselian epoch if less than 1984.0 and as a Julian epoch
*        otherwise.
*     EPOCHOUT = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        parameter FRAMEOUT) for a celestial co-ordinate system, then an
*        epoch value is needed to qualify it.  This is the epoch at
*        which the transformed sky position is required.  It should be
*        given as a decimal years value, with or without decimal places
*        ("1996.8" for example).  Such values are interpreted as a
*        Besselian epoch if less than 1984.0 and as a Julian epoch
*        otherwise.
*     FRAMEIN = LITERAL (Read)
*        A string specifying the co-ordinate Frame in which the input
*        position is supplied (see parameter POSIN).  The string can be
*        one of the following:
*
*        - A domain name such as SKY, AXIS, PIXEL, GRAPHICS, NDC,
*        CURPIC, BASEPIC, CURNDC, etc.
*
*        - An integer value giving the index of the required Frame
*        within the WCS component.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000)
*        (see  section "Sky Co-ordinate Systems" in SUN/95).
*
*        If a null parameter value is supplied, then the current Frame
*        in the current picture is used.  [!]
*     FRAMEOUT = LITERAL (Read)
*        A string specifying the co-ordinate Frame in which the
*        transformed position is required.  If a null parameter value is
*        supplied, then the current Frame in the picture is used.  The
*        string can be one of the following options.
*
*        - A domain name such as SKY, AXIS, PIXEL, GRAPHICS, NDC,
*        CURPIC, BASEPIC, CURNDC, etc.
*
*        - An integer value giving the index of the required Frame
*        within the WCS component.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000)
*        (see section "Sky Co-ordinate Systems" in SUN/95).
*
*        If a null parameter value is supplied, then the BASEPIC Frame
*        is used.  ["BASEPIC"]
*     POSIN = LITERAL (Read)
*        The co-ordinates of the position to be transformed, in the
*        co-ordinate Frame specified by parameter FRAMEIN (supplying
*        a colon ":" will display details of the required co-ordinate
*        Frame).  The position should be supplied as a list of
*        formatted axis values separated by spaces or commas.
*     POSOUT = LITERAL (Write)
*        The formatted co-ordinates of the transformed position, in the
*        co-ordinate Frame specified by parameter FRAMEOUT. The position
*        will be stored as a list of formatted axis values separated by
*        spaces.

*  Examples:
*     pictrans "100.3,-20.1" framein=pixel
*        This converts the position (100.3,-20.1), in pixel co-ordinates
*        within the current picture of the current graphics device, to
*        the BASEPIC co-ordinates of that point in the BASE picture.
*     pictrans "100.3,-20.1" framein=pixel frameout=graphics
*        This converts the position (100.3,-20.1), in pixel co-ordinates
*        within the current picture of the current graphics device, to
*        the GRAPHICS co-ordinates of that point (i.e. millimetres from
*        the bottom-left corner of the graphics device).
*     pictrans "10 10" framein=graphics frameout=basepic
*        This converts the position (10 10), in graphics co-ordinates
*        (i.e. the point which is 10mm above and to the right of the
*        lower-left corner of the graphics device), into BASEPIC
*        co-ordinates.

*  Notes:
*     -  BASEPIC co-ordinates locate a position within the entire
*     graphics device.  The bottom-left corner of the device screen has
*     BASEPIC co-ordinates of (0,0).  The shorter dimension of the
*     screen has length 1.0, and the other axis has a length greater
*     than 1.0.
*
*     -  NDC co-ordinates also locate a position within the entire
*     graphics device.  The bottom-left corner of the device screen has
*     NDC co-ordinates of (0,0), and the top-right corner has NDC
*     co-ordinates (1,1).
*
*     -  GRAPHICS co-ordinates also span the entire graphics device but
*     are measured in millimetres from the bottom left corner.
*
*     -  CURPIC co-ordinates locate a point within the current picture.
*     The bottom-left corner of the current picture has CURPIC
*     co-ordinates of (0,0).  The shorter dimension of the current
*     picture has length 1.0, and the other axis has a length greater
*     than 1.0.
*
*     -  CURNDC co-ordinates also locate a position within the current
*     picture. The bottom left corner of the current picture has CURNDC
*     co-ordinates of (0,0), and the top right corner has CURNDC
*     co-ordinates (1,1).

*     -  The transformed position is not written to the screen when the
*     message filter environment variable MSG_FILTER is set to QUIET.
*     The creation of the output Parameter POSOUT is unaffected
*     by MSG_FILTER.

*  Related Applications:
*     KAPPA: GDSTATE, PICIN, PICXY.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 2001-2002, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2009 Science and Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1993 August 19 (MJC):
*        Original version.
*     24-SEP-2001 (DSB):
*        Converted to AST/PGPLOT.
*     13-AUG-2002 (DSB):
*        Added CURNDC Frame.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2009 July 24 (MJC):
*        Remove QUIET parameter and use the current reporting level
*        instead (set by the global MSG_FILTER environment variable).
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'MSG_PAR'          ! Message-system constants

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER TEXT*128         ! Formatted text
      DOUBLE PRECISION CURPIC( 2 ) ! CURPIC position
      DOUBLE PRECISION POSIN( NDF__MXDIM ) ! Input position
      DOUBLE PRECISION POSOUT( NDF__MXDIM )! Output position
      INTEGER FRMIN              ! Pointer to requested input Frame
      INTEGER FRMOUT             ! Pointer to requested output Frame
      INTEGER IAT                ! No. of characters in TEXT variable
      INTEGER ICUR               ! Index of CURPIC Frame
      INTEGER IFRMIN             ! Index of input Frame
      INTEGER IPIC               ! AGI identifier for current picture
      INTEGER IPLOT              ! AST pointer for the Plot
      INTEGER MAP                ! Point er to AST Mapping
      INTEGER NIN                ! Number of input axes
      INTEGER NOUT               ! Number of output axes
      LOGICAL BOUND              ! Point is within the picture's bounds
      LOGICAL REPORT             ! Report results to screen?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Associate image display and start database activity.
      CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC, STATUS )

*  Set the PGPLOT viewport to match this picture, and get an AST Plot
*  for the picture.
      CALL KPG1_GDGET( IPIC, AST__NULL, .FALSE., IPLOT, STATUS )

*  Set the current Frame to be the required input Frame specified by
*  parameter FRAMEIN. If "WORLD" co-ordinates are requested, use
*  AGI_WORLD.  If "DATA" co-ordinates are requested, use "AGI_DATA".
      CALL MSG_SETC( 'PIC', 'picture' )
      CALL KPG1_ASFRM( 'FRAMEIN', 'EPOCHIN', IPLOT, 'AGI_WORLD',
     :                 'AGI_DATA', .TRUE., '^PIC', STATUS )

*  Get a pointer to the input Frame.
      FRMIN = AST_GETFRAME( IPLOT, AST__CURRENT, STATUS )

*  Get the index of the input Frame.
      IFRMIN = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Get the number of input axes.
      NIN = AST_GETI( FRMIN, 'NAXES', STATUS )

*  Get the input position. Do not supply a dynamic default.
      POSIN( 1 ) = AST__BAD
      CALL KPG1_GTPOS( 'POSIN', FRMIN, .FALSE., POSIN, 0.0D0, STATUS )

*  Set the current Frame to be the required output Frame specified by
*  parameter FRAMEOUT.
      CALL MSG_SETC( 'PIC', 'picture' )
      CALL KPG1_ASFRM( 'FRAMEOUT', 'EPOCHOUT', IPLOT, 'AGI_WORLD',
     :                 'AGI_DATA', .TRUE., '^PIC', STATUS )

*  Get a pointer to the output Frame.
      FRMOUT = AST_GETFRAME( IPLOT, AST__CURRENT, STATUS )

*  Get the number of output axes.
      NOUT = AST_GETI( FRMOUT, 'NAXES', STATUS )

*  Get the Mapping from input to output Frame.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IPLOT, IFRMIN, AST__CURRENT,
     :                                    STATUS ), STATUS )

*  Transform the input position into the output Frame.
      CALL AST_TRANN( MAP, 1, NIN, 1, POSIN, .TRUE., NOUT, 1, POSOUT,
     :                STATUS )

*  See if we are to report the results, i.e at NORMAL or higher priority.
      REPORT = MSG_FLEVOK( MSG__NORM, STATUS )

      IF ( REPORT ) THEN

*  If so, format the input position including axis symbols.
         TEXT = ' '
         IAT = 0

         CALL KPG1_ASPTP( FRMIN, NIN, POSIN, .TRUE., '  ', TEXT, IAT,
     :                    STATUS )

*  Add "  -->  " to the displayed text.
         CALL CHR_APPND( '  -->', TEXT, IAT )
         IAT = IAT + 2

*  Format the transformed position, including axis symbols.
         CALL KPG1_ASPTP( FRMOUT, NOUT, POSOUT, .TRUE., '  ', TEXT, IAT,
     :                    STATUS )

*  Display the text, between blank lines.
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETC( 'TEXT', TEXT )
         CALL MSG_OUT( 'PICTRANS_MSG', '  ^TEXT', STATUS )
         CALL MSG_BLANK( STATUS )

      END IF

*  Format the transformed position again, this time without axis
*  symbols.
      TEXT = ' '
      IAT = 0
      CALL KPG1_ASPTP( FRMOUT, NOUT, POSOUT, .FALSE., '  ', TEXT, IAT,
     :                 STATUS )

*  Write this text to the output parameter.
      CALL PAR_PUT0C( 'POSOUT', TEXT( : IAT ), STATUS )

*  Now get the index of the CURPIC Frame in the Plot.
      CALL KPG1_ASFFR( IPLOT, 'CURPIC', ICUR, STATUS )

*  Get the Mapping from input to CURPIC Frame.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IPLOT, IFRMIN, ICUR,
     :                                    STATUS ), STATUS )

*  Transform the input position into the CURPIC Frame.
      CALL AST_TRANN( MAP, 1, NIN, 1, POSIN, .TRUE., 2, 1, CURPIC,
     :                STATUS )

*  If the CURPIC value on both axes is within the range [0,1] the point
*  is inside the current picture.
      BOUND = CURPIC( 1 ) .GE. 0.0 .AND. CURPIC( 1 ) .LE. 1.0 .AND.
     :        CURPIC( 2 ) .GE. 0.0 .AND. CURPIC( 2 ) .LE. 1.0

*  Output the bound check.
      CALL PAR_PUT0L( 'BOUND', BOUND, STATUS )

*  Close down the database and device.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PICTRANS_ERR', 'PICTRANS: Failed to transform '/
     :                 /'a position between two picture co-ordinate '//
     :                 'Frames.', STATUS )
      END IF

      END
