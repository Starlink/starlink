      SUBROUTINE PICIN( STATUS )
*+
*  Name:
*     PICIN

*  Purpose:
*     Finds the attributes of a picture interior to the current picture.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PICIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application finds the attributes of a picture, selected by
*     name, that was created since the current picture and lies within
*     the bounds of the current picture.  The search starts from the
*     most-recent picture, unless the current picture is included,
*     whereupon the current picture is tested first.
*
*     The attributes reported are the name, comment, label, name of the
*     reference data object, the bounds in the co-ordinate Frame selected
*     by parameter FRAME.

*  Usage:
*     picin [name] [device] [frame]

*  ADAM Parameters:
*     COMMENT = LITERAL (Write)
*        The comment of the picture.  Up to 132 characters will be written.
*     CURRENT = _LOGICAL (Read)
*        If this is {\tt TRUE}, the current picture is compared against the
*        chosen name before searching from the most-recent picture
*        within the current picture. [FALSE]
*     DESCRIBE = _LOGICAL (Read)
*        This controls whether or not the report (when REPORT=TRUE)
*        should contain a description of the Frame being used.  [FALSE]
*     DEVICE = DEVICE (Read)
*        Name of the graphics device about which information is
*        required. [Current graphics device]
*     DOMAIN = LITERAL (Write)
*        The Domain name of the current co-ordinate Frame for the picture.
*     EPOCH = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        parameter FRAME) for a celestial co-ordinate system, then an
*        epoch value is needed to qualify it. This is the epoch at
*        which the displayed sky co-ordinates were determined. It should
*        be given as a decimal years value, with or without decimal places
*        ("1996.8" for example). Such values are interpreted as a Besselian
*        epoch if less than 1984.0 and as a Julian epoch otherwise.
*     FRAME = LITERAL (Read)
*        A string determining the co-ordinate Frame in which the bounds
*        of the picture are to be reported. When a picture is
*        created by an application such as PICDEF, DISPLAY, etc, WCS
*        information describing the available co-ordinate systems are stored
*        with the picture in the graphics database. This application can
*        report bounds in any of the co-ordinate Frames stored with the
*        current picture. The string supplied for FRAME can be one of the
*        following:
*
*        - A domain name such as SKY, AXIS, PIXEL, BASEPIC, NDC, CURPIC, etc.
*        The special domain AGI_WORLD is used to refer to the world co-ordinate
*        system stored in the AGI graphics database. This can be useful if
*        no WCS information was store with the picture when it was created.
*
*        - An integer value giving the index of the required Frame.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000) (see
*        section "Sky Co-ordinate Systems" in SUN/95).
*
*        If a null value (!) is supplied, bounds are reported in the
*        co-ordinate Frame which was current when the picture was created.
*        [!]
*     LABEL = LITERAL (Write)
*        The label of the picture.  It is blank if there is no label.
*     NAME = LITERAL (Read)
*        The name of the picture to be found within the current picture.
*        If it is null (!), the first interior picture is selected.
*        ["DATA"]
*     PNAME = LITERAL (Write)
*        The name of the picture.
*     REFNAM = LITERAL (Write)
*        The reference object associated with the picture.  It is blank if
*        there is no reference object.  Up to 132 characters will be written.
*     REPORT = _LOGICAL (Read)
*        If this is FALSE details of the picture are not reported, merely the
*        results are written to the output parameters.  It is intended for
*        use within procedures. [TRUE]
*     X1 = LITERAL (Write)
*        The lowest value found within the  picture for axis 1 of the
*        requested co-ordinate Frame (see parameter FRAME).
*     X2 = LITERAL (Write)
*        The highest value found within the  picture for axis 1 of the
*        requested co-ordinate Frame (see parameter FRAME).
*     Y1 = LITERAL (Write)
*        The lowest value found within the  picture for axis 2 of the
*        requested co-ordinate Frame (see parameter FRAME).
*     Y2 = LITERAL (Write)
*        The highest value found within the  picture for axis 2 of the
*        requested co-ordinate Frame (see parameter FRAME).

*  Examples:
*     picin
*        This reports the attributes of the last DATA picture within
*        the current picture for the current graphics device. The bounds
*        of the picture in its current co-ordinate Frame are reported.
*     picin frame=pixel
*        As above but the bounds of the picture in the PIXEL Frame are
*        reported.
*     picin refnam=(object) current
*        This reports the attributes of the last data picture within
*        the current picture for the current graphics device.  If there
*        is a reference data object, its name is written to the ICL
*        variable OBJECT.  The search includes the current picture.
*     picin x1=(x1) x2=(x2) y1=(y1) y2=(y2)
*        This reports the attributes of the last DATA picture within
*        the current picture for the current graphics device.  The
*        bounds of the current picture are written to the ICL
*        variables: X1, X2, Y1, Y2.

*  Notes:
*     This application is intended for use within procedures.  Also if
*     a DATA picture is selected and the current picture is included in
*     the search, this application informs about the same picture that
*     an application that works in a cursor interaction mode would
*     select, and so acts as a check that the correct picture will be
*     accessed.

*  Related Applications:
*     KAPPA: GDSTATE, PICDEF, PICLIST, PICTRANS, PICXY.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 August 20 (MJC):
*        Original version.
*     1993 August 19 (MJC):
*        Added raster co-ordinates.
*     24-SEP-2001 (DSB):
*        Converted to AST/PGPLOT.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     16-MAR-2011 (DSB):
*        Change call to KPG1_DSFRM so that the displayed pixel scales
*        are the median values taken at a range of different positions in
*        the picture, rather than just being the scales at the bottom
*        left corner.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PAR_ERR'          ! Parameter system errors

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATTR*10          ! Buffer for attribute name
      CHARACTER COM*80           ! Picture comment
      CHARACTER DOM0*30          ! Original Current Frame Domain
      CHARACTER LABEL*( DAT__SZNAM ) ! Picture label
      CHARACTER LFMT*80          ! Buffer for formatted lower axis value
      CHARACTER NAME*( DAT__SZNAM ) ! Picture name
      CHARACTER REFNAM*132       ! Reference object's name
      CHARACTER SYM*30           ! Buffer for an axis symbol string
      CHARACTER TEXT*256         ! Buffer for a line of output text
      CHARACTER UFMT*80          ! Buffer for formatted upper axis value
      DOUBLE PRECISION GLBND     ! Lower axis bound in requested Frame
      DOUBLE PRECISION GUBND     ! Upper axis bound in requested Frame
      DOUBLE PRECISION LBNDG( 2 )! Lower bounds of picture in GRAPHICS Frame
      DOUBLE PRECISION UBNDG( 2 )! Upper bounds of picture in GRAPHICS Frame
      DOUBLE PRECISION XL        ! Position of low bound in GRAPHICS Frame
      DOUBLE PRECISION XU        ! Position of high bound in GRAPHICS Frame
      INTEGER IAT                ! Used length of TEXT string
      INTEGER IAXIS              ! Axis index
      INTEGER ICURR              ! Index of original current Frame
      INTEGER IPIC               ! AGI current picture ID
      INTEGER IPICI              ! Interior picture identifier
      INTEGER IPICN              ! Next picture identifier
      INTEGER IPLOT              ! Pointer to picture's AST Plot
      INTEGER JAT                ! Used length of ATTR string
      INTEGER MAP                ! Pointer to Mapping from Base to Current
      INTEGER NCREF              ! Number of characters in reference
      LOGICAL CURRNT             ! Select the current picture if it matches the name?
      LOGICAL DESC               ! Give description of requested Frame?
      LOGICAL HASLAB             ! The picture has a label?
      LOGICAL REFOBJ             ! Is there a reference object?
      LOGICAL REPORT             ! Are the results to be reported?
      LOGICAL USECUR             ! Use original Current Frame?
      LOGICAL VALID              ! Is the reference object a locator?
      REAL X1                    ! Lower picture X bound
      REAL X2                    ! Upper picture X bound
      REAL Y1                    ! Lower picture Y bound
      REAL Y2                    ! Upper picture Y bound

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  See whether reporting is required or not.
      CALL PAR_GET0L( 'REPORT', REPORT, STATUS )

*  Find the name, defaulted to DATA.  A null means a null string and
*  so can be annulled.
      CALL ERR_MARK
      CALL PAR_DEF0C( 'NAME', 'DATA', STATUS )
      CALL PAR_GET0C( 'NAME', NAME, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         NAME = ' '
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL ERR_RLSE

*  Should the current picture be included in the search?
      CALL PAR_GET0L( 'CURRENT', CURRNT, STATUS )

*  Associate image display and start database activity.
      CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC, STATUS )

*  Exclude the current picture if requested to do so by skipping on to
*  the next picture in the database.
      IF( .NOT. CURRNT ) THEN
         CALL AGI_RCS( ' ', IPIC, IPICN, STATUS )
         CALL AGI_SELP( IPICN, STATUS )
      END IF

*  Find the last nominated picture that lies within the current picture,
*  and was created subsequently to the current picture.
      CALL KPG1_AGFND( NAME, IPICI, STATUS )

*  Set the PGPLOT viewport to match this picture, and get an AST Plot for
*  the picture.
      CALL KPG1_GDGET( IPICI, AST__NULL, .FALSE., IPLOT, STATUS )

*  Obtain the current AGI picture name and display it.
      CALL AGI_INAME( NAME, STATUS )
      IF( REPORT ) CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_SETC( 'INAME', NAME )
      CALL PAR_PUT0C( 'PNAME', NAME, STATUS )

*  Get the comment of the picture.
      CALL AGI_ICOM( COM, STATUS )
      CALL MSG_SETC( 'COM', COM )
      CALL PAR_PUT0C( 'COMMENT', COM, STATUS )

*  Obtain the label associated with the picture, if a label exists.
*  Write it to an output parameter.  A dummy is required if it doesn't
*  so that the old value is overwritten.
      HASLAB = .FALSE.
      CALL AGI_ILAB( IPICI, LABEL, STATUS )
      IF( LABEL( 1:1 ) .NE. ' ' ) THEN
         CALL MSG_SETC( 'LABEL', LABEL )
         CALL PAR_PUT0C( 'LABEL', LABEL, STATUS )
         HASLAB = .TRUE.
      ELSE
         CALL PAR_PUT0C( 'LABEL', ' ', STATUS )
      END IF

*  Report it to the user.
      IF( HASLAB .AND. REPORT ) THEN
         CALL MSG_OUT( 'INTPIC', 'Interior picture has name: ^INAME, '/
     :     /'comment: ^COM, label: ^LABEL.', STATUS )
      ELSE IF( REPORT ) THEN
         CALL MSG_OUT( 'INTPIC', 'Interior picture has name: ^INAME, '/
     :     /'comment: ^COM.', STATUS )
      END IF

*  Determine whether or not there is a reference object associated
*  with the current picture.
      CALL KPG1_AGREF( IPICI, 'READ', REFOBJ, REFNAM, STATUS )

*  If one exists translate its locator to a token containing the path
*  name and file name, and tidy the reference locator; or if the
*  reference is just a name, write it to a token.  Note that the token
*  is renewed if it is to be used twice.  Write a message containing
*  the reference object.
      IF( REFOBJ ) THEN
         CALL DAT_VALID( REFNAM( :DAT__SZLOC ), VALID, STATUS )
         IF( VALID ) THEN
            CALL KPG1_HMSG( 'RNAME', REFNAM( :DAT__SZLOC ) )
            CALL REF_ANNUL( REFNAM( :DAT__SZLOC ), STATUS )
            CALL MSG_LOAD( 'REFNAME', '^RNAME', REFNAM, NCREF, STATUS )
         ELSE
            CALL MSG_SETC( 'RNAME', REFNAM )
         END IF

         IF( REPORT ) THEN
            CALL MSG_RENEW
            CALL MSG_OUT( 'REFNAME', 'Reference data object: ^RNAME',
     :                    STATUS )
         END IF

*  Write the reference name to a parameter, using a dummy if there is
*  no object so that a previous name associated with another picture is
*  overwritten.
         CALL PAR_PUT0C( 'REFNAM', REFNAM, STATUS )
      ELSE
         CALL PAR_PUT0C( 'REFNAM', ' ', STATUS )
      END IF

*  Save the index of the Current Frame in the Plot.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Get the Domain of the Current Frame.
      DOM0 = AST_GETC( IPLOT, 'DOMAIN', STATUS )

*  Select the Frame to be reported.
      CALL MSG_SETC( 'OBJ', 'current graphics database picture' )
      CALL KPG1_ASFRM( 'FRAME', 'EPOCH', IPLOT, ' ', ' ', .TRUE.,
     :                 '^OBJ', STATUS )

*  Set a flag indicating whether the required Frame is the original
*  Current Frame.
      USECUR = AST_GETI( IPLOT, 'CURRENT', STATUS ) .EQ. ICURR

*  Get the bounds of the current PGPLOT window (i.e. the current picture),
*  and store double precision equivalents. These values are in GRAPHICS
*  co-ordinates (i.e. mm from the bottom left corner of the display surface).
      CALL PGQWIN( X1, X2, Y1, Y2 )
      LBNDG( 1 ) = DBLE( X1 )
      UBNDG( 1 ) = DBLE( X2 )
      LBNDG( 2 ) = DBLE( Y1 )
      UBNDG( 2 ) = DBLE( Y2 )

*  If required, report a description of the Frame being used.
      IF( REPORT ) THEN

*  See if a description of the Frame being used is required.
         CALL PAR_GET0L( 'DESCRIBE', DESC, STATUS )

*  Display the Domain of the original Current Frame unless this will be
*  done below.
         IF( .NOT. ( DESC .AND. USECUR ) ) THEN
            CALL MSG_SETC( 'DOM', DOM0 )
            CALL MSG_OUT( 'PICIN_MSG6', '   Current co-ordinate '//
     :                    'Frame: ^DOM', STATUS )
         END IF

*  If so, display it, indicating if this is the pictures current Frame or
*  not.
         IF( DESC ) THEN
            IF( USECUR ) THEN
               CALL KPG1_DSFRM( IPLOT, '   Current co-ordinate Frame:',
     :                          LBNDG, UBNDG, .TRUE., STATUS )
            ELSE
               CALL KPG1_DSFRM( IPLOT, '   Requested co-ordinate '//
     :                          'Frame:', LBNDG, UBNDG, .TRUE., STATUS )
            END IF

         ELSE
            CALL MSG_BLANK( STATUS )
         END IF

      END IF

*  Write the Domain of the Current Frame to an output parameter.
      CALL PAR_PUT0C( 'DOMAIN', DOM0, STATUS )

*  Get the Mapping from the Base (GRAPHICS) Frame in the Plot to the
*  current (requested) Frame.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IPLOT, AST__BASE,
     :                                    AST__CURRENT, STATUS ),
     :                    STATUS )

*  Give a heading for the axis bounds.
      IF( REPORT ) THEN
         CALL MSG_SETC( 'DOM', AST_GETC( IPLOT, 'DOMAIN', STATUS ) )
         CALL MSG_OUT( 'PICIN_MSG7', '   Picture bounds in the '//
     :                 '^DOM Frame:', STATUS )
      END IF

*  Loop round each axis in the requested Frame.
      DO IAXIS = 1, AST_GETI( IPLOT, 'NAXES', STATUS )

*  Use the Mapping to determine the bounds of the PGPLOT window
*  along this axis of the requested Frame.
         CALL AST_MAPBOX( MAP, LBNDG, UBNDG, .TRUE., IAXIS, GLBND,
     :                    GUBND, XL, XU, STATUS )

*  Construct a string holding the axis number.
         TEXT = '      Axis '
         IAT = 11
         CALL CHR_PUTI( IAXIS, TEXT, IAT )

*  Construct a string holding the name of the Symbol attribute for this
*  axis.
         ATTR = 'SYMBOL('
         JAT = 7
         CALL CHR_PUTI( IAXIS, ATTR, JAT )
         CALL CHR_APPND( ')', ATTR, JAT )

*  Get the symbol string.
         SYM = AST_GETC( IPLOT, ATTR( : JAT ), STATUS)

*  Remove any PGPLOT escape sequences.
         CALL KPG1_PGESC( SYM, STATUS )

*  Add the axis symbol in parenthesise to the output text if not blank.
         IF( SYM .NE. ' ' ) THEN
            CALL CHR_APPND( ' (', TEXT, IAT )
            CALL CHR_APPND( SYM, TEXT, IAT )
            CALL CHR_APPND( ') :', TEXT, IAT )
         ELSE
            CALL CHR_APPND( ' :', TEXT, IAT )
         END IF

         IAT = IAT + 1

*  Format and append the lower bound value.
         LFMT = AST_FORMAT( IPLOT, IAXIS, GLBND, STATUS )
         CALL CHR_APPND( LFMT, TEXT, IAT )

*  Add a delimiter string
         CALL CHR_APPND( ' to', TEXT, IAT )
         IAT = IAT + 1

*  Format and append the upper bound value.
         UFMT = AST_FORMAT( IPLOT, IAXIS, GUBND, STATUS )
         CALL CHR_APPND( UFMT, TEXT, IAT )

*  Display the text for this axis, if required.
         IF( REPORT ) CALL MSG_OUT( 'PICIN_MSG8', TEXT( : IAT ),
     :                              STATUS )

*  Store the first two axis values in the output parameters.
         IF( IAXIS .EQ. 1 ) THEN
            CALL PAR_PUT0C( 'X1', LFMT, STATUS )
            CALL PAR_PUT0C( 'X2', UFMT, STATUS )

         ELSE IF( IAXIS .EQ. 2 ) THEN
            CALL PAR_PUT0C( 'Y1', LFMT, STATUS )
            CALL PAR_PUT0C( 'Y2', UFMT, STATUS )

         END IF

      END DO

      IF( REPORT ) CALL MSG_BLANK( STATUS )

*  Close down the database and device.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PICIN_ERR', 'PICIN: Unable to find the '//
     :                 'attributes of the interior picture.', STATUS )
      END IF

      END
