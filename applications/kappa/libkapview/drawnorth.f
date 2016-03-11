      SUBROUTINE DRAWNORTH( STATUS )
*+
*  Name:
*     DRAWNORTH

*  Purpose:
*     Draws arrows parallel to the axes.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DRAWNORTH( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application draws a pair of arrows on top of a previously
*     displayed DATA picture which indicate the directions of the
*     labelled axes in the underlying picture, at the position specified
*     by Parameter ORIGIN.  For instance, if the underlying picture has
*     axes labelled with celestial co-ordinates, then the arrows will
*     by default indicate the directions of north and east.  The
*     appearance of the arrows, including the labels attached to each
*     arrow, may be controlled using the STYLE parameter.  The picture
*     area behind the arrows may optionally be cleared before drawing
*     the arrows (see Parameter BLANK).

*  Usage:
*     drawnorth [device] [length] [origin]

*  ADAM Parameters:
*     ARROW = _REAL (Read)
*        The size of the arrow heads are specified by this parameter.
*        Simple lines can be drawn by setting the arrow head size to
*        zero. The value should be expressed as a fraction of the
*        largest dimension of the underlying DATA picture.
*        [current value]
*     BLANK = _LOGICAL (Read)
*        If TRUE, then the area behind the arrows is blanked before the
*        arrows are drawn. This is done by drawing a rectangle filled
*        with the current background colour of the selected graphics
*        device.  The size of the blanked area can be controlled using
*        Parameter BLANKSIZE.  [FALSE]
*     BLANKSIZE = _REAL (Read)
*        Specifies the size of the blanked area (see Parameter BLANK).
*        A value of 1.0 results in the blanked area being just large
*        enough to contain the drawn arrows and labels.  Values larger
*        than 1.0 introduce a blank margin around the drawn arrows and
*        labels.  This parameter also specifies the size of the picture
*        stored in the graphics database.  [1.05]
*     DEVICE = DEVICE (Read)
*        The plotting device.  [Current graphics device]
*     EPOCH = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        Parameter FRAME) for a celestial co-ordinate system, then an
*        epoch value is needed to qualify it. This is the epoch at
*        which the supplied sky positions were determined. It should be
*        given as a decimal years value, with or without decimal places
*        ("1996.8" for example). Such values are interpreted as a Besselian
*        epoch if less than 1984.0 and as a Julian epoch otherwise.
*     FRAME = LITERAL (Read)
*        Specifies the co-ordinate Frame to which the drawn arrows
*        refer.  If a null (!) value is supplied, the arrows are drawn
*        parallel to the two axes which were used to annotate the
*        previously displayed picture.  If the arrows are required to be
*        parallel to the axes of some other Frame, the required Frame
*        should be specified using this parameter.  The string supplied
*        for FRAME can be one of the following options.
*
*        - A domain name such as SKY, AXIS, PIXEL, etc.
*
*        - An integer value giving the index of the required Frame.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000)
*        (see section "Sky Co-ordinate Systems" in SUN/95).
*
*        An error will be reported if a co-ordinate Frame is requested
*        which is not available in the previously displayed picture.  If
*        the selected Frame has more than two axes, the Parameter
*        USEAXIS will determine the two axes which are to be used.  [!]
*     LENGTH( 2 ) = _REAL (Read)
*        The lengths of the arrows, expressed as fractions of the
*        largest dimension of the underlying DATA picture.  If only one
*        value is supplied, both arrows will be drawn with the given
*        length.  One of the supplied values can be set to zero if only
*        a single arrow is required.  [current value]
*     OFRAME = LITERAL (Read)
*        Specifies the co-ordinate Frame in which the position of the
*        arrows will be supplied (see Parameter ORIGIN).  The following
*        Frames will always be available.
*
*        - "GRAPHICS" -- gives positions in millimetres from the
*        bottom-left corner of the plotting surface.
*
*        - "BASEPIC" -- gives positions in a normalised system in which
*        the bottom-left corner of the plotting surface is (0,0) and the
*        shortest dimension of the plotting surface has length 1.0.  The
*        scales on the two axes are equal.
*
*        - "CURPIC" -- gives positions in a normalised system in which
*        the bottom-left corner of the underlying DATA picture is (0,0)
*        and the shortest dimension of the picture has length 1.0.  The
*        scales on the two axes are equal.
*
*        - "NDC" -- gives positions in a normalised system in which the
*        bottom-left corner of the plotting surface is (0,0) and the
*        top-right corner is (1,1).
*
*        - "CURNDC" -- gives positions in a normalised system in which
*        the bottom-left corner of the underlying DATA picture is (0,0)
*        and the top-right corner is (1,1).
*
*        Additional Frames will be available, describing the
*        co-ordinates systems known to the data displayed within the
*        underlying picture.  These could include PIXEL, AXIS, SKY, for
*        instance, but the exact list will depend on the displayed data.
*        If a null value is supplied, the ORIGIN position should be
*        supplied in the Frame used to annotate the underlying picture
*        (supplying a colon ":" will display details of this
*        co-ordinate Frame).  ["CURNDC"]
*     ORIGIN = LITERAL (Read)
*        The co-ordinates at which to place the origin of the arrows,
*        in the Frame specified by Parameter OFRAME.  If a null (!)
*        value is supplied, OFRAME is ignored and the arrows are
*        situated at a default position near one of the corners, or at
*        the centre.  The supplied position can be anywhere within the
*        current picture.  An error is reported if the arrows and
*        labels cannot be drawn at any of these positions.  [!]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use for the vectors and annotated axes.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text
*        file preceded by an up-arrow character "^".  Such text files
*        should contain further comma-separated lists which will be read
*        and interpreted in the same manner.  Attribute settings are
*        applied in the order in which they occur within the list, with
*        later settings overriding any earlier settings given for the
*        same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*
*        where <name> is the name of a plotting attribute, and <value>
*        is the value to assign to the attribute.  Default values will
*        be used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!)---the initial default---is
*        supplied.  To apply changes of style to only the current
*        invocation, begin these attributes with a plus sign.  A mixture
*        of persistent and temporary style changes is achieved by
*        listing all the persistent attributes followed by a plus sign
*        then the list of temporary attributes.
*
*        See section "Plotting Attributes" in SUN/95 for a description
*        of the available attributes.  Any unrecognised attributes are
*        ignored (no error is reported).
*
*        The appearance of the arrows is controlled by the attributes
*        Colour(Axes), Width(Axes), etc. (the synonym Arrows may be
*        used in place of Axes).
*
*        The text of the label to draw against each arrow is specified
*        by the Symbol(1) and Symbol(2) attributes.  These default to
*        that corresponding attributes of the underlying picture.  The
*        appearance of these labels can be controlled using the
*        attributes Font(TextLab), Size(TextLab), etc.  The gap between
*        the end of the arrow and the corresponding label can be
*        controlled using attribute TextLabGap.  The drawing of labels
*        can be suppressed using attribute TextLab. [current value]
*     USEAXIS = GROUP (Read)
*        USEAXIS is only accessed if the co-ordinate Frame selected
*        using Parameter FRAME has more than two axes.  A group of two
*        strings should be supplied specifying the two axes to which
*        the two drawn arrows should refer.  Each axis can be specified
*        using one of the following options.
*
*        - An integer index of an axis within the current Frame of the
*        input NDF (in the range 1 to the number of axes in the current
*        Frame).
*
*        - An axis symbol string such as "RA" or "VRAD".
*
*        - A generic option where "SPEC" requests the spectral axis,
*        "TIME" selects the time axis, "SKYLON" and "SKYLAT" picks the
*        sky longitude and latitude axes respectively.  Only those axis
*        domains present are available as options.
*
*        A list of acceptable values is displayed if an illegal value
*        is supplied.  If a null (!) value is supplied, the first two
*        axes of the Frame are used.  [!]

*  Examples:
*     drawnorth
*        Draws a pair of arrows indicating the directions of the axes of
*        the previously displayed image, contour map, etc. The arrows
*        are drawn at the top left of the picture. The current values
*        for all other parameters are used.
*     drawnorth blank origin="0.5,0.5" style='TextBackColour=clear'
*        As above, but blanks out the picture area behind the arrows,
*        and positions them in the middle of the underlying DATA
*        picture. In addition, the text labels are drawn with a clear
*        background so that the underlying image can seen around the
*        text.
*     drawnorth blank blanksize=1.2 oframe=pixel origin="150,250"
*        As above, but positions the arrows at pixel co-ordinates
*        (150,250), and blanks out a larger area around the arrows.
*     drawnorth blank oframe=! origin="10:12:34,-12:23:37"
*        As above, but positions the arrows at RA=10:12:34 and
*        DEC=-12:23:37 (this assumes the underlying picture was
*        annotated with RA and DEC axes).
*     drawnorth length=[0.1,0] style='colour(arrows)=red'
*        Draws the axis-1 arrow with length equal to 0.1 of the longest
*        dimension of the underlying picture, but does not draw the
*        axis-2 arrow. Both arrows are drawn red.
*     drawnorth style='textlab=0'
*        Draws both arrows but does not draw any text labels.
*     drawnorth style="'Size(TextLab1)=2,Symbol(1)=A,Symbol(2)=B'"
*        Draws arrows with labels "A" and "B", using characters of twice
*        the default size for the label for the first axis.

*  Notes:
*     -  An error is reported if there is no existing DATA picture
*     within the current picture on the selected graphics device.
*     -  The application stores a picture in the graphics database with
*     name KEY which contains the two arrows. On exit the current
*     database picture for the chosen device reverts to the input
*     picture.

*  Copyright:
*     Copyright (C) 2002, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-2002 (DSB):
*        Original version.
*     3-SEP-2002 (DSB):
*        Modified to allow arrows to be palced anywhere within the
*        current picture.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2010 October 13 (MJC):
*        Permit temporary style attributes.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'CTM_PAR'          ! CTM_ Colour-Table Management constants
      INCLUDE 'AST_PAR'          ! AST_ constants and function declarations

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER DOM*30           ! Domain name
      DOUBLE PRECISION BBOX( 4 ) ! Box bounds
      DOUBLE PRECISION CC0( 2 )  ! Current Frame position of origin
      DOUBLE PRECISION GC0( 2 )  ! GRAPHICS Frame position of origin
      DOUBLE PRECISION GC1( 2 )  ! GRAPHICS Frame position of new origin
      INTEGER CURFRM             ! Pointer to original current Frame
      INTEGER FRM                ! Frame pointer
      INTEGER FS                 ! Results FrameSet
      INTEGER I                  ! Loop count
      INTEGER IAXIS( 2 )         ! Axes to pick from the old Current Frame
      INTEGER IBPFRM             ! Index of BASEPIC Frame
      INTEGER ICURR              ! Index of original current Frame
      INTEGER IPIC0              ! AGI id for the original current picture
      INTEGER IPICD0             ! AGI id for the existing DATA picture
      INTEGER IPICK              ! AGI id for the new KEY picture
      INTEGER IPLOT              ! Pointer to AST Plot for DATA picture
      INTEGER IPLOTC             ! Plot from current picture
      INTEGER NAX                ! No. of current Frame axes
      INTEGER NEWCUR             ! Pointer to the new Current Frame
      INTEGER NLENS              ! No. of LENGTH values supplied
      INTEGER PMAP               ! AST pointer to a PermMap
      LOGICAL BLANK              ! Blank background?
      LOGICAL GUESS              ! Find a default origin position?
      REAL ARROW                 ! Size of arrow heads
      REAL BLSIZE                ! Relative size of picture area
      REAL DX1                   ! Lower GRAPHICS X value of DATA picture
      REAL DX2                   ! Upper GRAPHICS X value of DATA picture
      REAL DY1                   ! Lower GRAPHICS Y value of DATA picture
      REAL DY2                   ! Upper GRAPHICS Y value of DATA picture
      REAL GBOX( 4 )             ! Box bounds
      REAL LENS( 2 )             ! Size of arrows
      REAL MARGIN                ! Amount to extend area by at each edge
      REAL MNSIZE                ! Size (in mm) of smallest picture dimension
      REAL MXSIZE                ! Size (in mm) of largest picture dimension
      REAL R                     ! Radius of encompassing circle
      REAL X1                    ! Lower GRAPHICS X value enclosing arrows
      REAL X2                    ! Upper GRAPHICS X value enclosing arrows
      REAL XX1                   ! Lower GRAPHICS X value enclosing arrows
      REAL XX2                   ! Upper GRAPHICS X value enclosing arrows
      REAL Y1                    ! Lower GRAPHICS Y value enclosing arrows
      REAL Y2                    ! Upper GRAPHICS Y value enclosing arrows
      REAL YY1                   ! Lower GRAPHICS Y value enclosing arrows
      REAL YY2                   ! Upper GRAPHICS Y value enclosing arrows

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Open the grahics device and see if there is an axisting DATA picture
*  with which we can attempt to align the new one. If such a picture is
*  found, a Plot is returned for it. Otherwise, an error is reported.
      CALL KPG1_PLOTA( AST__NULL, 'OLD', ' ', IPIC0, IPICD0, IPLOT,
     :                 STATUS )

* Save the bounds of this picture.
      CALL PGQWIN( DX1, DX2, DY1, DY2 )

*  In order to allow the arrows to be drawn anywhere within the original
*  current picture (as opposed to merely anywhere within the DATA picture),
*  we get the Plot fro the current picture and into it the Frames from the
*  DATA picture Plot, aligning the FrameSets in the BASEPIC Frame....

*  Get the Plot for the original current picture.
      CALL KPG1_GDGET( IPIC0, AST__NULL, .FALSE., IPLOTC, STATUS )

*  Remove all Frames except the GRAPHICS Frame and the BASEPIC Frame.
      I = 1
      DO WHILE( I .LE. AST_GETI( IPLOTC, 'NFRAME', STATUS ) )
         FRM = AST_GETFRAME( IPLOTC, I, STATUS )
         DOM = AST_GETC( FRM, 'DOMAIN', STATUS )
         IF( DOM .NE. 'BASEPIC' .AND. DOM .NE. 'GRAPHICS' ) THEN
            CALL AST_REMOVEFRAME( IPLOTC, I, STATUS )
            I = I - 1
         END IF
         CALL AST_ANNUL( FRM, STATUS )
         I = I + 1
      END DO

*  Delete the Base (GRAPHICS) Frame from the DATA picture Plot.
      CALL AST_REMOVEFRAME( IPLOT, AST_GETI( IPLOT, 'BASE', STATUS ),
     :                      STATUS )

*  Get a pointer to the original current Frame in the DATA picture plot,
*  and then make the BASEPIC Frame the current Frame.
      CURFRM = AST_GETFRAME( IPLOT, AST__CURRENT, STATUS )
      CALL KPG1_ASFFR( IPLOT, 'BASEPIC', IBPFRM, STATUS )
      CALL AST_SETI( IPLOT, 'CURRENT', IBPFRM, STATUS )

*  Add the Frames from the DATA picture plot into the current picture Plot,
*  aligning them in the BASEPIC Frame.
      CALL KPG1_ASMRG( IPLOTC, IPLOT, ' ', .TRUE., 0, STATUS )

*  Reinstate the original current Frame (set to BASPIC by the above call).
      FS = AST_FINDFRAME( IPLOTC, CURFRM, ' ', STATUS )

*  Select the Frame for which arrows are required. The selected Frame
*  becomes the current FRAME.
      CALL MSG_SETC( 'PIC', 'picture' )
      CALL KPG1_ASFRM( 'FRAME', 'EPOCH', IPLOTC, ' ', ' ', .TRUE.,
     :                 '^PIC', STATUS )

*  If the selected Frame has more than 2 axes, get the user to indicate
*  which two axes are to be used.
      NAX = AST_GETI( IPLOTC, 'NAXES', STATUS )
      IF( NAX .LT. 2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NAX', NAX )
         CALL ERR_REP( 'DRAWNORTH_ERR1', 'Selected co-ordinate Frame '//
     :                 'has ^NAX axes. Arrows can only be drawn for '//
     :                 'Frames with at least 2 axes.', STATUS )
         GO TO 999

      ELSE IF( NAX .GT. 2 ) THEN

*  Get the required number of axis selections. USe the first two axes by
*  default.
         IAXIS( 1 ) = 1
         IAXIS( 2 ) = 2
         CALL KPG1_GTAXI( 'USEAXIS', IPLOTC, 2, IAXIS, STATUS )

*  Create a new Frame by picking the selected axes from the original
*  Current Frame. This also returns a PermMap which goes from the
*  original Frame to the new one, using AST__BAD values for the
*  un-selected axes.
         NEWCUR = AST_PICKAXES( IPLOTC, 2, IAXIS, PMAP, STATUS )

*  Add this new Frame into the FrameSet. It becomes the Current Frame.
         CALL AST_ADDFRAME( IPLOTC, AST__CURRENT, PMAP, NEWCUR,
     :                      STATUS )

      END IF

*  Note the index of the current Frame in the Plot.
      ICURR = AST_GETI( IPLOTC, 'CURRENT', STATUS )

*  Allow the user to specify the Frame in which ORIGIN is to be given.
*  Any Frame in the Plot can be chosen. The selected Frame becomes the
*  Current Frame in the Plot.
      CALL MSG_SETC( 'PIC', 'picture' )
      CALL KPG1_ASFRM( 'OFRAME', 'EPOCH', IPLOTC, ' ', ' ', .TRUE.,
     :                 '^PIC', STATUS )

*  Get the position of the arrow origin. Check first that no error has
*  occurred.
      IF( STATUS .NE. SAI__OK ) GOTO 999
      CALL KPG1_GTPOS( 'ORIGIN', IPLOTC, .FALSE., CC0, GC0, STATUS )

*  If a null value was supplied for ORIGIN, annull the error and pass on
*  bad values for the origin. These cause a default origin to be found.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GC0( 1 ) = AST__BAD
         GC0( 1 ) = AST__BAD
         GUESS = .TRUE.
      ELSE
         GUESS = .FALSE.
      END IF

*  Reinstate the original current Frame.
      CALL AST_SETI( IPLOTC, 'CURRENT', ICURR, STATUS )

*  Get the other parameters defining the arrows.
      CALL PAR_GET0R( 'ARROW', ARROW, STATUS )
      CALL PAR_GET1R( 'LENGTH', 2, LENS, NLENS, STATUS )
      IF( NLENS .LT. 2 ) LENS( 2 ) = LENS( 1 )

* COnvert relative sizes to absolute sizes.
      MXSIZE = MAX( ABS( DX2 - DX1 ), ABS( DY2 - DY1 ) )
      MNSIZE = MIN( ABS( DX2 - DX1 ), ABS( DY2 - DY1 ) )
      ARROW = ARROW*MXSIZE
      LENS( 1 ) = LENS( 1 )*MXSIZE
      LENS( 2 ) = LENS( 2 )*MXSIZE

*  Get the bounds of the original current picture.
      CALL PGQWIN( X1, X2, Y1, Y2 )

*  Create a new Plot from the existing one. All the Plot attributes of
*  the new Plot are initialised to the "unset" defaults (i.e. any Plot
*  attributes which have been set in the existing Plot are forgotten). This
*  is done so that the set attributes of the existing Plot are not given
*  priority over the attribute settings in kappa_drawnorth_style.def.
      GBOX( 1 ) = X1
      GBOX( 2 ) = Y1
      GBOX( 3 ) = X2
      GBOX( 4 ) = Y2

      BBOX( 1 ) = DBLE( X1 )
      BBOX( 2 ) = DBLE( Y1 )
      BBOX( 3 ) = DBLE( X2 )
      BBOX( 4 ) = DBLE( Y2 )

      IPLOT = AST_PLOT( IPLOTC, GBOX, BBOX, ' ', STATUS )

*  Establish synonyms for AST graphical element names to be recognised
*  during the following call to KPG1_ASSET.
      CALL KPG1_ASPSY( '(ARROWS)', '(AXES)', STATUS )
      CALL KPG1_ASPSY( '(ARROW1)', '(AXIS1)', STATUS )
      CALL KPG1_ASPSY( '(ARROW2)', '(AXIS2)', STATUS )

*  Set the attributes of the Plot to give the required plotting style.
*  The plus requests support of temporary attributes.
      CALL KPG1_ASSET( 'KAPPA_DRAWNORTH', '+STYLE', IPLOTC, STATUS )

*  If we are to find a default origin, first try a position near the top
*  left corner of the underlying DATA picture. This first guess will
*  result in the arrows being within the DATA picture, but the labels may
*  not be.
      IF( GUESS ) THEN
         R = 1.3*MAX( LENS( 1 ), LENS( 2 ) )
         GC0( 1 ) = DX1 + R
         GC0( 2 ) = DY2 - R

*  Attempt to draw the arrows and labels in invisible ink, using this
*  origin. The bouding box of the plotted items is returned.
         CALL KPS1_DNRTH( .FALSE., IPLOT, LENS, ARROW, GC0,
     :                    MNSIZE, XX1, XX2, YY1, YY2, STATUS )

*  Check they were plotted.
         IF( XX2 .GT. XX1 .AND. YY2 .GT. YY1 ) THEN
            GUESS = .FALSE.

*  Modify the origin so that the bounding box does not extend outside the
*  DATA picture. Leave a 3mm margin.
            GC1( 1 ) = GC0( 1 ) - XX1 + DX1 + 3.0
            GC1( 2 ) = GC0( 2 ) - YY2 + DY2 - 3.0

*  Attempt to draw the arrows and labels in invisible ink, using this
*  modified origin.
            CALL KPS1_DNRTH( .FALSE., IPLOT, LENS, ARROW, GC1,
     :                       MNSIZE, XX1, XX2, YY1, YY2, STATUS )

*  Use this modified origin if the arrows and labels were succesfully
*  drawn.
            IF( XX2 .GT. XX1 .AND. YY2 .GT. YY1 ) THEN
               GC0( 1 ) = GC1( 1 )
               GC0( 2 ) = GC1( 2 )
            END IF

         END IF
      END IF

*  If we still need to find a default origin, try a position near the
*  bottom left corner of the underlying DATA picture.
      IF( GUESS ) THEN
         GC0( 1 ) = DX1 + R
         GC0( 2 ) = DY1 + R

*  Attempt to draw the arrows and labels in invisible ink, using this
*  origin. The bouding box of the plotted items is returned.
         CALL KPS1_DNRTH( .FALSE., IPLOT, LENS, ARROW, GC0,
     :                    MNSIZE, XX1, XX2, YY1, YY2, STATUS )

*  Check they were plotted.
         IF( XX2 .GT. XX1 .AND. YY2 .GT. YY1 ) THEN
            GUESS = .FALSE.

*  Modify the origin so that the bounding box does not extend outside the
*  DATA picture.
            GC1( 1 ) = GC0( 1 ) - XX1 + DX1 + 3.0
            GC1( 2 ) = GC0( 2 ) - YY1 + DY1 + 3.0

*  Attempt to draw the arrows and labels in invisible ink, using this
*  modified origin.
            CALL KPS1_DNRTH( .FALSE., IPLOT, LENS, ARROW, GC1,
     :                       MNSIZE, XX1, XX2, YY1, YY2, STATUS )

*  Use this modified origin if the arrows and labels were succesfully
*  drawn.
            IF( XX2 .GT. XX1 .AND. YY2 .GT. YY1 ) THEN
               GC0( 1 ) = GC1( 1 )
               GC0( 2 ) = GC1( 2 )
            END IF

         END IF
      END IF

*  If we still need to find a default origin, try a position near the
*  top right corner of the underlying DATA picture.
      IF( GUESS ) THEN
         GC0( 1 ) = DX2 - R
         GC0( 2 ) = DY2 - R

*  Attempt to draw the arrows and labels in invisible ink, using this
*  origin. The bouding box of the plotted items is returned.
         CALL KPS1_DNRTH( .FALSE., IPLOT, LENS, ARROW, GC0,
     :                    MNSIZE, XX1, XX2, YY1, YY2, STATUS )

*  Check they were plotted.
         IF( XX2 .GT. XX1 .AND. YY2 .GT. YY1 ) THEN
            GUESS = .FALSE.

*  Modify the origin so that the bounding box does not extend outside the
*  DATA picture.
            GC1( 1 ) = GC0( 1 ) - XX2 + DX2 - 3.0
            GC1( 2 ) = GC0( 2 ) - YY2 + DY2 - 3.0

*  Attempt to draw the arrows and labels in invisible ink, using this
*  modified origin.
            CALL KPS1_DNRTH( .FALSE., IPLOT, LENS, ARROW, GC1,
     :                       MNSIZE, XX1, XX2, YY1, YY2, STATUS )

*  Use this modified origin if the arrows and labels were succesfully
*  drawn.
            IF( XX2 .GT. XX1 .AND. YY2 .GT. YY1 ) THEN
               GC0( 1 ) = GC1( 1 )
               GC0( 2 ) = GC1( 2 )
            END IF

         END IF
      END IF

*  If we still need to find a default origin, try a position near the
*  bottom right corner of the underlying DATA picture.
      IF( GUESS ) THEN
         GC0( 1 ) = DX2 - R
         GC0( 2 ) = DY1 + R

*  Attempt to draw the arrows and labels in invisible ink, using this
*  origin. The bouding box of the plotted items is returned.
         CALL KPS1_DNRTH( .FALSE., IPLOT, LENS, ARROW, GC0,
     :                    MNSIZE, XX1, XX2, YY1, YY2, STATUS )

*  Check they were plotted.
         IF( XX2 .GT. XX1 .AND. YY2 .GT. YY1 ) THEN
            GUESS = .FALSE.

*  Modify the origin so that the bounding box does not extend outside the
*  DATA picture.
            GC1( 1 ) = GC0( 1 ) + DX2 - XX2 - 3.0
            GC1( 2 ) = GC0( 2 ) + DY1 - YY1 + 3.0

*  Attempt to draw the arrows and labels in invisible ink, using this
*  modified origin.
            CALL KPS1_DNRTH( .FALSE., IPLOT, LENS, ARROW, GC1,
     :                       MNSIZE, XX1, XX2, YY1, YY2, STATUS )

*  Use this modified origin if the arrows and labels were succesfully
*  drawn.
            IF( XX2 .GT. XX1 .AND. YY2 .GT. YY1 ) THEN
               GC0( 1 ) = GC1( 1 )
               GC0( 2 ) = GC1( 2 )
            END IF

         END IF
      END IF

*  If we still need to find a default origin, try the centre of the
*  underlying DATA picture.
      IF( GUESS ) THEN
         GC0( 1 ) = 0.5*( DX1 + DX2 )
         GC0( 2 ) = 0.5*( DY1 + DY2 )

*  Attempt to draw the arrows and labels in invisible ink, using this
*  origin. The bouding box of the plotted items is returned.
         CALL KPS1_DNRTH( .FALSE., IPLOT, LENS, ARROW, GC0,
     :                    MNSIZE, XX1, XX2, YY1, YY2, STATUS )

*  Check they were plotted.
         IF( XX2 .GT. XX1 .AND. YY2 .GT. YY1 ) GUESS = .FALSE.

      END IF

*  If we still need to find a default origin, report an error.
      IF( GUESS .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'DRAWNORTH_ERR2', 'Cannot find a usable '//
     :                 'default value for the ORIGIN parameter.',
     :                 STATUS )
         GO TO 999
      END IF

*  Find the bounds of the area just enclosing the arrows and labels.
*  These are returned as positions in GRAPHICS coords.
      CALL KPS1_DNRTH( .FALSE., IPLOTC, LENS, ARROW, GC0, MNSIZE,
     :                 XX1, XX2, YY1, YY2, STATUS )

*  Report an error if the supplied origin cannot be used.
      IF( ( XX2 .LT. XX1 .OR. YY2 .LT. YY1 ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'DRAWNORTH_ERR3', 'Cannot determine the axis '//
     :                 'directions at the specified origin.', STATUS )
         GO TO 999
      END IF

*  Find the relative size for the new picture, as a fraction of the box just
*  enclosing the arrows and labels.
      CALL PAR_GDR0R( 'BLANKSIZE', 1.05, 1.0, 10000.0, .FALSE., BLSIZE,
     :                STATUS )

*  Find the GRAPHICS bounds for the new picture.
      MARGIN = 0.5*( BLSIZE - 1.0 )*( XX2 - XX1 )
      XX2 = XX2 + MARGIN
      XX1 = XX1 - MARGIN

      MARGIN = 0.5*( BLSIZE - 1.0 )*( YY2 - YY1 )
      YY2 = YY2 + MARGIN
      YY1 = YY1 - MARGIN

*  Set the PGPLOT viewport to this area.
      CALL KPG1_PGCUT( XX1, XX2, YY1, YY2, STATUS )

*  Save the viewport as a new picture.
      CALL AGP_SVIEW( 'KEY', 'KAPPA_DRAWNORTH', IPICK, STATUS )

* Do we need to blank the background?
      CALL PAR_GET0L( 'BLANK', BLANK, STATUS )

* If so, clear the current viewport.
      IF( BLANK ) CALL KPG1_PGCLR( STATUS )

*  Draw the required arrows and labels.
      CALL KPS1_DNRTH( .TRUE., IPLOTC, LENS, ARROW, GC0, MNSIZE,
     :                 XX1, XX2, YY1, YY2, STATUS )

*  Report an error if the supplied origin cannot be used.
      IF( ( XX2 .LT. XX1 .OR. YY2 .LT. YY1 ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'DRAWNORTH_ERR5', 'Cannot determine the axis '//
     :                 'directions at the specified origin.', STATUS )
         GO TO 999
      END IF

*  Save the Plot with the new AGI picture.
      CALL KPG1_GDPUT( IPICK, ' ', ' ', IPLOTC, STATUS )

*  Arrive here if an error occurs.
 999  CONTINUE

*  Free resources
      CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'DRAWNORTH_ERR', 'DRAWNORTH: Error drawing '//
     :                 'arrows parallel to the axes.', STATUS )
      END IF

      END
