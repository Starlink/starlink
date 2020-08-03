      SUBROUTINE KPG1_ASSHR( ASP, F, X1, X2, Y1, Y2, JUST, RJUST, IPLOT,
     :                       OK, STATUS )
*+
*  Name:
*     KPG1_ASSHR

*  Purpose:
*     Shrinks a Plot so that it covers an area which allows all
*     annotation to fit within the specified area.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASSHR( ASP, F, X1, X2, Y1, Y2, JUST, RJUST, IPLOT, OK,
*                      STATUS )

*  Description:
*     This routine creates a new Plot covering the same window as the
*     current PGPLOT window, but the window is shrunk in GRAPHICS space
*     so that all the annotation produced by AST_GRID falls within the PGPLOT
*     viewport that is current on entry. (However, see Argument RJUST
*     for a way to prevent shrinkage along an axis.)  The sizes of
*     annotations, gaps, etc. are shrunk if this is necessary in order
*     to fit the annotations within the current PGPLOT viewport.

*  Arguments:
*     ASP = LOGICAL (Given)
*        The aspect ratio of the required plotting area (i.e. excluding
*        annotation) after shrinking. If this is zero or negative, the
*        largest possible area is used for the plotting area.
*     F = REAL (Given)
*        An amount by which to extend the margins left for annotation,
*        expressed as a factor of the height or width of the plotting
*        area. For instance, a value of 0.1 could be given to fit the
*        annotation "comfortably" into the Plot. A value of 0.0 will
*        result in the annotation being hard up against the edge of the
*        plot.
*     X1 = REAL (Given)
*        The GRAPHICS X co-ordinate at the left edge of the area into
*        which the annotation is to fit.
*     X2 = REAL (Given)
*        The GRAPHICS X co-ordinate at the right edge of the area into
*        which the annotation is to fit.
*     Y1 = REAL (Given)
*        The GRAPHICS Y co-ordinate at the bottom edge of the area into
*        which the annotation is to fit.
*     Y2 = REAL (Given)
*        The GRAPHICS Y co-ordinate at the top edge of the area into
*        which the annotation is to fit.
*     JUST = CHARACTER*2 (Given)
*        Indicates the justification of the new plot within the specified
*        area.  'BL', 'BC', 'BR', 'CL', 'CC', 'CR', 'TL', 'TC' or 'TR',
*        where B is Bottom, C is Centre, T is Top, L is Left and R is
*        Right. Only used if ASP > 0. Must be upper case. If either
*        character is a space, then the corresponding value from RJUST is
*        used instead. Other unrecognised values are treated as "C".
*     RJUST( 2 ) = REAL (Given)
*        Each element is used only if the corresponding element in JUST
*        is a space. The first element gives the fractional vertical position
*        of the new plot: 0.0 means put the new plot as low as possible, 1.0
*        means put it as high as possible. The second element gives the
*        fractional horizontal position of the new plot: 0.0 means put the
*        new plot as far to the left as possible, 1.0 means put it as far
*        to the right as possible.  A negative value requests that no
*        space be allocated for annotations, tick marks, gaps etc. along
*        the respective axis.
*     IPLOT = INTEGER (Given and Returned)
*        The Plot. The supplied Plot is annulled and a new one is
*        returned in its place. The new Plot contains all the Frames of
*        the supplied Plot, but its Plot attributes are all cleared. It may
*        be necessary for the caller to re-instate these attributes.
*     OK = LOGICAL (Returned)
*        Returned .FALSE. if there was insufficient room for the required
*        Plot. No error is reported in this case, and IPLOT os returned
*        unchanged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     Copyright (C) 2016, East Asian Observatory.
*     Copyright (C) 2020 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-OCT-1999 (DSB):
*        Original version.
*     6-DEC-2016 (DSB):
*        Added argument RJUST.
*     2020 July 30 (MJC):
*        Negative RJUST values make the Plot fill the current viewport
*        along the respective axis, i.e. ignore creating room for
*        annotations, tick marks, and gaps.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      REAL ASP
      REAL F
      REAL X1
      REAL X2
      REAL Y1
      REAL Y2
      CHARACTER JUST*2
      REAL RJUST( 2 )

*  Arguments Given and Returned:
      INTEGER IPLOT

*  Arguments Returned:
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL CHR_SIMLR          ! Two strings equal apart from case?

*  Local constants :
      REAL ARAT                  ! Nominal character aspect ratio
      PARAMETER ( ARAT = 1.5 )

      REAL RED                   ! Reduction in annotation size per try
      PARAMETER ( RED = 0.8 )

      INTEGER MXTRY              ! Max. number of tries.
      PARAMETER ( MXTRY = 6 )

*  Local Variables:
      CHARACTER EDGE1*6          ! The edge to receive Axis 1 labels
      CHARACTER EDGE2*6          ! The edge to receive Axis 2 labels
      DOUBLE PRECISION BBOX( 4 ) ! Bounds of original window
      DOUBLE PRECISION XC        ! Axis 1 value
      DOUBLE PRECISION YC        ! Axis 2 value
      INTEGER I                  ! Current Frame index in old Plot
      INTEGER ICURR              ! Current Frame index in old Plot
      INTEGER NC1                ! No. of characters in an Axis-1 value
      INTEGER NC2                ! No. of characters in an Axis-2 value
      INTEGER NCURR              ! Current Frame index in new Plot
      INTEGER NPLOT              ! New Plot
      INTEGER NTRY               ! No of tries to fit it in with smaller text
      LOGICAL EXTLAB             ! Are numerical labels drawn around edge?
      LOGICAL MORE               ! Find another graphics box?
      LOGICAL NUMLB1             ! Are numerical labels drawn for Axis 1
      LOGICAL NUMLB2             ! Are numerical labels drawn for Axis 2
      LOGICAL TKALL              ! Draw tick marks on all edges?
      LOGICAL TXTLB1             ! Are textual labels drawn for Axis 1
      LOGICAL TXTLB2             ! Are textual labels drawn for Axis 2
      REAL ASP0                  ! Aspect ratio of supplied area
      REAL CEN                   ! Position of window centre
      REAL GBOX( 4 )             ! Bounds of new window
      REAL HGT                   ! Height of window
      REAL HTHGT                 ! Height of text with horizontal baseline
      REAL HTWID                 ! Char. width of text with horizontal baseline
      REAL MAXEXT                ! Maximum allowed margin extension
      REAL MBOT                  ! Width of margin at bottom for annotation
      REAL MINDIM                ! Length of minimum viewport dimension
      REAL MJTKL1                ! Value of MajTickLen(1)
      REAL MJTKL2                ! Value of MajTickLen(2)
      REAL MLEFT                 ! Width of margin at left for annotation
      REAL MNTKL1                ! Value of MinTickLen(1)
      REAL MNTKL2                ! Value of MinTickLen(2)
      REAL MRIGHT                ! Width of margin at right for annotation
      REAL MTOP                  ! Width of margin at top for annotation
      REAL NLGAP1                ! Gap between Axis 1 and numerical labels
      REAL NLGAP2                ! Gap between Axis 2 and numerical labels
      REAL NLSIZE                ! Character size for numerical labels
      REAL OBOX( 4 )             ! Bounds of old window
      REAL RF                    ! Reduction factor to fit in available area
      REAL RJ                    ! Limited real justification value
      REAL TBOX( 4 )             ! Bounds of total box including annotation
      REAL TKLEN                 ! Max (-ve) length of tick marks
      REAL TKLEN1                ! Longest exterior tick length for Axis 1
      REAL TKLEN2                ! Longest exterior tick length for Axis 2
      REAL TLGAP1                ! Gap between Axis 1 and textual labels
      REAL TLGAP2                ! Gap between Axis 2 and textual labels
      REAL TLSIZE                ! Character size for textual labels
      REAL TTGAP                 ! Value of TitleGap
      REAL TTSIZE                ! Value of Size(Title)
      REAL VTHGT                 ! Height of text with vertical baseline
      REAL VTWID                 ! Char. width of text with vertical baseline
      REAL WID                   ! Width of window
*.

*  Initialise.
      OK = .FALSE.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the nominal text height and width in PGPLOT world co-ordinates.
*  This could be different for vertical and horizontal text. Assume a
*  nominal aspect ratio of ARAT for each character.
      CALL PGQCS( 4, VTHGT, HTHGT )
      HTWID = HTHGT/ARAT
      VTWID = VTHGT/ARAT

*  Store the aspect ratio of the available area.
      ASP0 = ( Y2 - Y1 )/( X2 - X1 )

*  Transform the centre position of the specified area (in GRAPHICS
*  co-ordinates)  into the current Frame.
      CALL AST_TRAN2( IPLOT, 1, 0.5*DBLE( X1 + X2 ),
     :                0.5*DBLE( Y1 + Y2 ), .TRUE., XC, YC, STATUS )

*  Find the number of characters in a formatted Axis-1 value. This will
*  not necessarily be the same as the number of digits which actually appear
*  in the grid annotations, because the Plot Class removes any un-necessary
*  trailing zeros, for instance. But it will be a conservative estimate
*  anyway.
      NC1 = CHR_LEN( AST_FORMAT( IPLOT, 1, XC, STATUS ) )

*  Find the number of characters in a formatted Axis-2 value.
      NC2 = CHR_LEN( AST_FORMAT( IPLOT, 2, YC, STATUS ) )

*  If required, attributes controlling the size of annotations will be
*  reduced in value in an attempt to fit the annotation and plotting area
*  inside the current PGPLOT viewport. On each attempt, such attributes
*  are reduced by a factor RED. NTRY records the number of such attempts.
*  Initialise it to zero.
      NTRY = 0

*  Jump here when making a new attempt with smaller annotation.
 10   CONTINUE

*  Get the required Plot attributes.
      EXTLAB = CHR_SIMLR( AST_GETC( IPLOT, 'Labelling', STATUS ),
     :                    'EXTERIOR' )
      NUMLB1 = AST_GETL( IPLOT, 'NumLab(1)', STATUS )
      NUMLB2 = AST_GETL( IPLOT, 'NumLab(2)', STATUS )
      TXTLB1 = AST_GETL( IPLOT, 'TextLab(1)', STATUS )
      TXTLB2 = AST_GETL( IPLOT, 'TextLab(2)', STATUS )
      EDGE1 = AST_GETC( IPLOT, 'Edge(1)', STATUS )
      EDGE2 = AST_GETC( IPLOT, 'Edge(2)', STATUS )
      MJTKL1 = AST_GETR( IPLOT, 'MajTickLen(1)', STATUS )
      MJTKL2 = AST_GETR( IPLOT, 'MajTickLen(2)', STATUS )
      MNTKL1 = AST_GETR( IPLOT, 'MinTickLen(1)', STATUS )
      MNTKL2 = AST_GETR( IPLOT, 'MinTickLen(2)', STATUS )
      TKLEN1 = MIN( MJTKL1, MNTKL1 )
      TKLEN2 = MIN( MJTKL2, MNTKL2 )
      TKALL = AST_GETL( IPLOT, 'TickAll', STATUS )
      NLGAP1 = AST_GETR( IPLOT, 'NumLabGap(1)', STATUS )
      NLGAP2 = AST_GETR( IPLOT, 'NumLabGap(2)', STATUS )
      TTGAP = AST_GETR( IPLOT, 'TitleGap', STATUS )
      TLGAP1 = AST_GETR( IPLOT, 'TextLabGap(1)', STATUS )
      TLGAP2 = AST_GETR( IPLOT, 'TextLabGap(2)', STATUS )
      NLSIZE = AST_GETR( IPLOT, 'Size(NumLab)', STATUS )
      TLSIZE = AST_GETR( IPLOT, 'Size(TextLab)', STATUS )
      TTSIZE = AST_GETR( IPLOT, 'Size(Title)', STATUS )

*  Start off with the plotting area being the maximum size (as if no
*  margins were required).
      IF( ASP .LE. 0.0  ) THEN
         WID = X2 - X1
         HGT = Y2 - Y1

*  If the required window is taller than the available space, use the
*  full height, and find the corresponging width.
      ELSE IF( ASP .GT. ASP0 ) THEN
         HGT = Y2 - Y1
         WID = HGT/ASP

*  If the required window is wider than the available space, use the
*  full width, and find the corresponding height.
      ELSE
         WID = X2 - X1
         HGT = WID*ASP
      END IF

*  Anchor the required corner.
      IF( JUST( 1:1 ) .EQ. ' ' ) then
         RJ = MAX( 0.0, MIN( 1.0, RJUST( 1 ) ) )
         GBOX( 2 ) = Y1*( 1.0 - RJ ) + ( Y2 - HGT )*RJ
         GBOX( 4 ) = GBOX( 2 ) + HGT
      ELSE IF( JUST( 1:1 ) .EQ. 'B' ) THEN
         GBOX( 2 ) = Y1
         GBOX( 4 ) = Y1 + HGT
      ELSE IF( JUST( 1:1 ) .EQ. 'T' ) THEN
         GBOX( 4 ) = Y2
         GBOX( 2 ) = Y2 - HGT
      ELSE
         CEN = 0.5*( Y1 + Y2 )
         GBOX( 2 ) = CEN - 0.5*HGT
         GBOX( 4 ) = CEN + 0.5*HGT
      END IF

      IF( JUST( 2:2 ) .EQ. ' ' ) then
         RJ = MAX( 0.0, MIN( 1.0, RJUST( 2 ) ) )
         GBOX( 1 ) = X1*( 1.0 - RJ ) + ( X2 - WID)*RJ
         GBOX( 3 ) = GBOX( 1 ) + WID
      ELSE IF( JUST( 2:2 ) .EQ. 'L' ) THEN
         GBOX( 1 ) = X1
         GBOX( 3 ) = X1 + WID
      ELSE IF( JUST( 2:2 ) .EQ. 'R' ) THEN
         GBOX( 3 ) = X2
         GBOX( 1 ) = X2 - WID
      ELSE
         CEN = 0.5*( X1 + X2 )
         GBOX( 1 ) = CEN - 0.5*WID
         GBOX( 3 ) = CEN + 0.5*WID
      END IF

*  Loop round reducing the size of the plotting area until all the
*  annotation falls within the specified area. This loop only changes
*  the size of the plotting area, NOT the size of the annotation.
      MORE = .TRUE.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Store the length of the minimum dimension.
         MINDIM = MIN( ABS( GBOX( 1 ) - GBOX( 3 ) ),
     :                 ABS( GBOX( 2 ) - GBOX( 4 ) ) )

*  Find the margin required for annotation at the bottom of the plot.
*  Tick marks and numerical labels are only drawn on the bottom edge
*  if labelling is "Exterior".
         IF( EXTLAB ) THEN

*  First, find the space needed for any tick marks on the bottom edge.
            IF( CHR_SIMLR( EDGE1, 'BOTTOM' ) .OR. (
     :          CHR_SIMLR( EDGE1, 'TOP' ) .AND. TKALL ) ) THEN
               TKLEN = TKLEN1

            ELSE IF( CHR_SIMLR( EDGE2, 'BOTTOM' ) .OR. (
     :               CHR_SIMLR( EDGE2, 'TOP' ) .AND. TKALL ) ) THEN
               TKLEN = TKLEN2

            ELSE
               TKLEN = 0.0
            END IF

*  Only tick marks with negative lengths need to be taken into account
*  because positive lengths extend inwards, into the plotting area and so do
*  not contibute to the margin.
            IF( TKLEN .LT. 0.0 ) THEN
               MBOT = -MINDIM*TKLEN
            ELSE
               MBOT = 0.0
            END IF

*  If numerical labels are being drawn on the bottom edge, see how far
*  they extend from the edge of the data area.
            IF( NUMLB1 .AND. CHR_SIMLR( EDGE1, 'BOTTOM' ) ) THEN
               MBOT = MAX( MBOT, NLGAP1*MINDIM + NLSIZE*HTHGT )

            ELSE IF( NUMLB2 .AND. CHR_SIMLR( EDGE2, 'BOTTOM' ) ) THEN
               MBOT = MAX( MBOT, NLGAP2*MINDIM + NLSIZE*HTHGT )
            END IF

         END IF

*  If text labels are being drawn on the bottom edge, see how far
*  they extend. These do not depend on the nature of the labellign
*  (Exterior or Interior).
         IF( TXTLB1 .AND. CHR_SIMLR( EDGE1, 'BOTTOM' ) ) THEN
            MBOT = MBOT + MAX( 0.0, TLGAP1*MINDIM + TLSIZE*HTHGT )
         ELSE IF( TXTLB2 .AND. CHR_SIMLR( EDGE2, 'BOTTOM' ) ) THEN
            MBOT = MBOT + MAX( 0.0, TLGAP2*MINDIM + TLSIZE*HTHGT )
         END IF

*  Now do the same for the top margin.
         IF( EXTLAB ) THEN

            IF( CHR_SIMLR( EDGE1, 'TOP' ) .OR. (
     :          CHR_SIMLR( EDGE1, 'BOTTOM' ) .AND. TKALL ) ) THEN
               TKLEN = TKLEN1

            ELSE IF( CHR_SIMLR( EDGE2, 'TOP' ) .OR. (
     :               CHR_SIMLR( EDGE2, 'BOTTOM' ) .AND. TKALL ) ) THEN
               TKLEN = TKLEN2

            ELSE
               TKLEN = 0.0
            END IF

            IF( TKLEN .LT. 0.0 ) THEN
               MTOP = -MINDIM*TKLEN
            ELSE
               MTOP = 0.0
            END IF

            IF( NUMLB1 .AND. CHR_SIMLR( EDGE1, 'TOP' ) ) THEN
               MTOP = MAX( MTOP, NLGAP1*MINDIM + NLSIZE*HTHGT )
            ELSE IF( NUMLB2 .AND. CHR_SIMLR( EDGE2, 'TOP' ) ) THEN
               MTOP = MAX( MTOP, NLGAP2*MINDIM + NLSIZE*HTHGT )
            END IF

         END IF

         IF( TXTLB1 .AND. CHR_SIMLR( EDGE1, 'TOP' ) ) THEN
            MTOP = MTOP + MAX( 0.0, TLGAP1*MINDIM + TLSIZE*HTHGT )

         ELSE IF( TXTLB2 .AND. CHR_SIMLR( EDGE2, 'TOP' ) ) THEN
            MTOP = MTOP + MAX( 0.0, TLGAP2*MINDIM + TLSIZE*HTHGT )
         END IF

*  Add on space to the top margin for the title if required.
         IF( AST_GETC( IPLOT, 'TITLE', STATUS ) .NE. ' ' .AND.
     :       AST_GETL( IPLOT, 'DRAWTITLE', STATUS ) ) THEN
            MTOP = MTOP + MAX( 0.0, MINDIM*TTGAP + HTHGT*TTSIZE)
         END IF

*  Now do the same for the left margin, but taking account of the fact
*  that numerical labels on the left and right are horizontal and so extend
*  the margin by their length rather than their height.
         IF( EXTLAB ) THEN

            IF( CHR_SIMLR( EDGE1, 'LEFT' ) .OR. (
     :          CHR_SIMLR( EDGE1, 'RIGHT' ) .AND. TKALL ) ) THEN
               TKLEN = TKLEN1

            ELSE IF( CHR_SIMLR( EDGE2, 'LEFT' ) .OR. (
     :               CHR_SIMLR( EDGE2, 'RIGHT' ) .AND. TKALL ) ) THEN
               TKLEN = TKLEN2

            ELSE
               TKLEN = 0.0
            END IF

            IF( TKLEN .LT. 0.0 ) THEN
               MLEFT = -MINDIM*TKLEN
            ELSE
               MLEFT = 0.0
            END IF

            IF( NUMLB1 .AND. CHR_SIMLR( EDGE1, 'LEFT' ) ) THEN
               MLEFT = MAX( MLEFT, NLGAP1*MINDIM + NLSIZE*HTWID*NC1 )

            ELSE IF( NUMLB2 .AND. CHR_SIMLR( EDGE2, 'LEFT' ) ) THEN
               MLEFT = MAX( MLEFT, NLGAP2*MINDIM + NLSIZE*HTWID*NC2 )
            END IF

         END IF

         IF( TXTLB1 .AND. CHR_SIMLR( EDGE1, 'LEFT' ) ) THEN
            MLEFT = MLEFT + MAX( 0.0, TLGAP1*MINDIM + TLSIZE*VTHGT )
         ELSE IF( TXTLB2 .AND. CHR_SIMLR( EDGE2, 'LEFT' ) ) THEN
            MLEFT = MLEFT + MAX( 0.0, TLGAP2*MINDIM + TLSIZE*VTHGT )
         END IF

*  Now do the same for the right margin.
         IF( EXTLAB ) THEN

            IF( CHR_SIMLR( EDGE1, 'RIGHT' ) .OR. (
     :          CHR_SIMLR( EDGE1, 'LEFT' ) .AND. TKALL ) ) THEN
               TKLEN = TKLEN1

            ELSE IF( CHR_SIMLR( EDGE2, 'RIGHT' ) .OR. (
     :               CHR_SIMLR( EDGE2, 'LEFT' ) .AND. TKALL ) ) THEN
               TKLEN = TKLEN2

            ELSE
               TKLEN = 0.0
            END IF

            IF( TKLEN .LT. 0.0 ) THEN
               MRIGHT = -MINDIM*TKLEN
            ELSE
               MRIGHT = 0.0
            END IF

            IF( NUMLB1 .AND. CHR_SIMLR( EDGE1, 'RIGHT' ) ) THEN
               MRIGHT = MAX( MRIGHT, NLGAP1*MINDIM + NLSIZE*HTWID*NC1 )

            ELSE IF( NUMLB2 .AND. CHR_SIMLR( EDGE2, 'RIGHT' ) ) THEN
               MRIGHT = MAX( MRIGHT, NLGAP2*MINDIM + NLSIZE*HTWID*NC2 )
            END IF

         END IF

         IF( TXTLB1 .AND. CHR_SIMLR( EDGE1, 'RIGHT' ) ) THEN
            MRIGHT = MRIGHT + MAX( 0.0, TLGAP1*MINDIM + TLSIZE*VTHGT )
         ELSE IF( TXTLB2 .AND. CHR_SIMLR( EDGE2, 'RIGHT' ) ) THEN
            MRIGHT = MRIGHT + MAX( 0.0, TLGAP2*MINDIM + TLSIZE*VTHGT )
         END IF

*  Extend all margins by the specified fraction of the viewport size
*  to give some spaces around the edges.
         MAXEXT = MAX( 0.0, 0.5*( 0.95*ABS( GBOX( 3 ) - GBOX( 1 ) ) -
     :                            ( MLEFT + MRIGHT ) ) )
         MLEFT = MLEFT + MIN( F*ABS( GBOX( 3 ) - GBOX( 1 ) ), MAXEXT )
         MRIGHT = MRIGHT + MIN( F*ABS( GBOX( 3 ) - GBOX( 1 ) ), MAXEXT )


         MAXEXT = MAX( 0.0, 0.5*( 0.95*ABS( GBOX( 4 ) - GBOX( 2 ) ) -
     :                            ( MTOP + MBOT ) ) )
         MBOT = MBOT + MIN( F*ABS( GBOX( 4 ) - GBOX( 2 ) ), MAXEXT )
         MTOP = MTOP + MIN( F*ABS( GBOX( 4 ) - GBOX( 2 ) ), MAXEXT )

*  Find the width and height of the area including annotation.
         WID = GBOX( 3 ) - GBOX( 1 ) + MLEFT + MRIGHT
         HGT = GBOX( 4 ) - GBOX( 2 ) + MTOP + MBOT

*  Find the factor required to scale this box to fit within the available
*  space.
         RF = MIN( 1.0, MIN( ( Y2 - Y1 )/HGT, ( X2 - X1 )/WID ) )

*  Scale the width, height and margins by this value.
         WID = WID*RF
         HGT = HGT*RF

         MLEFT = MLEFT*RF
         MRIGHT = MRIGHT*RF
         MTOP = MTOP*RF
         MBOT = MBOT*RF

*  Anchor the required corner. TBOX is the box enclosing the plotting
*  area and the annotation.
         IF( JUST( 1:1 ) .EQ. ' ' ) then
            RJ = MAX( 0.0, MIN( 1.0, RJUST( 1 ) ) )
            TBOX( 2 ) = Y1*( 1.0 - RJ ) + ( Y2 - HGT )*RJ
            TBOX( 4 ) = TBOX( 2 ) + HGT
         ELSE IF( JUST( 1:1 ) .EQ. 'B' ) THEN
            TBOX( 2 ) = Y1
            TBOX( 4 ) = Y1 + HGT
         ELSE IF( JUST( 1:1 ) .EQ. 'T' ) THEN
            TBOX( 4 ) = Y2
            TBOX( 2 ) = Y2 - HGT
         ELSE
            CEN = 0.5*( Y1 + Y2 )
            TBOX( 2 ) = CEN - 0.5*HGT
            TBOX( 4 ) = CEN + 0.5*HGT
         END IF

         IF( JUST( 2:2 ) .EQ. ' ' ) then
            RJ = MAX( 0.0, MIN( 1.0, RJUST( 2 ) ) )
            TBOX( 1 ) = X1*( 1.0 - RJ ) + ( X2 - WID)*RJ
            TBOX( 3 ) = TBOX( 1 ) + WID
         ELSE IF( JUST( 2:2 ) .EQ. 'L' ) THEN
            TBOX( 1 ) = X1
            TBOX( 3 ) = X1 + WID
         ELSE IF( JUST( 2:2 ) .EQ. 'R' ) THEN
            TBOX( 3 ) = X2
            TBOX( 1 ) = X2 - WID
         ELSE
            CEN = 0.5*( X1 + X2 )
            TBOX( 1 ) = CEN - 0.5*WID
            TBOX( 3 ) = CEN + 0.5*WID
         END IF

*  Save the old graphics box.
         OBOX( 1 ) = GBOX( 1 )
         OBOX( 2 ) = GBOX( 2 )
         OBOX( 3 ) = GBOX( 3 )
         OBOX( 4 ) = GBOX( 4 )

*  Set up the new graphics box.
         GBOX( 1 ) = TBOX( 1 ) + MLEFT
         GBOX( 2 ) = TBOX( 2 ) + MBOT
         GBOX( 3 ) = TBOX( 3 ) - MRIGHT
         GBOX( 4 ) = TBOX( 4 ) - MTOP

*  Leave the loop if none of the box corners moved by more than 0.1
*  millimetre.
         MORE = .FALSE.
         DO I = 1, 4
            IF( ABS( GBOX( I ) - OBOX( I ) ) .GT. 0.1 ) THEN
               MORE = .TRUE.
            END IF
         END DO

      END DO

      IF ( JUST( 1:1 ) .EQ. ' ' .AND. RJUST( 1 ) .LT. 0.0 ) THEN
         GBOX( 2 ) = TBOX( 2 )
         GBOX( 4 ) = TBOX( 4 )
      END IF

      IF ( JUST( 2:2 ) .EQ. ' ' .AND. RJUST( 2 ) .LT. 0.0 ) THEN
         GBOX( 1 ) = TBOX( 1 )
         GBOX( 3 ) = TBOX( 3 )
      END IF

*  Check that the plotting area found above has significant width and height
*  (more than 2mm).
      OK = ( GBOX( 3 ) - GBOX( 1 ) .GE. 2.0 ) .AND.
     :     ( GBOX( 4 ) - GBOX( 2 ) .GE. 2.0 )

*  If not, we need to reduce the size of the annotation and try again.
*  Do this up to a maximum of MXTRY times. Reduce each attribute by a
*  factor of RED. If the labelling is not "Exterior", do not change the
*  values of attributes which control numerical labels and tick marks
*  since these will not be drawn in the margins.
      IF( .NOT. OK .AND. NTRY .LT. MXTRY ) THEN

         IF( EXTLAB ) THEN
            CALL AST_SETR( IPLOT, 'Size(NumLab)', NLSIZE*RED, STATUS )
            CALL AST_SETR( IPLOT, 'NumLabGap(1)', NLGAP1*RED, STATUS )
            CALL AST_SETR( IPLOT, 'NumLabGap(2)', NLGAP2*RED, STATUS )
            CALL AST_SETR( IPLOT, 'MajTickLen(1)', MJTKL1*RED, STATUS )
            CALL AST_SETR( IPLOT, 'MajTickLen(2)', MJTKL2*RED, STATUS )
            CALL AST_SETR( IPLOT, 'MinTickLen(1)', MNTKL1*RED, STATUS )
            CALL AST_SETR( IPLOT, 'MinTickLen(2)', MNTKL2*RED, STATUS )
         END IF

         CALL AST_SETR( IPLOT, 'Size(TextLab)', TLSIZE*RED, STATUS )
         CALL AST_SETR( IPLOT, 'Size(Title)', TTSIZE*RED, STATUS )
         CALL AST_SETR( IPLOT, 'TextLabGap(1)', TLGAP1*RED, STATUS )
         CALL AST_SETR( IPLOT, 'TextLabGap(2)', TLGAP2*RED, STATUS )
         CALL AST_SETR( IPLOT, 'TitleGap', TTGAP*RED, STATUS )

*  Go back and try to find a usable plotting area using smaller annotation.
         NTRY = NTRY + 1
         GO TO 10

      END IF

*  If we have got a usable plotting area...
      IF( OK ) THEN

*  The graphics area covered by a Plot cannot be changed after it has been
*  created. Therefore, since we want a Plot covering a different area to
*  the original Plot, we have to create a new Plot covering the required
*  area and then add all the original Plot Frames back into it.

*  Create the new Plot holding the Base (GRAPHICS) Frame from the supplied
*  Plot. The new Plot covers the screen area found above. This area is
*  mapped into the specified area.
         BBOX( 1 ) = DBLE( X1 )
         BBOX( 2 ) = DBLE( Y1 )
         BBOX( 3 ) = DBLE( X2 )
         BBOX( 4 ) = DBLE( Y2 )
         NPLOT = AST_PLOT( AST_GETFRAME( IPLOT, AST__BASE, STATUS ),
     :                     GBOX, BBOX, ' ', STATUS )

*  We are now in the slightly odd situation of having two Frames with
*  Domain GRAPHICS in the Plot. Note the index of the one inherited from
*  the original Plot (i.e. the Current Frame). We will remove this Frame
*  later.
         NCURR = AST_GETI( NPLOT, 'CURRENT', STATUS )

*  Add the FrameSet representing the original Plot into this new Plot,
*  using a UnitMap to connect the two corresponding GRAPHICS Frames.
*  Note, the index of the Current Frame first so that it can be
*  reinstated later.
         ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )
         CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE',
     :                                              STATUS ), STATUS )
         CALL AST_ADDFRAME( NPLOT, AST__CURRENT, AST_UNITMAP( 2, ' ',
     :                                                        STATUS ),
     :                      IPLOT, STATUS )

*  We now have three GRAPHICS Frames in the Plot! Clear the Domain of the
*  current Frame to avoid confusion.
         CALL AST_CLEAR( IPLOT, 'DOMAIN', STATUS )

*  There are now two copies of the original GRAPHICS Frame in the Plot
*  (i.e. the two Frames connected by the UnitMap). We can get rid of one.
*  Get rid of the one which was originally in the new Plot.
         CALL AST_REMOVEFRAME( NPLOT, NCURR, STATUS )

*  Re-instate the original Current Frame, allowing for the one extra Frame
*  introduced within the new Plot (there were two extra Frames but we've
*  just removed one of them).
         CALL AST_SETI( NPLOT, 'CURRENT', ICURR + 1, STATUS )

*  Annul the supplied Plot.
         CALL AST_ANNUL( IPLOT, STATUS )

*  Export the returned Plot.
         IPLOT = AST_CLONE( NPLOT, STATUS )
         CALL AST_EXPORT( IPLOT, STATUS )

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
