      SUBROUTINE KPS1_CNTKY( IPLOT, NCONT, CNTLEV, CNTUSD, FRMOFF, 
     :                       UNITS, STATUS )
*+
*  Name:
*     KPS1_CNTKY

*  Purpose:
*     Produce a key of contour heights.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CNTKY( IPLOT, NCONT, CNTLEV, CNTUSD, FRMOFF, UNITS, STATUS )

*  Description:
*     This routine plots an enumerated list of contour heights, units and 
*     a title.  The text is drawn using PGPLOT and it is located within
*     the current PGPLOT viewport. The appearance of various components
*     in the key can be controlled by setting suitable attributes in the
*     supplied AST Plot. The vertical position of the top of the key may 
*     be specified. 
*
*     Contour heights that were selected, but not actually used (because
*     they lay beyond the range of values within the sub-array), are
*     excluded from the plotted list.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        Pointer to the AST Plot. The key heading is drawn using the
*        "Title" plotting attributes (colour, font, etc). The Contour
*        indices are drawn using the "TextLab" plotting attributes. The
*        contour values are drawn using the "NumLab" plotting attributes.
*        Contour indices are formatted as axis 1 values (i.e. using
*        attributes Format(1), Digits(1), etc). Contour values are
*        formatted as axis 2 values.
*     NCONT = INTEGER (Given)
*        Number of contour heights.
*     CNTLEV( NCONT ) = REAL (Given)
*        Contour heights.
*     CNTUSD( NCONT ) = LOGICAL (Given)
*        If true the corresponding contour height has actually been
*        plotted and so should appear in the key.
*     FRMOFF = REAL (Given)
*        The fractional position in the y direction of the top of the
*        key within the viewport. Zero is the bottom, 1.0 is the top.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units of the heights.  If this is null no units line will
*        be plotted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-MAR-1998 (DSB):
*        Original version, based on CNTKEY by MJC.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IPLOT
      INTEGER NCONT
      REAL CNTLEV( NCONT )
      LOGICAL CNTUSD( NCONT )
      REAL FRMOFF
      CHARACTER UNITS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns significant length of a string

*  Local constants :
      REAL ARAT                  ! Nominal character aspect ratio
      PARAMETER ( ARAT = 1.5 )

      REAL TXTHGT                ! Nominal text height, in metres.
      PARAMETER ( TXTHGT = 0.004 )

      INTEGER LINELN             ! Max no. of characters on a single
      PARAMETER ( LINELN = 17 )  ! line of the heading.

*  Local variables :
      CHARACTER LINE*(LINELN)    ! Text string
      CHARACTER TEXT*255         ! Text string
      DOUBLE PRECISION ATTR( 20 )! Saved graphics attribute values
      INTEGER I                  ! loop counter
      INTEGER IAT                ! Line break position
      INTEGER TLEN               ! Used length of text
      REAL HGT                   ! Height for Title text
      REAL HGT1                  ! Height for NumLab text
      REAL HGT2                  ! Height for TextLab text
      REAL MXHGT                 ! Height of a key line
      REAL X                     ! X at bottom left of string
      REAL X1                    ! Lower x bound of key picture
      REAL X2                    ! Upper x bound of key picture
      REAL XCH                   ! Height of text with vertical baseline
      REAL XL                    ! X co-ordinate of left justified text
      REAL XM                    ! X extent of key picture, in metres
      REAL Y1                    ! Lower y bound of key picture
      REAL Y2                    ! Upper y bound of key picture
      REAL YC                    ! Y coord. of centre of key object
      REAL YM                    ! Y extent of key picture, in metres
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the PGPLOT viewport and window to match the current AGI picture,
*  and get the extent of the window in world coordinates, and metres.
      CALL KPG1_GDQPC( X1, X2, Y1, Y2, XM, YM, STATUS )

*  Get the current PGPLOT character heights in world coordinates.
      CALL PGQCS( 4, XCH, HGT )

*  Set the default character height. This will be over-ridden if an 
*  explicit character height has been set in the supplied Plot. HGT
*  is a value in world coordinates. If the current pgplot value would 
*  result in LINELN characters being wider than the available space 
*  (i.e. X2-X1) - assuming an aspect ratio of ARAT for each character 
*  - then the character height is reduced so that 20 characters would 
*  just fit in.
      HGT = MIN( HGT, ARAT*ABS( X2 - X1 )/LINELN )
      CALL KPG1_PGSHT( HGT, STATUS )

*  Find the PGPLOT character height in world coordinates used by the
*  supplied Plot when drawing numerical labels. The pre-existing
*  PGPLOT attribute values are saved in ATTR as the new ones are set, the
*  PGPLOT character height is then obtained, and the original attributes
*  are then re-instated.
      CALL KPG1_PGSTY( IPLOT, 'NUMLAB', .TRUE., ATTR, STATUS )
      CALL PGQCS( 4, XCH, HGT1 )
      CALL KPG1_PGSTY( IPLOT, 'NUMLAB', .FALSE., ATTR, STATUS )

*  Find the PGPLOT character height in world coordinates used by the
*  supplied Plot when drawing textual labels. 
      CALL KPG1_PGSTY( IPLOT, 'TEXTLAB', .TRUE., ATTR, STATUS )
      CALL PGQCS( 4, XCH, HGT2 )
      CALL KPG1_PGSTY( IPLOT, 'TEXTLAB', .FALSE., ATTR, STATUS )

*  Find the maximum of these 2 character heights. This is the height of
*  1 line in the list of contour values.
      MXHGT = MAX( HGT1, HGT2 )

*  Establish the plotting style used by the supplied Plot for the Title.
*  Save the current PGPLOT attribute values in ATTR.
      CALL KPG1_PGSTY( IPLOT, 'TITLE', .TRUE., ATTR, STATUS )

*  Get the corresponding PGPLOT character height in world coordinates.
      CALL PGQCS( 4, XCH, HGT )

*  Determine the vertical position for the top line of text in the key.
      YC = FRMOFF*( Y2 - Y1 ) + Y1 - HGT

*  If a title has been set in the Plot use it as the heading. 
      IF( AST_TEST( IPLOT, 'TITLE', STATUS ) ) THEN
         TEXT = AST_GETC( IPLOT, 'TITLE', STATUS )

*  If no title has been set, use a default heading.
      ELSE
         TEXT = ' '
         TEXT = 'Contour heights'
         IF( UNITS .NE. ' ' ) TEXT( LINELN + 1: ) = 'in '//UNITS
      END IF

*  Produce the heading, splitting it up into lines of no more than LINELN
*  characters.
      IAT = 0
      DO WHILE( .TRUE. ) 
         CALL CHR_LINBR( TEXT, IAT, LINE ) 
         IF( IAT .EQ. 0 ) GO TO 10

*  Display this line. Note, the X coordinate gets modified by the call to 
*  KPG1_PGTXT so use a temporary copy (XL) in order not to loose the left
*  hand X value.
         XL = X1 
         CALL KPG1_PGTXT( 0.0, LINE, XL, YC, STATUS )

*  Set the vertical position for the next line in the key.
         YC = YC - 1.3 * HGT
      END DO

 10   CONTINUE

*  Re-instate original PGPLOT attributes.
      CALL KPG1_PGSTY( IPLOT, 'TITLE', .FALSE., ATTR, STATUS )

*  Start with the first contour at the top of the key. Leave a small gap
*  between the heading an dthe first contour level.
      YC = YC - 0.8*MXHGT
      DO I = 1, NCONT

*  If contour height has been used...
         IF ( CNTUSD( I ) ) THEN

*  Convert contour number to a string, formatting it like axis 1 in the
*  supplied Plot.
            TEXT = AST_FORMAT( IPLOT, 1, DBLE( I ), STATUS )
            TLEN = CHR_LEN( TEXT )

*  Append the separator.
            TEXT( TLEN + 1 : ) = ': '
            TLEN = TLEN + 2

*  Set plotting style to mimic textual labels produced by the supplied Plot.
            CALL KPG1_PGSTY( IPLOT, 'TEXTLAB', .TRUE., ATTR, STATUS )

*  Draw the string. 
            X = X1
            CALL KPG1_PGTXT( 0.0, TEXT( : TLEN ), X, YC, STATUS )

*  Re-establish the original plotting style.
            CALL KPG1_PGSTY( IPLOT, 'TEXTLAB', .FALSE., ATTR, STATUS )

*  Convert the contour value to a string, formatting it like axis 2 in the
*  supplied Plot.
            TEXT = AST_FORMAT( IPLOT, 2, DBLE( CNTLEV( I ) ), STATUS )
            TLEN = CHR_LEN( TEXT )

*  Draw the string using the style of the numerical labels produced by the 
*  supplied Plot. Using the X value returned by the previous call to
*  KPG1_PGTXT causes the text to be appended to the previous text.
            CALL KPG1_PGSTY( IPLOT, 'NUMLAB', .TRUE., ATTR, STATUS )
            CALL KPG1_PGTXT( 0.0, TEXT( : TLEN ), X, YC, STATUS )
            CALL KPG1_PGSTY( IPLOT, 'NUMLAB', .FALSE., ATTR, STATUS )

*  Move down the picture to write next entry in the key
            YC = YC - 1.3 * MXHGT

         END IF

      END DO

*  Flush the output.
      CALL PGUPDT

      END
