      SUBROUTINE KPS1_CLPKY( IPLOT, DATMAX, DATMIN, SPBND, SPFRM, 
     :                       LABEL, UNIT, FRMOFF, STATUS )
*+
*  Name:
*     KPS1_CLPKY

*  Purpose:
*     Produces a key describing each cell of a CLINPLOT plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CLPKY( IPLOT, DATMAX, DATMIN, SPBND, SPFRM, LABEL, UNIT,
*                      FRMOFF, STATUS )

*  Description:
*     This routine plots the range of data value and spectral axis value
*     covered by a single cell in a CLINPLOT plot. It also displays
*     details of the data value axis and spectral axis.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        Pointer to the AST Plot.  
*     DATMAX = REAL (Given)
*        The largest data value.
*     DATMIN = REAL (Given)
*        The smallest data value.
*     SPBND( 2 ) = DOUBLE PRECISION (Given)
*        The bounds of the WCS spectral axis.
*     SPFRM = INTEGER (Given)
*        A pointer to the AST Frame that describes the spectral WCS axis.
*     LABEL = CHARACTER * ( * ) (Given)
*        The Label component from the displayed NDF.
*     UNIT = CHARACTER * ( * ) (Given)
*        The Unit component from the displayed NDF.
*     FRMOFF = REAL (Given)
*        The fractional position in the y direction of the top of the
*        key within the viewport.  Zero is the bottom, 1.0 is the top.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-JUN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IPLOT
      REAL DATMAX
      REAL DATMIN
      DOUBLE PRECISION SPBND( 2 )
      INTEGER SPFRM
      CHARACTER LABEL*(*)
      CHARACTER UNIT*(*)
      REAL FRMOFF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns significant length of string

*  Local constants :
      REAL ARAT                  ! Nominal character aspect ratio
      PARAMETER ( ARAT = 1.5 )

      INTEGER LINELN             ! Max no. of characters on a single
      PARAMETER ( LINELN = 23 )  ! line of the heading.

*  Local variables :
      CHARACTER LINE*(LINELN)    ! One line text string
      CHARACTER SLABEL*80        ! Spectral label
      CHARACTER SUNIT*20         ! Spectral unit
      CHARACTER TEXT*200         ! Multi-line text string
      DOUBLE PRECISION ATTR( 20 )! Saved graphics attribute values
      INTEGER IAT                ! Line break position
      LOGICAL FIRST              ! Is this the first line?
      LOGICAL MORE               ! Draw another line?
      REAL HGT                   ! Height for text
      REAL X                     ! X at bottom left of string
      REAL X1                    ! Lower x bound of key picture
      REAL X2                    ! Upper x bound of key picture
      REAL XCH                   ! Height of text with vertical baseline
      REAL XL                    ! X co-ordinate of left justified text
      REAL XM                    ! X extent of key picture, in metres
      REAL Y1                    ! Lower y bound of key picture
      REAL Y2                    ! Upper y bound of key picture
      REAL YC                    ! Y co-ordinate of centre of key object
      REAL YM                    ! Y extent of key picture, in metres
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the PGPLOT viewport and window to match the current AGI picture,
*  and get the extent of the window in world coordinates, and metres.
      CALL KPG1_GDQPC( X1, X2, Y1, Y2, XM, YM, STATUS )

*  Get the current PGPLOT character heights in world coordinates.
      CALL PGQCS( 4, XCH, HGT )

*  Set the default character height.  This will be overridden if an 
*  explicit character height has been set in the supplied Plot. HGT
*  is a value in world coordinates.  If the current pgplot value would 
*  result in LINELN characters being wider than the available space 
*  (i.e. X2-X1) - assuming an aspect ratio of ARAT for each character 
*  - then the character height is reduced so that 20 characters would 
*  just fit in.
      HGT = MIN( HGT, ARAT*ABS( X2 - X1 )/LINELN )
      CALL KPG1_PGSHT( HGT, STATUS )

*  Establish the plotting style used by the supplied Plot for drawing
*  strings drawn with AST_TEXT. Save the current PGPLOT attribute values in 
*  ATTR.
      CALL KPG1_PGSTY( IPLOT, 'STRINGS', .TRUE., ATTR, STATUS )

*  Get the corresponding PGPLOT character height in world co-ordinates.
      CALL PGQCS( 4, XCH, HGT )

*  Determine the vertical position for the top line of text in the key.
      YC = FRMOFF*( Y2 - Y1 ) + Y1 - HGT

*  Display each line of the key.  Note, the X co-ordinate gets modified by
*  the call to KPG1_PGTXT so use a temporary copy (XL) in order not to lose
*  the left-hand X value.
      XL = X1 
      CALL KPG1_PGTXT( 0.0, 'Data value axis:', XL, YC, STATUS )
      YC = YC - 1.3 * HGT

      LINE = ' '
      IAT = 0
      CALL CHR_APPND( '  Top:', LINE, IAT )
      IAT = IAT + 1
      CALL CHR_PUTR( DATMAX, LINE, IAT )
      XL = X1 
      CALL KPG1_PGTXT( 0.0, LINE( :IAT ), XL, YC, STATUS )
      YC = YC - 1.3 * HGT

      LINE = ' '
      IAT = 0
      CALL CHR_APPND( '  Bottom:', LINE, IAT )
      IAT = IAT + 1
      CALL CHR_PUTR( DATMIN, LINE, IAT )
      XL = X1 
      CALL KPG1_PGTXT( 0.0, LINE( :IAT ), XL, YC, STATUS )
      YC = YC - 1.3 * HGT

      IF( UNIT .NE. ' ' ) THEN 
         LINE = ' '
         IAT = 0
         CALL CHR_APPND( '  Unit:', LINE, IAT )
         IAT = IAT + 1
         CALL CHR_APPND( UNIT, LINE, IAT )
         XL = X1 
         CALL KPG1_PGTXT( 0.0, LINE( :IAT ), XL, YC, STATUS )
         YC = YC - 1.3 * HGT
      END IF

      IF( LABEL .NE. ' ' ) THEN 
         TEXT = ' '
         IAT = 0
         CALL CHR_APPND( '  Label:', TEXT, IAT )
         IAT = IAT + 1
         CALL CHR_APPND( LABEL, TEXT, IAT )

*  Since Label ispotentially long, split it up into lines of no more 
*  than LINELN characters. Add an appropriate number of spaces to the
*  start of each line.
         IAT = 0
         MORE = .TRUE.
         FIRST = .TRUE.
         DO WHILE( MORE ) 
            CALL CHR_LINBR( TEXT, IAT, LINE ) 
            IF( IAT .EQ. 0 ) THEN
               MORE = .FALSE.
            ELSE 
               XL = X1 
               IF( FIRST ) THEN
                  CALL KPG1_PGTXT( 0.0, '  '//LINE, XL, YC, STATUS )
                  FIRST = .FALSE.
               ELSE 
                  CALL KPG1_PGTXT( 0.0, '    '//LINE, XL, YC, STATUS )
               END IF
               YC = YC - 1.3 * HGT
            END IF
         END DO

      END IF

      YC = YC - 1.3 * HGT

      XL = X1 
      CALL KPG1_PGTXT( 0.0, 'Spectral axis:', XL, YC, STATUS )
      YC = YC - 1.3 * HGT

      LINE = ' '
      IAT = 0
      CALL CHR_APPND( '  Left:', LINE, IAT )
      IAT = IAT + 1
      CALL CHR_PUTR( REAL( SPBND( 1 ) ), LINE, IAT )
      XL = X1 
      CALL KPG1_PGTXT( 0.0, LINE( :IAT ), XL, YC, STATUS )
      YC = YC - 1.3 * HGT

      LINE = ' '
      IAT = 0
      CALL CHR_APPND( '  Right:', LINE, IAT )
      IAT = IAT + 1
      CALL CHR_PUTR( REAL( SPBND( 2 ) ), LINE, IAT )
      XL = X1 
      CALL KPG1_PGTXT( 0.0, LINE( :IAT ), XL, YC, STATUS )
      YC = YC - 1.3 * HGT

      SUNIT = AST_GETC( SPFRM, 'Unit', STATUS )
      IF( SUNIT .NE. ' ' ) THEN 
         LINE = ' '
         IAT = 0
         CALL CHR_APPND( '  Unit:', LINE, IAT )
         IAT = IAT + 1
         CALL CHR_APPND( SUNIT, LINE, IAT )
         XL = X1 
         CALL KPG1_PGTXT( 0.0, LINE( :IAT ), XL, YC, STATUS )
         YC = YC - 1.3 * HGT
      END IF

      SLABEL = AST_GETC( SPFRM, 'Label', STATUS )
      IF( SLABEL .NE. ' ' ) THEN 
         TEXT = ' '
         IAT = 0
         CALL CHR_APPND( '  Label:', TEXT, IAT )
         IAT = IAT + 1
         CALL CHR_APPND( SLABEL, TEXT, IAT )

*  Since Label is potentially long, split it up into lines of no more 
*  than LINELN characters.
         IAT = 0
         FIRST = .TRUE.
         MORE = .TRUE.
         DO WHILE( MORE ) 
            CALL CHR_LINBR( TEXT, IAT, LINE ) 
            IF( IAT .EQ. 0 ) THEN
               MORE = .FALSE.
            ELSE 
               XL = X1 
               IF( FIRST ) THEN
                  CALL KPG1_PGTXT( 0.0, '  '//LINE, XL, YC, STATUS )
                  FIRST = .FALSE.
               ELSE 
                  CALL KPG1_PGTXT( 0.0, '    '//LINE, XL, YC, STATUS )
               END IF
               YC = YC - 1.3 * HGT
            END IF
         END DO
      END IF

*  If the spectral axis is described by a SpecFrame, include SpecFrame
*  specific things.
      IF( AST_ISASPECFRAME( SPFRM, STATUS ) ) THEN
         LINE = ' '
         IAT = 0
         CALL CHR_APPND( '  Rest frame:', LINE, IAT )
         IAT = IAT + 1
         CALL CHR_APPND( AST_GETC( SPFRM, 'StdOfRest', STATUS ), LINE, 
     :                   IAT )
         XL = X1 
         CALL KPG1_PGTXT( 0.0, LINE( :IAT ), XL, YC, STATUS )
         YC = YC - 1.3 * HGT

         IF( AST_TEST( SPFRM, 'RestFreq', STATUS ) ) THEN
            LINE = ' '
            IAT = 0
            CALL CHR_APPND( '  Rest freq:', LINE, IAT )
            IAT = IAT + 1
            CALL CHR_PUTR( AST_GETR( SPFRM, 'RestFreq', STATUS ),
     :                     LINE, IAT )
            XL = X1 
            CALL KPG1_PGTXT( 0.0, LINE( :IAT ), XL, YC, STATUS )
            YC = YC - 1.3 * HGT
            XL = X1 
            CALL KPG1_PGTXT( 0.0, '                 (GHz)', XL, YC, 
     :                       STATUS )
            YC = YC - 1.3 * HGT
         END IF
      

      END IF


*  Re-establish the original plotting style.
      CALL KPG1_PGSTY( IPLOT, 'STRINGS', .FALSE., ATTR, STATUS )

*  Flush the output.
      CALL PGUPDT

      END
