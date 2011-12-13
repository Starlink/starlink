      SUBROUTINE ATL_CPPLA( IPLOT1, IPLOT2, FIXATE, STATUS )
*+
*  Name:
*     ATL_CPPLA

*  Purpose:
*     Copy attributes from one Plot to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_CPPLA( IPLOT1, IPLOT2, FIXATE, STATUS )

*  Description:
*     This routine copies all public attribute values from one AST Plot to
*     another AST Plot. The attributes copied are those that affect the
*     visual appearance of the Plot.

*  Arguments:
*     IPLOT1 = INTEGER (Given)
*        The source Plot.
*     IPLOT2 = INTEGER (Given)
*        The destination Plot.
*     FIXATE = LOGICAL (Given)
*        If .FALSE., then attribute values are only set in IPLOT2 if they
*        have been assigned an explicit value (i.e. are not defaulted) in
*        IPLOT1. If .TRUE., then values are set explicitly in IPLOT2 whether
*        they are default values or not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-JUN-2006 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER IPLOT1
      INTEGER IPLOT2
      LOGICAL FIXATE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER NUATTR             ! No. of unqualified attr names
      PARAMETER ( NUATTR = 13 )

      INTEGER NEATTR             ! No. of attr names qualified by element
      PARAMETER ( NEATTR = 5 )

      INTEGER NAATTR             ! No. of attr names qualified by axis
      PARAMETER ( NAATTR = 16 )

      INTEGER NELEM              ! No. of element names
      PARAMETER ( NELEM = 15 )

*  Local Variables:
      CHARACTER ANAME( NAATTR )*15 ! Attr names qualified by (axis)
      CHARACTER ATTR*40            ! Full attribute name
      CHARACTER ELEM( NELEM )*15   ! Plot element names
      CHARACTER ENAME( NEATTR )*15 ! Attr names qualified by (element)
      CHARACTER UNAME( NUATTR )*15 ! Unqualified attribute names
      INTEGER I                    ! Attribute index
      INTEGER IAT                  ! Length of attribute name
      INTEGER J                    ! Element index

*  Names of unqualified attributes.
      DATA UNAME / 'Abbrev',
     :             'Border',
     :             'Clip',
     :             'ClipOp',
     :             'DrawTitle',
     :             'Escape',
     :             'Grf',
     :             'Grid',
     :             'Invisible',
     :             'Labelling',
     :             'TickAll',
     :             'TitleGap',
     :             'Tol' /

*  Names of attributes qualified by (element).
      DATA ENAME / 'Colour',
     :             'Font',
     :             'Size',
     :             'Style',
     :             'Width' /

*  Names of attributes qualified by (axis).
      DATA ANAME / 'DrawAxes',
     :             'Edge',
     :             'Gap',
     :             'LabelAt',
     :             'LabelUnits',
     :             'LabelUp',
     :             'LogGap',
     :             'LogPlot',
     :             'LogTicks',
     :             'MajTickLen',
     :             'MinTickLen',
     :             'MinTick',
     :             'NumLab',
     :             'NumLabGap',
     :             'TextLab',
     :             'TextLabGap' /

*  Names of attributes qualified by (axis).
      DATA ELEM / '(Axis1)',
     :            '(Axis2)',
     :            '(Border)',
     :            '(Curves)',
     :            '(Grid1)',
     :            '(Grid2)',
     :            '(Markers)',
     :            '(NumLab1)',
     :            '(NumLab2)',
     :            '(Strings)',
     :            '(TextLab1)',
     :            '(TextLab2)',
     :            '(Ticks1)',
     :            '(Ticks2)',
     :            '(Title)' /

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the values of all unqualified attribute. Unless FIXATE is .TRUE.,
*  only those attributes that have explicitly been assigned a value are
*  copied (in which case we ensure attributes that are unset in IPLOT1
*  are also unset in IPLOT2).
      DO I = 1, NUATTR
         IF( FIXATE .OR. AST_TEST( IPLOT1, UNAME( I ), STATUS ) ) THEN
            CALL AST_SETC( IPLOT2, UNAME( I ),
     :                     AST_GETC( IPLOT1, UNAME( I ), STATUS ),
     :                     STATUS )
         ELSE
            CALL AST_CLEAR( IPLOT2, UNAME( I ), STATUS )
         END IF
      END DO

*  Do the same for all attribute qualified by (axis) ( i.e. (1) or (2) ).
      DO I = 1, NAATTR
         ATTR = ANAME( I )
         IAT = CHR_LEN( ATTR ) + 1

         ATTR( IAT : ) = '(1)'
         IF( FIXATE .OR. AST_TEST( IPLOT1, ATTR, STATUS ) ) THEN
            CALL AST_SETC( IPLOT2, ATTR,
     :                     AST_GETC( IPLOT1, ATTR, STATUS ),
     :                     STATUS )
         ELSE
            CALL AST_CLEAR( IPLOT2, ATTR, STATUS )
         END IF

         ATTR( IAT : ) = '(2)'
         IF( FIXATE .OR. AST_TEST( IPLOT1, ATTR, STATUS ) ) THEN
            CALL AST_SETC( IPLOT2, ATTR,
     :                     AST_GETC( IPLOT1, ATTR, STATUS ),
     :                     STATUS )
         ELSE
            CALL AST_CLEAR( IPLOT2, ATTR, STATUS )
         END IF

      END DO

*  Do the same for all attribute qualified by element name.
      DO I = 1, NEATTR
         ATTR = ENAME( I )
         IAT = CHR_LEN( ATTR ) + 1

         DO J = 1, NELEM
            ATTR( IAT : ) = ELEM( J )
            IF( FIXATE .OR. AST_TEST( IPLOT1, ATTR, STATUS ) ) THEN
               CALL AST_SETC( IPLOT2, ATTR,
     :                        AST_GETC( IPLOT1, ATTR, STATUS ),
     :                        STATUS )
            ELSE
               CALL AST_CLEAR( IPLOT2, ATTR, STATUS )
            END IF
         END DO

      END DO

      END
