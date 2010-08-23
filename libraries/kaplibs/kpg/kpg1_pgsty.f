      SUBROUTINE KPG1_PGSTY( IPLOT, ELEM, SET, ATTRS, STATUS )
*+
*  Name:
*     KPG1_PGSTY

*  Purpose:
*     Establishes values for graphics attributes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGSTY( IPLOT, ELEM, SET, ATTRS, STATUS )

*  Description:
*     This routine establishes current PGPLOT attributes so that they
*     correspond to the values for a named graphics element stored in the
*     supplied Plot (see SUN/210). Only attributes which have values
*     explicitly set in the Plot are changed. If no value has been set
*     for a Plot attribute, the corresponding PGPLOT attribute is left
*     unchanged.
*
*     If SET is supplied .TRUE., then the PGPLOT attributes for the
*     specified graphics element are extracted from the supplied Plot and
*     made active. The previously active values are returned in ATTRS. If
*     SET is supplied .FALSE., the values supplied in ATTRS are made current
*     (in this case ATTRS is returned unchanged).

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer to a Plot.
*     ELEM = CHARACTER * ( * ) (Given)
*        The name of an AST graphics element.
*     SET = LOGICAL (Given)
*        Should the Plot values be made curent? Otherwise the values in
*        ATTRS are made current.
*     ATTS( * ) = DOUBLE PRECISION (Given and Returned)
*        On entry, the attribute values to set if SET is .FALSE., These
*        should have been obtained from a previous call to this routine.
*        On exit, the attribute values current on entry to this routine
*        are returned (unless SET is .FALSE. in which case the supplied values
*        are returned unchanged). This array should have at least 5 elements.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-MAR-1998 (DSB):
*        Original version.
*     5-JUN-2006 (DSB):
*        Correct prologue description of ATTRS.
*     {enter_further_changes_here}

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
      CHARACTER ELEM*(*)
      LOGICAL SET

*  Arguments Given and Returned:
      DOUBLE PRECISION ATTRS( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER GRF__TEXT          ! Values identifying graphics primatives
      PARAMETER ( GRF__TEXT = 0 )! (see grf.h)

      INTEGER GRF__LINE
      PARAMETER ( GRF__LINE = 1 )

      INTEGER GRF__MARK
      PARAMETER ( GRF__MARK = 2 )


      INTEGER GRF__NATTR          ! The number of graphics attributes
      PARAMETER ( GRF__NATTR = 5 )! (see grf.h)

*  Local Variables:
      CHARACTER AT*30            ! Supplied graphics element name in parenthesise
      CHARACTER ATT*50           ! Complete attribute name
      CHARACTER ATTNAM( GRF__NATTR )*6 ! Plot attribute names
      DOUBLE PRECISION ATTV      ! Plot attribute value
      INTEGER ATTLEN( GRF__NATTR )     ! Length of Plot attribute names
      INTEGER I                  ! Attribute index (see AST include file grf.h)
      INTEGER IAT                ! Significant length of AT
      INTEGER ISTAT              ! GRF_ATTR local status
      INTEGER PRIM               ! The sort of graphics primative being drawn
      LOGICAL USE                ! Use the value?

*  Local Data. The index of each attribute name in these arrays must
*  correspond to the symbolic integer values used to represent the
*  attribute in the grf_kappa.c file (see grf.h).
      DATA ATTNAM / 'STYLE', 'WIDTH', 'SIZE', 'FONT', 'COLOUR' /,
     :     ATTLEN /       5,      5,       4,     4,         6 /

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the type of object being drawn; lines, markers or text.
      IF( ELEM .EQ. 'BORDER' .OR.
     :    ELEM .EQ. 'GRID' .OR.
     :    ELEM .EQ. 'GRID1' .OR.
     :    ELEM .EQ. 'GRID2' .OR.
     :    ELEM .EQ. 'CURVES' .OR.
     :    ELEM .EQ. 'TICKS' .OR.
     :    ELEM .EQ. 'TICKS1' .OR.
     :    ELEM .EQ. 'TICKS2' .OR.
     :    ELEM .EQ. 'AXES' .OR.
     :    ELEM .EQ. 'AXIS1' .OR.
     :    ELEM .EQ. 'AXIS2' ) THEN
         PRIM = GRF__LINE

      ELSE IF( ELEM .EQ. 'NUMLAB' .OR.
     :    ELEM .EQ. 'NUMLAB1' .OR.
     :    ELEM .EQ. 'NUMLAB2' .OR.
     :    ELEM .EQ. 'TEXTLAB' .OR.
     :    ELEM .EQ. 'TEXTLAB1' .OR.
     :    ELEM .EQ. 'TEXTLAB2' .OR.
     :    ELEM .EQ. 'TITLE' .OR.
     :    ELEM .EQ. 'STRINGS' ) THEN
         PRIM = GRF__TEXT

      ELSE IF( ELEM .EQ. 'MARKERS' ) THEN
         PRIM = GRF__MARK

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ELEM', ELEM )
         CALL ERR_REP( 'KPG1_PGSTY_1', 'KPG1_PGSTY: Unknown AST '//
     :                 'graphical element name ''^ELEM'' supplied '//
     :                 '(programming error).', STATUS )
         GO TO 999
      END IF

*  Enclose the element name in parenthesise
      AT = '('
      IAT = 1
      CALL CHR_APPND( ELEM, AT, IAT )
      CALL CHR_APPND( ')', AT, IAT )

*  Do each graphics attribute.
      DO I = 1, GRF__NATTR

*  Form the complete attribute name (eg"STYLE(BORDER)" ).
         ATT = ATTNAM( I )( : ATTLEN( I ) )//AT( : IAT )

*  If the Plot values are being made current...
         IF( SET ) THEN

*  If the Plot attribute has been set explicitly (i.e. not defaulted), use it.
            USE = AST_TEST( IPLOT, ATT, STATUS )
            ATTV = AST_GETD( IPLOT, ATT, STATUS )
            CALL GRF_GATTR( I - 1, USE, ATTV, ATTRS( I ), PRIM, ISTAT )

*  Otherwise, make the value supplied in ATTRS the current active PGPLOT value.
*  Discard the old value.
         ELSE
            USE = ( ATTRS( I ) .NE. AST__BAD )
            CALL GRF_GATTR( I - 1, USE, ATTRS( I ), ATTV, PRIM, ISTAT )
         END IF

*  Report an error if anything went wrong.
         IF( ISTAT .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'ATT', ATTNAM( I ) )
            CALL MSG_SETC( 'ELEM', ELEM )
            CALL ERR_REP( 'KPG1_PGSTY_1', 'An error occurred in '//
     :                    'GRF_GATTR while setting the PGPLOT ^ATT '//
     :                    'attribute to mimic the AST ^ELEM value.',
     :                    STATUS )
            GO TO 999
         END IF

      END DO

 999  CONTINUE

      END
