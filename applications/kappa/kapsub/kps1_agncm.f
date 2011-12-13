      SUBROUTINE KPS1_AGNCM( PARAM1, PARAM2, IGRP, NREG, STATUS )
*+
*  Name:
*     KPS1_AGNCM

*  Purpose:
*     Combines existing regions in the supplied group for ARDGEN

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNCM( PARAM1, PARAM2, IGRP, NREG, STATUS )

*  Description:
*     A logical operator (AND, OR, NOT, etc.) is obtained from the
*     environment using the parameter supplied in PARAM1.  The regions
*     to be combined are then obtained from the environment using
*     parameter PARAM2.  Regions are identified by indices which
*     correspond to the order in which the regions were defined.  The
*     two regions (or one region in the case of the NOT operator) are
*     then located in the supplied GRP group.  The text for the two
*     regions is copied to the end of the group, with the operator text
*     interposed between them.  The elements containing the operator and
*     the second region are indented with spaces so that they are
*     interpreted as being a continuation of the first region, and the
*     whole thing is placed between opening and closing parentheses.
*     The original regions are deleted from the returned group.

*  Arguments:
*     PARAM1 = CHARACTER * ( * ) (Given)
*        The name of the parameter to use to get the operator.
*     PARAM2 = CHARACTER * ( * ) (Given)
*        The name of the parameter to use to get the region indices to
*        operate on.
*     IGRP = INTEGER (Given and Returned)
*        The GRP identifier for the group holding the ARD descriptions.
*     NREG = INTEGER (Given and Returned)
*        The number of regions in the group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-DEC-1994 (DSB):
*        Original version.
*     1995 March 15 (MJC):
*        Brought into line with the KAPPA style (e.g. sorting the
*        variables and not using continuation lines), and shortened long
*        lines.  Fixed a bug that did caused region indices not to be in
*        increasing order.
*     18-SEP-2001 (DSB):
*        Added argument NREG.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP public constants

*  Arguments Given:
      CHARACTER PARAM1*(*)
      CHARACTER PARAM2*(*)

*  Arguments Given and Returned:
      INTEGER IGRP
      INTEGER NREG

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of string less trailing blanks

*  Local Variables:
      CHARACTER OPER*10          ! Operator string
      CHARACTER TEXT*( GRP__SZNAM ) ! An element of text from the group
      INTEGER I                  ! Current group element index
      INTEGER IGRP2              ! Temporary group identifier
      INTEGER ITEMP              ! Temporary storage
      INTEGER LOPER              ! Used length of OPER
      INTEGER LTEXT              ! Used length of TEXT
      INTEGER REG                ! Current region index
      INTEGER REGS( 2 )          ! Region indices for operands
      INTEGER SIZE               ! Number of elements in group

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Warn the user and return if there are currently no defined regions.
      IF ( NREG .EQ. 0 ) THEN
         CALL MSG_OUT( 'KPS1_AGNCM_MSG1', 'There are currently no '/
     :                 /'regions defined.', STATUS )
         GO TO 999
      END IF

*  Obtain the operator for combining the regions.  Cancel the current
*  value of the parameter, ready for next time.
      CALL PAR_CHOIC( PARAM1, 'AND', 'NOT,AND,OR,XOR,EQV', .FALSE.,
     :                OPER, STATUS )
      CALL PAR_CANCL( PARAM1, STATUS )

*  Add dots before and after the operator to conform with ARD syntax.
*  Also add a leading space so that the operator can be used as a
*  continuation line in a region description (continuation lines are
*  marked by having leading spaces).
      CALL CHR_PREFX( ' .', OPER, LOPER )
      CALL CHR_APPND( '.', OPER, LOPER )

*  Save the length of the supplied group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  Warn the user and return if there are currently insufficient defined
*  regions for the operator.  All operators require two operands except
*  for NOT which only requires one.
      IF ( OPER .NE. ' .NOT.' ) THEN

         IF ( NREG .LT. 2 ) THEN
            CALL MSG_OUT( 'KPS1_AGNCM_MSG2', 'There is currently only '/
     :                    /'one region defined.', STATUS )
            GO TO 999
         END IF

*  Obtain a pair of indices for the regions to be combined.  Cancel the
*  current value of the parameter, ready for next time.
         REGS( 1 ) = NREG - 1
         REGS( 2 ) = NREG
         CALL PAR_GDR1I( PARAM2, 2, REGS, 1, NREG, .FALSE., REGS,
     :                   STATUS )
         CALL PAR_CANCL( PARAM2, STATUS )

*  Warn the user and return if the supplied region indices are equal.
         IF ( REGS( 1 ) .EQ. REGS( 2 ) ) THEN
            CALL MSG_OUT( 'KPS1_AGNCM_MSG3', 'The same region has '/
     :                    /'been given twice.', STATUS )
            GO TO 999
         END IF

*  Ensure that the region indices are in increasing order.
         IF ( REGS( 1 ) .GT. REGS( 2 ) ) THEN
            ITEMP = REGS( 1 )
            REGS( 1 ) = REGS( 2 )
            REGS( 2 ) = ITEMP
         END IF

*  Find the index within the group of the start of the first region.
         I = 0
         REG = 0

         DO WHILE( REG .NE. REGS( 1 ) .AND. STATUS .EQ. SAI__OK )
            I = I + 1
            CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )
            IF ( TEXT( 1 : 1 ) .NE. ' ' ) REG = REG + 1
         END DO

*  Copy this text to the end of the group, putting an opening
*  parenthesis infront of it.  At the same time, erase the original
*  region by overwriting it with blank strings.
         CALL CHR_PREFX( '( ', TEXT, LTEXT )
         CALL GRP_PUT( IGRP, 1, TEXT( : LTEXT ), 0, STATUS )
         CALL GRP_PUT( IGRP, 1, ' ', I, STATUS )

*  Now copy any continuation lines (lines with one or more leading
*  spaces) for this region to the end of the group, and delete the
*  original.
         I = I + 1
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )

         DO WHILE( TEXT( 1 : 1 ) .EQ. ' ' .AND. STATUS .EQ. SAI__OK )
            CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )
            CALL GRP_PUT( IGRP, 1, ' ', I, STATUS )
            I = I + 1
            CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )
         END DO

*  The start of a new region has been reached.  Increment the region
*  index.
         REG = REG + 1

*  Insert a continuation line containing the operator.
         CALL GRP_PUT( IGRP, 1, OPER( : LOPER ), 0, STATUS )

*  Now find the start of the second region.
         DO WHILE( REG .NE. REGS( 2 ) .AND. STATUS .EQ. SAI__OK )
            I = I + 1
            CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )
            IF ( TEXT( 1 : 1 ) .NE. ' ' ) REG = REG + 1
         END DO

*  Copy this text to the end of the group, putting two leading spaces
*  in front of it so that it becomes a continuation of the current
*  (combined) region.  At the same time, erase the original region by
*  overwriting it with blank strings.
         CALL CHR_PREFX( '  ', TEXT, LTEXT )
         CALL GRP_PUT( IGRP, 1, TEXT( : LTEXT ), 0, STATUS )
         CALL GRP_PUT( IGRP, 1, ' ', I, STATUS )

*  Now copy any continuation lines (lines with one or more leading
*  spaces) for this region to the end of the group, and delete the
*  original.  Take care in case the region just copied is the last one
*  in the group.
         IF ( I .LT. SIZE ) THEN
            I = I + 1
            CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )

            DO WHILE( TEXT( 1 : 1 ) .EQ. ' ' .AND. STATUS .EQ. SAI__OK )
               CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )
               CALL GRP_PUT( IGRP, 1, ' ', I, STATUS )

               IF ( I .LT. SIZE ) THEN
                  I = I + 1
                  CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )
               ELSE
                  TEXT = 'FINISHED'
               END IF

            END DO

         END IF

*  Append a closing parenthesis to the last element in the group.
         CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
         CALL GRP_GET( IGRP, SIZE, 1, TEXT, STATUS )
         LTEXT = CHR_LEN( TEXT )
         TEXT( LTEXT + 1 : ) = ' )'
         CALL GRP_PUT( IGRP, 1, TEXT, SIZE, STATUS )

*  Reduce the number of regions by 1.
         NREG = NREG - 1

*  Now handle the case of unary operators (just .NOT. at the moment).
      ELSE

*  Obtain the index for the regions to be inverted.  Cancel the current
*  value of the parameter, ready for next time.
         REGS( 1 ) = NREG
         CALL PAR_GDR0I( PARAM2, REGS( 1 ), 1, NREG, .FALSE.,
     :                   REGS( 1 ),  STATUS )
         CALL PAR_CANCL( PARAM2, STATUS )

*  Find the index within the group of the start of the region.
         I = 0
         REG = 0

         DO WHILE( REG .NE. REGS( 1 ) .AND. STATUS .EQ. SAI__OK )
            I = I + 1
            CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )
            IF ( TEXT( 1 : 1 ) .NE. ' ' ) REG = REG + 1
         END DO

*  Copy this text to the end of the group, putting an opening
*  parenthesis and ".NOT." infront of it.  At the same time, erase the
*  original region by overwriting it with blank strings.
         CALL CHR_PREFX( '( .NOT. ', TEXT, LTEXT )
         CALL GRP_PUT( IGRP, 1, TEXT( : LTEXT ), 0, STATUS )
         CALL GRP_PUT( IGRP, 1, ' ', I, STATUS )

*  Now copy any continuation lines (lines with one or more leading
*  spaces) for this region to the end of the group, and delete the
*  original.  Take care in case the region just copied is the last one
*  in the group.
         IF ( I .LT. SIZE ) THEN
            I = I + 1
            CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )

            DO WHILE( TEXT( 1 : 1 ) .EQ. ' ' .AND. STATUS .EQ. SAI__OK )
               CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )
               CALL GRP_PUT( IGRP, 1, ' ', I, STATUS )

               IF ( I .LT. SIZE ) THEN
                  I = I + 1
                  CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )
               ELSE
                  TEXT = 'FINISHED'
               END IF

            END DO

         END IF

*  Append a closing parenthesis to the last element in the group.
         CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
         CALL GRP_GET( IGRP, SIZE, 1, TEXT, STATUS )
         LTEXT = CHR_LEN( TEXT )
         TEXT( LTEXT + 1 : ) = ' )'
         CALL GRP_PUT( IGRP, 1, TEXT, SIZE, STATUS )

      END IF

 999  CONTINUE

*  Create a copy of the group, from which blank elements have been
*  removed.
      CALL GRP_REMOV( IGRP, ' ', IGRP2, STATUS )

*  Delete the original group and return the identifier for the new
*  group.
      CALL GRP_DELET( IGRP, STATUS )
      IGRP = IGRP2

      END
