      SUBROUTINE KPS1_AGNCM( PARAM1, PARAM2, IGRP, STATUS )
*+
*  Name:
*     KPS1_AGNCM

*  Purpose:
*     Combines existing regions in the supplied group for ARDGEN

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNCM( PARAM1, PARAM2, IGRP, STATUS )

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
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP public constants
      
*  Arguments Given:
      CHARACTER * ( * ) PARAM1 
      CHARACTER * ( * ) PARAM2 
      
*  Arguments Given and Returned:
      INTEGER IGRP
      
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of string less trailing blanks

*  Local Variables:
      INTEGER I                  ! Current group element index
      INTEGER IGRP2              ! Temporary group identifier
      INTEGER ITEMP              ! Temporary storage
      INTEGER LOPER              ! Used length of OPER
      INTEGER LTEXT              ! Used length of TEXT
      INTEGER NREG               ! Number of defined regions
      CHARACTER * ( 10 ) OPER    ! Operator string
      INTEGER REG                ! Current region index
      INTEGER REGS( 2 )          ! Region indices for operands
      INTEGER SIZE               ! Number of elements in group
      CHARACTER * ( GRP__SZNAM ) TEXT ! An element of text from the
                                 ! group

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the number of regions currently defined.  Each region may take
*  several lines of text to describe.  The first line for each region
*  starts at character 1, whereas subsequent lines start with one or
*  more spaces.  Therefore, only count lines with no leading spaces.
      NREG = 0
      
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )      
      DO I = 1, SIZE
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )
         IF ( TEXT( 1 : 1 ) .NE. ' ' ) NREG = NREG + 1
      END DO

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
