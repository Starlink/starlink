************************************************************************

      SUBROUTINE AGI_1FNEXT ( FRELEN, FRELIS, NEXFRE, FREEID, STATUS )

*+
*  Name:
*     AGI_1FNEXT

*  Purpose:
*     Get next member from free list.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AGI_1FNEXT( FRELEN, FRELIS, NEXFRE, FREEID, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Get the next member from the free list. If there are no more
*     free then return an error in STATUS. The next free element in
*     the free list is copied from NEXFRE into FREEID and NEXFRE is
*     updated by copying the contents of the element of FRELIS pointed
*     to by NEXFRE. For example if the contents were
*                --------------          -----------------
*                | NEXFRE | 6 |          | FRELIS(3) | 9 |
*                --------------          -----------------
*                                        .               .
*                                        -----------------
*                                        | FRELIS(6) | 3 |
*                                        -----------------
*     Then the result will be
*     --------------          --------------          -----------------
*     | FREEID | 6 |          | NEXFRE | 3 |          | FRELIS(3) | 9 |
*     --------------          --------------          -----------------
*                                                     .               .
*                                                     ------------------
*                                                     | FRELIS(6) | -2 |
*                                                     ------------------

*  Authors:
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     Aug 1988
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Arguments Given:
*     Length of the array containing the free list.
      INTEGER FRELEN


*  Arguments Given and Returned:
*     Array containing the free list
      INTEGER FRELIS( FRELEN )

*     Pointer indicating next free in free list array
      INTEGER NEXFRE


*  Arguments Returned:
*     Indicator to free member
      INTEGER FREEID


*  Status:
      INTEGER STATUS

*.


*   If there are no more free members then return an error status
      IF ( NEXFRE .LT. 0 ) THEN
         STATUS = 1

*   Else get the next free member
      ELSE
         FREEID = NEXFRE
         NEXFRE = FRELIS( NEXFRE )

*   Put a -2 into the old place. This is not strictly neccessary for
*   correct operation, but it allows the release routine to check that
*   the item has been previously given away.
         FRELIS( FREEID ) = -2

      ENDIF

      END

