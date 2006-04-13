************************************************************************

      SUBROUTINE AGI_1FINIT ( FRELEN, FRELIS, NEXFRE )

*+
*  Name:
*     AGI_1FINIT

*  Purpose:
*     Initialise a free list.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AGI_1FINIT( FRELEN, FRELIS, NEXFRE )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Initialise a free list so that NEXFRE and FRELIS indicate the
*     sequence of elements from 1 to FRELEN. The free list is terminated
*     with a -1 to indicate no more free elements. The initialisation is
*     as follows
*              --------------          -----------------
*              | NEXFRE | 1 |          | FRELIS(1) | 2 |
*              --------------          -----------------
*                                      | FRELIS(2) | 3 |
*                                      -----------------
*                                      .               .
*                                      ------------------
*                                      | FRELIS(N) | -1 |
*                                      ------------------

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


*  Arguments Returned:
*     Array containing the free list
      INTEGER FRELIS( FRELEN )

*     Pointer indicating next free in free list array
      INTEGER NEXFRE


*  Local Constants:
      INTEGER I

*.


*   Make NEXFRE point to first member of free list
      NEXFRE = 1

*   Make sequence in free list array. Terminate with -1.
      DO I = 1, FRELEN - 1
         FRELIS( I ) = I + 1
      ENDDO
      FRELIS( FRELEN ) = -1

      END

