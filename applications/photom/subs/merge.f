************************************************************************

      SUBROUTINE MERGE ( KEY, NX1, NY1, LIST1, NX2, NY2, LIST2,
     :                   NM, MLIST )

*+
*  Name :
*     MERGE
*
*  Purpose :
*     This merges two lists sorted into descending order into one list in
*     descending order.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL MERGE( KEY, NX1, NY1, LIST1, NX2, NY2, LIST2, NM, MLIST )
*
*  Description :
*     This merges two lists sorted into descending order into one list in
*     descending order. The merge takes place by column, the key indicates
*     which column to use. The two lists must have the same number of columns.
*
*  Arguments :
*     KEY = INTEGER (Given)
*        Which column to use as key for merge
*     NX1 = INTEGER (Given)
*        Number of columns in list1
*     NY1 = INTEGER (Given)
*        Number of rows in list1
*     LIST1( NX1, NY1 ) = REAL (Given)
*        First list to be merged
*     NX2 = INTEGER (Given)
*        Number of columns in list2
*     NY2 = INTEGER (Given)
*        Number of rows in list2
*     LIST2( NX2, NY2 ) = REAL (Given)
*        Second list to be merged
*     NM = INTEGER (Given and Returned)
*        Number of rows in merged list
*     MLIST( NX1, NM ) = REAL (Returned)
*        Merged list
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-JUL-1987 (NE):
*        Original version.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      INTEGER KEY
      INTEGER NX1
      INTEGER NY1
      REAL LIST1( NX1, NY1 )
      INTEGER NX2
      INTEGER NY2
      REAL LIST2( NX2, NY2 )

*  Arguments Given and Returned :
      INTEGER NM

*  Arguments Returned :
      REAL MLIST( NX1, NM )

*  Local Variables :
      INTEGER I, J, NJ1, NJ2
*.

*   Verify input
      IF ( ( NX1 .EQ. NX2 ) .AND. ( NM .GE. NY1 + NY2 ) ) THEN

         J = 1
         NJ1 = 1
         NJ2 = 1

*   Merge both lists until one of them runs out
         DO WHILE( ( NJ1 .LE. NY1 ) .AND. ( NJ2 .LE. NY2 ) )

            IF ( LIST1( KEY, NJ1 ) .GT. LIST2( KEY, NJ2 ) ) THEN
               DO I = 1, NX1
                  MLIST( I, J ) = LIST1( I, NJ1 )
               ENDDO
               NJ1 = NJ1 + 1
            ELSE
               DO I = 1, NX2
                  MLIST( I, J ) = LIST2( I, NJ2 )
               ENDDO
               NJ2 = NJ2 + 1
            ENDIF

            J = J + 1
         ENDDO

*   Have reached the end of one list so fill mlist with the remainder
         DO WHILE ( NJ1 .LE. NY1 )
            DO I = 1, NX1
               MLIST( I, J ) = LIST1( I, NJ1 )
            ENDDO
            NJ1 = NJ1 + 1
            J = J + 1
         ENDDO

         DO WHILE ( NJ2 .LE. NY2 )
            DO I = 1, NX2
               MLIST( I, J ) = LIST2( I, NJ2 )
            ENDDO
            NJ2 = NJ2 + 1
            J = J + 1
         ENDDO

      ENDIF

      END

* $Id$
