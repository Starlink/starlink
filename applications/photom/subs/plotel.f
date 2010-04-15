************************************************************************

      SUBROUTINE PLOTEL ( IZONE, NE, ELLIPS )

*+
*  Name :
*     PLOTEL
*
*  Purpose :
*     This plots out the array in array ellips
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL PLOTEL( IZONE, NE, ELLIPS )
*
*  Description :
*     This plots out the array in array ellips
*
*  Arguments :
*     IZONE = INTEGER (Given)
*        Image zone
*     NE = INTEGER (Given)
*        Number of vertices in array ellipse
*     ELLIPS( 2, NE ) = REAL (Given)
*        Array of vertices of ellipse
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
*     10-OCT-1987 (NE):
*        Original version.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

*  Arguments Given :
      INTEGER IZONE
      INTEGER NE
      REAL ELLIPS( 2, NE )

*  Local Variables :
      INTEGER I, STATUS
*.

*   Initialise status
      STATUS = SAI__OK

*   Draw ellipse
      CALL SGS_SELZ ( IZONE, STATUS )
      CALL SGS_BPOLY( ELLIPS( 1, 1 ), ELLIPS( 2, 1 ) )
      DO I = 2, NE
         CALL SGS_APOLY( ELLIPS( 1, I ), ELLIPS( 2, I ) )
      ENDDO
      CALL SGS_APOLY( ELLIPS( 1, 1 ), ELLIPS( 2, 1 ) )
      CALL SGS_OPOLY

      END

* $Id$
