************************************************************************

      SUBROUTINE CLGRID ( NX, NY, GRID, NXL, NXH, NYL, NYH )

*+
*  Name :
*     CLGRID
*
*  Purpose :
*     This clears the grid array between the limits nxl, nxh, nyl, nyh
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     SUBROUTINE CLGRID( NX, NY, GRID, NXL, NXH, NYL, NYH )
*
*  Description :
*     This clears the grid array between the limits nxl, nxh, nyl, nyh
*
*  Arguments :
*     NX = INTEGER (Given)
*        X dimension of grid array
*     NY = INTEGER (Given)
*        Y dimension of grid array
*     GRID( NX, NY ) = REAL (Returned)
*        Work space array
*     NXL = INTEGER (Given)
*        Lower x limit
*     NXH = INTEGER (Given)
*        Upper x limit
*     NYL = INTEGER (Given)
*        Lower y limit
*     NYH = INTEGER (Given)
*        Upper y limit
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
*     10-NOV-1987 (NE):
*        Original version.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      INTEGER NX
      INTEGER NY
      INTEGER NXL
      INTEGER NXH
      INTEGER NYL
      INTEGER NYH

*  Arguments Returned :
      REAL GRID( NX, NY )

*  Local Variables :
      INTEGER I, J
*.

      DO J = NYL, NYH
         DO I = NXL, NXH
            GRID( I, J ) = 0.0
         ENDDO
      ENDDO

      END

* $Id$
