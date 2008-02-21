************************************************************************

      SUBROUTINE HEADER ( NX, NY, A, E, THETA, STATUS )

*+
*  Name :
*     HEADER
*
*  Purpose :
*     Write a header block
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL HEADER( NX, NY, A, E, THETA, STATUS )
*
*  Description :
*     Write a header block
*
*  Arguments :
*     NX = INTEGER (Given)
*        Size of data array
*     NY = INTEGER (Given)
*        Size of data array
*     A = REAL (Given)
*        Semi-major axis of elliptical aperture
*     E = REAL (Given)
*        Eccentricity of elliptical aperture
*     THETA = REAL (Given)
*        Orientation anti-clockwise from x-axis
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-FEB-1988 (NE):
*        Original version.
*     21-FEB-2008 (PWD):
*        Use simple assignment for constant strings.
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
      REAL A
      REAL E
      REAL THETA

*  Status :
      INTEGER STATUS

*  Local Variables :
      CHARACTER * 68 TEXT
*.

*   Write out the header to the terminal
      TEXT = '=================================='//
     :       '=================================='
      CALL MSG_OUT( ' ', TEXT, STATUS )

      TEXT = '         nx       ny        a        e       theta'
      CALL MSG_OUT( ' ', TEXT, STATUS )

      WRITE( TEXT, '( 2X, 2I9, F11.2, F10.3, F9.1 )' )
     :       NX, NY, A, E, THETA
      CALL MSG_OUT( ' ', TEXT, STATUS )

      CALL MSG_OUT( ' ', ' ', STATUS )

      TEXT = '           x        y      mag     magerr      sky'//
     :       '       signal code'
      CALL MSG_OUT( ' ', TEXT, STATUS )

      END

