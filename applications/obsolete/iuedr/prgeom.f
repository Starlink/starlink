      SUBROUTINE PRGEOM( STATUS )

*+
*  Name:
*     SUBROUTINE PRGEOM

*  Purpose:
*     Print geometry information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRGEOM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          06-OCT-88     IUEDR Vn. 2.0
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS        ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMGEOM'

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Avoid saying nothing.
      IF ( NOGEOM ) THEN
         GO TO 999
      END IF

*   Banner.
      CALL LINE_WCONT( '%p%2w Geometry:\\' )
      CALL PRTBUF( STATUS )

*   Type.
      CALL LINE_WRITS( '%p%4w Geometry type is ''%s''.\\', GEOMTP )
      CALL PRTBUF( STATUS )

*   Explain.
      CALL LINE_WCONT(
     :        '%p%4w Geometry is represented by a 2-D Chebyshev\\' )
      CALL LINE_WCONT( ' Polynomial.\\' )
      CALL PRTBUF( STATUS )

*   Size.
      CALL LINE_WRITI( '%p%4w Coefficients matrix has size (%i,\\',
     :                 NGTERM( 1 ) )
      CALL LINE_WRITI( '%i).\\', NGTERM( 2 ) )
      CALL PRTBUF( STATUS )

*   End.
      CALL LINE_WCONT( '%p%2w end.\\' )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

 999  CONTINUE
      END
