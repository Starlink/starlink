      SUBROUTINE SCULIB_GET_FILENAME(PARAM, FILENAME, STATUS)
*+
*  Name:
*     SCULIB_GET_FILENAME

*  Purpose:
*     Find the filename associated with a parameter

*  Invocation:
*     CALL SCULIB_GET_FILENAME(PARAM, FILENAME, STATUS)

*  Description:
*     This routine finds the name of the filename associated with
*     a NDF parameter.

*  Arguments:
*     PARAM = CHAR (Given)
*       Name of the parameter
*     FILENAME = CHAR (Returned)
*       Name of the file associated with PARAM
*     STATUS = INTEGER (Given & Returned)
*       Global status

*  Implementation Status:
*     Use SUBPAR

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}
 
*  History:
*     $Id$
*     $Log$
*     Revision 1.1  1997/09/03 21:54:39  timj
*     Initial revision
*

*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions
 
*  Arguments Given:
      CHARACTER * (*) PARAM

*  Arguments Returned:
      CHARACTER * (*) FILENAME

*  Status:
      INTEGER STATUS                 ! Global status

*  Local Variables:
      INTEGER IPAR                   ! Location in the parameter system

*.

      IF (STATUS .NE. SAI__OK) RETURN

      CALL SUBPAR_FINDPAR(PARAM, IPAR, STATUS)
      CALL SUBPAR_GETNAME(IPAR, FILENAME, STATUS)

      END

