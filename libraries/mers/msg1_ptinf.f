      SUBROUTINE MSG1_PTINF( INF )
*+
*  Name:
*     MSG1_PTINF

*  Purpose:
*     Set the value of element MSGINF in the MSG_CMN common block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG1_PTINF( INF )

*  Description:
*     This routine sets the value of element MSGINF in the MSG_CMN 
*     common block. This should be used instead of directly accessing the
*     common block since access from a different shared library may result in
*     the common block value being uninitialised by the corresponding BLOCK DATA
*     module.

*  Arguments:
*     INF = INTEGER (Given)
*        The new value for MSGINF.

*  Notes:
*     - This routine attempts to execute even if an error has previous
*     occurred.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-JUL-2004 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
      IMPLICIT NONE                     
      INCLUDE 'MSG_CMN'                 
      INTEGER INF

      MSGINF = INF

      END
