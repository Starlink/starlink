      SUBROUTINE ECH_COPY_BYTES( COUNT, FROM, TO )
*+
*  Name:
*     ECHOMOP - ECH_COPY_BYTES

*  Purpose:
*     Copy a byte array.
*     (Used for copying from mapped memory to local).

*  Description:
*     This routine copies 'count' bytes from byte array object 'from' to
*     byte array object 'to'.

*  Invocation:
*     CALL ECH_COPY_BYTES( COUNT, FROM, TO )

*  Arguments:
*     COUNT = INTEGER (Given)
*        Number of bytes to copy.
*     FROM = BYTE (Given)
*        Source data.
*     TO = BYTE (Returned)
*        Destination data.

*  Authors:
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     29-MAR-1996 (MJC):
*       Tidy up, remove status from subroutine as not used.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER COUNT
      BYTE FROM( COUNT )

*  Arguments Returned:
      BYTE TO( COUNT )
*-

*  Local Variables:
      INTEGER I
*.
      DO I = 1, COUNT
         TO( I ) = FROM( I )
      END DO

      END
