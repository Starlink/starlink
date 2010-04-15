      SUBROUTINE JCMT_GETPHT (PHT, C3NPP, C3NMAP, C3NSAMPLE, X, Y,
     :   STATUS)
*+
*  Name:
*     JCMT_GETPHT

*  Purpose:
*     Copy the pointing history table into X, Y arrays.

*  Language:
*     FORTRAN
*

*  Invocation:
*     CALL JCMT_GETPHT (PHT, C3NPP, C3NMAP, C3NSAMPLE, X, Y, STATUS)

*  Description:
*     {routine_description}

*  Arguments:
*     PHT (C3NPP, C3NMAP) = REAL (Given)
*        The pointing history table
*     C3NPP = INTEGER (Given)
*        The number of items per pixel, normally 2 (x,y)
*     C3NMAP = INTEGER (Given)
*        The total number of map points.
*     C3NSAMPLE = INTEGER (Given)
*        The number of valid map points.
*     X (C3NMAP) = REAL (Returned)
*        The x offset of the pixel (cell units)
*     Y (C3NMAP) = REAL (Returned)
*        The y offset of the pixel (cell units)
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: J.Lightfoot, adapted from a similar routine by
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAY-1991: REVAD::JFL
*        Original version.
*     11-NOV-1991: REVAD::JFL
*        Modified to only look at first C3NSAMPLE points in p.h.t. to
*        allow for possibility that observation was aborted.
*     10-FEB-1993: REVAD::JFL
*        calculation of map extent removed
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER C3NPP
      INTEGER C3NMAP
      INTEGER C3NSAMPLE
      REAL PHT (C3NPP, C3NMAP)

*  Arguments Returned:
      REAL X (C3NMAP)
      REAL Y (C3NMAP)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I

*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, C3NSAMPLE
         X(I) = PHT(1,I)
         Y(I) = PHT(2,I)
      END DO

      END

