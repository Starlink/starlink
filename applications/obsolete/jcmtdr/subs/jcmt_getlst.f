      SUBROUTINE JCMT_GETLSTXY (SCAN_TABLE_1, C3NO_SCAN_VARS1, C3NIS,
     :   C3NSAMPLE, LST, X, Y, STATUS)
*+
*  Name:
*     JCMT_GETLSTXY

*  Purpose:
*     Copy the LST, x, y info for each pixel from SCAN_TABLE_1 into LST
*     X and Y arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_GETLSTXY (SCAN_TABLE_1, C3NO_SCAN_VARS1, C3NIS,
*    :   C3NSAMPLE, LST, X, Y, STATUS)

*  Description:
*     {routine_description}

*  Arguments:
*     SCAN_TABLE_1 (C3NO_SCAN_VARS1, C3NIS) = DOUBLE PRECISION (Given)
*        Raw scan table from GSD file
*     C3NO_SCAN_VARS1 = INTEGER (Given)
*        Number of items per point in scan table
*     C3NIS = INTEGER (Given)
*        Number of scans in scan table
*     C3NSAMPLE = INTEGER (Given)
*        Number of valid scans in scan table
*     LST (C3NIS) = DOUBLE PRECISION (Returned)
*        The local sidereal time of each observed point
*     X (C3NIS) = REAL (Returned)
*        The x offset of each observed point
*     Y (C3NIS) = REAL (Returned)
*        The y offset of each observed point
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: J.Lightfoot, adapted from a similar routine by
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAY-1991: (REVAD::JFL): Original version.
*     11-NOV-1991: (REVAD::JFL): Modified to read only first C3NSAMPLE
*                                items from scan table to cater for case
*                                where observation was aborted.
*     10-FEB-1992: (REVAD::JFL): calculation of map extent removed
*     13-JUN-1994: (REVAD::HME): include SAE_PAR and use IMPLICIT NONE.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS
      INTEGER C3NO_SCAN_VARS1, C3NIS, C3NSAMPLE
      INTEGER I
      REAL X(C3NIS), Y(C3NIS)
      DOUBLE PRECISION SCAN_TABLE_1 (C3NO_SCAN_VARS1, C3NIS)
      DOUBLE PRECISION LST(C3NIS)
*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*  Transfer the first C3NSAMPLE values from the scan table to the appropriate
*  places

      DO I = 1, C3NSAMPLE
         LST(I) = SCAN_TABLE_1 (1, I)
         X(I) = SCAN_TABLE_1 (3, I)
         Y(I) = SCAN_TABLE_1 (4, I)
      END DO

      END

