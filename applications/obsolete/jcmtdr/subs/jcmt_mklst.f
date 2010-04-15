      SUBROUTINE JCMT_MKLSTXY (SCAN_TABLE_1, C3NO_SCAN_VARS1, C3NIS,
     :   C3NSAMPLE, C6SD, C3SRT, LST, X, Y, C3MXP, STATUS)
*+
*  Name:
*     JCMT_MKLSTXY

*  Purpose:
*     Extracts and expands pixel LST and x, y from the SCAN table for
*     'on the fly' observation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_MKLSTXY (SCAN_TABLE_1, C3NO_SCAN_VARS1, C3NIS, C3NSAMPLE,
*    :   C6SD, C3SRT, LST, X, Y, C3MXP, STATUS)

*  Description:
*     takes the SCAN_TABLE_1 specification of the LST, X, Y, and DIRECTION
*     of the start of each `on the fly' ROW action and, with the value
*     of C3SRT input calculates the LST and x, y co-ordinates of the pixels
*     measured along the scan. It is assumed that x, y are the ideal
*     positions measured, in cell units. C6SD is also input and specifies
*     whether the scans are VERTICAL (in altitude) or HORIZONTAL (in azimuth).
*
*     The assumed order of the items in the scan table is hardwired.
*     An error is returned if C6SD is not one of the allowed values, or if
*     the length of a scan as specified in the scan table differs from C6MXP,
*     the number of points in each scan phase.
*
*     Only the first C3NSAMPLE scans are read.
*
*     The routine cannot handle scanning in RA, Dec

*  Arguments:
*     SCAN_TABLE_1 (C3NO_SCAN_VARS1, C3NIS) = DOUBLE PRECISION (Given)
*        the scan table
*     C3NO_SCAN_VARS1 = INTEGER (Given)
*        the number of items stored in the scan table per scan
*     C3NIS = INTEGER (Given)
*        the total number of scans intended for the observation
*     C3NSAMPLE = INTEGER (Given)
*        the number of valid scans
*     C6SD = CHARACTER*(*) (Given)
*        the direction of the individual scans; VERTICAL or HORIZONTAL
*     C3SRT = INTEGER (Given)
*        the time taken per scan (seconds)
*     LST (C3MXP, C3NIS) = DOUBLE PRECISION (Returned)
*        the LST of each pixel in HOURS
*     X (C3MXP, C3NIS) = REAL (Returned)
*        the x offset of each pixel
*     Y (C3MXP, C3NIS) = (Returned)
*        the y offset of each pixel
*     C3MXP = INTEGER (Given)
*        the number of map points per phase (scan)
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Deficiencies:

*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: J.Lightfoot, modified from a similar routine by
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAY-1991: (REVAD::JFL) : Original version.
*     11-NOV-1991: (REVAD::JFL) : Modified to read C3NSAMPLE points from
*                                 scan table to cater for aborted observations.
*     10-FEB-1993: (REVAD::JFL) : calculation of map extent removed.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER C3NO_SCAN_VARS1
      INTEGER C3NIS
      INTEGER C3NSAMPLE
      INTEGER C3MXP
      INTEGER C3SRT
      DOUBLE PRECISION SCAN_TABLE_1 (C3NO_SCAN_VARS1, C3NIS)
      CHARACTER*(*) C6SD

*  Arguments Returned:
      DOUBLE PRECISION LST (C3MXP, C3NIS)
      REAL X (C3MXP, C3NIS)
      REAL Y (C3MXP, C3NIS)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION DUT2ST    ! factor for converting from UT to
                                 ! sidereal seconds
      PARAMETER (DUT2ST = 86636.55536d0/86400)

*  Local Variables:
      INTEGER I, ISTART, IEND, INCR, PIXEL
      INTEGER LENGTH, DIRECTION
      DOUBLE PRECISION LSTSTART, XSTART, YSTART, PIXTIME
*.

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

      IF ((C6SD .EQ. 'HORIZONTAL') .OR.
     :    (C6SD .EQ. 'VERTICAL'))  THEN

         DO I = 1, C3NSAMPLE

*  read items describing the start of this scan

            LSTSTART = SCAN_TABLE_1 (1, I)
            XSTART = SCAN_TABLE_1 (3, I)
            YSTART = SCAN_TABLE_1 (4, I)
            DIRECTION = NINT(SCAN_TABLE_1(5,I))
            LENGTH = NINT(SCAN_TABLE_1(6,I))

*  check LENGTH and C3MXP match

            IF (LENGTH .NE. C3MXP) THEN
               CALL PAR_WRUSER ('JCMT_MKLSTXY - Scan length and '//
     :            'C3MXP do not match', STATUS)
               STATUS = SAI__ERROR
               RETURN
            END IF

*  calculate time per point

            PIXTIME = REAL(C3SRT) / LENGTH/ 3660.0 * DUT2ST

*  loop along scan, working out LST and x,y

            DO PIXEL = 1, C3MXP

               LST(PIXEL,I) = LSTSTART + ABS(PIXEL-1) * PIXTIME
               IF (C6SD .EQ. 'HORIZONTAL') THEN
                  X(PIXEL,I) = XSTART + DIRECTION * (PIXEL-1)
                  Y(PIXEL,I) = YSTART
               ELSE
                  X(PIXEL,I) = XSTART
                  Y(PIXEL,I) = YSTART + DIRECTION * (PIXEL-1)
               END IF

            END DO

         END DO

      ELSE

         CALL PAR_WRUSER ('JCMT_MKLSTXY - Bad scan direction C6SD',
     :      STATUS)
         STATUS = SAI__ERROR

      END IF

      END
