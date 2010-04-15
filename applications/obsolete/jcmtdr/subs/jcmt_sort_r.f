      SUBROUTINE JCMT_SORT_RDATA (C3NCH, C3MXP, C3NIS, C3NSAMPLE,
     :   INDATA, NCH, X, Y, RBAD, XMIN, XSPACE, NX, YMIN, YSPACE, NY,
     :   OUTDATA, STATUS)
*+
*  Name:
*      JCMT_SORT_RDATA

*  Purpose:
*     sort real data according to X, Y arrays

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE JCMT_SORT_RDATA (C3NCH, C3MXP, C3NIS, C3NSAMPLE,
*    :   INDATA, NCH, X, Y, RBAD, XMIN, XSPACE, NX, YMIN, YSPACE, NY,
*    :   OUTDATA, STATUS)

*  Description:
*     Sorts data from the JCMT GSD file according to the positions in the
*     X, Y arrays. The input data may be multichanneled, which is
*     indicated by the value of C3NCH. The channel which is to be selected
*     is indicated by NCH. The data is put into OUTDATA at the position
*     determined by the map grid calculated by JCMT_CALCULATE_GRID. Output
*     array elements that don't get anything put into them are set RBAD.

*  Arguments:
*     C3NCH = INTEGER (Given)
*        The number of channels in the input data
*     C3MXP = INTEGER (Given)
*        The number of map points per phase (scan) in the input data
*     C3NIS = INTEGER (Given)
*        The total number of scans in the input data
*     C3NSAMPLE = INTEGER (Given)
*        The number of valid scans in the input data
*     INDATA (C3NCH, C3MXP, C3NIS) = REAL (Given)
*        The input data
*     NCH = INTEGER (Given)
*        The channel to be sorted
*     X (C3XMP, C3NIS) = REAL (Given)
*        The list of x offsets from the image centre
*     Y (C3XMP, C3NIS) = REAL (Given)
*        The list of y offsets from the image centre
*     RBAD = REAL (Given)
*        The value to which unspecified, `bad', pixels are set.
*     XMIN = REAL (Given)
*        The x offset of the first pixel on the x-axis
*     XSPACE = REAL (Given)
*        The spacing between pixels along the x-axis
*     NX = INTEGER (Given)
*        The number of x pixels
*     YMIN = REAL (Given)
*        The y offset of the first pixel on the y-axis
*     YSPACE = REAL (Given)
*        The spacing between pixels along the y axis
*     NY = INTEGER (Given)
*        The number of y pixels
*     OUTDATA (NX, NY) = REAL (Returned)
*        The sorted array of output data
*     STATUS = INTEGER (Given and returned)
*        Global status

*  Implementation Deficiencies:
*     [routine_deficiencies]...

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JAN-1990 (JBVAD::PAH):
*        Original version.
*     16-MAY-1991 (REVAD::JFL):
*        Modified to use X, Y rather than PHT
*     17-MAY-1991 (REVAD::JFL)
*        Modified to use magic values
*     11-NOV-1991 (REVAD::JFL)
*        Modified to work on only C3NSAMPLE scans to handle aborted observations
*      8-FEB-1993 (REVAD::JFL)
*        Modified to place points on grid calculated by JCMT_CALCULATE_GRID
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

      INTEGER STATUS
      INTEGER C3NCH, C3MXP, C3NIS, C3NSAMPLE
      INTEGER NCH
      REAL INDATA (C3NCH, C3MXP, C3NIS)
      REAL X (C3MXP, C3NIS), Y (C3MXP, C3NIS)
      REAL RBAD
      REAL XMIN, XSPACE, YMIN, YSPACE
      INTEGER NX, NY
      REAL OUTDATA (NX, NY)

      INTEGER I, J, IS, P
*.

      IF (STATUS .NE. SAI__OK) RETURN

*  initialise output data

      DO J = 1, NY
         DO I = 1, NX
            OUTDATA (I, J) = RBAD
         END DO
      END DO

*  do the sorting

      DO IS = 1, C3NSAMPLE
         DO P = 1, C3MXP
            IF (NX .EQ. 1) THEN
               I = 1
            ELSE
               I = NINT ((X(P,IS) - XMIN) / XSPACE) + 1
            END IF
            IF (NY .EQ. 1) THEN
               J = 1
            ELSE
               J = NINT ((Y(P,IS) - YMIN) / YSPACE) + 1
            END IF
            OUTDATA (I, J) = INDATA (NCH,P,IS)
         END DO
      END DO

      END
