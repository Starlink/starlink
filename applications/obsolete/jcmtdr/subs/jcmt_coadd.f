      SUBROUTINE JCMT_COADD (NELM, IN, WEIGHT, FBAD, COADD,
     :   COADD_WEIGHT, STATUS)
*+
*  Name:
*     JCMT_COADD

*  Purpose:

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_COADD (NELM, IN, WEIGHT, FBAD, COADD, COADD_WEIGHT,
*    :   STATUS)

*  Description:
*
*  Arguments:
*     NELM = INTEGER (Given)
*        the size of the arrays.
*     IN (NELM) = REAL (Given)
*        the input data.
*     WEIGHT = REAL (Given)
*        the weight of the input data in the coadd.
*     FBAD = REAL (Given)
*        bad value for reals.
*     COADD (NELM) = REAL (Given and returned)
*        the coadded result.
*     COADD_WEIGHT (NELM) = REAL (Given and returned)
*        the accumulated weight of the coadded data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: John Lightfoot

*  History:
*      26-SEP-1991 (REVAD::JFL): Original version

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants

*  Arguments Given:
      INTEGER NELM
      REAL IN (NELM)
      REAL WEIGHT
      REAL FBAD

*  Arguments Given and Returned:
      REAL COADD (NELM)
      REAL COADD_WEIGHT (NELM)

*  Status:
      INTEGER STATUS                             ! Global status

*  External Functions:

*  Local Constants:

*  Local Variables:
      INTEGER I
      REAL SUM

*  Local data:
*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, NELM

         IF (IN(I) .NE. FBAD) THEN
            IF (COADD(I) .NE. FBAD) THEN
               SUM = COADD_WEIGHT(I) * COADD(I) + IN(I) * WEIGHT
               COADD_WEIGHT(I) = COADD_WEIGHT(I) + WEIGHT
               COADD(I) = SUM / COADD_WEIGHT(I)
            ELSE
               COADD(I) = IN(I)
               COADD_WEIGHT(I) = WEIGHT
            END IF
         END IF

      END DO

      END
