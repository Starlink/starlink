      SUBROUTINE JCMT_CALC_OFFSETS (NPIX, RA, DEC, RACEN, DECCEN,
     :   RA_OFF, DEC_OFF, STATUS)
*+
*  Name:
*     JCMT_CALC_OFFSETS

*  Purpose:
*     To calculate tangent plane offsets of pixels relative to map
*     centre

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_CALC_OFFSETS (NPIX, RA, DEC, RACEN, DECCEN,
*    :   RA_OFF, DEC_OFF, STATUS)

*  Description:
*
*  Arguments:
*     NPIX                                  = INTEGER (Given)
*        the size of the arrays.
*     RA (NPIX)                             = DOUBLE PRECISION (Given)
*        the RA of the pixel (radians)
*     DEC (NPIX)                            = DOUBLE PRECISION (Given)
*        the dec of the pixel (radians)
*     RACEN                                 = DOUBLE PRECISION (Given)
*        the RA of the tangent point (map centre) in radians
*     DECCEN                                = DOUBLE PRECISION (Given)
*        the dec of the tangent point (radians)
*     RA_OFF (NPIX)                         = REAL (Returned)
*        the tangent plane offset in RA (arcsec)
*     DEC_OFF (NPIX)                        = REAL (Returned)
*        offset in dec (arcsec)
*     STATUS                                = INTEGER (Given and Returned)
*        The global status.
*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: John Lightfoot

*  History:
*      1-JUN-1992 (REVAD::JFL): Original version

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'                        ! for DAS2R

*  Arguments Given:
      INTEGER NPIX
      DOUBLE PRECISION RA (NPIX)
      DOUBLE PRECISION DEC (NPIX)
      DOUBLE PRECISION RACEN
      DOUBLE PRECISION DECCEN

*  Arguments Returned:
      REAL RA_OFF (NPIX)
      REAL DEC_OFF (NPIX)

*  Status:
      INTEGER STATUS                             ! Global status

*  External Functions:

*  Local Constants:

*  Local Variables:
      INTEGER IGNORE
      INTEGER I
      INTEGER SLA_STATUS
      DOUBLE PRECISION XI, ETA

*  Local data:
*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, NPIX

*  call SLA routine to calculate tangent plane offsets

         CALL SLA_DS2TP (RA(I), DEC(I), RACEN, DECCEN, XI, ETA,
     :      SLA_STATUS)
         IF (SLA_STATUS .NE. 0) THEN
            IGNORE = 0
            CALL PAR_WRUSER ('JCMT_CALC_OFFSETS - bad status '//
     :        'from SLA_DS2TP - ', IGNORE)
            IF (SLA_STATUS .EQ. 1) THEN
               IGNORE = 0
               CALL PAR_WRUSER ('  star too far from axis', IGNORE)
            ELSE IF (SLA_STATUS .EQ. 2) THEN
               IGNORE = 0
               CALL PAR_WRUSER ('  antistar too far from axis', IGNORE)
            ELSE IF (SLA_STATUS .EQ. 3) THEN
               IGNORE = 0
               CALL PAR_WRUSER ('  antistar on tangent plane', IGNORE)
            END IF
         END IF

*  convert offsets to arcsec

         RA_OFF (I) = XI / DAS2R
         DEC_OFF (I) = ETA / DAS2R

      END DO

      END
