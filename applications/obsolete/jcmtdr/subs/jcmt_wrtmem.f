      SUBROUTINE JCMT_WRITE_MEM (LU, BINARY, NPIX, DATA, RA_POS_OFF,
     :   DEC_POS_OFF, RA_NEG_OFF, DEC_NEG_OFF, NOISE, PARALLACTIC,
     :   STATUS)
*+
*  Name:
*     JCMT_WRITE_MEM

*  Purpose:
*     To write data values out to a DBMEM format file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_WRITE_MEM (LU, BINARY, NPIX, DATA, RA_POS_OFF, DEC_POS_OFF,
*    :   RA_NEG_OFF, DEC_NEG_OFF, NOISE, PARALLACTIC, STATUS)

*  Description:
*

*  Arguments:
*     LU                     = INTEGER (Given)
*        LU of output file
*     BINARY                 = LOGICAL (Given)
*        T if output is to an unformatted file
*     NPIX                   = INTEGER (Given)
*        The number of data points to be output
*     DATA (NPIX)            = REAL (Given)
*        The data value
*     RA_POS_OFF (NPIX)      = REAL (Given)
*        The RA offset of the +ve beam position (arcsec)
*     DEC_POS_OFF (NPIX)     = REAL (Given)
*        The dec offset of the +ve beam position (arcsec)
*     RA_NEG_OFF (NPIX)      = REAL (Given)
*        The RA offset of the -ve beam position (arcsec)
*     DEC_NEG_OFF (NPIX)     = REAL (Given)
*        The dec offset of the -ve beam position (arcsec)
*     NOISE (NPIX)           = REAL (Given)
*        The noise (sigma) on the data
*     PARALLACTIC (NPIX)     = REAL (Given)
*        The parallactic angle of the datapoint at the time of observation
*        (radians)
*     STATUS                 = INTEGER (Given and returned)
*        Global status

*  Authors:
*     REVAD::JFL: John Lightfoot

*  History:
*     1-JUN-1992: Original version.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants

*  Arguments Given:
      INTEGER LU
      LOGICAL BINARY
      INTEGER NPIX
      REAL DATA (NPIX)
      REAL RA_POS_OFF (NPIX)
      REAL DEC_POS_OFF (NPIX)
      REAL RA_NEG_OFF (NPIX)
      REAL DEC_NEG_OFF (NPIX)
      REAL NOISE (NPIX)
      REAL PARALLACTIC (NPIX)

*  Arguments Returned:

*  Status:
      INTEGER STATUS                             ! Global status

*  External Functions:

*  Local Constants:

*  Local Variables:
      INTEGER I

*   local data
*.

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, NPIX
         IF (BINARY) THEN
            WRITE (LU) DATA(I), RA_POS_OFF(I), DEC_POS_OFF(I),
     :         RA_NEG_OFF(I), DEC_NEG_OFF(I), NOISE(I),
     :         PARALLACTIC(I)
         ELSE
            WRITE (LU, '(7(1PE12.4))') DATA(I), RA_POS_OFF(I),
     :         DEC_POS_OFF(I), RA_NEG_OFF(I), DEC_NEG_OFF(I), NOISE(I),
     :         PARALLACTIC(I)
         END IF
      END DO

      END
