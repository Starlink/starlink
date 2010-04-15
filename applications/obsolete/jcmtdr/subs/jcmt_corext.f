      SUBROUTINE JCMT_COREXTC (NPIXEL, AIRMASS, TAU, INDATA, FBAD,
     :   OUTDATA, STATUS)
*+
*  Name:
*     JCMT_COREXTC

*  Purpose:
*     Perform extinction correction

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_COREXTC (NPIXEL, AIRMASS, TAU, INDATA, FBAD, OUTDATA, STATUS)

*  Description:
*     {routine_description}

*  Arguments:
*     NPIXEL = INTEGER (Given)
*        The number of map pixels
*     AIRMASS( NPIXEL ) = REAL (Given)
*        The airmass for each pixel
*     TAU( NPIXEL ) = REAL (Given)
*        The optical depth at each pixel
*     INDATA( NPIXEL ) = REAL (Given)
*        The input map data
*     FBAD = REAL (Given)
*        Data value signifying bad data to be ignored
*     OUTDATA( NPIXEL ) = REAL (Returned)
*        The corrected map data
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1989 (JBVAD::PAH):
*        Original version.
*     21-MAY-1991 (REVAD::JFL):
*        Airmass changed to real, FBAD added to argument list, instead of
*        being include in common.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NPIXEL
      REAL AIRMASS (NPIXEL)
      REAL TAU (NPIXEL)
      REAL INDATA (NPIXEL)
      REAL FBAD

*  Arguments Returned:
      REAL OUTDATA (NPIXEL)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IPIX               ! pixel counter

*.

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

*  Loop through pixels correcting them unless bad

      DO IPIX = 1, NPIXEL
         IF (INDATA(IPIX) .NE. FBAD) THEN
            OUTDATA(IPIX) = INDATA(IPIX) * EXP(TAU(IPIX)*AIRMASS(IPIX))
         ELSE
            OUTDATA(IPIX) = FBAD
         END IF
      END DO

      END

