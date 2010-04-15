      SUBROUTINE JCMT_CONVERT (NPIXEL, COORD, MJDSTART, B1950, RA, DEC,
     :   STATUS)
*+
*  Name:
*     JCMT_CONVERT

*  Purpose:
*     Precess RA, dec from date of observation to B1950 or J2000 depending
*     on B1950 flag.

*  Language:
*     Starlink Fortran 77

*  Invocation:

*  Description:

*  Arguments:
*     NPIXEL                   = INTEGER (Given)
*        The number of RA,dec pairs
*     COORD                    = CHARACTER*(*) (Given)
*        The coordinate system of the input RA,dec pairs
*     MJDSTART                 = DOUBLE PRECISION (Given)
*        The modified Julian day at which the observation occured
*     B1950                    = LOGICAL (Given)
*        T if output coords are to be B1950, otherwise J2000
*     RA (NPIXEL)              = DOUBLE PRECISION (Given and Returned)
*        The RA of the beam position for each pixel
*     DEC (NPIXEL)             = DOUBLE PRECISION (Given and Returned)
*        The apparent dec.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: John Lightfoot (ROE)
*     {enter_new_authors_here}

*  History:
*     1-JUN-1992: Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  External functions:
      DOUBLE PRECISION SLA_EPB   ! MJD to Besselian epoch
      DOUBLE PRECISION SLA_EPJ   ! MJD to Julian epoch

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'        ! standard astronomical constants

*  Arguments Given:
      INTEGER NPIXEL
      CHARACTER*(*) COORD
      DOUBLE PRECISION MJDSTART
      LOGICAL B1950

*  Arguments Given and Returned:
      DOUBLE PRECISION RA (NPIXEL)
      DOUBLE PRECISION DEC (NPIXEL)

*  Status:
      INTEGER STATUS                ! Global status

*  Local Variables:
      INTEGER I
      DOUBLE PRECISION BCURRENT     ! Besselian epoch of observation
      DOUBLE PRECISION JCURRENT     ! Julian epoch of observation
      DOUBLE PRECISION DIGNORE      !

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

*  calculate the current epoch, and the epoch of the centre coords in
*  Julian and Besselian form

      BCURRENT = SLA_EPB (MJDSTART)
      JCURRENT = SLA_EPJ (MJDSTART)

      IF (B1950) THEN

         IF (COORD .EQ. 'FK4') THEN

*  just precess to 1950

            DO I = 1, NPIXEL
               CALL SLA_PRECES ('FK4', BCURRENT, 1950.0D0, RA(I),
     :            DEC(I))
            END DO

         ELSE IF (COORD .EQ. 'FK5') THEN

*  precess to 2000 then convert to FK4

            DO I = 1, NPIXEL
               CALL SLA_PRECES ('FK5', JCURRENT, 2000.0D0, RA(I),
     :            DEC(I))
               CALL SLA_FK54Z (RA(I), DEC(I), 1950D0, RA(I), DEC(I),
     :            DIGNORE, DIGNORE)
            END DO
         END IF

      ELSE

         IF (COORD .EQ. 'FK4') THEN

*  precess to 1950 then convert

            DO I = 1, NPIXEL
               CALL SLA_PRECES ('FK4', BCURRENT, 1950.0D0, RA(I),
     :            DEC(I))
               CALL SLA_FK45Z (RA(I), DEC(I), 1950.0D0, RA(I),
     :            DEC(I))
            END DO

         ELSE IF (COORD .EQ. 'FK5') THEN

*  precess to 2000

            DO I = 1, NPIXEL
               CALL SLA_PRECES ('FK5', JCURRENT, 2000.0D0, RA(I),
     :            DEC(I))
            END DO
         END IF
      END IF

      END

