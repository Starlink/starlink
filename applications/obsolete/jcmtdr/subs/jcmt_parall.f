      SUBROUTINE JCMT_PARALLACTIC (NPIXEL, RA_POS, DEC_POS, RA_NEG,
     :   DEC_NEG, LST, LAT, ETA, STATUS)
*+
*  Name:
*     JCMT_PARALLACTIC

*  Purpose:
*     To calculate the parallactic angle of the point midway between the
*     +ve and -ve beams

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_PARALLACTIC (NPIXEL, RA_POS, DEC_POS, RA_NEG,
*    :   DEC_NEG, LST, LAT, ETA, STATUS)

*  Description:
*
*  Arguments:
*     NPIXEL                         = INTEGER (Given)
*        the size of the arrays
*     RA_POS (NPIXEL)                = DOUBLE PRECISION (Given)
*        the RA of the positive beam (radians)
*     DEC_POS (NPIXEL)               = DOUBLE PRECISION (Given)
*        the dec of the +ve beam (radians)
*     RA_NEG (NPIXEL)                = DOUBLE PRECISION (Given)
*        the RA of the -ve beam (radians)
*     DEC_NEG (NPIXEL)               = DOUBLE PRECISION (Given)
*        the dec of the -ve beam (radians)
*     LST (NPIXEL)                   = DOUBLE PRECISION (Given)
*        the LST at twhich the pixel was observed (radians)
*     LAT                            = DOUBLE PRECISION (Given)
*        the latitude of the telescope
*     ETA (NPIXEL)                   = REAL (Returned)
*        the parallactic angle (radians)
*     STATUS                         = INTEGER (Given and Returned)
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
      INCLUDE 'ASTRO_PAR'                        ! for SPI

*  Arguments Given:
      INTEGER NPIXEL
      DOUBLE PRECISION RA_POS (NPIXEL)
      DOUBLE PRECISION DEC_POS (NPIXEL)
      DOUBLE PRECISION RA_NEG (NPIXEL)
      DOUBLE PRECISION DEC_NEG (NPIXEL)
      DOUBLE PRECISION LST (NPIXEL)
      DOUBLE PRECISION LAT

*  Arguments Given and Returned:

*  Arguments Returned:
      REAL ETA (NPIXEL)

*  Status:
      INTEGER STATUS                             ! Global status

*  External Functions:

*  Local Constants:

*  Local Variables:
      INTEGER I
      DOUBLE PRECISION RA, DEC
      DOUBLE PRECISION H_A                 ! hour angle
      DOUBLE PRECISION Z                   ! zenith distance
      DOUBLE PRECISION COSZ                ! cos (z)
      REAL SINETA                          ! sin (ETA)

*  Local data:
*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, NPIXEL

*  RA, dec of point midway between beams

         RA = (RA_POS (I) + RA_NEG (I)) / 2.0D0
         DEC = (DEC_POS (I) + DEC_NEG (I)) / 2.0D0

         H_A = LST (I) - RA
         COSZ = SIN(LAT) * SIN(DEC) + COS(LAT) * COS(DEC) * COS(H_A)
         Z = ACOS (COSZ)
         IF (SIN(Z) .GT. 0.0D0) THEN
            SINETA = SIN (H_A) * COS (LAT) / SIN (Z)
            ETA (I) = ASIN (SINETA)
         ELSE
            IF (DEC .GT. LAT) THEN
               ETA (I) = SPI
            ELSE
               ETA (I) = 0.0
            END IF
         END IF

*  John Richer's definition of parallactic angle seems to be pi/2 - mine

         ETA (I) = SPI - ETA (I)

      END DO

      END
