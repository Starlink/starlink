
      SUBROUTINE FIND15( DEC, EPOCYR, RA, ECLAT, ECLONG, STATUS )
*+
*  Name:
*     FIND15

*  Purpose:
*     Converts equatorial coordinates at epoch EPOCYR, to ecliptic
*     coordinates at the same epoch.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND15( DEC, EPOCYR, RA, ECLAT, ECLONG, STATUS )

*  Description:
*     Converts equatorial coordinates at epoch EPOCYR, to ecliptic
*     coordinates at the same epoch.

*  Arguments:
*     DEC = REAL (Given)
*        Declination at epoch EPOCYR
*     EPOCYR = REAL (Given)
*        Epoch at which equatorial coords are given and ecliptic coords
*        are evaluated given in years and decimal years eg 1950.0
*     RA = REAL (Given)
*        Right ascention at epoch EPOCYR
*     ECLAT = REAL (Returned)
*        Ecliptic latitude at epoch EPOCYR
*     ECLONG = REAL (Returned)
*        Ecliptic longitude at epoch EPOCYR
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     FINDCRDD:
*        FIND41
*        IRA:
*        IRA_NORM

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*        {enter_new_authors_here}

*  History:
*     16-JAN-1992 (DCP):
*        Original version.
*        This original version is adapted from CEQTECL, a subroutine
*        of POSNTIM, contained in its utilities subdirectory.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors

*  Arguments Given:
      REAL DEC
      REAL EPOCYR
      REAL RA

*  Arguments Returned:
      REAL ECLAT
      REAL ECLONG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL E1                    ! Constant in Obliquity of eclip poly.
      PARAMETER ( E1 = 23.452294 )
      REAL E2                    ! Coeff. of T in Ob. of eclip. poly.
      PARAMETER ( E2 = -0.0130125 )
      REAL E3                    ! Coeff. of T^2 in Ob of eclip. poly.
      PARAMETER ( E3 = -0.00000164 )
      REAL E4                    ! Coeff. of T^3 in Ob of eclip. poly.
      PARAMETER ( E4 = 0.000000503 )
      REAL PIBY2                 ! PI divided by 2 as radians
      PARAMETER ( PIBY2 = 1.5707963268 )

*  Local Variables:
      REAL DTR                   ! Degrees to radians
      REAL E                     ! Mean obliquity of ecliptic
      DOUBLE PRECISION ECLATD    ! Ecliptic latitude double precision
      DOUBLE PRECISION ECLNGD    ! Ecliptic longitude double precision
      REAL EQ                    ! Side EQ in spherical triangle.
      REAL EQP                   ! Angle EQP in spherical triangle.
      REAL PEQ                   ! Angle PEQ in spherical triangle.
      REAL T                     ! Time as decimal centuries since 1900
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate degrees to radians constant
      DTR = PIBY2 / 90.0

*  Calculate the time in decimal centuries from 1900 for use in
*  Obliquity of ecliptic calculation.
      T = ( EPOCYR - 1900.0 ) / 100.0

*  Calculate obliquity of ecliptic
      E = DTR * (E1 + T*(E2 + T*(E3 + T*E4)))

*  Use spherical triangle routine to evaluate PI/2 -  angle of
*  unnormalised ecliptic coordinate values.
      CALL FIND41( RA+PIBY2, PIBY2-DEC, E, EQP, PEQ, EQ, STATUS)

*  Calculate unnormalised ecliptic longitude
      ECLONG = PIBY2-PEQ

*  Calculate ecliptic latitude
      ECLAT = PIBY2-EQ

*  Use IRA_NORM to normalise ecliptic coordinates
      ECLATD = DBLE( ECLAT )
      ECLNGD = DBLE( ECLONG )

      CALL IRA_NORM( ECLNGD, ECLATD, STATUS)

      ECLAT = REAL( ECLATD )
      ECLONG = REAL( ECLNGD )

      END
