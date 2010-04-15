      SUBROUTINE FIND12( ECLAT1, ECLNG1, EPOCY1, EPOCY2,
     : ECLAT2, ECLNG2, STATUS )
*+
*  Name:
*     FIND12

*  Purpose:
*     To transform ecliptic coordinates from epoch EPOCY1 to
*     epoch EPOCY2.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND12( ECLAT1, ECLNG1, EPOCY1, EPOCY2,
*     : ECLAT2, ECLNG2, STATUS )

*  Description:
*     To transform ecliptic coordinates from epoch EPOCY1 to
*     epoch EPOCY2.

*  Arguments:
*     ECLAT1 = REAL (Given)
*        Ecliptic latitude at epoch EPOCY1
*     ECLNG1 = REAL (Given)
*        Ecliptic longitude at epoch EPOCY1
*     EPOCY1 = REAL (Given)
*        Epoch at which first ecliptic coords are given,
*        in years and decimal years eg 1950.0
*     EPOCY2 = REAL (Given)
*        Epoch at which second ecliptic coords are to be calculated,
*        in years and decimal years eg 1983.5
*     ECLAT2 = REAL (Returned)
*        Ecliptic latitude calculated for epoch EPOCY2
*     ECLNG2 = REAL (Returned)
*        Ecliptic longitude calculated for epoch EPOCY2
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     FINDCRDD:
*        FIND41
*     IRA:
*        IRA_NORM

*  External Routines Used:
*     IRA:
*        IRA_NORM

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     16-JAN-1992 (DCP):
*        Original version.
*        This original version is adapted from ECLTT, a subroutine
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
      REAL ECLAT1
      REAL ECLNG1
      REAL EPOCY1
      REAL EPOCY2

*  Arguments Returned:
      REAL ECLAT2
      REAL ECLNG2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL EPOCY0                ! Base year for epoch ie 1900
      PARAMETER ( EPOCY0 = 1900 )
      REAL CPI0                  ! [constant_description]
      PARAMETER ( CPI0 = 3.036018 )
      REAL CPI1                  ! [constant_description]
      PARAMETER ( CPI1 = 0.015932 )
      REAL A0                    ! [constant_description]
      PARAMETER ( A0 = 0.2436499E-3 )
      REAL A1                    ! [constant_description]
      PARAMETER ( A1 = 0.1076E-6 )
      REAL B0                    ! [constant_description]
      PARAMETER ( B0 = 2.284E-6 )
      REAL B1                    ! [constant_description]
      PARAMETER ( B1 = -.003E-6 )
      REAL PIBY2                 ! PI divided by 2 as radians
      PARAMETER ( PIBY2 = 1.5707963268 )

*  Local Variables:
      REAL AA                    ! [local_variable_description]
      REAL AVEEPO                ! Average of required epoch and given
                                 ! epoch in decimal centuries since the
                                 ! base year EPOCY0
      REAL BB                    ! [local_variable_description]
      REAL CPI                   ! [local_variable_description]
      REAL DIFEPO                ! Difference between required epoch and
                                 ! given epoch in years.
      DOUBLE PRECISION ECLATD    ! Ecliptic latitude double precision
      DOUBLE PRECISION ECLNGD    ! Ecliptic longitude double precision
      REAL EEN                   ! [local_variable_description]
*  In the following variables describing the sides and angles of a
*  sperical triangle, P is the pole, E1 is the given ecliptic posn. and
*  E2 is the required ecliptic posn.
      REAL E1E2                  ! The side E1E2
      REAL E1E2P                 ! The angle E1E2P
      REAL E1P                   ! The side E1P
      REAL E1PE2                 ! The angle E1PE2
      REAL E2P                   ! The side E2P
      REAL PE1E2                 ! The angle PE1E2
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the difference between the required and the given epoch
      DIFEPO = EPOCY2 - EPOCY1

*  Calculate the average of the required and the given epoch in decimal
*  centuries from the base epoch ( EPOCY0 at present is 1900)
      AVEEPO = ( EPOCY1 + EPOCY2 - 2.0*EPOCY0 ) / 200.0

*  Calculate variables that are average time dependant, to be used later
*  in calculating the sides/angles for the spherical triangle.
      CPI = CPI0 + AVEEPO*CPI1
      AA = A0 + AVEEPO*A1
      BB = B0 + AVEEPO*B1

*  Determine whether EEN should be + or - PI/2 depending on whether
*  the difference between the required epoch and the given one is
*  +ve or -ve
      EEN = PIBY2
      IF ( DIFEPO .LT. 0.0 ) EEN = -PIBY2


*  Calculate the known sides of the spherical triangle
      PE1E2 = EEN - CPI + ECLNG1
      E1P = PIBY2 - ECLAT1
      E1E2 = BB * ABS( DIFEPO )

*  Use spherical triangle routine to evaluate unkown sides and angles
      CALL FIND41( PE1E2, E1P, E1E2, E1PE2, E1E2P, E2P, STATUS)

*  Calculate unnormalised ecliptic longitude
      ECLNG2 = CPI + AA*DIFEPO + EEN - E1E2P

*  Calculate ecliptic latitude
      ECLAT2 = PIBY2 - E2P

*  Use IRA_NORM to normalise ecliptic coordinates
      ECLATD = DBLE( ECLAT2 )
      ECLNGD = DBLE( ECLNG2 )

      CALL IRA_NORM( ECLNGD, ECLATD, STATUS)

      ECLAT2 = REAL( ECLATD )
      ECLNG2 = REAL( ECLNGD )

      END
