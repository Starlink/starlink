      SUBROUTINE FIND17( SODEC, SORA, SPSLG,
     :          SLELA5, SLELAT, SLELO5, SLELOT, SLPSI, SLTH,
     :          TESTOK, STATUS )
*+
*  Name:
*     FIND17

*  Purpose:
*     This is the first test used in FINDCRDD to determine whether an
*     observation passes sufficiently close to the source position that
*     the user might require a subsection, ie a scan, from it.
*     This first test accepts or rules out all the observations in a
*     SOP by determining whether the source position is sufficiently
*     far from the position of the Sun, at the time the SOP was taken,
*     for it to be feasible for IRAS to observe the source.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND17( SODEC, SORA, SPSLG,
*     :          SLELA5, SLELAT, SLELO5, SLELOT, SLPSI, SLTH,
*     :          TESTOK, STATUS )

*  Description:
*     This is the first test used in FINDCRDD to determine whether an
*     observation passes sufficiently close to the source position that
*     the user might require a subsection, ie a scan, from it.
*     This first test accepts or rules out all the observations in a
*     SOP by determining whether the source position is sufficiently
*     far from the position of the Sun, at the time the SOP was taken,
*     for it to be feasible for IRAS to observe the source.
*
*     The subroutine first calculates the satellite angles of the
*     source (i.e. The satellite angles that IRAS would have had if it
*     were to have observed the source position).
*     This involves three stages:-
*        i)   Calculate the ecliptic coords of the source position at
*           epoch 1950 from the equatorial coords, using FIND15.
*        ii)  Calculate the ecliptic coords of the source position at
*           the average time of observation ie 1983.5 from the ecliptic
*           1950 coords, using FIND12.
*        iii) Calculate the satellite angles for the source position.
*           This is different for each SOP because you have to use the
*           Solar longitude of the SOP to calculate Satellite angles
*           from the ecliptic position of the source. This uses the
*           subroutine FIND41.
*
*     It then determines whether the satellite sun angle ( theta ) for
*     the source is within the bounds imposed by the geometry of the
*     mission. (ie you can't observe too close to the Sun)
*     SUNLO (= 1.030 rads ) .LE. THETA .LE. SUNHI (= 2.112 rads)

*  Arguments:
*     SODEC = REAL (Given)
*        Source declination ( 1950 ) in radians - this routine does not
*        use common so SODEC( ISOURC ) is supplied in this argument.
*     SORA = REAL (Given)
*        Source RA (1950) in radians - this routine does not use
*        common so SORA( ISOURC ) is supplied in this argument.
*     SPSLG = REAL (Given)
*        Solar longitude at start of SOP
*     SLELA5 = REAL (Returned)
*        Source ecliptic latitude (1950)
*     SLELAT = REAL (Returned)
*        Source ecliptic latitude at time of observation
*     SLELO5 = REAL (Returned)
*        Source ecliptic longitude (1950)
*     SLELOT = REAL (Returned)
*        Source ecliptic longitude at time of observation
*     SLPSI = REAL (Returned)
*        Source psi
*     SLTH = REAL (Returned)
*        Source theta
*     TESTOK = LOGICAL (Returned)
*        .TRUE. if test is passed
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  External Routines Used:
*     FINDCRDD routines:
*        FIND12, FIND15, FIND41

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1992 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
*  FINDCRDD common (in FICOMN.FOR) is not included. The two variables
*  required from it SODEC and SORA for the source under consideration
*  are supplied via the argument list.

*  Arguments Given:
      REAL SODEC
      REAL SORA
      REAL SPSLG

*  Arguments Returned:
      REAL SLELA5
      REAL SLELAT
      REAL SLELO5
      REAL SLELOT
      REAL SLPSI
      REAL SLTH
      LOGICAL TESTOK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL PIBY2                 ! PI divided by 2 as radians
      PARAMETER ( PIBY2 = 1.5707963268 )
      REAL TWOPI                 ! 2*PI as radians
      PARAMETER ( TWOPI = 6.2831853072 )
      REAL SUNHI                 ! The maximum Theta that IRAS can
                                 ! have because of the limitation of
                                 ! not looking too close to the SUN
      PARAMETER ( SUNHI = 2.112 )
      REAL SUNLO                 ! The minimum Theta that IRAS can
                                 ! have because of the limitation of
                                 ! not looking too close to the SUN
      PARAMETER ( SUNLO = 1.030 )

*  Local Variables:
      REAL A                     ! Internal angle of spherical triangle
      REAL AB                    ! One side of spherical triangle
      REAL AC                    ! One side of spherical triangle
      REAL C                     ! One angle of sperical triangle
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the ecliptic coords of the source position at epoch 1950
*  from the equatorial coords.
      CALL FIND15( SODEC, 1950.0, SORA, SLELA5, SLELO5, STATUS )

*  Calculate the ecliptic coords of the source position at the average
*  time of observation (1983.5) from the ecliptic 1950 coords.
      CALL FIND12( SLELA5, SLELO5, 1950.0, 1983.5,
     : SLELAT, SLELOT, STATUS )

*  Check whether the status from FIND15  and FIND12 is O.K
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up values for sides and included angle for spherical triangle
*  The included angle A is solar longitude - source ecliptic longitude
      A  = SPSLG - SLELOT

*  One side AB is PI/2
      AB = PIBY2

*  The other side is PI/2 - source ecliptic latitude
      AC = PIBY2 - SLELAT

*  Call FIND41 to calculate the other side and angles of the spherical
*  triangle ( B is source psi (unnormalised), BC is source theta)
      CALL FIND41( A, AB, AC, SLPSI, C, SLTH, STATUS)

*  Check whether the status from FIND41 is O.K
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check whether source psi is within the correct range ie 0 to 2*PI
*  and if not normalise it
      IF ( SLPSI .LT. 0.0 ) SLPSI = SLPSI + TWOPI

*  Check whether source theta is within the region in which IRAS is
*  not pointing too close to the Sun
      IF ( ( SLTH .GE. SUNLO) .AND. ( SLTH .LE. SUNHI) ) THEN
         TESTOK = .TRUE.
      ELSE
         TESTOK = .FALSE.
      END IF

      END
