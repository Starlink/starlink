      SUBROUTINE FIND19( OBPS, OBPSHI, OBPSLO, OBPSR, OBST1D, OBST2D,
     :                   OBTH, SLCRSZ, SLELA5, SLELO5,
     :                   SPPAFT, SPSLG, SPSLGR, SPSPD, SPSTCL, SPSTCR,
     :                   SLPSI, SLTH, SLCRHW, SLELAT, SLELOT, TESTOK,
     :                   THDIFF,
     :                   SPSLGT, TIMCT, TIMYRS,
     :                   STATUS )
*+
*  Name:
*     FIND19

*  Purpose:
*     This is the third test used in FINDCRDD to determine whether a
*     scan passes sufficiently close to the source position that the
*     user might require it.
*     The third stage of testing is a refinement on the second.
*     Again the satellite angles IRAS would have had to see the source
*     are calculated, and compared with the actual satellite angle
*     range for the observation under consideration. But in this stage
*     both the source position and the solar longitude are evaluated
*     at the epoch of the best estimate of the crossing time.
*     The calculation is repeated through four iterations to refine the
*     crossing time, and hence the positions. The first of these
*     iterations is only used to get an initial estimate of the crossing
*     time and no acceptance test is carried out in this iteration.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND19( OBPS, OBPSHI, OBPSLO, OBPSR, OBST1D, OBST2D,
*     :            OBTH, SLCRSZ, SLELA5, SLELO5,
*     :            SPPAFT, SPSLG, SPSLGR, SPSPD, SPSTCL, SPSTCR,
*     :            SLPSI, SLTH, SLCRHW, SLELAT, SLELOT, TESTOK,
*     :            SPSLGT, TIMCT, TIMYRS,
*     :            STATUS )

*  Description:
*     This is the third test used in FINDCRDD to determine whether a
*     scan passes sufficiently close to the source position that the
*     user might require it.
*     The third stage of testing is a refinement on the second.
*     Again the satellite angles IRAS would have had to see the source
*     are calculated, and compared with the actual satellite angle
*     range for the observation under consideration. But in this stage
*     both the source position and the solar longitude are evaluated
*     at the epoch of the best estimate of the crossing time.
*     The calculation is repeated through four iterations to refine the
*     crossing time, and hence the positions. The first of these
*     iterations is only used to get an initial estimate of the crossing
*     time and no acceptance test is carried out in this iteration.
*
*     The subroutine carries out the following stages:-
*
*     -  Calculate the cross scan test size as half the cross scan
*        extent plus the focal plane half width. This is less than the
*        stage 2 cross scan test, and is not applied until the source
*        and satellite epochs are close in time (ie not for the first
*        iteration)
*
*     -  Set the initial crossing time as the start time of the
*        observation.
*
*     For 4 iterations - The first of these is only to get an
*     approximate crossing time, and the cross scan test is ommitted
*     as this would be too tight.
*
*     - Calculate the crossing time, as decimal years eg 1983.???
*       using FIND46.
*
*     - Calculate the ecliptic coords of the source at this time,
*       from the source ecliptic 1950 coords, using FIND12.
*
*     - Calculate the solar longitude at this time, from SOP, solar
*       longitude and solar longitude rate, using FIND43.
*
*     - Calculate the source satellite angles at this time, from the
*       source ecliptic coords at this time, and the solar longitude
*       at this time, using FIND41.
*
*     - Calculate the abberation error to be applied, using FIND01
*
*     - Calculate
*       the absolute value of ( source theta - observation theta )
*       and compare it with the cross scan half width test size.
*
*     If this criterion is met then
*
*     - Compare the source psi with the range of psi covered by
*       the observation. This consists of three alternatives:-
*       i)  The observation psi lo is greater than zero and
*           observation psi lo < source psi <= observation psi hi.
*       ii)  The observation psi lo is less than zero and
*            observation psi lo + 2*pi < source psi <= 2*pi.
*       iii) The observation psi lo is less than zero and
*            0 < source psi <= observation psi hi.
*
*     If this criterion is met then
*
*     - Calculate the revised crossing time as either:-
*       i)   If criterion i) above is met then
*            time of crossing = time of observation start +
*            ( source psi - start of observation psi ) / observation
*                                                           psi rate
*       ii)  If criterion ii) above is met then
*            time of crossing = time of observation end +
*            ( source psi - end of observation psi ) / observation
*                                                        psi rate
*       iii) If criterion iii) above is met then
*            time of crossing = time of observation start +
*            ( source psi - start of observation psi ) / observation
*                                                           psi rate
*       The calculation is actually incorperated in the test criteria
*       above.

*  Arguments:
*     OBPS = REAL (Given)
*        Psi at start of observation
*     OBPSHI = REAL (Given)
*        Highest Psi value in observation.
*     OBPSLO = REAL (Given)
*        Lowest Psi value in observation.
*     OBPSR = REAL (Given)
*        Psi rate for observation
*     OBST1D = DOUBLE PRECISION (Given)
*        Satcal time of start of observation double precision version.
*     OBST2D = DOUBLE PRECISION (Given)
*        Satcal time of end of observation double precision version.
*     OBTH = REAL (Given)
*        Theta for observation
*     SLCRSZ = REAL (Given)
*        Source cross scan size in radians - this routine does not use
*        common so SOCRSZ( ISOURC ) is supplied in this argument.
*     SLELA5 = REAL (Given)
*        Source ecliptic latitude (1950)
*     SLELO5 = REAL (Given)
*        Source ecliptic longitude (1950)
*     SPPAFT = DOUBLE PRECISION (Given)
*        PAF time for start of SOP
*     SPSLG = REAL (Given)
*        Solar longitude at start of SOP
*     SPSLGR = REAL (Given)
*        Solar longitude rate for SOP
*     SPSPD = REAL (Given)
*        Sign of psi dot for SOP
*     SPSTCL = DOUBLE PRECISION (Given)
*        Satcal time at start of SOP
*     SPSTCR = REAL (Given)
*        Satcal rate for SOP
*     SLPSI = REAL (Given and Returned)
*        Source psi
*     SLTH = REAL (Given and Returned)
*        Source theta
*     SLCRHW = REAL (Given and Returned)
*        Source cross scan half width test size
*     SLELAT = REAL (Given and Returned)
*        Source ecliptic latitude at time of observation
*     SLELOT = REAL (Given and Returned)
*        Source ecliptic longitude at time of observation
*     TESTOK = LOGICAL (Given and Returned)
*        .TRUE. if test is passed
*     THDIFF = REAL (Given and Returned)
*         The absolute value of ( the difference between the theta of
*         the source and the observation theta )
*     SPSLGT = REAL (Returned)
*        Solar longitude at crossing time.
*     TIMCT = DOUBLE PRECISION (Returned)
*        SATCAL time of crossing time. ie best current estimate of the
*        time of closest approach of the observation to the source.
*     TIMYRS = REAL (Returned)
*        Time at which ecliptic coords are calculated in years and
*        decimal years
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     FINDCRDD routines:
*        FIND01, FIND12, FIND43, FIND41, FIND46

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     21-JAN-1992 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL OBPS
      REAL OBPSHI
      REAL OBPSLO
      REAL OBPSR
      DOUBLE PRECISION OBST1D
      DOUBLE PRECISION OBST2D
      REAL OBTH
      REAL SLCRSZ
      REAL SLELA5
      REAL SLELO5
      DOUBLE PRECISION SPPAFT
      REAL SPSLG
      REAL SPSLGR
      REAL SPSPD
      DOUBLE PRECISION SPSTCL
      REAL SPSTCR

*  Arguments Given and Returned:
      REAL SLPSI
      REAL SLTH
      REAL SLCRHW
      REAL SLELAT
      REAL SLELOT
      LOGICAL TESTOK
      REAL THDIFF

*  Arguments Returned:
      REAL SPSLGT
      DOUBLE PRECISION TIMCT
      REAL TIMYRS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL FPHWTH                ! IRAS Focal plane half width
      PARAMETER ( FPHWTH = 0.0047 )
      REAL TWOPIR                ! Two PI in radians
      PARAMETER ( TWOPIR = 6.2831853072 )
      REAL PIBY2                 ! PI / 2 in radians
      PARAMETER ( PIBY2 = 1.5707963268 )

*  Local Variables:
      REAL A                     ! Included angle in sperical triangle
      REAL AB                    ! Side in spherical triangle
      REAL AC                    ! Side in spherical triangle
      REAL C                     ! Angle in spherical triangle
      REAL SLDPSI                ! Delta Psi correction for abberation
                                 ! error
      REAL SLPSIA                ! Psi corrected for abberation error
      REAL SLDTH                 ! Delta Theta correction for abberation
                                 ! error
      REAL SLTHA                 ! Theta corrected for abberation error
      INTEGER ITCONT             ! Iteration count
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the cross scan half width as half the required cross scan
*  extent + the focal plane half width.
      SLCRHW = SLCRSZ / 2.0 + FPHWTH

*  For the first estimate of the crossing time to be the start time of
*  the observation
      TIMCT = OBST1D

*  For four iterations
      DO 100 ITCONT = 1, 4

*  Transform the estimated crossing time from SATCAL to years
*  (with decimal places)
         CALL FIND46( TIMCT, SPPAFT, SPSTCL, SPSTCR, TIMYRS, STATUS )

*  Calculate the source ecliptic coordinates at the epoch of the
*  estimated crossing time
         CALL FIND12( SLELA5, SLELO5, 1950.0, TIMYRS,
     :                SLELAT, SLELOT, STATUS )

*  Calculate the solar longitude at the time of the estimated crossing
*  time.
         CALL FIND43( SPSLG, SPSLGR, SPSTCL, TIMCT, SPSLGT, STATUS )

*  Set up values for sides and included angle for spherical triangle
*  The included angle A is solar longitude - source ecliptic longitude
         A  = SPSLGT - SLELOT

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
         IF ( SLPSI .LT. 0.0 ) SLPSI = SLPSI + TWOPIR

*  Calculate the abberation error
         CALL FIND01( SLPSI, SLTH, SPSPD, SLDPSI, SLDTH, STATUS )

*  Correct source psi and source theta for the abberation error
         SLPSIA = SLPSI + SLDPSI
         SLTHA  = SLTH  + SLDTH

*  Calculate the absolute value of ( source theta - observation theta )
         THDIFF = ABS( SLTHA - OBTH )

*  If either the difference in theta is less than cross scan half
*  width test size or it is the first iteration
         IF ( ( THDIFF .LE. SLCRHW ) .OR. ( ITCONT .EQ. 1 ) ) THEN

*  Test whether the source psi is within the range of observation psi
*  This consists of three alternatives but the first test is whether
*  the lower limit of the obsrvation range is greater than zero
            IF ( OBPSLO .GE. 0.0 ) THEN

*  Given that the observation psi lo is greater than zero
*  the source psi is within the range if:-
*     observation psi lo < source psi <= observation psi hi.
               IF ( ( SLPSIA .GE. OBPSLO ) .AND.
     :              ( SLPSIA .LE. OBPSHI ) ) THEN
                  TESTOK = .TRUE.

*  Calculate revised estimate of crossing time
                  TIMCT = OBST1D +
     :                    ( DBLE( SLPSIA) - DBLE( OBPSHI) ) /
     :                                            DBLE( OBPSR )
               ELSE

*  Source psi is not within observation psi range, set the TESTOK flag
*  to false and return
                  TESTOK = .FALSE.
                  RETURN
               END IF
            ELSE

*  Given that the observation psi lo is greater than zero
*  the source psi is within the range if either:-
*     observation psi lo + 2*pi < source psi <= 2*pi.
               IF ( ( SLPSIA .GE. ( OBPSLO + TWOPIR) ) .AND.
     :              ( SLPSIA .LE. TWOPIR)  ) THEN
                  TESTOK = .TRUE.

*  Calculate revised estimate of crossing time
                  TIMCT = OBST2D +
     :                    ( DBLE( SLPSIA) - DBLE( OBPSHI + TWOPIR ) ) /
     :                                            DBLE( OBPSR )

*  or if:-
*     0 < source psi <= observation psi hi.
               ELSE IF ( ( SLPSIA .GE. 0.0 ) .AND.
     :                   ( SLPSIA .LE. OBPSHI ) ) THEN
                  TESTOK = .TRUE.

*  Calculate revised estimate of crossing time
                  TIMCT = OBST1D +
     :                    ( DBLE( SLPSIA) - DBLE( OBPSHI) ) /
     :                                            DBLE( OBPSR )
               ELSE

*  Source psi is not within observation psi range, set the TESTOK flag
*  to false and return
                  TESTOK = .FALSE.
                  RETURN
               END IF
            END IF
         ELSE

*  Source theta is not within the half cross scan test size of the
*  observation theta. Set the test ok flag to false and return.
            TESTOK = .FALSE.
            RETURN
         END IF
 100    CONTINUE

      END
