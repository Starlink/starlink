      SUBROUTINE CAP_SCLBR (RADIUS, STATUS)
*+
*  Name:
*     CAP_SCLBR
*  Purpose:
*     Draw the scale bar.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_SCLBR (RADIUS; STATUS)
*  Description:
*     Draw the scale bar.
*  Arguments:
*     RADIUS  =  DOUBLE PRECISION (Given)
*        Radius of the plot (radians).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Scale the radius by two thirds.
*     Find the largest scale bar which is smaller than the scaled
*     length.
*     If a bar was found then
*       Scale the length of this bar from radians to normalised
*       coordinates.
*       Plot the scale bar.
*       Label the scale bar.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     22/8/96 (ACD): Original version.
*     3/6/97  (ACD): Changed the subroutine prefix from CIO to CAP.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CIO_PAR'           ! CIO parametric constants.
*  Global Variables:
      INCLUDE 'CHART_CMN'         ! CATCHART common block.
*  Arguments Given:
      DOUBLE PRECISION
     :  RADIUS
*  Status:
      INTEGER STATUS    ! Global status.
*  Local Constants:
      INTEGER MAXBAR    ! Number of scale bars.
      PARAMETER (MAXBAR = 17)
*  Local Variables:
      REAL
     :  BARLEN(MAXBAR), ! Length of each scale bar.
     :  LENGTH,         ! Scaled length.
     :  SCLBAR,         ! Length of the chosen scale bar.
     :  XLINE(2),       ! X coordinates of the current line.
     :  YLINE(2),       ! Y      "      "   "     "     "  .
     :  XTEXT,          ! X text position.
     :  YTEXT           ! Y  "      "    .
      CHARACTER
     :  BARLBL(MAXBAR)*11  ! Label for each scale bar.
      INTEGER
     :  BAR             ! Number of the chosen scale bar.
      LOGICAL
     :  FOUND,          ! Flag; has a scale bar been found?
     :  MORE            ! Flag; more scale bars to examine?
*  Local Data:
      DATA BARLEN
     : /1.7453293E0,    ! 100 degree.
     :  8.726646E-1,    ! 50 degree.
     :  3.490659E-1,    ! 20 degree.
     :  1.745329E-1,    ! 10 degree.
     :  8.72665E-2,     ! 5 degree.
     :  3.49066E-2,     ! 2 degree.
     :  1.74533E-2,     ! 1 degree.
     :  8.7266E-3,      ! 30 arcmin.
     :  2.9089E-3,      ! 10 arcmin.
     :  1.4544E-3,      ! 5 arcmin.
     :  5.8178E-4,      ! 2 arcmin.
     :  2.9089E-4,      ! 1 arcmin.
     :  1.4544E-4,      ! 30 arcsec.
     :  4.8481E-5,      ! 10 arcsec.
     :  2.4241E-5,      ! 5 arcsec.
     :  9.6963E-6,      ! 2 arcsec.
     :  4.8481E-6/      ! 1 arcsec.
      DATA BARLBL
     : /'100 degree.',
     :  '50 degree.',
     :  '20 degree.',
     :  '10 degree.',
     :  '5 degree.',
     :  '2 degree.',
     :  '1 degree.',
     :  '30 arcmin.',
     :  '10 arcmin.',
     :  '5 arcmin.',
     :  '2 arcmin.',
     :  '1 arcmin.',
     :  '30 arcsec.',
     :  '10 arcsec.',
     :  '5 arcsec.',
     :  '2 arcsec.',
     :  '1 arcsec.'/
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Scale the radius by two thirds.

         LENGTH = REAL(RADIUS) * 2.0E0 / 3.0E0

*
*       Attempt to find the largest scale bar which is smaller than the
*       scaled length.

         FOUND = .FALSE.
         MORE = .TRUE.
         BAR = 0

         DO WHILE (MORE)
            BAR = BAR + 1

            IF (BAR .LE. MAXBAR) THEN
               IF (BARLEN(BAR) .LE. LENGTH) THEN
                  FOUND = .TRUE.
                  MORE = .FALSE.
               END IF
            ELSE
               MORE = .FALSE.
            END IF
         END DO

*
*       Proceed if a suitable bar was found.

         IF (FOUND) THEN

*
*          Scale the length of this bar from radians to normalised
*          coordinates.

            SCLBAR = BARLEN(BAR) * (CVYMX__CIO - CVYMN__CIO) /
     :                                (CWYMX__CIO - CWYMN__CIO)

*
*          Construct and plot the scale bar.

            XLINE(1) = 7.0E-1
            YLINE(1) = 1.0E-1
            XLINE(2) = 7.0E-1
            YLINE(2) = YLINE(1) + SCLBAR

            CALL PGLINE (2, XLINE, YLINE)

            XLINE(1) = 6.9E-1
            YLINE(1) = 1.0E-1
            XLINE(2) = 7.1E-1
            YLINE(2) = 1.0E-1

            CALL PGLINE (2, XLINE, YLINE)

            YLINE(1) = YLINE(1) + SCLBAR
            YLINE(2) = YLINE(2) + SCLBAR

            CALL PGLINE (2, XLINE, YLINE)

*
*          Label the scale.

            XTEXT = 7.1E-1
            YTEXT = 1.0E-1 + (SCLBAR / 2.0E0)

            CALL PGTEXT (XTEXT, YTEXT, BARLBL(BAR) )

         END IF

      END IF

      END
