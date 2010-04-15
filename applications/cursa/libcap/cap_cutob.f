      SUBROUTINE CAP_CUTOB (UTDATE, LST, LONG, UTMJD, STATUS)
*+
*  Name:
*     CAP_CUTOB
*  Purpose:
*     Calculate UT of observation from UT date and LST.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CUTOB (UTDATE, LST, LONG; UTMJD; STATUS)
*  Description:
*     Calculate UT the of observation, expressed as an MJD, from the UT
*     date and the LST of observation.
*
*     IMPORTANT NOTE: because the sidereal day is shorter than the UT
*     day, sometimes 0 hours LST can happen twice in one UT day.  The
*     routine does not handle this case properly and hence caution
*     should be excercised in using it.  The effect is not important for
*     UKSTU plates because at the longitude of Siding Springs it can
*     only happen near 10:00am UT, which is daylight.  However, the
*     effect could be important for plates exposed at other geographical
*     longitudes.
*  Arguments:
*     UTDATE  =  CHARACTER*(*) (Given)
*        The UT date of observation, coded as a CHARACTER string, as
*        follows:  'YYMMDD'.
*     LST  =  CHARACTER*(*) (Given)
*        The local mean sidereal time of the observation, coded as a
*        CHARACTER string, as follows: 'HHMM'.
*     LONG  =  DOUBLE PRECISION (Given)
*        Geographical longitude of the observatory where the plate
*        the plate was exposed, expressed in radians with the
*        (conventional astronomical) convention that east is positive.
*     UTMJD  =  DOUBLE PRECISION (Returned)
*        UT of the observation, expressed as an MJD.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the year, month and day of observation from CHARACTER
*     strings to INTEGERs.
*     Express the UT date as an MJD.
*     If ok then
*       Compute the GMST corresponding to the UT date.
*       Compute the LST corresponding to the GMST.
*       Compute the difference in LST between the date of observation
*       and the time of observation.
*       Convert this difference from LST to UT.
*       Add the difference in UT to the UT date to give the UT time
*       of observation.
*     else
*       Set the UT time of observation to zero.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     19/2/95 (ACD): Original version.
*     19/2/95 (ACD): First stable version.
*     21/5/98 (ACD): Re-badged for CURSA.
*-
*  Type Definitions:
      IMPLICIT NONE       ! No implicit typing
*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CAP_PAR'   ! CPC parametric constants.
*  Global Variables:
*     <...>
*  Arguments Given:
      CHARACTER
     :  UTDATE*(*),
     :  LST*(*)
      DOUBLE PRECISION
     :  LONG
*  Arguments Returned:
      DOUBLE PRECISION
     :  UTMJD
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      DOUBLE PRECISION SLA_GMST
*  Local Constants:
*     <...>
*  Local Variables:
      INTEGER
     :  YEAR,    ! Year  of observation.
     :  MONTH,   ! Month "      "      .
     :  DAY,     ! Day   "      "      .
     :  DSTAT,   ! Status converting date to UT MJD.
     :  LSTHR,   ! Hours   of LST.
     :  LSTMIN   ! Minutes "   " .
      DOUBLE PRECISION
     :  MJDATE,  ! Date expressed as an UT MJD.
     :  GMSTDT,  ! Date expressed as GMST.
     :  LSTDAT,  ! Date expressed as LST.
     :  LSTRAD,  ! LST expressed as radians.
     :  DIFLST,  ! Difference between date and time, LST, radians.
     :  DIFUT    !     "         "     "    "   "  , UT,     "   .
*.

      IF (STATUS .EQ. SAI__OK) THEN
C        print2999, utdate, lst, long
C2999    format(1x, 'utdate: ', a /
C    :          1x, 'lst: ', a /
C    :          1x, 'long: ', 1pd20.8 / )

*
*       Convert the year, month and day of observation from a CHARACTER
*       string to INTEGERs.

         CALL CHR_CTOI (UTDATE(1 : 2), YEAR, STATUS)
         CALL CHR_CTOI (UTDATE(3 : 4), MONTH, STATUS)
         CALL CHR_CTOI (UTDATE(5 : 6), DAY, STATUS)

*
*       Express the UT date as an MJD.  If the conversion fails then
*       set the status and report an error.

         CALL SLA_CALDJ (YEAR, MONTH, DAY, MJDATE, DSTAT)

         IF (DSTAT .NE. 0) THEN
            STATUS = SAI__ERROR

            CALL MSG_SETC ('UTDATE', UTDATE)

            IF (DSTAT .EQ. 1) THEN
               CALL MSG_SETC ('CODE', 'bad year')
            ELSE IF (DSTAT .EQ. 2) THEN
               CALL MSG_SETC ('CODE', 'bad month')
            ELSE IF (DSTAT .EQ. 3) THEN
               CALL MSG_SETC ('CODE', 'bad day')
            ELSE
               CALL MSG_SETC ('CODE', 'bad error code')
            END IF

            CALL ERR_REP ('CATCOORD_MJD', 'Error converting UT date '/
     :        /'^UTDATE to MJD (^CODE).', STATUS)
         END IF

*
*       Proceed if ok.

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Compute the GMST corresponding to the UT date.  Note that
*          the GMST is returned in radians.

            GMSTDT = SLA_GMST(MJDATE)

*
*          Compute the LST corresponding to the GMST.  Remember the
*          sign convention for the longitude.  Corrections are not
*          made for polar motion and the equation of the equinoxes.

            LSTDAT = GMSTDT + LONG

*
*          Convert the LST of the start of the observation from hours and
*          minutes to radians.

            CALL CHR_CTOI (LST(1 : 2), LSTHR, STATUS)
            CALL CHR_CTOI (LST(3 : 4), LSTMIN, STATUS)

            LSTRAD = (DBLE(LSTHR) + (DBLE(LSTMIN) / 6.0D1) )
     :        * CAP__PI / 1.2D1

*
*          Compute the difference in LST between the date of observation
*          and the time of observation (in radians).

            DIFLST = LSTRAD - LSTDAT

*
*          Convert this difference from LST to UT.  The formula is
*          taken from the 'Astronomical Almanac' 1984, pS15.  However,
*          only the constant term is used; the first and second order
*          terms in UT are ignored.

            DIFUT = DIFLST * 9.97269566329084D-1

*
*          Convert the UT difference from radians to fractions of a
*          day and then add it to the UT date to finally yield the
*          UT time expressed as an MJD.

            UTMJD = MJDATE + (DIFUT / (CAP__PI * 2.0D0) )

         ELSE

*
*          An error has occurred; set the UT time of observation to zero
*          in order to give it an explicit value.

            UTMJD = 0.0D0
         END IF

      END IF

      END
