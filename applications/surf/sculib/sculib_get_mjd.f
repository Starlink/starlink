      SUBROUTINE SCULIB_GET_MJD(N_FITS, FITS, LST_REF, MJD, EPOCH,
     :     STARTUP_TIME, STATUS)
*+
*  Name:
*     SCULIB_GET_MJD

*  Purpose:
*     Obtain the Modified Julian date from the UT stored in FITS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_GET_MJD(N_FITS, FITS, LST_REF, MJD, EPOCH,
*    :      STARTUP_TIME, STATUS)

*  Description:
*     This routine retrieves the UTSTART and UTDATE values from the FITS
*     array and calculates the MJD from these. It can corrects for the
*     time taken before beginning the observation by comparing the
*     LST stored in the header (written when the observation begins and
*     before the telescope gets on source) and the reference LST supplied to
*     the routine (which is assumed to be the LST when the data acquisition
*     starts).

*  Arguments:
*     N_FITS = _INTEGER (Given)
*        Size of FITS array
*     FITS  = _CHARACTER (Given)
*        FITS values
*     LST_REF = DOUBLE PRECISION (Given)
*        Reference LST. This is the LST at which data acquisition
*        begins. The MJD should be calculated for the start of
*        data taking rather than the time the telescope begain its
*        slew. This number is normally read from the LST_STRT array
*        and should be in radians. If a negative value or a zero
*        value is given it is assumed that no correction should be applied.
*        This does mean that LST_REF can never be exactly zero but that
*        is thought be a remarkably unlikely event.
*     MJD = DOUBLE PRECISION (Returned)
*        Modified Julian date of observation
*     EPOCH = _REAL (Returned)
*        Julian Epoch of observation
*     STARTUP_TIME = _REAL (Returned)
*        The time taken, in seconds, for the observation to begin.
*        If the time stored in the STSTART is larger than LST_REF
*        the assumption is that a day boundary has been crossed since
*        the LST_REF should always be more recent than STSTART (the
*        data can not be taken before the observation starts). A startup
*        time longer than 5 minutes or so should be treated with
*        suspicion. If the startup time is longer than 10 minutes it is
*        not used for the MJD calculation since this should never
*        be the case. Set to zero if LST_REF was negative.
*     STATUS = _INTEGER (Given & Returned)
*        Global status

*  Notes:
*     Uses the UTDATE and UTSTART and STSTART FITS headers.
*     These must be present.
*     - The date must be stored in UTDATE in format YYYY:M:D
*     - The time must be stored in UTSTART in format HH:MM:SS.SS
*     - The MJD is calculated for the start of data taking rather
*     than simply using the values stored in UTSTART. This is because
*     there is always a delay between writing the FITS headers and
*     taking data.

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL:  John Lightfoot (RoE)


*  Copyright:
*     Copyright (C) 1995-2000 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.6  2000/07/10 21:09:30  timj
*     Documentation tweaks for V1.6
*
*     Revision 1.5  2000/06/16 01:25:57  timj
*     Correct for startup error
*
*     1997 March 21 (TIMJ)
*        Extract from main tasks


*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER          N_FITS
      CHARACTER*(*)    FITS (N_FITS)
      DOUBLE PRECISION LST_REF

*  Arguments Returned:
      REAL             EPOCH
      DOUBLE PRECISION MJD
      REAL             STARTUP_TIME

*  Status:
      INTEGER SLA_STATUS         ! Status for SLA_ routines
      INTEGER CHR_STATUS         ! Status for CHR_ routines
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER       CHR_LEN      ! Length of string
      INTEGER       INDEX        ! Returns position in string
      DOUBLE PRECISION SLA_EPJ   ! Conversion of MJD to EPOCH

*  Local constants:
      INTEGER   MAX_DIM          ! max number of dims in array
      PARAMETER (MAX_DIM = 4)
      DOUBLE PRECISION DR2S      ! radians to seconds of time
      PARAMETER ( DR2S = 1.37509870831397570104315571553852408797773D4)
      DOUBLE PRECISION D2PI     ! 2 * PI
      PARAMETER ( D2PI = 6.283185307179586476925286766559005768394339 )

*  Local Variables:
      LOGICAL CORRECT_SLEW       ! Correct the MJD for slew time
      DOUBLE PRECISION DTIME     ! start time difference (radians)
      INTEGER ID                 ! day of an input observation
      INTEGER IEND               ! index of end of sub-string
      INTEGER IHOUR              ! hour in which observation started
      INTEGER IM                 ! month in which observation started
      INTEGER IMIN               ! minute at which observation started
      INTEGER ISTART             ! index of start of sub-string
      INTEGER IY                 ! year in which input observation started
      DOUBLE PRECISION LST_HDR   ! LST read from STSTART header (radians)
      DOUBLE PRECISION SEC       ! second at which observation started
      CHARACTER *15    STSTART   ! String form of STSTART header item
      CHARACTER*(15) UTDATE      ! UT date
      CHARACTER*(15) UTSTART     ! Time of observation

*  Local Data:
*.
      IF (STATUS .NE. SAI__OK) RETURN

*     Are we correcting for slew time
      CORRECT_SLEW = .TRUE.
      IF (LST_REF .LT. 1.0D-200) CORRECT_SLEW = .FALSE.

*     Initialize
      DTIME = 0.0D0
      STARTUP_TIME = 0.0

*     Read fits values
      CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS,
     :     'UTDATE', UTDATE, STATUS)
      CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS,
     :     'UTSTART', UTSTART, STATUS)

      IF (CORRECT_SLEW) THEN
*     =============== CALCULATE SLEW CORRECTION ===================

         CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS,
     :        'STSTART', STSTART, STATUS)
         CALL SCULIB_DECODE_ANGLE(STSTART, LST_HDR, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
*     Convert time to radians
            LST_HDR = LST_HDR * 15.0D0

*     Calculate the startup time (in radians)
            DTIME = LST_REF - LST_HDR

*            print *,'DTIME ',DTIME, LST_REF, LST_HDR, ' ',STSTART

*     If this value is negative assume that the values crossed a
*     day boundary so add a day
            IF (DTIME .LT. 0) DTIME = DTIME + D2PI

*     Convert to seconds
            STARTUP_TIME = REAL ( DTIME * DR2S )

         END IF

      END IF

*     Decode FITS values
      IF ( STATUS .EQ. SAI__OK ) THEN

*     UTDATE  -> IY   IM  ID
         CHR_STATUS = SAI__OK

         ISTART = 1
         IEND = INDEX (UTDATE,':')
         IEND = MAX (ISTART,IEND)
         CALL CHR_CTOI (UTDATE (ISTART:IEND-1), IY, CHR_STATUS)
         UTDATE (IEND:IEND) = ' '
         ISTART = IEND + 1
         IEND = INDEX (UTDATE,':')
         IEND = MAX (ISTART,IEND)
         CALL CHR_CTOI (UTDATE (ISTART:IEND-1), IM, CHR_STATUS)
         UTDATE (IEND:IEND) = ' '
         ISTART = IEND + 1
         IEND = MAX (ISTART,CHR_LEN(UTDATE))
         CALL CHR_CTOI (UTDATE (ISTART:IEND), ID, CHR_STATUS)

*     UTSTART -> HH  MM  SS.SS
         ISTART = 1
         IEND = INDEX (UTSTART,':')
         IEND = MAX (ISTART,IEND)
         CALL CHR_CTOI (UTSTART (ISTART:IEND-1), IHOUR,
     :        CHR_STATUS)
         UTSTART (IEND:IEND) = ' '
         ISTART = IEND + 1
         IEND = INDEX (UTSTART,':')
         IEND = MAX (ISTART,IEND)
         CALL CHR_CTOI (UTSTART (ISTART:IEND-1), IMIN,
     :        CHR_STATUS)
         UTSTART (IEND:IEND) = ' '
         ISTART = IEND + 1
         IEND = MAX (ISTART,CHR_LEN(UTSTART))
         CALL CHR_CTOD (UTSTART (ISTART:IEND), SEC, CHR_STATUS)

         IF (CHR_STATUS .EQ. SAI__OK) THEN
            CALL SLA_CLDJ (IY, IM, ID, MJD, STATUS)
            MJD = MJD + ((SEC/60.0D0 + DBLE(IMIN)) / 60.0D0 +
     :           DBLE(IHOUR)) / 24.0D0
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETI ('SLA', STATUS)
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_GET_MJD: error '//
     :              'returned by SLA_CLDJ - status = ^SLA', STATUS)
            END IF
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC ('UTDATE', UTDATE)
            CALL MSG_SETC ('UTSTART', UTSTART)
            CALL ERR_REP (' ', 'SCULIB_GET_MJD: error '//
     :           'converting UTDATE=^UTDATE and UTSTART=^UTSTART '//
     :           'to UT1', STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN

*     Correct for startup time if it seems to be a reasonable correction
         IF ( CORRECT_SLEW .AND. STARTUP_TIME .LT. 600 ) THEN ! 10 minutes

            MJD = MJD + ( DTIME / D2PI ) ! time as fraction of a day

         END IF

*     Calculate EPOCH

         EPOCH = REAL(SLA_EPJ(MJD))

      END IF

      END
