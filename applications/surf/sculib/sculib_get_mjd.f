      SUBROUTINE SCULIB_GET_MJD(N_FITS, FITS, MJD, EPOCH, STATUS)
*+
*  Name:
*     SCULIB_GET_MJD

*  Purpose:
*     Obtain the Modified Julian date from the UT stored in FITS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE SCULIB_GET_MJD(N_FITS, FITS, MJD, STATUS)

*  Description:
*     This routine retrieves the UTSTART and UTDATE values from the FITS
*     array and calculates the MJD from these.

*  Arguments:
*     N_FITS = _INTEGER (Given)
*        Size of FITS array
*     FITS  = _CHARACTER (Given)
*        FITS values
*     MJD = DOUBLE PRECISION (Returned)
*        Modified Julian date of observation
*     EPOCH = _REAL (Returned)
*        Epoch of observation
*     STATUS = _INTEGER (Given & Returned)
*        Global status

*  Prior Requirements:
*     The locator to the structure must already be available.

*  Notes:
*     This routine does not annul the locator.

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL:  John Lightfoot (RoE)

*  History:
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
      INTEGER       N_FITS
      CHARACTER*(*) FITS (N_FITS)

*  Arguments Returned:
      REAL             EPOCH
      DOUBLE PRECISION MJD

*  Status:
      INTEGER CHR_STATUS         ! Status for CHR_ routines
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER       CHR_LEN      ! Length of string
      INTEGER       INDEX        ! Returns position in string
      DOUBLE PRECISION SLA_EPJ   ! Conversion of MJD to EPOCH

*  Local constants:
      INTEGER   MAX_DIM          ! max number of dims in array
      PARAMETER (MAX_DIM = 4)

*  Local Variables:
      INTEGER ID                 ! day of an input observation
      INTEGER IEND               ! index of end of sub-string
      INTEGER IHOUR              ! hour in which observation started
      INTEGER IM                 ! month in which observation started
      INTEGER IMIN               ! minute at which observation started
      INTEGER ISTART             ! index of start of sub-string
      INTEGER IY                 ! year in which input observation started
      DOUBLE PRECISION SEC       ! second at which observation started
      CHARACTER*(15) UTDATE      ! UT date
      CHARACTER*(15) UTSTART     ! Time of observation


*  Local Data:
*.
      IF (STATUS .NE. SAI__OK) RETURN

*     Read fits values
      CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS, 
     :     'UTDATE', UTDATE, STATUS)
      CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS, 
     :     'UTSTART', UTSTART, STATUS)


*     Decode FITS values
      IF (STATUS .EQ. SAI__OK) THEN
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

*     Calculate EPOCH

      IF (STATUS .EQ. SAI__OK) THEN

         EPOCH = REAL(SLA_EPJ(MJD))

      END IF

      END
