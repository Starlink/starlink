      SUBROUTINE REDS_CHGPNT (STATUS)
*+
*  Name:
*     CHANGE_POINTING

*  Purpose:
*     Routine to change the pointing corrections to map data.

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL REDS_CHGPNT( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This application is used to change the pointing corrections to map
*     data.
*
*        If the observing mode of the input datafile is `MAP' the
*     application will search for pointing corrections in the file and, if it
*     finds any, report them. You will be asked if you wish to change the
*     pointing correction data in the file. `No' will result in the data
*     remaining unaltered, `yes' will then ask you for the time of the 
*     pointing offset (LST in hh mm ss.ss format) and the azimuth and 
*     elevation correction (in arcseconds) that would have to be added to 
*     the observation position to correct the pointing at that time. If 
*     you supply no data the existing pointing corrections will be removed.
*     Corrections will be requested until a negative number is given
*     for the local sidereal time.

*  Usage:
*     change_pointing in change_point

*  ADAM Parameters:
*     CHANGE_POINT = _CHAR (Read)
*         If true you will be prompted for pointing corrections.
*     IN = NDF (Read)
*         Name of NDF to change.
*     MSG_FILTER = _CHAR (Read)
*         Message filter level. (Default is NORM)
*     POINT_DAZ = _REAL (Read)
*         The Azimuth pointing correction (arcsec).
*     POINT_DEL = _REAL (Read)
*         The elevation pointing correction (arcsec).
*     POINT_LST = _CHAR (Read)
*         The sidereal time of the pointing correction. Returning -1
*         will end the pointing offsets specification.

*  Related Application:
*     REBIN

*  Authors:
*     JFL:  J.Lightfoot (jfl@roe.ac.uk)
*     TIMJ: T.Jenness   (timj@jach.hawaii.edu)

*  History:
*     1997 March 19 (timj)
*        Separate from MODIFY
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}

*-

*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'REDS_SYS'                         ! REDS constants
      INCLUDE 'MSG_PAR'                          ! MSG__ constants

*    Import:
*    Import-Export:
*    Export:

*    Status :
      INTEGER STATUS

*    External references:

*    Global variables :

*    Local Constants:
      INTEGER          MAX__DIM                  ! max number of dimensions in
      PARAMETER (MAX__DIM = 4)                   ! array
      CHARACTER * 15   TSKNAME                   ! Name of task
      PARAMETER (TSKNAME = 'CHANGE_POINTING')  

*    Local variables :
      LOGICAL          CHANGE_POINT              ! .TRUE. if the user wants
                                                 ! to change the pointing
                                                 ! correction array
      DOUBLE PRECISION DTEMP                     ! scratch double
      CHARACTER*80     FITS (SCUBA__MAX_FITS)    ! array of FITS keywords
      INTEGER          I                         ! DO loop index
      INTEGER          IHMSF (4)                 ! hours, minutes, seconds
                                                 ! and fractional seconds
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC        ! HDS locator of .FITS
                                                 ! extension
      INTEGER          IN_NDF                    ! NDF index of input file
      CHARACTER*(DAT__SZLOC) IN_REDSX_LOC        ! HDS locator of .REDS
                                                 ! extension
      INTEGER          ITEMP                     ! scratch integer
      LOGICAL          LOOPING                   ! .TRUE. while looping and
                                                 ! reading pointing corrections
      LOGICAL          LST_OK                    ! .TRUE. if sidereal times of
                                                 ! pointing corrections are
                                                 ! monotonically increasing
      LOGICAL          LST_THERE                 ! TRUE if read in pointing corrections
      INTEGER          NREC                      ! number of history records
                                                 ! in input file
      INTEGER          N_FITS                    ! number of items in FITS
                                                 ! array
      INTEGER          N_POINT                   ! number of pointing 
                                                 ! corrections
      CHARACTER*40     OBJECT                    ! name of observed object
      CHARACTER*40     OBSERVING_MODE            ! observing mode of file
      LOGICAL          PHOTOM                    ! .TRUE. if the PHOTOM
                                                 ! application has been run
                                                 ! on the input file
      REAL             POINT_DAZ (SCUBA__MAX_POINT)
                                                 ! azimuth pointing correction
                                                 ! (arcsec)
      REAL             POINT_DEL (SCUBA__MAX_POINT)
                                                 ! elevation pointing
                                                 ! correction (arcsec)
      DOUBLE PRECISION POINT_LST (SCUBA__MAX_POINT)
                                                 ! sidereal times of pointing
                                                 ! corrections (radians)
      LOGICAL          REBIN                     ! .TRUE. if the REBIN
                                                 ! application has been run on
                                                 ! the input file
      LOGICAL          REDUCE_SWITCH             ! .TRUE. if the REDUCE_SWITCH
                                                 ! application has been run on
                                                 ! the input file
      INTEGER          RUN_NUMBER                ! run number of input file
      CHARACTER*1      SIGN                      ! + or -
      CHARACTER*80     STEMP                     ! scratch string
*    Internal References :
*    Local data :
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Set the MSG output level (for use with MSG_OUTIF)
      CALL MSG_IFGET('MSG_FILTER', STATUS)

* initialise some flags and locators

      LST_THERE = .FALSE.
      IN_FITSX_LOC = DAT__NOLOC
      IN_REDSX_LOC = DAT__NOLOC

*  start up the NDF system and open the demodulated data file

      CALL NDF_BEGIN

      CALL NDF_ASSOC ('IN', 'UPDATE', IN_NDF, STATUS)

*  check that the history of the file is OK

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (IN_NDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

         PHOTOM = .FALSE.
         REBIN = .FALSE.
         REDUCE_SWITCH = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (IN_NDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP .EQ. 'PHOTOM') THEN
                  PHOTOM = .TRUE.
               ELSE IF (STEMP .EQ. 'REBIN') THEN
                  REBIN = .TRUE.
               ELSE IF (STEMP .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               END IF
            END DO
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            IF (.NOT. REDUCE_SWITCH) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: this file has not '//
     :           'been through the REDUCE_SWITCH application', STATUS)
            END IF
            IF (PHOTOM) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: this file contains '//
     :           'PHOTOM data that has already been reduced', STATUS)
            END IF
            IF (REBIN) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: this file contains '//
     :           'data that has already been rebinned', STATUS)
            END IF
         END IF
      END IF

*  get some locators

      CALL NDF_XLOC (IN_NDF, 'FITS', 'UPDATE', IN_FITSX_LOC, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_XLOC (IN_NDF, 'REDS', 'UPDATE', IN_REDSX_LOC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            IN_REDSX_LOC = DAT__NOLOC
         END IF
      END IF
      
*     and read in some parameters describing the observation
      
      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: input file contains too '//
     :           'many FITS items', STATUS)
         END IF
      END IF

      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, N_FITS,
     :     STATUS)

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN',
     :     RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBJECT',
     :     OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :     OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETC ('MODE', OBSERVING_MODE)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM, ' ', 
     :     '^PKG: run ^RUN was a ^MODE observation of ^OBJECT',
     :     STATUS)

*     pointing corrections for MAP observations

      IF (OBSERVING_MODE .EQ. 'MAP' 
     :     .OR. OBSERVING_MODE .EQ. 'FOCUS'
     :     .OR. OBSERVING_MODE .EQ. 'ALIGN'
     :     .OR. OBSERVING_MODE .EQ. 'POINTING') THEN

*     find and report the start and finish LST of the observation

         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'STSTART', STEMP, STATUS)
         DO I = 1, 2
            ITEMP = INDEX (STEMP,':')
            IF (ITEMP .NE. 0) THEN
               STEMP (ITEMP:ITEMP) = ' '
            END IF
         END DO
         CALL MSG_SETC ('START_LST', STEMP)
         
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STEND',
     :        STEMP, STATUS)
         DO I = 1, 2
            ITEMP = INDEX (STEMP,':')
            IF (ITEMP .NE. 0) THEN
               STEMP (ITEMP:ITEMP) = ' '
            END IF
         END DO
         CALL MSG_SETC ('END_LST', STEMP)
         CALL MSG_SETC('PKG', PACKAGE)
         
         CALL MSG_OUTIF (MSG__NORM, ' ', 
     :        '^PKG: observation started at LST '//
     :        '^START_LST and ended at ^END_LST', STATUS)

*     find and report the nature of any current pointing correction

         IF (STATUS .EQ. SAI__OK) THEN
            CALL CMP_GET1D(IN_REDSX_LOC, 'POINT_LST', SCUBA__MAX_POINT,
     :           POINT_LST, N_POINT, STATUS)
            CALL CMP_GET1R(IN_REDSX_LOC, 'POINT_DAZ', SCUBA__MAX_POINT,
     :           POINT_DAZ, N_POINT, STATUS)
            CALL CMP_GET1R(IN_REDSX_LOC, 'POINT_DEL', SCUBA__MAX_POINT,
     :           POINT_DEL, N_POINT, STATUS)

            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_ANNUL(STATUS)
               N_POINT = 0
            ELSE
               LST_THERE = .TRUE.
            END IF
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            IF (N_POINT .EQ. 0) THEN
               CALL MSG_SETC('PKG', PACKAGE)
               CALL MSG_OUTIF (MSG__NORM, ' ', 
     :              '^PKG: no pointing corrections found',
     :              STATUS)
            ELSE
               CALL MSG_SETC('PKG', PACKAGE)
               CALL MSG_OUTIF (MSG__NORM, ' ', 
     :              '^PKG: the following pointing '//
     :              'corrections currently apply (LST dAZ dEL):-', 
     :              STATUS)

               DO I = 1, N_POINT
                  CALL SLA_DR2TF (2, POINT_LST(I), SIGN, IHMSF)
                  
                  STEMP = SIGN
                  WRITE (STEMP(2:3), '(I2.2)') IHMSF (1)
                  STEMP (4:4) = ' '
                  WRITE (STEMP(5:6), '(I2.2)') IHMSF (2)
                  STEMP (7:7) = ' '
                  WRITE (STEMP(8:9), '(I2.2)') IHMSF (3)
                  STEMP (10:10) = '.'
                  WRITE (STEMP(11:12), '(I2.2)') IHMSF (4)

                  CALL MSG_SETC ('LST', STEMP)
                  CALL MSG_SETR ('DAZ', POINT_DAZ(I))
                  CALL MSG_SETR ('DEL', POINT_DEL(I))

                  CALL MSG_OUTIF (MSG__NORM, ' ', 
     :                 ' - ^LST    ^DAZ ^DEL', STATUS)
               END DO

            END IF
         END IF

*     read in new set of pointing corrections if necessary

         CALL PAR_GET0L ('CHANGE_POINT', CHANGE_POINT, STATUS)
         IF (CHANGE_POINT .AND. (STATUS.EQ.SAI__OK)) THEN
            N_POINT = 0
            LOOPING = .TRUE.

            DO WHILE (LOOPING)
               CALL PAR_GET0C ('POINT_LST', STEMP, STATUS)
               CALL PAR_CANCL ('POINT_LST', STATUS)

               IF (STATUS .NE. SAI__OK) THEN
                  LOOPING = .FALSE.
               ELSE
                  ITEMP = 1
                  CALL SLA_DAFIN (STEMP, ITEMP, DTEMP, STATUS)
                  IF (STATUS .NE. 0) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC('TASK', TSKNAME)
                     CALL ERR_REP (' ', '^TASK: error reading '//
     :                    'sidereal time - it must be in 5 10 34.6 '//
     :                    'format', STATUS)
                     LOOPING = .FALSE.
                  ELSE IF (DTEMP .LT. 0.0D0) THEN
                     LOOPING = .FALSE.
                  ELSE
                     N_POINT = N_POINT + 1
                     POINT_LST (N_POINT) = DTEMP * 15.0D0
                     
                     CALL PAR_GET0R ('POINT_DAZ', POINT_DAZ(N_POINT),
     :                    STATUS)
                     CALL PAR_CANCL ('POINT_DAZ', STATUS)
                     CALL PAR_GET0R ('POINT_DEL', POINT_DEL(N_POINT),
     :                    STATUS)
                     CALL PAR_CANCL ('POINT_DEL', STATUS)
                  END IF
               END IF
               
            END DO

*     check that POINT_LST increases monotonically
*     Should really just sort the pointing corrections

            IF (STATUS .EQ. SAI__OK) THEN
               LST_OK = .TRUE.

               IF (N_POINT .GT. 1) THEN
                  DO I = 2, N_POINT
                     IF (POINT_LST(I) .LT. POINT_LST(I-1)) THEN
                        LST_OK = .FALSE.
                     END IF
                  END DO
               END IF

               IF (.NOT. LST_OK) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC('TASK', TSKNAME)
                  CALL ERR_REP (' ', '^TASK: sidereal times of '//
     :                 'pointing corrections are not monotonically '//
     :                 'increasing', STATUS)
               END IF
            END IF
         END IF

*     write out pointing corrections

         IF (N_POINT .EQ. 0) THEN
*     Remove an existing component
            IF (LST_THERE) THEN
               CALL DAT_ERASE(IN_REDSX_LOC, 'POINT_LST', STATUS)
               CALL DAT_ERASE(IN_REDSX_LOC, 'POINT_DAZ', STATUS)
               CALL DAT_ERASE(IN_REDSX_LOC, 'POINT_DEL', STATUS)
               IF (STATUS .EQ. SAI__OK) THEN 
                  CALL MSG_SETC('PKG', PACKAGE)
                  CALL MSG_OUTIF(MSG__NORM, 'POINT',
     :                 '^PKG: Erasing pointing corrections', 
     :                 STATUS)
               END IF

            END IF
         ELSE
            CALL CMP_MOD(IN_REDSX_LOC, 'POINT_LST', '_DOUBLE', 1,
     :           N_POINT, STATUS)
            CALL CMP_MOD(IN_REDSX_LOC, 'POINT_DAZ', '_REAL', 1, N_POINT,
     :           STATUS)
            CALL CMP_MOD(IN_REDSX_LOC, 'POINT_DEL', '_REAL', 1, N_POINT,
     :           STATUS)
            
            CALL CMP_PUT1D(IN_REDSX_LOC, 'POINT_LST', N_POINT,
     :           POINT_LST, STATUS)
            CALL CMP_PUT1R(IN_REDSX_LOC, 'POINT_DAZ', N_POINT,
     :           POINT_DAZ, STATUS)
            CALL CMP_PUT1R(IN_REDSX_LOC, 'POINT_DEL', N_POINT,
     :           POINT_DEL, STATUS)
         END IF

      ELSE
*     Just inform user that this was not a MAP obs

         STATUS = SAI__ERROR
         CALL MSG_SETC('TASK', TSKNAME)
         CALL ERR_REP(' ','^TASK: Pointing corrections'//
     :        ' can only be applied to MAP data', STATUS)

      END IF

*     close file and tidy up

      IF (IN_FITSX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      END IF

      IF (IN_REDSX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_REDSX_LOC, STATUS)
      END IF

      CALL NDF_END (STATUS)

      END
