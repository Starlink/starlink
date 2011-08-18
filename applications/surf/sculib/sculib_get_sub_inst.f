      SUBROUTINE SCULIB_GET_SUB_INST(PACKAGE, N_FITS, FITS, PARAM,
     :     N_SUB, SUB_POINTER, WAVE, INST, FILT, STATUS)
*+
*  Name:
*     SCULIB_GET_SUB_INST

*  Purpose:
*     Ask for the specific sub-instrument

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_GET_SUB_INST(PACKAGE, N_FITS, FITS, PARAM,
*    :     SUB_POINTER, WAVE, INST, FILT, STATUS)

*  Description:
*     This routine finds which sub-instruments are present in the
*     file. If there is only one this is returned, otherwise
*     the user is given a choice.

*  Arguments:
*     PACKAGE = _CHAR (Given)
*        Name of software package for informational message
*     N_FITS = _INTEGER (Given)
*        Number of FITS items
*     FITS(N_FITS) = _CHAR*80 (Given & Returned)
*        The FITS array
*     PARAM = _CHAR (Given)
*        Name of the ADAM parameter used to ask for sub instrument
*     N_SUB = _INTEGER (Returned)
*        Number of sub instruments in file
*     SUB_POINTER = _INTEGER (Returned)
*        Index of selected sub instrument
*     WAVE = _REAL (Returned)
*        Wavelength (microns) of selected sub instrument
*     INST = _CHAR (Returned)
*        Name of selected sub instrument
*     FILT = _CHAR (Returned)
*        Name of selected filter
*     STATUS = _INTEGER (Given & Returned)
*        Global status

*  Notes:
*     I actually need to include REDS_SYS (for SCUBA__MAX_SUB) but I am
*     doing it by hand at the moment.

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL:  John Lightfoot (RoE)


*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1997 March 27 (TIMJ)
*        Extract from main tasks
*     2011-08-18 (TIMJ):
*        Update all _N FITS headers and not just the ones being returned.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! for VAL__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER         N_FITS
      CHARACTER * (*) PACKAGE
      CHARACTER * (*) PARAM

*  Arguments given & returned:
      CHARACTER * (*) FITS(N_FITS)

*  Arguments Returned:
      CHARACTER * (*) FILT
      CHARACTER * (*) INST
      INTEGER         N_SUB
      INTEGER         SUB_POINTER
      REAL            WAVE

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER  SCUBA__MAX_SUB    ! I need to include REDS_SYS!!!
      PARAMETER (SCUBA__MAX_SUB = 5)
*  Local Variables:
      INTEGER I                         ! Loop counter
      INTEGER IPOSN                     ! Position in string
      INTEGER ITEMP                     ! Dummy variable
      CHARACTER*80 STEMP                ! Temp string
      CHARACTER*40 SUBLIST              ! List of available sub instruments
      CHARACTER*15 SUB_FILTER (SCUBA__MAX_SUB)
                                        ! filters in front of sub-instruments
      CHARACTER*15 SUB_INSTRUMENT (SCUBA__MAX_SUB)
                                        ! sub-instruments used
      CHARACTER*15 SUB_REQUIRED         ! sub-instrument required for reduction
      REAL    SUB_WAVE (SCUBA__MAX_SUB) ! wavelengths of observation



*.
      IF (STATUS .NE. SAI__OK) RETURN

*  find and report the sub instruments used and filters for this observation

      CALL SCULIB_GET_FITS_I (N_FITS, N_FITS, FITS, 'N_SUBS',
     :  N_SUB, STATUS)

      CALL MSG_SETC('PKG',PACKAGE)
      CALL MSG_OUT (' ', '^PKG: file contains data for the '//
     :  'following sub-instrument(s)', STATUS)

      IF (N_SUB .GT. 0) THEN

*     Get SUB_instrument list
         STEMP = 'SUB_'
         DO I = 1, N_SUB
            ITEMP = 4
            CALL CHR_PUTI (I, STEMP, ITEMP)
            CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS,
     :        STEMP, SUB_INSTRUMENT(I), STATUS)
            CALL CHR_UCASE (SUB_INSTRUMENT(I))
         END DO

*     Get FILTer list
         STEMP = 'FILT_'
         DO I = 1, N_SUB
            ITEMP = 5
            CALL CHR_PUTI (I, STEMP, ITEMP)
            CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS,
     :        STEMP, SUB_FILTER(I), STATUS)
            CALL CHR_UCASE (SUB_FILTER(I))
         END DO

*       Get WAVElength list
         STEMP = 'WAVE_'
         DO I = 1, N_SUB
            ITEMP = 5
            CALL CHR_PUTI (I, STEMP, ITEMP)
            CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS,
     :        STEMP, SUB_WAVE(I), STATUS)
         END DO

*       List out all the sub-instruments and filters
         DO I = 1, N_SUB
            CALL MSG_SETC ('SUB', SUB_INSTRUMENT(I))
            CALL MSG_SETC ('FILT', SUB_FILTER(I))
            CALL MSG_OUT (' ', ' - ^SUB with filter ^FILT', STATUS)
         END DO
      END IF

*  get the sub-instrument of interest and check it's OK

      IF (N_SUB .EQ. 1) THEN

*  If we only have one wavelength we dont need to ask

         SUB_POINTER = 1
         SUB_REQUIRED = SUB_INSTRUMENT(SUB_POINTER)
      ELSE
         SUB_POINTER = VAL__BADI

*  Put all possible answers in a string

         IF (N_SUB .GT. 0) THEN
            SUBLIST = ' '
            IPOSN = 0
            CALL CHR_APPND(SUB_INSTRUMENT(1), SUBLIST, IPOSN)
            DO I = 2, N_SUB
               CALL CHR_APPND(',',SUBLIST,IPOSN)
               CALL CHR_APPND(SUB_INSTRUMENT(I), SUBLIST, IPOSN)
            END DO

*  Ask for the sub array

            CALL PAR_CHOIC('SUB_INSTRUMENT', SUB_INSTRUMENT(1), SUBLIST,
     :           .TRUE., SUB_REQUIRED, STATUS)
            CALL CHR_UCASE (SUB_REQUIRED)

            DO I = 1, N_SUB
               IF (SUB_REQUIRED .EQ. SUB_INSTRUMENT(I)) THEN
                  SUB_POINTER =I
               END IF
            END DO
         END IF

*     This shouldnt be needed since we are using PAR_CHOIC
         IF (STATUS .EQ. SAI__OK) THEN
            IF (SUB_POINTER .EQ. VAL__BADI) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC ('SUB', SUB_REQUIRED)
               CALL ERR_REP (' ', 'SCULIB_GET_SUB_INST: the file '//
     :              'does not contain data for sub-instrument ^SUB',
     :              STATUS)
            END IF
         END IF
      END IF

*  modify the FITS keywords to reflect the fact that the data only come
*  from 1 sub-instrument in the output file

*     Check status since SUB_POINTER can be set to bad (and therefore
*     ruin the array lookup) if status was not good.
      IF (STATUS .EQ. SAI__OK) THEN

         CALL SCULIB_REWRITE_FITS_I (N_FITS, N_FITS, FITS,
     :        'N_SUBS', 1, STATUS)
         CALL SCULIB_REWRITE_FITS_C (N_FITS, N_FITS, FITS,
     :        'SUB_1', SUB_INSTRUMENT(SUB_POINTER), STATUS)
         CALL SCULIB_REWRITE_FITS_C (N_FITS, N_FITS, FITS,
     :        'FILT_1', SUB_FILTER(SUB_POINTER), STATUS)
         CALL SCULIB_REWRITE_FITS_R (N_FITS, N_FITS, FITS,
     :        'WAVE_1', SUB_WAVE(SUB_POINTER), STATUS)

*     Also need to update T_COLD, ETATEL and TAUZ to keep the header
*     consistent
         CALL SCULIB__UPDATE_SUBITEM( N_FITS, FITS,
     :        SUB_POINTER, 'TAUZ_', STATUS )
         CALL SCULIB__UPDATE_SUBITEM( N_FITS, FITS,
     :        SUB_POINTER, 'T_COLD_', STATUS )
         CALL SCULIB__UPDATE_SUBITEM( N_FITS, FITS,
     :        SUB_POINTER, 'ETATEL_', STATUS )


*     Setup current values of wavelength and filter
         WAVE = SUB_WAVE(SUB_POINTER)
         INST = SUB_INSTRUMENT(SUB_POINTER)
         FILT = SUB_FILTER(SUB_POINTER)

      END IF

      END

      SUBROUTINE SCULIB__UPDATE_SUBITEM( N_FITS, FITS,
     :     CURPOS, ROOT, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

      INTEGER N_FITS
      CHARACTER*(*) FITS(N_FITS)
      INTEGER CURPOS
      CHARACTER*(*) ROOT
      INTEGER STATUS

      REAL CURVAL
      CHARACTER *(8) STEMP
      INTEGER ITEMP


      IF (STATUS .NE. SAI__OK) RETURN

      STEMP = ROOT
      ITEMP = CHR_LEN(ROOT)
      CALL CHR_PUTI( CURPOS, STEMP, ITEMP )

*     Get the current value
      CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS,
     :     STEMP, CURVAL, STATUS)

*     Build up the new card with index 1
      ITEMP = CHR_LEN(ROOT)
      CALL CHR_PUTI( 1, STEMP, ITEMP )

*     and update the header
      CALL SCULIB_REWRITE_FITS_R (N_FITS, N_FITS, FITS,
     :     STEMP, CURVAL, STATUS)

      END
