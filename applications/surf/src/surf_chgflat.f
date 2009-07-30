      SUBROUTINE SURF_CHGFLAT (STATUS)
*+
*  Name:
*     CHANGE_FLAT

*  Purpose:
*     Change the flatfield in a SCUBA datafile

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL SURF_CHGFLAT( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     The flatfield information is stored inside each demodulated
*     data file and this task can be used to change the flatfield that is
*     stored internally. The new flatfield is read from a text file.

*  Usage:
*     change_flat in new_flat

*  ADAM Parameters:
*     IN = NDF (Read)
*         Name of NDF to change.
*     MSG_FILTER = CHAR (Read)
*         Message filter level (Default is NORM).
*     NEW_FLAT = CHAR (Read)
*         Name of the new flatfield file.

*  Related Application:
*     SURF: FLATFIELD, SCUQUICK

*  Examples:
*     change_flat test newflat.dat
*        This will change the flatfield stored in test.sdf to that stored
*        in newflat.dat.

*  Authors:
*     JFL:  J.Lightfoot (jfl@roe.ac.uk)
*     TIMJ: T.Jenness   (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     13 March 1997 (timj)
*         Make separate FLATFIELD task from MODIFY
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SURF_PAR'                         ! SURF constants
      INCLUDE 'MSG_PAR'                          ! MSG__ constants
      INCLUDE 'CNF_PAR'                          ! For CNF_PVAL function

*  Arguments Given:

*  Arguments Given & Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      BYTE SCULIB_BITON                          ! function to set a specified
                                                 ! bit in a byte 
*  Global variables:

*  Local Constants:
      INTEGER          MAX__DIM                  ! max number of dimensions in
      PARAMETER (MAX__DIM = 4)                   ! array
      CHARACTER * 11   TSKNAME                   ! Name of task
      PARAMETER (TSKNAME = 'CHANGE_FLAT')

*  Local variables:
      INTEGER          B                         ! DO loop index
      BYTE             BADBIT                    ! NDF badbit mask
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                                 ! ADC numbers of bolometers
                                                 ! used in the observation
      REAL             BOL_CALB (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! bolometer flatfield factors

      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                                 ! channel numbers of
                                                 ! bolometers used in the 
                                                 ! observation
      DOUBLE PRECISION BOL_DAY (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! time and day number on which
                                                 ! the bolometer flatfield was
                                                 ! measured
      REAL             BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! Nasmyth dU3 coords of 
                                                 ! bolometers
      REAL             BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! Nasmyth dU4 coords of 
                                                 ! bolometers
      INTEGER          BOL_QUAL (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! bolometer flatfield quality
      CHARACTER*20     BOL_REF (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! flatfield reference
                                                 ! bolometers
      REAL             BOL_RTEMP (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! scratch real bolometer data
      INTEGER          BOL_RUN (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! run number on which the
                                                 ! bolometer flatfield was
                                                 ! measured
      CHARACTER*20     BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! bolometer types
      INTEGER          DIM (MAX__DIM)            ! dimensions of array
      CHARACTER*80     FITS (SCUBA__MAX_FITS)    ! array of FITS keywords
      LOGICAL          FITS_CHANGED              ! .TRUE. if any FITS item is
                                                 ! changed
      LOGICAL          FLATFIELD                 ! .TRUE. if the FLATFIELD
                                                 ! application has been run on
                                                 ! the input file
      INTEGER          I                         ! DO loop index
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC        ! HDS locator of .FITS
                                                 ! extension
      INTEGER          IN_NDF                    ! NDF index of input file
      INTEGER          IN_QUALITY_PTR            ! pointer to QUALITY array
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC       ! HDS locator of .SCUBA
                                                 ! extension
      CHARACTER*(DAT__SZLOC) IN_SCUCDX_LOC       ! HDS locator of .SCUCD
                                                 ! extension
      INTEGER          ITEMP                     ! scratch integer
      INTEGER          NDIM                      ! number of dimensions in 
                                                 ! array
      CHARACTER*80     NEW_FLAT                  ! the name of the file
                                                 ! containing the new flatfield
      INTEGER          NREC                      ! number of history records
                                                 ! in input file
      INTEGER          N_BEAM                    ! the 'beam' dimension of the
                                                 ! data array
      INTEGER          N_BOLS                    ! number of bolometers
                                                 ! measured in observation
      INTEGER          N_FITS                    ! number of items in FITS
                                                 ! array
      INTEGER          N_POS                     ! number of positions measured
                                                 ! in observation
      CHARACTER*40     OBJECT                    ! name of observed object
      CHARACTER*40     OBSERVING_MODE            ! observing mode of file
      LOGICAL          PHOTOM                    ! .TRUE. if the PHOTOM
                                                 ! application has been run
                                                 ! on the input file
      INTEGER          QUALITY_BIT               ! the value to which the 3rd
                                                 ! quality bit is to be set
      LOGICAL          QUALITY_MAPPED            ! .TRUE. if the QUALITY array
                                                 ! has been mapped
      LOGICAL          REBIN                     ! .TRUE. if the REBIN
                                                 ! application has been run on
                                                 ! the input file
      LOGICAL          REDUCE_SWITCH             ! .TRUE. if the REDUCE_SWITCH
                                                 ! application has been run on
                                                 ! the input file
      INTEGER          RUN_NUMBER                ! run number of input file
      CHARACTER*80     STEMP                     ! scratch string

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN




* initialise some flags and locators

      FITS_CHANGED = .FALSE.
      QUALITY_MAPPED = .FALSE.

      IN_FITSX_LOC = DAT__NOLOC
      IN_SCUBAX_LOC = DAT__NOLOC
      IN_SCUCDX_LOC = DAT__NOLOC

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

         FLATFIELD = .FALSE.
         PHOTOM = .FALSE.
         REBIN = .FALSE.
         REDUCE_SWITCH = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (IN_NDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP(:9) .EQ. 'FLATFIELD') THEN
                  FLATFIELD = .TRUE.
               ELSE IF (STEMP(:6) .EQ. 'PHOTOM') THEN
                  PHOTOM = .TRUE.
               ELSE IF (STEMP(:5) .EQ. 'REBIN') THEN
                  REBIN = .TRUE.
               ELSE IF (STEMP(:13) .EQ. 'REDUCE_SWITCH') THEN
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
               CALL ERR_REP (' ', 'TSKNAME: this file contains '//
     :           'data that has already been rebinned', STATUS)
            END IF
         END IF
      END IF

*  get some locators

      CALL NDF_XLOC (IN_NDF, 'FITS', 'UPDATE', IN_FITSX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'UPDATE', IN_SCUBAX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUCD', 'READ', IN_SCUCDX_LOC, STATUS)

 
*  and read in some parameters describing the observation
     
      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: input file contains too '//
     :        'many FITS items', STATUS)
         END IF
      END IF

      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, N_FITS,
     :   STATUS)
      FITS_CHANGED = .FALSE.

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN',
     :  RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBJECT',
     :  OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :  OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETC ('MODE', OBSERVING_MODE)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM, ' ', 
     :     '^PKG: run ^RUN was a ^MODE observation of ^OBJECT',
     :     STATUS)


*     check that the file has not already been flat-fielded

      IF (STATUS .EQ. SAI__OK) THEN
         IF (FLATFIELD) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: the data file has '//
     :           'already been through the FLATFIELD application',
     :           STATUS)
         END IF
      END IF

*     set the badbit mask to include bit 1 - the one set by the flatfield
*     quality, then modify the quality array

      CALL NDF_BB (IN_NDF, BADBIT, STATUS)
      BADBIT = SCULIB_BITON (BADBIT, 1)
      CALL NDF_SBB (BADBIT, IN_NDF, STATUS)

*     get the name of the new flat-field file and read in the contents

      CALL PAR_GET0C ('NEW_FLAT', NEW_FLAT, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

         CALL SCULIB_READBOLS (NEW_FLAT, SCUBA__NUM_CHAN,
     :        SCUBA__NUM_ADC, BOL_TYPE, BOL_DU3, BOL_DU4, BOL_CALB,
     :        BOL_RTEMP, BOL_RTEMP, BOL_RTEMP, BOL_QUAL, BOL_DAY,
     :        BOL_RUN, BOL_REF, STATUS)

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETC('TASK', TSKNAME)
            CALL MSG_SETC('FILE', NEW_FLAT)
            CALL ERR_REP(' ', '^TASK: Error reading ^FILE flatfield'// 
     :           ' file',STATUS)
         END IF
      END IF

*     put the new data to the bolometer description arrays in the data file

      NDIM = 2
      DIM (1) = SCUBA__NUM_CHAN
      DIM (2) = SCUBA__NUM_ADC
      CALL CMP_PUTNC (IN_SCUBAX_LOC, 'BOL_TYPE', NDIM, DIM, BOL_TYPE,
     :     DIM, STATUS)
      CALL CMP_PUTNR (IN_SCUBAX_LOC, 'BOL_DU3', NDIM, DIM, BOL_DU3,
     :     DIM, STATUS)
      CALL CMP_PUTNR (IN_SCUBAX_LOC, 'BOL_DU4', NDIM, DIM, BOL_DU4,
     :     DIM, STATUS)
      CALL CMP_PUTNR (IN_SCUBAX_LOC, 'BOL_CALB', NDIM, DIM, BOL_CALB, 
     :     DIM, STATUS)
      CALL CMP_PUTNI (IN_SCUBAX_LOC, 'BOL_QUAL', NDIM, DIM, BOL_QUAL,
     :     DIM, STATUS)

*     write the name of the new flat-field file to the FITS header

      CALL SCULIB_REWRITE_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'FLAT', NEW_FLAT, STATUS)
      FITS_CHANGED = .TRUE.

*  read the number and A/D,channel of the bolometers used
 
      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'N_BOLS', N_BOLS, STATUS)
 
      CALL CMP_GET1I(IN_SCUBAX_LOC, 'BOL_CHAN', 
     :     SCUBA__NUM_CHAN * SCUBA__NUM_ADC, BOL_CHAN, ITEMP, STATUS)
      
      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_BOLS) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: dimension of '//
     :           '.SCUBA.BOL_CHAN does not match main data array',
     :           STATUS)
         END IF
      END IF
 
      CALL CMP_GET1I(IN_SCUBAX_LOC, 'BOL_ADC', 
     :     SCUBA__NUM_CHAN * SCUBA__NUM_ADC, BOL_ADC, ITEMP, STATUS)
      
      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_BOLS) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: dimension of '//
     :           '.SCUBA.BOL_ADC does not match main data array',
     :           STATUS)
         END IF
      END IF
      
*  map the quality array and check its dimensions
 
      CALL NDF_DIM (IN_NDF, MAX__DIM, DIM, NDIM, STATUS)
      CALL NDF_MAP (IN_NDF, 'QUALITY', '_UBYTE', 'UPDATE', 
     :     IN_QUALITY_PTR, ITEMP, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
         QUALITY_MAPPED = .TRUE.
      END IF
 
      N_POS = DIM (2)
 
      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :        OBSERVING_MODE .EQ. 'POLPHOT') THEN
            N_BEAM = DIM (3)
            IF ((NDIM .NE. 3)                  .OR.
     :           (DIM(1) .NE. N_BOLS)           .OR.
     :           (DIM(2) .LT. 1)                .OR.
     :           (DIM(3) .NE. SCUBA__MAX_BEAM)) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL MSG_SETC ('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data array '//
     :              'has bad dimensions - (^NDIM) ^DIM1 ^DIM2 ^DIM3',
     :              STATUS)
            END IF
         ELSE
            N_BEAM = 1
            IF ((NDIM .NE. 2)        .OR.
     :           (DIM(1) .NE. N_BOLS) .OR.
     :           (DIM(2) .LT. 1))     THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETC ('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data array '//
     :              'has bad dimensions - (^NDIM) ^DIM1 ^DIM2', STATUS)
            END IF
         END IF
      
*  reset the flatfield quality bit according to the new flatfield file
 
         DO B = 1, N_BOLS
            QUALITY_BIT = BOL_QUAL (BOL_CHAN(B), BOL_ADC(B))
            CALL SCULIB_SET_QUALITY (N_BOLS, N_POS, N_BEAM, 
     :           %VAL(CNF_PVAL(IN_QUALITY_PTR)), 
     :           B, B, 1, N_POS, 1, N_BEAM, 1,
     :           QUALITY_BIT, STATUS)
         END DO
      END IF

*     close file and tidy up

      IF (IN_FITSX_LOC .NE. DAT__NOLOC) THEN
         IF (FITS_CHANGED) THEN
            CALL DAT_PUT1C (IN_FITSX_LOC, N_FITS, FITS, STATUS)
         END IF
         CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      END IF
      IF (IN_SCUBAX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      END IF
      IF (IN_SCUCDX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)
      END IF

      CALL NDF_ANNUL(IN_NDF, STATUS)

      CALL NDF_END (STATUS)

      END
