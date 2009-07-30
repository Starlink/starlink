      SUBROUTINE SURF_CHGQUAL (STATUS)
*+
*  Name:
*     CHANGE_QUALITY

*  Purpose:
*     Set SCUBA data quality bad or good.

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL SURF_CHGQUAL( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This application is used to set SCUBA data quality bad or good by
*     using SCUBA sections to specify a subset of the full data.
*
*       Once the data specification has been decoded the application will
*     read from parameter BAD_QUALITY whether quality should be set good
*     or bad. A `yes' answer will mark the area bad, a `no' answer will
*     mark the area good (an area will only be good if no other QUALITY 
*     bits are set - CHANGE_QUALITY only uses QUALITY bit 3). The section
*     can be inverted by using the negation character at the end of the
*     section.

*  Usage:
*     change_quality ndf{spec1}{specn} bad_quality

*  ADAM Parameters:
*     BAD_QUALITY = LOGICAL (Read)
*         Set quality to BAD. Answering this question with a `yes' will
*         mean that the selected data will be set to BAD. `no'
*         will set them to good.
*     IN = CHAR (Read)
*         Name of data set and the specification of the data to be changed.
*         Usually of the form `ndf{spec1}{spec2}' where ndf is the filename
*         and spec1...n are the section specifications.
*         The section can be read from the SECTION parameter if the 
*         SCUBA section is omitted.
*     MSG_FILTER = CHAR (Read)
*         Message filter level. Default is NORM.
*     SECTION() = CHAR (Read)
*         This array parameter can be used to specify SCUBA sections.
*         Curly brackets must still be given. Since this is an array
*         parameter square brackets must be used to specify more than
*         one component:
* 
*             SECTION > [ {b3} , {i2} ]
*
*         would supply two SECTIONS of {b3} and {i2}. Only {b3} will
*         be used if the square brackets are not used. Care must also
*         be taken when using commas in SCUBA sections - the parameter
*         system will split multiple entries on commas unless the entire
*         section is quoted:
*
*             SECTION > [ "{b3,5}" , {i2} ]
*
*         If necessary the negation character should come after a
*         section (ie after the closing curly bracket) and that 
*         negation applies to the combined section and not just the string 
*         containing the negation character:
*
*             SECTION > [ {b3}-, {i2} ]
*
*         implies that the section consists of everything except bolometer 3
*         and integration 2.
*
*         This parameter is only used when no SCUBA section was specified
*         via the IN parameter.

*  Examples:
*     change_quality 'ndf{}' BAD_QUALITY=false
*         Select the entire array and unset bit 3.
*     change_quality 'ndf{b2}' BAD_QUALITY
*         Select the second bolometer and mark it bad.
*     change_quality 'ndf{b2;i3}-' BAD_QUALITY
*         Select the third integration of bolometer two but set all
*         other data points bad by inverting the section.
*     change_quality 'ndf{b16}{i2}' BAD_QUALITY
*         Select all of bolometer 16 and the whole of integration 2.
*     change_quality 'ndf{e5,16:18}' MSG_FILTER=quiet
*         Select exposure 5 and 16 through 18. Messaging is turned off.
*     change_quality ndf
*         Since no section has been specified, the user will be prompted
*         for a section later.
*     change_quality test SECTION='["{b41,52}",{i3}]' BAD_QUALITY
*         Set bolometers 41 and 52 as well as integration 3 to bad quality.
*         Use of SECTION here is not recommended given the complication
*         when using commas and square brackets.
*     change_quality test SECTION='[{b2;i2}-]' BAD_QUALITY
*         Set everything bad except bolometer 2 and integration 2.

*  Notes:
*     Samples are marked bad by setting bit 3 of the quality array. 
*     The effects of CHANGE_QUALITY  can be removed by changing the 
*     value of the bad bit mask (with the KAPPA task SETBB or by running 
*     CHANGE_QUALITY on the entire array [section is {} for entire array] 
*     but with BAD_QUALITY=false) so that bit 3 (decimal value of 8) is 
*     no longer used as a masking bit.

*  Related Application:
*     SURF: CHANGE_DATA, REBIN, SCUPHOT;
*     KAPPA: SETBB


*  Authors:
*     JFL:  J.Lightfoot (jfl@roe.ac.uk)
*     TIMJ: T.Jenness   (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'                   ! for DAT__SZLOC
      INCLUDE 'PRM_PAR'                   ! for VAL__NBx
      INCLUDE 'SURF_PAR'                  ! SURF constants
      INCLUDE 'MSG_PAR'                   ! MSG__ constants
      INCLUDE 'CNF_PAR'                   ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN                     ! Length of string
      EXTERNAL CHR_LEN
      BYTE SCULIB_BITON                   ! function to set a specified
                                          ! bit in a byte 
*  Local Constants:
      INTEGER          MAX__DIM           ! max number of dimensions in
      PARAMETER (MAX__DIM = 4)            ! array
      CHARACTER * 1    NEGCHAR            ! Character used to negate a section
      PARAMETER (NEGCHAR = '-')           ! 
      CHARACTER * 14   TSKNAME            ! Name of task
      PARAMETER (TSKNAME = 'CHANGE_QUALITY')
      INTEGER          BITNUM             ! Bit affected by this task
      PARAMETER (BITNUM = 3)

*  Local variables:
      BYTE             BADBIT             ! NDF badbit mask
      LOGICAL          BAD_QUALITY        ! .TRUE. if quality to be set
                                          ! bad, .FALSE. if to be set good
      BYTE             BTEMP              ! Value to fill empty array
      CHARACTER*128    DATA_SPEC(SCUBA__MAX_SECT) ! data-spec part of IN
      INTEGER          DIM (MAX__DIM)     ! dimensions of array
      CHARACTER*128    FILE               ! ndf name part of IN
      CHARACTER*80     FITS (SCUBA__MAX_FITS)
                                          ! array of FITS keywords
      LOGICAL          FLATFIELD          ! .TRUE. if the FLATFIELD
                                          ! application has been run on
                                          ! the input file
      INTEGER          I                  ! DO loop index
      CHARACTER*256    IN                 ! input filename and data-spec
      INTEGER          IN_DEM_PNTR_PTR    ! pointer to .SCUBA.DEM_PNTR
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC ! HDS locator of .FITS
                                          ! extension
      INTEGER          IN_NDF             ! NDF index of input file
      INTEGER          IN_QUALITY_PTR     ! pointer to QUALITY array
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                          ! HDS locator of .SCUBA extension
      INTEGER          ITEMP              ! scratch integer
      INTEGER          NDIM               ! number of dimensions in array
      INTEGER          NEGPOS             ! Position of NEGCHAR
      INTEGER          NREC               ! number of history records in
                                          ! input file
      INTEGER          N_BEAM             ! the 'beam' dimension of the
                                          ! data array
      INTEGER          N_BOLS             ! number of bolometers
                                          ! measured in observation
      INTEGER          N_EXPOSURES        ! number of exposures per
                                          ! integration
      INTEGER          N_FITS             ! number of items in FITS array
      INTEGER          N_INTEGRATIONS     ! number of integrations in
                                          ! measurement
      INTEGER          N_MEASUREMENTS     ! number of measurements in
                                          ! observation
      INTEGER          N_POS              ! number of positions measured
                                          ! in observation
      INTEGER          N_SPEC             ! Number of specifications
      CHARACTER*40     OBJECT             ! name of observed object
      CHARACTER*40     OBSERVING_MODE     ! observing mode of file
      LOGICAL          PHOTOM             ! .TRUE. if the PHOTOM
                                          ! application has been run
                                          ! on the input file
      LOGICAL          REBIN              ! .TRUE. if the REBIN
                                          ! application has been run on
                                          ! the input file
      LOGICAL          REDUCE_SWITCH      ! .TRUE. if the REDUCE_SWITCH
                                          ! application has been run on
                                          ! the input file
      INTEGER          RUN_NUMBER         ! run number of input file
      CHARACTER*80     STEMP              ! scratch string
      LOGICAL          SWITCH_EXPECTED    ! .TRUE. if switch is to be
                                          ! specified in data-spec
      LOGICAL          USE_SECT           ! Am I using the section or not?

*.

      IF (STATUS .NE. SAI__OK) RETURN




*     initialise some flags and locators

      IN_FITSX_LOC = DAT__NOLOC
      IN_SCUBAX_LOC = DAT__NOLOC

*     start up the NDF system and open the demodulated data file

      CALL NDF_BEGIN

*     read the filename and data-spec, separate them

      CALL PAR_GET0C ('IN', IN, STATUS)


*     Split up into filename and data spec
      CALL SCULIB_SPLIT_FILE_SPEC(IN, SCUBA__MAX_SECT, FILE, N_SPEC,
     :     DATA_SPEC, USE_SECT, STATUS)

*     open the data NDF

      CALL NDF_OPEN (DAT__ROOT, FILE, 'UPDATE', 'OLD', IN_NDF, ITEMP,
     :  STATUS)

*     check that the history of the file is OK

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
               CALL MSG_SETC('TASK',TSKNAME)
               CALL ERR_REP (' ', '^TASK: this file has not '//
     :              'been through the REDUCE_SWITCH application', 
     :              STATUS)
            END IF
            IF (PHOTOM) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK',TSKNAME)
               CALL ERR_REP (' ', '^TASK: this file contains '//
     :              'PHOTOM data that has already been reduced', STATUS)
            END IF
            IF (REBIN) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK',TSKNAME)
               CALL ERR_REP (' ', '^TASK: this file contains '//
     :              'data that has already been rebinned', STATUS)
            END IF
         END IF
      END IF

*     get some locators

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', IN_FITSX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)

*     and read in some parameters describing the observation
      
      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK',TSKNAME)
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

*     read the number of bolometers used

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'N_BOLS', N_BOLS, STATUS)

*     map in the DEM_PNTR array and get its dimensions

      CALL SCULIB_GET_DEM_PNTR (3, IN_SCUBAX_LOC, IN_DEM_PNTR_PTR,
     :  ITEMP, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, STATUS)

*     check there aren't too many measurements, integrations, exposures,
*     switches for this routine

      IF (STATUS .EQ. SAI__OK) THEN
         IF (N_MEASUREMENTS .GT. SCUBA__MAX_MEAS) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('N', N_MEASUREMENTS)
            CALL ERR_REP (' ', '^TASK: too many measurements - ^N',
     :        STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         IF (N_INTEGRATIONS .GT. SCUBA__MAX_INT) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('N', N_INTEGRATIONS)
            CALL ERR_REP (' ', '^TASK: too many integrations - ^N',
     :        STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         IF (N_EXPOSURES .GT. SCUBA__MAX_EXP) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('N', N_EXPOSURES)
            CALL ERR_REP (' ', '^TASK: too many exposures - ^N',
     :        STATUS)
         END IF
      END IF

*     map the quality array and check its dimensions

      CALL NDF_DIM (IN_NDF, MAX__DIM, DIM, NDIM, STATUS)
      CALL NDF_MAP (IN_NDF, 'QUALITY', '_UBYTE', 'UPDATE', 
     :  IN_QUALITY_PTR, ITEMP, STATUS)

      N_POS = DIM (2)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :        OBSERVING_MODE .EQ. 'POLPHOT') THEN
            N_BEAM = DIM (3)
            IF ((NDIM .NE. 3)                  .OR.
     :          (DIM(1) .NE. N_BOLS)           .OR.
     :          (DIM(2) .LT. 1)                .OR.
     :          (DIM(3) .NE. SCUBA__MAX_BEAM)) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :           'array has bad dimensions - (^NDIM) ^DIM1 '//
     :           '^DIM2 ^DIM3', STATUS)
            END IF
         ELSE
            N_BEAM = 1
            IF ((NDIM .NE. 2)        .OR.
     :          (DIM(1) .NE. N_BOLS) .OR.
     :          (DIM(2) .LT. 1))     THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :           'array has bad dimensions - (^NDIM) ^DIM1 '//
     :           '^DIM2', STATUS)
            END IF
         END IF
      END IF

      CALL MSG_SETI ('NBOLS', N_BOLS)
      CALL MSG_SETI ('NPOS', N_POS)
      CALL MSG_SETC('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM,' ', '^PKG: file has data for ^NBOLS '//
     :  'bolometers, measured at ^NPOS positions.', STATUS)
      CALL MSG_SETI ('NMEAS', N_MEASUREMENTS)
      CALL MSG_SETI ('NINT', N_INTEGRATIONS)
      CALL MSG_SETI ('NEXP', N_EXPOSURES)
      CALL MSG_OUTIF (MSG__NORM, ' ', 
     :     ' - there are data for ^NEXP exposure(s) '//
     :     'in ^NINT integration(s) in ^NMEAS measurements.', STATUS)

*     Ask for a parameter if no data specifications were given

      IF (N_SPEC .EQ. 0) THEN

         CALL PAR_GET1C('SECTION', SCUBA__MAX_SECT, DATA_SPEC,
     :        N_SPEC, STATUS)

*     Check to see if someone has slipped a NEGCHAR in there to negate it

         USE_SECT = .TRUE.

         DO I = 1, N_SPEC

            NEGPOS = 1
            CALL CHR_FIND(DATA_SPEC(I), NEGCHAR, .TRUE., NEGPOS)

*     If the string contains the character remove it and set USE_SECT
            IF (NEGPOS .LE. CHR_LEN(DATA_SPEC(I))) THEN
               USE_SECT = .FALSE.
               CALL CHR_RMCHR(NEGCHAR, DATA_SPEC(I))
            END IF

         END DO


      END IF


*     Report that we are using an inverted section if necessary
      IF (.NOT.USE_SECT) THEN

         CALL MSG_SETC('TASK', TSKNAME)

         CALL MSG_OUTIF(MSG__NORM,' ','^TASK: The inverse section '//
     :        'has been selected', STATUS)

      END IF



*     do we want to set quality bad or good

      CALL PAR_GET0L ('BAD_QUALITY', BAD_QUALITY, STATUS)

*     set the badbit mask to include bit 3 - the one set by this routine, then
*     modify the quality array

      CALL NDF_BB (IN_NDF, BADBIT, STATUS)
      BADBIT = SCULIB_BITON (BADBIT, BITNUM)
      CALL NDF_SBB (BADBIT, IN_NDF, STATUS)

*     and set quality for the selected bolometers and positions

      IF (STATUS .EQ. SAI__OK) THEN

         SWITCH_EXPECTED = .FALSE.

         CALL SCULIB_MASK_DATA(USE_SECT, 'BIT', N_SPEC, DATA_SPEC,
     :        %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)), 
     :        1, N_EXPOSURES, N_INTEGRATIONS,
     :        N_MEASUREMENTS, N_POS, N_BOLS, N_BEAM, SWITCH_EXPECTED,
     :        0.0, BTEMP, BITNUM, BAD_QUALITY, IN_QUALITY_PTR,
     :        STATUS)

      END IF

*     close file and tidy up

      IF (IN_FITSX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      END IF
      IF (IN_SCUBAX_LOC .NE. DAT__NOLOC) THEN
         CALL CMP_UNMAP (IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
         CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      END IF

      CALL NDF_ANNUL(IN_NDF, STATUS)
      CALL NDF_END (STATUS)

      END
