      SUBROUTINE SURF_CHGDATA (STATUS)
*+
*  Name:
*     CHANGE_DATA

*  Purpose:
*     Set SCUBA data to any value.

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL SURF_CHGDATA( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This application is used to set SCUBA data to any value by using
*     SCUBA sections to specify a subset of the full data. Data, Variance
*     and Quality arrays can be modified.
*
*       Once the data specification has been decoded the application will
*     read from parameter VALUE the value of the data that should be used.
*     All data specified by the section (or by the inverse of this section
*     if specified) will be set to this value.

*  Usage:
*     change_data ndf{spec1}{spec2}{specn} value=??

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*         The name of the NDF array component which should be changed:
*         "Data","Error", "Quality" or "Variance" (where "Error" is the
*         alternative to "Variance" and causes the square root of the
*         variance values to be taken). The default component is always DATA.
*         If "Quality" is specified, then the quality values are treated
*         as numerical values (in the range 0 to 255).
*     IN = CHAR (Read)
*         Name of data set and the specification of the data to be changed.
*         Usually of the form `ndf{spec1}{spec2}' where ndf is the filename
*         and spec1...n are the section specifications.
*         The section can be read from the SECTION parameter if the 
*         SCUBA section is omitted.
*     MSG_FILTER = CHAR (Read)
*         Message filter level. Default is NORM.
*     OUT = NDF (Write)
*         Name of the NDF that stores the modified data.
*     SECTION() = CHAR (Read)
*         This parameter can be used to specify SCUBA sections.
*         Curly brackets must still be given. Since this is an array
*         parameter square brackets must be used to specify more than
*         one component: 
*
*               SECTION > [ {b3} , {i2} ]
*
*         would supply two SECTIONS of {b3} and {i2}. Only {b3} will
*         be used if the square brackets are not used. Care must also
*         be taken when using commas in SCUBA sections - the parameter
*         system will split multiple entries on commas unless the entire
*         section is quoted:
*
*               SECTION > [ "{b3,5}" , {i2} ]
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
*     VALUE = LITERAL (Read)
*         Value to which all selected data points should be set. A value
*         of `bad' will set the data point to VAL__BAD (Starlink bad data
*         value). For COMP=Quality only numbers 0 to 255 are allowed -
*         numbers outside this range are assumed to be bad values.

*  Examples:
*     change_data 'ndf{b2}' value=bad out=changed
*         Copy all data in ndf.sdf to changed.sdf and change all data
*         in bolometer 2 to bad.
*     change_data 'ndf{}' comp=variance value=0.0001
*         Copy ndf.sdf to the output file (asked for explicitly) and
*         set all variance values to 0.0001.
*     change_data test  section='[{b47},{i3}]' value=1.02
*         Select data from bolometer 47 and integration 3 in test.sdf and set
*         this to a value of 1.02. This method of selecting a section is not
*         recommended given the complication using commas and square
*         brackets.
*     change_data test2 section='["{b2,5}", {i2}-]' value=0.2 comp=err
*         Select everything except integration 2 and bolometers 2 and 5.
*         Set the error for this section to 0.2
*     change_data 'phot{i2:6}{b3}' comp=quality value=8
*         Explicitly set the quality array to 8 for integrations 2 through
*         6 and bolometer 3. The task CHANGE_QUALITY is recommended in this 
*         case since then only bit 3 is affected.
*     change_data 'map{i2,5}-' value=0.0 
*         Set everything except integrations 2 and 5 to zero.


*  Notes:
*     - This software sets the actual value in the specified component
*       and so, unlike CHANGE_QUALITY, is not reversible. For this reason 
*       a new output file is created.
*     - This task does not attempt to create a component if the specified
*       component is missing. A Variance array can be created using the 
*       KAPPA task SETVAR if necessary.
*     - The SECTION parameter is not used if a SCUBA section was given
*       via the IN parameter.

*  Related Application:
*     SURF: CHANGE_QUALITY, REBIN, SCUPHOT

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
      INCLUDE 'CNF_PAR'                   ! CNF_PVAL function

*  Arguments Given:

*  Arguments Given & Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN

*  Global variables:

*  Local Constants:
      INTEGER          MAX__DIM           ! max number of dimensions in
      PARAMETER (MAX__DIM = 4)            ! array
      CHARACTER * 1    NEGCHAR            ! Character used to negate a section
      PARAMETER (NEGCHAR = '-')           ! 
      CHARACTER * 14   TSKNAME            ! Name of task
      PARAMETER (TSKNAME = 'CHANGE_DATA')

*  Local variables:
      BYTE             BVALUE             ! Value of byte
      CHARACTER*10     CHG_TYPE           ! Type of array to change
      CHARACTER*10     COMP               ! Selected component
      CHARACTER*80     COMPLIS            ! List of allowed components
      CHARACTER*128    DATA_SPEC(SCUBA__MAX_SECT)  ! data-spec part of IN
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
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                          ! HDS locator of .SCUBA extension
      INTEGER          ITEMP              ! scratch integer
      LOGICAL          ISVAR              ! Is there a VARIANCE component
      LOGICAL          ISQUAL             ! Is there a QUALITY component
      INTEGER          IVALUE             ! Integer form of value
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
      INTEGER          N_SPEC             ! number of specifications
      CHARACTER*40     OBJECT             ! name of observed object
      CHARACTER*40     OBSERVING_MODE     ! observing mode of file
      CHARACTER*132    OUTFILE          ! Default output filename
      INTEGER          OUT_DATA_PTR       ! Mapped output data
      INTEGER          OUT_NDF            ! Output NDF identifier
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
      CHARACTER*80     SVALUE             ! String version of new value
      CHARACTER * (10) SUFFIX_STRINGS(SCUBA__N_SUFFIX) ! Suffix for OUT 
      CHARACTER*10     TYPE               ! Data type of component
      LOGICAL          USE_SECT           ! Use or not to use
      REAL             VALUE              ! New data value

*  Internal References:

*  Local data:
      DATA SUFFIX_STRINGS /'!_cdat','a','_cdat'/

*  External:
      INCLUDE 'NUM_DEC_CVT'               ! Convert UB to integer
      INCLUDE 'NUM_DEF_CVT'               ! Function definitions
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
      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)

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

*     Get number of output data points
      CALL NDF_DIM (IN_NDF, MAX__DIM, DIM, NDIM, STATUS)
      N_POS = DIM (2)

*     Generate a default name for the output file
      CALL SCULIB_CONSTRUCT_OUT(FILE, SUFFIX_ENV, SCUBA__N_SUFFIX,
     :     SUFFIX_OPTIONS, SUFFIX_STRINGS, OUTFILE, STATUS)
 
*     set the default
      CALL PAR_DEF0C('OUT', OUTFILE, STATUS)

*     Now propogate the output file from the input and open it.

      CALL NDF_PROP (IN_NDF, 'Data,Variance,Quality,Axis,Units', 'OUT', 
     :     OUT_NDF, STATUS)

*     close input file and tidy up

      IF (IN_FITSX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      END IF
      IF (IN_SCUBAX_LOC .NE. DAT__NOLOC) THEN
         CALL CMP_UNMAP(IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
         CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      END IF

      CALL NDF_ANNUL(IN_NDF, STATUS)

*     Find out whether Variance and Quality exist in output file

      CALL NDF_STATE(OUT_NDF, 'Variance', ISVAR, STATUS)
      CALL NDF_STATE(OUT_NDF, 'Quality', ISQUAL, STATUS)

*     Set up the choices
      COMPLIS = 'Data'   ! Always have data
      IF (ISVAR) CALL CHR_APPND(',Variance,Error', COMPLIS, 
     :     CHR_LEN(COMPLIS))
      
      IF (ISQUAL) CALL CHR_APPND(',Quality', COMPLIS, CHR_LEN(COMPLIS))

*     Now ask for the component of choice

      IF ( COMPLIS .EQ. 'Data' ) THEN
         COMP = 'DATA'
         TYPE = '_REAL'
      ELSE
         CALL PAR_CHOIC( 'COMP', 'Data', COMPLIS( :CHR_LEN(COMPLIS)),
     :        .FALSE., COMP, STATUS )
         TYPE = '_REAL'
         IF ( COMP .EQ. 'QUALITY') TYPE = '_UBYTE'
      END IF

*     map in the DEM_PNTR array and get its dimensions
      IN_SCUBAX_LOC = DAT__NOLOC
      CALL NDF_XLOC (OUT_NDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)
      CALL SCULIB_GET_DEM_PNTR (3, IN_SCUBAX_LOC, IN_DEM_PNTR_PTR,
     :  ITEMP, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, STATUS)

      
*     map the data array and check its dimensions

      CALL NDF_DIM (OUT_NDF, MAX__DIM, DIM, NDIM, STATUS)
      CALL NDF_MAP (OUT_NDF, COMP, TYPE, 'UPDATE', 
     :  OUT_DATA_PTR, ITEMP, STATUS)

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

*     Get the new value. Some complication with bytes

      CALL PAR_GET0C ('VALUE', SVALUE, STATUS)
      CALL CHR_UCASE(SVALUE)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (SVALUE .EQ. 'BAD') THEN
            VALUE = VAL__BADR
            IF (COMP .EQ. 'QUALITY') BVALUE = VAL__BADUB
         ELSE

            IF (COMP .EQ.'QUALITY') THEN
*     Convert to INT and then BYTE
               CALL CHR_CTOI(SVALUE, IVALUE, STATUS)

               IF (IVALUE .GT. NUM_UBTOI(VAL__MAXUB) .OR.
     :              IVALUE .LT. 0) THEN
                  BVALUE = VAL__BADUB
               ELSE
                  BVALUE = NUM_ITOUB(IVALUE)
               END IF

            ELSE
               CALL CHR_CTOR(SVALUE, VALUE, STATUS)
            END IF

            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_SETC('TYPE', TYPE)
               CALL MSG_SETC('VAL', SVALUE)
               CALL ERR_REP(' ','^TASK: Error converting ^VAL to'//
     :              ' ^TYPE', STATUS)
            END IF
         END IF
      END IF

*     decode each data specification

      IF (STATUS .EQ. SAI__OK) THEN

         IF (COMP .EQ. 'QUALITY') THEN
            CHG_TYPE = 'BYTE'
         ELSE
            CHG_TYPE = 'REAL'
         END IF

         SWITCH_EXPECTED = .FALSE.

         CALL SCULIB_MASK_DATA(USE_SECT, CHG_TYPE, N_SPEC, DATA_SPEC,
     :        %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)), 
     :        1, N_EXPOSURES, N_INTEGRATIONS,
     :        N_MEASUREMENTS, N_POS, N_BOLS, N_BEAM, SWITCH_EXPECTED,
     :        VALUE, BVALUE, 0, .TRUE., OUT_DATA_PTR,
     :        STATUS)


      END IF

*     Close file and tidy up

      IF (IN_SCUBAX_LOC .NE. DAT__NOLOC) THEN
         CALL CMP_UNMAP(IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
         CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      END IF

      CALL NDF_UNMAP(OUT_NDF, '*', STATUS)
      CALL NDF_ANNUL(OUT_NDF, STATUS)

      CALL NDF_END (STATUS)

      END
