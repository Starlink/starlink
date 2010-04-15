*+  CGS3DR_REDRUN - Reduce a single run with/without skysub
      SUBROUTINE CGS3DR_REDRUN (RUNNUM, STATUS)
*    Description :
*     Performs a basic data reduction sequence on a single CGS3 run,
*     whether polarimetry or not, also reducing and using the previous
*     (sky) run if sky division was requested.
*    Invocation :
*     CALL CGS3DR_REDRUN (RUNNUM, STATUS)
*    Parameters :
*     RUNNUM = INTEGER (READ)
*           Run number to reduce
*     STATUS = INTEGER (UPDATE)
*           ADAM Status return
*    Method :
*     If status bad then return
*     Get observation to reduce.
*     Get if dividing by sky
*     Get if polarimetry
*     if (divbysky) reduce sky subspectrum
*     reduce object subspectrum
*     if (divbysky) divide object by sky
*     plot object
*    Deficiencies :
*     None Known
*    Bugs :
*     None Known
*    Authors :
*     A. Bridger (JAC::AB)
*     Jim Emerson (QMW::JPE)
*    History :
*      7-Nov-91: Original (JAC::AB)
*     14-Dec-92: Fully implement (JAC::AB)
*     15-Dec-92: Add polarimetry option and plotting (JAC::AB)
*      4-Jan-93: Replace TASK_OBEY and _DONE with CGS3DR_OBEYW (JAC::AB)
*      4-Jan-93: REDUCE_RUN split, producing this routine to actually
*                do the work (JAC::AB)
*      7-Jan-93: Add VERBOSE and PLOTTING controls (JAC::AB)
*     22-Feb-93: Make resistent to failures to get FITS items (JAC::AB)
*     28-Aug-93: Force ESPLOT to always use SOFT device (JAC::AB)
*     13-Jun-95: Add options to produce seperate output spectra for each cycle
*                (JAC::AB, after mods. by QMW::JPE)
*     23-Aug-95: Correct bug in div by sky control (JAC::AB)
*      5-Mar-96: use CGS3POL action from RED3 task, not tsp
*      6-Mar-96: change _P suffix for polarimetry output to lower case
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER  RUNNUM
*    Import-Export :
*     None
*    Export :
*     None
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN                         ! Length of a string
*    Global variables :
      INCLUDE 'CGS3DR_CMN'
*    Local Constants :
*     None
*    Local variables :
      LOGICAL      LDIVBYSKY          ! Local version of DIVBYSKY
      CHARACTER*80 COMMENT            ! String to put FITS comments in
      CHARACTER*80 RUNFIL,SKYFIL      ! Filename of object spec to reduce
      CHARACTER*80 RUNRED,SKYRED      ! Filename of object spec to reduce
      CHARACTER*80 POLRED(4)          ! Four filenames for pol. reduction
      CHARACTER*20 OBJECT             ! Object name
      CHARACTER*10 MODE               ! Observation mode
      CHARACTER*3  POL                ! Was polarizer in or out?
      CHARACTER*2  TMPSTR             ! temporary string
      CHARACTER*4  TMPSTRCY           ! temporary string '_single CYcle number'
      CHARACTER*74 STRINGS(6)         ! Strings to use for messages
      CHARACTER*440 INVAL, OUTVAL     ! ADAM Message strings
      INTEGER      SKYNUM             ! Sky run number to reduce
      INTEGER      ICYC               ! Cycle number loop variable
      INTEGER      I                  ! Loop variable
      INTEGER      NCYC               ! Number of cycles on object
      INTEGER      SCYC               ! Number of cycles on sky
      INTEGER      LCYCBEG            ! Local version of CYCBEG
      INTEGER      LCYCEND            ! Local version of CYCEND
      INTEGER      DIMS(4)	      ! Values of each dimension of data file
      INTEGER      NDIM               ! returned by DSA_DATA_SIZE
      INTEGER      ELEMENTS           ! returned by DSA_DATA_SIZE
*    Internal References :
*     None
*    Local data :
*     None
*-

*    If status bad then return
      IF (STATUS .NE. SAI__OK) RETURN

*    Get observation to reduce.
      CALL CGS3DR_GENRAWNAME (RUNNUM, RUNFIL, STATUS)

*    Open the file
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT ('RUN_FILE', RUNFIL, STATUS)

*    Determine dimensions of input.
      CALL DSA_DATA_SIZE ('RUN_FILE', 4, NDIM, DIMS, ELEMENTS,
     : STATUS)

      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP (' ', 'Failed to find dimensions ', STATUS)
      END IF

      NCYC  = DIMS(4)

*    check and change value of LCYCEND if zero
      LCYCEND = CYCEND
      LCYCBEG = CYCBEG
      IF (LCYCEND .EQ. 0) LCYCEND = NCYC

      IF (LCYCBEG .LE. 0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI ('CB', LCYCBEG)
         CALL ERR_REP (' ', 'Illegal value for CYCBEG (^CB)', STATUS)
         CALL DSA_CLOSE (STATUS)
         RETURN
      END IF
      IF (LCYCEND .GT. NCYC .OR. LCYCEND .LT. LCYCBEG) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI ('CE', LCYCEND)
         CALL ERR_REP (' ', 'Illegal value for CYCEND (^CE)', STATUS)
         CALL DSA_CLOSE (STATUS)
         RETURN
      END IF

      IF (VERBOSE) THEN
         CALL MSG_SETI ('CYC',NCYC)
         CALL MSG_OUT (' ','Observation contains ^CYC cycles', STATUS)
      END IF

*    Get if dividing by sky
      IF (DIVBYSKY) THEN
         CALL DSA_GET_FITS_L ('RUN_FILE', 'C3SKYDIV', 1, LDIVBYSKY,
     :    COMMENT, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP (' ', 'Failed to get SKYDIV from file', STATUS)
            CALL ERR_ANNUL (STATUS)
            CALL MSG_OUT (' ', 'Will not divide by sky' ,STATUS)
            LDIVBYSKY = .FALSE.
         ELSE
            IF (VERBOSE) THEN
               IF (LDIVBYSKY) THEN
                  CALL MSG_OUT (' ', 'Data are to be divided by sky',
     :             STATUS)
               ELSE
                  CALL MSG_OUT (' ', 'Data are not to be '/
     :             /'divided by sky', STATUS)
               END IF
            END IF
         END IF
      ELSE
         LDIVBYSKY = .FALSE.
         IF (VERBOSE) CALL MSG_OUT (' ', 'Data are not to be '/
     :    /'divided by sky', STATUS)
      END IF

*    Get if polarimetry
      CALL DSA_GET_FITS_C ('RUN_FILE', 'C3POL', 1, POL,
     : COMMENT, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP (' ', 'Failed to get C3POL', STATUS)
         CALL ERR_ANNUL (STATUS)
         CALL MSG_OUT (' ', 'Will use MODE instead', STATUS)
         POL  = 'Unk'
      END IF

      CALL DSA_GET_FITS_C ('RUN_FILE', 'MODE', 1, MODE,
     : COMMENT, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP (' ', 'Failed to get MODE', STATUS)
      END IF

      IF (POL .EQ. 'IN' .OR. MODE .EQ. 'LPCGS3') THEN
*       do one final check on object name, if the first 7 characters are
*       "SKY for" then this is a calibration sky and even if POL is IN and/
*       or MODE is LPCGS3 then do NOT treat it as polarimetry.
         CALL DSA_GET_FITS_C ('RUN_FILE', 'OBJECT', 1, OBJECT,
     :    COMMENT, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP (' ', 'Failed to get OBJECT', STATUS)
         END IF
         IF (OBJECT(:7) .EQ. 'SKY for') THEN
            POL = 'OUT'
         ELSE
            POL = 'IN'
         END IF
      END IF

      CALL DSA_CLOSE (STATUS)

*    if (divbysky) reduce sky subspectrum - assumed to be previous run
      IF (LDIVBYSKY) THEN

         SKYNUM = RUNNUM - 1
         CALL CGS3DR_GENRAWNAME (SKYNUM, SKYFIL, STATUS)

         IF (VERBOSE) THEN
*          Determine dimensions of input sky.
            CALL DSA_NAMED_INPUT ('SKY_FILE', SKYFIL, STATUS)
            CALL DSA_DATA_SIZE ('SKY_FILE', 4, NDIM, DIMS, ELEMENTS,
     :       STATUS)
            SCYC  = DIMS(4)
            CALL MSG_OUT (' ', 'Reducing Sky observation '/
     :       /SKYFIL(:CHR_LEN(SKYFIL)), STATUS)
            CALL MSG_SETI ('SYC',SCYC)
            CALL MSG_OUT (' ','Sky contains ^SYC cycles', STATUS)
            CALL MSG_OUT (' ', 'Reducing all of Sky observation '/
     :       /SKYFIL(:CHR_LEN(SKYFIL)), STATUS)
            CALL DSA_CLOSE (STATUS)
         ENDIF

         STRINGS(1) = SKYFIL(1:CHR_LEN(SKYFIL))
         CALL TASK_ENC0I (1, STRINGS(2), STATUS)
         CALL TASK_ENC0I (0, STRINGS(3), STATUS)
         CALL TASK_ENC0R (NSIGMA, STRINGS(4), STATUS)
         CALL CGS3DR_GENREDNAME (SKYNUM, SKYRED, STATUS)
         STRINGS(5) = SKYRED(1:CHR_LEN(SKYRED))
         CALL CHR_FILL( ' ', INVAL )
         CALL CHR_FILL( ' ', OUTVAL )
         CALL TASK_CNCAT (5, STRINGS, INVAL, STATUS)
         CALL CGS3DR_OBEYW (REDUCTION_TASK, 'CGS3_41', INVAL, OUTVAL,
     :    120000, STATUS)

*       plot it
         IF (PLOTTING) THEN
            STRINGS(1) =  SKYRED(1:CHR_LEN(SKYRED))
            STRINGS(2) = 'WHOLE=T'
            STRINGS(3) = 'AUTOSCALE=T'
            STRINGS(4) = 'HARDCOPY=F'
            STRINGS(5) = 'LABEL='//SKYRED(1:CHR_LEN(SKYRED))
            STRINGS(6) = '\\'
            CALL CHR_FILL( ' ', INVAL )
            CALL CHR_FILL( ' ', OUTVAL )
            CALL TASK_CNCAT (6, STRINGS, INVAL, STATUS)

            IF (VERBOSE) CALL MSG_OUT (' ', 'Plotting summed '/
     :       /'sky spectrum', STATUS)

            CALL CGS3DR_OBEYW (FIGARO_TASK, 'ESPLOT', INVAL, OUTVAL,
     :       120000, STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP (' ', 'Error plotting sky', STATUS)
            END IF
         END IF
      END IF

      IF (CYCBYCYC) THEN

*       reduce each cycle of the object spectrum separately
         IF (VERBOSE) THEN
            CALL MSG_OUT (' ', 'Reducing each cycle of object '/
     :       /'observation '//RUNFIL(:CHR_LEN(RUNFIL)), STATUS)
         END IF

         DO ICYC = LCYCBEG, LCYCEND
            IF (VERBOSE) THEN
               CALL MSG_SETI ('CYC',ICYC)
               CALL MSG_SETI ('CY',NCYC)
               CALL MSG_OUT (' ', 'Working on cycle ^CYC of ^CY',
     :          STATUS)
            END IF
            WRITE (TMPSTRCY, '(A2,I2.2)') '_C', ICYC

            STRINGS(1) = RUNFIL(1:CHR_LEN(RUNFIL))
            CALL TASK_ENC0I (ICYC, STRINGS(2), STATUS)
            CALL TASK_ENC0I (ICYC, STRINGS(3), STATUS)
            CALL TASK_ENC0R (NSIGMA, STRINGS(4), STATUS)
            CALL CGS3DR_GENREDNAME (RUNNUM, RUNRED, STATUS)
            STRINGS(5) = RUNRED(:CHR_LEN(RUNRED))//TMPSTRCY
            RUNRED = STRINGS(5)
            CALL CHR_FILL( ' ', INVAL )
            CALL CHR_FILL( ' ', OUTVAL )
            CALL TASK_CNCAT (5, STRINGS, INVAL, STATUS)

*          Use different action, depending on whether polarimetry or not
            IF (POL .NE. 'IN') THEN
               CALL CGS3DR_OBEYW (REDUCTION_TASK, 'CGS3_41', INVAL,
     :          OUTVAL, 120000, STATUS)
            ELSE
               CALL CGS3DR_OBEYW (REDUCTION_TASK, 'CGS3_42', INVAL,
     :          OUTVAL, 120000, STATUS)

*             and split up into the four waveplates
               STRINGS(1) = RUNRED(1:CHR_LEN(RUNRED))
               DO I = 1,4
                  CALL TASK_ENC0I (I, STRINGS(2), STATUS)
                  CALL TASK_ENC0I (I, STRINGS(3), STATUS)
                  WRITE (TMPSTR, '(A,I1)') '_',I
                  POLRED(I) = STRINGS(1)(:CHR_LEN(STRINGS(1)))//TMPSTR
                  IF (VERBOSE) THEN
                     CALL MSG_SETI ('PPOS', I)
                     CALL MSG_SETI ('CYC', ICYC)
                     CALL MSG_OUT (' ', 'Creating spectrum for cycle '/
     :                /'^CYC waveplate position ^PPOS, output file is '/
     :                /POLRED(I), STATUS)
                  END IF
                  STRINGS(4) = POLRED(I)
                  CALL CHR_FILL( ' ', INVAL )
                  CALL CHR_FILL( ' ', OUTVAL )
                  CALL TASK_CNCAT (4, STRINGS, INVAL, STATUS)
                  CALL CGS3DR_OBEYW (REDUCTION_TASK, 'EXTRACT3', INVAL,
     :             OUTVAL, 120000, STATUS)
               ENDDO

            END IF

*          if (divbysky) divide object by sky
            IF (STATUS .EQ. SAI__OK .AND. LDIVBYSKY) THEN

               IF (POL .NE. 'IN') THEN
                  STRINGS(1) = RUNRED(1:CHR_LEN(RUNRED))
                  STRINGS(2) = SKYRED(1:CHR_LEN(SKYRED))
                  STRINGS(3) = RUNRED(1:CHR_LEN(RUNRED))
                  CALL CHR_FILL( ' ', INVAL )
                  CALL CHR_FILL( ' ', OUTVAL )
                  CALL TASK_CNCAT (3, STRINGS, INVAL, STATUS)
                  IF (VERBOSE) THEN
                     CALL MSG_OUT (' ', 'Dividing object by sky',
     :                STATUS)
                  END IF
                  CALL CGS3DR_OBEYW (FIGARO_TASK, 'IDIV', INVAL, OUTVAL,
     :             120000, STATUS)
               ELSE
                  DO I = 1, 4
                     STRINGS(1) = POLRED(I)
                     STRINGS(2) = SKYRED(1:CHR_LEN(SKYRED))
                     STRINGS(3) = POLRED(I)
                     CALL CHR_FILL( ' ', INVAL )
                     CALL CHR_FILL( ' ', OUTVAL )
                     CALL TASK_CNCAT (3, STRINGS, INVAL, STATUS)
                     IF (VERBOSE) THEN
                        CALL MSG_SETI ('PPOS', I)
                        CALL MSG_OUT (' ', 'Dividing waveplate '/
     :                   /'position ^PPOS by sky', STATUS)
                     END IF
                     CALL CGS3DR_OBEYW (FIGARO_TASK, 'IDIV', INVAL,
     :                OUTVAL, 120000, STATUS)
                  END DO
               END IF
            END IF

*          Plot the result
            IF (POL .NE. 'IN') THEN
               IF (PLOTTING) THEN
                  STRINGS(1) = RUNRED(1:CHR_LEN(RUNRED))
                  STRINGS(2) = 'WHOLE=T'
                  STRINGS(3) = 'AUTOSCALE=T'
                  STRINGS(4) = 'HARDCOPY=F'
                  STRINGS(5) = 'LABEL='//RUNRED(1:CHR_LEN(RUNRED))
                  STRINGS(6) = '\\'
                  CALL CHR_FILL( ' ', INVAL )
                  CALL CHR_FILL( ' ', OUTVAL )
                  CALL TASK_CNCAT (6, STRINGS, INVAL, STATUS)
                  IF (VERBOSE) THEN
                     CALL MSG_SETI ('CYC', ICYC)
                     CALL MSG_OUT (' ', 'Plotting object spectrum for'/
     :                /' cycle ^CYC', STATUS)
                  END IF
                  CALL CGS3DR_OBEYW (FIGARO_TASK, 'ESPLOT', INVAL,
     :             OUTVAL, 120000, STATUS)
               END IF
            ELSE
*         for polarimetry, feed into reduction task (red3), not tsp as on Vax
               STRINGS(1) = POLRED(1)
               STRINGS(2) = POLRED(2)
               STRINGS(3) = POLRED(3)
               STRINGS(4) = POLRED(4)
               STRINGS(5) = RUNRED(:CHR_LEN(RUNRED))//'_p'
               CALL CHR_FILL( ' ', INVAL )
               CALL CHR_FILL( ' ', OUTVAL )
               CALL TASK_CNCAT (5, STRINGS, INVAL, STATUS)
               IF (VERBOSE) THEN
                  CALL MSG_SETI ('CYC', ICYC)
                  CALL MSG_OUT (' ', 'Creating TSP spectrum for cycle '/
     :             /'^CYC', STATUS)
               END IF
               CALL CGS3DR_OBEYW (REDUCTION_TASK, 'CGS3POL', INVAL,
     :          OUTVAL, 120000, STATUS)

*             ...and then plot
               IF (PLOTTING) THEN
                  STRINGS(1) = STRINGS(5)
                  STRINGS(2) = 'DEVICE="'/
     :                 /GDEVICE(1:CHR_LEN(GDEVICE))//'"'
                  STRINGS(3) = 'LABEL='//STRINGS(5)
                  STRINGS(4) = 'AUTO=T'
                  CALL CHR_FILL( ' ', INVAL )
                  CALL CHR_FILL( ' ', OUTVAL )
                  CALL TASK_CNCAT (4, STRINGS, INVAL, STATUS)
                  IF (VERBOSE) THEN
                     CALL MSG_SETI ('CYC', ICYC)
                     CALL MSG_OUT (' ', 'Plotting polarization '/
     :                /'spectrum for cycle ^CYC', STATUS)
                  END IF
                  CALL CGS3DR_OBEYW (TSP_TASK, 'EPLOT', INVAL, OUTVAL,
     :             120000, STATUS)
               END IF
            END IF
         END DO

      ELSE

*       sum requested cycles of the object spectrum
         STRINGS(1) = RUNFIL(1:CHR_LEN(RUNFIL))
         CALL TASK_ENC0I (LCYCBEG, STRINGS(2), STATUS)
         CALL TASK_ENC0I (LCYCEND, STRINGS(3), STATUS)
         CALL TASK_ENC0R (NSIGMA, STRINGS(4), STATUS)
         CALL CGS3DR_GENREDNAME (RUNNUM, RUNRED, STATUS)
         IF (VERBOSE) THEN
            IF (LCYCEND .NE. NCYC .OR. LCYCBEG .NE. 1) THEN
               CALL MSG_SETI ('SCYC', LCYCBEG)
               CALL MSG_SETI ('ECYC', LCYCEND)
               CALL MSG_OUT (' ', 'Reducing cycles ^SCYC to ^ECYC of '/
     :          /'object observation '//RUNFIL(:CHR_LEN(RUNFIL)),
     :          STATUS)
            ELSE
               CALL MSG_OUT (' ', 'Reducing all cycles of '/
     :          /'object observation '//RUNFIL(:CHR_LEN(RUNFIL)),
     :          STATUS)
            END IF
         END IF
         STRINGS(5) = RUNRED(1:CHR_LEN(RUNRED))
         CALL CHR_FILL( ' ', INVAL )
         CALL CHR_FILL( ' ', OUTVAL )
         CALL TASK_CNCAT (5, STRINGS, INVAL, STATUS)

*       Use different action, depending on whether polarimetry or not
         IF (POL .NE. 'IN') THEN
            CALL CGS3DR_OBEYW (REDUCTION_TASK, 'CGS3_41', INVAL, OUTVAL,
     :       120000, STATUS)
         ELSE
            CALL CGS3DR_OBEYW (REDUCTION_TASK, 'CGS3_42', INVAL, OUTVAL,
     :       120000, STATUS)

*          and split up into the four waveplates.
            STRINGS(1) = RUNRED(1:CHR_LEN(RUNRED))
            DO I = 1,4
               CALL TASK_ENC0I (I, STRINGS(2), STATUS)
               CALL TASK_ENC0I (I, STRINGS(3), STATUS)
               WRITE (TMPSTR, '(A,I1)') '_',I
               POLRED(I) = RUNRED(:CHR_LEN(RUNRED))//TMPSTR
               IF (VERBOSE) THEN
                  CALL MSG_SETI ('PPOS', I)
                  CALL MSG_OUT (' ', 'Creating spectrum for waveplate '/
     :             /'position ^PPOS', STATUS)
               END IF
               STRINGS(4) = POLRED(I)
               CALL CHR_FILL( ' ', INVAL )
               CALL CHR_FILL( ' ', OUTVAL )
               CALL TASK_CNCAT (4, STRINGS, INVAL, STATUS)
               CALL CGS3DR_OBEYW (REDUCTION_TASK, 'EXTRACT3', INVAL,
     :          OUTVAL, 120000, STATUS)
            ENDDO

         END IF

*       if (divbysky) divide object by sky
         IF (STATUS .EQ. SAI__OK .AND. LDIVBYSKY) THEN

            IF (POL .NE. 'IN') THEN
               STRINGS(1) = RUNRED(1:CHR_LEN(RUNRED))
               STRINGS(2) = SKYRED(1:CHR_LEN(SKYRED))
               STRINGS(3) = RUNRED(1:CHR_LEN(RUNRED))
               CALL CHR_FILL( ' ', INVAL )
               CALL CHR_FILL( ' ', OUTVAL )
               CALL TASK_CNCAT (3, STRINGS, INVAL, STATUS)
               IF (VERBOSE) CALL MSG_OUT (' ', 'Dividing object by sky',
     :          STATUS)
               CALL CGS3DR_OBEYW (FIGARO_TASK, 'IDIV', INVAL, OUTVAL,
     :          120000, STATUS)
            ELSE
               DO I = 1, 4
                  STRINGS(1) = POLRED(I)
                  STRINGS(2) = SKYRED(1:CHR_LEN(SKYRED))
                  STRINGS(3) = POLRED(I)
                  CALL CHR_FILL( ' ', INVAL )
                  CALL CHR_FILL( ' ', OUTVAL )
                  CALL TASK_CNCAT (3, STRINGS, INVAL, STATUS)
                  IF (VERBOSE) THEN
                     CALL MSG_SETI ('PPOS', I)
                     CALL MSG_OUT (' ', 'Dividing waveplate position '/
     :                /'^PPOS by sky', STATUS)
                  END IF
                  CALL CGS3DR_OBEYW (FIGARO_TASK, 'IDIV', INVAL, OUTVAL,
     :             120000, STATUS)
                  call dsa_close (status)  ! try this
               END DO
            END IF
         END IF

*       Plot the result
         IF (POL .NE. 'IN') THEN
            IF (PLOTTING) THEN
               STRINGS(1) = RUNRED(1:CHR_LEN(RUNRED))
               STRINGS(2) = 'WHOLE=T'
               STRINGS(3) = 'AUTOSCALE=T'
               STRINGS(4) = 'HARDCOPY=F'
               STRINGS(5) = 'LABEL='//RUNRED(1:CHR_LEN(RUNRED))
               STRINGS(6) = '\\'
               CALL CHR_FILL( ' ', INVAL )
               CALL CHR_FILL( ' ', OUTVAL )
               CALL TASK_CNCAT (6, STRINGS, INVAL, STATUS)
               IF (VERBOSE) CALL MSG_OUT (' ',
     :          'Plotting object spectrum', STATUS)
               CALL CGS3DR_OBEYW (FIGARO_TASK, 'ESPLOT', INVAL, OUTVAL,
     :          120000, STATUS)
            END IF
         ELSE
*          for polarimetry, feed into reduction task (red3), not tsp as on Vax
            STRINGS(1) = POLRED(1)
            STRINGS(2) = POLRED(2)
            STRINGS(3) = POLRED(3)
            STRINGS(4) = POLRED(4)
            STRINGS(5) = RUNRED(:CHR_LEN(RUNRED))//'_p'
            CALL CHR_FILL( ' ', INVAL )
            CALL CHR_FILL( ' ', OUTVAL )
            CALL TASK_CNCAT (5, STRINGS, INVAL, STATUS)
            IF (VERBOSE) CALL MSG_OUT (' ', 'Creating TSP spectrum',
     :       STATUS)
            CALL CGS3DR_OBEYW (REDUCTION_TASK, 'CGS3POL', INVAL, OUTVAL,
     :       120000, STATUS)

*          ...and then plot
            IF (PLOTTING) THEN
               STRINGS(1) = STRINGS(5)
               STRINGS(2) = 'DEVICE="'//GDEVICE(1:CHR_LEN(GDEVICE))//'"'
               STRINGS(3) = 'LABEL='//STRINGS(5)
               STRINGS(4) = 'AUTO=T'
               CALL CHR_FILL( ' ', INVAL )
               CALL CHR_FILL( ' ', OUTVAL )
               CALL TASK_CNCAT (4, STRINGS, INVAL, STATUS)
               IF (VERBOSE) CALL MSG_OUT (' ',
     :          'Plotting polarization spectrum', STATUS)

              CALL CGS3DR_OBEYW (TSP_TASK, 'EPLOT', INVAL, OUTVAL,
     :          120000, STATUS)


            END IF
         END IF
      END IF


      END
