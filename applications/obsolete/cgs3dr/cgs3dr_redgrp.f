*+  CGS3DR_REDGRP - REDUCE a group of CGS3 runs
      SUBROUTINE CGS3DR_REDGRP (GRPNUM, STATUS)
*    Description :
*     Performs a basic data reduction sequence on a group of CGS3 runs,
*     merging as appropriate. Calculates which runs to reduce from
*     header information in the FIRST group, assumed to be in GRPNUM.
*    Invocation :
*     CALL CGS3DR_REDGRP (GRPNUM, STATUS)
*    Parameters :
*     GRPNUM = INTEGER (READ)
*           Number of first OBJECT run in the group.
*     STATUS = INTEGER (UPDATE)
*           ADAM Status return
*    Method :
*     If status bad then return
*     Get first observation to reduce
*     Get if dividing by sky
*     Get nu,mber of subspectra to expect
*     if (divbysky) increment between runs = 2, otherwise 1.
*     for all subspectra
*        reduce run
*        if first run then
*           create group spectrum
*        else
*           merge with group spectrum
*           plot group spectrum
*        end if
*     end for
*    Deficiencies :
*     None Known
*    Bugs :
*     None Known
*    Authors :
*     A. Bridger (JAC::AB)
*    History :
*      4-Jan-93: Original (JAC::AB)
*      7-Jan-93: Add VERBOSE and PLOTTING control (JAC::AB)
*     22-Feb-93: Add checks to ensure is start of a group (JAC::AB)
*     28-Aug-93: Force ESPLOT to always use SOFT device (JAC::AB)
*     14-Jun-95: Don't allow group reduction if CYCBYCYC is TRUE (JAC::AB)
*     20-Dec-95: increase length of strings (KK)
*     26-Dec-95: no abbreviated key words sent to other tasks
*      5-Mar-96: change CGS3POL from tsp task to reduction task (red3)
*      6-Mar-96: change _P suffix for polarimetry output file to lower case
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER GRPNUM
*    Import-Export :
*     None
*    Export :
*     None
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER  CHR_LEN                ! Length of string
*    Global variables :
      INCLUDE 'CGS3DR_CMN'
*    Local Constants :
*     None
*    Local variables :
      LOGICAL      LDIVBYSKY          ! Local version of DIVBYSKY
      LOGICAL      FDIVBYSKY          ! FITS version of DIVBYSKY
      INTEGER      RUNNUM             ! Run number to reduce
      INTEGER      INCR               ! Increment between runs
      INTEGER      NSPEC              ! Number of subspectra
      INTEGER      SSPEC              ! Number of subspectrum
      INTEGER      I                  !
      CHARACTER*80 GRPFIL,RUNRED      ! Filename of first object run and
     :                                !  reduced run
      CHARACTER*80 POLGRP(4)          ! Filename of 4 pol grps.
      CHARACTER*80 POLRED(4)          ! Filename of 4 pol reduced runs.
      CHARACTER*80 COMMENT            ! String to put FITS comments in
      CHARACTER*74 STRINGS(6)         ! Strings to use for messages
      CHARACTER*440 INVAL, OUTVAL     ! ADAM Message strings
      CHARACTER*10 MODE               ! observing mode
      CHARACTER*3  POL                ! If polarimetry
      CHARACTER*2  TMPSTR             ! Temporary string
*    Internal References :
*     None
*    Local data :
*     None
*-

*    If status bad then return
      IF (STATUS .NE. SAI__OK) RETURN

*    do not continue if CYCBYCYC is true
      IF (CYCBYCYC) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'Reduction of groups is not allowed'/
     :    /' with CYCBYCYC set to TRUE', STATUS)
      END IF

*    Get first observation to reduce.
      CALL CGS3DR_GENRAWNAME (GRPNUM, GRPFIL, STATUS)

*    Open the file
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT ('GRP_FILE', GRPFIL, STATUS)

*    Get if dividing by sky
      IF (STATUS .EQ. SAI__OK) THEN
         CALL DSA_GET_FITS_L ('GRP_FILE', 'C3SKYDIV', 1, FDIVBYSKY,
     :    COMMENT, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP (' ' ,'Failed to get C3SKYDIV', STATUS)
            CALL ERR_ANNUL (STATUS)
            LDIVBYSKY = .FALSE.
            CALL MSG_OUT (' ',' Will NOT divide by sky', STATUS)
         ELSE
            IF (DIVBYSKY) LDIVBYSKY = FDIVBYSKY
         END IF
      ENDIF

      IF (STATUS .EQ. SAI__OK) THEN
         IF (FDIVBYSKY) THEN
            INCR = 2
         ELSE
            INCR = 1
         END IF

*       Get number of subspectra
         CALL DSA_GET_FITS_I ('GRP_FILE', 'C3NSPECT', 1, NSPEC,
     :    COMMENT, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP (' ' ,'Failed to get C3NSPECT', STATUS)
            CALL ERR_REP (' ','Cannot reduce group', STATUS)
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            CALL DSA_GET_FITS_I ('GRP_FILE', 'C3SSPECT', 1, SSPEC,
     :       COMMENT, STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP (' ' ,'Failed to get C3SSPECT', STATUS)
               CALL ERR_REP (' ','Cannot reduce group', STATUS)
            END IF
            IF (STATUS .EQ. SAI__OK .AND. SSPEC .NE. 1) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('SSPEC', SSPEC)
               CALL ERR_REP (' ','Subspectrum no. = ^SSPEC', STATUS)
               CALL ERR_REP (' ','Not the start of a group', STATUS)
            END IF
         END IF
      END IF

*    Get if polarimetry
      IF (STATUS .EQ. SAI__OK) THEN
         CALL DSA_GET_FITS_C ('GRP_FILE', 'C3POL', 1, POL,
     :       COMMENT, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP (' ' ,'Failed to get C3POL', STATUS)
            CALL ERR_ANNUL (STATUS)
            POL = 'Unk'
            CALL MSG_OUT (' ','Will use MODE instead', STATUS)
         END IF
         CALL DSA_GET_FITS_C ('GRP_FILE', 'MODE', 1, MODE,
     :    COMMENT, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP (' ' ,'Failed to get MODE', STATUS)
            CALL ERR_REP (' ','Cannot reduce group', STATUS)
         END IF
      END IF

      IF (POL .EQ. 'IN' .OR. MODE .EQ. 'LPCGS3') POL = 'IN'

      CALL DSA_CLOSE (STATUS)


*    reduce the group
      IF (STATUS .EQ. SAI__OK) THEN

         DO RUNNUM = GRPNUM, GRPNUM + INCR*(NSPEC-1), INCR

*          reduce the run
            CALL MSG_SETI ('RUN', RUNNUM)
            CALL MSG_OUT (' ', 'Reducing run ^RUN', STATUS)
            CALL CGS3DR_REDRUN (RUNNUM, STATUS)
            CALL CGS3DR_GENREDNAME (RUNNUM, RUNRED, STATUS)

*          if the first run then create group file name and the file itself
            IF (RUNNUM .EQ. GRPNUM) THEN

               CALL CGS3DR_GENGRPNAME (GRPNUM, GRPFIL, STATUS)
               IF (POL .NE. 'IN') THEN
                  CALL DSA_OPEN (STATUS)
                  CALL DSA_NAMED_INPUT ('RUN_FILE', RUNRED, STATUS)
                  CALL DSA_NAMED_OUTPUT ('GRP_FILE', GRPFIL, 'RUN_FILE',
     :             0, 0, STATUS)
                  CALL DSA_CLOSE (STATUS)
               ELSE
                  DO I = 1,4
                     WRITE (TMPSTR, '(A,I1)') '_',I
                     POLRED(I) = RUNRED(:CHR_LEN(RUNRED))//TMPSTR
                     POLGRP(I) = GRPFIL(:CHR_LEN(GRPFIL))//TMPSTR
                     CALL DSA_OPEN (STATUS)
                     CALL DSA_NAMED_INPUT ('RUN_FILE', POLRED(I),
     :                STATUS)
                     CALL DSA_NAMED_OUTPUT ('GRP_FILE', POLGRP(I),
     :                'RUN_FILE', 0, 0, STATUS)
                     CALL DSA_CLOSE (STATUS)
                  END DO
               END IF

*          else merge the spectra
            ELSE

               IF (POL .NE. 'IN') THEN

*                merge with group file.
                  STRINGS(1) = GRPFIL(1:CHR_LEN(GRPFIL))
                  IF (VERBOSE) THEN
                     CALL MSG_SETI ('RUN', RUNNUM)
                     CALL MSG_OUT (' ', 'Merging run ^RUN with group '/
     :                /'file', STATUS)
                  END IF
                  STRINGS(2) = RUNRED(1:CHR_LEN(RUNRED))
                  STRINGS(3) = GRPFIL(1:CHR_LEN(GRPFIL))
                  CALL CHR_FILL( ' ', INVAL )
                  CALL CHR_FILL( ' ', OUTVAL )
                  CALL TASK_CNCAT (3, STRINGS, INVAL, STATUS)
                  CALL CGS3DR_OBEYW (FIGARO_TASK, 'ADJOIN', INVAL,
     :             OUTVAL, 120000, STATUS)

               ELSE
                  DO I = 1,4
                     STRINGS(1) = POLGRP(I)
                     WRITE (TMPSTR, '(A,I1)') '_',I
                     POLRED(I) = RUNRED(:CHR_LEN(RUNRED))//TMPSTR
                     STRINGS(2) = POLRED(I)
                     STRINGS(3) = POLGRP(I)
                     CALL CHR_FILL( ' ', INVAL )
                     CALL CHR_FILL( ' ', OUTVAL )
                     CALL TASK_CNCAT (3, STRINGS, INVAL, STATUS)
                     IF (VERBOSE) THEN
                        CALL MSG_SETI ('PPOS', I)
                        CALL MSG_OUT (' ', 'Merging spectra for '/
     :                   /'waveplate position ^PPOS', STATUS)
                     END IF
                     CALL CGS3DR_OBEYW (FIGARO_TASK, 'ADJOIN', INVAL,
     :                OUTVAL, 120000, STATUS)
                  END DO
               END IF

            END IF

         END DO

*       When merging is finished plot the result (for polarimetry also feed
*        into TSP)
         IF (POL .NE. 'IN') THEN
            STRINGS(1) = GRPFIL(1:CHR_LEN(GRPFIL))
            STRINGS(2) = 'WHOLE=T'
            STRINGS(3) = 'AUTOSCALE=T'
            STRINGS(4) = 'HARDCOPY=F'
            STRINGS(5) = 'LABEL='//GRPFIL
            STRINGS(6) = '\\'
            CALL CHR_FILL( ' ', INVAL )
            CALL CHR_FILL( ' ', OUTVAL )
            CALL TASK_CNCAT (6, STRINGS, INVAL, STATUS)
            IF (PLOTTING) THEN
               IF (VERBOSE) CALL MSG_OUT (' ',
     :          'Plotting merged object spectrum', STATUS)
               CALL CGS3DR_OBEYW (FIGARO_TASK, 'ESPLOT', INVAL, OUTVAL,
     :          120000, STATUS)
            END IF
         ELSE
*          for polarimetry, feed into TSP
            STRINGS(1) = POLGRP(1)
            STRINGS(2) = POLGRP(2)
            STRINGS(3) = POLGRP(3)
            STRINGS(4) = POLGRP(4)
            STRINGS(5) = GRPFIL(1:CHR_LEN(GRPFIL))//'_p'
            CALL CHR_FILL( ' ', INVAL )
            CALL CHR_FILL( ' ', OUTVAL )
            CALL TASK_CNCAT (5, STRINGS, INVAL, STATUS)
            IF (VERBOSE) CALL MSG_OUT (' ',
     :       'Creating TSP spectrum', STATUS)
            CALL CGS3DR_OBEYW (REDUCTION_TASK, 'CGS3POL', INVAL, OUTVAL,
     :       120000, STATUS)

*          ...and then plot
            STRINGS(1) = STRINGS(5)
            STRINGS(2) = 'DEVICE="'//GDEVICE(1:CHR_LEN(GDEVICE))//'"'
            STRINGS(3) = 'LABEL='//STRINGS(5)
            STRINGS(4) = 'AUTOSCALE=T'
            CALL CHR_FILL( ' ', INVAL )
            CALL CHR_FILL( ' ', OUTVAL )
            CALL TASK_CNCAT (4, STRINGS, INVAL, STATUS)
            IF (PLOTTING) THEN
               IF (VERBOSE) CALL MSG_OUT (' ',
     :          'Plotting polarization spectrum', STATUS)
               CALL CGS3DR_OBEYW (TSP_TASK, 'EPLOT', INVAL, OUTVAL,
     :          120000, STATUS)
            END IF

         END IF

      END IF


      END
