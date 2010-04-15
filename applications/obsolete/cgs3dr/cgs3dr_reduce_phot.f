*+  CGS3DR_REDUCE_PHOT - Reduce a single run with/without skysub
      SUBROUTINE CGS3DR_REDUCE_PHOT (STATUS)
*    Description :
*     Extracts scans from an individual CGS3 run and produces a raw
*     photometric measure by binning things together.
*    Invocation :
*     CALL CGS3DR_REDUCE_PHOT (STATUS)
*    Parameters :
*     STATUS = INTEGER (UPDATE)
*           ADAM Status return
*    Method :
*     If status bad then return
*     Get observation to reduce.
*    Deficiencies :
*     None Known
*    Bugs :
*     None Known
*    Authors :
*     K. Krisciunas (jach::kevin)
*    History :
*     20-Nov-95: Original (JAC::kevin)
*     12-Dec-95: make output file name explicitly lower case
*     20-Dec-95: increase length of strings
*     26-Dec-95: put in par_cancl, add .dat for output file
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
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
      INTEGER  PHOTNUM

      CHARACTER*80 RUNFIL             ! Filename of object spec to reduce
      CHARACTER*80 RUNRED             ! Filename of object spec to reduce
      CHARACTER*74 STRINGS(8)         ! Strings to use for messages
      CHARACTER*440 INVAL, OUTVAL     ! ADAM Message strings
      INTEGER      NCYC               ! Number of cycles on object
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
      CALL PAR_GET0I ('PHOTNUM', PHOTNUM, STATUS)
      CALL PAR_CANCL ('PHOTNUM', STATUS)

*    Generate file name like 16NOVnnnn

      CALL CGS3DR_GENRAWNAME (PHOTNUM, RUNFIL, STATUS)

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

***************

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

      CALL DSA_CLOSE (STATUS)  ! close it for now, but RED3 will open it again

         STRINGS(1) = RUNFIL(1:CHR_LEN(RUNFIL)) // '.sdf'
         STRINGS(2) = SUTDATE(1:CHR_LEN(SUTDATE)) // '.dat'
         CALL TASK_ENC0I (LCYCBEG, STRINGS(3), STATUS)
         CALL TASK_ENC0I (LCYCEND, STRINGS(4), STATUS)
         CALL TASK_ENC0I (ICHANBEG, STRINGS(5), STATUS)
         CALL TASK_ENC0I (ICHANEND, STRINGS(6), STATUS)
         CALL TASK_ENC0R (NSIGMA, STRINGS(7), STATUS)
         CALL TASK_ENC0L (VERBOSE_PH, STRINGS(8), STATUS)
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
         CALL CHR_FILL( ' ', INVAL )
         CALL CHR_FILL( ' ', OUTVAL )
         CALL TASK_CNCAT (8, STRINGS, INVAL, STATUS)
         CALL CGS3DR_OBEYW (REDUCTION_TASK, 'CGS3_PHRED', INVAL, OUTVAL,
     :       120000, STATUS)

         RUNRED = SUTDATE(1:CHR_LEN(SUTDATE)) // '.dat'
         CALL MSG_SETC ('OUTFIL', RUNRED)
         CALL MSG_OUT ( ' ',
     :        'ASCII output is to be found in ^OUTFIL', STATUS )

      END
