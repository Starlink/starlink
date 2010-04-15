C+
C                     D S A _ G E T _ E X P O S U R E
C
C  Routine name:
C     DSA_GET_EXPOSURE
C
C  Function:
C     Returns the exposure time associated with a data structure.
C
C  Description:
C     Many Figaro data structures opened, for example, by DSA_INPUT
C     will have an exposure time associated with them.  This routine
C     returns that time (in seconds), if it exists, or a supplied
C     default if it does not.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_GET_EXPOSURE (REF_NAME,DEFAULT,MAX_INVALID,TIME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name used to
C                      identify the structure.
C     (>) DEFAULT      (Float,ref)  A default time that may be used if no
C                      time value is in fact associated with the structure,
C                      or if the time is invalid.
C     (>) MAX_INVALID  (Float,ref) The maximum exposure time that is to be
C                      treated as invalid.  Usually, this will be zero, since
C                      it is usually easy for uninitialised values to look
C                      as if they are zero, and negative times are generally
C                      invalid.
C     (<) TIME         (Float,ref) The exposure time, in seconds, associated
C                      with the structure.
C     (!) STATUS       (Integer,ref) Status code.  If bad status is
C                      passed, this routine returns immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA_WRUSER, DSA_GET_ACTUAL_NAME, DSA__EXPOSURE_NAME,
C     DTA_RDVARF, ICH_LEN, ICH_CF
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure in question should have been already opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_REF_SLOT     Look up reference name in common tables.
C     DSA_WRUSER       Write string to user.
C     DSA_GET_ACTUAL_NAME  Get full name for structure.
C     DSA__EXPOSURE_NAME   Get name of object used for exposure time.
C     DTA_RDVARF       Read from a floating point data object.
C     ICH_LEN          Position of last non-blank char in string.
C     ICH_CF           Format a real number.
C
C  History:
C     10th Aug 1987    Original version.  KS / AAO.
C     12th Mar 1990    Now uses DSA__ routines rather than just assuming
C                      the original Figaro data format.  KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992    Remove unused variable declarations. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C
C+
      SUBROUTINE DSA_GET_EXPOSURE (REF_NAME,DEFAULT,MAX_INVALID,
     :                                                   TIME,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      REAL TIME, DEFAULT, MAX_INVALID
      CHARACTER*(*) REF_NAME
C
C     Functions used
C
      INTEGER ICH_LEN
      CHARACTER ICH_CF*16
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! DTA_ routine status codes
      INTEGER   IGNORE                      ! Dummy status value
      INTEGER   LENGTH                      ! Length of structure name
      CHARACTER NAME*80                     ! DTA_ name for exposure time
      CHARACTER NUMBER*16                   ! Used to format times
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE*80                ! Full name of structure
      LOGICAL   USE_DEF                     ! Indicates default must be used
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500          ! Error exit
C
C     Generate the name of the expsoure time data object and try to read it.
C
      USE_DEF=.FALSE.
      CALL DSA__EXPOSURE_NAME (REF_SLOT,NAME,LENGTH)
      CALL DTA_RDVARF(NAME,1,TIME,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
C
C        It doesn't exist.  Output a warning message.
C
         CALL DSA_WRUSER(
     :               'Note: there is no exposure time associated with ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,IGNORE)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         USE_DEF=.TRUE.
      ELSE
C
C        It did exist, and has been read.  See if it looks valid.
C
         IF (TIME.LE.MAX_INVALID) THEN
            CALL DSA_WRUSER(
     :               'Note: the exposure time associated with ')
            IGNORE=0
            CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,IGNORE)
            CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
            CALL DSA_WRUSER(' has a value of ')
            NUMBER=ICH_CF(TIME)
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
            CALL DSA_WRUSER(', which is unacceptable')
            USE_DEF=.TRUE.
         END IF
      END IF
C
      IF (USE_DEF) THEN
         CALL DSA_WRUSER('.  A default time of ')
         TIME=DEFAULT
         NUMBER=ICH_CF(TIME)
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER(' sec. has been assumed.  An exposure time ')
         CALL DSA_WRUSER('may be set using the LET command, eg ')
         CALL DSA_WRUSER('"LET '//NAME(:LENGTH)//' = value".')
         CALL DSA_WRFLUSH
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
