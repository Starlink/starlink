C+
C                     D S A _ G E T _ A I R M A S S
C
C  Routine name:
C     DSA_GET_AIRMASS
C
C  Function:
C     Returns the airmass value associated with a data structure.
C
C  Description:
C     Many Figaro data structures opened, for example, by DSA_INPUT
C     will have airmass values associated with them.  This routine
C     returns that value.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_GET_AIRMASS (REF_NAME,WHEN,AIRMASS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name used to
C                      identify the structure.
C     (>) WHEN         (Fixed string,descr) The first character of WHEN
C                      indicates whether the airmass value required is
C                      that for the S(tart) of the exposure, the E(nd) of
C                      the exposure, or the M(ean).  Case does not matter.
C     (<) AIRMASS      (Float,ref) The airmass value associated with the
C                      structure.
C     (!) STATUS       (Integer,ref) Status code.  If bad status is
C                      passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ routines.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA_WRUSER, DSA_GET_ACTUAL_NAME, DSA__SECZ_NAME,
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
C  Common variable details:
C     (<) DTA_CODE     (Integer) Last DTA_ error code.
C
C  Subroutine / function details:
C     DSA_REF_SLOT     Look up reference name in common tables.
C     DSA_WRUSER       Write string to user.
C     DSA_GET_ACTUAL_NAME  Get full name for structure.
C     DSA__SECZ_NAME   Get name of the data object holding the sec(z) value.
C     DTA_RDVARF       Read from a floating point data object.
C     ICH_LEN          Position of last non-blank char in string.
C     ICH_CF           Format a real number.
C
C  History:
C     10th Aug 1987    Original version.  KS / AAO.
C     12th Mar 1990    Now uses DSA__ routines rather than just assuming the
C                      orifinal Figaro data format.  KS/AAO
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version assumes that there is a single real data item, whose name
C     depends on the data format used, that holds a SEC(Z) value for the
C     observation. Since this does not distinguish between start, end and
C     other airmass values, this one value is returned no matter what the
C     value of WHEN. This is crude and will need improving one day.
C+
      SUBROUTINE DSA_GET_AIRMASS (REF_NAME,WHEN,AIRMASS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      REAL AIRMASS
      CHARACTER*(*) REF_NAME, WHEN
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
      CHARACTER NAME*80                     ! DTA_ name for airmass
      CHARACTER NUMBER*16                   ! Used to format values
      INTEGER   REF_SLOT                    ! Reference table slot #
      REAL      SECZ                        ! Sec Z value from structure
      CHARACTER STRUCTURE*80                ! Full name of structure
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
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
C     Generate the name of the sec(z) data object and try to read it.
C
      CALL DSA__SECZ_NAME (REF_SLOT,NAME,LENGTH)
      CALL DTA_RDVARF(NAME,1,SECZ,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
C
C        It doesn't exist.  Output a warning message.
C
         CALL DSA_WRUSER(
     :               'Note: there is no airmass value associated with ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,IGNORE)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         DTA_CODE=DTA_STATUS
         STATUS=DSA__DTAERR
      ELSE
C
C        It did exist, and has been read.  See if it looks valid.
C
         IF (SECZ.LE.0.0) THEN
            CALL DSA_WRUSER(
     :               'Note: the airmass value associated with ')
            IGNORE=0
            CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,IGNORE)
            CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
            CALL DSA_WRUSER(' has a value of ')
            NUMBER=ICH_CF(SECZ)
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
            CALL DSA_WRUSER(', which is unacceptable')
            STATUS=DSA__OBJINV
         END IF
      END IF
C
      IF (STATUS.NE.0) THEN
         CALL DSA_WRUSER(
     :      '.  An airmass value may be set using the LET command, eg ')
         CALL DSA_WRUSER('"LET '//NAME(:LENGTH)//' = value".')
         CALL DSA_WRFLUSH
      ELSE
C
C        The following polynomial conversion from sec Z to airmass
C        is lifted from TYB's Forth code.  It isn't clear where he
C        got the coefficients, but the results check with the table
C        given in section 60 of Allen.  (It seems to be quibbling to
C        include this when we ignore the Start and End distinction!)
C
         AIRMASS=-2.067E-4+1.00147*SECZ-4.501E-4*SECZ*SECZ
     :                                       -8.083E-4*SECZ*SECZ*SECZ
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
