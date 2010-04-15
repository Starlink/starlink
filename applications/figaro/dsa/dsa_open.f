C+
C                           D S A _ O P E N
C
C  Routine name:
C     DSA_OPEN
C
C  Function:
C     Opens the DSA_ system for a Figaro application.
C
C  Description:
C     DSA_OPEN should be called at the start of any Figaro application in
C     order to initialise the DSA_ routines.  It must be called before any
C     of the other DSA_ routines.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_OPEN (STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (!) STATUS       (Integer,ref) Status code.  If a bad status value
C                      is passed, this routine returns immediately.
C
C  External variables used:
C     Common variables used only by the DSA_ package.
C
C  External subroutines / functions used:
C     DSA_BLOCK, DSA_WRUSER, DSA__SET_FORMAT.
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th November 1995.
C-
C  Common variable details:
C     (!) OPEN_FLAG    (Logical) Flags if DSA_ system initialised.
C     (<) SYS_STATUS   (Integer) Overall DSA system status.
C     (<) BUFF_PTR     (Integer) Last character used in output buffer.
C     (<) LAST_BLANK   (Integer) Last blank char in output buffer.
C     (>) MAX_FILES    (Integer parameter) Max number of files open.
C     (>) MAX_MAPS     (Integer parameter) Max number of arrays mapped.
C     (>) MAX_REFS     (Integer parameter) Max number of reference names.
C     (<) FILE_USED    (Logical array) Flags file names in use.
C     (<) MAP_USED     (Logical array) Flags mapped array names in use.
C     (<) REF_USED     (Logical array) Flags reference names in use.
C     (>) MAX_WORK     (Integer parameter) Number of workspace slots available.
C     (<) WORK_USED    (Logical array) Indicates workspace slot in use.
C     (>) MAX_MAP_CALLS (Integer parameter) Maximum number of map calls.
C     (<) MAP_CALL_USED (Logical array) Indicates table entry in use.
C     (<) SHAPE_CHECK  (Logical) Indicates data shape has changed
C     (>) MAX_LUS      (Integer) Number of logical unit slots available.
C     (<) LU_USED      (Logical array) Indicates logical unit slot in use.
C     (<) LU_NUMBER    (Integer array) Logical unit numbers in use.
C     (>) MAX_FITSTR   (Integer parameter) Number of FITS buffer entries.
C     (<) FITS_CODES   (Integer array) Ref_slot & types for FITS strings.
C     (<) LOG_DELETES  (Logical) Indicates system should log when deleting
C                      arrays as the result of a structure consistency check.
C     (>) MAX_NDF_FITS (Integer parameter) Maximum number of NDF FITS strings.
C     (<) FITS_REFS    (Integer array) Reference slot numbers associated with
C                      each array entry.  0 implies unused.
C     (<) NAME_USED    (Logical array) This name pair already in use.
C     (<) QF_BOTH_OK   (Logical) Indicates program can handle files with
C                      both quality arrays and flagged data values.
C     (<) QF_LOGGED    (Logical) Set if a message about unexpected clash of
C                      quality specifications has been output.
C
C  Subroutine / function details:
C     DSA_BLOCK       Block data routine for DSA_ package.
C     DSA_WRUSER      Write message string to user.
C     DSA__SET_FORMAT Set common variables controlling disk formats used.
C
C  History:
C     9th June 1987.   Original version.  KS / AAO.
C     1st March 1988.  Initialisation of logical unit tables added.
C     4th July 1988.   Shape check flags added.
C     14th July 1988.  Bad (`flagged') value flag added, then removed.
C     12th Sept 1988.  References to discontinued common variables deleted.
C     29th Nov  1988.  FITS string buffer initialised.
C     11th Dec  1989.  LOG_DELETE added.  KS/AAO.
C     18th Jan  1990.  Call to DSA__SET_FORMAT added.  KS/AAO.
C     9th  Feb  1990.  NDF FITS items added.  KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     17th Dec 1992.   Added initialisation of temporary name variables.
C                      Also added SYS_STATUS. KS/AAO.
C     17th Feb 1995.   Added QF_BOTH_OK. KS/AAO.
C     29th Nov 1995.   Added QF_LOGGED. KS/AAO.
C+
      SUBROUTINE DSA_OPEN (STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
C
C     DSA_ system common data
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Reference to DSA_BLOCK is to force it to be linked in.
C
      EXTERNAL DSA_BLOCK
C
C     Local variables
C
      INTEGER I                                  ! Loop variable
C
C     Check for non-zero STATUS value.
C
      IF (STATUS.NE.0) RETURN
C
C     Check for system already open
C
      IF (OPEN_FLAG) THEN
         CALL DSA_WRUSER ('Attempt to open the Figaro routines ')
         CALL DSA_WRUSER
     :               ('(a call to DSA_OPEN) when the routines were ')
         CALL DSA_WRUSER
     :              ('already open.  Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__ALROPEN
      ELSE
C
C        System not open, so open it, setting all the various counts
C        to their initial values and clearing the various 'slot in
C        use' flags.
C
         OPEN_FLAG=.TRUE.
         DO I=1,MAX_FILES
            FILE_USED(I)=.FALSE.
         END DO
         DO I=1,MAX_MAPS
            MAP_USED(I)=.FALSE.
         END DO
         DO I=1,MAX_REFS
            REF_USED(I)=.FALSE.
         END DO
         DO I=1,MAX_REFS
            SHAPE_CHECK(I)=.FALSE.
         END DO
         DO I=1,MAX_WORK
            WORK_USED(I)=.FALSE.
         END DO
         DO I=1,MAX_MAP_CALLS
            MAP_CALL_USED(I)=.FALSE.
         END DO
         DO I=1,MAX_LUS
            LU_USED(I)=.FALSE.
         END DO
         DO I=1,MAX_FITSTR
            FITS_CODES(I)=0
         END DO
         DO I=1,MAX_NDF_FITS
            FITS_REFS(I)=0
         END DO
         DO I=1,MAX_TEMP_NAMES
            NAME_USED(I)=.FALSE.
         END DO
         BUFF_PTR=0
         LAST_BLANK=0
         SYS_STATUS=0
         QF_BOTH_OK=.FALSE.
         QF_LOGGED=.FALSE.
C
C        I'm not sure if this is a good idea or not - there could be
C        a lot of irritating error messages produced as a result, but
C        it will encourage people to write routines that handle errors.
C
         LOG_DELETES=.TRUE.
C
C        Call DSA__SET_FORMAT to set the common variables associated with
C        whatever disk formats we're supposed to handle - the things looked
C        at only by DSA__ routines.
C
         CALL DSA__SET_FORMAT (STATUS)
C
      END IF
C
      END
