C+
C                      D S A _ S E E K _ Q U A L I T Y
C
C  Routine name:
C     DSA_SEEK_QUALITY
C
C  Function:
C     Determines whether or not a data quality array exists.
C
C  Description:
C     This routine looks to see if a data structure contains a data
C     quality array. Although it is quite indifferent as to the existence
C     or non-existence of such a quality array, it will output a warning
C     message if it finds a quality array whose size does not match that
C     of the main data array.  It will treat such a non-matching quality
C     array as not existing.  Note that a meaningful quality array can be
C     generated from flagged pixel values held in the data array, but this
C     routine ignores that. DSA_SEEK_FLAGGED_VALUES should be used to test
C     for their presence.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SEEK_QUALITY (REF_NAME,EXIST,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (<) EXIST        (Logical,ref) True if there is quality data available.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables internal to the DSA package.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA_ARRAY_EXIST, DSA_ARRAY_SIZE, DSA_MAIN_SIZE
C     DSA_WRUSER, DSA__QUAL_NAME, ICH_LEN
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure should have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 17th February 1995
C-
C  Subroutine / function details:
C     DSA_REF_SLOT     Look up reference name in common tables.
C     DSA_ARRAY_EXIST  Determine if a named array exists.
C     DSA_ARRAY_SIZE   Determine dimensions of a named array.
C     DSA_MAIN_SIZE    Determine dimensions of main data array.
C     DSA_QF_CHECK     Check that program is handling data quality safely.
C     DSA_WRUSER       Output message string to user.
C     DSA__QUAL_NAME   Get name of quality array data object.
C     ICH_LEN          Position of last non-blank char in string.
C
C  Common variable details:
C     (!) QUAL_EXIST   (Integer array) State of knowledge about quality array.
C                      Indicates unknown (0), known not to exist (-1),
C                      exists (1).
C     (!) QF_HANDLING  (Integer array) Flags that record the use the program
C                      is making of the quality and flag information.
C     (>) QF_QUAL_SET  (Integer parameter) Indicates program has
C                      done a DSA_SEEK_QUALITY call which showed that
C                      the data had an associated quality array.
C
C  History:
C     13th July 1988.   Original version.  KS / AAO.
C     22nd Feb  1990.   Now used DSA__ routines to get array name. KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C     17th Feb 1995.    Now sets and checks QF_HANDLING. KS/AAO.
C+
      SUBROUTINE DSA_SEEK_QUALITY (REF_NAME,EXIST,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST
      CHARACTER*(*) REF_NAME
      INTEGER STATUS
C
C     Functions
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER   DIMS(10)                    ! Dimensions of quality array
      INTEGER   DIMS2(10)                   ! Dimensions of main data array
      CHARACTER ERROR*64                    ! Error string
      INTEGER   I                           ! Loop index through dimensions
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   LENGTH                      ! Object name length
      LOGICAL   MATCH                       ! Flags whether dimensions match
      INTEGER   NDIM                        ! # of dimensions in quality array
      INTEGER   NDIM2                       ! # of dimensions in main array
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE*128               ! Full name of structure
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
C
C     See if we know anything about the quality array.
C
      IF (QUAL_EXIST(REF_SLOT).NE.0) THEN
C
C        Yes, we already know, so set the flag directly
C
         EXIST=(QUAL_EXIST(REF_SLOT).GT.0)
      ELSE
C
C        Not known, so generate the name of the quality array and
C        see if it exists.
C
         CALL DSA__QUAL_NAME (REF_SLOT,OBJ_NAME,LENGTH)
         CALL DSA_ARRAY_EXIST(OBJ_NAME(:LENGTH),EXIST,STATUS)
C
C        If it exists, get its dimensions and check them against those of
C        the main data array.
C
         IF ((STATUS.EQ.0).AND.EXIST) THEN
            CALL DSA_ARRAY_SIZE(OBJ_NAME(:LENGTH),10,
     :                                    NDIM,DIMS,ERROR,STATUS)
            CALL DSA_MAIN_SIZE(REF_SLOT,.FALSE.,10,NDIM2,DIMS2,ERROR,
     :                                                        STATUS)
            IF (STATUS.NE.0) THEN
               IGNORE=0
               CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
               CALL DSA_WRUSER(
     :               'Unable to check quality array dimensions against')
               CALL DSA_WRUSER(
     :               ' those of the main data array in the structure ')
               CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
               CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
               CALL DSA_WRUSER('.')
               CALL DSA_WRFLUSH
            ELSE
               MATCH=NDIM.EQ.NDIM2
               IF (MATCH) THEN
                  DO I=1,NDIM
                     IF (DIMS(I).NE.DIMS2(I)) THEN
                        MATCH=.FALSE.
                        GO TO 340        ! Break out of loop
                     END IF
                  END DO
  340             CONTINUE
               END IF
               IF (.NOT.MATCH) THEN
                  IGNORE=0
                  CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,
     :                                                         IGNORE)
                  CALL DSA_WRUSER('The structure ')
                  CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
                  CALL DSA_WRUSER(
     :                   ' has a data quality array whose dimensions ')
                  CALL DSA_WRUSER(
     :                  'do not match those of its main data array.')
                  CALL DSA_WRFLUSH
                  EXIST=.FALSE.
               END IF
            END IF
         END IF
         IF (EXIST) THEN
            QUAL_EXIST(REF_SLOT)=1
            QF_HANDLING(REF_SLOT)=QF_HANDLING(REF_SLOT)+QF_QUAL_SET
            CALL DSA_QF_CHECK(REF_SLOT,STATUS)
         ELSE
            QUAL_EXIST(REF_SLOT)=-1
         END IF
      END IF
C
C     Exit
C
      END
