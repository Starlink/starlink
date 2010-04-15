C+
C                         D S A _ C H E C K _ B A D B I T S
C
C  Routine name:
C     DSA_CHECK_BADBITS
C
C  Function:
C     Checks a quality array against the BADBITS mask.
C
C  Description:
C     This routine is passed a quality array and a BADBITS mask value.
C     It is an interim routine designed for the period when DSA can
C     only handle quality arrays for which all the bits in the BADBITS
C     mask are set - in other words, while DSA (and Figaro) treat any
C     non-zero quality values as indicating bad values.  This routine
C     looks to see if the BADBITS mask is not all ones, and if so
C     checks to see if any of the quality values are non-zero but
C     only have bits set that are not in the BADBITS mask - these
C     being values that will be misinterpreted.  If this is the case,
C     a warning message is output, but this is all that happens.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_CHECK_BADBITS (REF_NAME,QARRAY,NELM,TYPE,BADBITS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME       (String,descr) The reference name associated
C                        with the structure whose quality values are
C                        being checked.  This is only used to generate
C                        any warning messages.
C     (>) QARRAY         (Array of any type,ref) The array of quality values.
C                        Although in principle this can be of any type,
C                        this implementation only really supports a byte
C                        array - but this is checked using TYPE, so other
C                        types will not cause serious problems.
C     (>) NELM           (Integer,ref) The number of elements in QARRAY.
C     (>) TYPE           (String,descr) The type of data in QARRAY.  This
C                        should be one of the data types supported by DSA
C                        (INT, FLOAT, etc), but in practice only BYTE will
C                        be properly supported by this routine - any other
C                        type will just result in a warning message.  (Mapping
C                        quality data in any type other than BYTE is fraught
C                        with problems anyway.)  TYPE must be in upper case.
C     (>) BADBITS        (Byte,ref) The value of the BADBITS flag associated
C                        with the quality array.
C     (!) STATUS         (Integer,ref) Status code. If bad (non-zero) status
C                        is passed to it this routine returns immediately.
C                        Although warnings may be ouput about the quality
C                        values, these do not result in bad status being
C                        returned, and in fact this routine does not change
C                        the value of STATUS.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DSA_WRUSER, DSA_GET_ACTUAL_NAME, ICH_LEN
C
C  Prior requirements:
C     The structure must have been opened already by DSA_INPUT or
C     some similar routine.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  History:
C     1st  May  1991.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This is an internal DSA routine and not to be called from outside
C     the DSA system.
C+
      SUBROUTINE DSA_CHECK_BADBITS (REF_NAME,QARRAY,NELM,TYPE,
     :                                               BADBITS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, STATUS
      BYTE QARRAY(*), BADBITS
      CHARACTER*(*) REF_NAME, TYPE
C
C     Functions
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER   I                   ! Loop index through quality array
      INTEGER   IGNORE              ! Status value we don't care about
      BYTE      MASKED_QUAL         ! Quality value correctly masked
      LOGICAL   PROBLEM             ! True if values will be misinterpreted
      CHARACTER STRUCTURE*64        ! Full name of structure.
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Check BADBITS - if it is the all ones that DSA will assume,
C     then we don't have any problems.
C
      IF (BADBITS.NE.-1) THEN
C WAS before Linux: IF (BADBITS.NE.'FF'X) THEN
C
C        It isn't, so we will have to check.  First, make sure TYPE
C        is BYTE.  (This is a short cut, really, because we obviously
C        could code for all the possibilities.)
C
         IF (TYPE.NE.'BYTE') THEN
            CALL DSA_WRUSER('Warning: The quality data in ')
            IGNORE=0
            CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,IGNORE)
            CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
            CALL DSA_WRUSER(' is being accessed as a "')
            CALL DSA_WRUSER(TYPE(:ICH_LEN(TYPE)))
            CALL DSA_WRUSER('" array instead of as a "BYTE" array.')
            CALL DSA_WRUSER(' Some of the quality values may be '//
     :                              'misinterpreted as a result.')
            CALL DSA_WRFLUSH
         ELSE
C
C           OK, it is really a BYTE array, and we need to check it.
C
            PROBLEM=.FALSE.
            DO I=1,NELM
               MASKED_QUAL=IAND(QARRAY(I),BADBITS)
               IF ((QARRAY(I).NE.0).AND.(MASKED_QUAL.EQ.0)) THEN
                  PROBLEM=.TRUE.
                  GO TO 340       ! Break out of I loop.
               END IF
            END DO
  340       CONTINUE
C
C           Report.
C
            IF (PROBLEM) THEN
               IGNORE=0
               CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,IGNORE)
               CALL DSA_WRUSER(
     :                     'Warning: The quality mask (BADBITS) in ')
               CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
               CALL DSA_WRUSER(' is not the all ones value expected')
               CALL DSA_WRUSER(' by this program, and the quality array'
     :                    //' associated with it contains values that')
               CALL DSA_WRUSER(
     :                 ' will be misinterpreted as a result of this.')
               CALL DSA_WRFLUSH
            END IF
         END IF
      END IF
C
      END
