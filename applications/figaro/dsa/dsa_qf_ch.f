C+
C                      D S A _ Q F _ C H E C K
C
C  Routine name:
C     DSA_QF_CHECK
C
C  Function:
C     Checks on the way a program is handling data quality.
C
C  Description:
C     The original versions of DSA did not allow a file to have both
C     data quality and flagged data values. Later versions removed this
C     restriction. However, this meant that it was possible for a program
C     to be coded so that it checked for flagged values, then checked for
C     data quality and then called DSA_USE_QUALITY if the data had a
C     quality and then (independently of whether or not it had called
C     DSA_USE_QUALITY) called DSA_USE_FLAGGED_VALUES if the file had
C     flagged values. The problem comes if such a program handles a file
C     with both types of data quality information, calls both USE routines,
C     but - having assumed that since only one type of information was
C     allowed in the file - assumes that only one will have been called
C     and so tries to handle the data using ONLY the flagged values or
C     ONLY the quality information. This routine checks for just that
C     sequence, and puts out a warning if it detects it. A program that
C     does really intend to use both flagged values and quality data
C     simultaneously - which is rather awkward to do, and not really
C     recommended - should call DSA_QUALITY_AND_FLAGS_OK.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_QF_CHECK (REF_SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The common table slot used by
C                        the structure to be checked.
C     (!) STATUS         (Integer,ref) Status code.  If bad status is
C                        passed to this routine it returns immediately.
C                        This routine always returns good status if
C                        it runs.
C
C  External subroutines / functions used:
C     DSA_GET_ACTUAL_NAME, DSA_WRUSER
C
C  Prior requirements:
C     The structure in question should have been opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th November 1995
C
C  Note:
C     This is an DSA internal routine and should not be called directly
C     from outside the DSA package.
C-
C  Subroutine / function details:
C     DSA_GET_ACTUAL_NAME   Get external name for an open structure
C     DSA_WRUSER            Output message to user
C
C  Common variable details:
C     (>) QF_HANDLING   (Integer array) Flags that record the use the program
C                       is making of the quality and flag information.
C     (>) QF_PROBLEM    (Integer parameter) The value of QF_HANDLING
C                       that may represent a possible problem (program
C                       is using both flagged values and data quality
C                       at the same time without having indicated that
C                       it knows that files may have both at once.
C     (>) QF_BOTH_OK    (Logical) Indicates that the program can handle files
C                       with both quality arrays and flagged data values.
C     (!) QF_LOGGED     (Logical) Set if a message about unexpected clash of
C                       quality specifications has been output.
C
C  History:
C     17th Feb 1995  Original version.  KS / AAO.
C     29th Nov 1995  No longer sets DATA_UPDATE flag. Checks QF_LOGGED
C                    to reduce output. Text of warning modified. KS/AAO.
C+
      SUBROUTINE DSA_QF_CHECK (REF_SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, STATUS
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Return immediately if bad status passed.
C
      IF (STATUS.NE.0) RETURN
C
      IF (REF_USED(REF_SLOT)) THEN
C
C        Check for the problem sequence. A program that calls
C        DSA_SEEK_QUALITY and gets a positive response will set the
C        QF_QUAL_SET flag in QF_HANDLING. One that calls
C        DSA_SEEK_FLAGGED_VALUES and gets a positive response will set the
C        QF_FLAG_SET flag. One that calls DSA_USE_QUALITY will set the
C        QF_USE_QUALITY flag. One that calls DSA_USE_FLAGGED_VALUES will
C        set the QF_USE_FLAGS flag. If all of these are set, the resulting
C        value in QF_HANDLING will be the and of all these flags, which
C        has the symbolic value QF_PROBLEM. If DSA_QUALITY_AND_FLAGS_OK has
C        also been called, the QF_BOTH_OK flag will be set as well, and
C        we should not take any action.
C
         IF ((QF_HANDLING(REF_SLOT).EQ.QF_PROBLEM)
     :                                     .AND..NOT.QF_BOTH_OK) THEN
            IF (.NOT.QF_LOGGED) THEN
               CALL DSA_WRUSER('Warning: the structure ')
               CALL DSA_WRUSER(ACTUAL_NAMES(REF_SLOT)
     :                 (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
               CALL DSA_WRUSER(
     :          ' has both quality information and flagged '
     :          //'data values. Older versions of Figaro did not allow '
     :          //'this. This program is using a '
     :          //'processing sequence that suggests it is an '
     :          //'older-style application that does not expect this. ')
               CALL DSA_WRUSER(
     :          'This may lead to incorrect processing of '
     :          //'the quality information. The program should be '
     :          //'modified, but a good workaround would be to convert '
     :          //'the data file to one with only one form of quality '
     :          //'information using, for example, ''flag2qual''')
               CALL DSA_WRFLUSH
               QF_LOGGED=.TRUE.
            ELSE
               CALL DSA_WRUSER ('Warning: the same quality information '
     :          //'problem also applies to ')
               CALL DSA_WRUSER(ACTUAL_NAMES(REF_SLOT)
     :                 (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
               CALL DSA_WRFLUSH
            END IF
         END IF
      END IF
C
      END
