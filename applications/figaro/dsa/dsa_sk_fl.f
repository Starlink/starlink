C+
C               D S A _ S E E K _ F L A G G E D _ V A L U E S
C
C  Routine name:
C     DSA_SEEK_FLAGGED_VALUES
C
C  Function:
C     Determines whether or not a data array may contain flagged values.
C
C  Description:
C     This routine looks to see if a the main data array in a data structure
C     may contain `flagged' values (otherwise known as `bad' values or
C     `magic numbers').  If this routine indicates that this is the case,
C     is does not guarantee that flagged values are actually present in
C     the data (it may be that all the data elements happen to be good).
C     Equally, if it indicates that the array may not contain flagged values,
C     it does not guarantee that it does not contain elements whose value
C     is that of the flag value for that type - merely that any pixels with
C     that value have got that way quite normally (a byte array may well
C     quite naturally have pixels with a value of -128, and a character
C     array may have characters which are '*', even if these are the flag
C     values for those types).  Note that if a data quality array is present
C     then its contents may be used to flag pixels in the data array, but this
C     routine ignores that. DSA_SEEK_QUALITY should be used to test for
C     the presence of a quality array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SEEK_FLAGGED_VALUES (REF_NAME,EXIST,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (<) EXIST        (Logical,ref) True if the data array may contain
C                      flagged values.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables internal to the DSA package.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA_SEEK_FLAGGED
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
C     DSA_REF_SLOT       Look up reference name in common tables.
C     DSA__SEEK_FLAGGED  See if a structure's main data array may be flagged.
C     DSA_QF_CHECK       Check that a program is handling data quality safely.
C
C  Common variable details:
C     (!) DATA_FLAGGED (Integer array) State of knowledge about data flagging.
C                      Indicates unknown (0), known not to exist (-1),
C                      exists (1).
C     (!) QF_HANDLING  (Integer array) Flags that record the use the program
C                      is making of the quality and flag information.
C     (>) QF_FLAG_SET  (Integer parameter) Indicates program has
C                      made a DSA_SEEK_FLAGGED_VALUES call that showed
C                      that the data did contain flagged values.
C
C  History:
C     13th July 1988.   Original version.  KS / AAO.
C     2nd March 1990.   Now uses DSA__SEEK_FLAGGED instead of assuming the
C                       original Figaro file format.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C     17th Feb 1995     Now sets and checks QF_HANDLING. KS/AAO.
C+
      SUBROUTINE DSA_SEEK_FLAGGED_VALUES (REF_NAME,EXIST,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST
      CHARACTER*(*) REF_NAME
      INTEGER STATUS
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! Status from seek - ignored
      INTEGER   REF_SLOT                    ! Reference table slot #
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
C     See if we know anything about the flag value.
C
      IF (DATA_FLAGGED(REF_SLOT).NE.0) THEN
C
C        Yes, we already know, so set the flag directly
C
         EXIST=(DATA_FLAGGED(REF_SLOT).GT.0)
      ELSE
C
C        Use DSA__SEEK_FLAGGED to look into the details of the structure
C
         CALL DSA__SEEK_FLAGGED (REF_SLOT,EXIST,DTA_STATUS)
         IF (EXIST) THEN
            DATA_FLAGGED(REF_SLOT)=1
            QF_HANDLING(REF_SLOT)=QF_HANDLING(REF_SLOT)+QF_FLAG_SET
            CALL DSA_QF_CHECK(REF_SLOT,STATUS)
         ELSE
            DATA_FLAGGED(REF_SLOT)=-1
         END IF
      END IF
C
C     Exit
C
      END
