C+
C                          D S A _ A C T N A M E
C
C  Routine name:
C     DSA_ACTNAME
C
C  Function:
C     Returns the actual name for a structure.
C
C  Description:
C     The 'actual' name for a structure is a name that identifies it
C     uniquely for future use - that is, it includes the full details
C     of the file, extension, version number, and structure part, in
C     a form that could be used as input to, for example, DSA_NAMED_INPUT.
C     That is, the version number and file extension are in brackets
C     and followed by the structure name.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_ACTNAME (FILENAME,STRUCT,ACTUAL_NAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) FILENAME       (Fixed string,descr) The full file specification
C                        for the file containing the structure.
C     (>) STRUCT         (Fixed string,descr) The structure part of the
C                        name, starting with a '.'
C     (<) ACTUAL_NAME    (Fixed string,descr) The actual name for the
C                        structure.
C     (!) STATUS         (Integer,ref) Status code.  If a bad status
C                        is passed, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:  ICH_LEN
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C
C  Note:
C     This is an DSA_ system internal routine and should not be
C     called directly from outside the DSA_ package.
C-
C  Subroutine / function details:
C     ICH_LEN    Position of last non-blank char in string.
C
C  History:
C     16th June 1987.   Original version.  KS / AAO.
C     31st July 1987.   (.ext;n) temporarily removed from name. KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_ACTNAME (FILENAME,STRUCT,ACTUAL_NAME,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) FILENAME, STRUCT, ACTUAL_NAME
      INTEGER STATUS
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER I                                ! Loop counter through filename
      INTEGER IEND                             ! Last char in filename.
      INTEGER IST                              ! Position of '.' before ext.
      LOGICAL VALID                            ! Indicates filename is 'valid'
C
C     Return immediately on bad status value
C
      IF (STATUS.NE.0) RETURN
C
C     Work back through the filename until we find the extension.
C
      VALID=.FALSE.
      IEND=ICH_LEN(FILENAME)
      DO I=IEND,1,-1
         IF (FILENAME(I:I).EQ.'.') THEN
            IST=I
            VALID=.TRUE.
            GO TO 340          ! Break out of loop
         END IF
      END DO
  340 CONTINUE
C
C     We expect the filename to be valid, but make sure we return
C     something even if it isn't.
C
      IF (VALID) THEN
C         ACTUAL_NAME=FILENAME(:IST-1)//'('//FILENAME(IST:IEND)
C     :                                              //')'//STRUCT
         ACTUAL_NAME=FILENAME(:IST-1)//STRUCT
      ELSE
         ACTUAL_NAME=FILENAME(:IEND)//STRUCT
      END IF
C
      END
