C+
C                           F I G _ E C S _ R E A D
C
C  Routine name:
C     FIG_ECS_READ
C
C  Function:
C     Reads an echelle order selection file produced by ECHSELECT
C
C  Description:
C     This routine reads in the data held in an order selection file
C     as produced by ECHSELECT.   Essentially, all the file contains
C     is a list of cross-sections associated either with sky or with
C     object for various orders.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_ECS_READ (FILENAME,NELM,ORDERS,CODE)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) FILENAME    (Fixed string,descr) The name of the file
C                     to be read.  This can be as full a description
C                     as is necessary.  Note that no search paths
C                     are used automatically by this routine.
C     (>) NELM        (Integer,ref) The number of cross-sections in
C                     the image to be processed using this file.
C                     This is also the number of elements in ORDERS,
C                     so should match the size of the image used
C                     when the file was produced.
C     (<) ORDERS      (Integer array,ref) Indicates for each cross-section
C                     whether it is unselected (=0), object for order M
C                     (=M) or sky for order M (= -M).
C     (<) CODE        (Integer, ref) Error code. 0=> OK, 1=> Information,
C                     2=> Warning, 3=> Error, 4=> Fatal.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     ICH_CI, ICH_LEN, PAR_WRUSER
C
C  Prior requirements:
C     This is an internal routine of ECHSELECT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 22nd Feb 1989.
C-
C  Subroutine / function details:
C     PAR_WRUSER    Output string to user
C     ICH_LEN       Position of last non-blank char in string
C     ICH_CI        Returns formatted version of an integer
C
C  History:
C     22nd Feb 1989.   Original version.  KS / AAO.
C     25th Jul 1996.   String catenation in light of Linux port.  MJCL / UCL.
C+
      SUBROUTINE FIG_ECS_READ (FILENAME,NELM,ORDERS,CODE)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, ORDERS(NELM), CODE
      CHARACTER*(*) FILENAME
C
C     Functions used
C
      INTEGER   ICH_LEN
      CHARACTER ICH_CI*8
C
C     Local variables
C
      LOGICAL   EOF              ! False until end of file reached
      LOGICAL   FOPEN            ! True if file opened OK
      INTEGER   FSTAT            ! Fortran I/O status
      INTEGER   I                ! Loop index through ORDERS
      INTEGER   IGNORE           ! Used for don't care status codes
      CHARACTER LINE*80          ! Line read from file
      INTEGER   LINENO           ! Line number in file
      INTEGER   LU               ! Logical unit number to use for file
      INTEGER   NEXT             ! Next available character in STRING
      INTEGER   ORDER            ! Order number read from file
      INTEGER   STATUS           ! DSA inherited status value
      CHARACTER STRING*80        ! String use to format messages
      INTEGER   XSECT            ! Cross-section number read from file
C
C     Error codes
C
      INTEGER OK, INFORM, WARNING, ERROR, FATAL
      PARAMETER (OK=0, INFORM=1, WARNING=2, ERROR=3, FATAL=4)
C
C     Initial values
C
      CODE=OK
      STATUS=0
      FSTAT=0
      FOPEN=.FALSE.
      DO I=1,NELM
         ORDERS(I)=0
      END DO
C
C     Get logical unit to use
C
      CALL DSA_GET_LU (LU,STATUS)
      IF (STATUS.NE.0) THEN
         CODE=ERROR
         GO TO 500
      END IF
C
C     Open file
C
      OPEN (UNIT=LU,FILE=FILENAME,STATUS='OLD',IOSTAT=FSTAT)
      IF (FSTAT.NE.0) GO TO 500
      FOPEN=.TRUE.
C
C     Loop reading lines from file until end of file or I/O error
C     (End of file is not treated as an I/O error, so FSTAT is cleared
C     when it occurs.)
C
      LINENO=0
      EOF=.FALSE.
      DO WHILE (.NOT.EOF)
         LINENO=LINENO+1
         READ (LU,'(A)',IOSTAT=FSTAT) LINE
         IF (FSTAT.LT.0) THEN
            EOF=.TRUE.
            FSTAT=0
         ELSE IF (FSTAT.NE.0) THEN
            GO TO 500
         ELSE
C
C           Line read OK.  If not comment or blank, decode
C           x-sect and corresponding order number from it.
C
            IF ((LINE(1:1).NE.'*').AND.(LINE.NE.' ')) THEN
               READ (LINE,*,IOSTAT=FSTAT) XSECT,ORDER
               IF (FSTAT.NE.0) THEN
                  STRING='Invalid line read from file '//
     :                    FILENAME(:ICH_LEN(FILENAME))
                  CALL PAR_WRUSER(STRING,IGNORE)
                  CALL PAR_WRUSER(LINE(:ICH_LEN(LINE)),IGNORE)
                  GO TO 500
               END IF
               IF ((XSECT.GT.NELM).OR.(XSECT.LT.1)) THEN
                  STRING='* Cross-section number '//ICH_CI(XSECT)
                  NEXT=ICH_LEN(STRING)+1
                  STRING(NEXT:)='is out of range for image.  Ignored *'
                  CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),IGNORE)
                  CODE=MAX(CODE,WARNING)
               ELSE
                  ORDERS(XSECT)=ORDER
               END IF
            END IF
         END IF
      END DO
C
C     On the way out, close down anything opened.  If FSTAT is non-zero,
C     there has been a Fortran I/O error.
C
  500 CONTINUE
      IF (FSTAT.NE.0) THEN
         IF (FOPEN) THEN
            STRING='I/O error opening file '//FILENAME
         ELSE
            STRING='I/O error reading from file '//
     :         FILENAME(:ICH_LEN(FILENAME))//' at line '//ICH_CI(LINENO)
         END IF
         CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),IGNORE)
         CALL GEN_FORTERR(FSTAT,.FALSE.,STRING)
         CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),IGNORE)
         CODE=MAX(CODE,ERROR)
      END IF
      IF (FOPEN) CLOSE(LU,IOSTAT=IGNORE)
      STATUS=0
      CALL DSA_FREE_LU (LU,STATUS)
C
      END
