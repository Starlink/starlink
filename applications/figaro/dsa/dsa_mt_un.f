C+
C                     D S A _ M A T C H _ U N I T S
C
C  Routine name:
C     DSA_MATCH_UNITS
C
C  Function:
C     Checks that the main data units for two data structures match.
C
C  Description:
C     This routine checks that the main data units for one structure
C     match those for another structure. If there is a discrepancy, an
C     error message is output, and bad status is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MATCH_UNITS (REF_NAME,REF_NAME2,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name used to
C                       refer to the first data structure.
C     (>) REF_NAME2     (Fixed string,descr) The reference name used to
C                       refer to the second data structure.
C     (!) STATUS        (Integer,ref) Status value.  If bad status is
C                       passed, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     ICH_LEN, ICH_CI, DSA_WRUSER, DSA_GET_ACTUAL_NAME, DSA_GET_DATA_INFO
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.  Both
C     structures should have been opened by, for example, DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variables used:
C     (>) MAX_AXES   (Integer parameter) Maximum number of axes
C
C  Subroutine / function details:
C     ICH_LEN        Position of last non-blank char in string
C     ICH_CI         Formats an integer into a string
C     DSA_WRUSER     Output message to user
C     DSA_GET_ACTUAL_NAME  Returns actual name of a data structure
C     DSA_GET_DATA_INFO    Returns data units associated with a structure
C
C  History:
C     7th Feb 1989.  Original version.  KS / AAO.
C     16th Oct 1990. Fixed bug where UNITS was being modified instead of UNITS2
C                    if the second string had blanks.  KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992  Remove unused variable declarations. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_MATCH_UNITS (REF_NAME,REF_NAME2,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) REF_NAME, REF_NAME2
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     DSA_ system error codes.
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   DUMMY                      ! Dummy argument, unused
      INTEGER   I                          ! Loop index
      INTEGER   IPT                        ! Pointer into UNITS
      INTEGER   IPT2                       ! Pointer into UNITS2
      INTEGER   LEN1                       ! Significant chars in UNITS
      INTEGER   LEN2                       ! Significant chars in UNITS2
      LOGICAL   MATCH                      ! Indicates unit strings match
      CHARACTER STRUCTURE*128              ! Full structure name
      CHARACTER UNITS*32                   ! Units for first axis
      CHARACTER UNITS2*32                  ! Units for second axis
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Get units for both axes and compare them (the comparison is a
C     little crude - the strings are converted to upper case, and
C     blanks removed before they are compared).
C
      CALL DSA_GET_DATA_INFO (REF_NAME,1,UNITS,0,DUMMY,STATUS)
      CALL DSA_GET_DATA_INFO (REF_NAME2,1,UNITS2,0,DUMMY,STATUS)
      IF (STATUS.NE.0) GO TO 500       ! Error exit
      LEN1=ICH_FOLD(UNITS)
      LEN2=ICH_FOLD(UNITS2)
      IPT=0
      DO I=1,LEN1
         IF (UNITS(I:I).NE.' ') THEN
            IPT=IPT+1
            UNITS(IPT:IPT)=UNITS(I:I)
         END IF
      END DO
      IPT2=0
      DO I=1,LEN2
         IF (UNITS2(I:I).NE.' ') THEN
            IPT2=IPT2+1
            UNITS2(IPT2:IPT2)=UNITS2(I:I)
         END IF
      END DO
      IF (IPT.NE.IPT2) THEN
         MATCH=.FALSE.
      ELSE
         IF (IPT.GT.0) THEN
            MATCH=UNITS(:IPT).EQ.UNITS2(:IPT2)
         ELSE
            MATCH=.TRUE.
         END IF
      END IF
      IF (.NOT.MATCH) THEN
         CALL DSA_WRUSER('The units for the main data array in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' (')
         CALL DSA_GET_DATA_INFO (REF_NAME,1,UNITS,0,DUMMY,STATUS)
         CALL DSA_WRUSER (UNITS(:LEN1))
         CALL DSA_WRUSER(') are not the same as those for the ')
         CALL DSA_WRUSER('main data array in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME2,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' (')
         CALL DSA_GET_DATA_INFO (REF_NAME2,1,UNITS2,0,DUMMY,STATUS)
         CALL DSA_WRUSER (UNITS2(:LEN2))
         CALL DSA_WRUSER (')')
         CALL DSA_WRFLUSH
         STATUS=DSA__UNIDIF
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
