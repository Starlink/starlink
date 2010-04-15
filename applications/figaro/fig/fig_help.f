C+
      SUBROUTINE FIG_HELP(NAME,STATUS)
C
C     F I G _ H E L P
C
C     Figaro utility.  Outputs the contents of a text file to
C     the terminal in order to provide help information.  The file
C     is searched for in the normal figaro directory order - ie
C     default, user, local and system Figaro directories.
C
C     Parameters -  (">" input)
C
C     (>) NAME     (Character) The name of the file to be output.
C                  This need not have an extension.  If it does not,
C                  .TXT is assumed.
C     (<) STATUS   (Integer) Returned as zero if the file could be
C                  found and opened OK.  Otherwise set to a Fortran
C                  I/O error code.
C
C                                       KS / AAO  4th Sept 1985
C     History:
C     04-SEP-1985  KS/AAO.  Original version.
C     04-SEP-1992  HME/UoE, Starlink.  Use lowercase extension for
C                  file name.
C                  Use DSA_OPEN_TEXT_FILE rather than FIG_OPFILE.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) NAME
C
C     Functions.
C
      INTEGER ICH_LEN
C
C     Local variables.
C
      INTEGER LU, FSTAT, NSTAT
      CHARACTER * ( 80 ) LINE
C
C     Get unit and open file.
C
      STATUS = 0
      CALL DSA_OPEN_TEXT_FILE( NAME, '.txt', 'OLD', .FALSE.,
     :   LU, LINE, STATUS )
C
C     List file contents and close file.
C
      FSTAT = 0
      DO WHILE ( FSTAT .EQ. 0 )
         READ( LU, '(A)', IOSTAT = FSTAT ) LINE
         IF ( FSTAT .EQ. 0 )
     :      CALL PAR_WRUSER( LINE(:ICH_LEN(LINE)), NSTAT )
      END DO
      CLOSE( LU, IOSTAT = FSTAT )
C
C     Dispose of the unit.
C
      FSTAT = 0
      CALL DSA_FREE_LU( LU, FSTAT )
C
      END
