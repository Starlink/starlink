C+
      SUBROUTINE DSA_DUMP_COMMON
C
C                        D S A _ D U M P _ C O M M O N
C
C  Routine name:
C     DSA_DUMP_COMMON
C
C  Function:
C     Diagnostic routine to dump the DSA_ system common variables.
C
C  Description:
C     A rather crude routine used purely for diagnostic purposes.
C     This dumps the contents of any of the DSA_ common variables
C     that are in use.  Output is by simple Fortran print statements.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_DUMP_COMMON
C
C  External variables used:
C     Internal common variables used by DSA_ system routines.
C
C  External subroutines / functions used:  ICH_LEN
C
C  Prior requirements:
C     DSA_OPEN should have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN    Position of last non-blank character in string.
C
C  History:
C     2nd July 1987  Original version.  KS / AAO.
C     27th Apr 1990  MAP_STRUCT changed to MAP_SCALED. KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C      2nd Jul 1993  Added some missing MAP_CALL_xxx variables. KS/AAO.
C
C  Note:
C     This routine is rather out of date - lots of new common items have
C     appeared that it doesn't know about.
C+
      IMPLICIT NONE
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER I                            ! Loop index through slots
      INTEGER J                            ! Loop index through axes
C
C     Files
C
      DO I=1,MAX_FILES
         IF (FILE_USED(I)) THEN
            PRINT *,'File slot ',I,',  Count = ',FILE_COUNT(I)
            PRINT *,FILE_NAMES(I)(:ICH_LEN(FILE_NAMES(I)))
            PRINT *,FILE_TOP_NAMES(I)(:ICH_LEN(FILE_TOP_NAMES(I)))
         END IF
      END DO
C
C     Map tables
C
      DO I=1,MAX_MAPS
         IF (MAP_USED(I)) THEN
            PRINT *,'Map slot ',I,',  Count = ',MAP_COUNT(I)
            PRINT *,MAP_NAMES(I)(:ICH_LEN(MAP_NAMES(I)))
            PRINT '(1x,a,z10)','Pointer = ',MAP_POINTER(I)
            PRINT *,'Workspace slot = ',MAP_WORK(I),'  Mode = ',
     :                                                MAP_MODE(I)
            PRINT *,'Type code = ',MAP_CODE(I),', actual type = ',
     :                           MAP_TYPE(I)(:ICH_LEN(MAP_TYPE(I)))
            IF (MAP_SCALED(I)) THEN
               PRINT *,'Base object is a scaled array'
            ELSE
               PRINT *,'Base object is a primitive array'
            END IF
         END IF
      END DO
C
C     Reference names
C
      DO I=1,MAX_REFS
         IF (REF_USED(I)) THEN
            PRINT *,'Reference slot ',I,',  Name = ',
     :                  REF_NAMES(I)(:ICH_LEN(REF_NAMES(I)))
            PRINT *,ACTUAL_NAMES(I)(:ICH_LEN(ACTUAL_NAMES(I)))
            PRINT *,OBJ_NAMES(I)(:OBJ_LEN(I))
            PRINT *,'File slot = ',REF_FILE(I)
            IF (DATA_NDIM(I).GT.0) THEN
               PRINT *,'Dimensions: ',(DATA_DIMS(J,I),J=1,DATA_NDIM(I))
            ELSE
               PRINT *,'Array dimensions not known'
            END IF
         END IF
      END DO
C
C     Work space
C
      DO I=1,MAX_WORK
         IF (WORK_USED(I)) THEN
            PRINT *,'Work slot ',I
            PRINT '(1X,A,Z10,A,I3)','Pointer = ',WORK_POINTER(I),
     :                                    ',  Link = ',WORK_LINK(I)
            PRINT *,'Type: ',WORK_TYPE(I)(:ICH_LEN(WORK_TYPE(I)))
         END IF
      END DO
C
C     Map call tables
C
      DO I=1,MAX_MAP_CALLS
         IF (MAP_CALL_USED(I)) THEN
            PRINT *,'Map call slot ',I
            PRINT *,'Mode: ',MAP_CALL_MODE(I),', Slot = ',
     :            MAP_CALL_SLOT(I),', Work slot = ',MAP_CALL_WORK(I)
            PRINT *,'Flag slot = ',MAP_CALL_FSLOT(I),
     :            ',Nflag = ',MAP_CALL_NFLAG(I),
     :            ',Variance slot = ',MAP_CALL_VSLOT(I)
         END IF
      END DO
C
      END
