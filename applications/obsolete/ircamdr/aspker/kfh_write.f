*+  KFH_WRITE - Write lookup table to file.
      SUBROUTINE KFH_WRITE(FILE,LUT,STATUS)
*    Description :
*     This subroutine writes the lookup table in LUT, to the
*     file mapped by the array FILE.
*    Invocation :
*     CALL KFH_WRITE(%VAL(POINTER),TABLE,STATUS)
*    Parameters :
*     POINTER = INTEGER
*        This a pointer into that part of virtual memory where
*        the file will reside.
*     TABLE(3,0:255) = REAL
*        This the lookup table with each entry normalised to
*        the range 0 to 1.
*     STATUS = INTEGER
*        The value of the status on entering this subroutine.
*    Method :
*     The table is copied over direct to the array mapping the
*     output file.
*    Authors :
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     21 July 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      REAL FILE(3,0:255)		! Array mapping the
*					! output file.
      INTEGER I				! General variable.
      INTEGER J				! General variable.
      REAL LUT(3,0:255)			! The lookup table.
*-

*
*    If the status is bad, then exit from this subroutine and
*    return to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

         DO I = 0,255,1
            DO J = 1,3,1
               FILE(J,I) = LUT(J,I)
            ENDDO
         ENDDO

      ENDIF

      END
