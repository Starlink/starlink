
*+  KFH_OUTRN - Transfers lookup table from array to file.
      SUBROUTINE KFH_OUTRN(FILE,TABLE,STATUS)
*    Description :
*     This routine takes the file mapped onto the array FILE
*     via the %VAL construct, and transfers into it the
*     lookup table stored in TABLE, converting from an integer
*     in the range 0 to 255, to a real number in the range
*     0 to 1 in the proccess.
*    Invocation :
*     CALL KFH_OUTRN(FILE,TABLE,STATUS)
*    Parameters :
*     FILE(3,0:255) = _REAL		! The array mapping
*					! onto the output file.
*     TABLE(3.0:255) = _INTEGER		! The users lookup
*					! table.
*     STATUS = _INTEGER                 ! Value of status on entry.
*    Method :
*     The elements of the lookup table are converted into
*     real numbers in the range 0 to 1 one at a time, and
*     stored in the file array.
*    Authors :
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     19 July 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      REAL FILE(3,0:255)		! Array mapping onto
*					! the output file.
      INTEGER I				! General variable.
      INTEGER J				! General variable.
      INTEGER TABLE(3,0:255)		! The users lookup
*					! table.
*-

*
*    If the value of status is bad, then exit from this subroutine
*    and return to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

         DO I = 0,255,1
            DO J = 1,3,1
               FILE(J,I) = REAL(TABLE(J,I))/255.0
            ENDDO
         ENDDO

      ENDIF

      END
