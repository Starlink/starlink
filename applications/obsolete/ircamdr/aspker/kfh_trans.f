*+  KFH_TRANS - Transfer lookup table from file to an array.
      SUBROUTINE KFH_TRANS(FILE,COLSET,STATUS)
*    Description :
*     This routine transfers data staright from a file
*     mapped by the array FILE into the array COLSET.
*     This routine is designed for lookup tables only.
*    Invocation :
*     CALL KFH_TRANS(%VAL(POINTER),ARRAY,STATUS)
*    Parameters :
*     POINTER = INTEGER
*           This pointer points to the position of the lookup
*           table in virtual memory.
*     ARRAY(3,0:255) = INTEGER
*            This is the array into which the table is put.
*     STATUS = INTEGER
*            Status value on entering this routine.
*    Method :
*     The data is stored as real numbers in the range 0 to 1.
*     However the lookup table requires integers in the range
*     0 to 255. Therefore the routine carries out the required
*     conversion on each element from the file and stores it
*     in the table array.
*    Authors :
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     28 July 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER COLSET(3,0:255)		! Array containing the
*					! final lookup table.
      REAL FILE(3,0:255)		! The array mapping
*					! the original lookup
*					! table stored on file.
      INTEGER I				! General variable.
      INTEGER J				! General variable.
*-

*
*    If the status is bad on entry to this routine, then return
*    to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

         DO I = 0,255,1
            DO J = 1,3,1
               COLSET(J,I) = NINT(FILE(J,I)*255.0)
            ENDDO
         ENDDO

      ENDIF

      END
