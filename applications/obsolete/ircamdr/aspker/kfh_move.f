*+  KFH_MOVE - Moves an array from one place to another.
      SUBROUTINE KFH_MOVE(FROM,TO,XDIM,YDIM,STATUS)
*    Description :
*     This routine moves the contents of the array FROM to
*     the array TO. It is used when data is mapped in using
*     the environment subroutines.
*    Invocation :
*     CALL KFH_WRITE(FROM,TO,XDIM,YDIM,STATUS)
*    Parameters :
*     FROM(XDIM,YDIM) = REAL
*           The array containing the data to be moved.
*     TO(XDIM,YDIM) = REAL
*           The array to which the data is moved.
*     XDIM = INTEGER
*           The x dimension of the data arrays.
*     YDIM = INTEGER
*           The y dimension of the data arrays.
*     STATUS = INTEGER
*           The value of status on entering this
*           subroutine.
*    Method :
*     The data is just copied straight across, one element
*     at a time.
*    Authors :
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     10 August 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER XDIM			! The x dimension of the
*					! arrays.
      INTEGER YDIM			! The y dimension of the
*					! arrays.
      REAL FROM(XDIM,YDIM)		! Array containing the
*					! data to be moved.
      INTEGER I				! General variable.
      INTEGER J				! General variable.
      REAL TO(XDIM,YDIM)		! Array to which the
*					! data is to be moved.
*-

*
*    If the status is not the correct value, then exit this routine
*    and return to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

         DO I = 1,YDIM,1
            DO J = 1,XDIM,1
               TO(J,I) = FROM(J,I)
            ENDDO
         ENDDO

      ENDIF

      END
