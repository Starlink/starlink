*+  KFH_TRANSFER - Copies an array .
      SUBROUTINE KFH_TRANSFER(INFIL,OUTFIL,NVALS,STATUS)
*    Description :
*     This routine makes a copy of an array.
*    Invocation :
*     CALL KFH_TRANSFER(INFIL,OUTFIL,NVALS,STATUS)
*    Parameters :
*     INFIL(NVALS) = REAL
*           The array to be copied.
*     OUTFIL(NVALS) = REAL
*           The array to hold the copy.
*     NVALS = INTEGER
*           The dimension of the array.
*     STATUS = INTEGER
*           The status value on entry to
*           this subroutine.
*    Method :
*     Copying the array is done using a loop
*     and making the new array element equal
*     the old array element.
*    Author :
*     S.Chan (RGVAD::KFH)
*    History :
*     11 October 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER NVALS                      ! Dimension of the array.
      INTEGER I                          ! General variable.
      REAL INFIL(NVALS)                  ! Array to be copied.
      REAL OUTFIL(NVALS)                 ! Array holding the copy.
*-

*
*    If the status is bad on entry, then return to the calling
*    program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Transfer old file to new file.
*

         DO I = 1,NVALS

            OUTFIL(I) = INFIL(I)

         END DO

      ENDIF

      END
