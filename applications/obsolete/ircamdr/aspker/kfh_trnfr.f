
*+  KFH_TRNFR - Transfers input table to main table.
      SUBROUTINE KFH_TRNFR(TEMP,LUT,TABLE,STATUS)
*    Description :
*     This routine copies the users private colour set from
*     the file mapped by TEMP and stores it in the correct
*     form in the main colour set table LUT.
*    Invocation :
*     CALL KFH_TRNFR(TEMP,LUT,TABLE,STATUS)
*    Parameters :
*     TEMP(3,0:255) = _REAL		! The input colour set.
*     LUT(3,0:255,NLUTS) = _INTEGER	! The colour sets table.
*     TABLE = _INTEGER			! A pointer to section
*					! of table into which
*					! new colour set is to
*					! be put.
*     STATUS = _INTEGER                 ! The status value on
*                                       ! entering this subroutine.
*    Method :
*     The input table is passed as a pointer to this subroutine
*     by the %VAL construct, and is mapped by the array TEMP.
*     The data is then transferred to LUT one element at a
*     time, each beeing multiplied by 255 and then converted
*     from a real number to an integer.
*    Authors :
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     18 July 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local Constants :
      INTEGER NLUTS			! The maximum number
      PARAMETER (NLUTS = 4)		! of allowed colour
*					! sets.
*    Local variables :
      INTEGER I				! General variable.
      INTEGER J				! General variable.
      INTEGER LUT(3,0:255,NLUTS)	! Main colour set table.
      INTEGER TABLE			! Pointer to users
*					! colour set space in
*					! colour set table.
      REAL TEMP(3,0:255)		! Users colour set.
*-

*
*    If the status value is bad, then return to the main program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Transfer table data from input array (which has elements
*       in range 0 to 1) to colour set array (which has elements
*       in range 0 to 255).
*

         DO I = 0,255,1
            DO J = 1,3,1
               LUT(J,I,TABLE) = NINT(255.0*TEMP(J,I))
            ENDDO
         ENDDO

      ENDIF

      END
