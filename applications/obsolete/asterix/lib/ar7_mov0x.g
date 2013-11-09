*+  AR7_MOV0<T> - Insert a <TYPE> value with given indices into a 7D array
      SUBROUTINE AR7_MOV0<T>( VALUE, INDICES, DIMS, DATA, STATUS )
*
*    Description :
*
*     Sets arbitrary element of any data array
*
*    Author :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     17 Nov 88 : Original (DJA)
*     18 May 89 : STATUS added (DJA)
*      6 Jun 94 : Code made generic (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER          STATUS
*
*    Import :
*
      <TYPE>           VALUE              ! Value to insert
      INTEGER          DIMS(7)            ! Dimensions of data array
      INTEGER          INDICES(7)         ! Insert point indices
*
*    Import-Export :
*
      <TYPE>           DATA(*)
*-
      IF ( STATUS .EQ. SAI__OK ) THEN

        CALL AR7_MOV0<T>_SUB(DIMS(1),DIMS(2),DIMS(3),DIMS(4),DIMS(5),
     :                         DIMS(6),DIMS(7),INDICES,VALUE,DATA)

      ENDIF

      END

*+
      SUBROUTINE AR7_MOV0<T>_SUB(D1,D2,D3,D4,D5,D6,D7,INDICES,VALUE,DATA)

*    Import :
      <TYPE>           VALUE              	! Value to insert
      INTEGER          D1,D2,D3,D4,D5,D6,D7	! Dimensions of data array
      INTEGER          INDICES(7)         	! Insert point indices
*
*    Import-Export :
*
      <TYPE>		DATA(D1,D2,D3,D4,D5,D6,D7)
*-
      DATA( INDICES(1), INDICES(2), INDICES(3),INDICES(4),
     :                INDICES(5),INDICES(6), INDICES(7) ) = VALUE

      END
