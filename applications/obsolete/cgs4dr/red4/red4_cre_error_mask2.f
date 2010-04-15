*+  RED4_CRE_ERROR_MASK2 - Copies input error array to output data array
      SUBROUTINE RED4_CRE_ERROR_MASK2( NELMX, NELMY, INPUT,
     :   MASK, STATUS )
*    Description :
*     This routine copies the error array of the input file into the
*     data array of the output file. Note that we square root the input
*     array which has been mapped as a variance so that the data numbers
*     are the same.
*    Invocation :
*     CALL RED4_CRE_ERROR_MASK2( NELMX, NELMY, INPUT, MASK, STATUS )
*    Parameters :
*     NELMX        = INTEGER( READ )
*           The size of the INPUT and MASK arrays in the X direction
*     NELMY        = INTEGER( READ )
*           The size of the INPUT and MASK arrays in the Y direction
*     INPUT( NELMX, NELMY ) = REAL( READ )
*           The input array
*     MASK( NELMX, NELMY )  = REAL( WRITE )
*           The mask array generated.
*     STATUS       = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Daly (JACH::PND)
*    History :
*     30-Jan-1992: Original version               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
*    Import :
      INTEGER
     :  NELMX,                      ! Size of arrays
     :  NELMY                       ! Size of arrays
      REAL
     :  INPUT( NELMX, NELMY )       ! Input array
*    Export :
      REAL
     :  MASK( NELMX, NELMY )        ! Output array
*    Status :
      INTEGER STATUS                ! ADAM status return
*    Local variables :
      INTEGER
     :  I,                          ! Loop counter
     :  J                           ! Loop counter
*-

*    Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    Loop around and set mask to input
      DO J = 1, NELMY
         DO I = 1, NELMX
            IF ( INPUT( I, J ).GE.0.0 ) THEN
               MASK( I, J ) = SQRT( INPUT( I, J ) )
            ELSE
               MASK( I, J ) = 0.0
            END IF
         END DO
      END DO

      END
