*+  SCULIB_MALLOC - get virtual memory
      SUBROUTINE SCULIB_MALLOC (SIZE, START_PTR, END_PTR, STATUS)
*    Description :
*     This routine gets SIZE bytes of virtual memory.
*
*     If status is bad on entry the routine will return immediately.
*
*     If START_PTR is not equal to 0 then
*        It's possible that VM may have already been allocated to 
*        START_PTR so issue an error message and set status bad.
*     else
*        Call PSX_MALLOC to allocate the required memory plus space for
*        2 integers at either end. 
*
*        If status is good then
*           Set START_PTR and END_PTR to point to the first and last
*           bytes in the section of memory to be used.
*
*           Call SCULIB_CFILLI to set the sentinel integers at either
*           end of the block to 37. SCULIB_FREE will check these for
*           corruption when the time comes to free the VM.
*        end if
*     end if
*    Invocation :
*     CALL SCULIB_MALLOC (SIZE, START_PTR, END_PTR, STATUS)
*    Parameters :
*     SIZE                = INTEGER (Given)
*           number of bytes required
*     START_PTR           = INTEGER (Given and returned)
*           pointer to beginning of virtual memory
*     END_PTR             = INTEGER (Returned)
*           pointer to end of virtual memory
*     STATUS              = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (ROE::JFL)
*    History :
*     $Id$
*     18-OCT-1994: Original version.
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                    ! for VAL__NBI
*    Import :
      INTEGER SIZE
*    Import-Export :
      INTEGER START_PTR
*    Export :
      INTEGER END_PTR
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

      IF (START_PTR .NE. 0) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_MALLOC: bad pointer value '//
     :     'on entry', STATUS)

      ELSE


*  Make sure data ends on 4 byte boundary (problems with BYTE allocs)
*  This is because of the CFILLI used for the sentinel number

         IF (MOD(SIZE, VAL__NBI) .NE. 0) THEN
            SIZE = SIZE + (VAL__NBI - MOD(SIZE, VAL__NBI))
         END IF

*  get memory
         CALL PSX_MALLOC (SIZE + 2 * VAL__NBI, START_PTR, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            START_PTR = START_PTR + VAL__NBI
            END_PTR = START_PTR + SIZE - 1

*  set sentinel integers
            CALL SCULIB_CFILLI (1, 37, %val(START_PTR - VAL__NBI))
            CALL SCULIB_CFILLI (1, 37, %val(END_PTR + 1))
         ELSE
            START_PTR = 0
         END IF

      END IF

      END
