      SUBROUTINE SCULIB_MALLOC (SIZE, START_PTR, END_PTR, STATUS)
*+
*  Name:
*     SCULIB_MALLOC

*  Purpose:
*     get virtual memory

*  Description:
*     This routine gets SIZE bytes of virtual memory.
*
*     If status is bad on entry the routine will return immediately.
*
*     If START_PTR is not equal to 0 then
*       - It's possible that VM may have already been allocated to 
*        START_PTR so issue an error message and set status bad.
*
*     else
*
*      -  Call PSX_MALLOC to allocate the required memory plus space for
*        2 integers at either end. 
*
*      -  If status is good then
*           Set START_PTR and END_PTR to point to the first and last
*           bytes in the section of memory to be used and
*           call SCULIB_CFILLI to set the sentinel integers at either
*           end of the block to 37. SCULIB_FREE will check these for
*           corruption when the time comes to free the VM.
*
*     end if

*  Invocation:
*     CALL SCULIB_MALLOC (SIZE, START_PTR, END_PTR, STATUS)

*  Arguments:
*     SIZE                = INTEGER (Given)
*           number of bytes required
*     START_PTR           = INTEGER (Given and returned)
*           pointer to beginning of virtual memory
*     END_PTR             = INTEGER (Returned)
*           pointer to end of virtual memory
*     STATUS              = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (ROE::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     $Log$
*     Revision 1.6  1999/08/19 03:37:16  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     18-OCT-1994: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                    ! for VAL__NBI

*  Arguments Given:
      INTEGER SIZE

*  Arguments Given & Returned:
      INTEGER START_PTR

*  Arguments Returned:
      INTEGER END_PTR

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:
      INTEGER BOUNDARY                     ! Number of bytes used for
      PARAMETER (BOUNDARY = VAL__NBD)      ! sentinel integers. Also the 
                                           ! alignment size for data

*  Local variables:

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      IF (START_PTR .NE. 0) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_MALLOC: bad pointer value '//
     :     'on entry', STATUS)

      ELSE IF (SIZE .LE. 0) THEN

*     Protect against a request for negative memory.

         STATUS = SAI__ERROR
         CALL MSG_SETI('SIZ',SIZE)
         CALL ERR_REP(' ', 'SCULIB_MALLOC: Cannot allocate ^SIZ '//
     :        'bytes of memory', STATUS)


      ELSE


*     Make sure data ends on 4 byte boundary (problems with BYTE allocs)
*     This is because of the CFILLI used for the sentinel number
*     The Alpha wants 8byte boundaries for DOUBLE arrays so use VAL__NBD
*     even though this is overkill for all the other smaller arrays.

         IF (MOD(SIZE, BOUNDARY) .NE. 0) THEN
            SIZE = SIZE + (BOUNDARY - MOD(SIZE, BOUNDARY))
         END IF

*  get memory
         CALL PSX_MALLOC (SIZE + 2 * BOUNDARY, START_PTR, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            START_PTR = START_PTR + BOUNDARY
            END_PTR = START_PTR + SIZE - 1

*  set sentinel integers
            CALL SCULIB_CFILLI (1, 37, %val(START_PTR - BOUNDARY))
            CALL SCULIB_CFILLI (1, 37, %val(END_PTR + 1))
         ELSE
            START_PTR = 0
         END IF

      END IF

      END
