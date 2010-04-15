      SUBROUTINE SCULIB_MALLOC (SIZE, START_PTR, END_PTR, STATUS)
*+
*  Name:
*     SCULIB_MALLOC

*  Purpose:
*     get virtual memory

*  Description:
*     This routine gets SIZE bytes of virtual memory. The pointer
*     stored in START_PTR will be suitable for use with CNF_PVAL.
*     The value stored in END_PTR can not be relied upon and must
*     simply be passed to SCULIB_FREE when the memory is to be
*     returned.
*
*     In some cases, additional memory will be allocated to allow
*     for checks on memory overwrites. This behaviour is not guaranteed.
*     as in some cases (eg 64bit systems) the pointers generated
*     by this behaviour can not be registered with CNF. It is highly
*     recommended that applications such as valgrind are used to
*     investigate memory overwrites and memory leaks.
*
*     If status is bad on entry the routine will return immediately.
*     else
*

*  Notes:
*     If sentinel integers are used (not guaranteed) the value stored
*     in them will be '37'. They will be checked by SCULIB_FREE.

*  Invocation:
*     CALL SCULIB_MALLOC (SIZE, START_PTR, END_PTR, STATUS)

*  Arguments:
*     SIZE = INTEGER (Given)
*       number of bytes required
*     START_PTR = INTEGER (Given and returned)
*       pseudo-pointer to beginning of virtual memory.
*       Must be initialised to 0 on entry.
*     END_PTR = INTEGER (Returned)
*       pointer to end of virtual memory. This pointer should
*       not be used. It should be retained and passed to
*       SCULIB_FREE.
*     STATUS = INTEGER (Given and returned)
*       global status

*  Authors:
*     J.Lightfoot (ROE::JFL)
*     Tim Jenness (JAC, Hawaii)

*  Copyright:
*     Copyright (C) 1995-2000,2004 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     $Log$
*     Revision 1.9  2004/09/01 00:40:27  timj
*     cnf-ify alloc/free
*
*     Revision 1.8  2000/10/16 21:18:33  timj
*     More info in debug statements
*
*     Revision 1.7  1999/08/19 21:17:24  timj
*     Add (commented) debug statements for checking that a malloc is
*     followed by a free (not very sophisticated).
*
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
      INCLUDE 'CNF_PAR'

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
      INTEGER SENTINEL                     ! Sentinel integer value
      PARAMETER (SENTINEL = 37 )

*  Local variables:
      INTEGER NSENT                        ! Number of sentinel integers

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
         NSENT = 1
         CALL PSX_MALLOC (SIZE + NSENT * BOUNDARY, START_PTR, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
*            START_PTR = START_PTR + BOUNDARY
*            END_PTR = START_PTR + SIZE - 1
*  set sentinel integers
*            CALL SCULIB_CFILLI (1, SENTINEL, %VAL(START_PTR - BOUNDARY))
*            CALL SCULIB_CFILLI (1, SENTINEL, %VAL(END_PTR + 1))

*  until we can attempt to register arbitrary pointers
*  this needs to simply contain the offset from START_PTR
            END_PTR = SIZE
            CALL SCULIB_CFILLI(1, SENTINEL,
     :          %VAL(CNF_PVAL(START_PTR)+SIZE))

         ELSE
            START_PTR = 0
            END_PTR = VAL__BADI
         END IF

      END IF

*     DEBUG statements (should be in CPP macro)
*     Simply write out all successful mallocs (ie good status)
*      CALL MSG_SETI('PTR', %LOC(START_PTR))
*      CALL MSG_SETI('ACTPTR', START_PTR)
*      CALL MSG_SETI('N',SIZE)
*     KLUGE the MSG__QUIET value
*      CALL MSG_OUTIF(1,' ','MALLOC: at ^ACTPTR using var ^PTR (^N)'
*     :     ,STATUS)

      END
