      SUBROUTINE SCULIB_FREE (NAME, START_PTR, END_PTR, STATUS)
*+
*  Name:
*     SCULIB_FREE

*  Purpose:
*     release virtual memory

*  Description:
*     This routine frees virtual memory obtained by SCULIB_MALLOC.
*
*     If status is bad on entry the routine will return immediately.
*
*     If START_PTR is not equal to 0 then:
*
*      - The sentinel integers above and below the used piece of memory
*        will be checked against the values they were set to by
*        SCULIB_MALLOC.
*      - PSX_FREE will be called to free the virtual memory. If
*        that's successful START_PTR and END_PTR will be cleared.
*        END_PTR will be set to bad.
*      - If either of the sentinel integers was corrupted then an
*        error will be reported and bad status returned.

*  Invocation:
*     CALL SCULIB_FREE (NAME, START_PTR, END_PTR, STATUS)

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        name associated with VM
*     START_PTR = INTEGER (Given and returned)
*        pointer to beginning of virtual memory, as returned by SCULIB_MALLOC
*     END_PTR = INTEGER (Given and returned)
*        pointer to end of virtual memory, as returned by SCULIB_MALLOC.
*        Note that this is not guaranteed to be a valid pointer.
*     STATUS = INTEGER (Given and returned)
*        global status

*  Authors:
*     J.Lightfoot (ROE::JFL)
*     T.Jenness (t.jenness@jach.hawaii.edu)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     18-OCT-1994: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                    ! for VAL__NBI
      INCLUDE 'CNF_PAR'                    ! for CNF_PVAL

*  Arguments Given:
      CHARACTER*(*) NAME

*  Arguments Given & Returned:
      INTEGER START_PTR
      INTEGER END_PTR

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      REAL    VAL_ITOR                     ! conversion from integer to real

*  Global variables:

*  Local Constants:
      INTEGER BOUNDARY                     ! Number of bytes
      PARAMETER (BOUNDARY = VAL__NBD)      ! Used for sentinel integers
      INTEGER SENTINEL                     ! Sentinel integer value
      PARAMETER (SENTINEL = 37 )

*  Local variables:
      LOGICAL LOWER                        ! .FALSE. if lower sentinel
                                           ! has been corrupted
      REAL    RTEMP                        ! scratch real
      LOGICAL UPPER                        ! .FALSE. if upper sentinel has been
                                           ! corrupted

*  Internal References:

*  Local data:

*.

      LOWER = .TRUE.
      UPPER = .TRUE.

      IF (STATUS .NE. SAI__OK) RETURN

*     DEBUG message. Should convert this to a CPP directive
*      CALL MSG_SETC('NM',NAME)
*      CALL MSG_SETI('PTR', %LOC(START_PTR))
*      CALL MSG_SETI('ACTPTR', START_PTR)
*     KLUGE the MSG__QUIET value
*      CALL MSG_OUTIF(1,' ','FREE: ^NM at ^ACTPTR using var ^PTR',STATUS)


      IF (START_PTR .NE. 0) THEN

*  check sentinel integers, calls to VAL routines are just a way of
*  accessing the VM


*  this is the lower sentinel.
*  Currently disabled because CNF_PVAL can not be used to register
*  the malloced pointer and the offset.

C         RTEMP = VAL_ITOR (.FALSE., %val(START_PTR-BOUNDARY), STATUS)
C         LOWER = (INT(RTEMP) .EQ. SENTINEL)

*  Check the upper sentinel if END_PTR is non-zero and non-bad
*  Note that END_PTR may or may not be a CNF pointer
         IF (END_PTR .NE. VAL__BADI .AND. END_PTR .NE. 0) THEN

            IF (CNF_PVAL(END_PTR) .GT. 0) THEN
*     valid CNF pointer
               RTEMP = VAL_ITOR (.FALSE., %VAL(CNF_PVAL(END_PTR)+1),
     :              STATUS)
            ELSE
*     assume we have the size of the allocated memory in bytes
               RTEMP = VAL_ITOR (.FALSE.,
     :              %VAL(CNF_PVAL(START_PTR)+END_PTR), STATUS)
            END IF
            UPPER = (INT(RTEMP) .EQ. SENTINEL)
         ELSE
            UPPER = .TRUE.
         END IF

*  release the memory.
*  Currently no sentinel at the start

C         CALL PSX_FREE (START_PTR - BOUNDARY, STATUS)
         CALL PSX_FREE (START_PTR, STATUS)

*  clear the pointer integers
         START_PTR = 0
         END_PTR = VAL__BADI

*  report sentinel corruption

         IF (.NOT. LOWER) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('NAME', NAME)
            CALL ERR_REP (' ', 'SCULIB_FREE: lower sentinel '//
     :        'of ^NAME VM has been corrupted', STATUS)
         END IF

         IF (.NOT. UPPER) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('NAME', NAME)
            CALL ERR_REP (' ', 'SCULIB_FREE: upper sentinel '//
     :        'of ^NAME VM has been corrupted', STATUS)
         END IF

      END IF

      END
