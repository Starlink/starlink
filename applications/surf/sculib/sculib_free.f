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
*     If START_PTR is not equal to 0 then
*
*        The sentinel integers above and below the used piece of memory
*        will be checked against the values they were set to by 
*        SCULIB_MALLOC.
*
*        PSX_FREE will be called to free the virtual memory. If 
*        that's successful START_PTR and END_PTR will be set to
*        the bad value.
*
*        If either of the sentinel integers was corrupted then an
*        error will be reported and bad status returned.
*
*     end if

*  Invocation:
*     CALL SCULIB_FREE (NAME, START_PTR, END_PTR, STATUS)

*  Arguments:
*     NAME                = CHARACTER*(*) (Given)
*           name associated with VM
*     START_PTR           = INTEGER (Given and returned)
*           pointer to beginning of virtual memory
*     END_PTR             = INTEGER (Given and returned)
*           pointer to end of virtual memory
*     STATUS              = INTEGER (Given and returned)
*           global status

*  Method:

*  Deficiencies:

*  Bugs:

*  Authors:
*     J.Lightfoot (ROE::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


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


*  Local variables:
      LOGICAL LOWER                        ! .FALSE. if lower sentinel 
                                           ! has been corrupted
      REAL    RTEMP                        ! scratch real
      LOGICAL UPPER                        ! .FALSE. if upper sentinel has been
                                           ! corrupted

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      IF (START_PTR .NE. 0) THEN

*  check sentinel integers, calls to VAL routines are just a way of
*  accessing the VM

         RTEMP = VAL_ITOR (.FALSE., %val(START_PTR-BOUNDARY), STATUS)
         LOWER = (INT(RTEMP) .EQ. 37)
         RTEMP = VAL_ITOR (.FALSE., %val(END_PTR+1), STATUS)
         UPPER = (INT(RTEMP) .EQ. 37) 

*  release the memory

         CALL PSX_FREE (START_PTR - BOUNDARY, STATUS)
         START_PTR = 0
         END_PTR = 0

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
