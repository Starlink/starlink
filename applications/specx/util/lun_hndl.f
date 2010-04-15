*  History:
*     17 Nov 1993 (hme):
*        Disuse VMS RTL include files.
*        Replace LIB${GET|FREE}_LUN with FIO_{G|P}UNIT.
*     02 Jan 1994 (rp):
*        Change status return to be FORTRAN value
*     09 Jan 1994 (rp):
*        Include TIDY parameter so that it only deletes it if this is true.
*        Replace FIO_{G|P}UNIT with U{GET|FREE}LUN
*     15 Jan 1994 (rp):
*        Add initialization for LUN table
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Add space after "allocated from"s
C--------------------------------------------------------------------
C
C           LUN_HNDL
C
C  Set of routines for Logical Unit Number management in VAX.
C  Uses the VAX run time library routines to allocate and deallocate
C  logical unit numbers. Keeps a table showing allocated units and
C  allocating routine.
C
C--------------------------------------------------------------------

      INTEGER*4 FUNCTION IGETLUN (LUN, ROUTINE, TIDY)

C  Routine to get a logical unit number, called from ROUTINE.
C  Function returns the VMS status.

      IMPLICIT   NONE

C     Formal parameters:

      INTEGER    LUN           ! Returned
      CHARACTER  ROUTINE*(*)   ! name of calling routine
      LOGICAL    TIDY          ! If true, close unit on error

C     Define the logical unit number table

      INCLUDE   'LUNTAB.INC'

C     Local variables

      INTEGER   J
      INTEGER   STATUS

C     Initialize table with BLOCK data, use EXTERNAL to force loading..

      EXTERNAL  INITLUN

C  Ok, go...

      STATUS = 0

C     Try for an allocation of a logical unit

      CALL UGETLUN (LUN, STATUS)
      IGETLUN = STATUS

C     Error handling

      IF (IGETLUN.NE.0) THEN
        PRINT *,'*** LUN allocation failed - '
        CALL LUN_DIAG

      ELSE

C    Enter the allocation in the LUN_table

        J = 1
        DO WHILE (J.le.MAXENT .and. UNIT_NUMBER(J).ne.0)
          J = J + 1
        END DO

        IF (J.gt.MAXENT) THEN
          PRINT *,'*** LUN_table error - no room for entry'
        ELSE
          UNIT_NUMBER(J)     = LUN
          CALLING_ROUTINE(J) = ROUTINE
          LUN_TIDY(J)        = TIDY
        END IF

      END IF

      RETURN
      END

C--------------------------------------------------------------------

      INTEGER*4 FUNCTION IFREELUN (LUN)

C  Routine to free the logical unit number LUN.
C  Function returns the VMS status.

      IMPLICIT   NONE

C     Formal parameters:

      INTEGER    LUN

C     Define the logical unit number table

      INCLUDE   'LUNTAB.INC'

C     Local variables:

      INTEGER   J
      INTEGER   STATUS
      LOGICAL   IOPEN

C  Ok, go...

      IFREELUN = 0

C     Look up entry in LUN_table

      J = 1
      DO WHILE (J.LE.MAXENT .AND. UNIT_NUMBER(J).NE.LUN)
        J = J + 1
      END DO

      IF (J.GT.MAXENT) THEN
        PRINT *,'*** LUN_table error - can''t find entry'
        GO TO 99
      ELSE
        INQUIRE (UNIT=UNIT_NUMBER(J), OPENED=IOPEN)
      END IF

C  Try to free the logical unit number

      IF (.NOT.IOPEN) THEN
        STATUS   = 0
        CALL UFREELUN (LUN, STATUS)
        IFREELUN = STATUS

C  Error handling

        IF (IFREELUN.NE.0) THEN
          PRINT *,'*** LUN deallocation failed - '
          PRINT *,'Unit number ',UNIT_NUMBER(J),
     &           ' allocated from ',CALLING_ROUTINE(J)
        END IF

C  Remove the allocation from the LUN_table

        UNIT_NUMBER(J)     =  0
        CALLING_ROUTINE(J) = ' '
        LUN_TIDY(J)        = .FALSE.

      ELSE
        PRINT *,'*** LUN deallocation failed -'
        PRINT *,'file still open on this unit'
        PRINT *,'Unit number ',UNIT_NUMBER(J),' allocated from ',
     &         CALLING_ROUTINE(J)
      END IF

   99 RETURN
      END

C--------------------------------------------------------------------

      SUBROUTINE TIDYLUN

C  Routine to tidy up the logical unit numbers in a sensible manner

      IMPLICIT   NONE

C     Define the logical unit number table

      INCLUDE   'LUNTAB.INC'

C     Local variables:

      INTEGER   J

C     Functions:

      INTEGER   IFREELUN
      INTEGER   ISTAT

C  Ok, go...

      DO J= 1, MAXENT
        IF (UNIT_NUMBER(J).ne.0 .and. LUN_TIDY(J)) THEN
          PRINT *,'TIDYLUN freeing logical unit number',UNIT_NUMBER(J)
          PRINT *,'...allocated from ',CALLING_ROUTINE(J)
          ISTAT = IFREELUN (UNIT_NUMBER(J))
        END IF
      END DO

      RETURN
      END

C--------------------------------------------------------------------

      SUBROUTINE LUN_DIAG

C  Routine to print the logical unit number table for diagnostic purposes

      IMPLICIT   NONE

C     Define the logical unit number table

      INCLUDE   'LUNTAB.INC'

C     Local variables:

      INTEGER   J

C  Ok, go...

      PRINT *,'Unit number, status, calling routine'
      DO J = 1, MAXENT
        IF (UNIT_NUMBER(J).NE.0) THEN
          PRINT '(1X,I4,3X,L1,3X,A24)',
     &            UNIT_NUMBER(J), LUN_TIDY(J), CALLING_ROUTINE(J)
        END IF
      END DO

      RETURN
      END

C--------------------------------------------------------------------
