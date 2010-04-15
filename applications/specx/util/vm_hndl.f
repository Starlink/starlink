*  History:
*     17 Nov 1993 (hme):
*        Disuse VMS RTL include files.
*        Replace LIB${GET|FREE}_VM with PSX_{MALLOC|FREE}.
*     04 Jan 1994 (rp):
*        Buffer input variable with NBYTES1: some problems aris
*        when PSX_MALLOC argument is an expression rather than a
*        variable name.
*     07 Jan 1994 (rp):
*        New version which can be used for *all* calls in program
*     09 Jan 1994 (rp):
*        Change calls to PSX_{MALLOC|FREE} to U{GET|FREE}VM
*     15 Jan 1994 (rp):
*        Initialize VM table in BLOCK DATA routine INITVMTAB
*        Use include file for table
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
C--------------------------------------------------------------------
C
C           VM_HNDL
C
C  Set of routines for virtual memory management
C  Use the run time library routines to allocate and deallocate
C  virtual memory. Keeps a table showing location and size of VM
C  areas currently allocated
C
C--------------------------------------------------------------------

      INTEGER*4 FUNCTION IGETVM (NBYTES, TIDY, NAME, IPTR)

C  Routine to get an area of virtual memory NBYTES long and return
C  the address of the allocated area in IPTR.
C  Function returns the VMS status.

      IMPLICIT   NONE

C     Formal parameters;

      INTEGER    NBYTES
      LOGICAL    TIDY
      CHARACTER  NAME*(*)
      INTEGER    IPTR

C     Define the virtual memory table

      INCLUDE    'VMTAB.INC'

C     Local variables:

      INTEGER     J
      INTEGER     NBYTES1
      INTEGER     STATUS

C     Initialize VM table with BLOCK DATA routine; EXTERNAL to force loading

      EXTERNAL    INITVM

C  Ok, go...

      STATUS  = 0

C     Try for an allocation of virtual memory

      NBYTES1 = NBYTES

*     PRINT *, ' -- igetvm --'
*     PRINT *, '    called from ', NAME
*     PRINT *, '    trying to get (# bytes) = ', NBYTES1

      CALL UGETVM (NBYTES1, IPTR, STATUS)
      IGETVM = STATUS

C     Error handling

      IF (IGETVM.NE.0) THEN
        PRINT *, '*** VM allocation failed - '

      ELSE
*       PRINT *, '     Virtual memory allocated:'
*       PRINT *, '     # of bytes requested/allocated = ',
*    &                NBYTES, NBYTES1

C  Enter the allocation in the VM_table

        J = 1
        DO WHILE (J.le.MAXENT .and. VM_BYTES(J).ne.0)
          J = J + 1
        END DO
        IF (J.GT.MAXENT) THEN
          PRINT *,'*** VM_table error - no room for entry'
        ELSE
          VM_BYTES(J) = NBYTES1
          VM_PTR(J)   = IPTR
          VM_NAME(J)  = NAME
          VM_TIDY(J)  = TIDY
        END IF

      END IF

      RETURN
      END

C--------------------------------------------------------------------

      INTEGER*4 FUNCTION IFREEVM (IPTR)

C  Routine to release an area of virtual memory NBYTES long
C  at the address IPTR.
C  Function returns the VMS status.

      IMPLICIT    NONE

C     Formal parameter:

      INTEGER    IPTR

C     Define the virtual memory table

      INCLUDE    'VMTAB.INC'

C     Local variables:

      INTEGER     J
      INTEGER     STATUS

C  Ok, go...

C     Look up entry in VM_table

      J = 1
      DO WHILE (J.LE.MAXENT .AND. VM_PTR(J).NE.IPTR)
        J = J + 1
      END DO
      IF (J.GT.MAXENT) THEN
        PRINT *,'*** VM_table error - can''t find entry'
      END IF

C     Try to free the allocation of virtual memory

      STATUS = 0
      CALL UFREEVM (IPTR, STATUS)
      IFREEVM = STATUS

C     Error handling

      IF (IFREEVM.NE.0) THEN
        PRINT *,'*** VM deallocation failed - '

      ELSE

C       Remove the allocation from the VM_table

        VM_BYTES(J) = 0
        VM_PTR(J)   = 0
        VM_NAME(J)  = ' '
        VM_TIDY(J)  = .FALSE.

      END IF

      RETURN
      END

C--------------------------------------------------------------------

      SUBROUTINE TIDYVM

C  Routine to tidy up the virtual memory in a sensible manner

      IMPLICIT   NONE

C     Local variables:

      INTEGER    ISTAT
      INTEGER    J

C     Functions:

      INTEGER    IFREEVM

C     Define the virtual memory table

      INCLUDE    'VMTAB.INC'

C  Ok, go...

      DO J = 1, MAXENT
        IF (VM_BYTES(J).ne.0 .and. VM_TIDY(J)) THEN
          PRINT *,'TIDYVM releasing virtual memory..',
     &            VM_BYTES(J), 'bytes'
          ISTAT = IFREEVM (VM_PTR(J))
        END IF
      END DO

      RETURN
      END

C--------------------------------------------------------------------

      SUBROUTINE VM_DIAG

C  Routine to print the virtual memory table for diagnostic purposes

      IMPLICIT   NONE

C     Local variables:

      INTEGER    J

C     Define the virtual memory table

      INCLUDE    'VMTAB.INC'

C  Ok, go...

      PRINT *,'Address, # of bytes, status and routine name'
      DO J = 1, MAXENT
        IF (VM_PTR(J) .ne. 0) THEN
          PRINT 1000, VM_PTR(J), VM_BYTES(J), VM_TIDY(J), VM_NAME(J)
        END IF
      END DO

 1000 FORMAT (I10.0,1X,I10.10,3X,L1,3X,A24)

      RETURN
      END

C--------------------------------------------------------------------
