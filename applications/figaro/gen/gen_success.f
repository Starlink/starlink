C+
C                         G E N _ S U C C E S S
C  Routine name:
C     GEN_SUCCESS
C
C  Function:
C     Tests to see if a VMS-style status value indicates success or an error.
C
C  Description:
C     VMS system routines, both system services and run-time library routines,
C     follow a particular VAX-specific convention in the status codes they
C     return. The least significant bit of the integer code they return
C     is set if the operation has completed successfully. The rest of the code
C     conveys other information, but usually only the success bit is of
C     real interest. Traditionally, the integer status returned by such a
C     routine is tested for success using the construct 'IF (STATUS) THEN'
C     - a code construct that involves a logical test on an integer. This
C     works perfectly on VMS Vaxes, and any other test is quite difficult
C     to code, so it has become common practice.  However, the resulting
C     test is not-portable, and even if versions of the VMS routines in
C     question can be ported to other systems, the code used to test their
C     values cannot. This routine provides a suitable replacement for
C     the status test that can be used on other machines.
C
C  Language: Fortran
C
C  Call:
C     SUCCESS = GEN_SUCCESS(STATUS)
C
C  Parameters:       (">" input, "<" output, "!" modified)
C     (>) STATUS     (Integer) The status code to be tested. This should be
C                    a code following the VMS convention that even numbers
C                    indicate failure and odd numbers represent success.
C
C  Returns:
C     (<) SUCCESS    (Logical) True if the status code represents success.
C                    That is, SUCCESS is teturned true if the least
C                    significant bit of STATUS was set.
C
C  Version date: 2nd Sept 1992.
C
C  Support: K. Shortridge, AAO.
C-
C  History:
C      2nd Sep 1992.  Original version. KS/AAO.
C
C  Note:
C     This implementation uses the IAND call, which is not standard
C     Fortran 77 and may not be supported on all machines - which is
C     why it is not a suitable replacement for the test. Putting it in
C     this routine ensures that even on a machine that does not support
C     IAND it will only be this routine that needs to be changed.
C+
      LOGICAL FUNCTION GEN_SUCCESS(STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
C
      GEN_SUCCESS=(IAND(STATUS,1).EQ.1)
C
      END
