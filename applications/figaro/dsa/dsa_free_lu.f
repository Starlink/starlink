C+
C                           D S A _ F R E E _ L U
C
C  Routine name:
C     DSA_FREE_LU
C
C  Function:
C     Releases a logical unit number.
C
C  Description:
C     This routine releases a logical unit number that had previously
C     been obtained by DSA_GET_LU, and closes any file that might be
C     open on it.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_FREE_LU (LU,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LU       (Integer,ref) The number of the reserved Fortran unit.
C     (!) STATUS   (Integer,ref) Status value.  If this is passed as non
C                  zero, this routine returns immediately.
C
C  External variables used:
C     Common variables used only by the DSA_ package.
C
C  External subroutines / functions used:
C     FIO_PUNIT, ICH_CI, ICH_LEN, DSA_WRUSER
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (<) VMS_CODE     (Integer) Last VMS routine error code.
C     (>) MAX_LUS      (Integer parameter) Number of logical unit slots.
C     (!) LU_USED      (Logical array) Indicates logical unit slot in use.
C     (>) LU_NUMBER    (Integer array) Logical unit numbers in use.
C
C  Subroutine / function details:
C     ICH_LEN       Position of last non-blank char in string
C     ICH_CI        Format an integer.
C     FIO_PUNIT     Releases a logical unit number.
C     DSA_WRUSER    Write message string to user.
C
C  History:
C     03 Mar 1988  Original version.  KS / AAO.
C     21 Aug 1992  Automatic portability modifications
C                  ("INCLUDE" syntax etc) made. KS/AAO
C     29 Aug 1992  Introduced GEN_SUCCESS to handle tests on status values
C                  that follow the VMS convention. KS/AAO.
C     29 Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     21 Jul 1993  No longer call gen_errmsg if lib$free_lun fails.
C                  And call fio_punit anyway.
C                  HME / UoE, Starlink.
C+
      SUBROUTINE DSA_FREE_LU (LU,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER LU, STATUS
C
C     Functions used
C
      INTEGER ICH_LEN
      CHARACTER ICH_CI*8
C
C     Local variables
C
      INTEGER   I                ! Loop index
      INTEGER   IGNORE           ! Dummy status value - ignored
      CHARACTER NUMBER*8         ! Used to format LU number
      INTEGER   SLOT             ! Number of common table slot used for LU
C
C     DSA_ system common data
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     See if the logical unit is in use, and get its slot.
C
      SLOT = 0
      DO I=1,MAX_LUS
         IF (LU_USED(I)) THEN
            IF (LU_NUMBER(I).EQ.LU) THEN
               SLOT = I
               GO TO 310     ! Break out of loop
            END IF
         END IF
      END DO
  310 CONTINUE
C
      IF (SLOT.EQ.0) THEN
         CALL DSA_WRUSER ('Unable to release logical unit ')
         NUMBER = ICH_CI (LU)
         CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER ('.  Unit has not been reserved. ')
         CALL DSA_WRUSER ('Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS = DSA__INVLU
         GO TO 500            ! Error exit
      END IF
C
C     Close any file that might be open on this unit.
C
      CLOSE (UNIT=LU,IOSTAT=IGNORE)
C
C     Attempt to release the logical unit
C
      CALL FIO_PUNIT(LU,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DSA_WRUSER ('Unable to release logical unit number ')
         NUMBER = ICH_CI (LU)
         CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER ('.')
         CALL DSA_WRFLUSH
         VMS_CODE = STATUS
         STATUS = DSA__VMSERR
         GO TO 500             ! Error exit
      END IF
C
C     All OK, flag slot as free
C
      LU_USED(SLOT) = .FALSE.
      STATUS = 0
C
C     Exit
C
  500 CONTINUE
C
      END
