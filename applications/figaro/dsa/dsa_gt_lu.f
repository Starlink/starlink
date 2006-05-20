C+
C                           D S A _ G E T _ L U
C
C  Routine name:
C     DSA_GET_LU
C
C  Function:
C     Obtains a free logical unit number.
C
C  Description:
C     This routine returns the number of a currently unused Fortran
C     logical unit.  The unit number is remembered by the DSA_ system
C     and DSA_CLOSE will automatically close any file open on that
C     unit and will release the unit number.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_GET_LU (LU,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (<) LU       (Integer,ref) The number of the reserved Fortran unit.
C     (!) STATUS   (Integer,ref) Status value.  If this is passed as non
C                  zero, this routine returns immediately.
C
C  External variables used:
C     Common variables used only by the DSA_ package.
C
C  External subroutines / functions used:
C     FIO_GUNIT, ICH_LEN, DSA_WRUSER
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
C     (<) LU_USED      (Logical array) Indicates logical unit slot in use.
C     (<) LU_NUMBER    (Integer array) Logical unit numbers in use.
C
C  Subroutine / function details:
C     ICH_LEN       Position of last non-blank char in string
C     FIO_GUNIT     Get a free logical unit number.
C     DSA_WRUSER    Write message string to user.
C
C  History:
C     02 Mar 1988  Original version.  KS / AAO.
C     21 Aug 1992  Automatic portability modifications
C                  ("INCLUDE" syntax etc) made. KS/AAO
C     29 Aug 1992  Introduced GEN_SUCCESS to handle tests on status values
C                  that follow the VMS convention. KS/AAO.
C     29 Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     21 Jul 1993  No longer call gen_errmsg if lib$get_lun fails.
C                  And call fio_gunit anyway.
C                  HME / UoE, Starlink.
C+
      SUBROUTINE DSA_GET_LU (LU,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER LU, STATUS
C
C     Local variables
C
      INTEGER   I                ! Loop index
      INTEGER   SLOT             ! Number of available common table slot
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
C     See if there is an available logical unit slot
C
      SLOT = 0
      DO I=1,MAX_LUS
         IF (.NOT.LU_USED(I)) THEN
            SLOT = I
            GO TO 310     ! Break out of loop
         END IF
      END DO
  310 CONTINUE
C
      IF (SLOT.EQ.0) THEN
         CALL DSA_WRUSER ('Unable to allocate logical unit slot. ')
         CALL DSA_WRUSER ('Too many logical units must be in use. ')
         CALL DSA_WRUSER ('Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS = DSA__NOLUSL
         GO TO 500            ! Error exit
      END IF
C
C     Attempt to get a spare logical unit
C
      CALL FIO_GUNIT(LU,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DSA_WRUSER ('Unable to obtain free logical unit number.')
         CALL DSA_WRFLUSH
         VMS_CODE = STATUS
         STATUS = DSA__VMSERR
         GO TO 500             ! Error exit
      END IF
C
C     All OK, set logical unit number in slot, flag slot in use
C
      LU_USED(SLOT) = .TRUE.
      LU_NUMBER(SLOT) = LU
      STATUS = 0
C
C     Exit
C
  500 CONTINUE
C
      END
