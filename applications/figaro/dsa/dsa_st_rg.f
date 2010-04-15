C+
C                         D S A _ S E T _ R A N G E
C
C  Routine name:
C     DSA_SET_RANGE
C
C  Function:
C     Records the max and min values for an array in a structure.
C
C  Description:
C     Records the maximum and minimum values for the main data array
C     in a structure associated with a specific reference name.  Note
C     that this will only work if the structure is indeed a structure,
C     and not just a primitive array.  Normally, if the data array in
C     a structure is mapped for write or update, then any range data
C     (if any) associated with it will be flagged as invalid when the
C     structure is closed unless this routine has been called to set
C     new values for the range.  Clearly, it is the responsibility of
C     the calling program to make sure that the values supplied are
C     accurate.  Error messages are output and bad status returned only
C     in the case where this routine fails to update a range structure
C     that already exists, is flagged as valid, and contains range
C     values that differ from those being set.  (This routine will
C     often fail to write a range structure. For example, the file may
C     be protected, or the base structure may be a primitive array;
C     however, in these cases there should not be an existing range
C     structure that disagrees with the values passed to this routine).
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SET_RANGE (REF_NAME,VMIN,VMAX,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name used to
C                      identify the structure.
C     (>) VMIN         (Real,ref) The minimum data value in the array.
C     (>) VMAX         (Real,ref) The maximum data value in the array.
C     (!) STATUS       (Integer,ref) Status code.  If bad status is
C                      passed, this routine returns immediately.
C
C  External variables used:
C     Only those used internally by the DSA_ routines.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA_WRUSER, DSA_WRNAME, DSA__CREATE_DATA_EXTRA,
C     DSA__RANGE_STRUCT_NAME, DTA_ERROR, DTA_WRVARI, DTA_RDVARI,
C     DTA_RDVARF, DTA_WRVARF, ICH_LEN
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure in question should have been already opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (<) RANGE_UPDATE  (Logical array) Indicates that the data range values
C                       have been updated.
C
C  Subroutine / function details:
C     DSA__CREATE_DATA_EXTRA  Make sure environment for additional data exists.
C     DSA__RANGE_STRUCT_NAME  Get name of sub-structure containing range info.
C     DSA_REF_SLOT     Look up reference name in common tables.
C     DSA_WRUSER       Write message to user.
C     DSA_WRNAME       Write name of data object to user.
C     DTA_ERROR        Get error string from a DTA_ error code.
C     DTA_WRVARI       Write an integer into a data object.
C     DTA_WRVARF       Write a real number into a data object.
C     DTA_RDVARI       Read an integer from a data object.
C     DTA_RDVARF       Read a real number from a data object.
C
C  History:
C     30th July 1987 Original version.  KS / AAO.
C     28th Feb  1990 Modified to use DSA__ routines to remove assumption
C                    that file is in original Figaro format.  KS/AAO.
C     12th Mar  1990 Now uses DSA__CREATE_DATA_EXTRA rather than using
C                    DSA_CREATE_EXTRA.  KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     5th  Oct 1992  HME / UoE, Starlink.  DSA_REF_SLOT was called with
C                    an empty and an extra argument (...,,...).
C
C  Note:
C     This routine assumes that, no matter what the file format used,
C     the data range values are held in a private structure (whose name
C     will vary from format to format), containing .VALID, .MAX, and .MIN
C+
      SUBROUTINE DSA_SET_RANGE (REF_NAME,VMIN,VMAX,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      REAL VMIN, VMAX
      CHARACTER*(*) REF_NAME
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! DTA_ routine status codes
      CHARACTER ERROR*64                    ! DTA_ error description
      LOGICAL   FIRST                       ! First time through loop
      INTEGER   LENGTH                      ! Length of range structure name
      REAL      MAXVAL                      ! Existing max value
      REAL      MINVAL                      ! Existing min value
      CHARACTER OBJ_NAME*80                 ! DTA_ name for range structure
      INTEGER   REF_SLOT                    ! Reference table slot #
      LOGICAL   RETRY                       ! Loop control variable
      INTEGER   VALID                       ! Value of valid flag
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables, and get the name of the
C     structure containing the range information.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500          ! Error exit
      CALL DSA__RANGE_STRUCT_NAME (REF_SLOT,OBJ_NAME,LENGTH)
C
C     This rather messy code is an attempt to produce the sequence
C     a) Try to set the range values , b) if that didn't work, try
C     to create the range structure, c) if necessary, try to set them
C     again.  The while loop, which will only be executed twice at most,
C     avoids having to repeat the set code, since a) and c) are in fact
C     the same operation.  And extra subroutines take too much documenting.
C
      FIRST=.TRUE.
      RETRY=.TRUE.
      DO WHILE (RETRY)
         CALL DTA_WRVARI(OBJ_NAME(:LENGTH)//'.VALID',1,1,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            CALL DTA_WRVARF(OBJ_NAME(:LENGTH)//'.MIN',1,
     :                                              VMIN,DTA_STATUS)
            IF (DTA_STATUS.EQ.0) THEN
               CALL DTA_WRVARF(OBJ_NAME(:LENGTH)//'.MAX',1,
     :                                              VMAX,DTA_STATUS)
            END IF
         END IF
         IF (DTA_STATUS.EQ.0) THEN
C
C           All set OK, so get out of loop
C
            RANGE_UPDATE(REF_SLOT)=.TRUE.
            RETRY=.FALSE.
         ELSE
C
C           It didn't work, so - if this is the first time through -
C           we assume this might be because the structure doesn't exist,
C           and we try to create it.  Note the assumption that the
C           range sub-structure is part of the `extra' information.
C
            DTA_CODE=DTA_STATUS
            IF (FIRST) THEN
               CALL DSA__CREATE_DATA_EXTRA (REF_SLOT,DTA_STATUS)
               CALL DTA_CRVAR(OBJ_NAME(:LENGTH),
     :                                  'Range_struct',DTA_STATUS)
               CALL DTA_CRVAR(OBJ_NAME(:LENGTH)//'.VALID',
     :                                           'INT',DTA_STATUS)
               CALL DTA_CRVAR(OBJ_NAME(:LENGTH)//'.MAX',
     :                                           'FLOAT',DTA_STATUS)
               CALL DTA_CRVAR(OBJ_NAME(:LENGTH)//'.MIN','FLOAT',
     :                                                   DTA_STATUS)
               FIRST=.FALSE.
            ELSE
C
C              If we've tried twice to set them and failed, we aren't
C              going to make it.  We only regard this as a reportable
C              error if there is an existing range structure that
C              is both valid and contains different values to the ones
C              we are trying to insert.
C
               CALL DTA_RDVARI(OBJ_NAME(:LENGTH)//'.VALID',1,
     :                                               VALID,DTA_STATUS)
               IF ((DTA_STATUS.EQ.0).AND.(VALID.NE.0)) THEN
                  CALL DTA_RDVARF(OBJ_NAME(:LENGTH)//'.MIN',1,
     :                                               MINVAL,DTA_STATUS)
                  CALL DTA_RDVARF(OBJ_NAME(:LENGTH)//'.MAX',1,
     :                                               MAXVAL,DTA_STATUS)
                  IF ((MAXVAL.NE.VMAX).OR.(MINVAL.NE.VMIN)) THEN
                     CALL DSA_WRUSER(
     :                 'Warning: Unable to modify the range structure ')
                     CALL DSA_WRNAME (OBJ_NAME(:LENGTH))
                     CALL DSA_WRUSER('. ')
                     CALL DTA_ERROR(DTA_CODE,ERROR)
                     CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
                     CALL DSA_WRUSER('.')
                     CALL DSA_WRFLUSH
                     STATUS=DSA__RNGUPD
                  END IF
               END IF
            RETRY=.FALSE.
            END IF
         END IF
      END DO
C
C     Exit
C
  500 CONTINUE
C
      END
