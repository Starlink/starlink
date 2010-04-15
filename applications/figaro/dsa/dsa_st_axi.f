C+
C                      D S A _ S E T _ A X I S _ I N F O
C
C  Routine name:
C     DSA_SET_AXIS_INFO
C
C  Function:
C     Sets some of the information held in an axis structure.
C
C  Description:
C     Sets a number of items in an axis structure.  If these items
C     do not in fact exist, then they are created.  At present, only a
C     very limited number of items are supported.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SET_AXIS_INFO (REF_NAME,AXIS,CHAR_ITEMS,CHAR_ARRAY,
C                                          NUM_ITEMS,NUM_ARRAY,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name used for
C                       the input structure whose axis is in question.
C     (>) AXIS          (Integer,ref) The number of the axis.
C     (>) CHAR_ITEMS    (Integer,ref) The number of character items to
C                       be set.
C     (>) CHAR_ARRAY    (Fixed string array,descr) The character items
C                       to be set.  CHAR_ARRAY(1) is the axis units,
C                       and CHAR_ARRAY(2) is the axis label.  At present,
C                       only those are supported.
C     (>) NUM_ITEMS     (Integer,ref) The number of numeric items to
C                       be set.
C     (>) NUM_ARRAY     (Double array, ref) The numeric items to be set.
C                       At present, only NUM_ARRAY(1), which is the `log
C                       binning' flag (non-zero => flag set), is supported.
C     (!) STATUS        (Integer,ref) Status code.  If bad status is
C                       passed, this routine returns immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     ICH_CI, ICH_LEN, DSA_FIND_REF, DSA_WRUSER, DSA_GET_ACTUAL_NAME,
C     GEN_NTH, ICH_FOLD, DTA_CRVAR, DTA_WRVARC, DTA_WRVARD, DTA_ERROR
C     DSA_VALIDATE_AXIS, DSA__AXIS_LABEL_NAME, DSA__AXIS_UNITS_NAME,
C     DSA__AXIS_LFLAG_NAME, DSA__CREATE_AXIS_EXTRA
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     GEN_NTH       Returns 'st','th','rd' etc appropriate to a number.
C     ICH_CI        Return an integer as a character string.
C     ICH_FOLD      Convert a string to upper case.
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_WRUSER    Output a message string to the user.
C     DSA_GET_ACTUAL_NAME  Get the full structure name from a ref_name.
C     DTA_WRVARC    Write a character data object
C     DTA_CRVAR     Create a data object
C     DTA_ERROR     Get a text description for a DTA error code.
C     DTA_WRVARD    Write a double precision data object
C     DSA_VALIDATE_AXIS    Make sure than an axis number is valid.
C     DSA__AXIS_LFLAG_NAME Get name of data object used for log binning flag.
C     DSA__AXIS_LABEL_NAME Get name of data object used for axis label.
C     DSA__AXIS_UNITS_NAME Get name of data object used for axis units.
C     DSA__CREATE_AXIS_EXTRA  Make sure an axis extension structure exists.
C
C  History:
C     12th Aug 1987   Original version.  KS / AAO.
C     15th Dec 1989   `Log binning' flag added. Now uses DSA__ routines
C                     for structure access.  KS / AAO.
C     26th Feb 1990   Added call to DSA__CREATE_AXIS_EXTRA.  KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     10th Sep 1992   There seems to be a problem for DAT_PUT (in
C                     DTA_WRVAR) to convert a _DOUBLE into an _INTEGER.
C                     Here we try to circumvent the problem by using
C                     DTA_WRVARI.  HME / UoE, Starlink.
C+
      SUBROUTINE DSA_SET_AXIS_INFO (REF_NAME,AXIS,CHAR_ITEMS,CHAR_ARRAY,
     :                                       NUM_ITEMS,NUM_ARRAY,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER AXIS, CHAR_ITEMS, NUM_ITEMS, STATUS
      DOUBLE PRECISION NUM_ARRAY(*)
      CHARACTER*(*) REF_NAME, CHAR_ARRAY(*)
C
C     Functions used
C
      CHARACTER GEN_NTH*2, ICH_CI*1
      INTEGER   ICH_LEN, ICH_FOLD
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! Status from DTA_ routines
      CHARACTER ERROR*64                    ! DTA error description
      LOGICAL   FAILED                      ! Flags unable to set info
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER NAME*128                    ! Name of axis objects
      CHARACTER OBJ_NAME*32                 ! DTA_ name of data object - ignored
      CHARACTER REF_NAME_UC*32              ! Upper case version of ref_name
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE_NAME*128          ! Full structure name from ref_name
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables.
C
      FAILED=.FALSE.
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Make sure the axis number is valid.
C
      CALL DSA_VALIDATE_AXIS (AXIS,REF_NAME_UC,STATUS)
      IF (STATUS.NE.0) GO TO 500   ! Error exit.
C
C     Attempt to create the axis structure.  If one exists, this will
C     fail and we ignore that.
C
      CALL DSA__CREATE_AXIS (REF_SLOT,AXIS,DTA_STATUS)
C
C     Set the `log binning' flag.  Putting the call to DSA__CREATE_AXIS_EXTRA
C     here rather than just after DSA__CREATE_AXIS is using the knowledge that
C     only the log binning flag is treated as an extra by the supported formats,
C     but is likely to be more efficient.
C
      IF (NUM_ITEMS.GE.1) THEN
         CALL DSA__CREATE_AXIS_EXTRA (REF_SLOT,AXIS,DTA_STATUS)
         CALL DSA__AXIS_LFLAG_NAME (REF_SLOT,AXIS,NAME,LENGTH)
         CALL DTA_CRVAR(NAME,'INT',DTA_STATUS)
         CALL DTA_WRVARI(NAME,1,INT(NUM_ARRAY(1)),DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            FAILED=.TRUE.
            GO TO 500          ! Error exit
         END IF
      END IF
C
C     Set the axis units
C
      IF (CHAR_ITEMS.GE.1) THEN
         CALL DSA__AXIS_UNITS_NAME (REF_SLOT,AXIS,NAME,LENGTH)
         NAME(LENGTH+1:)='[32]'
         CALL DTA_CRVAR(NAME,'CHAR',DTA_STATUS)
         NAME(LENGTH+1:)=' '
         CALL DTA_WRVARC(NAME,LEN(CHAR_ARRAY(1)),CHAR_ARRAY(1),
     :                                                    DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            FAILED=.TRUE.
            GO TO 500          ! Error exit
         END IF
      END IF
C
C     Set the axis label
C
      IF (CHAR_ITEMS.GE.2) THEN
         CALL DSA__AXIS_LABEL_NAME (REF_SLOT,AXIS,NAME,LENGTH)
         NAME(LENGTH+1:)='[32]'
         CALL DTA_CRVAR(NAME,'CHAR',DTA_STATUS)
         NAME(LENGTH+1:)=' '
         CALL DTA_WRVARC(NAME,LEN(CHAR_ARRAY(2)),CHAR_ARRAY(2),
     :                                                    DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            FAILED=.TRUE.
            GO TO 500          ! Error exit
         END IF
      END IF
C
C     Exit
C
  500 CONTINUE
      IF (FAILED) THEN
         CALL DSA_WRUSER('Failed to set axis information in ')
         CALL DSA_WRUSER('the '//ICH_CI(AXIS)//GEN_NTH(AXIS)//
     :                                                  ' axis of ')
         CALL DSA_GET_ACTUAL_NAME(REF_NAME_UC,STRUCTURE_NAME,STATUS)
         CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
         CALL DSA_WRUSER('. ')
         CALL DTA_ERROR (DTA_STATUS,ERROR)
         CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER ('. (File may be protected.)')
         CALL DSA_WRFLUSH
         STATUS=DSA__SETOBJ
      END IF
C
      END
