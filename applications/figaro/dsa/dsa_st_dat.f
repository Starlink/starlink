C+
C                      D S A _ S E T _ D A T A _ I N F O
C
C  Routine name:
C     DSA_SET_DATA_INFO
C
C  Function:
C     Sets information about the data array in a structure.
C
C  Description:
C     Sets a number of items in a data structure that relate directly
C     to the data array itself.  If these items do not in fact exist, then
C     they are created.  At present, only a very limited number of items
C     are supported.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SET_DATA_INFO (REF_NAME,CHAR_ITEMS,CHAR_ARRAY,
C                                          NUM_ITEMS,NUM_ARRAY,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name used for
C                       the input structure whose data is to be set.
C     (>) CHAR_ITEMS    (Integer,ref) The number of character items to
C                       be set.
C     (>) CHAR_ARRAY    (Fixed string array,descr) The character items
C                       to be set.  CHAR_ARRAY(1) is the axis units
C                       and CHAR_ARRAY(2) is the axis label.  At present,
C                       only those are supported.
C     (>) NUM_ITEMS     (Integer,ref) The number of numeric items to
C                       be set.
C     (>) NUM_ARRAY     (Double array,descr) The numeric items
C                       to be set.  NUM_ARRAY(1) is the magnitude flag
C                       (non-zero => magnitude data).
C                       At present, no others are supported.
C     (!) STATUS        (Integer,ref) Status code.  If bad status is
C                       passed, this routine returns immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DTA_CRVAR, DTA_ERROR, DTA_WRVARC, DTA_WRVARD, ICH_LEN,
C     DSA__CREATE_DATA_ENV, DSA__CREATE_DATA_EXTRA, DSA__MAG_FLAG_NAME,
C     DSA__UNITS_NAME, DSA__LABEL_NAME, DSA_WRUSER.
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_REF_SLOT  Look up reference name in common tables.
C     DSA_GET_ACTUAL_NAME  Get full name for structure.
C     DSA__CREATE_DATA_ENV Ensure environment for data array exists.
C     DSA__CREATE_DATA_EXTRA  Ensure structure for extra information exists.
C     DSA__MAG_FLAG_NAME   Get name of object holding magnitude flag.
C     DSA__LABEL_NAME      Get name of object holding data label.
C     DSA__UNITS_NAME      Get name of object holding data units.
C     DSA_WRUSER    Output a string to the user.
C     DTA_CRVAR     Create a data object.
C     DTA_ERROR     Get a text description for a DTA error code.
C     DTA_WRVARC    Write to a character data object.
C     DTA_WRVARD    Write to a data object in double precision.
C
C  History:
C     12th Aug 1987   Original version.  KS / AAO.
C     8th  Dec 1989   Comments reformatted to avoid problems when
C                     processing them using MAN.  KS/AAO.
C     27th Feb 1990   Modified to use DSA__ routines rather than assuming
C                     the original Figaro data format.  KS/AAO.
C     12th Mar 1990   Now uses DSA__CREATE_DATA_EXTRA rather than using
C                     DSA__CREATE_EXTRA.  KS/AAO.
C     5th  Apr 1991   Now makes sure existing string items are long enough for
C                     the strings to be written to them.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     10th Sep 1992   There seems to be a problem for DAT_PUT (in
C                     DTA_WRVAR) to convert a _DOUBLE into an _INTEGER.
C                     This shows up in %%CONV when it tries to set the
C                     magnitude flag. Here we try to circumvent the
C                     problem by using DTA_WRVARI.  HME / UoE, Starlink.
C+
      SUBROUTINE DSA_SET_DATA_INFO (REF_NAME,CHAR_ITEMS,CHAR_ARRAY,
     :                                       NUM_ITEMS,NUM_ARRAY,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CHAR_ITEMS, NUM_ITEMS, STATUS
      DOUBLE PRECISION NUM_ARRAY(*)
      CHARACTER*(*) REF_NAME, CHAR_ARRAY(*)
C
C     Functions used
C
      INTEGER   ICH_LEN
      CHARACTER*3 ICH_CI
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! Status from DTA_ routines
      CHARACTER ERROR*64                    ! DTA error description
      LOGICAL   EXIST                       ! True if string item exists
      LOGICAL   FAILED                      ! Indicates write failed
      INTEGER   IPTR                        ! Pointer to string
      INTEGER   LENGTH                      ! Object name length
      CHARACTER NAME*80                     ! Name of data objects
      INTEGER   NCH                         ! Significant chars in string
      INTEGER   NCHEX                       ! Size of existing string
      INTEGER   NDIM                        ! # of dimensions - ignored
      INTEGER   REF_SLOT                    ! Reference table slot # - ignored
      CHARACTER STRUCTURE*128               ! Full structure name
C
C     DSA_ system error codes
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
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Make sure the data structure exists by trying to create it - if it
C     already exists, this fails, but we ignore that.
C
      CALL DSA__CREATE_DATA_ENV (REF_SLOT,DTA_STATUS)
C
C     Set the data units
C
      IF (CHAR_ITEMS.GE.1) THEN
         EXIST=.FALSE.
         CALL DSA__UNITS_NAME (REF_SLOT,NAME,LENGTH)
         NCH=ICH_LEN(CHAR_ARRAY(1))
         CALL DTA_SZVAR(NAME,1,NDIM,NCHEX,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            IF (NCHEX.LT.NCH) THEN
               CALL DTA_DLVAR(NAME,STATUS)
            ELSE
               EXIST=.TRUE.
            END IF
         END IF
         IF (.NOT.EXIST) THEN
            NCH=MAX(32,NCH)
            NAME(LENGTH+1:)='['//ICH_CI(NCH)
            IPTR=ICH_LEN(NAME)
            NAME(IPTR+1:)=']'
            CALL DTA_CRVAR(NAME,'CHAR',DTA_STATUS)
            NAME(LENGTH+1:)=' '
         END IF
         CALL DTA_WRVARC(NAME,LEN(CHAR_ARRAY(1)),CHAR_ARRAY(1),
     :                                                  DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            FAILED=.TRUE.
            GO TO 500         ! Error exit
         END IF
      END IF
C
C     Set the data label
C
      IF (CHAR_ITEMS.GE.2) THEN
         EXIST=.FALSE.
         CALL DSA__LABEL_NAME (REF_SLOT,NAME,LENGTH)
         NCH=ICH_LEN(CHAR_ARRAY(2))
         CALL DTA_SZVAR(NAME,1,NDIM,NCHEX,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            IF (NCHEX.LT.NCH) THEN
               CALL DTA_DLVAR(NAME,STATUS)
            ELSE
               EXIST=.TRUE.
            END IF
         END IF
         IF (.NOT.EXIST) THEN
            NCH=MAX(32,NCH)
            NAME(LENGTH+1:)='['//ICH_CI(NCH)
            IPTR=ICH_LEN(NAME)
            NAME(IPTR+1:)=']'
            CALL DTA_CRVAR(NAME,'CHAR',DTA_STATUS)
            NAME(LENGTH+1:)=' '
         END IF
         CALL DTA_WRVARC(NAME,LEN(CHAR_ARRAY(2)),CHAR_ARRAY(2),
     :                                                  DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            FAILED=.TRUE.
            GO TO 500         ! Error exit
         END IF
      END IF
C
C     Get the magnitude flag. Note that calling DSA__CREATE_DATA_EXTRA only
C     here is using the knowledge that only the magnitude flag is treated as an
C     extra by any of the supported formats, but is more efficient than
C     calling it every time.
C
      IF (NUM_ITEMS.GE.1) THEN
         CALL DSA__CREATE_DATA_EXTRA (REF_SLOT,DTA_STATUS)
         CALL DSA__MAG_FLAG_NAME (REF_SLOT,NAME,LENGTH)
         CALL DTA_CRVAR(NAME,'INT',DTA_STATUS)
C        CALL DTA_WRVARD(NAME,1,NUM_ARRAY(1),DTA_STATUS)
         CALL DTA_WRVARI(NAME,1,INT(NUM_ARRAY(1)),DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            FAILED=.TRUE.
            GO TO 500         ! Error exit
         END IF
      END IF
C
C     Exit
C
  500 CONTINUE
      IF (FAILED) THEN
         CALL DSA_WRUSER('Failed to set data information in ')
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,STATUS)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER('. ')
         CALL DTA_ERROR (DTA_STATUS,ERROR)
         CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER ('. (File may be protected.)')
         CALL DSA_WRFLUSH
         STATUS=DSA__SETOBJ
      END IF
C
      END
