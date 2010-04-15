C+
C                      D S A _ G E T _ D A T A _ I N F O
C
C  Routine name:
C     DSA_GET_DATA_INFO
C
C  Function:
C     Obtains information about the data array in a structure.
C
C  Description:
C     Obtains a number of items from a data structure that relate directly
C     to the data array itself.  If these items do not in fact exist, then
C     character items are returned as blank, and numeric items are returned
C     as zero.  At present, only a very limited number of items are supported.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_GET_DATA_INFO (REF_NAME,CHAR_ITEMS,CHAR_ARRAY,
C                                          NUM_ITEMS,NUM_ARRAY,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name used for
C                       the input structure whose data is in question.
C     (>) CHAR_ITEMS    (Integer,ref) The number of character items to
C                       be returned.
C     (<) CHAR_ARRAY    (Fixed string array,descr) The character items
C                       returned.  CHAR_ARRAY(1) is the axis units
C                       and CHAR_ARRAY(2) is the axis label.  At present,
C                       only those are supported.
C     (>) NUM_ITEMS     (Integer,ref) The number of numeric items to
C                       be returned.
C     (<) NUM_ARRAY     (Double array,descr) The numeric items
C                       returned.  NUM_ARRAY(1) is the magnitude flag
C                       (non-zero => magnitude data).
C                       At present, no others are supported.
C     (!) STATUS        (Integer,ref) Status code.  If bad status is
C                       passed, this routine returns immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, ICH_CLEAN, DTA_SZVAR, DTA_RDVARC, DTA_RDVARD,
C     DSA__MAG_FLAG_NAME, DSA__UNITS_NAME, DSA__LABEL_NAME, DSA_WRUSER.
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_CLEAN     Clip a string at first non-printing character.
C     DSA_REF_SLOT  Look up reference name in common tables.
C     DSA__CREATE_DATA_ENV Ensure environment for data array exists.
C     DSA__MAG_FLAG_NAME   Get name of object holding magnitude flag.
C     DSA__LABEL_NAME      Get name of object holding data label.
C     DSA__UNITS_NAME      Get name of object holding data units.
C     DTA_SZVAR     Get dimensions of a data object
C     DTA_RDVARC    Read a character data object
C     DTA_RDVARD    Read a data object in double precision
C
C  History:
C     8th July 1987   Original version.  KS / AAO.
C     2nd May  1989   Fixed bug in reading of magnitude flag - thanks, MSC.
C                     KS / AAO.
C     8th Dec  1989   Comments reformatted to avoid problems when processed
C                     using MAN.  KS/AAO.
C     28th Feb 1990   Modified to use DSA__ routines rather than assuming
C                     the original Figaro data format.  KS/AAO.
C     12th Mar 1990   Comments corrected - DSA__CREATE_EXTRA not used.  KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     10th Sep 1992   There seems to be a problem for DAT_GET (in
C                     DTA_RDVAR) to get a _DOUBLE from an _INTEGER.
C                     This shows up in CALDIV when it tries to get the
C                     magnitude flag. Here we try to circumvent the
C                     problem by using DTA_RDVARI.  HME / UoE, Starlink.
C+
      SUBROUTINE DSA_GET_DATA_INFO (REF_NAME,CHAR_ITEMS,CHAR_ARRAY,
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
      INTEGER   ICH_CLEAN
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! Status from DTA_ routines
      INTEGER   I                           ! Loop variable
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER NAME*128                    ! Name of structure objects
      INTEGER   NCH                         ! Number of characters in string
      INTEGER   NDIM                        ! Dimensions of axis objects
      INTEGER   REF_SLOT                    ! Reference table slot # - ignored

C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Start by blanking out all the character items
C
      DO I=1,CHAR_ITEMS
         CHAR_ARRAY(I)=' '
      END DO
C
C     Get the data units
C
      IF (CHAR_ITEMS.GE.1) THEN
         CALL DSA__UNITS_NAME (REF_SLOT,NAME,LENGTH)
         CALL DTA_SZVAR(NAME,1,NDIM,NCH,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            NCH=MIN(NCH,LEN(CHAR_ARRAY(1)))
            CALL DTA_RDVARC(NAME,NCH,CHAR_ARRAY(1),DTA_STATUS)
            INVOKE=ICH_CLEAN(CHAR_ARRAY(1))
         END IF
      END IF
C
C     Get the data label
C
      IF (CHAR_ITEMS.GE.2) THEN
         CALL DSA__LABEL_NAME (REF_SLOT,NAME,LENGTH)
         CALL DTA_SZVAR(NAME,1,NDIM,NCH,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            NCH=MIN(NCH,LEN(CHAR_ARRAY(2)))
            CALL DTA_RDVARC(NAME,NCH,CHAR_ARRAY(2),DTA_STATUS)
            INVOKE=ICH_CLEAN(CHAR_ARRAY(2))
         END IF
      END IF
C
C     Get the magnitude flag
C
      IF (NUM_ITEMS.GE.1) THEN
         CALL DSA__MAG_FLAG_NAME (REF_SLOT,NAME,LENGTH)
         IF (DTA_STATUS.EQ.0) THEN
            CALL DTA_RDVARI(NAME,1,I,DTA_STATUS)
            NUM_ARRAY(1)=FLOAT(I)
         END IF
         IF (DTA_STATUS.NE.0) NUM_ARRAY(1)=0.0
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END


