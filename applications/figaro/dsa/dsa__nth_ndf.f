C+
C                     D S A _ _ N T H _ N D F _ F I T S
C
C  Routine name:
C     DSA__NTH_NDF_FITS
C
C  Function:
C     Finds a numbered FITS item in the NDF array.
C
C  Description:
C     This routine looks through the NDF FITS array in common, looking
C     for the n-th FITS item in it.  If there are that many items, it returns
C     the name of the item and the string number for it.  If the item cannot
C     be found, this will not be treated as an error - although the string
C     number for it will be returned as zero.  This routine treats the
C     comment-type keywords 'COMMENT', 'HISTORY' and blank separately,
C     regarding all instances of these as being part of the same three
C     multi-valued items.  It assumes that other keywords will only appear
C     once.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__NTH_NDF_FITS (REF_SLOT,NTH,ITEM,ELEMENTS,ISTR,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT        (Integer,ref) The common slot number for the
C                         reference name for the structure.
C     (>) NTH             (Integer,ref) The number of the FITS item
C                         required.
C     (<) ITEM            (Fixed string,descr) The name of the item
C                         in question.  This will be a FITS keyword.
C     (<) ELEMENTS        (Integer,ref) If the item is multi-valued,
C                         this is the number of elements it has.
C     (<) ISTR            (Integer,ref) The slot number for the item in
C                         question.
C     (!) STATUS          (Integer,ref) Status code.  If bad status is
C                         passed to it, this routine returns immedaitely.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used: None.
C
C  Prior requirements:
C     The FITS items (if any) for the structure must have been read in
C     by DSA__READ_NDF_FITS.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) MAX_NDF_FITS  (Integer parameter) Maximum number of NDF FITS strings.
C     (>) FITS_ARRAY    (String array) Used for all strings read from NDF
C                       format FITS structures.
C     (>) FITS_REFS     (Integer array) Reference slot numbers associated with
C                       each array entry.  0 implies unused.
C
C  History:
C     15th Feb 1990.   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA__NTH_NDF_FITS (REF_SLOT,NTH,ITEM,ELEMENTS,
     :                                                    ISTR,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, NTH, ELEMENTS, ISTR, STATUS
      CHARACTER*(*) ITEM
C
C     DSA common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   BLANK          ! Number of 'blank' keyword comments so far
      CHARACTER CHAR1*1        ! First character in keyword name
      INTEGER   COMMENT        ! Number of COMMENTs found so far
      INTEGER   COUNT          ! Number of distinct keywords found so far
      INTEGER   HISTORY        ! Number of HISTORY keyword entries so far
      INTEGER   INDX           ! Index into FITS common arrays
      LOGICAL   MORE           ! Controls loop through FITS arrays
      LOGICAL   MULTI          ! True if last keyword was comment-type
C
C     Return immediately if bad status passed.
C
      IF (STATUS.NE.0) RETURN
C
C     Start looking through the items in the FITS array.
C
      BLANK=0
      COMMENT=0
      HISTORY=0
      ISTR=0
      COUNT=0
      INDX=1
      MORE=.TRUE.
      DO WHILE (MORE)
         IF (FITS_REFS(INDX).EQ.REF_SLOT) THEN
            CHAR1=FITS_ARRAY(INDX)(1:1)
            IF (CHAR1.EQ.'C') THEN
               IF (FITS_ARRAY(INDX)(1:8).EQ.'COMMENT') THEN
                  IF (COMMENT.EQ.0) COUNT=COUNT+1
                  COMMENT=COMMENT+1
                  MULTI=.TRUE.
               ELSE
                  COUNT=COUNT+1
                  MULTI=.FALSE.
               END IF
            ELSE IF (CHAR1.EQ.'H') THEN
               IF (FITS_ARRAY(INDX)(1:8).EQ.'HISTORY') THEN
                  IF (HISTORY.EQ.0) COUNT=COUNT+1
                  HISTORY=HISTORY+1
                  MULTI=.TRUE.
               ELSE
                  COUNT=COUNT+1
                  MULTI=.FALSE.
               END IF
            ELSE IF (CHAR1.EQ.' ') THEN
               IF (FITS_ARRAY(INDX)(1:8).EQ.' ') THEN
                  IF (BLANK.EQ.0) COUNT=COUNT+1
                  BLANK=BLANK+1
                  MULTI=.TRUE.
               ELSE
                  COUNT=COUNT+1
                  MULTI=.FALSE.
               END IF
            ELSE
               COUNT=COUNT+1
               MULTI=.FALSE.
            END IF
            IF (COUNT.GE.NTH) THEN
               MORE=.FALSE.
               ISTR=INDX
            END IF
         END IF
         INDX=INDX+1
         IF (INDX.GT.MAX_NDF_FITS) MORE=.FALSE.
      END DO
C
      IF (ISTR.GT.0)THEN
         ELEMENTS=1
         ITEM=FITS_ARRAY(ISTR)(1:8)
         CHAR1=ITEM(1:1)
         IF (MULTI) THEN
            DO INDX=ISTR+1,MAX_NDF_FITS
               IF (FITS_REFS(INDX).EQ.REF_SLOT) THEN
                  IF (CHAR1.EQ.FITS_ARRAY(INDX)(1:1)) THEN
                     IF (ITEM.EQ.FITS_ARRAY(INDX)(1:8)) THEN
                        ELEMENTS=ELEMENTS+1
                     END IF
                  END IF
               END IF
            END DO
         END IF
      END IF
C
      END
