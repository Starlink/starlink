C+
C                    D S A _ _ F I T S _ S P A C E
C
C  Routine name:
C     DSA__FITS_SPACE
C
C  Function:
C     Attempts to get space for a specified number of FITS strings.
C
C  Description:
C     This routine attempts to get space in the common array FITS_ARRAY
C     for a specified number of FITS header strings.  If it cannot find
C     enough contiguous slots in the array it will compact the array,
C     removing any empty slots. If it still cannot find enough space, it
C     will generate an error message and return an error status code.  This
C     routine will flag the slots as in use for the specified reference
C     slot and will set the item numbers to the next available ones for
C     the slot.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__FITS_SPACE (REF_SLOT,STRINGS,FIRST,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT         (Integer,ref) Reference slot number for the
C                          structure being accessed.  This is only used
C                          if an error message has to be generated.
C     (>) STRINGS          (Integer,ref) The number of strings for which
C                          space must be found.
C     (<) FIRST            (Integer,ref) The arrary index of the first
C                          string in the block made available by this
C                          routine.
C     (!) STATUS           (Integer,ref) Status code.  If bad status is
C                          passed to it, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA routines
C
C  External subroutines / functions used:
C     DSA_WRUSER, ICH_LEN
C
C  Prior requirements:
C     DSA_OPEN should have been called to initialise the common variables.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN            Position of last non-blank character in string
C     DSA_WRUSER         Write message to user
C
C  Common variable details:
C     (>) ACTUAL_NAMES  (String array) The fully extended name for the structure
C     (>) MAX_NDF_FITS  (Integer parameter) Maximum number of NDF FITS strings.
C     (!) FITS_ARRAY    (String array) Used for all strings read from NDF
C                       format FITS structures.
C     (!) FITS_ITEMS    (Integer array) FITS item numbers for each array entry.
C                       This matches the string number in the original FITS
C                       header, but is -ve if the string has been modified.
C     (!) FITS_REFS     (Integer array) Reference slot numbers associated with
C                       each array entry.  0 implies unused.
C
C  History:
C     8th  Feb  1990.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA__FITS_SPACE (REF_SLOT,STRINGS,FIRST,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, STRINGS, FIRST, STATUS
C
C     Functions used
C
      INTEGER   ICH_LEN
C
C     DSA common variables
C
      INCLUDE 'DSA_COMMON'
C
C     DSA error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   AVAIL          ! Number of unused strings in the array
      LOGICAL   CLEAR          ! True if we are in an unused part of the array
      INTEGER   COUNT          ! Number of contiguous unused strings
      INTEGER   EMPTY_SLOT     ! Number of first empty slot in array
      LOGICAL   FOUND          ! True if there is enough space in the array
      INTEGER   ISTR           ! Index into the FITS arrays
      INTEGER   ITEM           ! Number of items for this ref slot in array
      LOGICAL   MORE           ! Use to control loops through the array
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Run through the array looking for contiguous space.  We exit the
C     loop when we've either found a big enough amount of spare space,
C     or at the end of the array.  On the way through, we count up the
C     number of unused slots (AVAIL). Note that CLEAR is set when we're
C     moving through a set of unused strings.  This code could be simpler
C     since we now expect the array always to be compacted, ie we don't
C     expect to find unused blocks in front of used ones (and the code
C     added here to count the number of items already in use for this
C     ref slot does assume that the array is compacted - ie by the time
C     we find the free space, we've passed through all the slots used by
C     this ref slot)
C
      ITEM=0
      AVAIL=0
      FOUND=.FALSE.
      MORE=.TRUE.
      ISTR=1
      CLEAR=.FALSE.
      EMPTY_SLOT=0
      DO WHILE (MORE)
         IF (FITS_REFS(ISTR).EQ.REF_SLOT) ITEM=ITEM+1
         IF (CLEAR) THEN
            IF (FITS_REFS(ISTR).EQ.0) THEN
               AVAIL=AVAIL+1
               COUNT=COUNT+1
               IF (COUNT.GE.STRINGS) THEN
                  MORE=.FALSE.
                  FIRST=EMPTY_SLOT
                  FOUND=.TRUE.
               END IF
            ELSE
               CLEAR=.FALSE.
            END IF
         ELSE
            IF (FITS_REFS(ISTR).EQ.0) THEN
               CLEAR=.TRUE.
               EMPTY_SLOT=ISTR
               COUNT=1
            END IF
         END IF
         ISTR=ISTR+1
         IF (ISTR.GT.MAX_NDF_FITS) MORE=.FALSE.
      END DO
C
C     If we didn't find it, see if we can get enough space by compacting
C     the array.
C
      IF (.NOT.FOUND) THEN
         IF (AVAIL.LT.STRINGS) THEN
            CALL DSA_WRUSER ('Unable to find work space for the FITS')
            CALL DSA_WRUSER (' items associated with the structure ')
            CALL DSA_WRUSER (ACTUAL_NAMES(REF_SLOT)
     :                          (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
            CALL DSA_WRUSER (
     :                '. Too many FITS items for internal buffers.')
            CALL DSA_WRFLUSH
            STATUS=DSA__XSFITS
            GO TO 500              ! Error exit
         END IF
C
C        The following loop slowly works through the array, moving up
C        any used slots into the first empty slot above them.  This
C        compacts the array, but maintains the order of the strings in it.
C
         EMPTY_SLOT=0
         DO ISTR=1,MAX_NDF_FITS
            IF (FITS_REFS(ISTR).EQ.0) THEN
               IF (EMPTY_SLOT.EQ.0) EMPTY_SLOT=ISTR
            ELSE
               IF (EMPTY_SLOT.NE.0) THEN
                  FITS_ARRAY(EMPTY_SLOT)=FITS_ARRAY(ISTR)
                  FITS_REFS(EMPTY_SLOT)=FITS_REFS(ISTR)
                  FITS_ITEMS(EMPTY_SLOT)=FITS_ITEMS(ISTR)
                  FITS_REFS(ISTR)=0
                  EMPTY_SLOT=MIN(EMPTY_SLOT+1,ISTR)
               END IF
            END IF
         END DO
         FIRST=EMPTY_SLOT
      END IF
C
C     Now flag the slots as in use and set the item numbers.
C
      DO ISTR=FIRST,FIRST+STRINGS-1
         ITEM=ITEM+1
         FITS_REFS(ISTR)=REF_SLOT
         FITS_ITEMS(ISTR)=ITEM
      END DO
C
C     Exit
C
  500 CONTINUE
C
      END
