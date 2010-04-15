C+
C                       D S A _ F I T S _ B U F F
C
C  Routine name:
C     DSA_FITS_BUFF
C
C  Function:
C     Obtains a slot for a new string in the FITS buffers.
C
C  Description:
C     Some strings that are written into the FITS section of a
C     DSA structure are written directly out to the structure.
C     Others - the comment keywords, `COMMENT', `HISTORY' and blank -
C     may legitimately by used a number of times and so have to be
C     buffered by the system.  (The alternative would be to extend
C     the arrays used each time a new string was output, which is
C     highly inefficient.) This routine looks in the FITS buffers used
C     by the DSA system for a slot to use for a new string.  If necessary
C     it will flush out the buffers.  If called with an item that does
C     not need to be buffered, it returns a slot number of zero, so
C     can be used to determine if an item needs buffering or not.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_FITS_BUFF (REF_SLOT,ITEM,STRING,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT           (Integer,ref) The reference number of the
C                            common slot for the structure to receive
C                            the FITS string.
C     (>) ITEM               (Fixed string,descr) The name (the FITS
C                            keyword) for the item.  Case insensitive.
C     (<) STRING             (Integer,ref) The number of the FITS_STRINGS
C                            string reserved by this routine.  If returned
C                            as zero (and with STATUS zero) then the
C                            string does not need to be buffered.
C     (!) STATUS             (Integer,ref) Status code.  This routine
C                            returns immediately if passed a bad status
C                            value.
C
C  External variables used:
C     Only common variables internal to the DSA system.
C
C  External subroutines / functions used:
C     DSA__FLUSH_FITS, ICH_FOLD
C
C  Prior requirements:
C     DSA_OPEN should have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C
C  Note:
C     This is a DSA system internal routine and is not intended to be
C     called from outside the DSA system.
C-
C  Subroutine / function details:
C     DSA__FLUSH_FITS   Flush the FITS buffer for a specified structure
C     ICH_FOLD          Convert a string to upper case
C
C  Common variable details:
C     (>) MAX_FITSTR    (Integer parameter) Maximum number of buffer strings.
C     (!) FITS_CODES    (Integer array) Code (comment, history, etc) for
C                       each buffered string, and ref_slot number.
C  History:
C     29th Nov  1988.   Original version.  KS / AAO.
C     13th Feb  1990.   DSA_FLUSH_FITS name change to DSA__.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C     25th Jul 1996     Moved DATA statement.  MJC/Starlink, UCL.
C
C  Note:
C     This routine is for the original Figaro data format and assumes
C     that FITS items are held in file.FITS.item.  It should not be
C     called (or needed) for NDF format data files.
C+
      SUBROUTINE DSA_FITS_BUFF (REF_SLOT,ITEM,STRING,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, STRING, STATUS
      CHARACTER*(*) ITEM
C
C     Functions used
C
      INTEGER ICH_FOLD
C
C     DSA system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Number of buffered keywords and their names - note, the order
C     here must match that used by DSA__FLUSH_FITS.  Really, an include
C     file should be used here.
C
      INTEGER NKEYS
      PARAMETER (NKEYS=3)
      CHARACTER KEYS(NKEYS)*8
C
C     Local variables
C
      INTEGER   IKEY           ! Index value for possible buffered keywords
      INTEGER   INVOKE         ! Dummy function reference
      INTEGER   ISTRNG         ! Index through strings in the buffer
      CHARACTER ITEM_UC*9      ! Upper case version of ITEM
      INTEGER   KEY            ! Number of buffered keyword being used
      INTEGER   SLOT           ! Reference slot number for a string
C
C     DATA statements
C
      DATA KEYS/'        ','COMMENT ','HISTORY '/
C
C     Return immediately if bad status passed.
C
      IF (STATUS.NE.0) RETURN
C
C     First, see if this is a keyword that needs buffering, and if
C     so get its code value.
C
      ITEM_UC=ITEM
      INVOKE=ICH_FOLD(ITEM_UC)
      KEY=0
      DO IKEY=1,NKEYS
         IF (KEYS(IKEY).EQ.ITEM_UC) THEN
            KEY=IKEY
            GO TO 320   ! Break out of IKEY loop
         END IF
      END DO
  320 CONTINUE
C
C     If this isn't one that needs buffering, that's all we need.
C
      IF (KEY.EQ.0) THEN
         STRING=0
      ELSE
C
C        It does need buffering, so look for a slot.
C
         STRING=0
         DO ISTRNG=1,MAX_FITSTR
            IF (FITS_CODES(ISTRNG).EQ.0) THEN
               STRING=ISTRNG
               GO TO 340      ! Break out of ISTRNG loop
            END IF
         END DO
  340    CONTINUE
         IF (STRING.EQ.0) THEN
C
C           If there is no slot available, first flush out the
C           items for this structure.
C
            CALL DSA__FLUSH_FITS (REF_SLOT,STATUS)
            IF (STATUS.NE.0) GO TO 500    ! Error exit
C
C           Now repeat the exercise
C
            DO ISTRNG=1,MAX_FITSTR
               IF (FITS_CODES(ISTRNG).EQ.0) THEN
                  STRING=ISTRNG
                  GO TO 360      ! Break out of ISTRNG loop
               END IF
            END DO
  360       CONTINUE
C
C           If there is still no spare slot (ie there were no
C           items buffered for this structure at all), flush out
C           the items for the structure using the first slot.
C           This is guaranteed to make the first slot available.
C
            IF (STRING.EQ.0) THEN
               SLOT=FITS_CODES(1)/256
               CALL DSA__FLUSH_FITS(SLOT,STATUS)
               IF (STATUS.NE.0) GO TO 500      ! Error exit
               STRING=1
            END IF
         END IF
C
C        Now we have a slot for our string, so reserve it.  Note the
C        way the code is used - the least significant byte is the item
C        type (keyword), the rest is the reference slot for the structure.
C
         FITS_CODES(STRING)=REF_SLOT*256+KEY
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
