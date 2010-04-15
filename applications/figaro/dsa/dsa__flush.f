C+
C                       D S A _ _ F L U S H _ F I T S
C
C  Routine name:
C     DSA__FLUSH_FITS
C
C  Function:
C     Flushes out any buffered FITS strings for a specified structure.
C
C  Description:
C     This routine clears out of the FITS string buffer all the strings
C     for a specified structure.  This needs to be called as part of
C     the closing down sequence for each structure, but can also be
C     called at any time in order to clear out space in the buffer.
C     Since it may involve the resizing of a data object in the structure
C     use of this routine should be limited to the times when it is
C     absolutely necessary.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__FLUSH_FITS (REF_SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT           (Integer,ref) The reference number of the
C                            common slot for the structure.
C     (!) STATUS             (Integer,ref) Status code.  This routine
C                            returns immediately if passed a bad status
C                            value.
C
C  External variables used:
C     Only common variables internal to the DSA system.
C
C  External subroutines / functions used:
C     DTA_CRNAM, DTA_CRVAR, DTA_ERROR, DTA_RNVAR, DTA_SZVAR, DTA_WRVARC
C     DSA_WRNAME, DSA_WRUSER, ICH_LEN
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
C     DTA_CRNAM     Generate the name of a DTA data object
C     DTA_CRVAR     Create a data object
C     DTA_ERROR     Get description of a DTA error code
C     DTA_RNVAR     Change name (or dimensions) of a data object
C     DTA_SZVAR     Get size of a data object
C     DTA_WRVARC    Write a character string to a data object
C     DSA_WRNAME    Write name of a data object to the user
C     DSA_WRUSER    Write a message to the user
C     ICH_LEN       Position of last non-blank char in string
C
C  Common variable details:
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C     (>) MAX_FITSTR    (Integer parameter) Maximum number of buffer strings.
C     (>) FITS_STRINGS  (String array) Comment strings being buffered.
C     (!) FITS_CODES    (Integer array) Code (comment, history, etc) for
C                       each buffered string, and ref_slot number.
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
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
C     29th Nov  1988.   Original version.  KS / AAO.
C     13th Feb  1990.   Renamed to DSA__ and modified to support NDF format
C                       files.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C     25th Jul 1996     Move DATA statement for Linux.  MJCL/Starlink, UCL.
C
C  Note:
C     This version supports both the original Figaro data format and
C     Starlink's NDF format.
C+
      SUBROUTINE DSA__FLUSH_FITS (REF_SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, STATUS
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     DSA system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Number of buffered keywords and their names - note, the order
C     here must match that used by DSA_FITS_BUFF.  Really, an include
C     file should be used here.
C
      INTEGER NKEYS
      PARAMETER (NKEYS=3)
      CHARACTER KEYS(NKEYS)*8
C
C     Local variables
C
      INTEGER   COUNT(NKEYS)   ! Occurrences of strings for each keyword
      INTEGER   DIMS(2)        ! Dimensions of data object
      INTEGER   DTA_STATUS     ! Status returned by DTA routine
      INTEGER   EMPTY_SLOT     ! Number of free string in NDF common array
      CHARACTER ERROR*64       ! DTA error description
      LOGICAL   EXIST          ! True if keyword already in structure
      INTEGER   FIRST_MOD      ! First modified string in block
      LOGICAL   FLUSH          ! True if there are FITS strings to write out
      INTEGER   FULL_SLOT      ! Number of last non-blank NDF string
      LOGICAL   GOT_END        ! True if NDF strings include an END record
      INTEGER   I              ! General loop index
      INTEGER   IKEY           ! Index value for possible buffered keywords
      INTEGER   ISTR           ! Index through strings in the NDF common buffer
      INTEGER   ISTRNG         ! Index through strings in the buffer
      INTEGER   LAST           ! Number of last NDF string for this ref slot
      INTEGER   LAST_MOD       ! Last modified string in block
      INTEGER   MODIFIED       ! Number of NDF strings modified for this slot
      LOGICAL   MORE           ! Used to control loop through NDF FITS array
      CHARACTER NAME*80        ! Name of FITS data object
      INTEGER   NCHAR          ! Length of NDF FITS strings in actual file array
      INTEGER   NDIM           ! Number of dimensions of data object
      CHARACTER NEWNAME*80     ! Name of FITS data object with new dimensions
      INTEGER   SLOT           ! Reference slot number for a string
      INTEGER   STRINGS        ! Number of NDF strings in common for this slot
      INTEGER   STRPTR         ! String to write to in structure array
C
      DATA KEYS/'        ','COMMENT ','HISTORY '/
C
C     Return immediately if bad status passed.
C
      IF (STATUS.NE.0) RETURN
C
C     What happens next defends on what format the data file uses.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        NDF format.  All strings are held in the array FITS_ARRAY.
C        First, we see how many strings there are, and whether the
C        FITS data object in the file (if any) will have to be
C        extended (or contracted, come to that, although that should
C        not happen).
C
         STRINGS=0
         MODIFIED=0
         DO I=1,MAX_NDF_FITS
            IF (FITS_REFS(I).EQ.REF_SLOT) THEN
               STRINGS=STRINGS+1
               IF (FITS_ITEMS(I).LT.0) MODIFIED=MODIFIED+1
               LAST=I
            END IF
         END DO
         IF (MODIFIED.GT.0) THEN
C
C           We remember that the last non-blank string should contain
C           an 'END', so we need to make sure that this is the case.
C           Note that this backwards search assumes that the items in
C           the common array will be in their FITS array item order
C           (there may be gaps, but the order will be correct).
C
            GOT_END=.FALSE.
            FULL_SLOT=0
            DO I=LAST,1,-1
               IF (FITS_REFS(I).EQ.REF_SLOT) THEN
                  IF (FITS_ARRAY(I).NE.' ') THEN
                     FULL_SLOT=I
                     GO TO 320   ! Break I loop
                  END IF
               END IF
            END DO
  320       CONTINUE
            IF (FULL_SLOT.NE.0) THEN
               GOT_END=FITS_ARRAY(FULL_SLOT)(1:8).EQ.'END'
            END IF
            IF (.NOT.GOT_END) THEN
C
C              We don't seem to have an END card, so we need to add one.
C              If there were trailing blank lines, we can use the last
C              one of them.  If not, we need to get a new line, if there
C              is one available.  If there isn't, well, we can do without
C              an 'END' record..
C
               IF (LAST.GT.FULL_SLOT) THEN
                  FITS_ARRAY(LAST)='END'
                  FITS_ITEMS(LAST)=-ABS(FITS_ITEMS(LAST))
                  MODIFIED=MODIFIED+1
               ELSE
                  DO I=LAST+1,MAX_NDF_FITS
                     IF (FITS_REFS(I).EQ.0) THEN
                        FITS_ARRAY(I)='END'
                        FITS_REFS(I)=REF_SLOT
                        STRINGS=STRINGS+1
                        FITS_ITEMS(I)=-STRINGS
                        MODIFIED=MODIFIED+1
                        GO TO 340          ! Break out of I loop
                     END IF
                  END DO
  340             CONTINUE
               END IF
            END IF
C
C           Now we know how many items we have in the common array for
C           this structure, and we know that some have been modified.
C           Now, we see how big the existing array is.  If there is no
C           existing array, we try to create it, if there is one we see
C           if it needs resizing.
C
            NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.MORE'
            CALL DTA_CRVAR (NAME,'EXT',DTA_STATUS)
            NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.MORE.FITS'
            CALL DTA_SZVAR (NAME,2,NDIM,DIMS,DTA_STATUS)
            IF (DTA_STATUS.NE.0) THEN
               NCHAR=80
               DIMS(1)=NCHAR
               DIMS(2)=STRINGS
               CALL DTA_CRNAM (OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                        '.MORE','FITS',2,DIMS,NEWNAME,DTA_STATUS)
               CALL DTA_CRVAR (NEWNAME,'CHAR',DTA_STATUS)
            ELSE
               NCHAR=DIMS(1)
               IF (DIMS(2).NE.STRINGS) THEN
                  DIMS(2)=STRINGS
                  CALL DTA_CRNAM (
     :                        OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                        '.MORE','FITS',2,DIMS,NEWNAME,DTA_STATUS)
                  CALL DTA_RNVAR (NAME,NEWNAME,DTA_STATUS)
               END IF
            END IF
            IF (DTA_STATUS.NE.0) THEN
               CALL DSA_WRUSER ('Error attempting to create or resize '
     :                                             //'the FITS array ')
               CALL DSA_WRNAME (NAME)
               CALL DTA_ERROR (DTA_STATUS,ERROR)
               CALL DSA_WRUSER ('. '//ERROR(:ICH_LEN(ERROR)))
               CALL DSA_WRUSER ('.')
               CALL DSA_WRFLUSH
               DTA_CODE=DTA_STATUS
               STATUS=DSA__DTAERR
               GO TO 500       ! Error exit
            END IF
         END IF
C
C        Finally, we now have a FITS object in the file to write to.
C        We have to allow for the fact that they may be split up in
C        there, and also for the possibility that they may not have
C        been modified and so need not be output.  The most efficient
C        way of updating an array in which only parts have changed is
C        unclear, but what we do here is look for contiguous strings
C        to output, and we output as many as possible in one go, even
C        if intermediate strings are unchanged, on the assumption that
C        the overhead on DTA calls is high and the overhead on writing
C        a lot of data is low.  (WFL wasn't very sure, but thought this
C        was probably correct - perhaps somebody should time it!)  As
C        we go through the array, we clear the ref slot flags for the
C        entries for this structure.
C
         FIRST_MOD=0
         MORE=.TRUE.
         ISTR=1
         DO WHILE (MORE)
            FLUSH=.FALSE.
            IF (FITS_REFS(ISTR).EQ.REF_SLOT) THEN
               FITS_REFS(ISTR)=0
               IF (FITS_ITEMS(ISTR).LT.0) THEN
                  IF (FIRST_MOD.EQ.0) FIRST_MOD=ISTR
               END IF
            ELSE
               IF (FIRST_MOD.NE.0) THEN
                  LAST_MOD=ISTR-1
                  FLUSH=.TRUE.
               END IF
            END IF
            ISTR=ISTR+1
            IF (ISTR.GT.MAX_NDF_FITS) THEN
               MORE=.FALSE.
               IF (FIRST_MOD.NE.0) THEN
                  LAST_MOD=ISTR-1
                  FLUSH=.TRUE.
               END IF
            END IF
            IF (FLUSH) THEN
               DIMS(1)=1
               DIMS(2)=ABS(FITS_ITEMS(FIRST_MOD))
               CALL DTA_CRNAM (OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                            '.MORE','FITS',2,DIMS,NAME,DTA_STATUS)
               CALL DTA_WRVARC (NAME,NCHAR*(LAST_MOD-FIRST_MOD+1),
     :                                 FITS_ARRAY(FIRST_MOD),DTA_STATUS)
               IF (DTA_STATUS.NE.0) THEN
                  CALL DSA_WRUSER ('Error attempting to write to '
     :                                             //'the FITS array ')
                  CALL DSA_WRNAME (NAME)
                  CALL DTA_ERROR (DTA_STATUS,ERROR)
                  CALL DSA_WRUSER ('. '//ERROR(:ICH_LEN(ERROR)))
                  CALL DSA_WRUSER ('.')
                  CALL DSA_WRFLUSH
                  DTA_CODE=DTA_STATUS
                  STATUS=DSA__DTAERR
                  GO TO 500       ! Error exit
               END IF
               FIRST_MOD=0
            END IF
         END DO
C
C        That's finally got all the FITS strings written out to the file.
C        Now we tidy up the common array, removing all entries for this
C        structure, and compacting it.  The compaction is a bit of an
C        overhead, but a number of the algorithms used here assume that
C        items for a single structure appear in order in the common array,
C        so either they need to be cleverer or we have to do this.  (This
C        same code is at the end of DSA__FITS_SPACE, and should really
C        be a separeate routine.)
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
C
      ELSE
C
C        Original Figaro format.  Most strings will have been written
C        directly into the data structure, except for those comment-type
C        keywords which may have multiple values and so are buffered
C        internally.
C
C        Run through the strings, counting the various strings for
C        the different keywords for this ref slot value.
C
         DO IKEY=1,NKEYS
            COUNT(IKEY)=0
         END DO
         DO ISTRNG=1,MAX_FITSTR
            SLOT=FITS_CODES(ISTRNG)/256
            IF (SLOT.EQ.REF_SLOT) THEN
               IKEY=MOD(FITS_CODES(ISTRNG),256)
               COUNT(IKEY)=COUNT(IKEY)+1
            END IF
         END DO
C
C        Now, for each of the keys we found, see how big the array
C        is so far in the file, increase it as required, then write
C        out the strings in the buffer.
C
         DO IKEY=1,NKEYS
            IF (COUNT(IKEY).GT.0) THEN
               IF (KEYS(IKEY).EQ.' ') THEN
                  NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                            '.FITS.BLANK_STRINGS'
               ELSE
                  NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                            '.FITS.'//KEYS(IKEY)
               END IF
               CALL DTA_SZVAR(NAME,2,NDIM,DIMS,DTA_STATUS)
               EXIST=DTA_STATUS.EQ.0
               IF (EXIST) THEN
                  STRPTR=DIMS(2)
                  DIMS(2)=DIMS(2)+COUNT(IKEY)
               ELSE
                  STRPTR=0
                  NDIM=2
                  DIMS(1)=80
                  DIMS(2)=COUNT(IKEY)
               END IF
               IF (KEYS(IKEY).EQ.' ') THEN
                  CALL DTA_CRNAM(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))
     :                      //'.FITS','BLANK_STRINGS',NDIM,DIMS,NEWNAME,
     :                                                       DTA_STATUS)
               ELSE
                  CALL DTA_CRNAM(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))
     :                //'.FITS',KEYS(IKEY),NDIM,DIMS,NEWNAME,DTA_STATUS)
               END IF
C
C              Either create the array for the item, or extend the
C              size of the existing array.
C
               IF (EXIST) THEN
                  CALL DTA_RNVAR(NAME,NEWNAME,DTA_STATUS)
               ELSE
                  CALL DTA_CRVAR(NEWNAME,'CHAR',DTA_STATUS)
               END IF
               IF (DTA_STATUS.NE.0) THEN
                  IF (EXIST) THEN
                     CALL DSA_WRUSER(
     :                'Error attempting to increase size of FITS item ')
                  ELSE
                     CALL DSA_WRUSER(
     :                          'Error attempting to create FITS item ')
                  END IF
                  CALL DSA_WRNAME(NAME)
                  CALL DSA_WRUSER('.  New name specification was "')
                  CALL DSA_WRUSER(NEWNAME(:ICH_LEN(NEWNAME)))
                  CALL DSA_WRUSER('". ')
                  CALL DTA_ERROR(DTA_STATUS,ERROR)
                  CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
                  CALL DSA_WRUSER('.')
                  CALL DSA_WRFLUSH
                  STATUS=DSA__DTAERR
                  DTA_CODE=DTA_STATUS
                  GO TO 500      ! Error exit
               END IF
C
C              Now write out the strings in the buffer.
C
               DO ISTRNG=1,MAX_FITSTR
                  SLOT=FITS_CODES(ISTRNG)/256
                  IF (SLOT.EQ.REF_SLOT) THEN
                     IF (MOD(FITS_CODES(ISTRNG),256).EQ.IKEY) THEN
                        STRPTR=STRPTR+1
                        DIMS(1)=1
                        DIMS(2)=STRPTR
                        IF (KEYS(IKEY).EQ.' ') THEN
                           CALL DTA_CRNAM(
     :                         OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))
     :                            //'.FITS','BLANK_STRINGS',2,DIMS,NAME,
     :                                                      DTA_STATUS)
                        ELSE
                           CALL DTA_CRNAM(
     :                         OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))
     :                            //'.FITS',KEYS(IKEY),2,DIMS,NAME,
     :                                                      DTA_STATUS)
                        END IF
                        CALL DTA_WRVARC(NAME,80,FITS_STRINGS(ISTRNG),
     :                                                      DTA_STATUS)
                        IF (DTA_STATUS.NE.0) THEN
                           CALL DSA_WRUSER(
     :                          'Error attempting to write FITS item ')
                           CALL DSA_WRNAME(NAME)
                           CALL DSA_WRUSER('". ')
                           CALL DTA_ERROR(DTA_STATUS,ERROR)
                           CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
                           CALL DSA_WRUSER('.')
                           CALL DSA_WRFLUSH
                           STATUS=DSA__DTAERR
                           DTA_CODE=DTA_STATUS
                           GO TO 500      ! Error exit
                        END IF
                        FITS_CODES(ISTRNG)=0
                     END IF
                  END IF
               END DO       ! End of loop through strings in buffer
            END IF
         END DO      ! End of loop through buffered keywords
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
