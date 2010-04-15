C+
C                    D S A _ _ R E A D _ N D F _ F I T S
C
C  Routine name:
C     DSA__READ_NDF_FITS
C
C  Function:
C     Ensures that the FITS data (if any) for a structure has been read.
C
C  Description:
C     Given the reference slot number for a structure, this routine
C     sees if anything is known about its FITS specific structure.  If
C     it has not already been looked for, this routine sees if such a
C     thing exists and if it does attempts to read it into the common
C     FITS buffer.  This routine is only for the case where the structure
C     in question has NDF format.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__READ_NDF_FITS (REF_SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT         (Integer,ref) Reference slot number for the
C                          structure in question.
C     (!) STATUS           (Integer,ref) Status code.  If bad status is
C                          passed to it, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA routines
C
C  External subroutines / functions used:
C     ICH_LEN, DTA_SZVAR, DTA_RDVARC, DTA_ERROR, DSA_WRNAME, DSA_WRUSER,
C     DSA__FITS_SPACE
C
C  Prior requirements:
C     The structure in question should already be open and should be
C     an NDF format structure.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN            Position of last non-blank character in string
C     DTA_SZVAR          Get dimensions of data structure object
C     DTA_RDVARC         Read characters from a data structure object
C     DTA_ERROR          Get text for a DTA error code
C     DSA_WRNAME         Write name of a structure object to the user
C     DSA_WRUSER         Write message to user
C     DSA__FITS_SPACE    Try to get space in common FITS_ARRAY for strings
C
C  Common variable details:
C     (<) DTA_CODE     (Integer) Last DTA_ system failure status code.
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C     (!) FITS_OPEN     (Logical array) Indicates FITS processing begun for
C                       this structure.
C     (<) FITS_ARRAY    (String array) Used for all strings read from NDF
C                       format FITS structures.
C     (<) FITS_ITEMS    (Integer array) FITS item numbers for each array entry.
C                       This matches the string number in the original FITS
C                       header, but is -ve if the string has been modified.
C     (<) FITS_REFS     (Integer array) Reference slot numbers associated with
C                       each array entry.  0 implies unused.
C
C  History:
C     8th  Feb  1990.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA__READ_NDF_FITS (REF_SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, STATUS
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
      CHARACTER ARRAY_NAME*64        ! DTA name of FITS array in structure
      LOGICAL   CLEAR                ! True past 'END' in header
      INTEGER   DIMS(2)              ! Dimensions of FITS array
      INTEGER   DTA_STATUS           ! Status code from DTA routines
      CHARACTER ERROR*64             ! Text for DTA system error code
      INTEGER   FIRST                ! First free slot in common FITS array
      INTEGER   I                    ! General loop index
      INTEGER   NDIM                 ! Number of dimensions of FITS array
      INTEGER   NSTR                 ! String number in common FITS array
      INTEGER   STRINGS              ! Number of strings in FITS array
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     See if this structure has already been checked by this routine.
C
      IF (.NOT.FITS_OPEN(REF_SLOT)) THEN
C
C        Generate name of FITS array, and see if it exists
C
         ARRAY_NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                                 '.MORE.FITS'
         CALL DTA_SZVAR (ARRAY_NAME,2,NDIM,DIMS,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
C
C           It does exist, and we know how large it is, so we see if we
C           can find space for it in the FITS common buffer.
C
            IF (NDIM.EQ.1) THEN
               STRINGS=1
            ELSE
               STRINGS=DIMS(2)
            END IF
            CALL DSA__FITS_SPACE (REF_SLOT,STRINGS,FIRST,STATUS)
            IF (STATUS.NE.0) GO TO 500      ! Error exit
C
C           OK, we have space for it, so now read it in.
C
            CALL DTA_RDVARC (ARRAY_NAME,DIMS(1)*STRINGS,
     :                                 FITS_ARRAY(FIRST),DTA_STATUS)
            IF (DTA_STATUS.NE.0) THEN
               CALL DSA_WRUSER ('Error reading FITS array from ')
               CALL DSA_WRNAME (ARRAY_NAME)
               CALL DSA_WRUSER ('. ')
               CALL DTA_ERROR (DTA_STATUS,ERROR)
               CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
               CALL DSA_WRUSER ('.')
               CALL DSA_WRFLUSH
               DTA_CODE=DTA_STATUS
               STATUS=DSA__DTAERR
               GO TO 500                   ! Error exit
            END IF
C
C           Fill in the other common variables relating to these strings.
C
            NSTR=FIRST
            DO I=1,STRINGS
               FITS_REFS(NSTR)=REF_SLOT
               FITS_ITEMS(NSTR)=I
               NSTR=NSTR+1
            END DO
C
C           'END' records are a nuisance, as is the question of how you
C           deal with non-blank strings that follow such a record.  What
C           we do here is erase the END record and any subsequent records.
C           This saves us having to shuffle the END down the array as we
C           add new records.
C
            NSTR=FIRST
            CLEAR=.FALSE.
            DO I=1,STRINGS
               IF (CLEAR) THEN
                  FITS_REFS(NSTR)=0
               ELSE IF (FITS_ARRAY(NSTR)(1:8).EQ.'END') THEN
                  CLEAR=.TRUE.
                  FITS_REFS(NSTR)=0
               END IF
               NSTR=NSTR+1
            END DO
C
         END IF
C
C        Indicate that this slot has been checked and any existing FITS
C        array has been read in.
C
         FITS_OPEN(REF_SLOT)=.TRUE.
C
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
