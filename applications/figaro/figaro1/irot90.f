C+
      SUBROUTINE IROT90
C
C     I R O T 9 0
C
C     Rotates an image through 90 degrees
C
C     Command parameters -
C
C     IMAGE  The name of the structure containing the image.
C
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for IMAGE.  In any case, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C     Command keywords -
C
C     ANTI   If specified, the rotation is anti-clockwise.
C
C                                      KS / CIT 7th March 1983
C     Modified:
C
C     19 May 1986  KS / AAO.  Close down order changed so that
C                  program unmaps before renaming.  This is a
C                  requirement of the new HDS based system.
C     01 Nov 1988  JM / RAL. Modified to use DSA_ routines
C                  Dynamic memory handling changed to use
C                  DYN_ routines.
C                  Now handles rotation of error and quality arrays.
C                  Program creates a new file even in `in situ' case.
C                  This is necessary as the alternative would involve
C                  reshaping the input array whilst mapped - which
C                  can't be done.
C     22 Jan 1990  KS / AAO. Now uses GEN_ROTB to rotate the quality
C                  array, and checks for flagged data values before
C                  mapping the main array.  Also unmaps data arrays
C                  explicitly after rotation to save virtual memory.
C                  And finally reverses the X array!
C     13 Jan 1992  JRL / RGO. Now asks whether to rotate clockwise or
C                  anti clockwise.
C     13 Oct 1992  HME / UoE, Starlink.  INCLUDE changed. Renamed from
C                  ROTATE to IROT90.
C     21 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Bad pixel handling.
C     26 Jul 1996  MJCL / Starlink, UCL.  Added parameter ABORT
C                  checking.
C     2005 June 8  MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL GEN_CHKNSF
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
      LOGICAL   AEXIST           ! Checks for existence of axes
                                 ! structures
      LOGICAL   ANTI             !
      INTEGER   APTR             ! Dynamic-mem pointer for axis data
      INTEGER   AXPTR            ! Dynamic-mem pointer for axis data
      INTEGER   DIMS(2)          ! Image (or axis) dimensions
      LOGICAL   EEXIST           ! True if image has an error array
      INTEGER   EPTR             ! Dynamic-mem pointer for input errors
      INTEGER   EPTRO            ! Dynamic-mem pointer for output errors
      LOGICAL   FLAGGED          ! True if data array has flagged values
      INTEGER   IPTR             ! Dynamic-mem pointer for IMAGE data
      LOGICAL   ISNEW            ! Is address new to CNF?
      INTEGER   J                ! Do loop variable
      INTEGER   MODE             !
      INTEGER   NAX              !
      INTEGER   NAXOUT           ! Axis number
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image - ignored
      INTEGER   NLINE            ! Second dimension of original image
      INTEGER   NPIX             ! First dimension of original image
      INTEGER   OPTR             ! Dynamic-mem pointer for OUTPUT data
                                 ! array
      LOGICAL   REV              ! True if axis data needs reversing
      LOGICAL   SINGLE           ! True if axis width is a single value
      INTEGER   SLOT             ! Slot number for input mapped data
      INTEGER   SLOTO            ! Slot number for output mapped data
      INTEGER   STATUS           ! Running status for DSA routines
      INTEGER   TPTR             ! Temporary dynamic-mem pointer for
                                 ! axis
      LOGICAL   WEXIST           ! True if axis has width data
      DOUBLE PRECISION WIDTH     ! Width value for axis, if single -
                                 ! ignored
      INTEGER   WPTR             ! Dynamic-mem pointer for axis-width
                                 ! data
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Open IMAGE file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Get size of data in IMAGE
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)

      NLINE=DIMS(2)
      NPIX=DIMS(1)
C
C     Map input data
C
      CALL DSA_USE_FLAGGED_VALUES ('IMAGE',STATUS)
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      CALL DSA_SEEK_FLAGGED_VALUES ('IMAGE',FLAGGED,STATUS)
C
C     anti-clockwise?
C
      CALL PAR_RDKEY('ANTI',.TRUE.,ANTI)
      IF ( PAR_ABORT() ) GO TO 500
      MODE = 1
      IF (.NOT. ANTI) MODE = -1
C
C     Get output structure name. New file needed due to data reshaping.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
C
C     Map output data
C
      DIMS(2)=NPIX
      DIMS(1)=NLINE
      IF (FLAGGED) CALL DSA_USE_FLAGGED_VALUES ('OUTPUT',STATUS)
      CALL DSA_RESHAPE_DATA('OUTPUT','IMAGE',NDIM,DIMS,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',OPTR,SLOTO,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Rotate input into output
C
      CALL JRL_ROT2D(%VAL(CNF_PVAL(IPTR)),NPIX,NLINE,MODE,
     :               %VAL(CNF_PVAL(OPTR)))
      CALL DSA_UNMAP(SLOT,STATUS)
      CALL DSA_UNMAP(SLOTO,STATUS)
C
C     Map and perform rotation on input and output error arrays if
C     the former exists.
C     Reshaping of the error array is automatically performed
C     by DSA_RESHAPE_DATA.
C
      CALL DSA_SEEK_ERRORS('IMAGE',EEXIST,STATUS)
      IF(EEXIST)THEN
         CALL DSA_MAP_ERRORS('IMAGE','READ','FLOAT',EPTR,SLOT,STATUS)
         CALL DSA_MAP_ERRORS('OUTPUT','WRITE','FLOAT',EPTRO,SLOTO,
     :                        STATUS)
         IF(STATUS.NE.0)GOTO 500
         CALL JRL_ROT2D(%VAL(CNF_PVAL(EPTR)),NPIX,NLINE,MODE,
     :                  %VAL(CNF_PVAL(EPTRO)))
         CALL DSA_UNMAP(SLOT,STATUS)
         CALL DSA_UNMAP(SLOTO,STATUS)
      END IF
C
C    Any axis structures must now be copied to the appropriate output
C    axis structure.
C
      DO J=1,2
         CALL DSA_SEEK_AXIS('IMAGE',J,AEXIST,STATUS)
         IF(AEXIST)THEN
            NAXOUT=J-1
            IF(NAXOUT.EQ.0)NAXOUT=2
            CALL DSA_AXIS_SIZE('IMAGE',J,2,NDIM,DIMS,NELM,STATUS)
            CALL DSA_RESHAPE_AXIS('OUTPUT',NAXOUT,'IMAGE',J,
     :                             NDIM,DIMS,STATUS)
         END IF
      END DO
C
C     Note that the rotation is such that X -> Y but Y -> -X, so
C     if there is a new X-array, it should be reversed, so long as
C     it isn't just the numbers 1..N (in each cross-section). If the
C     axis is reversed, any width array should be reversed too.
C
      IF (MODE .EQ. 1) THEN
         NAX = 1
      ELSE
         NAX = 2
      END IF
      CALL DSA_SEEK_AXIS ('OUTPUT',NAX,AEXIST,STATUS)
      IF (AEXIST) THEN
         CALL DSA_AXIS_SIZE ('OUTPUT',NAX,2,NDIM,DIMS,NELM,STATUS)
         CALL DSA_MAP_AXIS_DATA ('OUTPUT',NAX,'UPDATE','FLOAT',AXPTR,
     :                           SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
         REV=.FALSE.
         IF (NDIM.EQ.1) DIMS(2)=1
         APTR = AXPTR
         DO J=1,DIMS(2)
            REV = .NOT.GEN_CHKNSF(%VAL(CNF_PVAL(APTR)),DIMS(1))

C   Increment the pointer for the offset.  Tidy CNF pointer resource.
            CALL DYN_INCAD(APTR,'FLOAT',DIMS(1),TPTR,ISNEW,STATUS)
            IF (J.GT.1.AND.ISNEW) CALL CNF_UNREGP(APTR)
            APTR=TPTR
         END DO
         IF (REV) THEN

C  Return to the original pointer.
            APTR=AXPTR
            CALL GEN_REV2D(%VAL(CNF_PVAL(APTR)),DIMS(1),DIMS(2),.TRUE.,
     :                     %VAL(CNF_PVAL(APTR)))
            CALL DSA_UNMAP(SLOT,STATUS)
            CALL DSA_SEEK_WIDTH ('OUTPUT',NAX,WEXIST,SINGLE,WIDTH,
     :                           STATUS)
            IF (WEXIST.AND.(.NOT.SINGLE)) THEN
               CALL DSA_MAP_WIDTH ('OUTPUT',NAX,'UPDATE','FLOAT',
     :                             WPTR,SLOT,STATUS)
               IF (STATUS.NE.0) GO TO 500
               CALL GEN_REV2D(%VAL(CNF_PVAL(WPTR)),DIMS(1),DIMS(2),
     :                        .TRUE.,%VAL(CNF_PVAL(WPTR)))
               CALL DSA_UNMAP(SLOT,STATUS)
            END IF
         END IF
         IF (ISNEW) CALL CNF_UNREGP(TPTR)
      END IF
C
  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE JRL_ROT2D (IN,IX,IY,MODE,OUT)
C
C     J R L _ R O T 2 D
C
C     Rotates a 2D array through 90 degrees.  The array can
C     be any numeric type with 4 byte elements (ie REAL or
C     INTEGER but not INTEGER*2 or REAL*8).  This routine
C     attempts to operate efficiently by manipulating the
C     working set and adjusting the way it handles the data
C     according to the size of the working set.  This
C     routine is VAX specific, both in coding and concept.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IN      (Real or Integer array IN(IX,IY)) The data
C                 to be rotated.
C     (>) IX      (Integer) The first dimension of the input
C                 array.
C     (>) IY      (Integer) The second dimension of the input
C                 array.
C     (>) MODE    (Integer) +1 for direct (anti-clockwise) rotation, -1
C                 for retrograde rotation.
C     (<) OUT     (Real or Integer array OUT(IY,IX)) The
C                 rotated data.
C
C                                        KS / CIT  23rd Nov 1982
C     History:
C     23rd Nov 1982  KS / CIT.  Original version.
C     13th Jan 1992  JRL / RGO.  MODE argument added.
C     7th  Oct 1992  HME / UoE, Starlink.  Remove VAX SYS$ calls.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IX,IY,MODE
      INTEGER IN(1),OUT(1)
C
C     Local variables
C
      INTEGER SMALL,NEWRK,SMINC,LGINC,IYINC,IXINC
      INTEGER IXRES1,IXRES2,IYRES1,IYRES2,I,J
      INTEGER OUTBASE,INBASE,OUTINDEX,ININDEX
C
C     Get the smaller of the two array dimensions
C
      SMALL=MIN(IX,IY)
C
C     Find the current working set size.
C
C     CALL SYS$ADJWSL(,CURWRK)
C     CALL SYS$ADJWSL(%VAL(2048),NEWRK)
      NEWRK = 2048
C
C     The rotation is going to be performed in blocks SMINC
C     by LGINC. The criterion for an efficient rotation is that
C     the size of this block should be such that all the elements
C     can be held in the working set. 128 is the number of 4 byte
C     elements that fit into a page.
C
      IF (SMALL.LT.128) THEN
         SMINC=SMALL
      ELSE
         SMINC=128
      END IF
      LGINC=(REAL(NEWRK)*.75-REAL(SMINC))/(REAL(SMINC)*2./128.+1)
      IF (LGINC.GT.128) LGINC=(LGINC/128)*128
C
C     Now perform the rotation
C
      IF (SMALL.EQ.IY) THEN
         IYINC=SMINC
         IXINC=LGINC
      ELSE
         IXINC=SMINC
         IYINC=LGINC
      END IF
C
      DO IXRES1=1,IY,IYINC
         IXRES2=MIN(IY,IXRES1+IYINC-1)
         DO IYRES1=1,IX,IXINC
            IYRES2=MIN(IX,IYRES1+IXINC-1)
C
C            The section of code that follows is exactly
C            equivalent to -
C
C            do i=ixres1,ixres2
C               do j=iyres1,iyres2
C                  iyin=iy-i+1
C                  out(i,j)=in(j,iyin)
C               end do
C            end do
C
C            if IN is IN(IX,IY) and OUT is OUT(IY,IX), but
C            the compiler generates much better code from this
C            linear array version.
C
            OUTBASE=(IYRES1-1)*IY-1
            IF (MODE .EQ. 1) THEN
               INBASE=(IY-IXRES1)*IX-1
            ELSE
               INBASE = IXRES1*IX
            END IF
            DO I=IXRES1,IXRES2
               OUTINDEX=I+OUTBASE
               ININDEX=MODE*IYRES1+INBASE
               DO J=IYRES1,IYRES2
                  OUT(OUTINDEX+1)=IN(ININDEX+1)
                  OUTINDEX=OUTINDEX+IY
                  ININDEX=ININDEX+MODE
               END DO
               INBASE=INBASE-MODE*IX
            END DO
C
         END DO
      END DO
C
C     Restore the old working set limit
C
C     CALL SYS$ADJWSL(%VAL(NEWRK-CURWRK),CURWRK)
C
      END
C+
      SUBROUTINE JRL_ROTB (IN,IX,IY,MODE,OUT)
C
C     J R L _ R O T B
C
C     Rotates a 2D array through 90 degrees.  The array can
C     be any numeric type with 1 byte elements (ie BYTE).
C     This routine attempts to operate efficiently by manipulating
C     the working set and adjusting the way it handles the data
C     according to the size of the working set.  This
C     routine is VAX specific, both in coding and concept.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IN      (Byte array IN(IX,IY)) The data
C                 to be rotated.
C     (>) IX      (Integer) The first dimension of the input
C                 array.
C     (>) IY      (Integer) The second dimension of the input
C                 array.
C     (>) MODE    (Integer) +1 for direct (anti-clockwise) rotation, -1
C                 for retrograde rotation.
C     (<) OUT     (Byte array OUT(IY,IX)) The rotated data.
C
C
C                                        KS / AAO 22nd Jan 1990
C     History:
C     22nd Jan 1990  KS / AAO.  Original version.
C     13th Jan 1992  JRL / RGO.  MODE argument added.
C     7th  Oct 1992  HME / UoE, Starlink.  Remove VAX SYS$ calls.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IX,IY,MODE
      BYTE IN(IX*IY),OUT(IX*IY)
C
C     Local variables
C
      INTEGER SMALL,NEWRK,SMINC,LGINC,IYINC,IXINC
      INTEGER IXRES1,IXRES2,IYRES1,IYRES2,I,J
      INTEGER OUTBASE,INBASE,OUTINDEX,ININDEX
C
C     Get the smaller of the two array dimensions
C
      SMALL=MIN(IX,IY)
C
C     Find the current working set size.
C
C     CALL SYS$ADJWSL(,CURWRK)
C     CALL SYS$ADJWSL(%VAL(2048),NEWRK)
      NEWRK = 2048
C
C     The rotation is going to be performed in blocks SMINC
C     by LGINC. The criterion for an efficient rotation is that
C     the size of this block should be such that all the elements
C     can be held in the working set. 512 is the number of 1 byte
C     elements that fit into a page.
C
      IF (SMALL.LT.512) THEN
         SMINC=SMALL
      ELSE
         SMINC=512
      END IF
      LGINC=(REAL(NEWRK)*.75-REAL(SMINC))/(REAL(SMINC)*2./512.+1)
      IF (LGINC.GT.512) LGINC=(LGINC/512)*512
C
C     Now perform the rotation
C
      IF (SMALL.EQ.IY) THEN
         IYINC=SMINC
         IXINC=LGINC
      ELSE
         IXINC=SMINC
         IYINC=LGINC
      END IF
C
      DO IXRES1=1,IY,IYINC
         IXRES2=MIN(IY,IXRES1+IYINC-1)
         DO IYRES1=1,IX,IXINC
            IYRES2=MIN(IX,IYRES1+IXINC-1)
C
C            The section of code that follows is exactly
C            equivalent to -
C
C            do i=ixres1,ixres2
C               do j=iyres1,iyres2
C                  iyin=iy-i+1
C                  out(i,j)=in(j,iyin)
C               end do
C            end do
C
C            if IN is IN(IX,IY) and OUT is OUT(IY,IX), but
C            the compiler generates much better code from this
C            linear array version.
C
            OUTBASE=(IYRES1-1)*IY-1
            IF (MODE .EQ. 1) THEN
               INBASE=(IY-IXRES1)*IX-1
            ELSE
               INBASE = IXRES1*IX
            END IF
            DO I=IXRES1,IXRES2
               OUTINDEX=I+OUTBASE
               ININDEX=MODE*IYRES1+INBASE
               DO J=IYRES1,IYRES2
                  OUT(OUTINDEX+1)=IN(ININDEX+1)
                  OUTINDEX=OUTINDEX+IY
                  ININDEX=ININDEX+MODE
               END DO
               INBASE=INBASE-MODE*IX
            END DO
C
         END DO
      END DO
C
C     Restore the old working set limit
C
C     CALL SYS$ADJWSL(%VAL(NEWRK-CURWRK),CURWRK)
C
      END
