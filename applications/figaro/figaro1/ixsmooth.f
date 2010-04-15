C+
      SUBROUTINE IXSMOOTH
C
C     I X S M O O T H
C
C     This routine is the main body of IXSMOOTH, which smooths
C     cross-sections of an array in the X-direction by convolving
C     them with a gaussian of specified sigma and a specified
C     cutoff width.  Note that this is NOT a 2D smooth, but a series
C     of 1D smooths; hence the name.
C
C     The VARIANCE array, if present, is propagated in exactly the same
C     way as the DATA array.  This procedure it not formally correct and
C     the computed variance will probably under-estimate the true error.
C
C     Command parameters -
C
C     IMAGE  The name of the structure containing the image.
C
C     SIGMA  (Numeric) The sigma of the gaussian, expressed in array
C            elements.  (This is approximately the half width at
C            half maximum, to within a factor of ~1.17).
C
C     WIDTH  (Numeric) The number of array elements over which the
C            gaussian is calcualted - ie outside this range the
C            gaussian is assumed to be zero.  This simplifies the
C            computation and also allows the use of strangely shaped
C            functions such as gaussians with very large sigmas but
C            small widths - which are almost the same as rectangular
C            filters.
C
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C                                      KS / CIT 23rd March 1983
C
C     Modified:
C
C     28th July 1987  DJA/AAO.Revised DSA_ routines - some specs
C                     changed. Now uses DYN_ package for
C                     dynamic-memory handling. All WRUSERs changed to
C                     PAR_WRUSERs.
C     26th Mar  1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected
C                     in mapping calls.
C     28th May  1992  HME / UoE, Starlink.  Ensure at this level that
C                     WIDTH is an odd positive integer. GEN_GCONV
C                     does not work properly for even width.
C     22nd Sep  1992  HME / UoE, Starlink.  INCLUDE changed.
C     26th Jul  1996  MJCL / Starlink, UCL.  Added PAR_ABORT
C                     checking.
C     28th Oct  2001  VGG / Starlink, RAL and ACD / Starlink, UoE.
C                     Simple error propagation added.
C     2005 June 8     MJC / Starlink  Use CNF_PVAL for pointers to
C                     mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER DSA_TYPESIZE
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
      INTEGER      BYTES         ! Number of bytes of workspace required
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      CHARACTER    HIST(3)*50    ! Array for comments in the NDF history
      LOGICAL      ISNEW         ! Is address new to CNF?
      INTEGER      IY            !
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      NY            ! Size of 2nd dimension (if present)
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 !  array
      INTEGER      OSLOT         ! Map slot number for output data array
      INTEGER      OVPTR         ! Dynamic-memory pointer to output
                                 ! variance array
      INTEGER      OVSLOT        ! Map slot number for output variance
                                 ! array
      LOGICAL      PISNEW        ! Previous CNF pointer to data new?
      INTEGER      SCRPTR        !
      INTEGER      SCSLOT        ! Map slot number of workspace
      REAL         SIGMA         !
      INTEGER      STATUS        ! Running status for DSA_ routines
      INTEGER      TPTR          ! Temporary dynamic mem pointer
      REAL         VALUE         !
      INTEGER      WIDTH         !
      INTEGER      WPTR          ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT         ! Map slot number of workspace
      LOGICAL      VEXIST        ! Variance array exists?
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NY=NELM/NX
C
C     Get values for SIGMA and WIDTH
C
      CALL PAR_RDVAL('SIGMA',0.01,FLOAT(NX),1.,'Pixels',SIGMA)
      CALL PAR_RDVAL('WIDTH',1.,FLOAT(NX),5.*SIGMA,'Pixels',VALUE)
      IF ( PAR_ABORT() ) GO TO 500
      WIDTH=NINT(VALUE)
C
C     Make WIDTH the odd integer less or equal to what it is so far.
C     Note that WIDTH is already positive. The division by an integer is
C     deliberate!
C
      WIDTH = ( ( WIDTH + 1 ) / 2 - 1 ) * 2 + 1
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map data
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Check whether the variance array exists and if so then map it.
C
      VEXIST = .FALSE.
      CALL DSA_SEEK_VARIANCE ('IMAGE',VEXIST,STATUS)
      IF (VEXIST) THEN
         CALL DSA_MAP_VARIANCE('OUTPUT','UPDATE','FLOAT',OVPTR,
     :                         OVSLOT, STATUS)
         IF (STATUS .NE. 0) GOTO 500
      END IF
C
C     Get workspace for a) a result array, b) scratch space needed
C     by GCONV.
C
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',WPTR,WSLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(WIDTH,'FLOAT',SCRPTR,SCSLOT,STATUS)
C
C     Smooth the data, x-section by x-section, into the work
C     area and then copy back into the data.
C
      BYTES=NX*DSA_TYPESIZE('FLOAT',STATUS)
      PISNEW = .FALSE.
      DO IY=1,NY
         CALL GEN_GCONV(%VAL(CNF_PVAL(OPTR)),NX,SIGMA,WIDTH,
     :             %VAL(CNF_PVAL(SCRPTR)),%VAL(CNF_PVAL(WPTR)))
         CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(OPTR)))

         CALL DYN_INCAD(OPTR,'FLOAT',NX,TPTR,ISNEW,STATUS)
         IF (PISNEW) CALL CNF_UNREGP(OPTR)
         OPTR = TPTR
         PISNEW = ISNEW
      END DO
      IF (ISNEW) CALL CNF_UNREGP(TPTR)
C
C     If the variance array exists then smooth it.
C
      IF (VEXIST) THEN
         PISNEW = .FALSE.
         DO IY=1,NY
            CALL GEN_GCONV(%VAL(CNF_PVAL(OVPTR)),NX,SIGMA,WIDTH,
     :                     %VAL(CNF_PVAL(SCRPTR)),%VAL(CNF_PVAL(WPTR)))
            CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(WPTR)),
     :                    %VAL(CNF_PVAL(OVPTR)))

            CALL DYN_INCAD(OVPTR,'FLOAT',NX,TPTR,ISNEW,STATUS)
            IF (PISNEW) CALL CNF_UNREGP(OPTR)
            OVPTR = TPTR
            PISNEW = ISNEW
         END DO
         IF (ISNEW) CALL CNF_UNREGP(TPTR)
C
C        Add cautionary comments to the history section.
C
         HIST(1) =
     :     'The variance array has been simply re-sampled.'
         HIST(2) =
     :     'The resulting values are probably under-estimates'
         HIST(3) = 'of the true errors.'
         CALL DSA_PUT_HIST (OSLOT, 3, HIST, STATUS)
      END IF
C
C     Closedown everything
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
