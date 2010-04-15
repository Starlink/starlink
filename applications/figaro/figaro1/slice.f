C+
      SUBROUTINE SLICE
C
C     S L I C E
C
C     SLICE creates a 'spectrum' by taking a slice through a two
C     dimensional image.  This particular program is essentially
C     lifted from the AIPS package and uses the scheme described there
C     as 'Everett Interpolation'.  For more details see the comments
C     for the various subroutines such as FIG_CSLICE.  Note that the
C     'slice' through the image is conceptually of zero width - ie it
C     is a slice through the interpolated surface represented by the
C     discrete image data.
C
C     Command parameters -
C
C     IMAGE     (Character) The image from which the slice is to be
C               taken.
C     YSTART    (Numeric) The AXIS(2) value for the start of the slice.
C     YEND      (Numeric) The AXIS(2) value for the end of the slice.
C     XSTART    (Numeric) The AXIS(1) value for the start of the slice.
C     XEND      (Numeric) The AXIS(1) value for the end of the slice.
C               Note that YSTART etc can represent any point in the
C               image, and the convention is that axis data values
C               for pixels refer to the CENTERS of those pixels -
C               This means, for example, that if IMAGE is a 256 by 256
C               image, the commands
C               SLICE IMAGE XSTART=1.0 XEND=256.0 YSTART=128.0 YEND=128.0
C               EXTRACT IMAGE,128,128
C               will have exactly the same results.
C     ELEMENTS  (Numeric) Number of pixels in the resulting spectrum.
C     SPECTRUM  (Character) The resulting spectrum.
C
C                                               KS / CIT 23rd March 1984
C     Modified:
C
C     27th Jan 1989  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     17th Jan 1991  JMS / AAO. Added PAR_ABORT calls to support user
C                    requested aborts.
C     18th Jan 1991  JMS / AAO. Changed the maximum number of allowed
C                    data-array dimensions from 10 to 2.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected
C                    in mapping calls.
C     23rd Sep 1992  HME / UoE, Starlink.  INCLUDE changed.
C     27th Jul 1996  MJCL / Starlink, UCL.  One more PAR_ABORT check.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER GEN_BSEARCH        ! Returns the array index of the
                                 ! element closest to a given test value
      REAL GEN_ELEMF             ! Returns value of Nth element of a
                                 ! given array
C
C     Local variables
C
      CHARACTER CITEMS(2)*32     ! Axis character items retrieved
      INTEGER   DIMS(2)          ! Dimensions of an array
      REAL      ENDS(4)          ! The ends of the slice
      INTEGER   IND              ! Array index of number closest to
                                 ! VALUE
      INTEGER   IND1             ! Array index one less than IND (min
                                 ! value=1)
      INTEGER   IND2             ! Array index one greater than IND
      INTEGER   IORD             ! Order to be used for interpolation
      INTEGER   IPTR             ! Pointer to IMAGE data
      INTEGER   NAXIS            ! Axis number
      INTEGER   NCITEMS          ! Number of axis character items
                                 ! retrieved
      INTEGER   NDIM             ! Number of dimensions of an array
      INTEGER   NELM             ! Total no. of elements in data array
      DOUBLE PRECISION NITEMS(1) ! Axis numeric items retrieved
      INTEGER   NNITEMS          ! No. of axis numeric items retrieved
      INTEGER   NX               ! IMAGE's 1st data dimension
      INTEGER   NY               ! IMAGE's 2nd data dimension
      INTEGER   SLOT             ! Map slot number
      INTEGER   SPTR             ! Pointer to SPECT data
      INTEGER   STATUS           ! Running status for DSA_routines
      CHARACTER UNITS*64         ! Units found in IMAGE
      REAL      VAL1             ! (IND1)th element in axis array
      REAL      VAL2             ! (IND2)th element in axis array
      REAL      VALUE            ! Axis-array element
      REAL      VMAX             ! Last element in axis array
      REAL      VMIN             ! irst element in axis array
      REAL      XEND             ! AXIS(1) value for the end of slice
      LOGICAL   XEXIST           ! True if image has AXIS(1) array
      INTEGER   XPTR             ! Pointer to AXIS(1) data in IMAGE
      REAL      XSTART           ! AXIS(1) value for the start of slice
      REAL      YEND             ! AXIS(2) value for the end of slice
      LOGICAL   YEXIST           ! True if image has AXIS(2) array
      INTEGER   YPTR             ! Pointer to AXIS(2) data in IMAGE
      REAL      YSTART           ! AXIS(2) value for the start of slice
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
      NCITEMS=2
      NNITEMS=1
C
C     Open DSA
C
      CALL DSA_OPEN(STATUS)
C
C     Get name of IMAGE and open file
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
C
C     Get size of image
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
      NX=DIMS(1)
      NY=DIMS(2)
C
C     Get limits for slice.  Note that this is very messy, because
C     the limits can be in terms of non-integer pixel numbers.
C
C     Map data
      NAXIS=2
      CALL DSA_SEEK_AXIS('IMAGE',NAXIS,YEXIST,STATUS)
      IF(YEXIST)THEN
         CALL DSA_MAP_AXIS_DATA('IMAGE',NAXIS,'READ','FLOAT',YPTR,
     :                          SLOT,STATUS)
         CALL DSA_GET_AXIS_INFO('IMAGE',NAXIS,NCITEMS,CITEMS,
     :                           NNITEMS,NITEMS,STATUS)
         UNITS=CITEMS(1)
      END IF
      IF(STATUS.NE.0)GOTO 500

      IF (YEXIST) THEN
         VMAX=GEN_ELEMF(%VAL(CNF_PVAL(YPTR)),NY)
         VMIN=GEN_ELEMF(%VAL(CNF_PVAL(YPTR)),1)
      ELSE
         VMAX=NY
         VMIN=1.
      END IF

      CALL PAR_RDVAL('YSTART',VMIN,VMAX,VMIN,UNITS,VALUE)
      IF (PAR_ABORT()) GOTO 500

      IF (YEXIST) THEN
         IND=GEN_BSEARCH(%VAL(CNF_PVAL(YPTR)),NY,VALUE)
         IND1=MAX(IND-1,1)
         IND2=MIN(IND+1,NY)
         VAL1=GEN_ELEMF(%VAL(CNF_PVAL(YPTR)),IND1)
         VAL2=GEN_ELEMF(%VAL(CNF_PVAL(YPTR)),IND2)
         YSTART=(VALUE-VAL1)*FLOAT(IND2-IND1)/(VAL2-VAL1)+FLOAT(IND1)
      ELSE
         YSTART=VALUE
      END IF

      CALL PAR_RDVAL('YEND',VMIN,VMAX,VMAX,UNITS,VALUE)
      IF (PAR_ABORT()) GOTO 500

      IF (YEXIST) THEN
         IND=GEN_BSEARCH(%VAL(CNF_PVAL(YPTR)),NY,VALUE)
         IND1=MAX(IND-1,1)
         IND2=MIN(IND+1,NY)
         VAL1=GEN_ELEMF(%VAL(CNF_PVAL(YPTR)),IND1)
         VAL2=GEN_ELEMF(%VAL(CNF_PVAL(YPTR)),IND2)
         YEND=(VALUE-VAL1)*FLOAT(IND2-IND1)/(VAL2-VAL1)+FLOAT(IND1)
      ELSE
         YEND=VALUE
      END IF
C
C     ... and now for AXIS(1)
C
      NAXIS=1
      CALL DSA_SEEK_AXIS('IMAGE',NAXIS,XEXIST,STATUS)
      IF(XEXIST)THEN
         CALL DSA_MAP_AXIS_DATA('IMAGE',NAXIS,'READ','FLOAT',XPTR,
     :                          SLOT,STATUS)
         CALL DSA_GET_AXIS_INFO('IMAGE',NAXIS,NCITEMS,CITEMS,
     :                          NNITEMS,NITEMS,STATUS)
         UNITS=CITEMS(1)
      END IF
      IF(STATUS.NE.0)GOTO 500

      IF (XEXIST) THEN
         VMAX=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),NX)
         VMIN=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),1)
      ELSE
         VMAX=NX
         VMIN=1.
      END IF

      CALL PAR_RDVAL('XSTART',VMIN,VMAX,VMIN,UNITS,VALUE)
      IF (PAR_ABORT()) GOTO 500

      IF (XEXIST) THEN
         IND=GEN_BSEARCH(%VAL(CNF_PVAL(XPTR)),NX,VALUE)
         IND1=MAX(IND-1,1)
         IND2=MIN(IND+1,NX)
         VAL1=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),IND1)
         VAL2=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),IND2)
         XSTART=(VALUE-VAL1)*FLOAT(IND2-IND1)/(VAL2-VAL1)+FLOAT(IND1)
      ELSE
         XSTART=VALUE
      END IF

      CALL PAR_RDVAL('XEND',VMIN,VMAX,VMAX,UNITS,VALUE)
      IF (PAR_ABORT()) GOTO 500

      IF (XEXIST) THEN
         IND=GEN_BSEARCH(%VAL(CNF_PVAL(XPTR)),NX,VALUE)
         IND1=MAX(IND-1,1)
         IND2=MIN(IND+1,NX)
         VAL1=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),IND1)
         VAL2=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),IND2)
         XEND=(VALUE-VAL1)*FLOAT(IND2-IND1)/(VAL2-VAL1)+FLOAT(IND1)
      ELSE
         XEND=VALUE
      END IF

C     Get number of pixels for resulting spectrum
C
      CALL PAR_RDVAL('ELEMENTS',1.,10000.,FLOAT(MAX(NX,NY)),'Pixels',
     :                                                        VALUE)
      IF (PAR_ABORT()) GOTO 500
      NELM=VALUE
C
C     Get name of resulting spectrum and open file.
C
      CALL DSA_OUTPUT('SPECT','SPECTRUM','IMAGE',NO_DATA,0,STATUS)
C
C     Map the input image
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
C
C     Create and map the output spectrum data
C
      CALL DSA_RESHAPE_DATA('SPECT','IMAGE',1,NELM,STATUS)
      CALL DSA_MAP_DATA('SPECT','WRITE','FLOAT',SPTR,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Now do the real work
C
      ENDS(1)=XSTART
      ENDS(2)=YSTART
      ENDS(3)=XEND
      ENDS(4)=YEND
      IORD=3
      CALL FIG_CSLICE(%VAL(CNF_PVAL(IPTR)),NX,NY,NELM,ENDS,IORD,
     :                %VAL(CNF_PVAL(SPTR)))
  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END
