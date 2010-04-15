C+
      SUBROUTINE SCNSKY
C
C     S C N S K Y
C
C     Uses the 'minimum median' algorithm to create a sky spectrum
C     from an image without any lines clear of stars.  Each column
C     of the image is split into sections of a specified length and
C     the median of each section is calculated.  The sky element
C     corresponding to the column is set to the value of the minimum
C     median for that column (on the assumption that this is an
C     uncontaminated value).  This algorithm is suitable for exposures
C     taken using a CCD in scanned mode, hence the name of the routine.
C
C     Command parameters -
C
C     'IMAGE'    The name of the image
C
C     'SECTION'  The number of pixels in each section.
C
C     'MINIMUM'  A minimum 'valid' pixel value.  A median value
C                will be ignored if the range of pixels over which it
C                was calculated contained pixels below this value.
C
C     'SPECTRUM' The name of the resulting sky spectrum.
C
C     Command keywords -
C
C     'LIMIT'    Use the minimum pixel value to limit the median
C                calculations - this is needed because there is no
C                obviously illegal value for 'MINIMUM' which the
C                program can take as an indication that it is to be
C                ignored.  (If 'MINIMUM' is given in the command
C                string explicitly, 'LIMIT' need not be specified.)
C
C     Input data -
C
C     IMAGE is assumed to have a 2-D data structure.
C
C     Output data -
C
C     SPECTRUM is created with the same structure as IMAGE,
C     except that the data will only have one dimension, and if
C     IMAGE has an AXIS(2) structure, this will be omitted.  Any
C     AXIS(1) structure will be copied unchanged.
C
C                                     KS / CIT 25th May 1983
C     Modified:
C
C     31st Sep  1988 JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     7th Feb. 1991  JMS / AAO. Now copies the spectrum X-axis only if
C                    the number of its dimensions is equal to one.
C     23rd Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Call
C                    PAR_WRUSER rather than DSA_WRUSER.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
      LOGICAL PAR_GIVEN
C
C     Local variables
C
      INTEGER   DIMS(2)          ! Image dimensions
      INTEGER   IPTR             ! Dynamic-memory pointer for image data
      INTEGER   LSECT            ! No. of elements per section for
                                 ! columns
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image - ignored
      INTEGER   NX               ! First dimension of image
      INTEGER   NY               ! Second dimension of image
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   SPTR             ! Dynamic-memory pointer for spectrum
                                 ! data
      INTEGER   STATUS           ! Running status for DSA routines
      LOGICAL   USEMIN           ! True if the VMIN value is to be used
      REAL      VALUE            ! Initial & final value for LSECT
      REAL      VMIN             ! Minimum pixel value to be used
      INTEGER   WPTR             ! Dynamic memory element for workspace
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NO_DATA
      PARAMETER (NO_DATA=1)
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
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
C
C     Get size of data in IMAGE
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('Input data is not an image',STATUS)
         GO TO 500
      END IF
      NY=DIMS(2)
      NX=DIMS(1)
C
C     Get section length
C
      VALUE=MIN(20.,FLOAT(NY))
      CALL PAR_RDVAL('SECTION',1.,FLOAT(NY),VALUE,' ',VALUE)
      LSECT=VALUE
C
C     Get minimum valid pixel value
C
      IF (PAR_GIVEN('MINIMUM')) THEN
         USEMIN=.TRUE.
      ELSE
         CALL PAR_RDKEY('LIMIT',.FALSE.,USEMIN)
      END IF
      IF (USEMIN) THEN
         CALL PAR_RDVAL('MINIMUM',-1.0E30,1.0E30,0.,' ',VMIN)
      ELSE
         VMIN=0.
      END IF
C
C     Create SPECTRUM file modelled on IMAGE but without data and
C     axis structures.
C
      CALL DSA_OUTPUT('SPECT','SPECTRUM','IMAGE',NO_DATA,0,STATUS)
C
C     Create data structure of the appropriate size and copy
C     the AXIS(1) structure from IMAGE if the number of it's
C     dimensions equals one.
C
      CALL DSA_AXIS_SIZE('IMAGE',1,2,NDIM,DIMS,NELM,STATUS)
      CALL DSA_RESHAPE_DATA('SPECT','IMAGE',1,NX,STATUS)
      IF (NDIM.EQ.1) THEN
         CALL DSA_RESHAPE_AXIS('SPECT',1,'IMAGE',1,1,NX,STATUS)
      END IF
C
C     Map the input and output data
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)

      CALL DSA_MAP_DATA('SPECT','UPDATE','FLOAT',SPTR,SLOT,STATUS)
C
C     Get workspace
C
      CALL DSA_GET_WORK_ARRAY(LSECT,'FLOAT',WPTR,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Generate the sky spectrum
C
      CALL FIG_MEDMIN(%VAL(CNF_PVAL(IPTR)),NX,NY,LSECT,VMIN,USEMIN,
     :                %VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(SPTR)))

  500 CONTINUE

C     Close down everything

      CALL DSA_CLOSE(STATUS)

      END
C+
      SUBROUTINE FIG_MEDMIN (IMAGE,NX,NY,LEN,VMIN,USEMIN,WORK,SPECT)
C
C     F I G _ M E D M I N
C
C     Calculates a 'sky' spectrum for an image using the
C     'minimum-median' algorithm -
C
C     Each column (ie cross-section in the 'vertical' or Y
C     direction ie IMAGE(IX,1..NY)) is divided into a number of
C     sections.  The median value of the data is calculated
C     for each section, and the minimum median value is taken
C     as the sky value for that column and used for the sky
C     spectrum element corresponding to that column.
C
C     Parameters -  (">" input, "<" output, "W" workspace)
C
C     (>) IMAGE    (Real array IMAGE(NX,NY)) The input image.
C     (>) NX       (Integer) The first dimension of the image
C     (>) NY       (Integer) The second dimension of the image.
C     (>) LEN      (Integer) The number of elements in each of the
C                  sections the columns are to be divided into.
C     (>) VMIN     (Real) A minimum pixel value; if a range of pixels
C                  contains values less than VMIN, this range will be
C                  ignored in the calculation of the minimum median
C                  for that column.
C     (>) USEMIN   (Logical) True if the VMIN value is to be made
C                  use of.  If false VMIN is ignored.
C     (W) WORK     (Real array WORK(LEN)) Work array.
C     (<) SPECT    (Real array SPECT(NX)) The resulting sky spectrum.
C
C     Subroutines / functions used -
C
C     GEN_QFMED    (GEN_ package) Finds median of a real array
C
C                                           KS / CIT 31st Jan 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL USEMIN
      INTEGER LEN,NX,NY
      REAL    IMAGE(NX,NY),SPECT(NX),VMIN,WORK(LEN)
C
C     Functions used
C
      REAL GEN_QFMED
C
C     Number very close to the largest possible real value
C
      REAL RMAX
      PARAMETER (RMAX=1.7E38)
C
C     Local variables
C
      LOGICAL REJECT
      INTEGER IPT,ISPEND,ISPST,IX,IY,NELM
      REAL    MEDIAN,VALUE
C
C     Pre-load the spectrum with impossibly high numbers
C
      DO IX=1,NX
         SPECT(IX)=RMAX
      END DO
C
C     Loop through sections
C
      ISPST=1
      DO WHILE (ISPST.LE.NY)
C
C        Work out limits for section - note, all sections are NELM
C        long, even at the ends.
C
         ISPEND=ISPST+LEN-1
         IF (ISPEND.GT.NY) THEN
            ISPEND=NY
            ISPST=MAX(1,ISPEND-LEN+1)
         END IF
         NELM=ISPEND-ISPST+1
C
C        Loop through columns for this section
C
         DO IX=1,NX
C
C           Get the data values for the section.
C
            IPT=0
            REJECT=.FALSE.
            IF (USEMIN) THEN
C
C              Checking for unacceptably low pixels.
C
               DO IY=ISPST,ISPEND
                  VALUE=IMAGE(IX,IY)
                  IF (VALUE.GT.VMIN) THEN
                     IPT=IPT+1
                     WORK(IPT)=VALUE
                  ELSE
                     REJECT=.TRUE.
                     GO TO 330
                  END IF
               END DO
  330          CONTINUE
            ELSE
C
C              Without checking for unacceptably low pixels,
C
               DO IY=ISPST,ISPEND
                  IPT=IPT+1
                  WORK(IPT)=IMAGE(IX,IY)
               END DO
            END IF
C
C           Now calculate the median, if the range is OK.
C
            IF (.NOT.REJECT) THEN
               MEDIAN=GEN_QFMED(WORK,NELM)
C
C              If smaller than current result, use this
C
               SPECT(IX)=MIN(SPECT(IX),MEDIAN)
            END IF
         END DO
         ISPST=ISPST+LEN
      END DO
C
C     Finally, zero out any elements that are still set to the
C     high flag - not that there should be any.
C
      DO IX=1,NX
         IF (SPECT(IX).EQ.RMAX) SPECT(IX)=0.
      END DO
C
      END
