C+
      SUBROUTINE FIGS422
C
C     F I G S 4 2 2
C
C     Given a FIGS image-mode data hypercube, either sorted into
C     wavelength order (eg by FIGS424) or not, sums all the cycles
C     and wavelength planes within a specified wavelength range
C     to produce an image.
C
C     Command parameters -
C
C     'HCUBE'    (Character) The name of the hypercube to be processed.
C     'XSTART'   (Real) The start of the wavelength range to be
C                included.
C     'XEND'     (Real) The end of the wavelength range to be included.
C     'CYSTART'  (Integer) The first cycle to be included.
C     'CYEND'    (Integer) The last cycle to be included.
C     'IMAGE'    (Character) The name of the resulting image.
C
C     Command keywords -
C
C     'SPLIT'    If specified, FIGS422 will create a number of output
C                files, one for each cycle in the specified range,
C                rather than just one with all the cycles in the range
C                summed.  In this case, the output files will be named
C                using the name specified using 'IMAGE', but with the
C                cycle number appended.
C
C     Input data -
C
C     HCUBE is assumed is the actual hypercube data.
C
C     This routine assumes that the first axis of the cube data
C     represents wavelength, that the second and third represent the
C     X and Y dimensions of the image (this is an unfortunate,
C     since it means that the AXIS(1) structure of the hypercube
C     represents wavelength, the AXIS(2) represents the image X axis and
C     so forth) and the fourth axis represents scan cycle number.
C
C     Output data -
C
C     IMAGE is created with the same structure as HCUBE except that the
C     main data array will only have 2 dimensions, and any AXIS
C     sub-structures that HCUBE has will be deleted/renamed as
C     appropriate.
C
C                                     KS / AAO 6th Jan 1985
C
C     Modified:
C
C     19th May 1986  KS / AAO.  Will now accept an image mode cube,
C                    e.g. as produced by FIGS423.
C     27th Jun 1989  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     29th Mar 1991  JMS / AAO. Changed type from 'INTEGER' to 'INT'
C                    when mapping input data. Added STATUS checks to
C                    support user-requested aborts.
C     28th Sep 1992  HME / UoE, Starlink.  INCLUDE changed.
C     13th Mar 1996  HME / UoE, Starlink.  Adapted to the FDA library.
C                    Re-shape data before axes.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_LEN            ! Position of last non-blank char in
                                 ! string
      INTEGER ICH_ENCODE
      INTEGER DSA_TYPESIZE
C
C     Local variables
C
      INTEGER   BYTES            ! Bytes required for an array
      CHARACTER COMENT*32
      INTEGER   FSTAT            ! Status for FIG routines
      INTEGER   HXPTR            ! Dynamic memory element for axis data
      INTEGER   DIMS(10)         ! Image dimensions
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image - ignored
      INTEGER   NX               ! First dimension of image
      INTEGER   NY               ! Second dimension of image
      INTEGER   NU               ! Fourth dimension of image
      INTEGER   NT               ! Third dimension of image
      INTEGER   PSTAT            ! Status for PAR routines
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   SLOTO            ! Slot number for succesive output data
      INTEGER   STATUS           ! Running status for DSA routines

C
C     Local variables
C
      LOGICAL FAULT, REPEAT
      LOGICAL SELECT(416), SPLIT
      INTEGER CUPTR, CY1, CY2, ICYEN, ICYST
      INTEGER INVOKE, IX, LENGTH, MODE
      INTEGER NEXT, OLEN, OPTR, OULEN, PLANES
      INTEGER SFLAG, VECTOR(416)
      REAL DETSP, GRATORD, GRATS, GRATSP, GRT0ORD
      REAL GRTA2S, GRTPCH, STEP, WAVES(416), XEND, XMAX, XMIN, XSTART
      REAL VALUE
      CHARACTER OUTPUT*64, STRING*64
*      CHARACTER CMPNAM*16, CUCOMP*64, ERROR*64, HCUBE*64, NAME*64
*      CHARACTER OUCOMP*64, OUTPUT*64, STRING*64
C
C     Image mode parameter value
C
      INTEGER IMAGE1
      PARAMETER (IMAGE1=3)
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
      FAULT=.FALSE.
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get name of HCUBE file
C
      CALL DSA_INPUT ('HCUBE','HCUBE',STATUS)
C
C     Get mode of hypercube data
C
      CALL DSA_GET_FITS_I('HCUBE','FIG_MODE',0,MODE,COMENT,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER(
     :          'Unable to determine cube data mode (.FITS.FIG_MODE).',
     :            PSTAT)
         CALL PAR_WRUSER('This is probably not FIGS data.',PSTAT)
         GO TO 500
      END IF
      IF (MODE.NE.IMAGE1) THEN
         CALL PAR_WRUSER(
     :       'The data mode (.FITS.FIG_MODE) is not the correct '
     :       //' value for image mode.',PSTAT)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Get size of data in HCUBE
C
      CALL DSA_DATA_SIZE ('HCUBE',10,NDIM,DIMS,NELM,STATUS)
      IF (NDIM.LT.3) THEN
         CALL PAR_WRUSER('Input data is not a hypercube',PSTAT)
         FAULT=.TRUE.
         GO TO 500
      END IF
      IF (NDIM.EQ.3) THEN
         NU=1
      ELSE
         NU=DIMS(4)
      END IF
      NT=DIMS(3)
      NY=DIMS(2)
      NX=DIMS(1)
      IF (NX.GT.416) THEN
         CALL PAR_WRUSER(
     :    '1st dimension of cube is greater than 416.  This cannot be',
     :                                                        PSTAT)
         CALL PAR_WRUSER(
     :    'data from the FIGS data acquisition system.',PSTAT)
         GO TO 500
      END IF
C
C     What we want now is the wavelength array giving the wavelengths
C     corresponding to the element numbers in AXIS(1) (WAVES) and
C     the sort vector for the wavelength array (VECTOR). The latter is
C     so we can be sensible about the precision with which we test a
C     wavelength value to decide whether to include it or not.
C
C     See if this is sorted data, ie data that has been processed
C     by FIGS424.
C

      CALL DSA_GET_FITS_I('HCUBE','FIG_SORT',0,SFLAG,COMENT,STATUS)
      IF ((STATUS.EQ.0).AND.(SFLAG.NE.0)) THEN
C
C        It is sorted data. Get the wavelength array directly from
C        the hypercube structure.  The sorted array will be the same
C        as the wavelength array.
C
         CALL DSA_MAP_AXIS_DATA('HCUBE',1, 'READ','FLOAT',HXPTR,
     :                          SLOT,STATUS)
         BYTES=NX*DSA_TYPESIZE('FLOAT',STATUS)
         CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(HXPTR)),WAVES)
         DO IX=1,NX
            VECTOR(IX)=IX
         END DO
      ELSE
C
C        The data is unsorted.  We shall have to work out the
C        wavelength array directly from the data parameters.
C
         CALL DSA_GET_FITS_I('HCUBE','FIG_STEP',0,STEP,COMENT,STATUS)
         CALL DSA_GET_FITS_I('HCUBE','FIG_GRTS',0,GRATS,COMENT,STATUS)
         CALL DSA_GET_FITS_I('HCUBE','FIG_GRSP',0,GRATSP,COMENT,STATUS)
         CALL DSA_GET_FITS_I('HCUBE','FIG_GORD',0,GRATORD,COMENT,STATUS)
         CALL DSA_GET_FITS_I('HCUBE','FIG_GA2S',0,GRTA2S,COMENT,STATUS)
         CALL DSA_GET_FITS_I('HCUBE','FIG_GPCH',0,GRTPCH,COMENT,STATUS)
         CALL DSA_GET_FITS_I('HCUBE','FIG_G0OR',0,GRT0ORD,COMENT,STATUS)
         CALL DSA_GET_FITS_I('HCUBE','FIG_DTSP',0,DETSP,COMENT,STATUS)
         IF (STATUS.NE.0)  GO TO 340
C
C        Got here, all must be present.  Carry on.
C
         GO TO 350
C
C        If we get here, one or more must be missing.
C
  340    CONTINUE
         CALL PAR_WRUSER(
     :     'One or more of the quantities FIG_G0OR,FIG_DTSP,',PSTAT)
         CALL PAR_WRUSER(
     :     'FIG_STEP,FIG_GRTS,FIG_GRSP,FIG_GORD,FIG_GA2S,FIG_GPCH,',
     :                                                        PSTAT)
         CALL PAR_WRUSER(
     :     'are missing from the .FITS structure of the input file.',
     :                                                        PSTAT)
         FAULT=.TRUE.
         GO TO 500
C
  350    CONTINUE
C
C        Calculate the wavelength values for the data.
C
         CALL FIG_FIGWAVES(GRATS,STEP,GRTPCH,GRATORD,GRT0ORD,GRTA2S,
     :                               GRATSP,DETSP,LENGTH,WAVES,FSTAT)
         IF (FSTAT.NE.0) THEN
            FAULT=.TRUE.
            GO TO 500
         END IF
         IF (LENGTH.NE.NX) THEN
            CALL PAR_WRUSER(
     :       'Spectrum length deduced from grating parameters',PSTAT)
            CALL PAR_WRUSER('differs from 1st dimension of hypercube',
     :                                                         PSTAT)
            FAULT=.TRUE.
            GO TO 500
         END IF
C
C        Now generate the sort vector for the wavelength array
C
         CALL GEN_QFISORT(WAVES,NX,VECTOR)
      END IF
C
C     At this point, we have the wavelength values in the WAVES
C     array, either read directly from sorted data, or calculated,
C     and its sort vector in VECTOR.
C
C     Get the wavelength limits
C
      XMIN=WAVES(VECTOR(1))
      XMAX=WAVES(VECTOR(NX))
      STRING='Wavelength range of data is from '
      INVOKE=ICH_ENCODE(STRING,XMIN,34,3,NEXT)
      STRING(NEXT:)=' to '
      INVOKE=ICH_ENCODE(STRING,XMAX,NEXT+4,3,NEXT)
      STRING(NEXT:)=' microns'
      CALL PAR_WRUSER(' ',PSTAT)
      CALL PAR_WRUSER(STRING(:NEXT+7),PSTAT)
      CALL PAR_WRUSER(' ',PSTAT)
      CALL PAR_RDVAL('XSTART',XMIN,XMAX,XMIN,'microns',XSTART)
      CALL PAR_RDVAL('XEND',XSTART,XMAX,XMAX,'microns',XEND)
C
C     Using the wavelength array and the sort vector, determine which
C     elements of the wavelength array are to be included in the output.
C     This is all to get round the problem of trying to select a single
C     wavelength plane, for example, and not wanting to test for an
C     exact match with the specified (floating point) wavelength.
C
      CALL FIG_SEL422(XSTART,XEND,NX,WAVES,VECTOR,SELECT)
      PLANES=0
      DO IX=1,NX
         IF (SELECT(IX)) PLANES=PLANES+1
      END DO
      STRING='Wavelength range covers '
      INVOKE=ICH_ENCODE(STRING,FLOAT(PLANES),25,0,NEXT)
      STRING(NEXT:)=' wavelength planes'
      IF (PLANES.EQ.1) THEN
         NEXT=NEXT+16
      ELSE
         NEXT=NEXT+17
      END IF
      CALL PAR_WRUSER(' ',PSTAT)
      CALL PAR_WRUSER(STRING(:NEXT),PSTAT)
      CALL PAR_WRUSER(' ',PSTAT)
C
C     Now deal with the cycle range.  Get CYSTART, CYEND and SPLIT
C
      CALL PAR_RDVAL('CYSTART',1.,FLOAT(NU),1.,'cycles',VALUE)
      ICYST=VALUE
      CALL PAR_RDVAL('CYEND',VALUE,FLOAT(NU),FLOAT(NU),'cycles',VALUE)
      ICYEN=VALUE
      CALL PAR_RDKEY('SPLIT',.FALSE.,SPLIT)
C
C     Get name of IMAGE file to be created.
C
      CALL PAR_RDCHAR('IMAGE',' ',OUTPUT)
      OULEN=ICH_LEN(OUTPUT)
C
C     Map the input data
C
      CALL DSA_MAP_DATA('HCUBE','READ','INT',CUPTR,SLOT,STATUS)
C
C     The following loop is repeated as many times as there are
C     output files to be created.  The loop will be stopped by REPEAT
C     being set false, either because SPLIT is not set, or because all
C     the cycles have been output to their individual files.
C
      REPEAT=.TRUE.
      DO WHILE (REPEAT)
C
C        Determine the limits for this pass through the loop.  See if
C        the loop has to be repeated, and if SPLIT is set, append
C        the cycle number to the output file name.
C
         IF (SPLIT) THEN
            CY1=ICYST
            CY2=ICYEN
            ICYST=ICYST+1
            REPEAT=ICYST.LE.ICYEN
            INVOKE=ICH_ENCODE(OUTPUT,FLOAT(CY1),OULEN+1,0,NEXT)
            OLEN=NEXT-1
            STRING='Cycle '
            INVOKE=ICH_ENCODE(STRING,FLOAT(CY1),7,0,NEXT)
            STRING(NEXT:)=' being copied to file '//
     :                                          OUTPUT(:OLEN)//'.DST'
            NEXT=NEXT+26+OLEN
            CALL PAR_WRUSER(STRING(:NEXT),PSTAT)
         ELSE
            REPEAT=.FALSE.
            CY1=ICYST
            CY2=ICYEN
            OLEN=OULEN
         END IF
C
C        Open the output file for this pass through the loop
C
         CALL DSA_NAMED_OUTPUT('OUTPUT',OUTPUT(:OLEN),'HCUBE',NO_DATA,
     :                          NEW_FILE,STATUS)
C
C        Copy all of the input structure to the output, except for
C        the Axis(1) and and .U components (which are always lost) The .Z
C        structure is handled separately.  The .Y and .T components
C        will eventaully become the .X and .Y components of the new
C        image.
C
C        Copy all of the .Z structure except for the actual data,
C        after first creating the .Z output structure.
C
         NDIM=2
         DIMS(1)=NY
         DIMS(2)=NT
         CALL DSA_RESHAPE_DATA('OUTPUT','HCUBE',NDIM,DIMS,STATUS)
         CALL DSA_AXIS_SIZE('HCUBE',2,10,NDIM,DIMS,NELM,STATUS)
         CALL DSA_RESHAPE_AXIS('OUTPUT',1,'HCUBE',2,NDIM,DIMS,STATUS)
         CALL DSA_AXIS_SIZE('HCUBE',3,10,NDIM,DIMS,NELM,STATUS)
         CALL DSA_RESHAPE_AXIS('OUTPUT',2,'HCUBE',3,NDIM,DIMS,STATUS)
         NDIM=2
         DIMS(1)=NY
         DIMS(2)=NT
C
C        Map the output data
C
         CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,SLOTO,STATUS)
         IF(STATUS.NE.0)GOTO 500
C
C        Perform the basic processing of the hypercube down to
C        an image.
C
         CALL FIG_FIGS422(%VAL(CNF_PVAL(CUPTR)),NX,NY,NT,NU,CY1,CY2,
     :                    SELECT,%VAL(CNF_PVAL(OPTR)))
C
C        Close down the output file
C
         CALL DSA_UNMAP(SLOT,STATUS)
         CALL DSA_CLOSE_STRUCTURE('OUTPUT',STATUS)
         IF(STATUS.NE.0)GOTO 500
C
      END DO
C
C     Tidy up
C
  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR

      END

C+
      SUBROUTINE FIG_SEL422(XSTART,XEND,NX,WAVES,VECTOR,SELECT)
C
C     F I G _ S E L 4 2 2
C
C     Determines which wavelength planes are to be included in the
C     image being constructed by FIGS422.  It looks at each wavelength
C     value in turn and compares it with the limits selected by the
C     user, given the wavelength values of the adjacent elements.
C     A wavelength element is taken as having a width from half way
C     to the previous element to half way to the next element, and if
C     there is any overlap between this range and the selected range
C     then the element is included.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) XSTART    (Real) The start of the selected wavelength range.
C     (>) XEND      (Real) The end of the selected wavelength range.
C     (>) NX        (Integer) The number of wavelength elements.
C     (>) WAVES     (Real array WAVES(NX)) The wavelength values of the
C                   various elements.
C     (>) VECTOR    (Integer array VECTOR(NX)) The sort vector for WAVES
C                   ie WAVES(VECTOR(1)) is the lowest wavelength, while
C                   WAVES(VECTOR(NX)) is the highest.
C     (<) SELECT    (Logical array SELECT(NX)) Flags if the corresponding
C                   element of waves falls withinthe selected range.
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                             KS / AAO 25th Nov 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, VECTOR(NX)
      LOGICAL SELECT(NX)
      REAL    XSTART, XEND, WAVES(NX)
C
C     Local variables
C
      INTEGER I
      REAL XLIM1, XLIM2
C
      XLIM1=WAVES(VECTOR(1))*1.5-WAVES(VECTOR(2))*0.5
      DO I=1,NX-1
         XLIM2=(WAVES(VECTOR(I))+WAVES(VECTOR(I+1)))*0.5
         SELECT(VECTOR(I))=(XEND.GT.XLIM1).AND.(XLIM2.GT.XSTART)
         XLIM1=XLIM2
      END DO
      XLIM2=WAVES(VECTOR(NX))*1.5-WAVES(VECTOR(NX-1))*0.5
      SELECT(VECTOR(NX))=(XEND.GT.XLIM1).AND.(XLIM2.GT.XSTART)
C
      END
C+
      SUBROUTINE FIG_FIGS422(HCUBE,NX,NY,NT,NU,CY1,CY2,SELECT,IMAGE)
C
C     F I G _ F I G S 4 2 2
C
C     Sums all the cycles and selected wavelength planes of a
C     FIGS image mode hypercube to produce an image.
C
C     Parameters -  (">" input, "!" modified, "W" workspace)
C
C     (>) HCUBE     (Integer array HCUBE(NX,NY,NT,NU)) The data
C                   hypercube.
C     (>) NX        (Integer) First dimension of HCUBE.
C     (>) NY        (Integer) Second    "     "    "
C     (>) NT        (Integer) Third     "     "    "
C     (>) NU        (Integer) Fourth    "     "    "
C     (>) CY1       (Integer) First cycle (U value) to be included
C     (>) CY2       (Integer) Last cycle (U value) to be included
C     (>) SELECT    (Logical array SELECT(NX)) The selection vector
C                   indicating which wavelength planes are to be
C                   included in the image.
C     (<) IMAGE     (Real array IMAGE(NY,NT)) The resulting image.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     GEN_FILL      Sets an array of bytes to a constant value
C
C                                            KS / AAO 6th Jan 1985
C     Modified:
C
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY, NT, NU, CY1, CY2, HCUBE(NX,NY,NT,NU)
      LOGICAL SELECT(NX)
      REAL    IMAGE(NY,NT)
C
C     Local variables
C
      INTEGER IT, IU, IX, IY
C
C     Set the image to zero
C
      CALL GEN_FILL(NY*NT*4,0,IMAGE)
C
C     Add in the appropriate cube elements
C
      DO IU=CY1,CY2
         DO IT=1,NT
            DO IY=1,NY
               DO IX=1,NX
                  IF (SELECT(IX)) THEN
                     IMAGE(IY,IT)=IMAGE(IY,IT)+HCUBE(IX,IY,IT,IU)
                  END IF
               END DO
            END DO
         END DO
      END DO
C
      END

