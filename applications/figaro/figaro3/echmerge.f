      SUBROUTINE ECHMERGE
*+
*                               E C H M E R G E
*
*  Program name:
*     ECHMERGE
*
*  Function:
*     Merge scrunched echelle orders into a single long spectrum.
*
*  Description:
*     The program expects two input files, each of which may be 1D or
*     2D, but both of which must have the same number of pixels in X,
*     must have a recognised wavelength unit as the units of X and must
*     have (near enough) identical .X.DATA arrays. In practice this
*     means that both must have been scrunched on to the same wavelength
*     scale. (The details of this may change when SCRUNCH has been
*     upgraded to write an output file with a 2D .X.DATA array
*     describing the discontinuous scrunched orders.)
*
*     It creates a 1D output file which consists of all the orders from
*     the input files. Where orders overlap a weighted sum of the
*     overlapping orders is used. The formula used is:
*
*                    in1weight(i) * in1(i) + in2weight(i) * in2(i)
*        output(i) = ---------------------------------------------
*                             in1weight(i) + in2weight(i)
*
*     and the weights are simply the result of median smoothing the data
*     that they weight. This means that more weight is given to stronger
*     signal data, that data where one of the inputs is zero is set to
*     the other of the inputs and that data where both of the inputs are
*     equal is left unaltered. All of these are desirable qualities.
*     There may be less desirable statistical consequences and it is not
*     obvious that signal to noise ratio cannot be degraded although
*     intuitively it will not be since on the assumption of Poisson
*     statistics the weights are essentially just the inverse variances.
*     At low signal, a cutoff applies since the major noise contribution
*     will no longer be Poisson.
*
*     The output file can be the same as either of the two input files
*     and the second input file can be given a blank name, in which case
*     it is not required. Often the first run will use a single input
*     file to create the output file and subsequent runs will add in
*     more input files to the existing output file.
*
*  Parameters:
*
*     (>) IMAGE    (File) The name of the first input image. This can be
*                  1D or 2D and will normally be the output from
*                  SCRUNCH.  However it can also be the results of a
*                  previous run of this program.
*     (>) IMAGE1   (File) The name of the second input image. This can
*                  be 1D or 2D and will normally be the output from
*                  SCRUNCH. However it can also be the results of a
*                  previous run of this program. It must have the same X
*                  size as IMAGE, must agree in X units (which must be
*                  a recognised wavelength unit) and must more or less
*                  agree in the contents of .X.DATA. If no second
*                  input image is required, its name can be specified
*                  as blank.
*     (>) BOX      (Integer) The size of the box (in pixels) to be used
*                  in calculating the medians.  Should be odd; if even,
*                  BOX-1 will be used.
*     (>) CUTOFF   (Real) The ratio of higher signal to lower signal at
*                  which no contribution from the lower signal will be
*                  taken.
*     (<) OUTPUT   (File) The name of the output image. This will be
*                  a 1D image with the same size and X information as a
*                  row of either of the input images.
*
*  Language:
*     FORTRAN
*
*  External variables used:
*
*     None
*
*  Prior requirements:
*     None
*
*  Support: William Lupton, AAO
*
*  Version date: 08 Jun 1988
*-
*  History:
*     08 Jun 1988  WFL / AAO.  Original version.
*     03 Aug 1993  HME / UoE, Starlink.  Convert to DSA, use PAR_ABORT.
*     15 Feb 1996  HME / UoE, Starlink. Convert to FDA:
*                  No concurrent mapping. This was in fact a bug:
*                  If the user gave two input images, the first one
*                  would be mapped twice and the second not at all.
*     2005 June 1  MJC / Starlink.  Use CNF_PVAL for pointers to mapped
*                  data.
*     2015 June 19 MJC /EAO.  Permit units of um for microns.
*+

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*
*     Constant parameter declarations
*
      INTEGER MAXBOX,MAXDIMS
      PARAMETER (MAXBOX=51,MAXDIMS=10)
*
*     Local variable declarations
*
      INTEGER STATUS,INDIM,IDIMS(MAXDIMS),I1NDIM,I1DIMS(MAXDIMS)
      INTEGER IGNORE,IXNDIM,IXDIMS(MAXDIMS),I1XNDIM,I1XDIMS(MAXDIMS)
      INTEGER IPTR,I1PTR,BOX
      INTEGER OPTR,ONDIM,ODIMS(MAXDIMS)
      INTEGER NELM
      INTEGER SLOT
      LOGICAL I1OPEN,OOPEN,IXMAP,I1XMAP,I1MAP
      REAL VALUE,CUTOFF
      DOUBLE PRECISION DITEMS(1)
      CHARACTER IMAGE1*80,IXUNITS*16,I1XUNITS*16
      CHARACTER OIDENT*16
      CHARACTER*64 CITEMS(2)
      LOGICAL      ANGST         ! Units in Angstrom?
      LOGICAL      MICRONS       ! Wavelength in microns?
*
*     Functions
*
      LOGICAL PAR_ABORT
      INTEGER ICH_FOLD
*
*     Open DSA
*
      STATUS=0
      CALL DSA_OPEN(STATUS)
*
*     Initialise all flags
*
      I1OPEN = .FALSE.
      OOPEN = .FALSE.
      IXMAP = .FALSE.
      I1XMAP = .FALSE.
      I1MAP = .FALSE.
*
*     Get first input file.
*
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GOTO 500
*
*     Get second input file and, if it is not blank, open it.
*
      CALL PAR_SDCHAR('IMAGE1',' ',STATUS)
      CALL PAR_RDCHAR('IMAGE1',' ',IMAGE1)
      IF (PAR_ABORT()) GOTO 500
      IF (IMAGE1 .NE. ' ') THEN
         CALL DSA_NAMED_INPUT('IMAGE1',IMAGE1,STATUS)
         IF (STATUS.NE.0) GOTO 500
         I1OPEN=.TRUE.
      END IF
*
*     Check that the first input image is 1D or 2D.
*
      CALL DSA_DATA_SIZE('IMAGE',MAXDIMS,INDIM,IDIMS,NELM,STATUS)
      IF (STATUS.NE.0) GOTO 500
      IF (INDIM.LT.1.OR.INDIM.GT.2) THEN
         CALL PAR_WRUSER('First input image is not 1D or 2D',STATUS)
         GOTO 500
      END IF
      IF (INDIM.EQ.1) THEN
         IDIMS(2) = 1
      END IF
*
*     If we are using two input images, check that their X sizes tally ...
*
      IF (I1OPEN) THEN
         CALL DSA_DATA_SIZE('IMAGE1',MAXDIMS,I1NDIM,I1DIMS,NELM,STATUS)
         IF (STATUS.NE.0) GOTO 500
         IF (I1NDIM.LT.1.OR.I1NDIM.GT.2) THEN
            CALL PAR_WRUSER('Second input image is not 1D or 2D',STATUS)
            GOTO 500
         END IF
         IF (IDIMS(1).NE.I1DIMS(1)) THEN
            CALL PAR_WRUSER('First and second input images have '//
     :                                  'different X sizes',STATUS)
            GOTO 500
         END IF
         IF (I1NDIM.EQ.1) THEN
            I1DIMS(2) = 1
         END IF
*
*        ... that their X units are the same and are known wavelength units ...
*
         CALL DSA_GET_AXIS_INFO('IMAGE',1,2,CITEMS,1,DITEMS,STATUS)
         IXUNITS = CITEMS(1)
         IGNORE = ICH_FOLD(IXUNITS)

         MICRONS = INDEX(IXUNITS,'MICRON').NE.0.OR.
     :             INDEX(IXUNITS,'icron') .NE.0.OR.
     :             INDEX(IXUNITS,'UM')    .NE.0.OR.
     :             INDEX(IXUNITS,'um')    .NE.0
         ANGST = INDEX(IXUNITS,'ANGSTROM').NE.0.OR.
     :           INDEX(IXUNITS,'ngstrom') .NE.0

          IF (STATUS.NE.0.OR..NOT.(MICRONS.OR.ANGST)) THEN
             CALL PAR_WRUSER('First image does not have X units of '//
     :                                 'Angstroms or microns',STATUS)
            GOTO 500
         END IF

         CALL DSA_GET_AXIS_INFO('IMAGE1',1,2,CITEMS,1,DITEMS,STATUS)
         I1XUNITS = CITEMS(1)
         IGNORE = ICH_FOLD(I1XUNITS)

         MICRONS = INDEX(I1XUNITS,'MICRON').NE.0.OR.
     :             INDEX(I1XUNITS,'icron') .NE.0.OR.
     :             INDEX(I1XUNITS,'UM')    .NE.0.OR.
     :             INDEX(I1XUNITS,'um')    .NE.0
         ANGST = INDEX(I1XUNITS,'ANGSTROM').NE.0.OR.
     :           INDEX(I1XUNITS,'ngstrom') .NE.0

          IF (STATUS.NE.0.OR..NOT.(MICRONS.OR.ANGST)) THEN
            CALL PAR_WRUSER('Second image does not have X units of '//
     :                                  'Angstroms or microns',STATUS)
            GOTO 500
         END IF
         IF (IXUNITS.NE.I1XUNITS) THEN
            CALL PAR_WRUSER('First and second images have different '//
     :                                                'X units',STATUS)
            GOTO 500
         END IF
*
*        ... that their X.DATA arrays are 1D, are the same size as each
*        other and are the same size as the .Z.DATA arrays ...
*
         CALL DSA_AXIS_SIZE('IMAGE',1,MAXDIMS,IXNDIM,IXDIMS,NELM,STATUS)
         IF (STATUS.NE.0) GOTO 500
         IF (IXNDIM.NE.1) THEN
            CALL PAR_WRUSER('First input image .X.DATA is not 1D',
     :                                                     STATUS)
            GOTO 500
         END IF
         CALL DSA_AXIS_SIZE('IMAGE1',1,MAXDIMS,
     :      I1XNDIM,I1XDIMS,NELM,STATUS)
         IF (STATUS.NE.0) GOTO 500
         IF (I1XNDIM.NE.1) THEN
            CALL PAR_WRUSER('Second input image .X.DATA is not 1D',
     :                                                      STATUS)
            GOTO 500
         END IF
         IF (IXDIMS(1).NE.I1XDIMS(1)) THEN
            CALL PAR_WRUSER('First and second input images have '//
     :                            'different .X.DATA sizes',STATUS)
            GOTO 500
         END IF
         IF (IXDIMS(1).NE.IDIMS(1)) THEN
            CALL PAR_WRUSER('Input images have different .Z.DATA and '//
     :                                           '.X.DATA sizes',STATUS)
            GOTO 500
         END IF
*
*        ... and that their .X.DATA arrays are the same to within a reasonably
*        small tolerance.
*
         CALL DSA_MATCH_AXIS('IMAGE',1,'IMAGE1',1,STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('First and second input images have '//
     :                           'different .X.DATA arrays',STATUS)
            GOTO 500
         END IF
      END IF
*
*     Now that the input images are successfully checked we can read the
*     parameters controlling the size of the median filter that is passed
*     through the data prior to calculating weights and controlling how small
*     a signal must be to be ignored when combining orders.
*
      CALL PAR_RDVAL('BOX',0.0,FLOAT(MAXBOX),5.0,'Pixels',VALUE)
      BOX = NINT(VALUE)
      CALL PAR_RDVAL('CUTOFF',0.0,100.0,4.0,' ',CUTOFF)
      IF (PAR_ABORT()) GOTO 500
*
*     Now get the output image, force it to be a new file with 1-D data.
*
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,1,STATUS)
      CALL DSA_DELETE_AXIS('OUTPUT',2,STATUS)
      OIDENT = 'OUTPUT'
      ONDIM = 1
      ODIMS(1) = IDIMS(1)
      CALL DSA_RESHAPE_DATA('OUTPUT','OUTPUT',ONDIM,ODIMS,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
*
*     Now we are ready to map the input images.
*
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      IF (I1OPEN)
     :   CALL DSA_MAP_DATA('IMAGE1','READ','FLOAT',I1PTR,SLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
*
*     If we ever get here (which seems unlikely), we are ready to do the
*     actual work of combining the orders from the input images into the
*     output image. (If IMAGE1 is not being used put IDIMS(2) in I1DIMS(2) to
*     prevent an adjustable array error).
*
      IF (.NOT.I1OPEN) THEN
         I1DIMS(2) = IDIMS(2)
      END IF
      CALL FIG_ECHMERGE(IDIMS(1),IDIMS(2),I1DIMS(2),
     :                  %VAL(CNF_PVAL(IPTR)),I1OPEN,
     :                  %VAL(CNF_PVAL(I1PTR)),BOX,CUTOFF,OIDENT,
     :                  %VAL(CNF_PVAL(OPTR)))
*
*     It only remains to tidy up.
*
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
      END

*+
      SUBROUTINE FIG_ECHMERGE(NX,INY,I1NY,IMAGE,I1OPEN,IMAGE1,BOX,
     :                                       CUTOFF,OIDENT,OUTPUT)
*
*                          F I G _ E C H M E R G E
*
*  Routine name:
*     FIG_ECHMERGE
*
*  Function:
*     Merge orders from one or two scrunched echellograms into a 1D spectrum.
*
*  Description:
*     There may be one or two input files and either input file may in fact be
*     the same file as the output file. This makes it easy to combine orders
*     from multiple files into a single long spectrum.
*
*     If a new output file is being created it is first zeroed. Then if the
*     first input image is different from the output the orders from it are
*     added into the output and similarly for the second input image.
*
*  Language:
*     FORTRAN
*
*  Call:
*     CALL FIG_ECHMERGE NX,INY,I1NY,IMAGE,I1OPEN,IMAGE1,BOX,CUTOFF,OIDENT,
*                                                                  OUTPUT)
*
*  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
*
*     (>) NX            (Integer,ref) Size of X dimension of all the images
*     (>) INY           (Integer,ref) Size of Y dimension of IMAGE
*     (>) I1NY          (Integer,ref) Size of Y dimension of IMAGE1 (if IMAGE1
*                       is not being used, must be >0 but is not used)
*     (>) IMAGE(NX,INY) (Real array,ref) First input image
*     (>) I1OPEN        (Logical,ref) Whether IMAGE1 is being used
*     (>) IMAGE1(NX,I1NY) (Real array,ref) Second input image
*     (>) BOX           (Integer,ref) Width of box used for median filtering of
*                       orders as part of weights estimation
*     (>) CUTOFF        (Real,ref) Maximum ratio of stronger signal to weaker
*                       signal that will still allow a contribution from the
*                       weaker signal
*     (>) OIDENT        (Char,descr) Whether the output image is the same as one
*                       of the input images. Possible values are 'IMAGE' (it's
*                       the same as IMAGE), 'IMAGE1' (it's the same as IMAGE1)
*                       and 'OUTPUT' (it's a new empty image)
*     (<) OUTPUT(NX)    (Real array,ref) The output spectrum containing the
*                       merged orders
*
*  External variables used:
*
*     None
*
*  External subroutines / functions used:
*
*     FIG_ECHADDIN
*
*  Prior requirements:
*     None
*
*  Support: William Lupton, AAO
*
*  Version date: 08-Jun-88
*-
*  History:

*
*     Parameter declarations
*
      INTEGER NX,INY,I1NY,BOX
      LOGICAL I1OPEN
      REAL IMAGE(NX,INY),IMAGE1(NX,I1NY),CUTOFF,OUTPUT(NX)
      CHARACTER OIDENT*(*)
*
*     Local variable declarations
*
      INTEGER I
*
*     If the output image is not one of the input images, empty it.
*
      IF (OIDENT.EQ.'OUTPUT') THEN
         DO I = 1,NX
            OUTPUT(I) = 0.0
         END DO
      END IF
*
*     For each of the input images that is not the same as the output image,
*     add in the orders from that input image. Recall that there may not be
*     a second input image.
*
      IF (OIDENT.NE.'IMAGE') THEN
         CALL FIG_ECHADDIN(NX,INY,IMAGE,BOX,CUTOFF,OUTPUT)
      END IF
      IF (I1OPEN.AND.OIDENT.NE.'IMAGE1') THEN
         CALL FIG_ECHADDIN(NX,I1NY,IMAGE1,BOX,CUTOFF,OUTPUT)
      END IF
      END

*+
      SUBROUTINE FIG_ECHADDIN(NX,NY,IMAGE,BOX,CUTOFF,OUTPUT)
*
*                          F I G _ E C H A D D I N
*
*  Routine name:
*     FIG_ECHADDIN
*
*  Function:
*     Adds in orders from a scrunched echellograms to a 1D spectrum.
*
*  Description:
*     Loop through all the orders in the input image. For each, calculate
*     a median filtered version of both the input and the current state
*     of the output. Then use the median filtered values as weights when
*     combining the orders from the input image and the output image. Where
*     the higher signal is less than CUTOFF times the lower signal, do not
*     take any contribution from the lower signal (since to do so would
*     probably degrade the signal to noise ratio).
*
*  Language:
*     FORTRAN
*
*  Call:
*     CALL FIG_ECHADDIN NX,NY,IMAGE,BOX,CUTOFF,OUTPUT)
*
*  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
*
*     (>) NX            (Integer,ref) Size of X dimension of both the images
*     (>) NY            (Integer,ref) Size of Y dimension of IMAGE
*     (>) IMAGE(NX,NY)  (Real array,ref) Input image
*     (>) BOX           (Integer,ref) Width of box used for median filtering of
*                       orders as part of weights estimation
*     (>) CUTOFF        (Real,ref) Maximum ratio of stronger signal to weaker
*                       signal that will still allow a contribution from the
*                       weaker signal
*     (<) OUTPUT(NX)    (Real array,ref) The output spectrum containing the
*                       merged orders
*
*  External variables used:
*
*     None
*
*  External subroutines / functions used:
*
*     GEN_MEDFLT
*
*  Prior requirements:
*     None
*
*  Support: William Lupton, AAO
*
*  Version date: 08-Jun-88
*-
*  History:

*
*     Parameter declarations
*
      INTEGER NX,NY,BOX
      REAL IMAGE(NX,NY),CUTOFF,OUTPUT(NX)
*
*     Constant parameter declarations
*
      INTEGER MAXBOX,MAXNX
      PARAMETER (MAXBOX=51,MAXNX=50000)
*
*     Local variable declarations
*
      INTEGER I,J
      REAL WORK(MAXBOX),IFILT(MAXNX),OFILT(MAXNX),IWEIGHT,OWEIGHT
*
*     Loop through all the orders in the input image. For each, calculate
*     a median filtered version of both the input and the current state
*     of the output. Then use the median filtered values as weights when
*     combining the orders from the input image and the output image. Where
*     the higher signal is less than CUTOFF times the lower signal, do not
*     take any contribution from the lower signal (since to do so would
*     probably degrade the signal to noise ratio).
*
      DO I = 1,NY
         CALL GEN_MEDFLT(IMAGE(1,I),NX,1,BOX,1,WORK,IFILT)
         CALL GEN_MEDFLT(OUTPUT,NX,1,BOX,1,WORK,OFILT)
         DO J = 1,NX
            IWEIGHT = MAX(IFILT(J),0.0)
            OWEIGHT = MAX(OFILT(J),0.0)
            IF (IWEIGHT.GT.CUTOFF*OWEIGHT) THEN
               OUTPUT(J) = IMAGE(J,I)
            ELSE IF (OWEIGHT.GT.CUTOFF*IWEIGHT) THEN
               CONTINUE
            ELSE IF (IWEIGHT+OWEIGHT.LT.1E-6) THEN
               CONTINUE
            ELSE
               OUTPUT(J) = (IWEIGHT*IMAGE(J,I) + OWEIGHT*OUTPUT(J)) /
     :                                (IWEIGHT + OWEIGHT)
            END IF
         END DO
      END DO
      END
