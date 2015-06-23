C+
      SUBROUTINE FIGS424
C
C     F I G S 4 2 4
C
C     Given a FIGS image-mode data hypercubecube as produced by the FIGS
C     data acquisition system, processes it to produce a hypercube in
C     which the data has been sorted into wavelength order in accordance
C     with the wavelength parameters included in the hypercube.
C
C     Command parameters -
C
C     'HCUBE'    The name of the hypercube to be processed.
C                This should be a raw FIGS data hypercube.
C     'OUTPUT'   The name of the resulting hypercube.  If this is the
C                same as HCUBE the data is processed in situ, if not
C                a new output file is produced.
C
C     Command keywords - None
C
C     Input data -
C
C     HCUBE is assumed to have a structure with the actual
C     cube data in HCUBE.Z.DATA
C
C     This routine assumes that the first axis of the cube data
C     represents wavelength, that the second and third represent the
C     X and Y dimensions of the image (this is an unfortunate,
C     since it means that the .X axis of the hypercube represents
C     wavelength, the .Y represents the image X axis and so forth)
C     and the fourth axis represents scan cycle number.
C     The data is sorted into wavelength order using the various
C     grating parameters read from the .FITS sub-structure of HCUBE.
C     The data is only re-ordered in the first dimension of the
C     hypercube.
C
C     Output data -
C
C     OUTPUT is created with the same structure as HCUBE, but with
C     a .X structure added to contain the wavelength information.
C
C                                     KS / AAO 25th Nov 1985
C
C     Modified:
C
C     19th May 1986  KS / AAO.  Will now accept an image mode cube
C                    e.g. as produced by FIGS423.
C     23rd Jan 1989  KS / AAO.  WIDTH parameter belatedly added to
C                    call to FIG_FIGWAVES.
C      4th Aug 1993  HME / UoE, Starlink.  Convert to DSA.
C     13th Mar 1996  HME / UoE, Starlink.  Adapt to the FDA library.
C                    No error messages from DTA. Call PAR_WRUSER instead
C                    of FIG_DTAERR.
C     24th Jul 1996  MJCL / Starlink, UCL.  Abuse of SORTED as INTEGER
C                    removed.
C     2005 June 1    MJC / Starlink Use CNF_PVAL for pointers to mapped
C                    data.
*     2015 June 19   MJC /EAO.  Permit units of um for microns.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER DSA_TYPESIZE
C
C     Local variables
C
      LOGICAL SORTED
      INTEGER CUPTR, DIMS(10), LENGTH, MODE, NDIM, NT, NU
      INTEGER NX, NY, STATUS, VECTOR(416), NELM2, SLOT, XPTR
      REAL DETSP, GRATORD, GRATS, GRATSP, GRT0ORD
      REAL GRTA2S, GRTPCH, STEP, WAVES(416), WORK(416), WIDTH
      CHARACTER*80 CDUMMY
      CHARACTER*64 CITEMS(2)
C
C     Image mode parameter values
C
      INTEGER IMAGE1
      PARAMETER (IMAGE1=3)
C
C     Open DSA
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
C
C     Get HCUBE file
C
      CALL DSA_INPUT('HCUBE','HCUBE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get mode of cube data
C
      CALL DSA_GET_FITS_I('HCUBE','FIG_MODE',1,MODE,CDUMMY,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER(
     :          'Unable to determine cube data mode (.FITS.FIG_MODE)',
     :          STATUS)
         CALL PAR_WRUSER('This is probably not FIGS data.',STATUS)
         GO TO 500
      END IF
      IF (MODE.NE.IMAGE1) THEN
         CALL PAR_WRUSER(
     :       'The data mode (.FITS.FIG_MODE) does not represent'
     :       //' imaging mode data.',STATUS)
         GO TO 500
      END IF
C
C     Get size of data in HCUBE
C
      CALL DSA_DATA_SIZE('HCUBE',10,NDIM,DIMS,NELM2,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF (NDIM.LT.3) THEN
         CALL PAR_WRUSER('Input data is not a hypercube.',STATUS)
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
     :                                                        STATUS)
         CALL PAR_WRUSER(
     :    'data from the FIGS data acquisition system.',STATUS)
         GO TO 500
      END IF
C
C     Make sure this data isn't already sorted.
C
      CALL DSA_GET_FITS_I('HCUBE','FIG_SORT',1,SORTED,CDUMMY,STATUS)
      IF ((STATUS.EQ.0).AND.(SORTED)) THEN
         CALL PAR_WRUSER('This data has already been sorted.',STATUS)
         CALL PAR_WRUSER(
     :       'Applying FIGS424 to it does not make sense.',STATUS)
         GO TO 500
      END IF
C
C     Get the wavelength information out of the image structure.
C     It has to be there, so error if it isn't.
C
      CALL DSA_GET_FITS_F('HCUBE','FIG_STEP',1,STEP,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
      CALL DSA_GET_FITS_F('HCUBE','FIG_GRTS',1,GRATS,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
      CALL DSA_GET_FITS_F('HCUBE','FIG_GRSP',1,GRATSP,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
      CALL DSA_GET_FITS_F('HCUBE','FIG_GORD',1,GRATORD,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
      CALL DSA_GET_FITS_F('HCUBE','FIG_GA2S',1,GRTA2S,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
      CALL DSA_GET_FITS_F('HCUBE','FIG_GPCH',1,GRTPCH,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
      CALL DSA_GET_FITS_F('HCUBE','FIG_G0OR',1,GRT0ORD,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
      CALL DSA_GET_FITS_F('HCUBE','FIG_DSTP',1,DETSP,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
C
C     Got here, all must be present.  Carry on.
C
      GO TO 350
C
C     If we get here, one or more must be missing.
C
  340 CONTINUE
      CALL PAR_WRUSER(
     :     'One or more of the quantities FIG_G0OR,FIG_DTSP,',STATUS)
      CALL PAR_WRUSER(
     :     'FIG_STEP,FIG_GRTS,FIG_GRSP,FIG_GORD,FIG_GA2S,FIG_GPCH,',
     :                                                        STATUS)
      CALL PAR_WRUSER(
     :     'are missing from the .FITS structure of the input file.',
     :                                                        STATUS)
      GO TO 500
C
  350 CONTINUE
C
C     Get name of OUTPUT file to be created.  If this is not the same as
C     HCUBE, create an output file by copying HCUBE.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','HCUBE',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Calculate the wavelength values for the data, and generate
C     the data sort vector based on those values.
C
      CALL FIG_FIGWAVES(GRATS,STEP,GRTPCH,GRATORD,GRT0ORD,GRTA2S,
     :                       GRATSP,DETSP,LENGTH,WIDTH,WAVES,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF (LENGTH.NE.NX) THEN
         CALL PAR_WRUSER(
     :       'Spectrum length deduced from grating parameters',STATUS)
         CALL PAR_WRUSER('differs from 1st dimension of the hypercube',
     :                                                         STATUS)
         GO TO 500
      END IF
      CALL GEN_QFISORT(WAVES,LENGTH,VECTOR)
C
C     Map the output data
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','INT',CUPTR,SLOT,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to map hypercube data',STATUS)
         GO TO 500
      END IF
C
C     Sort the data
C
      CALL FIG_FIG424(%VAL(CNF_PVAL(CUPTR)),NX,NY,NT,NU,VECTOR,WORK)
C
C     Force a wavelength array - this code rather assumes that there
C     isn't one at present in the input structure.
C
      CALL GEN_FVSORT(VECTOR,NX,1,WORK,WAVES)
      CALL DSA_COERCE_AXIS_DATA('OUTPUT',1,'FLOAT',1,NX,STATUS)
      CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'UPDATE','FLOAT',
     :   XPTR,SLOT,STATUS)
      CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),WAVES,
     :              %VAL(CNF_PVAL(XPTR)))
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Error writing wavelength array',STATUS)
         GO TO 500
      END IF
      CITEMS(1)='um'
      CITEMS(2)='Wavelength'
      CALL DSA_SET_AXIS_INFO('OUTPUT',1,2,CITEMS,0,0D0,STATUS)
C
C     Set the sorted flag in the output data
C
      CALL DSA_PUT_FITS_I('OUTPUT','FIG_SORT',1,
     :   'Data has been sorted into wavelength order',STATUS)
C
C     Tidy up
C
  500 CONTINUE
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_FIG424(HCUBE,NX,NY,NT,NU,VECTOR,WORK)
C
C     F I G _ F I G 4 2 4
C
C     Re-orders the data in a raw FIGS image mode hypercube so that
C     its first dimesnion is in ascending order of wavelength.
C
C     Parameters -  (">" input, "!" modified, "W" workspace)
C
C     (!) HCUBE     (Integer array HCUBE(NX,NY,NT,NU)) The data
C                   hypercube.
C     (>) NX        (Integer) First dimension of HCUBE.
C     (>) NY        (Integer) Second    "     "    "
C     (>) NT        (Integer) Third     "     "    "
C     (>) NU        (Integer) Fourth    "     "    "
C     (>) VECTOR    (Integer array VECTOR(NX)) The sort vector to be
C                   used to re-order the wavelength dimension of HCUBE.
C     (W) WORK      (Integer array WORK(NX)) Workspace.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     GEN_IVSORT    Indirect integer array sort via a sort vector.
C
C                                            KS / AAO 26th Nov 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY, NT, NU, HCUBE(NX,NY,NT,NU), VECTOR(NX), WORK(NX)
C
C     Local variables
C
      INTEGER IT, IU, IY
C
C     Loop through all the wavelength vectors in the cube and
C     re-order them.
C
      DO IU=1,NU
         DO IT=1,NT
            DO IY=1,NY
               CALL GEN_IVSORT(VECTOR,NX,1,WORK,HCUBE(1,IY,IT,IU))
            END DO
         END DO
      END DO
C
      END
