C+
      SUBROUTINE FET321
C
C     F E T 3 2 1
C
C     Given a FIGS data cube as produced by the FIGS data acquisition
C     system running in one of the etalon modes, processes it to
C     produce a single spectrum, for one of the detectors only.
C
C     Command parameters -
C
C     'CUBE'     The name of the cube from which the planes
C                are to be taken.  This should be a raw FIGS data
C                cube.
C     'DETECTOR' The number of the detector to be used.
C     'SPECTRUM' The name of the resulting spectrum.
C     'CUTOFF'   The level (in sigma) at which a point will
C                be ignored (FIGS321 only)
C
C     Command keywords -
C
C     'ADD'      Add the data together rather than subtracting the
C                beamswitch and chop backgrounds
C
C     'BACK'     Return the background spectrum only
C
C     'NORM'     Normalize data to mean level of each cycle.
C
C     Input data -
C
C     CUBE is assumed to have a structure with the actual
C     cube data in CUBE.Z.DATA
C
C     This routine assumes that the first axis of the cube data
C     represents wavelength, that the second represents spectral
C     scans in the order A1a,A1b,B1a,B1b,B2a,B2b,A2a,A2b, where
C     A1,A2,B1,B2 represent the parts of the beamswitch ABBA cycle
C     and a and b represent the signal and background chop positions
C     respectively.  In etalon mode 2, there are no chop positions,
C     and the second axis is just A1,B1,A2,B2. This means that the
C     second dimension of the cube has to be either 4 or 8.  The
C     cube third axis represents beamswitch cycle number.
C     The data is normalized to give a figure in detected photons
C     per second.  Along the wavelength axis, the data is assumed
C     to be in order of etalon position, each etalon position having
C     n values where n is the number of detectors used.
C
C     Output data -
C
C     IMAGE is created with the same structure as CUBE
C     except that .Z.DATA will only have 1 dimension, and any
C     .Y or .T sub-structures that CUBE has will be deleted.
C     If a spectrum is produced the errors (derived from
C     the cycle to cycle statistics) are placed in the .Z.ERRORS
C     component
C
C     History:
C
C      5th May 1988  KS / AAO.   Original version, based on FIGS321.
C                    Only properly supports etalon mode 2.
C     28th Sep 1992  HME / UoE, Starlink.  TABs removed. Don't treat
C                    status from LIB$... as logical.
C     21st Jul 1993  HME / UoE, Starlink.  Don't call GEN_ERRMSG any
C                    more if LIB$GET/FREE_VM fail.
C     28th Jul 1993  HME / UoE, Starlink.  Use DSA_ to get workspaces.
C      3rd Aug 1993  HME / UoE, Starlink.  Convert to DSA, use
C                    PAR_ABORT.
C     13th Mar 1996  HME / UoE, Starlink.  Adapt to FDA library.
C                    No error messages from DTA. Call PAR_WRUSER instead
C                    of FIG_DTAERR.
C                    Fix bug whereby DSA_OPEN was called a second time
C                    instead of DSA_CLOSE.
C                    Fixed the broken-up format string in a WRITE.
C     2005 June 1    MJC / Starlink Use CNF_PVAL for pointers to mapped
C                    data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER DSA_TYPESIZE
C
C     Etalon mode parameter values
C
      INTEGER ETALON2, DETECTORS, MAX_ETL_STEP
      PARAMETER (ETALON2 = 5, DETECTORS = 16, MAX_ETL_STEP = 100)
C
C     Local variables
C
      LOGICAL ADD, NORM
      LOGICAL BACK
      INTEGER BYTES, CUPTR, DIMS(10), EPTR
      INTEGER MODE, NDIM, NELM, NELM2, NSPECT, NT, NX, NY, OPTR
      INTEGER SLOT, XPTR
      INTEGER STATUS, SWPERC, WPTR, EXPTR, NUMPTR, DETECTOR, IX
      REAL CHOPS, POINTS, INT, GAIN, FACTOR
      REAL SECS, ETLSSTEP, ETLSTP, ETLSWP
      REAL CUTOFF, POSNS(MAX_ETL_STEP), VALUE
      CHARACTER CHAR64*64
      CHARACTER*80 CDUMMY
      CHARACTER*64 CITEMS(2)
C
C     Open DSA, only to get workspaces through it.
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
C
C     Get name of CUBE file
C
      CALL DSA_INPUT('CUBE','CUBE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get mode of cube data
C
      CALL DSA_GET_FITS_I('CUBE','FIG_MODE',1,MODE,CDUMMY,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER(
     :          'Unable to determine cube data mode (.FITS.FIG_MODE)',
     :          STATUS)
         CALL PAR_WRUSER('This is probably not FIGS data.',STATUS)
         GO TO 500
      END IF
      IF (MODE.NE.ETALON2) THEN
         CALL PAR_WRUSER(
     :       'The data mode (.FITS.FIG_MODE) is not a value this '
     :       //'program can handle.',STATUS)
         CALL PAR_WRUSER('The data is not in etalon 2 mode.',STATUS)
         GO TO 500
      END IF
C
C     Get size of data in CUBE
C
      CALL DSA_DATA_SIZE('CUBE',10,NDIM,DIMS,NELM2,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF (NDIM.NE.3) THEN
         CALL PAR_WRUSER('Input data is not a cube',STATUS)
         GO TO 500
      END IF
      NT=DIMS(3)
      NY=DIMS(2)
      NX=DIMS(1)
      IF ((MODE.EQ.ETALON2).AND.(NY.NE.4)) THEN
         CALL PAR_WRUSER(
     :          'Second dimension of cube is not 4.  This cannot be',
     :                                                        STATUS)
         CALL PAR_WRUSER(
     :          'data from FIGS in etalon mode 2.',STATUS)
         GO TO 500
      END IF
      IF (MOD(NX,DETECTORS).NE.0) THEN
         CALL PAR_WRUSER(
     :     'Cube 1st dimension is not a multiple of # detectors',STATUS)
         CALL PAR_WRUSER(
     :                  'This cannot be FIGS etalon 2 mode data',STATUS)
         GO TO 500
      END IF
      IF ((NX/DETECTORS).GT.MAX_ETL_STEP) THEN
         CALL PAR_WRUSER(
     :     'Cube 1st dimension > # detectors * max etalon steps',STATUS)
         CALL PAR_WRUSER(
     :                  'This cannot be FIGS etalon 2 mode data',STATUS)
         GO TO 500
      END IF
C
C     See which detector is to be used
C
      CALL PAR_RDVAL('DETECTOR',1.0,16.0,1.0,'detectors',VALUE)
      IF (PAR_ABORT()) GO TO 500
      DETECTOR = VALUE
C
C     Get cutoff value
C
      CALL PAR_SDVAL('CUTOFF',1.0E38,STATUS)
      CALL PAR_RDVAL('CUTOFF',0.0,1.0E38,1.0E38,'SIGMA',CUTOFF)
      IF (PAR_ABORT()) GO TO 500
C
C     See if we have to add the data
C
      CALL PAR_RDKEY('ADD',.FALSE.,ADD)
C
C     See if background only required
C
      CALL PAR_RDKEY('BACK',.FALSE.,BACK)
C
C     See if normalizing required
C
      CALL PAR_RDKEY('NORM',.FALSE.,NORM)
      IF (PAR_ABORT()) GO TO 500
C
C     Work out how many signal spectra we are going to be adding
C
      IF (ADD) THEN
         NSPECT=NY
      ELSE
         NSPECT=NY/2
      END IF
C
C     Get etalon position information
C
      CALL DSA_GET_FITS_F('CUBE','FIG_ESTS',1,ETLSSTEP,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
      CALL DSA_GET_FITS_F('CUBE','FIG_ESSZ',1,ETLSTP,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
C
C     Get Intensity scaling information
C
      CALL DSA_GET_FITS_F('CUBE','FIG_PTS',1,POINTS,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
      CALL DSA_GET_FITS_F('CUBE','FIG_INT',1,INT,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
      CALL DSA_GET_FITS_F('CUBE','FIG_GAIN',1,GAIN,CDUMMY,STATUS)
      IF (STATUS.NE.0)  GO TO 340
      IF (MODE.EQ.ETALON2) THEN
         CALL DSA_GET_FITS_F('CUBE','FIG_ESWP',1,ETLSWP,CDUMMY,STATUS)
         IF (STATUS.NE.0)  GO TO 340
         SWPERC=1
         CHOPS=1
      ELSE
         CALL DSA_GET_FITS_F('CUBE','FIG_CHOP',1,CHOPS,CDUMMY,STATUS)
         IF (STATUS.NE.0)  GO TO 340
         SWPERC=2
         ETLSWP=1
      END IF
C
C     Got here, all must be present.  Carry on.
C
      GO TO 350
C
C     If we get here, one or more must be missing.
C
  340 CONTINUE
      CALL PAR_WRUSER(
     :     'Some of the quantities FIG_PTS,FIG_INT,FIG_ESTS,FIG_ESSZ',
     :                                                         STATUS)
      IF (MODE.EQ.ETALON2) THEN
         CALL PAR_WRUSER('FIG_GAIN,FIG_ESWP are missing from ',STATUS)
      ELSE
         CALL PAR_WRUSER('FIG_GAIN,FIG_CHOP are missing from ',STATUS)
      END IF
      CALL PAR_WRUSER('the .FITS structure of the input file.',STATUS)
      GO TO 500
C
  350 CONTINUE
C
C     Calculate intensity normalizing factor.  Note the way this works:
C     POINTS is the number of integrations performed at each etalon setting
C     INT    is the number of msec for each such integration
C     GAIN   is the gain factor of the system, and is such that
C            (GAIN/1000)/9.5 is the number of ADUs per detected photon.
C     SWPERC is the number of sweeps per chop
C     CHOPS  is the number of chops per recorded spectrum
C     ETLSWP is the number of etalon sweeps per recorded spectrum
C            (Note that in mode 2, the grating does not chop, but has
C            multiple sweeps instead.  In mode 1, ETLSWP=1, and SWPERC=2,
C            while in mode 2, CHOPS=1 and SWPERC=1)
C     NSPECT is the number of spectra being added up to give the data
C            for each cycle.  This depends on the mode, and also on the
C            setting of ADD.
C
C     This means that SECS=POINTS*(INT/1000)*ETLSWP*CHOPS*SWPERC gives the
C     total integration time in seconds represented by the data in each
C     element of each spectrum.  SECS*NSPECT is therefore the total
C     integration time in seconds represented by each element of the summed
C     spectra, for each cycle.
C
C     FACTOR=SECS*NSPECT*(GAIN/1000)/9.5 is therefore the number of ADUs
C     (counts) in the summed spectra that represent a data rate of 1
C     photon/sec. That is, adding up the spectra for a beamswitch cycle
C     and dividing each element by FACTOR will give a result in photons/sec.
C
      SECS = POINTS * (INT/1000) * ETLSWP * CHOPS * SWPERC
      FACTOR = SECS * NSPECT * (GAIN/1000) / 9.5
C
C     Get name of SPECTRUM file to be created. Force a new file.
C
      CALL DSA_OUTPUT('OUTPUT','SPECTRUM','CUBE',0,1,STATUS)
      CALL DSA_DELETE_AXIS('OUTPUT',3,STATUS)
      CALL DSA_DELETE_AXIS('OUTPUT',2,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Now reshape the .Z.DATA component in the output structure
C
      NDIM=1
      DIMS(1)=NX/DETECTORS
      NELM=DIMS(1)
      CALL DSA_RESHAPE_DATA('OUTPUT','OUTPUT',NDIM,DIMS,STATUS)
      CHAR64='Photons/sec'
      CALL DSA_SET_DATA_INFO('OUTPUT',1,CHAR64,0,0D0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map the input and output data
C
      CALL DSA_MAP_DATA('CUBE','READ','INT',CUPTR,SLOT,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,SLOT,STATUS)
C
C     Map the error array
C
      CALL DSA_MAP_ERRORS('OUTPUT','UPDATE','FLOAT',EPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Create workspace array
C
      BYTES = 4*NELM*NT + 4*NELM*NT + 4*NELM
      CALL DSA_GET_WORKSPACE(BYTES,WPTR,SLOT,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Error getting workspace',STATUS)
         GOTO 500
      ELSE
         EXPTR=WPTR+4*NELM*NT
         NUMPTR=EXPTR+4*NELM*NT
      END IF
C
C     Perform the basic processing of the cube down to either
C     an image or a spectrum.
C
      CALL FIG_FET321(%VAL(CNF_PVAL(CUPTR)),NX,NY,NT,NELM,DETECTOR,
     :                DETECTORS,ADD,BACK,FACTOR,NORM,CUTOFF,
     :                %VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(EXPTR)),
     :                %VAL(CNF_PVAL(NUMPTR)),%VAL(CNF_PVAL(OPTR)),
     :                %VAL(CNF_PVAL(EPTR)))
C
C     Force an etalon position array - this code rather assumes that
C     there isn't one at present in the input structure.
C
      DO IX=1,NELM
         POSNS(IX)=ETLSSTEP+(IX-1)*ETLSTP
      END DO
      CALL DSA_COERCE_AXIS_DATA('OUTPUT',1,'FLOAT',1,NELM,STATUS)
      CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'UPDATE','FLOAT',
     :   XPTR,SLOT,STATUS)
      CALL GEN_MOVE(NELM*DSA_TYPESIZE('FLOAT',STATUS),POSNS,
     :              %VAL(CNF_PVAL(XPTR)))
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Error writing etalon position array',STATUS)
         GO TO 500
      END IF
      CITEMS(1)=' '
      CITEMS(2)='Etalon position'
      CALL DSA_SET_AXIS_INFO('OUTPUT',1,2,CITEMS,0,0D0,STATUS)
C
C     Tidy up
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_FET321(CUBE,NX,NY,NT,NELM,DETECTOR,DETECTORS,
     :                  ADD,BACK,FACTOR,NORM,CUTOFF,IMAGE,EXCLUDE,
     :                                    NUM_CYCLES,SPECT,ERRORS)
C
C     F I G _ F E T 3 2 1
C
C     Collapses a FIGS data cube down in etalon mode to a single spectrum.
C
C     First an image is created (as for FIG_FIGS322). Then the
C     cycles of the image are combined to form a spectrum with
C     statistical errors.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) CUBE    (Integer array CUBE(NX,NY,NT)) The data cube.
C     (>) NX      (Integer) The first dimension of CUBE
C     (>) NY      (Integer) The second dimension of CUBE
C     (>) NT      (Integer) The third dimension of CUBE
C     (>) NELM    (Integer) The number of elements in the single
C                 detector spectrum.
C     (>) DETECTOR  (Integer) The detector number to use for the
C                 spectrum.
C     (>) DETECTORS (Integer) The number of detectors in use.
C     (>) ADD     (Logical) If true disables background subtraction
C     (>) BACK    (Logical) If true background only is returned.
C     (>) FACTOR  (Real) The intensity normalizing factor
C     (>) NORM    (Logical) True if normalizing of cycles required
C     (>) CUTOFF  (Real) Cutoff value for despiking (sigma)
C     (>) IMAGE   (Real array IMAGE(NELM,NT)) workspace array
C                 used to create the image
C     (>) EXCLUDE (Logical array EXCLUDE(NELM,NT)) workspace array
C                 used to indicate points to be excluded.
C     (>) NUM_CYCLES (Integer array NUM_CYCLES(NELM))  workspace array
C                 holding number of cycles actually added in to each
C                 spectral point.
C     (<) SPECT   (Real array SPECT(NELM)) The resulting spectrum.
C     (<) ERRORS  (Real array ERRORS(NELM)) The errors on the spectrum.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     GEN_FILL    (GEN_ package) Fill an array of bytes with a constant.
C
C     History:
C
C     6th May 1988.  KS / AAO.  Original version.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY, NT, NELM, CUBE(NX,NY,NT), NUM_CYCLES(NELM)
      INTEGER DETECTOR, DETECTORS
      LOGICAL ADD, BACK, NORM, EXCLUDE(NELM,NT)
      REAL    FACTOR, SPECT(NELM), ERRORS(NELM), IMAGE(NELM,NT)
      REAL    CUTOFF
C
C     Local variables
C
      LOGICAL FINISHED
      INTEGER IT, IX, IXC, NSUM, STATUS
      REAL X, SUM, NF, SIGMA
      CHARACTER*80 OUTMES
C
      CALL GEN_FILL(NELM*4,0,SPECT)
      CALL GEN_FILL(NELM*4,0,ERRORS)
      CALL GEN_FILL(NELM*NT*4,0,EXCLUDE)
C
C     Create a 2D array IMAGE (spectral points * cycles)
C
      DO IT=1,NT
         IXC = DETECTOR
         DO IX=1,NELM
            IF (NY.EQ.8) THEN
C
C              Etalon 1 or 3 mode
C
               IF (ADD) THEN
                  IMAGE(IX,IT)=
     :               +(CUBE(IXC,1,IT)+CUBE(IXC,2,IT))
     :               +(CUBE(IXC,3,IT)+CUBE(IXC,4,IT))
     :               +(CUBE(IXC,5,IT)+CUBE(IXC,6,IT))
     :               +(CUBE(IXC,7,IT)+CUBE(IXC,8,IT))
               ELSE IF (BACK) THEN
                  IMAGE(IX,IT)=
     :               +(CUBE(IXC,2,IT)+CUBE(IXC,3,IT))
     :               +(CUBE(IXC,5,IT)+CUBE(IXC,8,IT))
               ELSE
                  IMAGE(IX,IT)=
     :               +(CUBE(IXC,1,IT)-CUBE(IXC,2,IT))
     :               -(CUBE(IXC,3,IT)-CUBE(IXC,4,IT))
     :               -(CUBE(IXC,5,IT)-CUBE(IXC,6,IT))
     :               +(CUBE(IXC,7,IT)-CUBE(IXC,8,IT))
               END IF
            ELSE
C
C              Etalon 2 mode
C
               IF (ADD) THEN
                  IMAGE(IX,IT)=
     :               +(CUBE(IXC,1,IT)+CUBE(IXC,2,IT))
     :               +(CUBE(IXC,3,IT)+CUBE(IXC,4,IT))
               ELSE IF (BACK) THEN
                  IMAGE(IX,IT)=
     :               +(CUBE(IXC,2,IT)+CUBE(IXC,3,IT))
               ELSE
                  IMAGE(IX,IT)=
     :               +(CUBE(IXC,1,IT)+CUBE(IXC,4,IT))
     :               -(CUBE(IXC,2,IT)+CUBE(IXC,3,IT))
               END IF
            END IF
            IMAGE(IX,IT)=IMAGE(IX,IT)/FACTOR
            IXC = IXC + DETECTORS
         END DO
      END DO
C
C     Create the spectrum array
C
      FINISHED = .FALSE.
      DO WHILE (.NOT. FINISHED)
         FINISHED = .TRUE.
         IF (NORM) THEN
C
C        Sum all points to get mean level
C
            SUM = 0
            NSUM = 0
            DO IT=1,NT
               DO IX=1,NELM
                  IF (.NOT. EXCLUDE(IX,IT)) THEN
                     SUM=SUM+IMAGE(IX,IT)
                     NSUM=NSUM+1
                  END IF
               END DO
            END DO
            SUM=SUM/NSUM
         END IF
         CALL GEN_FILL(NELM*4,0,SPECT)
         CALL GEN_FILL(NELM*4,0,NUM_CYCLES)
         DO IT=1,NT
            DO IX=1,NELM
               IF (.NOT. EXCLUDE(IX,IT)) THEN
                   SPECT(IX)=SPECT(IX)+IMAGE(IX,IT)
                   NUM_CYCLES(IX)=NUM_CYCLES(IX)+1
               END IF
            END DO
         END DO
         DO IX=1,NELM
            SPECT(IX)=SPECT(IX)/REAL(NUM_CYCLES(IX))
         END DO
C
C     Create the errors array
C
         IF (NT .GT. 1) THEN
            CALL GEN_FILL(NELM*4,0,ERRORS)
            DO IT=1,NT
               IF (NORM) THEN
                  NF = 0
                  NSUM = 0
                  DO IX=1,NELM
                     IF (.NOT. EXCLUDE(IX,IT)) THEN
                         NF=NF+IMAGE(IX,IT)
                         NSUM = NSUM+1
                     END IF
                  END DO
                  NF=NF/NSUM
                  DO IX=1,NELM
                     X=SUM/NF*IMAGE(IX,IT)-SPECT(IX)
                     ERRORS(IX)=ERRORS(IX)+X*X
                  END DO
               ELSE
                  DO IX=1,NELM
                     IF (.NOT. EXCLUDE(IX,IT)) THEN
                        X=IMAGE(IX,IT)-SPECT(IX)
                        ERRORS(IX)=ERRORS(IX)+X*X
                     END IF
                  END DO
               END IF
            END DO
            DO IX=1,NELM
               ERRORS(IX)=SQRT(ERRORS(IX)/NUM_CYCLES(IX)/
     :                     (NUM_CYCLES(IX)-1))
            END DO
         END IF
C
C     Check for points to exclude - if there are any, set FINISHED to
C     false to repeat calculation of spectrum and error
C
         IF (CUTOFF .LT. 1E37) THEN
            DO IX=1,NELM
               SIGMA = ERRORS(IX) * SQRT(REAL(NUM_CYCLES(IX)))
               DO IT=1,NT
                  IF (.NOT. EXCLUDE(IX,IT)) THEN
                     IF (ABS(IMAGE(IX,IT)-SPECT(IX)) .GT.
     :                                           (CUTOFF*SIGMA)) THEN
                        EXCLUDE(IX,IT) = .TRUE.
                        FINISHED = .FALSE.
                        WRITE(OUTMES,
     :                     '('' Point'',I4,'' Cycle'',I4,'//
     :                     ' '' Excluded'')') IX,IT
                        CALL PAR_WRUSER(OUTMES,STATUS)
                     END IF
                  END IF
               END DO
            END DO
         END IF
      END DO
      END
