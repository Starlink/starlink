C+
      SUBROUTINE FIGS32N
C
C     F I G S 3 2 2 ,   F I G S 3 2 1
C
C     Given a FIGS data cube as produced by the FIGS data acquisition
C     system, processes it to produce either an image of wavelength
C     against cycle number (FIGS322) or a single spectrum (FIGS321).
C
C     Command parameters -
C
C     'CUBE'     The name of the cube from which the planes
C                are to be taken.  This should be a raw FIGS data
C                cube.
C     'IMAGE'    The name of the resulting image (FIGS322)
C     'SPECTRUM' The name of the resulting spectrum (FIGS321)
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
C                (FIGS321 only.)
C
C     Input data -
C
C     This routine assumes that the first axis of the cube data
C     represents wavelength, that the second represents spectral
C     scans in the order A1a,A1b,B1a,B1b,B2a,B2b,A2a,A2b, where
C     A1,A2,B1,B2 represent the parts of the beamswitch ABBA cycle
C     and a and b represent the signal and background chop positions
C     respectively.  In grating mode 2, there are no chop positions,
C     and the second axis is just A1,B1,A2,B2. Grating mode 3 data
C     is modified by the on-line acquisition software so that it
C     has the same format as grating mode 1 data.  This means that the
C     second dimension of the cube has to be either 4 or 8.  The
C     cube third axis represents beamswitch cycle number.
C     The data is sorted into wavelength order using the various
C     grating parameters read from the .FITS sub-structure of CUBE.
C     The data is normalized to give a figure in detected photons
C     per second.
C
C     Output data -
C
C     IMAGE is created with the same structure as CUBE
C     except that main data array will only have 1 or 2 dimensions, and
C     any AXIS sub-structures that CUBE has will be deleted/renamed
C     as appropriate. If a spectrum is produced the errors (derived from
C     the cycle to cycle statistics) are generated.
C
C                                     KS / AAO 8th June 1985
C
C     Modified:
C
C     14th Jun 1985  JAB / AAO.  BACK and NORM added, error calculation
C                    improved.
C     12th Aug 1985  KS / AAO.   Errors now calculated as percentages
C                    rather than absolute values.
C      9th Apr 1986  JAB / AAO.  CUTOFF added.
C     15th Apr 1986  KS / AAO.   Grating mode 3 now supported.
C     22nd Jul 1986  KS / AAO.   Reverted to absolute values for errors.
C                    .X.WIDTH now calculated and added to output.
C     27th Jun 1989  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     29th Mar 1991  JMS / AAO. Changed type from 'INTEGER' to 'INT'
C                    when mapping input data. Now uses DSA_SET_WIDTH to
C                    set axis width value. Added STATUS checks to
C                    support user requested aborts.
C     28th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. TABs
C                    removed.
C     13th Mar 1996  HME / UoE, Starlink.  Fixed broken-up format string
C                    in WRITE.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
*     2015 June 19   MJC /EAO.  Permit units of um for microns.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER DSA_TYPESIZE
      LOGICAL PAR_ABORT
C
C     Local variables
C
      LOGICAL   ADD              ! True if data are to be added
      LOGICAL   BACK             ! True if background only required
      INTEGER   BYTES            ! Bytes required for an array
      REAL      CHOPS            ! No. of chops per recorded spectrum
      CHARACTER CITEMS(2)*32     ! Axis character items retrieved
      CHARACTER COMENT*16        ! Comment associated with FITS items
      CHARACTER COMMAND*8        ! Figaro command name
      INTEGER   CUPTR            ! Dynamic memory element for CUBE data
      REAL      CUTOFF           ! Sigma level at which a point will be
                                 ! ignored (FIGS321 only)
      REAL      DETSP            ! The detector spacing in steps
      INTEGER   DIMS(10)         ! Image dimensions
      INTEGER   EPTR             ! Dynamic-memory pointer for error
                                 ! array
      INTEGER   EXPTR            ! Dynamic-memory pointer for workspace
      REAL      FACTOR           ! Integration time in seconds per
                                 ! element
      LOGICAL   FAULT            ! True if non_DSA problem found
      INTEGER   FSTAT            ! Status for FIG routines
      REAL      GAIN             ! The gain factor of the system
      REAL      GRATORD          ! Grating order.
      REAL      GRATS            ! Grating start step.
      REAL      GRATSP           ! Number of grating positions used
      REAL      GRATSW           ! No. of grating sweeps per recorded
                                 ! spectrum
      REAL      GRT0ORD          ! Grating step position at 0th order
      REAL      GRTA2S           ! Grating arc sec per halfstep
      REAL      GRTPCH           ! Grating pitch
      LOGICAL   GSPECT           ! Spectrum (not an image) to be
                                 ! generated?
      REAL      INT              ! No. of msec for each such integration
      INTEGER   LENGTH           ! Spectrum length
      INTEGER   MODE             ! Cube data grating mode
      INTEGER   NCITEMS          ! Number of axis character items
                                 ! retrieved
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image - ignored
      DOUBLE PRECISION NITEMS(1) ! Axis numeric items retrieved
      INTEGER   NNITEMS          ! Number of axis numeric items
                                 ! retrieved
      LOGICAL   NORM             ! True if normalising required
      INTEGER   NSPECT           ! No. of spectra added per cycle
      INTEGER   NT               ! Second dimension of CUBE
      INTEGER   NUMPTR           ! Dynamic-memory pointer for workspace
      INTEGER   NX               ! First dimension of CUBE
      INTEGER   NY               ! Second dimension of CUBE
      INTEGER   OPTR             ! Dynamic-memory pointer for OUTPUT
                                 ! data
      INTEGER   OXPTR            ! Dynamic-memory pointer for AXIS(1)
                                 ! data
      REAL      POINTS           ! No. of integrations performed at each
                                 ! grating setting
      INTEGER   PSTAT            ! Status for PAR routines
      REAL      SECS             ! Total integration time in seconds.
      INTEGER   SLOT             ! Slot number for mapped data
      INTEGER   SLOT1            ! Slot number for mapped workspace
      INTEGER   SLOT2            ! Slot number for mapped workspace
      INTEGER   STATUS           ! Running status for DSA routines
      REAL      STEP             ! Grating step value.
      INTEGER   SWPERC           ! No. of sweeps per chop
      INTEGER   VECTOR(416)      ! Sort vector
      REAL      WAVES(416)       ! Returned with the wavelength values
      REAL      WIDTH            ! The width of a detector in wavelength
                                 ! terms
      REAL      WORK(416)        ! Workspace
      INTEGER   WPTR             ! Dynamic-memory pointer for workspace
C
C     Grating mode parameter values
C
      INTEGER GRATING1, GRATING2, GRATING3
      PARAMETER (GRATING1=1, GRATING2=2, GRATING3=4)
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
      NNITEMS=0
      NCITEMS=2
      FAULT=.FALSE.
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get name of CUBE file
C
      CALL DSA_INPUT ('CUBE','CUBE',STATUS)
C
C     Get mode of cube data
C
      CALL DSA_GET_FITS_I('CUBE','FIG_MODE',0,MODE,COMENT,STATUS)
      IF(STATUS.NE.0)GOTO 500
      IF ((MODE.NE.GRATING1).AND.(MODE.NE.GRATING2).AND.
     :                              (MODE.NE.GRATING3)) THEN
         CALL PAR_WRUSER(
     :       'The data mode (FIG_MODE) is not a value this '
     :       //'program can handle.',PSTAT)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Get size of data in CUBE
C
      CALL DSA_DATA_SIZE ('CUBE',10,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
      IF (NDIM.NE.3) THEN
         CALL PAR_WRUSER('Input data is not an cube',PSTAT)
         FAULT=.TRUE.
         GO TO 500
      END IF
      NT=DIMS(3)
      NY=DIMS(2)
      NX=DIMS(1)
      IF (((MODE.EQ.GRATING1).OR.(MODE.EQ.GRATING3))
     :                                      .AND.(NY.NE.8)) THEN
         CALL PAR_WRUSER(
     :          'Second dimension of cube is not 8.  This cannot be',
     :                                                        PSTAT)
         IF (MODE.EQ.GRATING1) THEN
            CALL PAR_WRUSER(
     :             'data from FIGS in grating mode 1.',PSTAT)
         ELSE
            CALL PAR_WRUSER(
     :             'data from FIGS in grating mode 3.',PSTAT)
         END IF
         GO TO 500
      END IF
      IF ((MODE.EQ.GRATING2).AND.(NY.NE.4)) THEN
         CALL PAR_WRUSER(
     :          'Second dimension of cube is not 4.  This cannot be',
     :                                                        PSTAT)
         CALL PAR_WRUSER(
     :          'data from FIGS in grating mode 2.',PSTAT)
         GO TO 500
      END IF
      IF (NX.GT.416) THEN
         CALL PAR_WRUSER(
     :    '1st dimension of cube is greater than 416.  This cannot be',
     :                                                        PSTAT)
         CALL PAR_WRUSER(
     :    'data from the FIGS data acquisition system.',PSTAT)
         GO TO 500
      END IF
C
C     Find which command this is.  GSPECT indicates that a
C     spectrum (rather than an image) is to be generated.
C
      CALL PAR_COMMAND(COMMAND)
      GSPECT=COMMAND.EQ.'FIGS321'
C
C     Get name of IMAGE or SPECTRUM file to be created.
C
      IF (GSPECT) THEN
         CALL PAR_SDVAL('CUTOFF',1E38,PSTAT)
         CALL PAR_RDVAL('CUTOFF',0,1E38,1E38,'SIGMA',CUTOFF)
         IF (PAR_ABORT()) GOTO 500
         CALL DSA_OUTPUT('OUTPUT','SPECTRUM','CUBE',NO_DATA,NEW_FILE,
     :                    STATUS)
      ELSE
         CALL DSA_OUTPUT('OUTPUT','IMAGE','CUBE',NO_DATA,NEW_FILE,
     :                    STATUS)
      END IF
      IF(STATUS.NE.0)GOTO 500
C
C     See if we have to add the data
C
      CALL PAR_RDKEY('ADD',.FALSE.,ADD)
      IF (PAR_ABORT()) GOTO 500
C
C     See if background only required
C
      CALL PAR_RDKEY('BACK',.FALSE.,BACK)
      IF (PAR_ABORT()) GOTO 500
C
C     See if normalizing required
C
      IF (GSPECT) THEN
         CALL PAR_RDKEY('NORM',.FALSE.,NORM)
         IF (PAR_ABORT()) GOTO 500
      END IF
C
C     Work out how many signal spectra we are going to be adding
C
      IF (ADD) THEN
         NSPECT=NY
      ELSE
         NSPECT=NY/2
      END IF
C
C     Get the wavelength information out of the image structure.
C     It has to be there, so error if it isn't.
C
      CALL DSA_GET_FITS_F('CUBE','FIG_STEP',0,STEP,COMENT,STATUS)
      CALL DSA_GET_FITS_F('CUBE','FIG_GRTS',0,GRATS,COMENT,STATUS)
      CALL DSA_GET_FITS_F('CUBE','FIG_GRSP',0,GRATSP,COMENT,STATUS)
      CALL DSA_GET_FITS_F('CUBE','FIG_GORD',0,GRATORD,COMENT,STATUS)
      CALL DSA_GET_FITS_F('CUBE','FIG_GA2S',0,GRTA2S,COMENT,STATUS)
      CALL DSA_GET_FITS_F('CUBE','FIG_GPCH',0,GRTPCH,COMENT,STATUS)
      CALL DSA_GET_FITS_F('CUBE','FIG_G0OR',0,GRT0ORD,COMENT,STATUS)
      CALL DSA_GET_FITS_F('CUBE','FIG_DTSP',0,DETSP,COMENT,STATUS)
C
C  Get Intensity scaling information
C
      CALL DSA_GET_FITS_F('CUBE','FIG_PTS',0,POINTS,COMENT,STATUS)
      CALL DSA_GET_FITS_F('CUBE','FIG_INT',0,INT,COMENT,STATUS)
      CALL DSA_GET_FITS_F('CUBE','FIG_GAIN',0,GAIN,COMENT,STATUS)
      CALL DSA_GET_FITS_F('CUBE','FIG_DTSP',0,DETSP,COMENT,STATUS)

      IF ((MODE.EQ.GRATING1).OR.(MODE.EQ.GRATING3)) THEN
         CALL DSA_GET_FITS_F('CUBE','FIG_CHOP',0,CHOPS,COMENT,STATUS)
         SWPERC=2
         GRATSW=1
      ELSE
         CALL DSA_GET_FITS_F('CUBE','FIG_GRSW',0,GRATSW,COMENT,STATUS)
         SWPERC=1
         CHOPS=1
      END IF
      IF (STATUS.NE.0)  GO TO 350
C
C     Got here, all must be present.  Carry on.
C
      GO TO 350
C
C     If we get here, one or more must be missing.
C
  340 CONTINUE
      CALL PAR_WRUSER(
     :     'One or more of the quantities FIG_PTS,FIG_INT,',PSTAT)
      CALL PAR_WRUSER(
     :     'FIG_STEP,FIG_GRTS,FIG_GRSP,FIG_GORD,FIG_GA2S,FIG_GPCH,',
     :                                                        PSTAT)
      IF ((MODE.EQ.GRATING1).OR.(MODE.EQ.GRATING3)) THEN
         CALL PAR_WRUSER(
     :     'FIG_DTSP,FIG_G0OR,FIG_GAIN,FIG_CHOP are missing from ',
     :                                                        PSTAT)
      ELSE
         CALL PAR_WRUSER(
     :     'FIG_DTSP,FIG_G0OR,FIG_GAIN,FIG_GRSW are missing from ',
     :                                                        PSTAT)
      END IF
      CALL PAR_WRUSER('the FITS structure of the input file.',PSTAT)
      FAULT=.TRUE.
      GO TO 500
C
  350 CONTINUE
C
C     Calculate intensity normalizing factor.  Note the way this works:
C     POINTS is the number of integrations performed at each grating setting
C     INT    is the number of msec for each such integration
C     GAIN   is the gain factor of the system, and is such that
C            (GAIN/1000)/9.5 is the number of ADUs per detected photon.
C     SWPERC is the number of sweeps per chop
C     CHOPS  is the number of chops per recorded spectrum
C     GRATSW is the number of grating sweeps per recorded spectrum
C            (Note that in mode 2, the grating does not chop, but has
C            multiple sweeps instead.  In mode 1, GRATSW=1, and SWPERC=2,
C            while in mode 2, CHOPS=1 and SWPERC=1)
C     NSPECT is the number of spectra being added up to give the data
C            for each cycle.  This depends on the mode, and also on the
C            setting of ADD.
C
C     This means that SECS=POINTS*(INT/1000)*GRATSW*CHOPS*SWPERC gives the
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
      SECS = POINTS * (INT/1000) * GRATSW * CHOPS * SWPERC
      FACTOR = SECS * NSPECT * (GAIN/1000) / 9.5
C
C     Calculate the wavelength values for the data, and generate
C     the data sort vector based on those values.
C
      CALL FIG_FIGWAVES(GRATS,STEP,GRTPCH,GRATORD,GRT0ORD,GRTA2S,
     :                         GRATSP,DETSP,LENGTH,WIDTH,WAVES,FSTAT)
      IF (FSTAT.NE.0) THEN
         FAULT=.TRUE.
         GO TO 500
      END IF
      IF (LENGTH.NE.NX) THEN
         CALL PAR_WRUSER(
     :       'Spectrum length deduced from grating parameters',PSTAT)
         CALL PAR_WRUSER('differs from 1st dimension of data cube',
     :                                                         PSTAT)
         FAULT=.TRUE.
         GO TO 500
      END IF
      CALL GEN_QFISORT(WAVES,LENGTH,VECTOR)
C
C     Create the output data structure based on the input data
C     structure and map
C
      IF (GSPECT) THEN
         NDIM=1
         DIMS(1)=NX
         NELM=NX
      ELSE
         NDIM=2
         DIMS(1)=NX
         DIMS(2)=NT
         NELM=NX*NT
      END IF

      CALL DSA_RESHAPE_DATA('OUTPUT','CUBE',NDIM,DIMS,STATUS)
      CALL DSA_GET_DATA_INFO('OUTPUT',NCITEMS,CITEMS,NNITEMS,NITEMS,
     :                        STATUS)
      CITEMS(1)='Photons/sec'
      CALL DSA_SET_DATA_INFO('OUTPUT',NCITEMS,CITEMS,NNITEMS,NITEMS,
     :                        STATUS)
C
C     Map the input and output data
C
      CALL DSA_MAP_DATA('CUBE','READ','INT',CUPTR,SLOT,STATUS)

      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,SLOT,STATUS)
C
C     Map the error array
C
      IF (GSPECT) THEN
         CALL DSA_MAP_ERRORS('OUTPUT','UPDATE','FLOAT',EPTR,SLOT,STATUS)
      END IF
C
C     Create workspace array
C
      IF (GSPECT) THEN
         BYTES = DSA_TYPESIZE('FLOAT',STATUS)*(NX*NT + NX*NT + NX)
         CALL DSA_GET_WORK_ARRAY(NX*NT,'FLOAT',WPTR,SLOT,STATUS)
         CALL DSA_GET_WORK_ARRAY(NX*NT,'FLOAT',EXPTR,SLOT1,STATUS)
         CALL DSA_GET_WORK_ARRAY(NT,'FLOAT',NUMPTR,SLOT2,STATUS)
         IF(STATUS.NE.0) GOTO 500
      END IF
      IF(STATUS.NE.0) GOTO 500

C
C     Perform the basic processing of the cube down to either
C     an image or a spectrum.
C
      IF (GSPECT) THEN
         CALL FIG_FIGS321(%VAL(CNF_PVAL(CUPTR)),NX,NY,NT,VECTOR,ADD,
     :                    BACK,FACTOR,NORM,CUTOFF,%VAL(CNF_PVAL(WPTR)),
     :                    %VAL(CNF_PVAL(EXPTR)),%VAL(CNF_PVAL(NUMPTR)),
     :                    %VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(EPTR)))
      ELSE
         CALL FIG_FIGS322(%VAL(CNF_PVAL(CUPTR)),NX,NY,NT,VECTOR,ADD,
     :                    BACK,FACTOR,%VAL(CNF_PVAL(OPTR)))
      END IF
      CALL DSA_FREE_WORKSPACE(SLOT2,STATUS)
      CALL DSA_FREE_WORKSPACE(SLOT1,STATUS)
      CALL DSA_FREE_WORKSPACE(SLOT,STATUS)
C
C     If the original cube had a .T  structure, then
C     this may need renaming.
C
C
C     Copy all of the input structure to the output, except for
C     the .Y component (which is always lost) and the .T component
C     (which is lost if a spectrum is created).  The .Z structure is
C     handled separately.
C
C     The input AXIS(1) information is copied to the output.
C     If an image is being created the AXIS(3) information is
C     copied to the output AXIS(2) structure.

      CALL DSA_RESHAPE_AXIS('OUTPUT',1,'CUBE',1,1,NX,STATUS)
      IF(.NOT.GSPECT)THEN
         CALL DSA_RESHAPE_AXIS('OUTPUT',2,'CUBE',3,1,NT,STATUS)
      END IF
      IF(STATUS.NE.0) GOTO 500
C
C     Force a wavelength array - this code rather assumes that there
C     isn't one at present in the input structure.
C
      CALL GEN_FVSORT(VECTOR,NX,1,WORK,WAVES)
      CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'UPDATE','FLOAT',OXPTR,SLOT,
     :                       STATUS)
      IF(STATUS.NE.0) GOTO 500

      BYTES=NX*DSA_TYPESIZE('FLOAT',STATUS)
      CALL GEN_MOVE(BYTES,WAVES,%VAL(CNF_PVAL(OXPTR)))

      CITEMS(1)='um           '
      CITEMS(2)='Wavelength   '
      CALL DSA_SET_AXIS_INFO('OUTPUT',1,NCITEMS,CITEMS,NNITEMS,NITEMS,
     :                        STATUS)

C
C     Set axis width value.
C
      CALL DSA_SET_WIDTH ('OUTPUT',1,DBLE(WIDTH),STATUS)
C
C
  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR

      END
C+

      SUBROUTINE FIG_FIGS321(CUBE,NX,NY,NT,VECTOR,ADD,BACK,FACTOR,
     :    NORM,CUTOFF,IMAGE,EXCLUDE,NUM_CYCLES,SPECT,ERRORS)
C
C     F I G _ F I G S 3 2 1
C
C     Collapses a FIGS data cube down to a single spectrum.
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
C     (>) VECTOR  (Integer array VECTOR(NX)) The wavelength sort
C                 vector for the data.
C     (>) ADD     (Logical) If true disables background subtraction
C     (>) BACK    (Logical) If true background only is returned.
C     (>) FACTOR  (Real) The intensity normalizing factor
C     (>) NORM    (Logical) True if normalizing of cycles required
C     (>) CUTOFF  (Real) Cutoff value for despiking (sigma)
C     (>) IMAGE   (Real array IMAGE(NX,NT)) workspace array
C                 used to create the image
C     (>) EXCLUDE (Logical array EXCLUDE(NX,NT)) workspace array
C                 used to indicate points to be excluded.
C     (>) NUM_CYCLES (Integer array NUM_CYCLES(NX))  workspace array
C                 holding number of cycles actually added in to each
C                 spectral point.
C     (<) SPECT   (Real array SPECT(NX)) The resulting spectrum.
C     (<) ERRORS  (Real array ERRORS(NX)) The errors on the spectrum.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     GEN_FILL    (GEN_ package) Fill an array of bytes with a constant.
C     GEN_FVSORT  (  "     "   ) Indirect array sort using a sort vector
C
C                                            KS / AAO 21st May 1985
C     Modified:
C
C     12th Aug 1985.  KS / AAO.  Errors now calculated as percentages.
C     9th Apr 1986   JAB / AAO.  CUTOFF added
C     22nd July 1986  KS / AAO.  Revert to calculating errors as
C                     absolute values.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY, NT, VECTOR(NX), NUM_CYCLES(NX),CUBE(NX,NY,NT)
      LOGICAL ADD, BACK, NORM, EXCLUDE(NX,NT)
      REAL FACTOR, SPECT(NX), ERRORS(NX), IMAGE(NX,NT), CUTOFF
C
C     Local variables
C
      LOGICAL FINISHED
      INTEGER IT, IX, NSUM, STATUS
      REAL WORK(416), X, SUM, NF, SIGMA
      CHARACTER*80 OUTMES
C
      CALL GEN_FILL(NX*4,0,SPECT)
      CALL GEN_FILL(NX*4,0,ERRORS)
      CALL GEN_FILL(NX*NT*4,0,EXCLUDE)
C
C     Create a 2D array IMAGE (spectral points * cycles)
C
      DO IT=1,NT
         DO IX=1,NX
            IF (NY.EQ.8) THEN
C
C              Grating 1 or 3 mode
C
               IF (ADD) THEN
                  IMAGE(IX,IT)=
     :               +(CUBE(IX,1,IT)+CUBE(IX,2,IT))
     :               +(CUBE(IX,3,IT)+CUBE(IX,4,IT))
     :               +(CUBE(IX,5,IT)+CUBE(IX,6,IT))
     :               +(CUBE(IX,7,IT)+CUBE(IX,8,IT))
               ELSE IF (BACK) THEN
                  IMAGE(IX,IT)=
     :               +(CUBE(IX,2,IT)+CUBE(IX,3,IT))
     :               +(CUBE(IX,5,IT)+CUBE(IX,8,IT))
               ELSE
                  IMAGE(IX,IT)=
     :               +(CUBE(IX,1,IT)-CUBE(IX,2,IT))
     :               -(CUBE(IX,3,IT)-CUBE(IX,4,IT))
     :               -(CUBE(IX,5,IT)-CUBE(IX,6,IT))
     :               +(CUBE(IX,7,IT)-CUBE(IX,8,IT))
               END IF
            ELSE
C
C              Grating 2 mode
C
               IF (ADD) THEN
                  IMAGE(IX,IT)=
     :               +(CUBE(IX,1,IT)+CUBE(IX,2,IT))
     :               +(CUBE(IX,3,IT)+CUBE(IX,4,IT))
               ELSE IF (BACK) THEN
                  IMAGE(IX,IT)=
     :               +(CUBE(IX,2,IT)+CUBE(IX,3,IT))
               ELSE
                  IMAGE(IX,IT)=
     :               +(CUBE(IX,1,IT)+CUBE(IX,4,IT))
     :               -(CUBE(IX,2,IT)+CUBE(IX,3,IT))
               END IF
            END IF
            IMAGE(IX,IT)=IMAGE(IX,IT)/FACTOR
         END DO
         CALL GEN_FVSORT(VECTOR,NX,1,WORK,IMAGE(1,IT))
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
               DO IX=1,NX
                  IF (.NOT. EXCLUDE(IX,IT)) THEN
                     SUM=SUM+IMAGE(IX,IT)
                     NSUM=NSUM+1
                  END IF
               END DO
            END DO
            SUM=SUM/NSUM
         END IF
         CALL GEN_FILL(NX*4,0,SPECT)
         CALL GEN_FILL(NX*4,0,NUM_CYCLES)
         DO IT=1,NT
            DO IX=1,NX
               IF (.NOT. EXCLUDE(IX,IT)) THEN
                  SPECT(IX)=SPECT(IX)+IMAGE(IX,IT)
                  NUM_CYCLES(IX)=NUM_CYCLES(IX)+1
               END IF
            END DO
         END DO
         DO IX=1,NX
            SPECT(IX)=SPECT(IX)/REAL(NUM_CYCLES(IX))
         END DO
C
C     Create the errors array
C
         IF (NT .GT. 1) THEN
            CALL GEN_FILL(NX*4,0,ERRORS)
            DO IT=1,NT
               IF (NORM) THEN
                  NF = 0
                  NSUM = 0
                  DO IX=1,NX
                     IF (.NOT. EXCLUDE(IX,IT)) THEN
                        NF=NF+IMAGE(IX,IT)
                        NSUM = NSUM+1
                     END IF
                  END DO
                  NF=NF/NSUM
                  DO IX=1,NX
                     X=SUM/NF*IMAGE(IX,IT)-SPECT(IX)
                     ERRORS(IX)=ERRORS(IX)+X*X
                  END DO
               ELSE
                  DO IX=1,NX
                     IF (.NOT. EXCLUDE(IX,IT)) THEN
                        X=IMAGE(IX,IT)-SPECT(IX)
                        ERRORS(IX)=ERRORS(IX)+X*X
                     END IF
                  END DO
               END IF
            END DO
            DO IX=1,NX
               ERRORS(IX)=SQRT(ERRORS(IX)/NUM_CYCLES(IX)/
     :                    (NUM_CYCLES(IX)-1))
            END DO
         END IF
C
C     Check for points to exclude - if there are any, set FINISHED to
C     false to repeat calculation of spectrum and error
C
         IF (CUTOFF .LT. 1E37) THEN
            DO IX=1,NX
               SIGMA = ERRORS(IX) * SQRT(REAL(NUM_CYCLES(IX)))
               DO IT=1,NT
                  IF (.NOT. EXCLUDE(IX,IT)) THEN
                     IF (ABS(IMAGE(IX,IT)-SPECT(IX))
     :                     .GT. (CUTOFF*SIGMA)) THEN
                        EXCLUDE(IX,IT) = .TRUE.
                        FINISHED = .FALSE.
                        WRITE(OUTMES,'('' Point'',I4,'' Cycle'',I4,'//
     :                       ''' Excluded'')') IX,IT
                        CALL PAR_WRUSER(OUTMES,STATUS)
                     END IF
                  END IF
               END DO
            END DO
         END IF
      END DO
      END
C+
      SUBROUTINE FIG_FIGS322(CUBE,NX,NY,NT,VECTOR,ADD,BACK,FACTOR,IMAGE)
C
C     F I G _ F I G S 3 2 2
C
C     Collapses a FIGS data cube down to an image.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) CUBE    (Integer array CUBE(NX,NY,NT)) The data cube.
C     (>) NX      (Integer) The first dimension of CUBE
C     (>) NY      (Integer) The second dimension of CUBE
C     (>) NT      (Integer) The third dimension of CUBE
C     (>) VECTOR  (Integer array VECTOR(NX)) The wavelength sort
C                 vector for the data.
C     (>) ADD     (Logical) If true, disables background subtraction.
C     (>) BACK    (Logical) If true, background only is returned.
C     (>) FACTOR  (Real) The Intensity normalizing factor
C     (<) IMAGE   (Real array IMAGE(NX,NT)) The resulting image.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     GEN_FVSORT  (  "     "   ) Indirect array sort using a sort vector
C
C                                            KS / AAO 21st May 1985
C     Modified:
C
C     12th June 1986   KS / AAO. Grating mode 2 code corrected so
C                      correct spectra subtracted.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL ADD, BACK
      INTEGER NX, NY, NT, VECTOR(NX),CUBE(NX,NY,NT)
      REAL FACTOR, IMAGE(NX,NT)
C
C     Local variables
C
      INTEGER IT, IX
      REAL WORK(416)
C
      DO IT=1,NT
         DO IX=1,NX
            IF (NY.EQ.8) THEN
C
C              Grating 1 or 3 mode
C
               IF (ADD) THEN
                  IMAGE(IX,IT)=
     :               +(CUBE(IX,1,IT)+CUBE(IX,2,IT))
     :               +(CUBE(IX,3,IT)+CUBE(IX,4,IT))
     :               +(CUBE(IX,5,IT)+CUBE(IX,6,IT))
     :               +(CUBE(IX,7,IT)+CUBE(IX,8,IT))
               ELSE IF (BACK) THEN
                  IMAGE(IX,IT)=
     :               +(CUBE(IX,2,IT)+CUBE(IX,3,IT))
     :               +(CUBE(IX,5,IT)+CUBE(IX,8,IT))
               ELSE
                  IMAGE(IX,IT)=
     :               +(CUBE(IX,1,IT)-CUBE(IX,2,IT))
     :               -(CUBE(IX,3,IT)-CUBE(IX,4,IT))
     :               -(CUBE(IX,5,IT)-CUBE(IX,6,IT))
     :               +(CUBE(IX,7,IT)-CUBE(IX,8,IT))
               END IF
            ELSE
C
C              Grating 2 mode
C
               IF (ADD) THEN
                  IMAGE(IX,IT)=
     :               +(CUBE(IX,1,IT)+CUBE(IX,2,IT))
     :               +(CUBE(IX,3,IT)+CUBE(IX,4,IT))
               ELSE IF (BACK) THEN
                  IMAGE(IX,IT)=
     :               +(CUBE(IX,2,IT)+CUBE(IX,3,IT))
               ELSE
                  IMAGE(IX,IT)=
     :               +(CUBE(IX,1,IT)+CUBE(IX,4,IT))
     :               -(CUBE(IX,2,IT)+CUBE(IX,3,IT))
               END IF
            END IF
            IMAGE(IX,IT)=IMAGE(IX,IT)/FACTOR
         END DO
         CALL GEN_FVSORT(VECTOR,NX,1,WORK,IMAGE(1,IT))
      END DO
C
      END

