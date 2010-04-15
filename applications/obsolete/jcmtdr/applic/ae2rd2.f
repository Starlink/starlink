      SUBROUTINE AE2RD2
*+
*  Name:
*     AE2RD2

*  Purpose:
*     Rebin Az,el map(s) to RA,Dec by interpolation using NAG algorithm
*     following method of Renka & Cline.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AE2RD2

*  Description:
*     {routine_description}

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     REVAD::JFL: J.Lightfoot
*     REVAD::HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1991 (REVAD::JFL):
*        Original version
*     11-NOV-1991 (REVAD::JFL): Corrected major bug which failed to correct
*                               RA offsets for cos(dec) effect.
*     2-MAR-1992 (REVAD::JFL): Added pointing corrections, modified to use
*        more compact structure names, calls JCMT_OFFSET2RADEC which
*        can handle RA/Dec offsets and uses correct tangent plane calculations
*        in all cases.
*     26-MAY-1992 (REVAD::JFL): Name changed from AE2RD1 to AE2RD2
*     11-OCT-1993 (REVAD::JFL): Pointing corrections changed from dRA,dDec to
*        dAz, dAlt
*     22-MAR-1994 (REVAD::HME): Change include statements.
*     13-JUN-1994 (REVAD::HME): Disuse DSA_WRUSER.
*     13-JUN-1994 (REVAD::HME): Use copy of input file name for folding
*        to upper case
*     14-OCT-1994 (REVAD::JFL): Mods to handle maps correctly on border of
*        RA flipping between 2pi and 0
*     12-JAN-1995 (REVAD::HME): With (F)PAR on top of (A)PAR in Portable
*        Figaro, for INFILE the PAR_SDCHAR is obsolete, but the
*        PAR_RDCHAR must use the variable DEFAULT rather than the
*        constant 'END'.
*     {enter_changes_here}

*  Deficiencies:

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                    ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'


*  Dynamic memory include file - defines DYNAMIC_MEM
      INCLUDE 'DYNAMIC_MEMORY'

*  Data structure error codes
      INCLUDE 'DTA_CODES'

*  functions:
      LOGICAL PAR_ABORT                ! check PAR abort flag
      INTEGER ICH_FOLD                 ! convert string to upper case
      INTEGER DYN_ELEMENT              ! DSA dynamic memory
      INTEGER DYN_INCREMENT            !        "
      INTEGER DSA_TYPESIZE             ! `bytes per variable' function
      INTEGER ICH_LEN                  ! Figaro string length
      DOUBLE PRECISION SLA_EPB2D       ! Besselian epoch to MJD
      DOUBLE PRECISION SLA_EPJ         ! MJD to Julian epoch

*  Status:
      INTEGER STATUS                   ! Global status for DSA routines
      INTEGER DSTAT                    ! status for dta routines

*  Local Constants:
      INTEGER MAX_DIM                  ! max no of dimensions of array
      PARAMETER (MAX_DIM = 10)
      INTEGER MAX_FILES                ! max no of input files
      PARAMETER (MAX_FILES = 10)

*  Local Variables:
      LOGICAL B1950                    ! flag to indicate that the coordinates
                                       ! are to be changed to Besselian 1950
                                       ! equinox
      LOGICAL FAULT                    ! logical to flag a Figaro fault
      LOGICAL STRUC                    ! logical for testing structure
                                       ! existence
      LOGICAL POINTING_CORRECTION      ! T if a pointing correction is to be
                                       ! performed
      INTEGER INT_SIZE                 ! number of bytes in integer
      INTEGER FLOAT_SIZE               !    "      "     "  real
      INTEGER DOUBLE_SIZE              !    "      "     "  double precision
      INTEGER N_FILES                  ! number of input files
      INTEGER FILE                     ! input file index in DO loop
      INTEGER I                        ! DO loop index
      INTEGER NDIM                     ! number of dimensions of array
      INTEGER DIMS (MAX_DIM)           ! size of each dimension
      INTEGER NELM                     ! total number of elements in array
      INTEGER NX                       ! x dimension of input array
      INTEGER NY                       ! y  "       "      "
      INTEGER SLOT                     ! DSA slot number
      INTEGER ADDRESS                  ! DSA address
      INTEGER INPTR                    ! DSA pointer for input data array
      INTEGER XAXPTR                   !    "       "      "   x-axis
      INTEGER YAXPTR                   !    "       "      "
      INTEGER IN_D_SLOT                ! DSA slot number of input data array
      INTEGER IN_A1_SLOT               !    "       "      "      axis 1
      INTEGER IN_A2_SLOT               !    "       "      "      axis 2
      INTEGER NPIXEL (MAX_FILES)       ! number of pixels in each input file
      INTEGER MAX_PIXEL                ! number of pixels in largest input
                                       !   dataset
      INTEGER DATPTR (MAX_FILES)       ! DSA pointers to copies of input data
      INTEGER RAPPTR (MAX_FILES)       !    "       "      "      "      RAs
                                       !   (J2000)
      INTEGER DECPPTR (MAX_FILES)      !    "       "      "      "      Decs
                                       !   (J2000)
      INTEGER ICEN                     ! x index of map centre in output array
      INTEGER JCEN                     ! y index of map centre in output array
      INTEGER NXOUT, NYOUT             ! dimensions of output array
      INTEGER OUT_D_PTR                ! DSA pointer to output data array
      INTEGER OUT_V_PTR                !  "     "      " output variance array
      INTEGER OXAXPTR                  !  "     "      " output x axis
      INTEGER OYAXPTR                  !  "     "      "    "   y
      INTEGER LSTPTR                   !  "     "      " input LST array
      INTEGER NCORR                    ! number of points in pointing
                                       ! correction description
      INTEGER POINT_LSTPTR             ! DSA pointer to LST array in pointing
                                       !   correction description
      INTEGER POINT_DAZPTR             ! DSA pointer to az correction
      INTEGER POINT_DALTPTR            ! DSA pointer to alt correction
      INTEGER INCOPY                   ! DSA pointer to double precision copy
                                       !   of input dataset
      INTEGER RACOPY                   ! DSA pointer to double precision copy
                                       !   of input data RAs
      INTEGER DECCOPY                  ! DSA pointer to double precision copy
                                       !   of input data Decs
      INTEGER GRADS                    ! DSA pointer to scratch array for NAG
                                       !   routine
      INTEGER TRIANG                   !                 "
      INTEGER OUTCOPY_PTR              ! DSA pointer to scratch array holding
                                       !   rebinned dataset
      INTEGER OUTWEIGHT_PTR            ! DSA pointer to scratch array holding
                                       !   weights of coadded pixels
      INTEGER HMSF (4)                 ! holds converted RA and Dec output
                                       !   from SLA routine
      INTEGER NSTRT                    ! needed by SLA string decoding routines
      INTEGER IGNORE                   !
      REAL FBAD                        ! value to flag bad pixel
      REAL WEIGHT (MAX_FILES)          ! weights of input data files relative
                                       !   to first file
      REAL V2Y                         ! angle between vertical of local coords
                                       !   and y offset axis (anti-clockwise,
                                       !   radians)
      REAL X2Y                         ! angle between x and y offset axes
                                       !   (anti-clockwise, radians)
      DOUBLE PRECISION DTEMP           ! scratch
      DOUBLE PRECISION RAFILE          ! RA of input map centre
      DOUBLE PRECISION DECFILE         ! Dec of input map centre
      DOUBLE PRECISION EPOCH           ! Besselian epoch of input centre coords
      DOUBLE PRECISION JEPOCH          ! Julian epoch of input centre coords
      DOUBLE PRECISION MJDSTART        ! MJD of start of observations
      DOUBLE PRECISION MJDTEMP         !
      DOUBLE PRECISION RACEN           ! RA of output map centre
      DOUBLE PRECISION DECCEN          ! Dec of output map centre
      DOUBLE PRECISION LAT             ! Latitude of observatory (radians)
      DOUBLE PRECISION RAMIN, RAMAX, DECMIN, DECMAX ! limits of input pixel
                                                    ! positions
      DOUBLE PRECISION RA_CORR         ! RA offset correction factor for
                                       ! cos(declination) effect
      DOUBLE PRECISION TMAX, TMIN      ! used in working out RAMAX, etc.
      DOUBLE PRECISION CELLX           ! x unit cell dimension (arcsec)
      DOUBLE PRECISION CELLY           ! y unit cell dimension (arcsec)
      DOUBLE PRECISION PIXSPACE (MAX_FILES)
                                       ! max pixel spacing in each input file
                                       !(radians)
      DOUBLE PRECISION OUT_PIXSPACE    ! pixel spacing of output map
      DOUBLE PRECISION PIX_DIFF        ! measure of mismatch between pixel
                                       !   spacings of input data sets
      DOUBLE PRECISION DIGNORE         !
      CHARACTER*(80) INFILE            ! name of input file
      CHARACTER*(80) INFILU            ! dto in upper case
      CHARACTER*(80) FILENAME (MAX_FILES) ! names of all input files
      CHARACTER*(10) CENTRE_CRD        ! coord system for input file map centre
      CHARACTER*(10) LOCAL_CRD         ! coord system for input file local
                                       !   offsets
      CHARACTER*(80) DEFAULT           ! default for output filename
      CHARACTER*(32) SYSTEM            ! coord system of output map centre
      CHARACTER*(32) OBJECT (MAX_FILES) ! name of objects in input files
      CHARACTER*(80) OUT_OBJECT        ! name of output object (concatenation
                                       !   of input objects)
      CHARACTER*(64) JCMT_DTA_NAME     ! DTA name of the structure
                                       ! containing the JCMT specific
                                       ! data
      CHARACTER*(64) LST_DTA_NAME      ! DTA name of LST array
      CHARACTER*(128) DTA_NAME         ! temp variable for holding DTA names
      CHARACTER*(128) POS_STRUC_NAME   ! name of structure containing
                                       ! position related parameters
      CHARACTER*(128) PLST_DTA_NAME    ! name of LST array in pointing
                                       ! correction structure
      CHARACTER*(128) PDAZ_DTA_NAME    ! ..array containing AZ pointing
                                       ! correction
      CHARACTER*(128) PDALT_DTA_NAME   ! ..array containing alt pointing
                                       ! correction
      CHARACTER*(30) PROJECTION        ! projection used for pixel coordinates
      CHARACTER*(64) CHAR_ITEMS (2)    ! character array for DSA_SET_AXIS_INFO
      CHARACTER*(80) ERROR             ! DTA error message
      CHARACTER*(128) TEL_STRUC_NAME   ! Name of the telescope structure
      CHARACTER*(20) PROJ              ! projection type
      CHARACTER*(128) ENV_STRUC_NAME   ! DTA name of the environment structure
                                       ! LINEAR, FOURIER
      CHARACTER*(64) STEMP             ! temporary string
      CHARACTER*(64) STEMP1            !         "
      CHARACTER*(1)  SIGN              ! + or -

*  Local Constants:

*.

      CALL PAR_WRUSER ('Renka & Cline rebinning application.',IGNORE)

*  Initial values

      FAULT = .FALSE.
      B1950 = .FALSE.

*  find out if RAs and DECs are to be FK4 1950 or FK5 2000

      CALL PAR_RDKEY ('B1950', .FALSE., B1950)
      IF (PAR_ABORT()) THEN
         FAULT = .TRUE.
         GOTO 500
      END IF

*  Initialise DSA system, find size of variable types

      STATUS = SAI__OK
      CALL DSA_OPEN (STATUS)
      INT_SIZE = DSA_TYPESIZE ('INT', STATUS)
      FLOAT_SIZE = DSA_TYPESIZE ('FLOAT', STATUS)
      DOUBLE_SIZE = DSA_TYPESIZE ('DOUBLE', STATUS)

*  get the input file(s)

      DO FILE = 1, MAX_FILES

*  get the input file name

         IF (FILE .EQ. 1) THEN
            DEFAULT = ' '
         ELSE
            DEFAULT = 'END'
         END IF
         CALL PAR_SDCHAR ('INFILE', DEFAULT, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_RDCHAR ('INFILE', DEFAULT, INFILE)
            IF (PAR_ABORT()) THEN
               FAULT = .TRUE.
               GOTO 500
            END IF
            CALL PAR_CNPAR ('INFILE')
         END IF

*  convert name to upper case, open the file unless the end of the file list
*  has been reached

         INFILU = INFILE
         IGNORE = ICH_FOLD (INFILU)
         IF (INFILU .NE. 'END') THEN

            FILENAME (FILE) = INFILE
            CALL DSA_NAMED_INPUT ('IN', INFILE, STATUS)

*  check that the MORE.JCMT structure is there

            CALL DTA_CRNAM ('IN', 'MORE.JCMT', 0, 0, JCMT_DTA_NAME,
     :         DSTAT)
            CALL DTA_STRUC (JCMT_DTA_NAME, STRUC, DSTAT)
            IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT. STRUC) THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL PAR_WRUSER (
     :               'AE2RD2 - File is not in JCMT format',IGNORE)
               END IF
               FAULT = .TRUE.
               GOTO 500
            END IF

*  check that it doesn't contain a TSDAT structure which would mean that
*  the data is in `time-sorted' form

            CALL DTA_CRNAM (JCMT_DTA_NAME, 'TSDAT', 0, 0, DTA_NAME,
     :         DSTAT)
            CALL DTA_STRUC (DTA_NAME, STRUC, DSTAT)
            IF (DSTAT .EQ. 0) THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL PAR_WRUSER ('AE2RD2 - Input file is in '//
     :               'time-sequence format. Use TS2MAP '//
     :               'before AE2RD2.',IGNORE)
               END IF
               FAULT = .TRUE.
               GOTO 500
            END IF

*  Check the LST values are present

            CALL DTA_CRNAM (JCMT_DTA_NAME, 'LST', 0, 0, DTA_NAME,
     :         DSTAT)
            CALL DTA_STRUC (DTA_NAME, STRUC, DSTAT)
            IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT. STRUC) THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL PAR_WRUSER ('AE2RD2 - JCMT structure does not '//
     :               'contain LST array',IGNORE)
               END IF
               FAULT = .TRUE.
               GOTO 500
            END IF

*  check position structure present

            CALL DTA_CRNAM (JCMT_DTA_NAME, 'MAP', 0, 0,
     :         POS_STRUC_NAME,
     :         DSTAT)
            CALL DTA_STRUC (POS_STRUC_NAME, STRUC, DSTAT)
            IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT. STRUC) THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL PAR_WRUSER ('AE2RD2 - JCMT structure does not '//
     :               'contain positional information',IGNORE)
               END IF
               FAULT = .TRUE.
               GOTO 500
            END IF

*  telescope structure name

            CALL DTA_CRNAM (JCMT_DTA_NAME, 'TEL', 0, 0,
     :         TEL_STRUC_NAME, DSTAT)
            CALL DTA_STRUC (TEL_STRUC_NAME, STRUC, DSTAT)
            IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT. STRUC ) THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL PAR_WRUSER ('AE2RD2 - JCMT structure does not '//
     :               'contain telescope information',IGNORE)
               END IF
               FAULT = .TRUE.
               GOTO 500
            END IF

*  read the object name

            CALL DSA_OBJECT_NAME ('IN', OBJECT(FILE), STATUS)

*  tell DSA want to use magic values with input data, get flag value
*  for reals

            CALL DSA_USE_FLAGGED_VALUES ('IN', STATUS)
            CALL DSA_GET_FLAG_VALUE ('FLOAT', FBAD, STATUS)

*  map the main data array, don't bother with any error arrays because
*  I don't know yet how to propagate the errors through the Renka-Cline
*  algorithm.

            CALL DSA_DATA_SIZE ('IN', 2, NDIM, DIMS, NELM, STATUS)
            CALL DSA_MAP_DATA ('IN', 'READ', 'FLOAT', ADDRESS,
     :         IN_D_SLOT, STATUS)
            INPTR = DYN_ELEMENT(ADDRESS)
            NPIXEL(FILE) = NELM
            NX = DIMS(1)
            NY = DIMS(2)
            IF (NY .LT. 1) NY = 1
            CALL DSA_GET_WORK_ARRAY (NPIXEL(FILE), 'FLOAT', ADDRESS,
     :         SLOT, STATUS)
            DATPTR (FILE) = DYN_ELEMENT(ADDRESS)
            IF (STATUS .EQ. SAI__OK) THEN
               CALL GEN_MOVE (FLOAT_SIZE*NPIXEL(FILE),
     :            DYNAMIC_MEM(INPTR), DYNAMIC_MEM(DATPTR(FILE)))
            END IF

*  map the axes

            CALL DSA_MAP_AXIS_DATA ('IN', 1, 'READ', 'FLOAT', ADDRESS,
     :         IN_A1_SLOT, STATUS)
            XAXPTR = DYN_ELEMENT(ADDRESS)
            CALL DSA_MAP_AXIS_DATA ('IN', 2, 'READ', 'FLOAT', ADDRESS,
     :         IN_A2_SLOT, STATUS)
            YAXPTR = DYN_ELEMENT(ADDRESS)

*  map the LST array

            CALL DTA_CRNAM (JCMT_DTA_NAME, 'LST.DATA_ARRAY', 0, 0,
     :         LST_DTA_NAME, DSTAT)
            CALL DTA_MRVARD (LST_DTA_NAME, NELM, ADDRESS, DSTAT)
            LSTPTR = DYN_ELEMENT(ADDRESS)
            IF (DSTAT .NE. 0) THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL PAR_WRUSER ('AE2RD2 - Error mapping LST array',
     :               IGNORE)
                  STATUS = SAI__ERROR
               END IF
            END IF

*  exit if things have already gone wrong

            IF (STATUS .NE. SAI__OK) THEN
               FAULT = .TRUE.
               GOTO 500
            END IF

*  get the coordinate type and RA, Dec of the centre of the image

            CALL JCMT_GETC (POS_STRUC_NAME, 'CENTRE_CRD', CENTRE_CRD,
     :         STATUS)
            CALL JCMT_GETD (POS_STRUC_NAME, 'EPOCH', EPOCH, STATUS)
            CALL JCMT_GETD (POS_STRUC_NAME, 'RACEN', RAFILE, STATUS)
            CALL JCMT_GETD (POS_STRUC_NAME, 'DECCEN', DECFILE, STATUS)

*  get local coordinate system, angles of axes relative to local north

            CALL JCMT_GETC (POS_STRUC_NAME, 'LOCAL_CRD', LOCAL_CRD,
     :         STATUS)
            CALL JCMT_GETF (POS_STRUC_NAME, 'V2Y', V2Y, STATUS)
            CALL JCMT_GETF (POS_STRUC_NAME, 'X2Y', X2Y, STATUS)

*  data of observation in modified Julian days

            CALL JCMT_GETD (POS_STRUC_NAME, 'MJD_START', MJDSTART,
     :         STATUS)

*  get telescope parameters

            CALL JCMT_GETD (TEL_STRUC_NAME, 'LAT', LAT, STATUS)

*  Get workspace for the list of derived RAs and DECs after the
*  transformation from ALT-AZ

            CALL DSA_GET_WORK_ARRAY (NPIXEL(FILE), 'DOUBLE', ADDRESS,
     :         SLOT, STATUS)
            RAPPTR (FILE) = DYN_ELEMENT(ADDRESS)
            CALL DSA_GET_WORK_ARRAY (NPIXEL(FILE), 'DOUBLE', ADDRESS,
     :         SLOT, STATUS)
            DECPPTR (FILE) = DYN_ELEMENT(ADDRESS)

*  abort here if there's been a problem, confusing if try to carry on

            IF (STATUS .NE. SAI__OK) THEN
               FAULT = .TRUE.
               GOTO 500
            END IF

*  calculate the transformed list of ra and dec for each pixel. This routine
*  will output results in B1950 or J2000 coords according to the value
*  of B1950.

            IF (STATUS .EQ. SAI__OK) THEN
               CALL JCMT_OFFSET2RADEC (CENTRE_CRD, EPOCH, RAFILE,
     :            DECFILE, LOCAL_CRD, V2Y, X2Y, MJDSTART, LAT,
     :            NX, NY, DYNAMIC_MEM(XAXPTR), DYNAMIC_MEM(YAXPTR),
     :            DYNAMIC_MEM(LSTPTR), B1950,
     :            DYNAMIC_MEM(RAPPTR(FILE)), DYNAMIC_MEM(DECPPTR(FILE)),
     :            STATUS)
            ENDIF

*  also transform the map centre to B1950 if required

            IF (STATUS .EQ. SAI__OK) THEN
               IF (B1950) THEN
                  IF (CENTRE_CRD .EQ. 'RB') THEN
                     CALL SLA_PRECES ('FK4', EPOCH, 1950.0D0,
     :                  RAFILE, DECFILE)
                  ELSE IF (CENTRE_CRD .EQ. 'RJ') THEN
                     MJDTEMP = SLA_EPB2D (EPOCH)
                     JEPOCH = SLA_EPJ (MJDTEMP)
                     CALL SLA_PRECES ('FK5', JEPOCH, 2000.0D0,
     :                  RAFILE, DECFILE)
                     CALL SLA_FK54Z (RAFILE, DECFILE, 1950.0D0,
     :                  RAFILE, DECFILE, DIGNORE, DIGNORE)
                  END IF
               ELSE
                  IF (CENTRE_CRD .EQ. 'RB') THEN
                     CALL SLA_PRECES ('FK4', EPOCH, 1950.0D0,
     :                  RAFILE, DECFILE)
                     CALL SLA_FK45Z (RAFILE, DECFILE, 1950.0D0,
     :                  RAFILE, DECFILE)
                  ELSE IF (CENTRE_CRD .EQ. 'RJ') THEN
                     MJDTEMP = SLA_EPB2D (EPOCH)
                     JEPOCH = SLA_EPJ (MJDTEMP)
                     CALL SLA_PRECES ('FK5', JEPOCH, 2000.0D0,
     :                  RAFILE, DECFILE)
                  END IF
               END IF
            END IF

*  search for pointing correction structure in file, and map the
*  arrays if present

            POINTING_CORRECTION = .FALSE.
            IF (STATUS .EQ. SAI__OK) THEN
               CALL DTA_CRNAM (JCMT_DTA_NAME, 'PCORR.LST', 0, 0,
     :            PLST_DTA_NAME, DSTAT)
               CALL DTA_SZVAR (PLST_DTA_NAME, 1, NDIM, NCORR, DSTAT)
               CALL DTA_MRVARD (PLST_DTA_NAME, NCORR, ADDRESS, DSTAT)
               IF ((DSTAT.EQ.0) .AND. (NCORR.GE.1)) THEN
                  POINT_LSTPTR = DYN_ELEMENT (ADDRESS)
                  CALL DTA_CRNAM (JCMT_DTA_NAME, 'PCORR.D_AZ',
     :               0, 0, PDAZ_DTA_NAME, DSTAT)
                  CALL DTA_MRVARF (PDAZ_DTA_NAME, NCORR, ADDRESS, DSTAT)
                  IF (DSTAT .NE. 0) THEN
                     CALL PAR_WRUSER ('AE2RD2 - error reading D_AZ '//
     :                  'array of pointing correction',IGNORE)
                  ELSE
                     POINT_DAZPTR = DYN_ELEMENT (ADDRESS)
                     CALL DTA_CRNAM (JCMT_DTA_NAME, 'PCORR.D_ALT',
     :                  0, 0, PDALT_DTA_NAME, DSTAT)
                     CALL DTA_MRVARF (PDALT_DTA_NAME, NCORR, ADDRESS,
     :                  DSTAT)
                     IF (DSTAT .NE. 0) THEN
                        CALL PAR_WRUSER ('AE2RD2 - error reading '//
     :                     'D_ALT array of pointing correction',IGNORE)
                     ELSE
                        POINT_DALTPTR = DYN_ELEMENT (ADDRESS)
                        POINTING_CORRECTION = .TRUE.
                     END IF
                  END IF
               END IF
            END IF

*  correct the pointing if required

            IF (.NOT. POINTING_CORRECTION) THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL PAR_WRUSER ('AE2RD2 - No pointing corrections '//
     :               'applied',IGNORE)
               END IF
            ELSE
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL PAR_WRUSER ('AE2RD2 - Applying pointing '//
     :               'corrections',IGNORE)
                  CALL JCMT_CORRECT_POINTING (NPIXEL(FILE),
     :               DYNAMIC_MEM (RAPPTR(FILE)),
     :               DYNAMIC_MEM (DECPPTR(FILE)),
     :               DYNAMIC_MEM (LSTPTR),
     :               LAT, NCORR,
     :               DYNAMIC_MEM (POINT_LSTPTR),
     :               DYNAMIC_MEM (POINT_DAZPTR),
     :               DYNAMIC_MEM (POINT_DALTPTR),
     :               STATUS)
               END IF
            END IF

*  make sure that all the RAs are in the range 0-2pi

            IF (STATUS .EQ. SAI__OK) THEN
               DO I = 1, NPIXEL (FILE)
                  CALL SLA_DRANRM (DYNAMIC_MEM(RAPPTR(FILE)))
               END DO
            END IF

*  make centre of first input file be centre of output data set

            IF (FILE .EQ. 1) THEN
               RACEN = RAFILE
               DECCEN = DECFILE
               IF (B1950) THEN
                  SYSTEM = 'FK4 B1950.0'
               ELSE
                  SYSTEM = 'FK5 J2000.0'
               END IF
            END IF

*  get unit cell sizes in the original data and set output cell
*  size to be the smaller (in radians)

            DSTAT = 0
            CALL JCMT_GETD (POS_STRUC_NAME, 'CELL_X', CELLX, STATUS)
            CALL JCMT_GETD (POS_STRUC_NAME, 'CELL_Y', CELLY, STATUS)
            PIXSPACE (FILE) = MIN (ABS(CELLX),ABS(CELLY)) * DAS2R

*  the first file will be given a weight of 1 in the resample operation,
*  subsequent files will be weighted relative to the first

            IF (FILE .EQ. 1) THEN
               WEIGHT(1) = 1.0
            ELSE
               CALL PAR_SDVAL ('WEIGHT', 1.0, STATUS)
               CALL PAR_RDVAL ('WEIGHT', 0.0, 100.0, 1.0, ' ',
     :            WEIGHT(FILE))
               IF (PAR_ABORT()) THEN
                  FAULT = .TRUE.
                  GOTO 500
               END IF
               CALL PAR_CNPAR ('WEIGHT')
            END IF

*  unmap the pointing correction arrays

            CALL DTA_FRVAR (PLST_DTA_NAME, DSTAT)
            CALL DTA_FRVAR (PDAZ_DTA_NAME, DSTAT)
            CALL DTA_FRVAR (PDALT_DTA_NAME, DSTAT)
            DSTAT = 0

*  unmap the LST array and data arrays for this file, then close the file

            CALL DSA_POST_PROCESS_FLAGGED_VALUES ('IN', STATUS)
            CALL DTA_FRVAR (LST_DTA_NAME, DSTAT)
            CALL DSA_UNMAP (IN_D_SLOT, STATUS)
            CALL DSA_UNMAP (IN_A1_SLOT, STATUS)
            CALL DSA_UNMAP (IN_A2_SLOT, STATUS)
            CALL DSA_CLOSE_STRUCTURE ('IN', STATUS)

*  trap fault

            IF (STATUS .NE. SAI__OK) THEN
               FAULT = .TRUE.
               GOTO 500
            END IF

         ELSE

*  have finished input sequence, leave loop

            N_FILES = FILE - 1
            GOTO 100

         END IF

      END DO


*  check that there is some input data

 100  IF (N_FILES .LT. 1) THEN
         CALL PAR_WRUSER ('AE2RD2 - There is no input data',IGNORE)
         FAULT = .TRUE.
         GOTO 500
      END IF

*  set default RA and Dec of output centre, prompt user to specify
*  centre

      IF (STATUS .EQ. SAI__OK) THEN

         IF (B1950) THEN
            CALL PAR_WRUSER ('Coordinates are FK4 B1950.0',IGNORE)
         ELSE
            CALL PAR_WRUSER ('Coordinates are FK5 J2000.0',IGNORE)
         END IF

         CALL SLA_DR2TF (2, RACEN, SIGN, HMSF)
         STEMP = SIGN
         WRITE (STEMP(2:3),'(I2.2)') HMSF(1)
         STEMP (4:4) = ' '
         WRITE (STEMP(5:6),'(I2.2)') HMSF(2)
         STEMP (7:7) = ' '
         WRITE (STEMP(8:9),'(I2.2)') HMSF(3)
         STEMP (10:10) = '.'
         WRITE (STEMP(11:12),'(I2.2)') HMSF(4)
         CALL PAR_SDCHAR ('RA_CENTRE', STEMP, STATUS)
         CALL PAR_RDCHAR ('RA_CENTRE', STEMP, STEMP1)
         IF (PAR_ABORT()) THEN
            FAULT = .TRUE.
            GOTO 500
         END IF
         NSTRT = 1
         CALL SLA_DAFIN (STEMP1, NSTRT, RACEN, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('AE2RD2 - Error reading centre RA, '//
     :         'input must be in 5 45 36 type format.',IGNORE)
            FAULT = .TRUE.
            GOTO 500
         END IF
         RACEN = 15.0D0 * RACEN

         CALL SLA_DR2AF (1, DECCEN, SIGN, HMSF)
         STEMP = SIGN
         WRITE (STEMP(2:4),'(I3.3)') HMSF(1)
         STEMP (5:5) = ' '
         WRITE (STEMP(6:7),'(I2.2)') HMSF(2)
         STEMP (8:8) = ' '
         WRITE (STEMP(9:10),'(I2.2)') HMSF(3)
         STEMP (11:11) = '.'
         WRITE (STEMP(12:12),'(I1.1)') HMSF(4)
         CALL PAR_SDCHAR ('DEC_CENTRE', STEMP, STATUS)
         CALL PAR_RDCHAR ('DEC_CENTRE', STEMP, STEMP1)
         IF (PAR_ABORT()) THEN
            FAULT = .TRUE.
            GOTO 500
         END IF
         NSTRT = 1
         CALL SLA_DAFIN (STEMP1, NSTRT, DECCEN, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('AE2RD2 - Error reading centre Dec, '//
     :         'input must be in 5 10 20.66 or -35 56 67 format.',
     :         IGNORE)
            FAULT = .TRUE.
            GOTO 500
         END IF

      END IF

*  make the pixel spacing of the output map the minimum of that of the input
*  data, and issue a warning if the input pixel sizes differ by more than
*  10 per cent.

      OUT_PIXSPACE = 100000.0
      PIX_DIFF = 0.0
      DO FILE = 1, N_FILES
         IF (FILE .NE. 1) THEN
            PIX_DIFF = MAX (PIX_DIFF,
     :         ABS((OUT_PIXSPACE - PIXSPACE(FILE)) / OUT_PIXSPACE))
         END IF
         OUT_PIXSPACE = MIN (OUT_PIXSPACE, PIXSPACE(FILE))
      END DO

      IF (PIX_DIFF .GT. 0.1) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('Warning - pixel spacings of input '//
     :         'datasets differ by more than 10 per cent.',IGNORE)
         END IF
      END IF

*  find the extent of the input data

      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_RANGED (DYNAMIC_MEM(RAPPTR(1)), 1, NPIXEL(1),
     :      RAMAX, RAMIN)
         CALL JCMT_RANGED (DYNAMIC_MEM(DECPPTR(1)), 1, NPIXEL(1),
     :      DECMAX, DECMIN)
         IF (N_FILES .GT. 1) THEN
            DO FILE = 2, N_FILES
               CALL JCMT_RANGED (DYNAMIC_MEM(RAPPTR(FILE)), 1,
     :            NPIXEL(FILE), TMAX, TMIN)
               RAMAX = MAX (RAMAX, TMAX)
               RAMIN = MIN (RAMIN, TMIN)
               CALL JCMT_RANGED (DYNAMIC_MEM(DECPPTR(FILE)), 1,
     :            NPIXEL(FILE), TMAX, TMIN)
               DECMAX = MAX (DECMAX, TMAX)
               DECMIN = MIN (DECMIN, TMIN)
            END DO
         END IF
      END IF

*  calculate the size and centre pixel of the output array, RA increasing
*  to left (lower pixel x index). Correct RA size for cos(dec) effect.
*  Watch for maps straddling the flip from 2pi to 0 in RA.

      RA_CORR = MAX (COS(DECMIN),COS(DECMAX))
      DTEMP = RAMAX - RAMIN
      IF (DTEMP .GT. DPI) THEN
         DTEMP = 2.0D0 * DPI - DTEMP
      END IF
      NXOUT = NINT (DTEMP*RA_CORR/OUT_PIXSPACE) + 2
      NYOUT = NINT ((DECMAX-DECMIN)/OUT_PIXSPACE) + 2
      DTEMP = RAMAX - RACEN
      IF (DTEMP .GT. DPI) THEN
         DTEMP = 2.0D0 * DPI - DTEMP
      END IF
      ICEN = NINT (DTEMP*RA_CORR/OUT_PIXSPACE) + 2
      JCEN = NINT ((DECCEN-DECMIN)/OUT_PIXSPACE) + 2

*  error if output map is too big

      IF ((NXOUT .GT. 1000) .OR. (NYOUT .GT. 1000)) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('AE2RD2 - Output map is too big, it has '//
     :         'one or both dimensions greater than 1000 pixels.',
     :         IGNORE)
            FAULT = .TRUE.
            GOTO 500
         END IF
      END IF

*  create empty output structure, create data arrays

      CALL PAR_SDCHAR ('OUTPUT', ' ', STATUS)
      CALL DSA_OUTPUT ('OUT', 'OUTPUT', ' ', 1, 1, STATUS)
      NDIM = 2
      DIMS(1) = NXOUT
      DIMS(2) = NYOUT
      CALL DSA_COERCE_DATA_ARRAY ('OUT', 'FLOAT', NDIM, DIMS, STATUS)
      CALL DSA_COERCE_AXIS_DATA ('OUT', 1, 'FLOAT', 1, NXOUT, STATUS)
      CALL DSA_COERCE_AXIS_DATA ('OUT', 2, 'FLOAT', 1, NYOUT, STATUS)

*  tell DSA want to use flagged values in output structure

      CALL DSA_SET_FLAGGED_VALUES ('OUT', .TRUE., STATUS)
      CALL DSA_USE_FLAGGED_VALUES ('OUT', STATUS)

*  map the output data and array, set to bad values

      CALL DSA_MAP_DATA ('OUT', 'UPDATE', 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      OUT_D_PTR = DYN_ELEMENT(ADDRESS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL GEN_CFILL (1, NXOUT*NYOUT, FBAD, DYNAMIC_MEM(OUT_D_PTR))
      END IF

*  get workspace for a scratch copy of the output data array, and for an
*  array holding the coadded weights, set them both to FBAD

      CALL DSA_GET_WORK_ARRAY (NXOUT*NYOUT, 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      OUTCOPY_PTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORK_ARRAY (NXOUT*NYOUT, 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      OUTWEIGHT_PTR = DYN_ELEMENT (ADDRESS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL GEN_CFILL (1, NXOUT*NYOUT, FBAD, DYNAMIC_MEM(OUTCOPY_PTR))
         CALL GEN_CFILL (1, NXOUT*NYOUT, FBAD,
     :      DYNAMIC_MEM(OUTWEIGHT_PTR))
      END IF

*  get workspace for scratch arrays (make them big enough to handle the
*  largest input data set)

      MAX_PIXEL = 0
      DO FILE = 1, N_FILES
         MAX_PIXEL = MAX (MAX_PIXEL, NPIXEL(FILE))
      END DO

      CALL DSA_GET_WORK_ARRAY (MAX_PIXEL, 'DOUBLE', ADDRESS, SLOT,
     :   STATUS)
      INCOPY = DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORK_ARRAY (MAX_PIXEL, 'DOUBLE', ADDRESS, SLOT,
     :   STATUS)
      RACOPY = DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORK_ARRAY (MAX_PIXEL, 'DOUBLE', ADDRESS, SLOT,
     :   STATUS)
      DECCOPY = DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORK_ARRAY (7*MAX_PIXEL, 'INT', ADDRESS, SLOT,
     :   STATUS)
      TRIANG = DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORK_ARRAY (2*MAX_PIXEL, 'DOUBLE', ADDRESS, SLOT,
     :         STATUS)
      GRADS = DYN_ELEMENT(ADDRESS)

*  now work through input datasets, regridding them and coadding them
*  into the output map

      IF (STATUS .EQ. SAI__OK) THEN

         DO FILE = 1, N_FILES

            CALL JCMT_RENKACLINE_REGRID (NPIXEL(FILE),
     :         DYNAMIC_MEM (DATPTR(FILE)),
     :         FBAD, DYNAMIC_MEM (RAPPTR(FILE)),
     :         DYNAMIC_MEM(DECPPTR(FILE)),
     :         DYNAMIC_MEM(INCOPY), DYNAMIC_MEM(RACOPY),
     :         DYNAMIC_MEM(DECCOPY), DYNAMIC_MEM(TRIANG),
     :         DYNAMIC_MEM(GRADS), NXOUT, NYOUT, ICEN, JCEN,
     :         -OUT_PIXSPACE, OUT_PIXSPACE, RACEN, DECCEN,
     :         DYNAMIC_MEM(OUTCOPY_PTR), STATUS)

            IF (STATUS .NE. SAI__OK) THEN
               FAULT = .TRUE.
               GOTO 500
            END IF

            CALL JCMT_COADD (NXOUT*NYOUT, DYNAMIC_MEM(OUTCOPY_PTR),
     :         WEIGHT(FILE), FBAD, DYNAMIC_MEM(OUT_D_PTR),
     :         DYNAMIC_MEM(OUTWEIGHT_PTR), STATUS)

         END DO

      END IF

*  set up the axis data...
*     the x axis

      CALL DSA_MAP_AXIS_DATA ('OUT', 1, 'WRITE', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      OXAXPTR = DYN_ELEMENT(ADDRESS)
      IF (STATUS .EQ. SAI__OK) THEN

*     fill with array indices

         CALL GEN_NFILLF (NXOUT, DYNAMIC_MEM(OXAXPTR))

*     subtract the index of the map centre pixel

         CALL GEN_ADDCAF (DYNAMIC_MEM(OXAXPTR), NXOUT, REAL(-ICEN),
     :      DYNAMIC_MEM(OXAXPTR))

*     and multiply by minus the pixel size to give an offset in arcsec
*     +ve in increasing RA

         CALL GEN_MULCAF (DYNAMIC_MEM(OXAXPTR), NXOUT,
     :      REAL(-OUT_PIXSPACE*DR2AS), DYNAMIC_MEM(OXAXPTR))

      END IF

      CHAR_ITEMS(1) = 'ARCSEC'
      CHAR_ITEMS(2) = 'RA'
      CALL DSA_SET_AXIS_INFO ('OUT', 1, 2, CHAR_ITEMS, 0, 0, STATUS)

*     same for the y axis

      CALL DSA_MAP_AXIS_DATA ('OUT', 2, 'WRITE', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      OYAXPTR = DYN_ELEMENT(ADDRESS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL GEN_NFILLF (NYOUT, DYNAMIC_MEM(OYAXPTR))
         CALL GEN_ADDCAF (DYNAMIC_MEM(OYAXPTR), NYOUT, REAL(-JCEN),
     :      DYNAMIC_MEM(OYAXPTR))
         CALL GEN_MULCAF (DYNAMIC_MEM(OYAXPTR), NYOUT,
     :      REAL(OUT_PIXSPACE*DR2AS), DYNAMIC_MEM(OYAXPTR))
      END IF
      CHAR_ITEMS(1) = 'ARCSEC'
      CHAR_ITEMS(2) = 'DEC'
      CALL DSA_SET_AXIS_INFO ('OUT', 2, 2, CHAR_ITEMS, 0, 0, STATUS)

*  set the object name to be combination of the input names

      OUT_OBJECT = OBJECT(1)(:ICH_LEN(OBJECT(1)))
      IF (N_FILES .GT. 1) THEN
         DO FILE = 2, N_FILES
            OUT_OBJECT = OUT_OBJECT(:ICH_LEN(OUT_OBJECT))//
     :         '+'//OBJECT(FILE)(:ICH_LEN(OBJECT(FILE)))
         END DO
      END IF
      CALL DSA_SET_OBJECT ('OUT', OUT_OBJECT, STATUS)

*  create a `.MORE.JCMT_COORDS' structure to hold the RA, Dec and coordinate
*  system in a manner more accessible to the IRASTAG application than the
*  FITS structure is

      IF (STATUS .EQ. SAI__OK) THEN
         CALL DTA_CRVAR ('OUT.MORE', 'EXT', DSTAT)
         CALL DTA_CRVAR ('OUT.MORE.JCMT_COORDS', 'EXT_JCMT', DSTAT)

*  store RA, Dec and coordinate system

         CALL DTA_CRVAR ('OUT.MORE.JCMT_COORDS.RACEN', 'DOUBLE', DSTAT)
         CALL DTA_WRVARD ('OUT.MORE.JCMT_COORDS.RACEN', 1, RACEN,
     :      DSTAT)

         CALL DTA_CRVAR ('OUT.MORE.JCMT_COORDS.DECCEN', 'DOUBLE',
     :      DSTAT)
         CALL DTA_WRVARD ('OUT.MORE.JCMT_COORDS.DECCEN', 1, DECCEN,
     :      DSTAT)

         CALL DTA_CRNAM ('OUT.MORE.JCMT_COORDS', 'SYSTEM', 1, 64,
     :      DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_WRVARC ('OUT.MORE.JCMT_COORDS.SYSTEM',
     :      ICH_LEN(SYSTEM), SYSTEM, DSTAT)

*  pixel coords of centre and pixel size in radians

         CALL DTA_CRVAR ('OUT.MORE.JCMT_COORDS.ICEN', 'INT', DSTAT)
         CALL DTA_WRVARI ('OUT.MORE.JCMT_COORDS.ICEN', 1, ICEN,
     :      DSTAT)

         CALL DTA_CRVAR ('OUT.MORE.JCMT_COORDS.JCEN', 'INT', DSTAT)
         CALL DTA_WRVARI ('OUT.MORE.JCMT_COORDS.JCEN', 1, JCEN,
     :      DSTAT)

         CALL DTA_CRVAR ('OUT.MORE.JCMT_COORDS.PIXSIZE', 'DOUBLE',
     :      DSTAT)
         CALL DTA_WRVARD ('OUT.MORE.JCMT_COORDS.PIXSIZE', 1,
     :      OUT_PIXSPACE, DSTAT)
      END IF

*  store the names of the files as FITS items

      DO FILE = 1, N_FILES
         STEMP = 'FILE_'
         IF (FILE .LT. 10) THEN
            WRITE (STEMP(6:6),'(I1.1)') FILE
         ELSE
            WRITE (STEMP(6:7),'(I2.2)') FILE
         END IF
         CALL DSA_PUT_FITS_C ('OUT', STEMP(:8), FILENAME(FILE),
     :      'Name of input file whose data has been included in '//
     :      'the result', STATUS)
      END DO

*  and their weights

      DO FILE = 1, N_FILES
         STEMP = 'WT_'
         IF (FILE .LT. 10) THEN
            WRITE (STEMP(4:4),'(I1.1)') FILE
         ELSE
            WRITE (STEMP(4:5),'(I2.2)') FILE
         END IF
         CALL DSA_PUT_FITS_F ('OUT', STEMP(:8), WEIGHT(FILE),
     :      'Weight assigned to input dataset', STATUS)
      END DO

*  store centre RA and Dec in radians

      CALL DSA_PUT_FITS_D ('OUT', 'RA_RAD', RACEN, 'RA of output map '//
     :   'centre (radians)', STATUS)
      CALL DSA_PUT_FITS_D ('OUT', 'DEC_RAD', DECCEN, 'Dec of output '//
     :   'map centre (radians)', STATUS)

*  ...and converted

      CALL SLA_DR2TF (2, RACEN, SIGN, HMSF)
      STEMP = SIGN
      WRITE (STEMP(2:3),'(I2.2)') HMSF(1)
      STEMP (4:4) = ' '
      WRITE (STEMP(5:6),'(I2.2)') HMSF(2)
      STEMP (7:7) = ' '
      WRITE (STEMP(8:9),'(I2.2)') HMSF(3)
      STEMP (10:10) = '.'
      WRITE (STEMP(11:12),'(I2.2)') HMSF(4)

      CALL DSA_PUT_FITS_C ('OUT', 'RA', STEMP, 'RA of centre of '//
     :   'output map', STATUS)

      CALL SLA_DR2AF (1, DECCEN, SIGN, HMSF)
      STEMP = SIGN
      WRITE (STEMP(2:4),'(I3.3)') HMSF(1)
      STEMP (5:5) = ' '
      WRITE (STEMP(6:7),'(I2.2)') HMSF(2)
      STEMP (8:8) = ' '
      WRITE (STEMP(9:10),'(I2.2)') HMSF(3)
      STEMP (11:11) = '.'
      WRITE (STEMP(12:12),'(I1.1)') HMSF(4)

      CALL DSA_PUT_FITS_C ('OUT', 'DEC', STEMP, 'Dec of centre '//
     :   'of output map', STATUS)

*  the Coordinate system

      CALL DSA_PUT_FITS_C ('OUT', 'SYSTEM', SYSTEM, 'Coordinate '//
     :   'system and epoch of map', STATUS)

*  the method of combining used

      CALL DSA_PUT_FITS_C ('OUT', 'METHOD', 'RENKA & CLINE '//
     :   'interpolation', 'Method of rebinning used', STATUS)


*  tidy up

 500  IF (DSTAT .NE. 0) THEN
         CALL DTA_ERROR (DSTAT, ERROR)
         CALL PAR_WRUSER (ERROR(:ICH_LEN(ERROR)),IGNORE)
         FAULT=.TRUE.
      END IF
      CALL DSA_CLOSE (STATUS)
      IF (FAULT) CALL FIG_SETERR

      END

