      SUBROUTINE MAP2MEM
*+
*  Name:
*     MAP2MEM

*  Purpose:
*     Convert JCMT map files to a format readable by DBMEM programmes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAP2MEM

*  Description:

*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: J.Lightfoot
*     {enter_new_authors_here}

*  History:
*      1-JUN-1992: Original version
*     22-OCT-1993: file access changed to FIO (REVAD::JFL)
*     24-MAR-1994 (REVAD::HME): Change include statements. Disuse VMS
*        routines TIME and DATE, use PSX_* instead.
*     13-JUN-1994 (REVAD::HME): Use lower case for extensions .dbh,
*        .bin, .mem.

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
      LOGICAL PAR_ABORT                ! test PAR abort flag
      INTEGER DYN_ELEMENT              ! DSA dynamic memory
      INTEGER DYN_INCREMENT            !        "
      INTEGER DSA_TYPESIZE             ! `bytes per variable' function
      INTEGER ICH_LEN                  ! Figaro string length
      INTEGER ICH_FOLD                 ! Figaro conversion to upper case
      DOUBLE PRECISION SLA_EPB2D       ! Besselian epoch to MJD
      DOUBLE PRECISION SLA_EPJ         ! MJD to Julian epoch

*  Status:
      INTEGER STATUS                   ! Global status for DSA routines
      INTEGER DSTAT                    ! status for dta routines

*  Local Constants:
      INTEGER MAX_DIM                  ! max no of dimensions of array
      PARAMETER (MAX_DIM = 10)

*  Local Variables:
      LOGICAL B1950                    ! flag to indicate that the coordinates
                                       ! are to be Besselian 1950 equinox
      LOGICAL FAULT                    ! logical to flag a Figaro fault
      LOGICAL STRUC                    ! logical for testing structure
                                       ! existence
      LOGICAL POINTING_CORRECTION      ! T if a pointing correction is to be
                                       ! performed
      LOGICAL ERRORS                   ! T if error array is present
      LOGICAL BINARY                   ! T if output is to unformatted file
      INTEGER INT_SIZE                 ! number of bytes in integer
      INTEGER FLOAT_SIZE               !    "      "     "  real
      INTEGER DOUBLE_SIZE              !    "      "     "  double precision
      INTEGER NDIM                     ! number of dimensions of array
      INTEGER DIMS (MAX_DIM)           ! size of each dimension
      INTEGER NX                       ! x dimension of input array
      INTEGER NY                       ! y  "       "      "
      INTEGER SLOT                     ! DSA slot number
      INTEGER ADDRESS                  ! DSA address
      INTEGER INPTR                    ! DSA pointer for input data array
      INTEGER INEPTR                   !    "       "      "   error array
      INTEGER XAXPTR                   !    "       "      "   x-axis
      INTEGER YAXPTR                   !    "       "      "
      INTEGER NPIXEL                   ! number of pixels in input file
      INTEGER LSTPTR                   !  "     "      " input LST array
      INTEGER NCORR                    ! number of points in pointing correction
                                       !   description
      INTEGER POINT_LSTPTR             ! DSA pointer to LST array in pointing
                                       !   correction description
      INTEGER POINT_DAZPTR             ! DSA pointer to az correction
      INTEGER POINT_DALTPTR            ! DSA pointer to alt correction
      INTEGER RAPOS_PTR                ! DSA pointer to RA of +ve beam
      INTEGER DECPOS_PTR               !    "      "    dec
      INTEGER RANEG_PTR                !    "      "    RA of negative beam
      INTEGER DECNEG_PTR               !    "      "    dec
      INTEGER RAPOS_OFF_PTR            !    "      "    RA offset of +ve beam
      INTEGER DECPOS_OFF_PTR           !    "      "    dec
      INTEGER RANEG_OFF_PTR            !    "      "    RA offset of -ve beam
      INTEGER DECNEG_OFF_PTR           !    "      "    dec
      INTEGER ETA_PTR                  !    "      "    parallactic angle
      INTEGER HMSF (4)                 ! holds converted RA and Dec output
                                       !   from SLA routine
      INTEGER NSTRT                    ! needed by SLA string decoding routines
      INTEGER ITEMP                    !
      INTEGER IGNORE                   ! unimportant status
      INTEGER FD                       ! FIO file descriptor
      INTEGER LU                       ! logical unit of output file
      INTEGER NTICKS                   ! returned by PSX_TIME
      REAL FBAD                        ! value to flag bad pixel
      REAL V2Y                         ! angle between vertical of local coords
                                       !   and y offset axis (anti-clockwise,
                                       !   radians)
      REAL X2Y                         ! angle between x and y offset axes
                                       !   (anti-clockwise, radians)
      REAL CHOP_THROW                  ! chopper throw (arcsec)
      REAL CHOP_PA                     ! chopper position angle (radians)
      REAL POS_X                       ! x offset of +ve beam in chopper coords
      REAL POS_Y                       ! y offset
      REAL NEG_X                       ! x offset of -ve beam
      REAL NEG_Y                       ! y offset
      REAL RTEMP                       !
      DOUBLE PRECISION EPOCH           ! Besselian epoch of input centre coords
      DOUBLE PRECISION JEPOCH          ! Julian epoch of input centre coords
      DOUBLE PRECISION RACEN           ! RA of map centre of output values
      DOUBLE PRECISION DECCEN          ! Dec
      DOUBLE PRECISION MJDSTART        ! MJD of start of observations
      DOUBLE PRECISION MJDTEMP         !
      DOUBLE PRECISION LAT             ! Latitude of observatory (radians)
      DOUBLE PRECISION DIGNORE         !
      CHARACTER*(80) INFILE            ! Name of input file
      CHARACTER*(80) OUTFILE           ! Root name of output files
      CHARACTER*(10) CENTRE_CRD        ! coord system for input file map centre
      CHARACTER*(10) LOCAL_CRD         ! coord system for input file local
                                       !   offsets
      CHARACTER*(10) CHOP_CRD          ! coord system of chopper
      CHARACTER*(3)  COORD_SYSTEM      ! coord system of RA,decs at an
                                       !   intermediate stage
      CHARACTER*(1)  TEL_BEAM          ! Beam  at which telescope was pointing
      CHARACTER*(1)  POS_BEAM          ! Name of positive beam
      CHARACTER*(64) JCMT_DTA_NAME     ! DTA name of the structure
                                       ! containing the JCMT specific
                                       ! data
      CHARACTER*(64) LST_DTA_NAME      ! DTA name of LST array
      CHARACTER*(128) DTA_NAME         ! temp variable for holding DTA names
      CHARACTER*(128) POS_STRUC_NAME   ! name of structure containing
                                       ! position related parameters
      CHARACTER*(128) PLST_DTA_NAME    ! name of LST array in pointing
                                       ! correction structure
      CHARACTER*(128) PDAZ_DTA_NAME    ! .. array containing az pointing
                                       !    correction
      CHARACTER*(128) PDALT_DTA_NAME   ! .. array containing alt pointing
                                       !    correction
      CHARACTER*(80) ERROR             ! DTA error message
      CHARACTER*(128) TEL_STRUC_NAME   ! Name of the telescope structure
      CHARACTER*(128) ENV_STRUC_NAME   ! DTA name of the environment structure
      CHARACTER*(64) STEMP             ! temporary string
      CHARACTER*(64) STEMP1            !         "
      CHARACTER*(1)  SIGN              ! + or -
      CHARACTER*(24) NOW               ! time of run
      CHARACTER*(80) BUFFER            ! character buffer for FIO output
      CHARACTER*(20) SYSTEM            ! coordinate system of output data
      CHARACTER*(3)  EXTENSION         ! extension of output data file

*  Local Constants:

*.

*  Initial values

      FAULT = .FALSE.
      B1950 = .FALSE.

*  find out if RAs and DECs are to be transformed to 1950

      CALL PAR_RDKEY ('B1950', .FALSE., B1950)
      IF (PAR_ABORT()) THEN
         FAULT = .TRUE.
         GOTO 500
      END IF

*  Initialise DSA system, find size of variable types, bad values

      STATUS = SAI__OK
      CALL DSA_OPEN (STATUS)
      INT_SIZE = DSA_TYPESIZE ('INT', STATUS)
      FLOAT_SIZE = DSA_TYPESIZE ('FLOAT', STATUS)
      DOUBLE_SIZE = DSA_TYPESIZE ('DOUBLE', STATUS)
      CALL DSA_GET_FLAG_VALUE ('FLOAT', FBAD, STATUS)

*  get the input file name

      IF (STATUS .EQ. SAI__OK) THEN
         CALL PAR_RDCHAR ('INPUT', ' ', INFILE)
         IF (PAR_ABORT()) THEN
            FAULT = .TRUE.
            GOTO 500
         END IF
      END IF

*  open the input file

      CALL DSA_NAMED_INPUT ('IN', INFILE, STATUS)

*  check that the MORE.JCMT structure is there

      CALL DTA_CRNAM ('IN', 'MORE.JCMT', 0, 0, JCMT_DTA_NAME,
     :   DSTAT)
      CALL DTA_STRUC (JCMT_DTA_NAME, STRUC, DSTAT)
      IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT. STRUC) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('MAP2MEM - File is not in JCMT format',
     :        IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  check that it doesn't contain a TSDAT structure which would mean that
*  the data is in `time-sorted' form

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'TSDAT', 0, 0, DTA_NAME,
     :   DSTAT)
      CALL DTA_STRUC (DTA_NAME, STRUC, DSTAT)
      IF (DSTAT .EQ. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('MAP2MEM - Input file is in '//
     :        'time-sequence format. Use TS2MAP before MAP2MEM.',
     :        IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  Check the LST values are present

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'LST', 0, 0, DTA_NAME,
     :   DSTAT)
      CALL DTA_STRUC (DTA_NAME, STRUC, DSTAT)
      IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT. STRUC) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('MAP2MEM - JCMT structure does not '//
     :        'contain LST array ', IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  check position structure present

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'MAP', 0, 0,
     :   POS_STRUC_NAME, DSTAT)
      CALL DTA_STRUC (POS_STRUC_NAME, STRUC, DSTAT)
      IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT. STRUC) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('MAP2MEM - JCMT structure does '//
     :        'not contain positional information', IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  telescope structure name

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'TEL', 0, 0,
     :   TEL_STRUC_NAME, DSTAT)
      CALL DTA_STRUC (TEL_STRUC_NAME, STRUC, DSTAT)
      IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT. STRUC ) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('MAP2MEM - JCMT structure does '//
     :        'not contain telescope information', IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  tell DSA want to use magic values with input data

      CALL DSA_USE_FLAGGED_VALUES ('IN', STATUS)

*  map the main data array, and error array if present. If there is no
*  error array then map a work array and set it to the user's estimate
*  of the error.

      CALL DSA_DATA_SIZE ('IN', 2, NDIM, DIMS, NPIXEL, STATUS)
      CALL DSA_MAP_DATA ('IN', 'READ', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      INPTR = DYN_ELEMENT(ADDRESS)
      NX = DIMS(1)
      NY = DIMS(2)
      IF (NY .LT. 1) NY = 1
      CALL DSA_SEEK_ERRORS ('IN', ERRORS, STATUS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('IN', 'READ', 'FLOAT', ADDRESS,
     :      SLOT, STATUS)
         INEPTR = DYN_ELEMENT (ADDRESS)
      ELSE
         CALL DSA_GET_WORK_ARRAY (NPIXEL, 'FLOAT', ADDRESS, SLOT,STATUS)
         INEPTR = DYN_ELEMENT (ADDRESS)
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_RDVAL ('NOISE', 0.0, 1.0E37, 1.0, ' ', RTEMP)
            IF (PAR_ABORT()) THEN
               FAULT = .TRUE.
               GOTO 500
            END IF
            CALL GEN_CFILL (1, NPIXEL, RTEMP, DYNAMIC_MEM(INEPTR))
         END IF
      END IF

*  map the axes

      CALL DSA_MAP_AXIS_DATA ('IN', 1, 'READ', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      XAXPTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_AXIS_DATA ('IN', 2, 'READ', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      YAXPTR = DYN_ELEMENT(ADDRESS)

*  map the LST array

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'LST.DATA_ARRAY', 0, 0,
     :   LST_DTA_NAME, DSTAT)
      CALL DTA_MRVARD (LST_DTA_NAME, NPIXEL, ADDRESS, DSTAT)
      LSTPTR = DYN_ELEMENT(ADDRESS)
      IF (DSTAT .NE. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('MAP2MEM - Error mapping LST array',
     :        IGNORE)
            STATUS = SAI__ERROR
         END IF
      END IF

*  get the coordinate type and RA, Dec of the centre of the map

      CALL JCMT_GETC (POS_STRUC_NAME, 'CENTRE_CRD', CENTRE_CRD, STATUS)
      ITEMP = ICH_FOLD (CENTRE_CRD)
      CALL JCMT_GETD (POS_STRUC_NAME, 'EPOCH', EPOCH, STATUS)
      CALL JCMT_GETD (POS_STRUC_NAME, 'RACEN', RACEN, STATUS)
      CALL JCMT_GETD (POS_STRUC_NAME, 'DECCEN', DECCEN, STATUS)

*  get local coordinate system, angles of axes relative to local north

      CALL JCMT_GETC (POS_STRUC_NAME, 'LOCAL_CRD', LOCAL_CRD, STATUS)
      ITEMP = ICH_FOLD (LOCAL_CRD)
      CALL JCMT_GETF (POS_STRUC_NAME, 'V2Y', V2Y, STATUS)
      CALL JCMT_GETF (POS_STRUC_NAME, 'X2Y', X2Y, STATUS)

*  chopper info

      CALL JCMT_GETC (POS_STRUC_NAME, 'POS_BEAM', POS_BEAM, STATUS)
      ITEMP = ICH_FOLD (POS_BEAM)
      CALL JCMT_GETC (POS_STRUC_NAME, 'TEL_BEAM', TEL_BEAM, STATUS)
      ITEMP = ICH_FOLD (TEL_BEAM)
      CALL JCMT_GETC (POS_STRUC_NAME, 'CHOP_CRD', CHOP_CRD, STATUS)
      CHOP_CRD = CHOP_CRD (1:2)
      ITEMP = ICH_FOLD (CHOP_CRD)
      CALL JCMT_GETF (POS_STRUC_NAME, 'CHOP_THRW', CHOP_THROW, STATUS)
      CALL JCMT_GETF (POS_STRUC_NAME, 'CHOP_PA', CHOP_PA, STATUS)

*  check coord system and beam information is valid

      IF ((CENTRE_CRD .NE. 'RJ') .AND.
     :    (CENTRE_CRD .NE. 'RB')) THEN
         CALL PAR_WRUSER ('MAP2MEM - invalid CENTRE_CRD, '//
     :     'must be RB or RJ', IGNORE)
         STATUS = SAI__ERROR
      END IF
      IF ((LOCAL_CRD .NE. 'RJ') .AND.
     :    (LOCAL_CRD .NE. 'RB') .AND.
     :    (LOCAL_CRD .NE. 'AZ')) THEN
         CALL PAR_WRUSER ('MAP2MEM - invalid LOCAL_CRD, '//
     :     'must be RB, RJ or AZ', IGNORE)
         STATUS = SAI__ERROR
      END IF
      IF ((CHOP_CRD .NE. 'LO') .AND.
     :    (CHOP_CRD .NE. 'AZ')) THEN
         CALL PAR_WRUSER ('MAP2MEM - invalid CHOP_CRD, '//
     :     'must be LO or AZ', IGNORE)
         STATUS = SAI__ERROR
      END IF

*  and beam info

      IF ((TEL_BEAM .NE. 'R') .AND.
     :    (TEL_BEAM .NE. 'M') .AND.
     :    (TEL_BEAM .NE. 'L')) THEN
         CALL PAR_WRUSER ('MAP2MEM - invalid telescope beam, '//
     :     'must be L, M or R', IGNORE)
         STATUS = SAI__ERROR
      END IF
      IF ((POS_BEAM .NE. 'R') .AND.
     :    (POS_BEAM .NE. 'L')) THEN
         CALL PAR_WRUSER ('MAP2MEM - invalid +ve beam, '//
     :     'must be L or R', IGNORE)
         STATUS = SAI__ERROR
      END IF

*  date of observation in modified Julian days

      CALL JCMT_GETD (POS_STRUC_NAME, 'MJD_START', MJDSTART,
     :   STATUS)

*  get telescope parameters

      CALL JCMT_GETD (TEL_STRUC_NAME, 'LAT', LAT, DSTAT)

*  get name of output file

      IF (STATUS .EQ. SAI__OK) THEN
         CALL PAR_SDCHAR ('OUTPUT', INFILE, STATUS)
         CALL PAR_RDCHAR ('OUTPUT', ' ', OUTFILE)
         IF (PAR_ABORT()) THEN
            FAULT = .TRUE.
            GOTO 500
         END IF
      END IF

*  and if it should be a binary output file

      IF (STATUS .EQ. SAI__OK) THEN
         CALL PAR_RDKEY ('BINARY', .TRUE., BINARY)
         IF (PAR_ABORT()) THEN
            FAULT = .TRUE.
            GOTO 500
         END IF
      END IF

*  exit if things have already gone wrong

      IF (STATUS .NE. SAI__OK) THEN
         FAULT = .TRUE.
         GOTO 500
      END IF



*  Get workspace for the list of derived RAs and DECs of the +ve and
*  -ve beams

      CALL DSA_GET_WORK_ARRAY (NPIXEL, 'DOUBLE', ADDRESS, SLOT, STATUS)
      RAPOS_PTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORK_ARRAY (NPIXEL, 'DOUBLE', ADDRESS, SLOT, STATUS)
      DECPOS_PTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORK_ARRAY (NPIXEL, 'DOUBLE', ADDRESS, SLOT, STATUS)
      RANEG_PTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORK_ARRAY (NPIXEL, 'DOUBLE', ADDRESS, SLOT, STATUS)
      DECNEG_PTR = DYN_ELEMENT(ADDRESS)

*  and for same in arcsec offsets from map centre

      CALL DSA_GET_WORK_ARRAY (NPIXEL, 'FLOAT', ADDRESS, SLOT, STATUS)
      RAPOS_OFF_PTR = DYN_ELEMENT (ADDRESS)
      CALL DSA_GET_WORK_ARRAY (NPIXEL, 'FLOAT', ADDRESS, SLOT, STATUS)
      DECPOS_OFF_PTR = DYN_ELEMENT (ADDRESS)
      CALL DSA_GET_WORK_ARRAY (NPIXEL, 'FLOAT', ADDRESS, SLOT, STATUS)
      RANEG_OFF_PTR = DYN_ELEMENT (ADDRESS)
      CALL DSA_GET_WORK_ARRAY (NPIXEL, 'FLOAT', ADDRESS, SLOT, STATUS)
      DECNEG_OFF_PTR = DYN_ELEMENT (ADDRESS)

*  and for the parallactic angle

      CALL DSA_GET_WORK_ARRAY (NPIXEL, 'FLOAT', ADDRESS, SLOT, STATUS)
      ETA_PTR = DYN_ELEMENT (ADDRESS)

*  calculate the offsets (in arcsec) of the +ve and -ve beams in the
*  chopper coord system, with an x-axis increasing to the right. The L
*  beam is assumed to be the one on the left of the vertical when the
*  chopper position angle is 90 degrees

      IF (TEL_BEAM .EQ. 'M') THEN
         IF (POS_BEAM .EQ. 'L') THEN
            POS_X = - 0.5 * CHOP_THROW * SIN (CHOP_PA)
            POS_Y = - 0.5 * CHOP_THROW * COS (CHOP_PA)
            NEG_X = - POS_X
            NEG_Y = - POS_Y
         ELSE IF (POS_BEAM .EQ. 'R') THEN
            POS_X =  0.5 * CHOP_THROW * SIN (CHOP_PA)
            POS_Y =  0.5 * CHOP_THROW * COS (CHOP_PA)
            NEG_X = - POS_X
            NEG_Y = - POS_Y
         END IF
      ELSE IF (TEL_BEAM .EQ. 'L') THEN
         IF (POS_BEAM .EQ. 'L') THEN
            POS_X = 0.0D0
            POS_Y = 0.0D0
            NEG_X = CHOP_THROW * SIN (CHOP_PA)
            NEG_Y = CHOP_THROW * COS (CHOP_PA)
         ELSE IF (POS_BEAM .EQ. 'R') THEN
            POS_X = CHOP_THROW * SIN (CHOP_PA)
            POS_Y = CHOP_THROW * COS (CHOP_PA)
            NEG_X = 0.0D0
            NEG_Y = 0.0D0
         END IF
      ELSE IF (TEL_BEAM .EQ. 'R') THEN
         IF (POS_BEAM .EQ. 'L') THEN
            POS_X = - CHOP_THROW * SIN (CHOP_PA)
            POS_Y = - CHOP_THROW * COS (CHOP_PA)
            NEG_X = 0.0D0
            NEG_Y = 0.0D0
         ELSE IF (POS_BEAM .EQ. 'R') THEN
            POS_X = 0.0D0
            POS_Y = 0.0D0
            NEG_X = - CHOP_THROW * SIN (CHOP_PA)
            NEG_Y = - CHOP_THROW * COS (CHOP_PA)
         END IF
      END IF


*  calculate the ra and dec of the positive beam for each pixel. This
*  routine will output results in RA, dec at the epoch of the observation

      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_BEAM2RADEC (CENTRE_CRD, EPOCH, RACEN,
     :      DECCEN, LOCAL_CRD, V2Y, X2Y, MJDSTART, LAT,
     :      NX, NY, DYNAMIC_MEM(XAXPTR), DYNAMIC_MEM(YAXPTR),
     :      DYNAMIC_MEM(LSTPTR), CHOP_CRD, POS_X, POS_Y,
     :      DYNAMIC_MEM(RAPOS_PTR), DYNAMIC_MEM(DECPOS_PTR),
     :      COORD_SYSTEM, STATUS)
      ENDIF

*  and for the -ve beam

      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_BEAM2RADEC (CENTRE_CRD, EPOCH, RACEN,
     :      DECCEN, LOCAL_CRD, V2Y, X2Y, MJDSTART, LAT,
     :      NX, NY, DYNAMIC_MEM(XAXPTR), DYNAMIC_MEM(YAXPTR),
     :      DYNAMIC_MEM(LSTPTR), CHOP_CRD, NEG_X, NEG_Y,
     :      DYNAMIC_MEM(RANEG_PTR), DYNAMIC_MEM(DECNEG_PTR),
     :      COORD_SYSTEM, STATUS)
      ENDIF

*  search for pointing correction structure in file, and map the
*  arrays if present

      POINTING_CORRECTION = .FALSE.
      IF (STATUS .EQ. SAI__OK) THEN
         CALL DTA_CRNAM (JCMT_DTA_NAME, 'PCORR.LST', 0, 0,
     :      PLST_DTA_NAME, DSTAT)
         CALL DTA_SZVAR (PLST_DTA_NAME, 1, NDIM, NCORR, DSTAT)
         CALL DTA_MRVARD (PLST_DTA_NAME, NCORR, ADDRESS, DSTAT)
         IF ((DSTAT.EQ.0) .AND. (NCORR.GE.1)) THEN
            POINT_LSTPTR = DYN_ELEMENT (ADDRESS)
            CALL DTA_CRNAM (JCMT_DTA_NAME, 'PCORR.D_AZ', 0, 0,
     :         PDAZ_DTA_NAME, DSTAT)
            CALL DTA_MRVARF (PDAZ_DTA_NAME, NCORR, ADDRESS, DSTAT)
            IF (DSTAT .NE. 0) THEN
               CALL PAR_WRUSER ('MAP2MEM - error reading D_AZ '//
     :           'array of pointing correction', IGNORE)
            ELSE
               POINT_DAZPTR = DYN_ELEMENT (ADDRESS)
               CALL DTA_CRNAM (JCMT_DTA_NAME, 'PCORR.D_ALT',
     :            0, 0, PDALT_DTA_NAME, DSTAT)
               CALL DTA_MRVARF (PDALT_DTA_NAME, NCORR, ADDRESS,
     :            DSTAT)
               IF (DSTAT .NE. 0) THEN
                  CALL PAR_WRUSER ('MAP2MEM - error reading '//
     :              'D_ALT array of pointing correction', IGNORE)
               ELSE
                  POINT_DALTPTR = DYN_ELEMENT (ADDRESS)
                  POINTING_CORRECTION = .TRUE.
               END IF
            END IF
         ELSE
            DSTAT = 0
         END IF
      END IF

*  correct the pointing if required

      IF (.NOT. POINTING_CORRECTION) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('MAP2MEM - No pointing corrections '//
     :        'applied', IGNORE)
         END IF
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('MAP2MEM - Applying pointing '//
     :        'corrections', IGNORE)
            CALL JCMT_CORRECT_POINTING (NPIXEL,
     :         DYNAMIC_MEM (RAPOS_PTR),
     :         DYNAMIC_MEM (DECPOS_PTR),
     :         DYNAMIC_MEM (LSTPTR),
     :         LAT, NCORR,
     :         DYNAMIC_MEM (POINT_LSTPTR),
     :         DYNAMIC_MEM (POINT_DAZPTR),
     :         DYNAMIC_MEM (POINT_DALTPTR),
     :         STATUS)
            CALL JCMT_CORRECT_POINTING (NPIXEL,
     :         DYNAMIC_MEM (RANEG_PTR),
     :         DYNAMIC_MEM (DECNEG_PTR),
     :         DYNAMIC_MEM (LSTPTR),
     :         LAT, NCORR,
     :         DYNAMIC_MEM (POINT_LSTPTR),
     :         DYNAMIC_MEM (POINT_DAZPTR),
     :         DYNAMIC_MEM (POINT_DALTPTR),
     :         STATUS)
         END IF
      END IF

*  calculate the parallactic angle of the point halfway between the
*  +ve and negative beams

      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_PARALLACTIC (NPIXEL,
     :      DYNAMIC_MEM (RAPOS_PTR), DYNAMIC_MEM (DECPOS_PTR),
     :      DYNAMIC_MEM (RANEG_PTR), DYNAMIC_MEM (DECNEG_PTR),
     :      DYNAMIC_MEM (LSTPTR), LAT,
     :      DYNAMIC_MEM (ETA_PTR), STATUS)
      END IF

*  transform the beam coordinates to the desired output coordinate system

      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_CONVERT (NPIXEL, COORD_SYSTEM, MJDSTART, B1950,
     :      DYNAMIC_MEM (RAPOS_PTR), DYNAMIC_MEM (DECPOS_PTR),
     :      STATUS)
         CALL JCMT_CONVERT (NPIXEL, COORD_SYSTEM, MJDSTART, B1950,
     :      DYNAMIC_MEM (RANEG_PTR), DYNAMIC_MEM (DECNEG_PTR),
     :      STATUS)
      END IF

*  also transform the map centre to B1950 if required

      IF (STATUS .EQ. SAI__OK) THEN

         IF (B1950) THEN
            IF (CENTRE_CRD .EQ. 'RB') THEN
               CALL SLA_PRECES ('FK4', EPOCH, 1950.0D0,
     :            RACEN, DECCEN)
            ELSE IF (CENTRE_CRD .EQ. 'RJ') THEN
               MJDTEMP = SLA_EPB2D (EPOCH)
               JEPOCH = SLA_EPJ (MJDTEMP)
               CALL SLA_PRECES ('FK5', JEPOCH, 2000.0D0,
     :            RACEN, DECCEN)
               CALL SLA_FK54Z (RACEN, DECCEN, 1950.0D0,
     :            RACEN, DECCEN, DIGNORE, DIGNORE)
            END IF
         ELSE
            IF (CENTRE_CRD .EQ. 'RB') THEN
               CALL SLA_PRECES ('FK4', EPOCH, 1950.0D0,
     :            RACEN, DECCEN)
               CALL SLA_FK45Z (RACEN, DECCEN, 1950.0D0,
     :            RACEN, DECCEN)
            ELSE IF (CENTRE_CRD .EQ.'RJ') THEN
               MJDTEMP = SLA_EPB2D (EPOCH)
               JEPOCH = SLA_EPJ (MJDTEMP)
               CALL SLA_PRECES ('FK5', JEPOCH, 2000.0D0,
     :            RACEN, DECCEN)
            END IF
         END IF
      END IF

*  get desired map centre

      IF (STATUS .EQ. SAI__OK) THEN

         IF (B1950) THEN
            SYSTEM = 'RB1950.0'
            CALL PAR_WRUSER ('Coordinates are FK4 B1950.0', IGNORE)
         ELSE
            SYSTEM = 'RJ2000.0'
            CALL PAR_WRUSER ('Coordinates are FK5 J2000.0', IGNORE)
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
            CALL PAR_WRUSER ('MAP2MEM - Error reading centre RA, '//
     :        'input must be in 5 45 36 type format.', IGNORE)
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
            CALL PAR_WRUSER ('MAP2MEM - Error reading centre Dec, '//
     :        'input must be in 5 10 20.66 or -35 56 67 format.',
     :        IGNORE)
            FAULT = .TRUE.
            GOTO 500
         END IF
      END IF

*  calculate offsets of pixels from map centre

      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_CALC_OFFSETS (NPIXEL,
     :      DYNAMIC_MEM (RAPOS_PTR), DYNAMIC_MEM (DECPOS_PTR),
     :      RACEN, DECCEN,
     :      DYNAMIC_MEM (RAPOS_OFF_PTR), DYNAMIC_MEM (DECPOS_OFF_PTR),
     :      STATUS)
         CALL JCMT_CALC_OFFSETS (NPIXEL,
     :      DYNAMIC_MEM (RANEG_PTR), DYNAMIC_MEM (DECNEG_PTR),
     :      RACEN, DECCEN,
     :      DYNAMIC_MEM (RANEG_OFF_PTR), DYNAMIC_MEM (DECNEG_OFF_PTR),
     :      STATUS)
      END IF


*  OK, have calculated everything, now write the stuff out, header file first

      IF (STATUS .EQ. SAI__OK) THEN

         CALL FIO_OPEN (OUTFILE(:ICH_LEN(OUTFILE))//'.dbh', 'WRITE',
     :     'FORTRAN', 0, FD, STATUS)
         CALL FIO_UNIT (FD, LU, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (BINARY) THEN
               EXTENSION = 'bin'
            ELSE
               EXTENSION = 'mem'
            END IF
            IGNORE = 0
            CALL PSX_TIME  (NTICKS, IGNORE)
            CALL PSX_CTIME (NTICKS, NOW, IGNORE)

            WRITE (LU, *) 'Header for the DBMEM data file '//
     :        OUTFILE(:ICH_LEN(OUTFILE))//'.'//EXTENSION
            WRITE (LU, *) 'Written by MAP2MEM on '//NOW
            WRITE (LU, *) NPIXEL
            WRITE (LU, *) NX
            WRITE (LU, *) NY
            WRITE (LU, *) 7
            WRITE (LU, *) BINARY
            WRITE (LU, *) RACEN
            WRITE (LU, *) DECCEN
            WRITE (LU, *) SYSTEM
            WRITE (LU, *) 'RA-DEC MAP'
            WRITE (LU, *) ' '
            WRITE (LU, *) ' '
            WRITE (LU, *) ' '
            WRITE (LU, *) 'End of DBMEM header file'
            CALL FIO_CLOSE (FD, STATUS)
         ELSE
            CALL PAR_WRUSER ('MAP2MEM - Error opening output header ',
     :        IGNORE)
            FAULT = .TRUE.
         END IF

      END IF


      IF (STATUS .EQ. SAI__OK) THEN

*  now for the data file

         IF (BINARY) THEN
            CALL FIO_OPEN (OUTFILE(:ICH_LEN(OUTFILE))//'.bin',
     :        'WRITE', 'UNFORMATTED', 0, FD, STATUS)
         ELSE
            CALL FIO_OPEN (OUTFILE(:ICH_LEN(OUTFILE))//'.mem',
     :        'WRITE', 'FORTRAN', 0, FD, STATUS)
         END IF
         CALL FIO_UNIT (FD, LU, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL JCMT_WRITE_MEM (LU, BINARY, NPIXEL,
     :        DYNAMIC_MEM (INPTR), DYNAMIC_MEM (RAPOS_OFF_PTR),
     :        DYNAMIC_MEM (DECPOS_OFF_PTR),
     :        DYNAMIC_MEM (RANEG_OFF_PTR),
     :        DYNAMIC_MEM (DECNEG_OFF_PTR),
     :        DYNAMIC_MEM (INEPTR), DYNAMIC_MEM (ETA_PTR),
     :        STATUS)
            CALL FIO_CLOSE (FD, STATUS)
         ELSE
            CALL PAR_WRUSER ('MAP2MEM - Error opening output data ',
     :        IGNORE)
            FAULT = .TRUE.
         END IF

      END IF



*  tidy up

 500  IF (DSTAT .NE. 0) THEN
         CALL DTA_ERROR (DSTAT, ERROR)
         CALL PAR_WRUSER (ERROR(:ICH_LEN(ERROR)), IGNORE)
         FAULT=.TRUE.
      END IF
      CALL DSA_CLOSE (STATUS)
      IF (FAULT) CALL FIG_SETERR

      END

