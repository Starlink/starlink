      SUBROUTINE FAKE
*+
*  Name:
*     FAKE

*  Purpose:
*     Construct fake dataset

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FAKE

*  Description:
*     {routine_description}

*  [optional_subroutine_items]...
*  Authors:
*     J.Lightfoot (REVAD::JFL)
*     Tim Jenness (JACH::TIMJ)
*     {enter_new_authors_here}

*  History:
*       late 1991 (REVAD::JFL): Original version.
*      5-MAR-1992 (REVAD::JFL): Modified to use shorter structure names,
*                               RA/Dec or az-el tangent plane offsets,
*                               pointing corrections.
*     24-MAR-1994 (REVAD::HME): Change include statements. Disuse STR$UPCASE.
*     13-JUN-1994 (REVAD::HME): Disuse DSA_WRUSER.
*     14-OCT-1994 (REVAD::JFL): Safeguard against problems with maps straddling
*                               2pi -> 0 boundary in RA.
*     28-MAR-2003 (JACH:TIMJ): Fix warning for TS2MAP
*                              NELM is now initialised
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'


*  Dynamic memory include file - defines DYNAMIC_MEM
      INCLUDE 'DYNAMIC_MEMORY'

*  Data structure error codes
      INCLUDE 'DTA_CODES'

*  functions:
      LOGICAL PAR_ABORT                ! check PAR abort flag
      INTEGER DYN_ELEMENT              ! DSA dynamic memory
      INTEGER ICH_LEN                  ! Figaro string length
      INTEGER ICH_FOLD                 ! Figaro string to upper case
      DOUBLE PRECISION SLA_EPB2D       ! Besselian epoch to MJD
      DOUBLE PRECISION SLA_EPJ         ! MJD to Julian epoch

*  Status:
      INTEGER STATUS                   ! Global status for DSA routines
      INTEGER DSTAT                    ! status for dta routines

*  Local Constants:
      INTEGER MAX_DIM                  ! max no of dimensions of array
      PARAMETER (MAX_DIM = 10)

*  Local Variables:
      LOGICAL FAULT                    ! logical to flag a Figaro fault
      LOGICAL STRUC                    ! logical for testing structure
                                       ! existence
      LOGICAL ERRORS                   ! T if error array present in input data
      LOGICAL B1950                    !
      LOGICAL POINTING_CORRECTION      ! T if pointing correction structure is
                                       !   present in file
      INTEGER NDIM                     ! number of dimensions of array
      INTEGER DIMS (MAX_DIM)           ! size of each dimension
      INTEGER NELM                     ! total number of elements in array
      INTEGER NX                       ! x dimension of input array
      INTEGER NY                       ! y  "       "      "
      INTEGER NPIXEL                   ! total number of pixels in input array
      INTEGER I                        ! DO loop variable
      INTEGER SLOT                     ! DSA slot number
      INTEGER ADDRESS                  ! DSA address
      INTEGER INPTR                    ! DSA pointer for input data array
      INTEGER XAXPTR                   !    "       "      "   x-axis
      INTEGER YAXPTR                   !    "       "      "
      INTEGER RAPPTR                   ! DSA pointer to the derived list of
                                       ! observed right ascensions (J2000)
      INTEGER DECPPTR                  ! DSA pointer to the derived list of
                                       ! observed declinations (J2000)
      INTEGER OUTPTR                   ! DSA pointer to output array
      INTEGER LSTPTR                   ! DSA    "      " input LST array
      INTEGER NCORR                    ! number of entries in pointing
                                       !   correction description
      INTEGER POINT_LSTPTR             ! DSA pointer to LST array in pointing
                                       !   correction description
      INTEGER POINT_DAZPTR             ! DSA pointer to az pointing correction
      INTEGER POINT_DALTPTR            !        "       alt       "      "
      INTEGER IGNORE                   ! returned by ICH_FOLD
      REAL FBAD                        ! value to flag bad pixel
      REAL XOFF, YOFF                  ! offset from map centre of fake Airy
                                       ! disk
      REAL V2Y                         ! angle between vertical of local coords
                                       !   and y offset axis (anti-clockwise,
                                       !   radians)
      REAL X2Y                         ! angle between x and y offset axes
                                       !   (anti-clockwise, radians)
      DOUBLE PRECISION RACEN           ! RA of map centre (radians)
      DOUBLE PRECISION DECCEN          ! DEC of map centre (radians)
      DOUBLE PRECISION EPOCH           ! Besselian epoch of map centre coords
      DOUBLE PRECISION JEPOCH          ! Julian epoch of map centre coords
      DOUBLE PRECISION MJDSTART        ! MJD of start of observations
      DOUBLE PRECISION MJDTEMP         !
      DOUBLE PRECISION LAT             ! Latitude of observatory (radians)
      DOUBLE PRECISION CELLX           ! x unit cell dimension (arcsec)
      DOUBLE PRECISION CELLY           ! y unit cell dimension (arcsec)
      DOUBLE PRECISION PIXSPACE        ! output pixel spaceing (radians)
      DOUBLE PRECISION DIGNORE         !
      CHARACTER*(10) CENTRE_CRD        ! coord system for map centre
      CHARACTER*(10) LOCAL_CRD         ! coord system for local offsets
      CHARACTER*(64) JCMT_DTA_NAME     ! DTA name of the structure
                                       ! containing the JCMT specific
                                       ! data
      CHARACTER*(128) PLST_DTA_NAME    ! DTA name of LST array in pointing
                                       !   correction structure
      CHARACTER*(128) PDAZ_DTA_NAME    ! " .. array containing az correction
      CHARACTER*(128) PDALT_DTA_NAME   ! " .. array containing alt correction
      CHARACTER*(128) DTA_NAME         ! temp variable for holding DTA names
      CHARACTER*(128) POS_STRUC_NAME   ! name of structure containing
                                       ! position related parameters
      CHARACTER*(64) CHAR_ITEMS (2)    ! character array for DSA_SET_AXIS_INFO
      CHARACTER*(80) ERROR             ! DTA error message
      CHARACTER*(128) TEL_STRUC_NAME   ! Name of the telescope structure
      CHARACTER*(20) FAKE_TYPE         ! Type of fake required: AIRY or FLAT

*  Local Constants:

*.

*  Initial values

      FAULT = .FALSE.

*  Initialise DSA system

      STATUS = 0
      CALL DSA_OPEN (STATUS)

*  get the input file

      CALL DSA_INPUT ('IN', 'INPUT', STATUS)

*  check that it doesn't contain a TSDAT structure which would mean that
*  the data is in `time-sorted' form

      CALL DTA_CRNAM ('IN', 'MORE.JCMT.TSDAT', 0, 0, DTA_NAME,
     :   DSTAT)
      CALL DTA_STRUC (DTA_NAME, STRUC, DSTAT)
      IF (DSTAT .EQ. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('FAKE - Input file is in time-sequence '//
     :         'format. Use TS2MAP before FAKE.',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  check that the MORE.JCMT structure is there

      CALL DTA_CRNAM ('IN', 'MORE.JCMT', 0, 0, JCMT_DTA_NAME, DSTAT)
      CALL DTA_STRUC (JCMT_DTA_NAME, STRUC, DSTAT)
      IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT. STRUC) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('FAKE - File is not in JCMT format',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  Check the LST values are present and map them
*  Needs to be done after we have worked out the size of the array

      CALL DSA_DATA_SIZE ('IN', 2, NDIM, DIMS, NELM, STATUS)
      CALL DTA_CRNAM (JCMT_DTA_NAME, 'LST.DATA_ARRAY', 0, 0, DTA_NAME,
     :   DSTAT)
      CALL DTA_MRVARD (DTA_NAME, NELM, ADDRESS, DSTAT)
      LSTPTR = DYN_ELEMENT(ADDRESS)
      IF (DSTAT .NE. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('FAKE - JCMT structure does not contain '//
     :         'LST array',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  check position structure present

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'MAP', 0, 0, POS_STRUC_NAME,
     :   DSTAT)
      CALL DTA_STRUC (POS_STRUC_NAME, STRUC, DSTAT)
      IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT. STRUC) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('FAKE - JCMT structure does not contain '//
     :            'positional information',IGNORE)
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
            CALL PAR_WRUSER ('FAKE - JCMT structure does not contain '//
     :         'telescope information',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  tell DSA want to use magic values with input data, get flag value
*  for reals

      CALL DSA_USE_FLAGGED_VALUES ('IN', STATUS)
      CALL DSA_GET_FLAG_VALUE ('FLOAT', FBAD, STATUS)

*  map the main data array

      CALL DSA_DATA_SIZE ('IN', 2, NDIM, DIMS, NELM, STATUS)
      CALL DSA_MAP_DATA ('IN', 'READ', 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      INPTR = DYN_ELEMENT(ADDRESS)
      NPIXEL = NELM
      NX = DIMS(1)
      NY = DIMS(2)
      IF (NY .LT. 1) NY = 1

*  map the axes

      CALL DSA_MAP_AXIS_DATA ('IN', 1, 'READ', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      XAXPTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_AXIS_DATA ('IN', 2, 'READ', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      YAXPTR = DYN_ELEMENT(ADDRESS)

*  get the coordinate type and RA, Dec of the centre of the map

      CALL JCMT_GETC (POS_STRUC_NAME, 'CENTRE_CRD', CENTRE_CRD,
     :   STATUS)
      CALL JCMT_GETD (POS_STRUC_NAME, 'EPOCH', EPOCH, STATUS)
      CALL JCMT_GETD (POS_STRUC_NAME, 'RACEN', RACEN, STATUS)
      CALL JCMT_GETD (POS_STRUC_NAME, 'DECCEN', DECCEN, STATUS)

*  get local coordinate system, angles of cell axes relative to local north

      CALL JCMT_GETC (POS_STRUC_NAME, 'LOCAL_CRD', LOCAL_CRD, STATUS)
      CALL JCMT_GETF (POS_STRUC_NAME, 'V2Y', V2Y, STATUS)
      CALL JCMT_GETF (POS_STRUC_NAME, 'X2Y', X2Y, STATUS)

*  date of observation in modified Julian days

      CALL JCMT_GETD (POS_STRUC_NAME, 'MJD_START', MJDSTART, STATUS)

*  get telescope parameters

      CALL JCMT_GETD (TEL_STRUC_NAME, 'LAT', LAT, STATUS)

*  get unit cell sizes of input data, set output cell size (affects diameter
*  of fake Airy disk) to be the larger (in radians)

      CALL JCMT_GETD (POS_STRUC_NAME, 'CELL_X', CELLX, STATUS)
      CALL JCMT_GETD (POS_STRUC_NAME, 'CELL_Y', CELLY, STATUS)
      PIXSPACE = MAX(ABS(CELLX),ABS(CELLY)) * DAS2R

*  Get workspace for the list of derived RAs and DECs after the
*  transformation from ALT-AZ

      CALL DSA_GET_WORK_ARRAY (NPIXEL, 'DOUBLE', ADDRESS, SLOT,
     :   STATUS)
      RAPPTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORK_ARRAY (NPIXEL, 'DOUBLE', ADDRESS, SLOT,
     :   STATUS)
      DECPPTR = DYN_ELEMENT(ADDRESS)

*  abort here if there's been a problem, confusing if try to carry on

      IF (STATUS .NE. SAI__OK) THEN
         FAULT = .TRUE.
         GOTO 500
      END IF

*  calculate the transformed list of ra and dec for each pixel in B1950
*  coords

      B1950 = .TRUE.
      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_OFFSET2RADEC (CENTRE_CRD, EPOCH, RACEN, DECCEN,
     :      LOCAL_CRD, V2Y, X2Y, MJDSTART, LAT, NX, NY,
     :      DYNAMIC_MEM(XAXPTR), DYNAMIC_MEM(YAXPTR),
     :      DYNAMIC_MEM(LSTPTR), B1950, DYNAMIC_MEM(RAPPTR),
     :      DYNAMIC_MEM(DECPPTR), STATUS)
      ENDIF

*  also transform the map centre to B1950

      IF (STATUS .EQ. SAI__OK) THEN
         IF (CENTRE_CRD .EQ. 'RB') THEN
            CALL SLA_PRECES ('FK4', EPOCH, 1950.0D0, RACEN, DECCEN)
         ELSE IF (CENTRE_CRD .EQ. 'RJ') THEN
            MJDTEMP = SLA_EPB2D (EPOCH)
            JEPOCH = SLA_EPJ (MJDTEMP)
            CALL SLA_PRECES ('FK5', JEPOCH, 2000.0D0, RACEN, DECCEN)
            CALL SLA_FK54Z (RACEN, DECCEN, 1950.0D0, RACEN, DECCEN,
     :         DIGNORE, DIGNORE)
         END IF
      END IF

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
            CALL DTA_CRNAM (JCMT_DTA_NAME, 'PCORR.D_AZ',
     :         0, 0, PDAZ_DTA_NAME, DSTAT)
            CALL DTA_MRVARF (PDAZ_DTA_NAME, NCORR, ADDRESS, DSTAT)
            IF (DSTAT .NE. 0) THEN
               CALL PAR_WRUSER ('FAKE - error reading D_AZ '//
     :            'array of pointing correction',IGNORE)
            ELSE
               POINT_DAZPTR = DYN_ELEMENT (ADDRESS)
               CALL DTA_CRNAM (JCMT_DTA_NAME, 'PCORR.D_ALT',
     :            0, 0, PDALT_DTA_NAME, DSTAT)
               CALL DTA_MRVARF (PDALT_DTA_NAME, NCORR, ADDRESS,
     :            DSTAT)
               IF (DSTAT .NE. 0) THEN
                  CALL PAR_WRUSER ('FAKE - error reading '//
     :               'D_ALT array of pointing correction',IGNORE)
               ELSE
                  POINT_DALTPTR = DYN_ELEMENT (ADDRESS)
                  POINTING_CORRECTION = .TRUE.
               END IF
            END IF
         END IF
      END IF
      DSTAT = 0

*  uncorrect the pointing if required

      IF (.NOT. POINTING_CORRECTION) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('FAKE - No pointing corrections '//
     :         'applied',IGNORE)
         END IF
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('FAKE - Adding pointing corrections',
     :         IGNORE)
            CALL JCMT_CORRECT_POINTING (NPIXEL,
     :         DYNAMIC_MEM (RAPPTR),
     :         DYNAMIC_MEM (DECPPTR),
     :         DYNAMIC_MEM (LSTPTR),
     :         LAT, NCORR,
     :         DYNAMIC_MEM (POINT_LSTPTR),
     :         DYNAMIC_MEM (POINT_DAZPTR),
     :         DYNAMIC_MEM (POINT_DALTPTR),
     :         STATUS)
         END IF
      END IF

*  make sure that all RAs are in the range 0 - 2pi

      IF (STATUS .EQ. SAI__OK) THEN
         DO I = 1, NPIXEL
            CALL SLA_DRANRM (DYNAMIC_MEM(RAPPTR))
         END DO
      END IF

*  open output structure. Force a new file to be created as this is a
*  radical change.

      CALL DSA_OUTPUT ('OUT', 'OUTPUT', 'IN', 0, 1, STATUS)

*  tell DSA want to use flagged values in output structure

      CALL DSA_SET_FLAGGED_VALUES ('OUT', .TRUE., STATUS)
      CALL DSA_USE_FLAGGED_VALUES ('OUT', STATUS)

*  map the output data

      CALL DSA_MAP_DATA ('OUT', 'UPDATE', 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      OUTPTR = DYN_ELEMENT(ADDRESS)

*  find out what sort of fake is required

      IF (STATUS .EQ. SAI__OK) THEN
         CALL PAR_RDCHAR ('FAKE_TYPE', 'AIRY', FAKE_TYPE)
         IF (PAR_ABORT()) THEN
            FAULT = .TRUE.
            GOTO 500
         END IF
         IGNORE = ICH_FOLD(FAKE_TYPE)
         IF (FAKE_TYPE .EQ. 'AIRY') THEN
            CALL PAR_RDVAL ('XOFF', -60.0, 60.0, 0.0, 'arcsec ', XOFF)
            IF (PAR_ABORT()) THEN
               FAULT = .TRUE.
               GOTO 500
            END IF
            CALL PAR_RDVAL ('YOFF', -60.0, 60.0, 0.0, 'arcsec ', YOFF)
            IF (PAR_ABORT()) THEN
               FAULT = .TRUE.
               GOTO 500
            END IF
            XOFF = XOFF / 206265.0
            YOFF = YOFF / 206265.0
         END IF
      END IF

*  fake the data

      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_FAKE_DATA (FAKE_TYPE, XOFF, YOFF, PIXSPACE,
     :      NPIXEL, RACEN, DECCEN, DYNAMIC_MEM(RAPPTR),
     :      DYNAMIC_MEM(DECPPTR), DYNAMIC_MEM(OUTPTR), FBAD, STATUS)
      END IF

*  tidy up

 500  IF (DSTAT .NE. 0) THEN
         CALL DTA_ERROR (DSTAT, ERROR)
         CALL PAR_WRUSER (ERROR(:ICH_LEN(ERROR)),IGNORE)
         FAULT=.TRUE.
      END IF
      CALL DSA_CLOSE (STATUS)
      IF (FAULT) CALL FIG_SETERR

      END
