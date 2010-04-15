      SUBROUTINE JCMTEXTC
*+
*  Name:
*     JCMTEXTC

*  Purpose:
*     Perform extinction correction on JCMT map data

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMTEXTC

*  Description:

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1989 (JBVAD::PAH):
*        Original version.
*     21-MAY-1991 (REVAD::JFL):
*        Checked through, changed to work out airmass and tau here rather
*        than in MAKEMAP, changed to always prompt for ENDTAU
*     28-FEB-1992 (REVAD::JFL):
*        Modified to use tangent plane calculations for pixel coords,
*        and to work for AZ or RA/Dec maps
*     22-MAR-1994 (REVAD::HME):
*        Change include statements.
*     13-JUN-1994 (REVAD::HME):
*        Disuse DSA_WRUSER.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
*  Dynamic memory include file - defines DYNAMIC_MEM
      INCLUDE 'DYNAMIC_MEMORY'

*  Data structure error codes
      INCLUDE 'DTA_CODES'

*  functions:
      LOGICAL PAR_ABORT          ! check PAR abort flag
      INTEGER DYN_ELEMENT        ! DSA dynamic memory function
      INTEGER ICH_LEN            ! Figaro string length function
      INTEGER ICH_FOLD           ! Figaro conversion to upper case

*  Status:
      INTEGER STATUS             ! Global status for DSA routines
      INTEGER DSTAT              ! status for dta routines

*  Local Constants:

*  Local Variables:
      LOGICAL STRUC              ! logical for testing structure
                                 ! existence
      LOGICAL FAULT              ! T if want to signal Figaro error
      LOGICAL ERRORS             ! T if error array present in input data
      LOGICAL TAUINTERP          ! T if want to interpolate zenith extinction
                                 ! between TAU and ENDTAU
      INTEGER NDIM               ! number of dimensions of array
      INTEGER DIMS (2)           ! size of each dimension
      INTEGER NELM               ! total number of elements in array
      INTEGER ADDRESS            ! DSA VM address
      INTEGER SLOT               !   " slot
      INTEGER INPTR              ! DSA pointer for input data
      INTEGER INEPTR             !    "      "      "    errors
      INTEGER OUTPTR             !    "      "     output data
      INTEGER OUTEPTR            !    "      "      "    errors
      INTEGER XAXPTR             !    "      "     input x axis
      INTEGER YAXPTR             !    "      "      "    y axis
      INTEGER TAUPTR             !    "      "     zenith optical depth
      INTEGER AIRPTR             !    "      "     airmass
      INTEGER LSTPTR             !    "      "     LST array
      INTEGER NPIXEL             ! number of input pixels
      INTEGER NX                 ! x dimension of image
      INTEGER NY                 ! y dimension of image
      INTEGER IGNORE             !
      REAL V2Y                   ! angle between `local' vertical and y offset
                                 ! axis (radians)
      REAL X2Y                   ! angle between x and y offset axes
                                 !   (anti-clockwise, radians)
      REAL TAU                   ! zenith optical depth at start of observation
      REAL ENDTAU                !    "       "      "     end
      REAL FBAD                  ! `magic' value for real data
      DOUBLE PRECISION MJDSTART  ! MJD of start of observation
      DOUBLE PRECISION EPOCH     ! Besselian epoch of map centre coords
      DOUBLE PRECISION RACEN     ! RA of map centre (radians)
      DOUBLE PRECISION DECCEN    ! Dec of map centre (radians)
      DOUBLE PRECISION LAT       ! Latitude of observatory
      CHARACTER*(64) JCMT_DTA_NAME ! DTA name of the structure
                                 ! containing the JCMT specific
                                 ! data
      CHARACTER*(128) DTA_NAME   ! temp variable for holding DTA names
      CHARACTER*(128) POS_STRUC_NAME ! DTA name of position
                                 ! structure
      CHARACTER*(128) LST_STRUC_NAME ! DTA name of LST
                                 ! structure
      CHARACTER*(128) TEL_STRUC_NAME ! DTA name of TEL structure
      CHARACTER*(10) CENTRE_CRD  ! coordinate system of map centre coords
      CHARACTER*(10) LOCAL_CRD   ! coordinate system of local offsets
      CHARACTER*(80) ERROR       ! error message

*.

*  Initial values

      FAULT = .FALSE.

*  Initialise DSA system

      STATUS = 0
      CALL DSA_OPEN (STATUS)

*  get the input file

      CALL DSA_INPUT ('IN', 'INPUT', STATUS)

*  check that the MORE.JCMT structure is there

      CALL DTA_CRNAM ('IN', 'MORE.JCMT', 0, 0, JCMT_DTA_NAME, DSTAT)
      CALL DTA_STRUC (JCMT_DTA_NAME, STRUC, DSTAT)
      IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT.STRUC) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('JCMTEXTC - File is not in JCMT format',
     :         IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  check that the TSDAT structure is NOT there, don't allow this application
*  to work on data sorted into time sequence

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'TSDAT', 0, 0, DTA_NAME, DSTAT)
      CALL DTA_STRUC (DTA_NAME, STRUC, DSTAT)
      IF (DSTAT .EQ. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('JCMTEXTC - File has been sorted '//
     :         'into time sequence',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  check that the TAU structure is NOT there, if it is it means that the
*  data has already been extinction corrected

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'TAU', 0, 0, DTA_NAME, DSTAT)
      CALL DTA_STRUC (DTA_NAME, STRUC, DSTAT)
      IF (DSTAT .EQ. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('JCMTEXTC - File has already been '//
     :         'extinction corrected',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  look for position structure name

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'MAP', 0, 0, POS_STRUC_NAME,
     :   DSTAT)
      CALL DTA_STRUC (POS_STRUC_NAME, STRUC, DSTAT)
      IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT.STRUC) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('JCMTEXTC - File does not contain '//
     :         'MAP information',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  look for telescope structure

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'TEL', 0, 0, TEL_STRUC_NAME,
     :   DSTAT)
      CALL DTA_STRUC (POS_STRUC_NAME, STRUC, DSTAT)
      IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT.STRUC) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('JCMTEXTC - File does not contain '//
     :         'TEL information',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  look for LST structure name

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'LST', 0, 0, LST_STRUC_NAME,
     :   DSTAT)
      CALL DTA_STRUC (LST_STRUC_NAME, STRUC, DSTAT)
      IF (DSTAT.EQ.DTA_NOTFND .OR. .NOT.STRUC) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('JCMTEXTC - File does not contain '//
     :         'LST information',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  get the coordinate type and RA, Dec of the centre of the image

      CALL JCMT_GETC (POS_STRUC_NAME, 'CENTRE_CRD', CENTRE_CRD,
     :   STATUS)
      IGNORE = ICH_FOLD (CENTRE_CRD)
      CALL JCMT_GETD (POS_STRUC_NAME, 'EPOCH', EPOCH, STATUS)
      CALL JCMT_GETD (POS_STRUC_NAME, 'RACEN', RACEN, STATUS)
      CALL JCMT_GETD (POS_STRUC_NAME, 'DECCEN', DECCEN, STATUS)

*  get local coordinate system, angles of axes relative to local north

      CALL JCMT_GETC (POS_STRUC_NAME, 'LOCAL_CRD', LOCAL_CRD,
     :   STATUS)
      IGNORE = ICH_FOLD (LOCAL_CRD)
      CALL JCMT_GETF (POS_STRUC_NAME, 'V2Y', V2Y, STATUS)
      CALL JCMT_GETF (POS_STRUC_NAME, 'X2Y', X2Y, STATUS)

*  data of observation in modified Julian days

      CALL JCMT_GETD (POS_STRUC_NAME, 'MJD_START', MJDSTART,
     :   STATUS)

*  get telescope parameters

      CALL JCMT_GETD (TEL_STRUC_NAME, 'LAT', LAT, STATUS)


*  open the output file, insist on creating new file

      CALL DSA_OUTPUT ('OUT', 'OUTPUT', 'IN', 0, 1, STATUS)

*  tell DSA using magic values and map the input and output main data arrays

      CALL DSA_USE_FLAGGED_VALUES ('IN', STATUS)
      CALL DSA_GET_FLAG_VALUE ('FLOAT', FBAD, STATUS)
      CALL DSA_DATA_SIZE ('IN', 2, NDIM, DIMS, NELM, STATUS)
      CALL DSA_MAP_DATA ('IN', 'READ', 'FLOAT', ADDRESS, SLOT, STATUS)
      INPTR = DYN_ELEMENT(ADDRESS)
      NPIXEL = NELM
      NX = DIMS(1)
      NY = DIMS(2)

      CALL DSA_USE_FLAGGED_VALUES ('OUT', STATUS)
      CALL DSA_MAP_DATA ('OUT', 'UPDATE', 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      OUTPTR = DYN_ELEMENT(ADDRESS)

*  map the input and output error arrays if present

      CALL DSA_SEEK_ERRORS ('IN', ERRORS, STATUS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('IN', 'READ', 'FLOAT', ADDRESS, SLOT,
     :      STATUS)
         INEPTR = DYN_ELEMENT (ADDRESS)
         CALL DSA_MAP_ERRORS ('OUT', 'UPDATE', 'FLOAT', ADDRESS, SLOT,
     :      STATUS)
         OUTEPTR = DYN_ELEMENT (ADDRESS)
      END IF

*  map the axes

      CALL DSA_MAP_AXIS_DATA ('IN', 1, 'READ', 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      XAXPTR = DYN_ELEMENT (ADDRESS)
      CALL DSA_MAP_AXIS_DATA ('IN', 2, 'READ', 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      YAXPTR = DYN_ELEMENT (ADDRESS)

*  map the LST array

      CALL DTA_CRNAM (LST_STRUC_NAME, 'DATA_ARRAY', 0, 0, DTA_NAME,
     :   DSTAT)
      CALL DTA_MRVARD (DTA_NAME, NPIXEL, ADDRESS, DSTAT)
      LSTPTR = DYN_ELEMENT(ADDRESS)
      IF (DSTAT .NE. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('JCMTEXTC - Error mapping LST '//
     :         'array',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  create an airmass structure in the output JCMT section and map the data array

      CALL DTA_CRNAM ('OUT', 'MORE.JCMT', 0, 0, JCMT_DTA_NAME, DSTAT)
      CALL DTA_CRNAM (JCMT_DTA_NAME, 'AIRMASS', 0, 0, DTA_NAME, DSTAT)
      CALL DTA_CRVAR (DTA_NAME, 'struc', DSTAT)
      DIMS(1) = NX
      DIMS(2) = NY
      CALL DTA_CRNAM (JCMT_DTA_NAME, 'AIRMASS.DATA_ARRAY', 2, DIMS,
     :   DTA_NAME, DSTAT)
      CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
      CALL DTA_CRNAM (JCMT_DTA_NAME, 'AIRMASS.DATA_ARRAY', 0, 0,
     :   DTA_NAME, DSTAT)
      CALL DTA_MUVARF (DTA_NAME, NPIXEL, ADDRESS, DSTAT)
      AIRPTR = DYN_ELEMENT (ADDRESS)
      IF (DSTAT .NE. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('JCMTEXTC - Error creating airmass '//
     :         'structure',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  and call a routine to calculate the airmass

      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_GET_AIRMASS (CENTRE_CRD, EPOCH, RACEN, DECCEN,
     :      LOCAL_CRD, V2Y, X2Y, MJDSTART, LAT, NX, NY,
     :      DYNAMIC_MEM (XAXPTR), DYNAMIC_MEM (YAXPTR),
     :      DYNAMIC_MEM (LSTPTR), FBAD, DYNAMIC_MEM (AIRPTR), STATUS)
      END IF


*  now get the zenith extinction

*  first get the extinctions at the start and end of the observation

      IF (STATUS .EQ. SAI__OK) THEN
         CALL PAR_RDVAL ('TAU', 0.0, 20.0, 0.0, ' ', TAU)
         IF (PAR_ABORT()) THEN
            FAULT = .TRUE.
            GOTO 500
         END IF
         TAUINTERP = .TRUE.
         CALL PAR_RDVAL ('ENDTAU', 0.0, 20.0, 0.0, ' ', ENDTAU)
         IF (PAR_ABORT()) THEN
            FAULT = .TRUE.
            GOTO 500
         END IF
      END IF

*  create the extinction array in the JCMT structure and map it

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'TAU', 0, 0, DTA_NAME, DSTAT)
      CALL DTA_CRVAR (DTA_NAME, 'struc', DSTAT)
      CALL DTA_CRNAM (JCMT_DTA_NAME, 'TAU.DATA_ARRAY', 2, DIMS,
     :   DTA_NAME, DSTAT)
      CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
      CALL DTA_CRNAM (JCMT_DTA_NAME, 'TAU.DATA_ARRAY', 0, 0, DTA_NAME,
     :   DSTAT)
      CALL DTA_MUVARF (DTA_NAME, NPIXEL, ADDRESS, DSTAT)
      TAUPTR = DYN_ELEMENT (ADDRESS)
      IF (DSTAT .NE. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('JCMTEXTC - Error creating opacity '//
     :         'structure',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  and fill it with interpolation between TAU and ENDTAU
*  according to pixel LST

      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_SETTAU (TAU, ENDTAU, TAUINTERP, NPIXEL,
     :      DYNAMIC_MEM(INPTR), FBAD,
     :      DYNAMIC_MEM(LSTPTR), DYNAMIC_MEM(TAUPTR), STATUS)
      END IF

*  do the correction on the data

      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_COREXTC (NPIXEL, DYNAMIC_MEM(AIRPTR),
     :     DYNAMIC_MEM(TAUPTR), DYNAMIC_MEM(INPTR), FBAD,
     :     DYNAMIC_MEM(OUTPTR), STATUS)
      END IF

*  if required do the correction on the error array

      IF (ERRORS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL JCMT_COREXTC (NPIXEL, DYNAMIC_MEM(AIRPTR),
     :         DYNAMIC_MEM(TAUPTR), DYNAMIC_MEM(INEPTR),
     :         FBAD, DYNAMIC_MEM(OUTEPTR), STATUS)
         END IF
      END IF

*  tidy up

 500  IF (DSTAT .NE. 0) THEN
         CALL DTA_ERROR(DSTAT,ERROR)
         CALL PAR_WRUSER (ERROR(:ICH_LEN(ERROR)),IGNORE)
         FAULT=.TRUE.
      END IF

      CALL DSA_CLOSE (STATUS)

      IF (FAULT) CALL FIG_SETERR

      END

