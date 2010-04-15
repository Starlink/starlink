      SUBROUTINE RESTORE
*+
*  Name:
*     RESTORE

*  Purpose:
*     Deconvolve a dual or triple beam map into its equivalent single beam map

*  Language:
*     Starlink Fortran 77 (FIGARO environment)


*  Invocation:
*     CALL RESTORE

*  Description:
*     This is the top level routine that extracts all of the relevant
*     information which is passed to the routines JCMT_DECONV or
*     JCMT_3POS_DECONV depending whether the chop is 2 or 3 position.
*     Will take a chopped beam map in JCMT format and using the Emerson,
*     Klein amd Haslam algorithm produce the equivalent single beam map
*

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JAN-1990 (JBVAD::PAH):
*        Original version.
*      9-JAN-1991 (REVAD::JFL): calls JCMT_DECONV rather than DECONV
*     21-MAY-1991 (REVAD::JFL): several changes, including ability
*        to deconvolve vertical scans
*     23-MAY-1991 (REVAD::JFL): added error array handling
*     26-JUL-1991 (REVAD::JFL): checked it works with both NDF and DST files
*     28-FEB-1992 (REVAD::JFL): modified to use shorter structure names
*      3-MAR-1992 (REVAD::JFL): modified to use default beam UNBAL -1.0
*      3-JUN-1992 (REVAD::JFL): modified to read TEL_BEAM and POS_BEAM
*        from input file
*      6-JUL-1993 (REVAD::JFL): modified to read CHOP_FUN and handle either
*        2 or 3-position chopping
*     22-MAR-1994 (REVAD::HME): Change include statements.
*     13-JUN-1994 (REVAD::HME): Disuse DSA_WRUSER. Add FBAD to argument
*        list of JCMT_DECONV_ERRORS and JCMT_3POS_DECONV_ERRORS.
*     16-JUN-1994 (REVAD::HME): Change maximum dimensionality in
*        DSA_DATA_SIZE call to the parameter MAX_DIM.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'        ! astronomical parameters

*  Dynamic memory include file - defines DYNAMIC_MEM
      INCLUDE 'DYNAMIC_MEMORY'

*  Data structure error codes
      INCLUDE 'DTA_CODES'

*  functions:
      LOGICAL PAR_ABORT          ! check PAR abort flag
      INTEGER DYN_ELEMENT        ! DSA dynamic memory
      INTEGER ICH_LEN            ! Figaro string length function
      INTEGER ICH_FOLD           ! convert to upper case
      INTEGER DSA_TYPESIZE       ! gives number bytes in variable type

*  Status:
      INTEGER STATUS             ! Global status for DSA routines
      INTEGER DSTAT              ! status for dta routines

*  Local Constants:
      INTEGER MAX_DIM            ! max no of dimensions of array
      PARAMETER (MAX_DIM = 2)

*  Local Variables:
      LOGICAL STRUC              ! logical for testing structure
                                 ! existence
      LOGICAL FAULT              ! T if want to signal Figaro error
      LOGICAL ERRORS             ! T if error array present in input data
      INTEGER NDIM               ! number of dimensions of array
      INTEGER DIMS(MAX_DIM)      ! size of each dimension
      INTEGER NELM               ! total number of elements in array
      INTEGER ADDRESS            ! DSA VM address
      INTEGER SLOT               ! DSA slot
      INTEGER INPTR              !  "  VM pointer for input data
      INTEGER OUTPTR             !  "    "      "     output
      INTEGER INEPTR             !  "    "      "     input errors
      INTEGER OUTEPTR            !  "    "      "     output "
      INTEGER CFNPTR             !  "    "      "     convolution function
      INTEGER SWAPPTR            !  "    "      "     work array holding
                                 ! data swapped in x,y
      INTEGER SWAPEPTR           !  "    "      "     work array holding
                                 ! error swapped in x,y
      INTEGER NX                 ! x dimension of image
      INTEGER NY                 ! y dimension of image
      INTEGER CFN_SIZE           ! size of convolution function, =twice length
                                 ! of map axis were scanning along
      INTEGER FSIZE              ! no. bytes in real number
      INTEGER IGNORE             !
      REAL UNBAL                 ! relative amplitude of left/right beams
      REAL FBAD                  ! flag value for bad pixel
      DOUBLE PRECISION PIXSIZE   ! pixel spacing in the x direction in
                                 ! arcseconds
      DOUBLE PRECISION BSEP      ! dual beam separation in arcseconds
      CHARACTER*(64) JCMT_DTA_NAME ! DTA name of the structure
                                 ! containing the JCMT specific
                                 ! data
      CHARACTER*(128) DTA_NAME   ! temp variable for holding DTA names
      CHARACTER*(128) POS_STRUC_NAME ! DTA name of position
                                 ! structure
      CHARACTER*(80) ERROR       ! DTA error description
      CHARACTER*80 MESSAGE       ! output message
      CHARACTER*20 SCAN_DIR      ! direction of scans that make up map;
                                 ! HORIZONTAL or VERTICAL
      CHARACTER*1 POS_BEAM       ! +ve beam, L or R, during map
      CHARACTER*1 TEL_BEAM       ! telescope beam during map
      CHARACTER*15 CHOP_FUN      ! chopping function used in observation
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
      IF ( DSTAT.EQ.DTA_NOTFND .OR. .NOT. STRUC ) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('File is not in JCMT format',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  check that the TSDAT structure is NOT there, this application won't
*  work on data sorted into a time sequence

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'TSDAT', 0, 0, DTA_NAME,
     :   DSTAT)
      CALL DTA_STRUC (DTA_NAME, STRUC, DSTAT)
      IF (DSTAT .EQ. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('File has been sorted into time '//
     :         'sequence, use TS2MAP before RESTORE',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  position structure name

      CALL DTA_CRNAM (JCMT_DTA_NAME, 'MAP', 0, 0, POS_STRUC_NAME,
     :   DSTAT)
      CALL DTA_STRUC (POS_STRUC_NAME, STRUC, DSTAT)
      IF ( DSTAT.EQ.DTA_NOTFND .OR. .NOT. STRUC ) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('File does not contain MAP '//
     :         'information',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  find scan direction, read it in and report it

      CALL JCMT_GETC (POS_STRUC_NAME, 'SCAN_DIR', SCAN_DIR, STATUS)
      IGNORE = ICH_FOLD (SCAN_DIR)
      IF (SCAN_DIR .EQ. 'HORIZONTAL') THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('Map was scanned along x-cells',IGNORE)
         END IF
      ELSE IF (SCAN_DIR .EQ. 'VERTICAL') THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('Map was scanned along y-cells',IGNORE)
         END IF
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('Invalid scan direction (SCAN_DIR).'//
     :         ' Should be HORIZONTAL or VERTICAL',IGNORE)
         END IF
         FAULT = .TRUE.
         GOTO 500
      END IF

*  find pixel separation

      IF (SCAN_DIR .EQ. 'HORIZONTAL') THEN
         CALL JCMT_GETD (POS_STRUC_NAME, 'CELL_X', PIXSIZE, STATUS)
      ELSE
         CALL JCMT_GETD (POS_STRUC_NAME, 'CELL_Y', PIXSIZE, STATUS)
      END IF

*  find chopper function, chopper throw and telescope beam. Issue warning
*  if telescope beam is not M (for middle)

      CALL JCMT_GETC (POS_STRUC_NAME, 'CHOP_FUN', CHOP_FUN, STATUS)
      IGNORE = ICH_FOLD (CHOP_FUN)
      CALL JCMT_GETD (POS_STRUC_NAME, 'CHOP_THRW', BSEP, STATUS)
      CALL JCMT_GETC (POS_STRUC_NAME, 'TEL_BEAM', TEL_BEAM, STATUS)
      IGNORE = ICH_FOLD (TEL_BEAM)
      IF (STATUS .EQ. SAI__OK) THEN
         IF (TEL_BEAM .NE. 'M') THEN
            CALL PAR_WRUSER ('RESTORE - warning, this application '//
     :         'only works properly for maps made with the telescope '//
     :         'in the middle beam.',IGNORE)
         END IF
      END IF

      IF (CHOP_FUN .EQ. 'SQUARE') THEN

*  Find +ve beam of square chop, error if it is not L or R

         CALL JCMT_GETC (POS_STRUC_NAME, 'POS_BEAM', POS_BEAM, STATUS)
         IGNORE = ICH_FOLD (POS_BEAM)
         IF (STATUS .EQ. SAI__OK) THEN
            IF ((POS_BEAM.NE.'L') .AND. (POS_BEAM.NE.'R')) THEN
               CALL PAR_WRUSER ('RESTORE - invalid positive beam.',
     :            IGNORE)
               FAULT = .TRUE.
               GOTO 500
            END IF
         END IF

*  read the ratio of the beam amplitudes

         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_RDVAL ('UNBAL', 0.0, 2.0, 1.0, ' ', UNBAL)
            IF (PAR_ABORT()) THEN
               FAULT = .TRUE.
               GOTO 500
            END IF
            WRITE (MESSAGE, '(''ratio of beams ='', F6.3)') UNBAL
            CALL PAR_WRUSER (MESSAGE, STATUS)
            WRITE(MESSAGE,
     :        '(''separation of beams='',F6.1,'' arcseconds'')') BSEP
            CALL PAR_WRUSER (MESSAGE, STATUS)
         END IF

*  set the sign of UNBAL according to the identity of the +ve beam
*  UNBAL should be amp(lhb) / ab(amp(rhb)) on entry to JCMT_DECONV

         IF (POS_BEAM .EQ. 'R') THEN
            UNBAL = -UNBAL
         END IF
      END IF

*  tell DSA that we want to use magic values, and find the flag for reals

      CALL DSA_USE_FLAGGED_VALUES ('IN', STATUS)
      CALL DSA_GET_FLAG_VALUE ('FLOAT', FBAD, STATUS)

*  map the main data array

      CALL DSA_DATA_SIZE ('IN', MAX_DIM, NDIM, DIMS, NELM, STATUS)
      CALL DSA_MAP_DATA ('IN', 'READ', 'FLOAT', ADDRESS, SLOT, STATUS)
      INPTR = DYN_ELEMENT (ADDRESS)
      NX = DIMS(1)
      NY = DIMS(2)

*  search for an error array, and map it if present

      CALL DSA_SEEK_ERRORS ('IN', ERRORS, STATUS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('IN', 'READ', 'FLOAT', ADDRESS, SLOT,
     :      STATUS)
         INEPTR = DYN_ELEMENT (ADDRESS)
      END IF

*  get workspace to hold the convolution functions used

      IF (SCAN_DIR .EQ. 'HORIZONTAL') THEN
         CFN_SIZE = 2 * NX
      ELSE
         CFN_SIZE = 2 * NY
      END IF
      CALL DSA_GET_WORK_ARRAY (CFN_SIZE, 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      CFNPTR = DYN_ELEMENT(ADDRESS)

*  open output file, force a new structure to be created

      CALL DSA_OUTPUT ('OUT', 'OUTPUT', 'IN', 0, 1, STATUS)

*  use flagged values

      CALL DSA_USE_FLAGGED_VALUES ('OUT', STATUS)

*  map the output data array, and errors if present

      CALL DSA_MAP_DATA ('OUT', 'UPDATE', 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      OUTPTR = DYN_ELEMENT(ADDRESS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('OUT', 'UPDATE', 'FLOAT', ADDRESS,
     :      SLOT, STATUS)
         OUTEPTR = DYN_ELEMENT (ADDRESS)
      END IF

*  set flag to indicate may be magic values in output array

      CALL DSA_SET_FLAGGED_VALUES ('OUT', .TRUE., STATUS)

      IF (SCAN_DIR .EQ. 'HORIZONTAL') THEN

*  just call routine to do deconvolution

         IF (STATUS .EQ. SAI__OK) THEN

            IF (CHOP_FUN .EQ. 'TRIPOS') THEN

               IF (.NOT. ERRORS) THEN

                  CALL JCMT_3POS_DECONV (DYNAMIC_MEM(INPTR), NX, NY,
     :              FBAD, PIXSIZE, BSEP, DYNAMIC_MEM(CFNPTR),
     :              DYNAMIC_MEM(OUTPTR), STATUS)

               ELSE

                  CALL JCMT_3POS_DECONV_ERRORS (DYNAMIC_MEM(INPTR),
     :              DYNAMIC_MEM(INEPTR), NX, NY, FBAD, PIXSIZE, BSEP,
     :              DYNAMIC_MEM(CFNPTR), DYNAMIC_MEM(OUTPTR),
     :              DYNAMIC_MEM(OUTEPTR), STATUS)

               END IF


            ELSE

               IF (.NOT. ERRORS) THEN

                  CALL JCMT_DECONV (DYNAMIC_MEM(INPTR), NX, NY, FBAD,
     :              PIXSIZE, BSEP, UNBAL, DYNAMIC_MEM(CFNPTR),
     :              DYNAMIC_MEM(OUTPTR), STATUS)

               ELSE

                  CALL JCMT_DECONV_ERRORS (DYNAMIC_MEM(INPTR),
     :              DYNAMIC_MEM(INEPTR), NX, NY, FBAD, PIXSIZE, BSEP,
     :              UNBAL, DYNAMIC_MEM(CFNPTR), DYNAMIC_MEM(OUTPTR),
     :              DYNAMIC_MEM(OUTEPTR), STATUS)

               END IF

            END IF
         END IF

      ELSE

         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('Deconvolution of VERTICAL scan data '//
     :         'has not been tested. Even if the '//
     :         'program does not crash you should '//
     :         'check your results.',IGNORE)
         END IF

*  JCMT_DECONV, the routine that actually does the deconvolving,
*  assumes that the chop was along the x direction of the input
*  array so, first get some workspace and copy into it the data
*  such that the original y axis is now the x axis...

         CALL DSA_GET_WORK_ARRAY (NX*NY, 'FLOAT', ADDRESS, SLOT,
     :      STATUS)
         SWAPPTR = DYN_ELEMENT(ADDRESS)
         IF (ERRORS) THEN
            CALL DSA_GET_WORK_ARRAY (NX*NY, 'FLOAT', ADDRESS, SLOT,
     :         STATUS)
            SWAPEPTR = DYN_ELEMENT(ADDRESS)
         END IF
         FSIZE = DSA_TYPESIZE ('FLOAT', STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL JCMT_SWAPXY (DYNAMIC_MEM(INPTR), NX, NY,
     :         DYNAMIC_MEM(SWAPPTR), STATUS)

*  call routine to do deconvolution

            IF (CHOP_FUN .EQ. 'TRIPOS') THEN

               CALL JCMT_3POS_DECONV (DYNAMIC_MEM(SWAPPTR), NY, NX,
     :           FBAD, PIXSIZE, BSEP, DYNAMIC_MEM(CFNPTR),
     :           DYNAMIC_MEM(OUTPTR), STATUS)

            ELSE

               CALL JCMT_DECONV (DYNAMIC_MEM(SWAPPTR), NY, NX, FBAD,
     :           PIXSIZE, BSEP, UNBAL, DYNAMIC_MEM(CFNPTR),
     :           DYNAMIC_MEM(OUTPTR), STATUS)

            END IF

*  copy the deconvolve result back into the workspace

            CALL GEN_MOVE (FSIZE*NX*NY, DYNAMIC_MEM(OUTPTR),
     :         DYNAMIC_MEM(SWAPPTR))

*  and swap back into the output array

            CALL JCMT_SWAPXY (DYNAMIC_MEM(SWAPPTR), NY, NX,
     :         DYNAMIC_MEM(OUTPTR), STATUS)

*  same process for errors if present

            IF (ERRORS) THEN
               CALL JCMT_SWAPXY (DYNAMIC_MEM(INPTR), NX, NY,
     :            DYNAMIC_MEM(SWAPPTR), STATUS)
               CALL JCMT_SWAPXY (DYNAMIC_MEM(INEPTR), NX, NY,
     :         DYNAMIC_MEM(SWAPEPTR), STATUS)
               IF (CHOP_FUN .EQ. 'TRIPOS') THEN
                  CALL JCMT_3POS_DECONV_ERRORS (DYNAMIC_MEM(SWAPPTR),
     :              DYNAMIC_MEM(SWAPEPTR), NY, NX, FBAD, PIXSIZE, BSEP,
     :              DYNAMIC_MEM(CFNPTR), DYNAMIC_MEM(OUTPTR),
     :              DYNAMIC_MEM(OUTEPTR), STATUS)
               ELSE
                  CALL JCMT_DECONV_ERRORS (DYNAMIC_MEM(SWAPPTR),
     :              DYNAMIC_MEM(SWAPEPTR), NY, NX, FBAD, PIXSIZE, BSEP,
     :              UNBAL, DYNAMIC_MEM(CFNPTR), DYNAMIC_MEM(OUTPTR),
     :              DYNAMIC_MEM(OUTEPTR), STATUS)
               END IF
               CALL GEN_MOVE (FSIZE*NX*NY, DYNAMIC_MEM(OUTPTR),
     :            DYNAMIC_MEM(SWAPPTR))
               CALL GEN_MOVE (FSIZE*NX*NY, DYNAMIC_MEM(OUTEPTR),
     :            DYNAMIC_MEM(SWAPEPTR))
               CALL JCMT_SWAPXY (DYNAMIC_MEM(SWAPPTR), NY, NX,
     :            DYNAMIC_MEM(OUTPTR), STATUS)
               CALL JCMT_SWAPXY (DYNAMIC_MEM(SWAPEPTR), NY, NX,
     :            DYNAMIC_MEM(OUTEPTR), STATUS)
            END IF

         END IF

      END IF


*  tidy up

 500  IF (DSTAT.NE.0) THEN
         CALL DTA_ERROR(DSTAT,ERROR)
         CALL PAR_WRUSER (ERROR(:ICH_LEN(ERROR)),IGNORE)
         FAULT=.TRUE.
      END IF
      CALL DSA_CLOSE (STATUS)
      IF (FAULT) CALL FIG_SETERR

      END


