      SUBROUTINE DREAM2SURF ( STATUS )
*+
*  Name:
*     DREAM2SURF

*  Purpose:
*     Convert DREAM output data into SURF data format

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DREAM2SURF ( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This routine reads in a DREAM format file and converts it
*     into a file format that can be processed by SURF.
*     This new file will have been 'REDUCE_SWITCH'ed and 
*     'FLATFIELD'ed.

*  Usage:
*     dream2surf in out

*  ADAM Parameters:
*     FLATFILE = CHAR (Read)
*        File containing the flatfield description
*     FSCYCLE = INTEGER (Read)
*        First cycle number to read from file
*     IN = FILE (Read)
*        The name of the DREAM input file to open.
*     MSG_FILTER = CHAR (Read)
*         Message filter level. Default is NORM.
*     NRCYCLE = INTEGER (Read)
*         Number of cycles to read from file
*     OUT = NDF (Write)
*        The NDF output file.

*  Examples:

*  Notes:

*  Authors:
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)
*     GREVE: H.W. van Someren Greve (greve@nfra.nl)

*  Copyright:
*     Copyright (C) 1998 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  History:
*     $Log$
*     Revision 1.4  1999/08/03 19:32:28  timj
*     Add copyright message to header.
*
*     Revision 1.3  1998/06/24 19:26:03  timj
*     Finally get jiggle pattern to work for new format sol files.
*
*     Revision 1.2  1998/06/19 19:28:26  timj
*     First released version
*
*     Revision 1.1  1998/05/14 20:41:03  timj
*     Initial revision
*

*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE
 
*  Global constants:
      INCLUDE 'SAE_PAR'       ! Starlink status
      INCLUDE 'DAT_PAR'       ! DAT__ constants
      INCLUDE 'PRM_PAR'       ! For VAL__
      INCLUDE 'MSG_PAR'       ! For MSG_

      INCLUDE 'SCU_SOL'       ! Description of DREAM header file
      INCLUDE 'SURF_PAR'      ! Standard SCUBA include

*  COMMON data
      COMMON SOLPA            ! DREAM common block

*  Status:
      INTEGER STATUS

*  External references
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

*  Local Constants:
      INTEGER       DREAM__NBYTES
      PARAMETER     (DREAM__NBYTES = 4)  ! Number of bytes per DREAM record
      INTEGER       SRECSIZE                 
      PARAMETER     (SRECSIZE = 4096)          ! Small record size in bytes
      INTEGER       HEADER
      PARAMETER     (HEADER   = 5120)          ! Nr of I4 in Header records
      CHARACTER*(10) TSKNAME                   ! Task name
      PARAMETER (TSKNAME = 'DREAM2SURF')

      DOUBLE PRECISION LAT_OBS           ! Latitude of JCMT (degrees)
      PARAMETER (LAT_OBS = 19.8258323669)
      DOUBLE PRECISION LONG_OBS          ! Longitude of JCMT (degrees)
      PARAMETER (LONG_OBS = 204.520278931)
       


*  Local variables:
      INTEGER ADC             ! A to D number
      REAL             BOL_CALB (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! bolometer flatfield factors
      DOUBLE PRECISION BOL_DAY (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! time and day number on which
                                                 ! the bolometer flatfield was
                                                 ! measured
      REAL             BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! Nasmyth dU3 coords of 
                                                 ! bolometers
      REAL             BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! Nasmyth dU4 coords of 
                                                 ! bolometers
      INTEGER          BOL_QUAL (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! bolometer flatfield quality
      CHARACTER*20     BOL_REF (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! flatfield reference
                                                 ! bolometers
      REAL             BOL_RTEMP (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! scratch real bolometer data
      INTEGER          BOL_RUN (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! run number on which the
                                                 ! bolometer flatfield was
                                                 ! measured
      CHARACTER*20     BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                                 ! bolometer types
      INTEGER BOL_ADC ( SCUBA__NUM_ADC * SCUBA__NUM_CHAN ) ! ADC numbers
      INTEGER BOL_CHAN ( SCUBA__NUM_ADC * SCUBA__NUM_CHAN ) ! Channel numbers
      CHARACTER *10 CENT_CRD  ! Centre coordinate system
      INTEGER CHAN            ! Channel number
      CHARACTER * 80 CTEMP    ! Scratch string
      DOUBLE PRECISION DEC    ! Declination
      INTEGER DEMBNDS ( 4 )   ! Dimensions of DEM_PNTR
      INTEGER DEM_PNTR        ! Pointer to DEM_PNTR extension
      INTEGER DIM ( 4 )       ! Dimensions of an array
      DOUBLE PRECISION DLST   ! Time per cycle
      INTEGER DREAM_PTR       ! Pointer to DREAM input data
      DOUBLE PRECISION DTEMP  ! Scratch double
      CHARACTER * (80) FITS (SCUBA__MAX_FITS)  ! FITS extension
      INTEGER LBND ( 2 )      ! Lower bounds of output NDF
      INTEGER LUN             ! Logical unit number of input file
      INTEGER ERR             ! Error from DREAM system
      INTEGER FD              ! File descriptor of input
      CHARACTER*80 FLATFILE   ! Flatfield file name
      INTEGER FSCYCLE         ! First cycle to read from input file
      INTEGER I               ! Loop integer
      INTEGER IERR            ! For VEC_
      INTEGER IPOSN           ! Position in string
      INTEGER ITEMP           ! Scratch integer
      REAL    JIGL_X ( SCUBA__MAX_JIGGLE ) ! X jiggle positions
      REAL    JIGL_Y ( SCUBA__MAX_JIGGLE ) ! Y jiggle positions
      DOUBLE PRECISION LONGITUDE ! Longitude degrees west
      DOUBLE PRECISION LST    ! LST of observation
      DOUBLE PRECISION LST_STRT ! Start LST of observation (decimal hours)
      INTEGER MAXCYC          ! Maximum number of cycles that can be read
      DOUBLE PRECISION MJD    ! MJD of observation
      INTEGER NBYTES          ! Number of bytes per data record
      INTEGER NDIM            ! Number of dimensions in an array
      INTEGER NERR            ! For VEC_
      INTEGER NRCYCLE         ! Number of cycles to read
      INTEGER NREC            ! Number of records per cycle
      INTEGER NSVAL           ! Number of solved values per cycle
      INTEGER N_FITS          ! Number of FITS components
      INTEGER OFFSET          ! Y offset in output array
      INTEGER OUT_A_PTR       ! Pointer to axis
      INTEGER OUT_NDF         ! Output NDF identifier
      INTEGER OUT_PTR         ! Pointer to output data (NDF)
      INTEGER OUT_QUAL_PTR    ! Pointer to quality array
      INTEGER OUT_VAR_PTR     ! Pointer to variance array
      CHARACTER*(DAT__SZLOC) OUT_SCUCD_LOC ! Locator to SCUCD extension
      CHARACTER*(DAT__SZLOC) OUT_SCUBA_LOC ! Locator to SCUBA extension
      CHARACTER*(DAT__SZLOC) OUT_REDS_LOC ! Locator to REDS extension
      CHARACTER*(DAT__SZLOC) OUT_FIG_LOC ! Locator to FIGARO extension
      CHARACTER*(DAT__SZLOC) OUT_FITS_LOC ! Locator to FITS extension
      DOUBLE PRECISION RA     ! Right ascension of source
      INTEGER RECIN           ! Current input record
      INTEGER RECORD          ! Record number in input file
      INTEGER RECSS           ! Record size in words
      CHARACTER *20 RUNNO     ! Run number in 0 padded form (eg 0015) 
      CHARACTER * 80 STEMP    ! Scratch string
      INTEGER UBND ( 2 )      ! Upper bounds of output NDF

*-

      IF (STATUS .NE. SAI__OK) RETURN

*     Read in the DREAM file (currently assume that 
*     user will supply full path or use $DREAM_OUT

      CALL RIO_ASSOC('IN', 'READ', 'UNFORMATTED', SRECSIZE, FD,
     :     STATUS)

*     Since we are using the DREAM I/O routines we need to get
*     the file unit number
      CALL FIO_UNIT(FD, LUN, STATUS)

*     Read header into common block

      IF (STATUS .EQ. SAI__OK) THEN
         RECORD = 1             ! Record number 1
         RECSS = SRECSIZE / DREAM__NBYTES ! Output record size in words

         CALL DISK_IO (2, LUN, RECSS, RECORD, SOLPA, HEADER, ERR)

         IF (ERR .NE. 0) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TSK', TSKNAME)
            CALL MSG_SETI('ERR', ERR)
            CALL MSG_SETI('REC', RECORD)
            CALL ERR_REP(' ','^TSK: Error ^ERR in reading record'//
     :           ' ^REC', STATUS)
         END IF
      END IF

*     Ask for the first cycle to select
      CALL PAR_GDR0I('FSCYCLE', 1, 1, r_Ncycle, .FALSE., FSCYCLE,
     :     STATUS)

*     Ask for the number of cycles to be selected
      MAXCYC = r_Ncycle - FSCYCLE + 1
      CALL PAR_GDR0I('NRCYCLE', MAXCYC, 1, MAXCYC, .FALSE., 
     :     NRCYCLE, STATUS)

*     Now we can start doing things.
*     Calculate total number of pixel values per cycle

      NSVAL = r_Nbol * MAX_PIX

      IF (NSVAL .LE. 0) THEN

         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TSK', TSKNAME)
            CALL MSG_SETI('NPT', NSVAL)
            CALL ERR_REP(' ','^TSK: Number of observed points'//
     :           'is too small (^NPT)', STATUS)
         END IF
      END IF

*     Start NDF
      CALL NDF_BEGIN

*     Open an NDF file
*     Default file name constructed from the UT date

*     Year
      CALL CHR_ITOC(GR_YY, STEMP, ITEMP)
      IPOSN = CHR_LEN(STEMP)

*     Month (pad with leading zero)
      IF (GR_MN .LT. 10) CALL CHR_APPND('0', STEMP, IPOSN)
      CALL CHR_ITOC(GR_MN, CTEMP, ITEMP)
      CALL CHR_APPND(CTEMP, STEMP, IPOSN)

*     Day
      IF (GR_DD .LT. 10) CALL CHR_APPND('0', STEMP, IPOSN)
      CALL CHR_ITOC(GR_DD, CTEMP, ITEMP)
      CALL CHR_APPND(CTEMP,STEMP,IPOSN)

*     Find observation number from filename
*     Assumes string of form xxx_NNNN

      ITEMP = CHR_LEN ( SOLVE_DATA )
      RUNNO = SOLVE_DATA(ITEMP-3:)

*     Append an _
      CALL CHR_APPND('_', STEMP, IPOSN)

*     Append run number to out
      CALL CHR_APPND(RUNNO, STEMP, IPOSN)

*     Append DREAM signature
      CALL CHR_APPND('_drm', STEMP, IPOSN)


*     Set default
      CALL PAR_DEF0C('OUT', STEMP, STATUS)

*     Bounds of NDF
      LBND(1) = 1
      LBND(2) = 1
      UBND(1) = R_NBOL
      UBND(2) = NPIX * NRCYCLE

      CALL NDF_CREAT('OUT', '_REAL', 2, LBND, UBND, OUT_NDF, STATUS)

*     Map the output data array (plus the other arrays for SURF
*     compatibility)
      CALL NDF_MAP(OUT_NDF, 'QUALITY','_UBYTE', 'WRITE/ZERO',
     :     OUT_QUAL_PTR, ITEMP, STATUS)
      CALL NDF_MAP(OUT_NDF, 'DATA', '_REAL', 'WRITE', OUT_PTR,
     :     ITEMP, STATUS)
      CALL NDF_MAP(OUT_NDF, 'VARIANCE', '_REAL', 'WRITE/ZERO',
     :     OUT_VAR_PTR, ITEMP, STATUS)


*     Get some memory to store the data from each cycle
*     We are using 4 byte words but define in parameter

      NBYTES = MAX(DREAM__NBYTES * NSVAL, SRECSIZE)
      CALL PSX_MALLOC(NBYTES, DREAM_PTR, STATUS)

*     Loop over cycles

*     Calculate number of records per cycle
      NREC = (NSVAL + RECSS - 1 ) / RECSS

*     Loop
      DO I = FSCYCLE, NRCYCLE

         IF (STATUS .EQ. SAI__OK) THEN

*     Determine inpur record number
            RECIN = HEAD_S / RECSS + (I * NREC)

*     Read pixel intensities into memory
            CALL DISK_IO (2, LUN, RECSS, RECIN, %VAL(DREAM_PTR),
     :           NSVAL, ERR)

            IF (ERR .NE. 0) THEN
               print *,'in if ',ERR, STATUS
               STATUS = SAI__ERROR
               CALL MSG_SETC('TSK', TSKNAME)
               CALL MSG_SETI('ERR', ERR)
               CALL MSG_SETI('REC', RECIN)
               CALL ERR_REP(' ','^TSK: Error ^ERR in reading record'//
     :              ' ^REC', STATUS)
            END IF

*     Calculate current offset position in output array (time axis)
            OFFSET = (I - FSCYCLE) * NPIX + 1

*     Now we need to write this data to the NDF file
            CALL DREAM_DATA_TO_SURF_DATA(R_NBOL, MAX_PIX,
     :           NPIX * NRCYCLE,
     :           OFFSET, INT_POS, %VAL(DREAM_PTR), %VAL(OUT_PTR),
     :           STATUS)

         END IF

      END DO

*     Free the memory assoicated with each cycle
      CALL PSX_FREE(DREAM_PTR, STATUS)

*     Close the input file
      CALL RIO_CLOSE(FD, STATUS)

*     Unmap the data arrays
      CALL NDF_UNMAP(OUT_NDF, '*', STATUS)

*     Axis information is simply integration number.
*     Steal this from REDUCE_SWITCH
*
*     Axis 1: bolometers   2: Data

*     Deal with BOLOMETER axis
 
      CALL NDF_AMAP(OUT_NDF, 'CENTRE', 1, '_INTEGER', 'WRITE',
     :     OUT_A_PTR, ITEMP, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_NFILLI (R_NBOL, %val(OUT_A_PTR))
      END IF
      CALL NDF_ACPUT ('Bolometer', OUT_NDF, 'LABEL', 1, STATUS)
      CALL NDF_AUNMP (OUT_NDF, 'CENTRE', 1, STATUS)

*     Integrations
      CALL NDF_AMAP (OUT_NDF, 'CENTRE', 2, '_REAL', 'WRITE',
     :     OUT_A_PTR, ITEMP, STATUS)
 
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_NFILLR (ITEMP, %val(OUT_A_PTR))
         CALL SCULIB_MULCAR(ITEMP, %VAL(OUT_A_PTR), 1.0/REAL(NPIX),
     :        %VAL(OUT_A_PTR))
      END IF

      CALL NDF_ACPUT ('Integration', OUT_NDF, 'LABEL', 2, STATUS)
      CALL NDF_AUNMP (OUT_NDF, 'CENTRE', 2, STATUS)


*     Now we can start writing header information to the file

*     SCUCD extension
      CALL NDF_XNEW(OUT_NDF, 'SCUCD', 'SCUCD_ST', 0, 0,
     :     OUT_SCUCD_LOC, STATUS)

*     SCUBA extension
      CALL NDF_XNEW(OUT_NDF, 'SCUBA', 'SCUBA_ST', 0, 0,
     :     OUT_SCUBA_LOC, STATUS)

*     REDS extension
      CALL NDF_XNEW(OUT_NDF, 'REDS', 'SURF_EXTENSION', 0, 0,
     :     OUT_REDS_LOC, STATUS)

*     FIGARO extension (for completeness)
      CALL NDF_XNEW(OUT_NDF, 'FIGARO', 'FIGARO_EXT', 0, 0,
     :     OUT_FIG_LOC, STATUS)
      CALL DAT_ANNUL(OUT_FIG_LOC, STATUS)

*     First write DEM_PNTR array
*     Create the component
*     Note that DEM_PNTR is always 1 dimensional in this
*     since there are no switches, exposures or measurements.
*     Only has 3 dimensions since there are no switches

      DEMBNDS ( 1 ) = 1
      DEMBNDS ( 2 ) = NRCYCLE
      DEMBNDS ( 3 ) = 1
      CALL CMP_MOD(OUT_SCUBA_LOC, 'DEM_PNTR', '_INTEGER', 3,
     :     DEMBNDS, STATUS)

*     Map it (rather do this as otherwise I need to create
*     the array on the fly
      CALL CMP_MAPV(OUT_SCUBA_LOC, 'DEM_PNTR', '_INTEGER',
     :     'WRITE', DEM_PNTR, ITEMP, STATUS)

*     Loop over cycles again
      DO I = 1, NRCYCLE
         OFFSET = (I-1) * NPIX + 1
         CALL VEC_ITOI(.FALSE., 1, OFFSET,
     :        %VAL(DEM_PNTR + ((I-1) * VAL__NBI)), IERR, NERR,
     :        STATUS)
      END DO

*     Unmap DEM_PNTR
      CALL CMP_UNMAP(OUT_SCUBA_LOC, 'DEM_PNTR', STATUS)

*     Write LST information (Same dimensions as DEM_PNTR)
      DEMBNDS ( 1 ) = 1
      DEMBNDS ( 2 ) = 1
      DEMBNDS ( 3 ) = NRCYCLE
      DEMBNDS ( 4 ) = 1
      CALL CMP_MOD(OUT_SCUCD_LOC, 'LST_STRT', '_DOUBLE', 4,
     :     DEMBNDS, STATUS)

*     Map it (rather do this as otherwise I need to create
*     the array on the fly (re-use DEM_PNTR variable)
      CALL CMP_MAPV(OUT_SCUCD_LOC, 'LST_STRT', '_DOUBLE',
     :     'WRITE', DEM_PNTR, ITEMP, STATUS)

*     This is based on STIME
*     Returns LST in radians and the MJD
*     Dont know why John chose degrees east of meridian
*     convert back to degrees west.
      LONGITUDE =  LONG_OBS - 360.0D0

      CALL LST_FROM_UT(GR_YY, GR_MN, GR_DD, UT_HH, UT_MN, UT_SS,
     :     LONGITUDE, LST_STRT, MJD, STATUS)

*     increment in LST per cycle
*     I think this is stored in cycle_t and is in millisec in the header
*     Convert to radians
      DLST = (CYCLE_T / (1000.0D0 * 3600.0D0)) * 15.0D0 * PI / 180.0D0

*     Loop over cycles
      DO I = FSCYCLE, NRCYCLE
         LST = LST_STRT + (DBLE(I-1) * DLST)
         
         CALL VEC_DTOD(.FALSE., 1, LST,
     :        %VAL(DEM_PNTR + (I-FSCYCLE) * VAL__NBD), IERR, NERR,
     :        STATUS)

      END DO

*     Unmap LST_STRT
      CALL CMP_UNMAP(OUT_SCUCD_LOC, 'LST_STRT', STATUS)

*     Now deal with jiggle patterns
*     Create the JIGL_X and JIGL_Y components
      CALL CMP_MOD(OUT_SCUCD_LOC, 'JIGL_X', '_REAL', 1,
     :     NPIX, STATUS)
      CALL CMP_MOD(OUT_SCUCD_LOC, 'JIGL_Y', '_REAL', 1,
     :     NPIX, STATUS)

*     Now loop over jiggle positions and store the relevant
*     ones (> -1)

*     SURF requires that bolometer coordinates are derived by
*          Xpos = Bol_Xpos - Jigl_X
*          Ypos = Bol_Ypos - Jigl_Y
*
*     DREAM currently assumes
*          Xpos = -Bol_Xpos + Jigl_X
*          Ypos =  Bol_Ypos - Jigl_Y

*     I end up doing this
*          Invert x positions in flatfield (nasty)
*          Negate jiggle position for X and Y

      ITEMP = 0
      DO I = 0, MAX_PIX  - 1
         IF (INT_POS(I) .NE. - 1) THEN 
            ITEMP = ITEMP + 1
            JIGL_X(ITEMP) = JIG_X(I)
            JIGL_Y(ITEMP) = JIG_Y(I)
         END IF
      END DO


*     Write the jiggle pattern to the extensions
      CALL CMP_PUT1R(OUT_SCUCD_LOC, 'JIGL_X', NPIX, JIGL_X, STATUS)
      CALL CMP_PUT1R(OUT_SCUCD_LOC, 'JIGL_Y', NPIX, JIGL_Y, STATUS)


*     Now work out the order in which the bolometers are stored.

*     Create the BOL_CHAN and BOL_ADC extensions
      CALL CMP_MOD(OUT_SCUBA_LOC, 'BOL_CHAN', '_INTEGER', 1,
     :     R_NBOL, STATUS)
      CALL CMP_MOD(OUT_SCUBA_LOC, 'BOL_ADC', '_INTEGER', 1,
     :     R_NBOL, STATUS)


*     Need to loop over input bolometers and decode the name
*     DREAM stores the storage order in BOL_ORDER and the
*     name in BOL_NAME

      DO I = 0, R_NBOL - 1

*     Determine the ADC and Channel number from the name
         CALL SCULIB_BOLDECODE ( BOL_NAME(I), ADC, CHAN, STATUS)

*     Now put these values into the correct slot of BOL_CHAN and BOL_ADC
*     (Am I supposed to use BOL_ORDER?)
         BOL_CHAN ( I + 1) = CHAN
         BOL_ADC ( I + 1) = ADC
*         BOL_CHAN ( BOL_ORDER(I) + 1) = CHAN
*         BOL_ADC ( BOL_ORDER(I) + 1) = ADC

      END DO

*     Write the bolometer order to the extensions
      CALL CMP_PUT1I(OUT_SCUBA_LOC, 'BOL_ADC', R_NBOL, BOL_ADC, STATUS)
      CALL CMP_PUT1I(OUT_SCUBA_LOC, 'BOL_CHAN', R_NBOL, BOL_CHAN,STATUS)

*     Read flatfield from a text file
      CALL PAR_GET0C ('FLATFILE', FLATFILE, STATUS)

      CALL SCULIB_READBOLS (FLATFILE, SCUBA__NUM_CHAN,
     :     SCUBA__NUM_ADC, BOL_TYPE, BOL_DU3, BOL_DU4, BOL_CALB,
     :     BOL_RTEMP, BOL_RTEMP, BOL_RTEMP, BOL_QUAL, BOL_DAY,
     :     BOL_RUN, BOL_REF, STATUS)

*     Now write the flatfield information to the extensions
      NDIM = 2
      DIM (1) = SCUBA__NUM_CHAN
      DIM (2) = SCUBA__NUM_ADC

*     Create the extensions
      CALL CMP_MODC(OUT_SCUBA_LOC, 'BOL_TYPE', 20, NDIM, DIM, STATUS)
      CALL CMP_MOD(OUT_SCUBA_LOC, 'BOL_DU3', '_REAL', NDIM, DIM,
     :     STATUS)
      CALL CMP_MOD(OUT_SCUBA_LOC, 'BOL_DU4', '_REAL', NDIM, DIM,
     :     STATUS)

*     Only really interested in the BOL_TYPE and DU3, DU4 arrays
*     Write the data
      CALL CMP_PUTNC (OUT_SCUBA_LOC, 'BOL_TYPE', NDIM, DIM, BOL_TYPE,
     :     DIM, STATUS)
      CALL CMP_PUTNR (OUT_SCUBA_LOC, 'BOL_DU3', NDIM, DIM, BOL_DU3,
     :     DIM, STATUS)
      CALL CMP_PUTNR (OUT_SCUBA_LOC, 'BOL_DU4', NDIM, DIM, BOL_DU4,
     :     DIM, STATUS)



*     FITS EXTENSION -------------------
      N_FITS = 0  ! No FITS components to start with

*     Object name (default to something if nothing in header)
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'OBJECT', 'DREAM', 'Name of object', STATUS)

*     RUN number
      CALL CHR_CTOI(RUNNO, ITEMP, STATUS)
      CALL SCULIB_PUT_FITS_I(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'RUN', ITEMP, 'Run number of observation', STATUS)

*     Observation, Sample mode (Always MAP/JIGGLE) and sample coords
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MODE', 'MAP', 'The type of observation', STATUS)
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SAM_MODE', 'JIGGLE', 'Sampling method', STATUS)
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SAM_CRDS', 'NA', 'Coordinate system of sampling mesh', 
     :     STATUS)
      CALL SCULIB_PUT_FITS_I(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SAM_PA', -1, 'Scan P.A. rel. to lat. line; 0=lat, 90=long', 
     :     STATUS)

*     Coordinates of observation

*     Declination
*     Convert to string D:M:S
      IF (DECSN .GE. 0.0D0) THEN
         STEMP = '+'
      ELSE
         STEMP = '-'
      END IF
      IPOSN = 1

      DEC = DBLE(DECDD) + (DBLE(DECMN)/60.0D0) + (DBLE(DECSS)/3600.0D0)
      CALL CHR_RTOAN(REAL(DEC), 'DEGREES', STEMP, IPOSN)

*     Store in LAT
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'LAT', STEMP, 'Object Latitude', STATUS)

*     Store in token
      CALL MSG_SETC('DEC', STEMP)

*     Determine Right ascension 
      RA = DBLE(RAHH) + (DBLE(RAMN)/60.0D0) + (DBLE(RASS)/3600.0D0)
      IPOSN = 0
      CALL CHR_RTOAN(REAL(RA), 'HOURS', STEMP, IPOSN)

*     Store in LONG
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'LONG', STEMP, 'Object longitude', STATUS)

*     Store in message token
      CALL MSG_SETC('RA', STEMP)

*     Write coordinates to display
      CALL MSG_SETC('TSK',TSKNAME)
      CALL MSG_OUTIF(MSG__NORM, ' ', '^TSK: Coordinates: '//
     :     '^RA, ^DEC', STATUS)


*     Need to ask for coordinate frame of the tracking centre
      CALL PAR_CHOIC('COORDS', 'RB', 'AZ,RD,RB,GA,RJ', .TRUE.,
     :     CENT_CRD, STATUS)

      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CENT_CRD',CENT_CRD,'Centre coordinate system', STATUS)



*     UT Date.
*     Convert Year
      CALL CHR_ITOC(GR_YY, STEMP, ITEMP)
      IPOSN = CHR_LEN(STEMP)
      CALL CHR_APPND(':',STEMP, IPOSN)

*     Convert month
      CALL CHR_ITOC(GR_MN, CTEMP, ITEMP)
      CALL CHR_APPND(CTEMP, STEMP, IPOSN)
      CALL CHR_APPND(':',STEMP, IPOSN)

*     Convert day
      CALL CHR_ITOC(GR_DD, CTEMP, ITEMP)
      CALL CHR_APPND(CTEMP, STEMP, IPOSN)

*     Write the MJD
      CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MJD-OBS', MJD, 'Modified Julian Date of obsstart',
     :     STATUS)

*     Write the date
*      STEMP = '1998:1:1'
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'UTDATE', STEMP, 'UT Date of observation', STATUS)     



*     Write the UT time
*     Convert hour
      CALL CHR_ITOC(UT_HH, STEMP, ITEMP)
      IPOSN = CHR_LEN(STEMP)
      CALL CHR_APPND(':',STEMP, IPOSN)

*     Convert minute
      CALL CHR_ITOC(UT_MN, CTEMP, ITEMP)
      CALL CHR_APPND(CTEMP, STEMP, IPOSN)
      CALL CHR_APPND(':',STEMP, IPOSN)

*     Convert second
      CALL CHR_ITOC(UT_SS, CTEMP, ITEMP)
      CALL CHR_APPND(CTEMP, STEMP, IPOSN)

      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'UTSTART', STEMP, 'UT at start of observation', STATUS)

*     Convert LST (Hours minutes seconds) into a time
*     First convert LST_STRT to hours (should be radians to this point)
      LST_STRT = LST_STRT * 180.0D0 / (PI * 15.0D0)

      IPOSN = 0
      CALL CHR_DTOAN(LST_STRT, 'HOURS', STEMP, IPOSN)

*     and write it out
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'STSTART', STEMP, 'ST at start of observation', STATUS)


*     Also store STEND
      IPOSN = 0

      DTEMP = LST_STRT + (DLST * DBLE(FSCYCLE + NRCYCLE - 1) 
     :     * 180.0D0 / (PI * 15.0D0))
      CALL CHR_DTOAN(DTEMP,
     :     'HOURS',STEMP, IPOSN)
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'STEND', STEMP, 'ST at start of observation', STATUS)

*     exposure time per sample
      CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'EXP_TIME', DBLE(CYCLE_T / (1000.0 * REAL(NPIX))),
     :     'Exposure time for each basic measurement (sec)',
     :     STATUS)


*     Set up MAP_X and MAP_Y offsets
      CALL SCULIB_PUT_FITS_I(SCUBA__MAX_FITS, N_FITS, FITS,
     :      'MAP_X', 0.0, 
     :      'Map X offset from telescope centre (arcsec)',
     :      STATUS)
      CALL SCULIB_PUT_FITS_I(SCUBA__MAX_FITS, N_FITS, FITS,
     :      'MAP_Y', 0.0, 
     :      'Map Y offset from telescope centre (arcsec)',
     :      STATUS)

*     Number of bolometers
      CALL SCULIB_PUT_FITS_I(SCUBA__MAX_FITS, N_FITS, FITS,
     :      'N_BOLS', R_NBOL, 
     :      'Number of bolometers selected', STATUS)

*     State of the observation (anything except ABORT)
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'STATE','Terminating', 'SCUCD state', STATUS)

*     Version of SCUCD (set to 0 for now)
      CALL SCULIB_PUT_FITS_I(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'VERSION', 0, 'SCUCD version (DREAM data)',STATUS)

*     Jiggle info
      CALL SCULIB_PUT_FITS_I(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'JIGL_CNT', NPIX, 'Number of offsets in a jiggle pattern',
     :     STATUS)
      CALL SCULIB_PUT_FITS_I(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'J_PER_S', NPIX, 'Number of jiggles per switch',
     :     STATUS)
      CALL SCULIB_PUT_FITS_I(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'J_REPEAT', NPIX, 'No. of jiggle pattern repeats in switch',
     :     STATUS)

*     Center of array
      CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CNTR_DU3', 0.0D0, 'Nasmyth dU3 coord of instrument centre',
     :     STATUS)
      CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CNTR_DU4', 0.0D0, 'Nasmyth dU4 coord of instrument centre',
     :     STATUS)

*     Position of the telescope
      CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'LAT-OBS', LAT_OBS, 'Latitude of observatory (degrees)',
     :     STATUS)
      CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'LONG-OBS', LONG_OBS, 
     :     'East Longitude of observatory (degrees)',
     :     STATUS)

*     Need to supply some chop information (meaningless)
      CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CHOP_THR', 0.0D0, 'Chopper throw (arcsec)',
     :     STATUS)
      CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CHOP_PA', 0.0D0, 'Chopper P.A., 0 = in lat, 90 = in long',
     :     STATUS)
      CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CHOP_FRQ', SMU_F, 'Chopper frequency (Hz)',
     :     STATUS)
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CHOP_CRD', 'NA', 'Chopper coordinate system',
     :     STATUS)
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CHOP_FUN', 'DREAM', 'Chopper waveform',
     :     STATUS)



*     Sub instrument information
*     Assume that we have one sub-instrument and that it is LONG
      CALL SCULIB_PUT_FITS_I(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'N_SUBS', 1, 'Number of sub-instruments used',
     :     STATUS)

      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SUB_1','LONG', 'SCUBA sub-instrument being used', STATUS)
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SUB_2','not used', 'SCUBA sub-instrument being used',STATUS)
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SUB_3','not used', 'SCUBA sub-instrument being used',STATUS)
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SUB_4','not used', 'SCUBA sub-instrument being used',STATUS)
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SUB_5','not used', 'SCUBA sub-instrument being used',STATUS)

      CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'TAUZ_1',0.0D0, 'Zenith sky optical depth',STATUS)

*     FILTERS and wavelength

      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'FILT_1','850', 'Filter name', STATUS)
      CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'WAVE_1',862.0D0, 'Wavelength of map (microns)', STATUS)



*     Write FITS component
      CALL NDF_XNEW (OUT_NDF, 'FITS', '_CHAR*80', 1, N_FITS, 
     :     OUT_FITS_LOC, STATUS)
      CALL DAT_PUT1C (OUT_FITS_LOC, N_FITS, FITS, STATUS)

*     Annul extension locators
      CALL DAT_ANNUL(OUT_SCUCD_LOC, STATUS)
      CALL DAT_ANNUL(OUT_SCUBA_LOC, STATUS)
      CALL DAT_ANNUL(OUT_REDS_LOC, STATUS)
      CALL DAT_ANNUL(OUT_FITS_LOC, STATUS)

*     Write the HISTORY information. (DREAM info will be written
*     automatically when NDF is closed
      CALL NDF_HCRE(OUT_NDF, STATUS)

*     Close the NDF and shut down the NDF system (write DREAM history)
      CALL NDF_ANNUL(OUT_NDF, STATUS)
 
*     It seems that the only way to write multiple history
*     entries is to open and close the NDF multiple times!

*     REDUCE_SWITCH
*     Re-open the file
      CALL NDF_ASSOC('OUT', 'UPDATE', OUT_NDF, STATUS)

*     Need to write REDUCE_SWITCH and FLATFIELD tags to fool SURF
*     into thinking that the data have been processed by these tasks
      CALL NDF_HPUT(' ', 'REDUCE_SWITCH', .TRUE., 1, 
     :     'This is a dummy history component to fool SURF',
     :     .FALSE., .TRUE., .FALSE., OUT_NDF, STATUS)

*     Close the NDF
      CALL NDF_ANNUL(OUT_NDF, STATUS)

*     FLATFIELD
*     Re-open the file
      CALL NDF_ASSOC('OUT', 'UPDATE', OUT_NDF, STATUS)


      CALL NDF_HPUT(' ', 'FLATFIELD', .TRUE., 1, 
     :     'This is a dummy history component to fool SURF',
     :     .FALSE., .TRUE., .FALSE., OUT_NDF, STATUS)

*     Close the NDF
      CALL NDF_ANNUL(OUT_NDF, STATUS)


*     Shut down NDF system
      CALL NDF_END(STATUS)

      END


      SUBROUTINE DREAM_DATA_TO_SURF_DATA(N_BOL, N_JIG, MAX_PTS,
     :     START_POS, JIG_QUAL, DREAM, SURF,
     :     STATUS)
*+
*  Name:
*     DREAM_DATA_TO_SURF_DATA

*  Purpose:
*     Convert DREAM data record to SURF data array

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL DREAM_DATA_TO_SURF_DATA ( STATUS )

*  Arguments:
*     N_BOL  = INTEGER (Given)
*        Number of bolometers in input data
*     N_JIG  = INTEGER (Given)
*        Total number of jiggle positions in DREAM jiggle data
*     MAX_PTS = INTEGER (Given)
*        Max number of jiggle positions in SURF data
*     START_POS = INTEGER (Given)
*        Start position (Time axis) in SURF array 
*     JIG_QUAL = INTEGER ( N_JIG )
*        Array determining whether a jiggle position was used
*        or not. Not used if equal to -1.
*     DREAM = REAL ( N_JIG, N_BOL )
*        The DREAM data array
*     SURF = REAL ( N_BOL, MAX_PTS )
*        The SURF data array
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This routine converts the DREAM data (JIGGLE, BOL)
*     to SURF format (BOL, JIGGLE) whilst removing the jiggle
*     positions that were not observed.


*  Authors:
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)

*  History:
*     $Log$
*     Revision 1.4  1999/08/03 19:32:28  timj
*     Add copyright message to header.
*
*     Revision 1.3  1998/06/24 19:26:03  timj
*     Finally get jiggle pattern to work for new format sol files.
*
*     Revision 1.2  1998/06/19 19:28:26  timj
*     First released version
*
*     Revision 1.1  1998/05/14 20:41:03  timj
*     Initial revision
*

*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE
 
*  Global constants:
      INCLUDE 'SAE_PAR'       ! Starlink status
*      INCLUDE 'SCU_SOL'       ! Description of DREAM header file

*  COMMON data
*      COMMON SOLPA            ! DREAM common block

*  Arguments Given:
      INTEGER N_BOL
      INTEGER N_JIG
      INTEGER MAX_PTS
      REAL    DREAM( N_JIG, N_BOL )
      INTEGER JIG_QUAL( N_JIG )
      INTEGER START_POS

*  Arguments Returned:
      REAL    SURF ( N_BOL, MAX_PTS )

*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER BOL               ! Counter for bolometer
      INTEGER CURRENT           ! How many jiggles so far
      INTEGER JIG               ! Counter for loop over jiggles

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Loop over bolometers
      DO BOL = 1, N_BOL

*     Loop over jiggle positions. Note that DREAM has more jiggle 
*     positions defined than are actually stored so keep track
*     with a counter
         CURRENT = 0
         
         DO JIG = 1, N_JIG

            IF (JIG_QUAL(JIG) .NE. -1) THEN
               CURRENT = CURRENT + 1

               SURF(BOL, START_POS + CURRENT - 1) = DREAM(JIG, BOL)

            END IF

         END DO

      END DO


      END


      SUBROUTINE LST_FROM_UT(YY, MN, DD, HH, MM, SS,
     :     LONGITUDE, LST, MJD, STATUS)
*+
*  Name:
*     LST_FROM_UT

*  Purpose:
*     Calculate LST given UT

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL DREAM_DATA_TO_SURF_DATA ( YY, MN, DD, HH, MM, SS,
*    :    LST, LONGITUDE, MJD, STATUS)

*  Arguments:
*     YY = INTEGER (Given)
*        Gregorian year (eg 1998)
*     MN = INTEGER (Given)
*        Month number (starting at 1)
*     DD = INTEGER (Given)
*        Day of month
*     HH = INTEGER (Given)
*        Hour of day
*     MM = INTEGER (Given)
*        Minute of hour
*     SS = INTEGER (Given)
*        Second of minute
*     LONGITUDE = DOUBLE (Given)
*        Longitude of telescope in decimal degrees
*     LST = DOUBLE (Returned)
*        Local sidereal time in decimal degre
*     MJD = DOUBLE (Returned)
*        Modified julian date
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     Converts a UT to local sidereal time and Modified Julian date


*  Authors:
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)

*  History:
*     $Log$
*     Revision 1.4  1999/08/03 19:32:28  timj
*     Add copyright message to header.
*
*     Revision 1.3  1998/06/24 19:26:03  timj
*     Finally get jiggle pattern to work for new format sol files.
*
*     Revision 1.2  1998/06/19 19:28:26  timj
*     First released version
*
*     Revision 1.1  1998/05/14 20:41:03  timj
*     Initial revision
*

*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE
 
*  Global constants:
      INCLUDE 'SAE_PAR'       ! Starlink status

*  COMMON data
*      COMMON SOLPA            ! DREAM common block

*  Arguments Given:
      INTEGER YY
      INTEGER MN
      INTEGER DD
      INTEGER HH
      INTEGER MM
      INTEGER SS
      DOUBLE PRECISION LONGITUDE

*  Arguments Returned:
      DOUBLE PRECISION MJD
      DOUBLE PRECISION LST

*  Status:
      INTEGER STATUS

*  Constants:
      DOUBLE PRECISION DPI            ! PI
      PARAMETER (DPI = 3.141592653589793238462643383)
      DOUBLE PRECISION DD2R           ! Decimal degrees to radians
      PARAMETER (DD2R = 0.0174532925199432957692369076)

*  External references
      DOUBLE PRECISION SLA_GMSTA
      DOUBLE PRECISION SLA_EQEQX
      EXTERNAL SLA_GMSTA
      EXTERNAL SLA_EQEQX

*  Local Variables:
      DOUBLE PRECISION DAYFRAC        ! Fraction of day
      DOUBLE PRECISION DMJD           ! MJD of day
      DOUBLE PRECISION EQEQX          ! Equation of the equinox
      DOUBLE PRECISION GMST           ! Greenwich mean sidereal time
      INTEGER SLA_STATUS              ! Status from SLA routines

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Calculate fraction of day
*     Length of day is 86400 seconds
      DAYFRAC = ((DBLE(HH) * 3600.0D0) + (DBLE(MM) * 60.0D0) +
     :     DBLE(SS)) / 86400.0D0

*     Calculate the modified Julian date
      CALL SLA_CLDJ(YY, MN, DD, DMJD, SLA_STATUS)

      IF (SLA_STATUS .NE. 0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('C', SLA_STATUS)
         CALL ERR_REP(' ','LST_FROM_UT: Error calculating MJD:'//
     :        ' Code ^C from SLA_CLDJ', STATUS)

         CALL MSG_SETI('YY',YY)
         CALL MSG_SETI('MN',MN)
         CALL MSG_SETI('DD',DD)
         CALL ERR_REP(' ','LST_FROM_UT: UT input: ^YY ^MN ^DD',
     :        STATUS)
      END IF

*     Calculate Sidereal time of Greenwich

      GMST = SLA_GMSTA( DMJD, DAYFRAC)
      MJD  = DMJD + DAYFRAC

*     Equation of the equinoxes
      EQEQX = SLA_EQEQX( MJD )
      
*     Local sidereal time = Greenwich mean sidereal time +
*                           Equation of the equinox +
*                           Longitude in radians

      LST = GMST + EQEQX + (LONGITUDE * DD2R) 

*     Add 24 h if less than 0
      IF (LST .LT. 0.0D0) LST = LST + (2.0 * DPI)


      END
