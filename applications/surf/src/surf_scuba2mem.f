      SUBROUTINE SURF_SCUBA2MEM ( STATUS )
*+
*  Name:
*     SCUBA2MEM

*  Purpose:
*     Convert SCUBA data to a form usable by SCUBA DBMEM

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL SURF_SCUBA2MEM ( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This routine reads in SCUBA demodulated data and writes it
*     out with tangent plane offsets to an NDF in a form that is understandable
*     by the SCUBA implementation of DBMEM. All beam positions are calculated
*     and stored.

*  Usage:
*     scuba2mem in out

*  ADAM Parameters:
*     IN = CHAR (Read)
*        The name ofthe input file to be processed. This is a demodulated
*        data file. RESTORE should not have been run on it.
*     LAT_OUT = CHAR (Read)
*        The latitude of the output map centre. The supplied default value
*        is that of the map centre of the observation in the output
*        coordinates.
*     LONG_OUT = CHAR (Read)
*        The longitude of the output map centre. The supplied default value 
*        is that of the map centre of the observation in the output
*        coordinates.
*     MSG_FILTER = CHAR (Read)
*         Message filter level. Default is NORM.
*     OUT = NDF (Write)
*         This file contains the data in a format usable by SCUBA DBMEM.
*         The file format is described below.
*     OUT_COORDS = CHAR (Read)
*        The coordinate system of the output map. Available coordinate
*        systems are:
*        - AZ:  Azimuth/elevation offsets 
*        - NA:  Nasmyth offsets
*        - PL:  RA/Dec Offsets from moving centre (eg Planets)
*        - RB:  RA/Dec (B1950)
*        - RJ:  RA/Dec (J2000)
*        - RD:  RA/Dec (epoch of observation)
*        - GA:  Galactic coordinates (J2000)
*
*        For RD current epoch is taken from the first input file.
*     SHIFT= REAL(2) (Read)
*        The pointing shift [X,Y] to be applied that would bring the
*        map into the correct position. 
*        This is a shift in the output coordinate frame. CHANGE_POINTING
*        should be used to add Az/El pointing offsets.

*  Examples:
*     scuba2mem out_coords=RJ o34_lon_ext o34_mem

*  Notes:
*     Can be used on JIGGLE and SCAN data.

*  Format of output file:
*     SCUBA DBMEM requires the data, positions of every beam and the
*     LST for every point. This information (along with a standard FITS
*     header) is stored as a standard NDF. The data array is constructed
*     as follows:
*         3 dimensional: N_BOL * N_POS * ((N_BEAM * 2) + 1)
*         where N_BOL is the number of bolometers
*               N_POS is the number of samples for each bolometer (time axis)
*               N_BEAM is the number of beams
*
*         The 3rd dimension contains the actual data value plus positions
*         of every beam associated with the data point. Each beam has two
*         positions (X offset and Y offset)
*         Axis components store bolometer number, LST and beam weight.

*  Related Applications:
*     SURF: EXTRACT_DATA, REBIN
*     DBMEM

*  Authors:
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)
 
*  History:
*     $Log$
*     Revision 1.3  1998/04/28 20:50:20  timj
*     Support the return of FITS information from SURF_READ_REBIN_NDF
*
*     Revision 1.2  1998/03/18 23:32:54  timj
*     Add BOLWT
*
*     Revision 1.1  1998/01/27 02:01:33  timj
*     Initial revision
*
*     {note_history_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
* Type Definitions:
      IMPLICIT NONE

* Global constants:
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'                ! for VAL__ constants
      INCLUDE 'SAE_PAR'
      INCLUDE 'SURF_PAR'               ! SURF definitions

* Status:
      INTEGER STATUS

* External references:

* Local Constants:
      INTEGER     MAX_FILE      ! max number of input files
      PARAMETER (MAX_FILE = 1)
      INTEGER     NDIM          ! Number of dimensions in output file
      PARAMETER (NDIM = 3)

* Local variables:
      REAL    BOLWT (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! Bolometer weights
      INTEGER BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers measured in
                                        ! input file
      INTEGER BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! channel numbers of bolometers measured
                                        ! in input file
      INTEGER BOL_DEC_END               ! pointer to end of BOL_DEC_PTR scratch
                                        ! space
      INTEGER BOL_DEC_PTR               ! pointer to scratch space holding
                                        ! apparent Dec / y offset positions of
                                      ! measured points in input file (radians)
      INTEGER BOL_RA_END                ! pointer to end of BOL_RA_PTR scratch
                                        ! space
      INTEGER BOL_RA_PTR                ! pointer to scratch space holding
                                        ! apparent RA / x offset positions of
                                      ! measured points in input file (radians)
      INTEGER DATA_END                  ! Pointer to end of input data
      INTEGER DATA_PTR                  ! Pointer to input data
      CHARACTER * 40  DATA_SPEC(SCUBA__MAX_SECT) ! Array of section specs
      CHARACTER * 80  FITS ( SCUBA__MAX_FITS ) ! FITS information
      CHARACTER * 132 FNAME             ! Input filename
      INTEGER I                         ! Loop counter
      INTEGER IERR                      ! Position of error in VEC copy
      CHARACTER * 132 INSTRING          ! Input filename string + section
      INTEGER INT_LIST(MAX_FILE, SCUBA__MAX_INT + 1) ! Integration pointers
      DOUBLE PRECISION IN_DEC_CEN       ! Declination of map centre
      DOUBLE PRECISION IN_RA_CEN        ! RA of map centre
      INTEGER IN_NDF                    ! NDF identified of input file
      DOUBLE PRECISION IN_UT1           ! MJD of start of observation
      INTEGER ITEMP                     ! scratch integer
      INTEGER LBND(NDIM)                ! Lower bounds of output array
      INTEGER LST_PTR(2)                ! Array of pointers to LST (start/end)
      INTEGER NERR                      ! Number of errors in VEC_ copy
      INTEGER NSPEC                     ! Number of SCUBA sections
      INTEGER N_BEAM                    ! Number of beams requested
      INTEGER N_BOL                     ! Number of bolometers in input file
      INTEGER N_FILE                    ! File number (always 1)
      INTEGER N_FITS                    ! Number of fits entries in input
      INTEGER N_INTS                    ! Number of integrations in file
      INTEGER N_POS                     ! Number of samples (time axis)
      INTEGER N_PTS                     ! Number of points per beam
      CHARACTER * 40 OBJECT             ! name of object
      CHARACTER * 132 OUTFILE           ! Suggested output filename
      INTEGER OUTNDF                    ! Identifier to output NDF
      CHARACTER * 5  OUT_COORDS         ! Output coordinate system
      INTEGER OUT_A_PTR                 ! Pointer to axis
      INTEGER OUT_DATA_PTR              ! Pointer
      DOUBLE PRECISION OUT_DEC_CEN      ! Declination of output map centre
      DOUBLE PRECISION OUT_RA_CEN       ! RA of output map centre
      DOUBLE PRECISION OUT_ROTATION     ! angle between apparent N and N of
                                        ! output coord system (radians)
      CHARACTER * 15 SUB_INSTRUMENT     ! Sub instrument name
      CHARACTER * (10) SUFFIX_STRINGS(SCUBA__N_SUFFIX) ! Suffix for OUT
      INTEGER UBND(NDIM)                ! Upper bounds of output array
      LOGICAL USE_SECTION               ! Are we using the section or inverse
      CHARACTER * 15 UTDATE             ! date of observation
      CHARACTER * 15 UTSTART            ! UT of start of observation
      BYTE    QBITS                     ! BADBITS mask of input file
      LOGICAL QMF                       ! .false. = return quality array
                                        ! .true.= return masked data
                                        ! This is used in NDF_SQMF.
      INTEGER QUALITY_END               ! Pointer to end of input quality
      INTEGER QUALITY_PTR               ! Pointer to input quality
      INTEGER SECNDF                    ! Identifier to propogating section
      REAL    SHIFT(2)                  ! Tangent plane shift (X, Y)
      LOGICAL USELST                    ! TRUE if we want the LST information
      INTEGER VARIANCE_END              ! Pointer to end of input variance
      INTEGER VARIANCE_PTR              ! Pointer to input variance
      REAL    WAVELENGTH                ! Wavelength of data (microns)
      DOUBLE PRECISION OUT_LAT         ! longitude of output map centre 
                                       ! (radians)
      DOUBLE PRECISION OUT_LONG        ! longitude of output map centre
                                       ! (radians)

* Local data:
 
      DATA SUFFIX_STRINGS /'!_mem','m','_mem'/
 
 
*-
 
      IF (STATUS .NE. SAI__OK) RETURN
 
*     Set the MSG output level (for use with MSG_OUTIF)
 
      CALL MSG_IFGET('MSG_FILTER', STATUS)

*     start up the NDF system
 
      CALL NDF_BEGIN

*     Read the filename (and possibly a SCUBA section)
      CALL PAR_GET0C ('IN', INSTRING, STATUS)
         
*     Parse the filename string
      CALL SCULIB_SPLIT_FILE_SPEC(INSTRING, SCUBA__MAX_SECT, FNAME, 
     :     NSPEC, DATA_SPEC, USE_SECTION, STATUS)

*     Open the input file
      CALL NDF_FIND (DAT__ROOT, FNAME, IN_NDF, STATUS) 


*     Read in the input data
*     We will do automatic quality masking 
      QMF = .TRUE.
      N_FILE = 1    ! Tell the system this is the first file

      OUT_COORDS = 'RJ'  ! Output coordinates of map

*     Specify the required number of beams
*     This will be adjusted by the SURF_READ_REBIN_NDF subroutine
*     if we have selected an input data set that is incompatible
*     with the beam selection:
*        N_BEAM = 1  will return the middle-beam (standard position)
*        N_BEAM = 2 will return L and R beams for RASTER and JIGGLE
*        N_BEAM = 3 will return L, M and R beams (as M,L,R) for JIGGLE
*                   data that has been reduce_switched and for SCAN
*                   data. N_BEAM is set to 2 if a single switch of a
*                   JIGGLE map is requested.
*      Note that 3 position chopping with 2 switches is not supported.

*     Get the required number of beams
      CALL PAR_GET0I('NBEAMS', N_BEAM, STATUS)

*      N_BEAM = 3

*     We want the LST array
      USELST = .TRUE.

*     Calculate the beam positions and read in the data.

      CALL SURF_READ_REBIN_NDF( IN_NDF, MAX_FILE, 
     :     NSPEC, DATA_SPEC, OUT_COORDS, N_FILE, USE_SECTION,
     :     N_BOL, N_POS, N_INTS, N_BEAM,
     :     IN_UT1, IN_UT1, IN_RA_CEN, 
     :     IN_DEC_CEN, FITS, N_FITS, WAVELENGTH, SUB_INSTRUMENT, 
     :     OBJECT, UTDATE, UTSTART, 
     :     BOL_ADC, BOL_CHAN, BOL_RA_PTR,
     :     BOL_RA_END, BOL_DEC_PTR,
     :     BOL_DEC_END, DATA_PTR, 
     :     DATA_END, VARIANCE_PTR,
     :     VARIANCE_END, QMF, QUALITY_PTR,
     :     QUALITY_END, QBITS, USELST, LST_PTR,
     :     INT_LIST, BOLWT, STATUS)

*     Number of points per beam
      N_PTS = N_POS * N_BOL

*     JSR would rather have offsets from a map centre than apparent RA/Decs
*     so we now have to calculate the offsets before storing the positions.
*     Use the actual map centre for this since John will take care of any
*     differences when coadding files.
*     Weights will be dealt with in DBMEM

*     Get the tangent plane shifts (in an array)
      SHIFT(1) = 0.0
      SHIFT(2) = 0.0

      CALL PAR_DEF1R('SHIFT', 2, SHIFT, STATUS)
      CALL PAR_GET1R('SHIFT', 2, SHIFT, ITEMP, STATUS)

*     Convert to radians
      SHIFT(1) = SHIFT(1) / REAL(R2AS)
      SHIFT(2) = SHIFT(2) / REAL(R2AS)

*     The NA, AZ and PL frames already have offsets so we dont need
*     to do anything with them

      IF ((OUT_COORDS.NE.'NA'.AND.OUT_COORDS.NE.'AZ' 
     :     .AND. OUT_COORDS.NE.'PL')) THEN

*     The output centre is the same as the input centre

         OUT_RA_CEN = IN_RA_CEN
         OUT_DEC_CEN = IN_DEC_CEN


*     convert the RA,Decs of the observed points to tangent plane offsets
*     from the chosen output centre
*     Can process all the beams in one go

         OUT_ROTATION = 0.0D0   ! Since we are using Apparent RA/Dec

*  calculate the apparent RA,Dec of the selected output centre

         CALL SCULIB_CALC_OUTPUT_COORDS (OUT_RA_CEN, OUT_DEC_CEN, 
     :        IN_UT1, OUT_COORDS, OUT_LONG, OUT_LAT, STATUS)
 
         CALL SCULIB_CALC_APPARENT (OUT_LONG, OUT_LAT, 0.0D0, 0.0D0,
     :        0.0D0, 0.0D0, OUT_COORDS, 0.0, IN_UT1, 0.0D0, 0.0D0,
     :        OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION, STATUS)


         CALL SCULIB_APPARENT_2_TP (N_PTS * N_BEAM, 
     :        %val(BOL_RA_PTR), %val(BOL_DEC_PTR), 
     :        OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION,
     :        DBLE(SHIFT(1)), DBLE(SHIFT(2)), STATUS)
            
      ELSE

*     NA, AZ or PL

         OUT_RA_CEN = 0.0
         OUT_DEC_CEN = 0.0

*     Add on the tangent plane shift

         IF (STATUS .EQ. SAI__OK) THEN

            CALL SCULIB_ADDCAD(N_PTS * N_BEAM,
     :           %VAL(BOL_RA_PTR), DBLE(SHIFT(1)), 
     :           %VAL(BOL_RA_PTR))
            CALL SCULIB_ADDCAD(N_PTS * N_BEAM,
     :           %VAL(BOL_DEC_PTR), DBLE(SHIFT(2)), 
     :           %VAL(BOL_DEC_PTR))
            
         END IF

      END IF

*     Convert everything to arcsec

      IF (STATUS .EQ. SAI__OK) THEN

         CALL SCULIB_MULCAD(N_PTS * N_BEAM,
     :        %VAL(BOL_RA_PTR), R2AS,
     :        %VAL(BOL_RA_PTR), STATUS)
         CALL SCULIB_MULCAD(N_PTS * N_BEAM,
     :        %VAL(BOL_DEC_PTR), R2AS,
     :        %VAL(BOL_DEC_PTR), STATUS) 

      END IF

*     Construct the output name
      CALL SCULIB_CONSTRUCT_OUT(FNAME, SUFFIX_ENV, SCUBA__N_SUFFIX,
     :     SUFFIX_OPTIONS, SUFFIX_STRINGS, OUTFILE, STATUS)

*     Create the output file
*     Would like to propogate the FITS header to the output 
*     plus the other extensions for information.
*     Will update the FITS header later.
*     This means that I may as well get a section from the input
*     file and propogate it to the output (without the Variance and
*     Quality arrays).

*     Get a section of the input file but make it the correct size
*     for the output file (so that we do this instead of using NDF_SBND)

      LBND(1) = 1
      LBND(2) = 1
      LBND(3) = 1
      UBND(1) = N_BOL
      UBND(2) = N_POS
      UBND(3) = (2 * N_BEAM) + 1

      CALL NDF_SECT(IN_NDF, NDIM, LBND, UBND, SECNDF, STATUS)

*     Set the default output filename
      CALL PAR_DEF0C('OUT', OUTFILE, STATUS)

*     Propogate to the output NDF
      CALL NDF_PROP (SECNDF, ' ', 'OUT', OUTNDF, STATUS)

*     Close the input file
      CALL NDF_ANNUL(SECNDF, STATUS)
      CALL NDF_ANNUL(IN_NDF, STATUS)

*     Map the output array (DOUBLE for intial test)
      CALL NDF_MAP(OUTNDF, 'DATA', '_REAL', 'WRITE/BAD',
     :     OUT_DATA_PTR, ITEMP, STATUS)

*     Now copy some data in.
*     First the actual data values

      CALL VEC_RTOR(.FALSE., N_PTS, %VAL(DATA_PTR),
     :     %VAL(OUT_DATA_PTR), IERR, NERR, STATUS)


*     Go through a beam at a time

      DO I = 1, N_BEAM

*     Now the X positions
         CALL VEC_DTOR(.FALSE., N_PTS, 
     :        %VAL(BOL_RA_PTR + VAL__NBD * N_PTS * (I - 1)),
     :        %VAL(OUT_DATA_PTR + VAL__NBR * (2 * I - 1) * N_PTS ),
     :        IERR, NERR, STATUS)

*     Now Y
         CALL VEC_DTOR(.FALSE., N_PTS, 
     :        %VAL(BOL_DEC_PTR + VAL__NBD * N_PTS * (I - 1)),
     :        %VAL(OUT_DATA_PTR + VAL__NBR * (2 * I) * N_PTS ),
     :        IERR, NERR, STATUS)

      END DO

*     Set up the axes
*     Axis 1: Bolometer number
*     Axis 2: LST
*     Axis 3: Beam
      
      CALL NDF_ACRE(OUTNDF, STATUS)

*     Deal with BOLOMETER axis
 
      CALL NDF_ASTYP('_INTEGER', OUTNDF, 'CENTRE', 1, STATUS)

      CALL NDF_AMAP(OUTNDF, 'CENTRE', 1, '_INTEGER', 'WRITE',
     :     OUT_A_PTR, ITEMP, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_NFILLI (N_BOL, %val(OUT_A_PTR))
      END IF
      CALL NDF_ACPUT ('Bolometer', OUTNDF, 'LABEL', 1, STATUS)
      CALL NDF_AUNMP (OUTNDF, 'CENTRE', 1, STATUS)

*     Deal with LST axis

      CALL NDF_ASTYP('_DOUBLE', OUTNDF, 'CENTRE', 2, STATUS)

      CALL NDF_AMAP(OUTNDF, 'CENTRE', 2, '_DOUBLE', 'WRITE',
     :     OUT_A_PTR, ITEMP, STATUS)

      CALL VEC_DTOD(.FALSE., N_POS, 
     :     %VAL(LST_PTR(1)), %VAL(OUT_A_PTR),
     :     IERR, NERR, STATUS)

      CALL NDF_AUNMP (OUTNDF, 'CENTRE', 2, STATUS)

      CALL NDF_ACPUT('LST', OUTNDF, 'LABEL', 2, STATUS)
      CALL NDF_ACPUT('radians', OUTNDF, 'UNITS', 2, STATUS)



*     Unmap the data and close down
      CALL SCULIB_FREE ('IN_DATA', DATA_PTR,
     :     DATA_END, STATUS)
      CALL SCULIB_FREE ('IN_VARIANCE', VARIANCE_PTR,
     :     VARIANCE_END, STATUS)
      CALL SCULIB_FREE ('BOL_RA', BOL_RA_PTR,
     :     BOL_RA_END, STATUS)
      CALL SCULIB_FREE ('BOL_DEC', BOL_DEC_PTR,
     :     BOL_DEC_END, STATUS)

      CALL SCULIB_FREE('LST', LST_PTR(1), LST_PTR(2), STATUS)

*     Close the output file
      CALL NDF_ANNUL(OUTNDF, STATUS)

      CALL NDF_END(STATUS)


      END
