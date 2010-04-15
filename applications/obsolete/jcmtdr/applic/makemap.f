      SUBROUTINE makemap
*+
*  Name:
*     MAKEMAP

*  Purpose:
*     To convert GSDD files into HDS files for FIGARO

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAKEMAP

*  Description:
*     Imports data from GSD files produced at the JCMT in Hawaii, and
*     converts it into a form suitable for procdessing by standard
*     FIGARO commands i.e. produces HDS files with a structure
*     understood by FIGARO. As well as simply importing the data makemap
*     sorts the data correctly so that the main data array contains a
*     representation of the map as seen on the sky.

*     Questions.
*     What's in LST array for `on-the-fly' version 3.

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     REVAD::JFL: John Lightfoot (RoE)
*     REVAD::HME: Horst Myerdierks (RoE)
*     JACH::TIMJ: Tim Jenness (JAC)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1989 (JBVAD::PAH):
*        Original version.
*      9-MAY-1991 (REVAD::JFL): GSD atmospheric pressure already in mbar
*                               so took out conversion from mmHg. Also
*                               GSD longitude is +ve towards west, changed
*                               output to be +ve towards east.
*     10-MAY-1991 (REVAD::JFL): Modified so that only unmaps workspace arrays
*                               that it has mapped.
*     13-MAY-1991 (REVAD::JFL): Unmodified again, DSA_CLOSE should get rid of
*                               them anyway.
*     15-21-MAY-1991 (REVAD::JFL): Re-organise it all, reduced number of
*                               redundant items read in, concentrated on
*                               using scan tables to sort map.
*     26-JUL-1991 (REVAD::JFL)  Modified to produce NDF structures rather
*                               than old Figaro
*     26-FEB-1991 (REVAD::JFL)  Length of structure names cut down, modified
*                               to read in centre, local coord systems and
*                               cell axes correctly.
*      3-JUN-1992 (REVAD::JFL)  Modified to store chop coords, position angle
*                               telescope beam and +ve beam.
*     10-FEB-1993 (REVAD::JFL)  Modified to handle non-integral cell offsets,
*                               use JCMT_CALCULATE_GRID to calculate sample
*                               grid required.
*      4-JUN-1993 (REVAD::JFL)  Warning if centre-moving flag set removed at
*                               version 1.2
*      5-JUL-1993 (REVAD::JFL)  Now reads chopper function, still version 1.2
*     21-OCT-1993 (REVAD::JFL)  DSA_WRUSER replaced by PAR_WRUSER
*     22-MAR-1994 (REVAD::HME)  Change include statements.
*     28-NOV-1994 (REVAD::HME)  Change calls to GSD_INQUIRE_ARRAY into
*                               calls to GSD_INQ_SIZE. Although the
*                               latter is classified as obsolete, it is
*                               the one that Specx uses and that Rachael
*                               has ported to Unix.
*     29-NOV-1994 (REVAD::HME)  Change beam throw to be written with
*                               DTA_WVARF. Change centre observing
*                               frequency to be got and written in
*                               double precision.
*     13-DEC-1994 (hme@roe)     Get day of month via NINT rather than
*                               INT. The conversion of the floating
*                               point number that was the date from VAX
*                               to a different machine may make the
*                               number just that little bit smaller than
*                               the original.
*                               C3FLY must be LOGICAL*1. To the Figaro
*                               file we must writen an integer. We write
*                               1 for true and 0 for false. The VAX used
*                               to write 255 for true, but the Sun would
*                               write 16777216, which is an even number
*                               and might not be true on a VAX.
*     12-JAN-1995 (hme@roe)     On failure to open GSD file, abort
*                               _with_ an error message.
*     27-MAR-2003 (timj@jach)   Must initialize FAULT variable on Linux
*     10-SEP-2006 (timj@jach)   Fixes for g95
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

*  Dynamic memory include file - defines DYNAMIC_MEM

      INCLUDE 'DYNAMIC_MEMORY'

*  SAE constants

      INCLUDE 'SAE_PAR'

*  GSD constants

      INCLUDE 'PRM_PAR'
      INCLUDE 'GSD_PAR'

*  general astronomical parameters

      INCLUDE 'ASTRO_PAR'

*  Local Constants:

*  Functions
      LOGICAL PAR_ABORT                ! check PAR abort flag
      LOGICAL PAR_GIVEN                !
      INTEGER DYN_ELEMENT              !
      INTEGER ICH_LEN                  !
      INTEGER ICH_FOLD                 !

*  Local variables

      LOGICAL FAULT                    ! T if want to signal Figaro fault

*  Arguments for GSD_ routines

      INTEGER FILELEN
      INTEGER FD
      REAL VERSION
      CHARACTER*30 LABEL
      INTEGER NITEM
      INTEGER NUMBER
      CHARACTER*(GSD__SZNAME) NAME
      CHARACTER*(GSD__SZUNIT) UNIT
      CHARACTER TYPE
      LOGICAL TABLE
      LOGICAL ARRAY
      INTEGER GSDINDEX(GSD__SZINDEX) ! 5
      INTEGER SIZE
      INTEGER MAXDIMS
      CHARACTER*(GSD__SZNAME) DIMNAMES(GSD__MXDIM)
      CHARACTER*(GSD__SZUNIT) DIMUNITS(GSD__MXDIM)
      INTEGER DIMVALS(GSD__MXDIM),DIMNUMBERS(GSD__MXDIM),
     :   DIMINDEX(GSD__SZINDEX,GSD__MXDIM)
      INTEGER ACTDIMS
      INTEGER ACTVALS

*  -GSD file items

      LOGICAL*1 C3FLY                  ! T if data taken `on the fly'
      INTEGER C3SRT                    ! time per scan (= time per map point
                                       ! for discrete pixel mode, = time per
                                       ! ROW for `on the fly')
      INTEGER C3NMAP                   ! no of points in pointing history table
                                       ! (i.e. number of points in map)
      INTEGER C3NPP                    ! other dimension of pointing history
                                       ! table (usually 2 for x,y)
      INTEGER C3NO_SCAN_VARS1          ! number of variables per entry in
                                       ! scan table 1
      INTEGER C3NCH                    ! number of backend data channels stored
      INTEGER C3NIS                    ! planned no. of scans measured in
                                       !   observation, = no of entries in
                                       !   scan tables
      INTEGER C3NSAMPLE                ! actual number of scans measured, = no
                                       !   of non-zero entries in scan tables
      INTEGER C3MXP                    ! no of map points measured per phase
      REAL C6DX, C6DY                  ! cell size in X and Y
      CHARACTER*(GSD__SZCHAR) C6SD     ! scan direction; HORIZONTAL
                                       ! or VERTICAL
      CHARACTER*(GSD__SZCHAR) C4FUN    ! chopper function

*  -DSA stuff

      INTEGER SLOT                     ! slot number
      INTEGER ADDRESS                  ! address
      INTEGER ST1PTR                   ! pointer to scan table 1
      INTEGER LSTPTR                   ! pointer to LSA temporary array
      INTEGER XPTR                     ! pointer to pixel x co-ord array
      INTEGER YPTR                     ! pointer to pixel y co-ord array
      INTEGER PHTPTR                   ! pointer to pointing history table
      INTEGER DATPTR                   ! pointer to input data
      INTEGER ERRPTR                   !    "       "     errors
      INTEGER XAXPTR                   ! pointer to output x-axis
      INTEGER YAXPTR                   !    "       "      y   "
      INTEGER ODPTR                    ! pointer to output data array
      INTEGER OEPTR                    !    "       "      error  "
      INTEGER OLSTPTR                  !    "       "      LST     "
      INTEGER IFLAG                    ! magic value for integers
      REAL FFLAG                       ! magic value for real numbers
      DOUBLE PRECISION DFLAG           ! magic value for double precision
      CHARACTER*20 CHAR_ITEMS(2)       ! axis units and label

*  -general

      LOGICAL LTEMP                    !
      INTEGER STATUS                   !
      INTEGER NCH                      ! backend data channel to be read
      INTEGER IY, IM, ID               ! year month day of observation
      INTEGER NX, NY                   ! number of map pixels in x and y
      INTEGER NLST                     ! number of LST values in LST array
      INTEGER NELM                     !
      INTEGER NPOINTS                  ! number of valid map points
      INTEGER DSTAT                    ! DTA status
      INTEGER SLASTAT                  ! SLA status
      INTEGER IDX                      !
      INTEGER ITEMP                    !
      INTEGER IGNORE                   ! unimportant status
      REAL XMIN, XMAX                  ! extrema of map in x offset
      REAL YMIN, YMAX                  ! extrema of map in y offset
      REAL XSPACE, YSPACE              ! pixel spacing of output map grid
      REAL RVAL                        !
      REAL RTEMP                       !
      REAL INT_TIME                    ! integration time per pixel (s)
      DOUBLE PRECISION DATE, HOUR      ! date and UT of observation
      DOUBLE PRECISION DJM             ! modified Julian day
      DOUBLE PRECISION MJDSTART        !         "           + UT/24
      DOUBLE PRECISION DFACTOR         ! conversion factor from ST to
                                       ! radians
      DOUBLE PRECISION DTEMP           !
      CHARACTER*80 FILENAME            ! name of input GSD file
      CHARACTER*80 OUTFNAM             ! name of output Figaro file
      CHARACTER*15 ELEMENT             ! DTA item name
      CHARACTER*(GSD__SZUNIT) CELLX_UNIT, CELLY_UNIT
                                       ! units of cell sizes in X and Y
      CHARACTER*80 MESSAGE             ! output message
      CHARACTER*80 ERROR               ! DTA error translation
      CHARACTER*128 JCMT_DTA_NAME      ! DTA name of JCMT section
      CHARACTER*128 SEC_NAME           ! "      "    current section
      CHARACTER*128 DTA_NAME           ! "      "    general item
      CHARACTER*(2*GSD__SZCHAR+1) STRING !

*  Initialisations
      DATA NUMBER / 0 /
      DATA MAXDIMS/GSD__MXDIM/

*.

*     Initialise variables
      STATUS = SAI__OK

      FAULT = .FALSE.

*  Start DSA, find bad values for various type of data

      CALL DSA_OPEN (STATUS)
      CALL DSA_GET_FLAG_VALUE ('INT', IFLAG, STATUS)
      CALL DSA_GET_FLAG_VALUE ('FLOAT', FFLAG, STATUS)
      CALL DSA_GET_FLAG_VALUE ('DOUBLE', DFLAG, STATUS)
      IF (STATUS .NE. 0) THEN
         CALL PAR_WRUSER('MAKEMAP - failed to open DSA system, '//
     :      'fatal error', IGNORE)
         GOTO 500
      END IF

*  Get name of input file, and open using the GSD open routine

      CALL PAR_RDCHAR ('GSDFILE', ' ', FILENAME)
      IF (PAR_ABORT()) THEN
         FAULT = .TRUE.
         GOTO 500
      END IF
      CALL GSD_OPEN_READ(FILENAME, FD, VERSION, LABEL, NITEM, STATUS)

*  say what version the GSDFILE is

      IF (STATUS .EQ. SAI__OK) THEN
         WRITE (MESSAGE,'(''GSD Version'', X, F3.1)') VERSION
         CALL PAR_WRUSER (MESSAGE, STATUS)
      ELSE
         CALL PAR_WRUSER('MAKEMAP - failed to open GSD file, '//
     :      'fatal error', IGNORE)
         GOTO 500
      END IF

** This first section aims to read in the data array and items essential
*  to describe the observation. An error reading from the GSD file will
*  be fatal

*  get the scan time in seconds (in `on-the-fly' mode this is the
*  total time taken for each raster leg, in discrete pixel mode it
*  is the time per map point).

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C3SRT'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0I (GSDINDEX, C3SRT, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - failed to read GSD item '//
     :        'C3SRT, fatal error', IGNORE)
            GOTO 500
         END IF
      END IF

*  Was the data taken in chop-scan mode (on-the-fly) (T), or in
*  discrete pixel mode (F).

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C3FLY'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0L (GSDINDEX, C3FLY, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - failed to read GSD item '//
     :        'C3FLY, fatal error', IGNORE)
            GOTO 500
         END IF
      END IF

*  if `on the fly' read the direction of the component scans;
*  VERTICAL (in altitude) or HORIZONTAL (in azimuth)

      IF (C3FLY) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            NAME = 'C6SD'
            CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY,
     :         GSDINDEX, STATUS)
            CALL GSD_GET0C (GSDINDEX, C6SD, STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL PAR_WRUSER ('MAKEMAP - failed to read GSD item '//
     :           'C6SD, fatal error', IGNORE)
               GOTO 500
            END IF
         END IF
      END IF

*  get the number of backend data channels. If this parameter is missing
*  then assume that it is 1.

      NAME = 'C3NCH'
      IF (STATUS .EQ. SAI__OK) THEN
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY,
     :      GSDINDEX, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            CALL GSD_GET0I (GSDINDEX, C3NCH, STATUS)
         ELSE
            STATUS = SAI__OK
            C3NCH = 1
         END IF
      END IF

*  read the channel to be read into the output map

      IF (STATUS .EQ. SAI__OK) THEN
         CALL PAR_RDVAL ('CHANNEL', 1.0, REAL(C3NCH), 1.0, ' ', RVAL)
         IF (PAR_ABORT()) THEN
            FAULT = .TRUE.
            GOTO 500
         END IF
      END IF
      NCH = NINT(RVAL)

*  get the planned number of scans in the observation

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C3NIS'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0I (GSDINDEX, C3NIS, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - failed to read GSD item '//
     :        'C3NIS, fatal error', IGNORE)
            GOTO 500
         END IF
      END IF

*  ..and the actual number of scans measured

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C3NSAMPLE'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0I (GSDINDEX, C3NSAMPLE, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - failed to read GSD item '//
     :        'C3NSAMPLE, fatal error', IGNORE)
            GOTO 500
         ELSE
            IF (C3NSAMPLE .EQ. 0) THEN
               CALL PAR_WRUSER ('MAKEMAP - No scans were taken '//
     :            'during the observation', IGNORE)
               GOTO 500
            ELSE IF (C3NSAMPLE .LT. C3NIS) THEN
               CALL PAR_WRUSER ('MAKEMAP - the observation was '//
     :            'aborted, will only read in scans containing data',
     :            IGNORE)
            END IF
         END IF
      END IF

*  get the number of map points per phase (phase is the same as scan
*  in this context)

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C3MXP'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0I (GSDINDEX, C3MXP, STATUS)
         IF (C3MXP .LT. 1) C3MXP = 1
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - failed to read GSD item '//
     :        'C3MXP, fatal error', IGNORE)
            GOTO 500
         END IF
      END IF

*  the x cell size, and the units in which measured

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C6DX'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0R (GSDINDEX, C6DX, STATUS)
         CELLX_UNIT = UNIT
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - Error reading x cell size '//
     :        'and units from GSD item C6DX, fatal error', IGNORE)
            GOTO 500
         END IF
      END IF

*  and the y cell size

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C6DY'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0R (GSDINDEX, C6DY, STATUS)
         CELLY_UNIT = UNIT
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - Error reading y cell size '//
     :        'and units from GSD item C6DY, fatal error', IGNORE)
            GOTO 500
         END IF
      END IF


** The object of this section is to obtain the measured data value
** and the LST of its observation for each map pixel. This is complicated
** because of changes in the way things were stored between versions
** 3 and 4 of GSD, and by the fact that the storage method depends
** on whether the data was taken as discrete pixels or `on the fly'
** in a series of ROW actions.

      IF (VERSION .GE. 3.999) THEN

*  For GSD versions greater than 4 the scan tables were stored.
*  Entries are made in the scan tables at the beginning of each scan
*  which means that there will be a different number of entries
*  depending whether the map was obtained `on-the-fly' or as discrete
*  pixels. In an on-the-fly observation the `scan' is a ROW of the
*  raster which may cover many map points. For discrete pixel
*  observations a `scan' refers to a single map point.

*  C12SCAN_TABLE_1 is an array dimensioned (C3NO_SCAN_VARS1, C3NIS)
*  where C3NO_SCAN_VARS1 is assumed to be 6, giving for each scan
*  the LST, airmass, x offset, y offset, direction, scan_length.
*  C3NIS is the number of scans made in the observation.

*  get C3NO_SCAN_VARS1

         IF (STATUS .EQ. SAI__OK) THEN
            NAME = 'C3NO_SCAN_VARS1'
            CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY,
     :         GSDINDEX, STATUS)
            CALL GSD_GET0I (GSDINDEX, C3NO_SCAN_VARS1, STATUS)

*  find the scan table

            NAME = 'C12SCAN_TABLE_1'
            CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY,
     :         GSDINDEX, STATUS)
            CALL GSD_INQ_SIZE(FD, NUMBER, MAXDIMS, DIMNAMES, DIMUNITS,
     :         DIMVALS, ACTDIMS, SIZE, STATUS)
*           CALL GSD_INQUIRE_ARRAY (GSDINDEX, MAXDIMS, DIMNUMBERS,
*    :         DIMNAMES, DIMUNITS, DIMVALS, DIMINDEX, ACTDIMS, SIZE,
*    :         STATUS)

*  get workspace for scan table, and copy table into it (map
*  in as double precision to give accuracy in LST calculations)

            CALL DSA_GET_WORK_ARRAY (SIZE, 'DOUBLE', ADDRESS, SLOT,
     :         STATUS)
            ST1PTR = DYN_ELEMENT(ADDRESS)

            IF (STATUS .NE. SAI__OK) THEN
               CALL PAR_WRUSER ('MAKEMAP - failed to read scan '//
     :           'table, fatal error', IGNORE)
               GOTO 500
            ELSE

               CALL GSD_GET1D (GSDINDEX, 1, SIZE, 1, SIZE,
     :            DYNAMIC_MEM(ST1PTR), ACTVALS, STATUS)

               IF (.NOT. C3FLY) THEN

*  for a map made of integrations on discrete points the contents of
*  the scan table are, for each observed point; lst,airmass,x,y.
*  We only need the LST and the x, y. The airmass will be recalculated
*  in JCMTEXTC. The dimensions of LST, x, y are C3NIS. The corresponding
*  data array which is dealt with later will be dimensioned
*  (C3NCH, C3MXP, C3NIS) and C3MXP should always be 1 in this case.

*  get workspace for LST array

                  CALL DSA_GET_WORK_ARRAY (C3NIS, 'DOUBLE', ADDRESS,
     :               SLOT, STATUS)
                  LSTPTR = DYN_ELEMENT(ADDRESS)

*  get workspace for x and y arrays

                  CALL DSA_GET_WORK_ARRAY (C3NIS, 'FLOAT', ADDRESS,
     :               SLOT, STATUS)
                  XPTR = DYN_ELEMENT(ADDRESS)
                  CALL DSA_GET_WORK_ARRAY (C3NIS, 'FLOAT', ADDRESS,
     :               SLOT, STATUS)
                  YPTR = DYN_ELEMENT(ADDRESS)

*  call routine to copy what's required into the temporary arrays

                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL JCMT_GETLSTXY (DYNAMIC_MEM(ST1PTR),
     :                  C3NO_SCAN_VARS1, C3NIS, C3NSAMPLE,
     :                  DYNAMIC_MEM(LSTPTR), DYNAMIC_MEM(XPTR),
     :                  DYNAMIC_MEM(YPTR), STATUS)
                  END IF
                  IF (STATUS .NE. SAI__OK) THEN
                     CALL PAR_WRUSER ('MAKEMAP - Failed to '//
     :                 'decode scan table, fatal error', IGNORE)
                     GOTO 500
                  END IF

*  set number of valid points to C3NSAMPLE

                  NPOINTS = C3NSAMPLE


               ELSE

*  for 'on the fly' observations the scan table items are stored
*  only at the start of each scan in the raster. The LST and x,y must be
*  recalculated for each of the map points using C6SD to specify
*  the scan direction and C3SRT to indicate the length of time
*  spent on each scan. In this case the dimensions of LST, x, y are
*  (C3MXP,C3NIS). The corresponding data array to be dealt with later
*  is dimensioned (C3NCH, C3MXP, C3NIS).

*  get workspace for the LST array

                  NELM = C3MXP * C3NIS
                  CALL DSA_GET_WORK_ARRAY (NELM, 'DOUBLE', ADDRESS,
     :               SLOT, STATUS)
                  LSTPTR = DYN_ELEMENT(ADDRESS)

*  get workspace for x and y arrays

                  CALL DSA_GET_WORK_ARRAY (NELM, 'FLOAT', ADDRESS,
     :               SLOT, STATUS)
                  XPTR = DYN_ELEMENT(ADDRESS)
                  CALL DSA_GET_WORK_ARRAY (NELM, 'FLOAT', ADDRESS,
     :               SLOT, STATUS)
                  YPTR = DYN_ELEMENT(ADDRESS)

*  call routine to get LST, x, y from scan table into temporary arrays

                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL JCMT_MKLSTXY (DYNAMIC_MEM(ST1PTR),
     :                  C3NO_SCAN_VARS1, C3NIS, C3NSAMPLE, C6SD,
     :                  C3SRT, DYNAMIC_MEM(LSTPTR),
     :                  DYNAMIC_MEM(XPTR), DYNAMIC_MEM(YPTR), C3MXP,
     :                  STATUS)
                  END IF
                  IF (STATUS .NE. SAI__OK) THEN
                     CALL PAR_WRUSER ('MAKEMAP - Failed to decode '//
     :                 'scan table, fatal error', IGNORE)
                     GOTO 500
                  END IF

*  number of valid points is C3NSAMPLE * C3MXP

                  NPOINTS = C3NSAMPLE * C3MXP

               END IF
            END IF
         END IF

      ELSE

*  Before version 4 no scan tables were stored, so use the pointing history
*  table to specify x, y and the LST item C14LST to specify the LST for each
*  pixel. The pointing history table holds results for all points measured,
*  and I assume LST array does as well. This probably implies that
*  we are dealing with data taken in discrete pixels, so generate an
*  error if C3FLY is set.

         IF (STATUS .EQ. SAI__OK) THEN

            IF (C3FLY) THEN

               IF (STATUS .EQ. SAI__OK) THEN
                  CALL PAR_WRUSER ('MAKEMAP - Can''t yet handle '//
     :            'on-the-fly data from GSD 3.0 files', IGNORE)
                  FAULT = .TRUE.
                  GOTO 500
               END IF

            ELSE

*  The pointing history table is an array dimensioned (C3NPP, C3NMAP),
*  find these values

               IF (STATUS .EQ. SAI__OK) THEN
                  NAME = 'C3NMAP'
                  CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY,
     :               GSDINDEX, STATUS)
                  CALL GSD_GET0I (GSDINDEX, C3NMAP, STATUS)
                  IF (STATUS .NE. SAI__OK) THEN
                     CALL PAR_WRUSER ('MAKEMAP - failed to read '//
     :                  'GSD item C3NMAP, fatal error', IGNORE)
                     GOTO 500
                  END IF
               END IF

               IF (STATUS .EQ. SAI__OK) THEN
                  NAME = 'C3NPP'
                  CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY,
     :               GSDINDEX, STATUS)
                  CALL GSD_GET0I (GSDINDEX, C3NPP, STATUS)
                  IF (STATUS .NE. SAI__OK) THEN
                     CALL PAR_WRUSER ('MAKEMAP - failed to read GSD '//
     :                  'item C3NPP, fatal error', IGNORE)
                     GOTO 500
                  END IF
               END IF

*  Generate an error if C3NIS does not equal C3NMAP, because that means
*  that our assumptions about how version 3 data was stored are not correct.

               IF (C3NIS .NE. C3NMAP) THEN
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL PAR_WRUSER ('MAKEMAP - GSD version 3 C3NIS '//
     :                  'does not equal C3NMAP', IGNORE)
                     FAULT = .TRUE.
                     GOTO 500
                  END IF
               END IF

*  Find pointing history table. This is supposed to contain the real
*  offsets (in cell units) of the map points observed. However, at
*  present the positions saved are just the ideal positions.
*  The positions are stored for each pixel whether observed in
*  'on the fly' or discrete modes

               IF (STATUS .EQ. SAI__OK) THEN
                  NAME = 'C14PHIST'
                  CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY,
     :               GSDINDEX, STATUS)
                  CALL GSD_INQ_SIZE(FD, NUMBER, MAXDIMS, DIMNAMES,
     :               DIMUNITS, DIMVALS, ACTDIMS, SIZE, STATUS)
*                 CALL GSD_INQUIRE_ARRAY (GSDINDEX, MAXDIMS,
*    :               DIMNUMBERS, DIMNAMES, DIMUNITS, DIMVALS, DIMINDEX,
*    :               ACTDIMS, SIZE, STATUS)

*  get workspace for it and copy it in

                  CALL DSA_GET_WORK_ARRAY (SIZE, 'FLOAT', ADDRESS,
     :               SLOT, STATUS)
                  PHTPTR = DYN_ELEMENT(ADDRESS)

                  IF (STATUS .NE. SAI__OK) THEN
                     CALL PAR_WRUSER ('MAKEMAP - failed to read '//
     :                  'pointing history table, fatal error', IGNORE)
                     GOTO 500
                  ELSE

                     CALL GSD_GET1R (GSDINDEX, 1, SIZE, 1, SIZE,
     :                  DYNAMIC_MEM(PHTPTR), ACTVALS, STATUS)

*  get workspace for x and y arrays

                     CALL DSA_GET_WORK_ARRAY (C3NMAP, 'FLOAT', ADDRESS,
     :                  SLOT, STATUS)
                     XPTR = DYN_ELEMENT(ADDRESS)
                     CALL DSA_GET_WORK_ARRAY (C3NMAP, 'FLOAT', ADDRESS,
     :                  SLOT, STATUS)
                     YPTR = DYN_ELEMENT(ADDRESS)

*  call routine to transfer PHT values to temporary arrays

                     IF (STATUS .EQ. SAI__OK) THEN
                        CALL JCMT_GETPHT (DYNAMIC_MEM(PHTPTR),
     :                     C3NPP, C3NMAP, C3NSAMPLE,
     :                     DYNAMIC_MEM(XPTR), DYNAMIC_MEM(YPTR),
     :                     STATUS)
                     END IF

*  set number of valid points

                     NPOINTS = C3NSAMPLE

                     IF (STATUS .NE. SAI__OK) THEN
                        CALL PAR_WRUSER ('MAKEMAP - Failed to '//
     :                     'decode pointing history table, fatal '//
     :                     'error', IGNORE)
                        GOTO 500
                     ELSE

*  find the LST array

                        NAME = 'C14LST'
                        CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE,
     :                     TABLE, GSDINDEX, STATUS)
                        CALL GSD_INQ_SIZE(FD, NUMBER, MAXDIMS, DIMNAMES,
     :                     DIMUNITS, DIMVALS, ACTDIMS, SIZE, STATUS)
*                       CALL GSD_INQUIRE_ARRAY (GSDINDEX, MAXDIMS,
*    :                     DIMNUMBERS, DIMNAMES, DIMUNITS, DIMVALS,
*    :                     DIMINDEX, ACTDIMS, SIZE, STATUS)

*  generate an error if LST does not have C3NIS values

                        IF (C3NIS .NE. SIZE) THEN
                           IF (STATUS .EQ. SAI__OK) THEN
                              CALL PAR_WRUSER ('MAKEMAP - GSD '//
     :                           'version 3 LST array does not '//
     :                           'have C3NIS values', IGNORE)
                           END IF
                           FAULT = .TRUE.
                           GOTO 500
                        END IF

*  get workspace for it and copy it in (double precision for accuracy
*  in later LST calculations)

                        CALL DSA_GET_WORK_ARRAY (C3NIS, 'DOUBLE',
     :                     ADDRESS, SLOT, STATUS)
                        LSTPTR = DYN_ELEMENT(ADDRESS)
                        IF (STATUS .EQ. SAI__OK) THEN
                           CALL GSD_GET1D (GSDINDEX, 1, SIZE, 1,
     :                        SIZE, DYNAMIC_MEM(LSTPTR), ACTVALS,
     :                        STATUS)
                        END IF
                        IF (STATUS .NE. SAI__OK) THEN
                           CALL PAR_WRUSER ('MAKEMAP - Error '//
     :                        'reading LST array, fatal error', IGNORE)
                           FAULT = .TRUE.
                           GOTO 500
                        END IF

                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF

**  calculate the size and shape of the rectangular array needed to
**  contain the measured points

      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_CALCULATE_GRID (NPOINTS,
     :     DYNAMIC_MEM(XPTR), DYNAMIC_MEM(YPTR),
     :     XMIN, XMAX, XSPACE, NX,
     :     YMIN, YMAX, YSPACE, NY, STATUS)
      END IF


**  Now find the data array

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C13DAT'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - Failed to find data array '//
     :         'C13DAT, fatal error', IGNORE)
            GOTO 500
         ELSE

*  The data array should be dimensioned (C3NCH, C3MXP, C3NIS),
*  enquire size and if different bail out because it means our
*  assumptions about how the data are stored are incorrect.

            CALL GSD_INQ_SIZE(FD, NUMBER, MAXDIMS, DIMNAMES,
     :         DIMUNITS, DIMVALS, ACTDIMS, SIZE, STATUS)
*           CALL GSD_INQUIRE_ARRAY (GSDINDEX, MAXDIMS, DIMNUMBERS,
*    :         DIMNAMES, DIMUNITS, DIMVALS, DIMINDEX, ACTDIMS, SIZE,
*    :         STATUS)
            IF (SIZE .NE. C3NCH*C3MXP*C3NIS) THEN
               CALL PAR_WRUSER ('MAKEMAP - Data array is not '//
     :            'C3NCH*C3MXP*C3NIS in size, fatal error', IGNORE)
               FAULT = .TRUE.
               GOTO 500
            END IF

*  get temporary array and read in the data

            CALL DSA_GET_WORK_ARRAY (SIZE, 'FLOAT', ADDRESS, SLOT,
     :         STATUS)
            DATPTR = DYN_ELEMENT(ADDRESS)
            IF (STATUS .EQ. SAI__OK) THEN
            CALL GSD_GET1R (GSDINDEX, 1, SIZE, 1, SIZE,
     :         DYNAMIC_MEM(DATPTR), ACTVALS, STATUS)
            END IF
         END IF
      END IF

*  catch-all abort if have been any problems reading in the ESSENTIAL GSD info

      IF (STATUS .NE. SAI__OK) GOTO 500


** OK, ready to create output file

*  Set the default filename of the output to be the same as the input
*  (minus .GSD suffix)

      IDX = INDEX(FILENAME,']') + 1
      IDX = INDEX(FILENAME(IDX:),'.')
      IF (IDX .NE. 0) THEN
        OUTFNAM = FILENAME(:IDX-1)
      ELSE
        OUTFNAM = FILENAME
      ENDIF
      CALL PAR_SDCHAR ('OUTPUT', OUTFNAM, STATUS)

*  create a new empty structure

      CALL DSA_OUTPUT ('OUTPUT', 'OUTPUT', ' ', 1, 1, STATUS)

*  create data array in the output file

      DIMVALS(1) = NX
      DIMVALS(2) = NY
      CALL DSA_SIMPLE_OUTPUT ('OUTPUT', 'D', 'FLOAT', 2,
     :   DIMVALS, STATUS)

*  tell DSA we want to use `magic' values

      CALL DSA_USE_FLAGGED_VALUES ('OUTPUT', STATUS)
      CALL DSA_SET_FLAGGED_VALUES ('OUTPUT', .TRUE., STATUS)

*  map the output data array and call a routine to sort
*  the input data into it according to x,y

      CALL DSA_MAP_DATA ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      ODPTR = DYN_ELEMENT(ADDRESS)

      IF (STATUS .EQ. SAI__OK) THEN
         CALL JCMT_SORT_RDATA (C3NCH, C3MXP, C3NIS, C3NSAMPLE,
     :      DYNAMIC_MEM(DATPTR), NCH, DYNAMIC_MEM(XPTR),
     :      DYNAMIC_MEM(YPTR), FFLAG, XMIN, XSPACE, NX, YMIN, YSPACE,
     :      NY, DYNAMIC_MEM(ODPTR), STATUS)
      END IF

*  find the error array, if present

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C13ERR'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            STATUS = SAI__OK
         ELSE
            CALL GSD_INQ_SIZE(FD, NUMBER, MAXDIMS, DIMNAMES,
     :         DIMUNITS, DIMVALS, ACTDIMS, SIZE, STATUS)
*           CALL GSD_INQUIRE_ARRAY (GSDINDEX, MAXDIMS, DIMNUMBERS,
*    :         DIMNAMES, DIMUNITS, DIMVALS, DIMINDEX, ACTDIMS, SIZE,
*    :         STATUS)
            CALL DSA_GET_WORK_ARRAY (SIZE, 'FLOAT', ADDRESS, SLOT,
     :         STATUS)
            ERRPTR = DYN_ELEMENT(ADDRESS)
            CALL DSA_MAP_ERRORS ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS,
     :         SLOT, STATUS)
            OEPTR = DYN_ELEMENT(ADDRESS)

            IF (STATUS .EQ. SAI__OK) THEN
               CALL GSD_GET1R (GSDINDEX, 1, SIZE, 1, SIZE,
     :            DYNAMIC_MEM(ERRPTR), ACTVALS, STATUS)
               CALL JCMT_SORT_RDATA (C3NCH, C3MXP, C3NIS, C3NSAMPLE,
     :            DYNAMIC_MEM(ERRPTR), NCH, DYNAMIC_MEM(XPTR),
     :            DYNAMIC_MEM(YPTR), FFLAG, XMIN, XSPACE, NX, YMIN,
     :            YSPACE, NY, DYNAMIC_MEM(OEPTR), STATUS)
            END IF
         END IF
      END IF

*  construct the x-axis  ..

*  .. create the axis and map it

      CALL DSA_MAP_AXIS_DATA ('OUTPUT', 1, 'WRITE', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      XAXPTR = DYN_ELEMENT(ADDRESS)
      IF (STATUS .EQ. SAI__OK) THEN

*  fill it with index numbers (i.e. AXIS(1) = 1.0, AXIS(2) = 2.0 etc.)

         CALL GEN_NFILLF (NX, DYNAMIC_MEM(XAXPTR))

*  multiply the axis by the spacing between pixels in the map grid (in cell
*  units)

         CALL GEN_MULCAF (DYNAMIC_MEM(XAXPTR), NX, XSPACE,
     :      DYNAMIC_MEM(XAXPTR))

*  add to the axis numbers the offset of the first point

         CALL GEN_ADDCAF (DYNAMIC_MEM(XAXPTR), NX, XMIN-XSPACE,
     :      DYNAMIC_MEM(XAXPTR))

*  multiply the axis (which is currently in cell units) by the cell size

         CALL GEN_MULCAF (DYNAMIC_MEM(XAXPTR), NX, C6DX,
     :      DYNAMIC_MEM(XAXPTR))

      END IF

*  set the axis name and units

      CHAR_ITEMS(1) = CELLX_UNIT
      CHAR_ITEMS(2) = 'X offset'
      CALL DSA_SET_AXIS_INFO ('OUTPUT', 1, 2, CHAR_ITEMS, 0, 0,
     :   STATUS)

*  now the same for the y axis

      CALL DSA_MAP_AXIS_DATA ('OUTPUT', 2, 'WRITE', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      YAXPTR = DYN_ELEMENT(ADDRESS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL GEN_NFILLF (NY, DYNAMIC_MEM(YAXPTR))
         CALL GEN_MULCAF (DYNAMIC_MEM(YAXPTR), NY, YSPACE,
     :      DYNAMIC_MEM(YAXPTR))
         CALL GEN_ADDCAF (DYNAMIC_MEM(YAXPTR), NY, YMIN-YSPACE,
     :      DYNAMIC_MEM(YAXPTR))
         CALL GEN_MULCAF (DYNAMIC_MEM(YAXPTR), NY, C6DY,
     :      DYNAMIC_MEM(YAXPTR))
      END IF
      CHAR_ITEMS(1) = CELLY_UNIT
      CHAR_ITEMS(2) = 'Y offset'
      CALL DSA_SET_AXIS_INFO ('OUTPUT', 2, 2, CHAR_ITEMS, 0, 0,
     :   STATUS)


** now make the JCMT-specific structure and store the LST pixel
** array in it

*  create the 'MORE.JCMT' structure

      IF (STATUS .EQ. SAI__OK) THEN
         CALL DTA_CRVAR ('OUTPUT.MORE', 'EXT', DSTAT)
         CALL DTA_CRVAR ('OUTPUT.MORE.JCMT', 'EXT_JCMT', DSTAT)

*  create an NDF to hold the LST array, map the array

         DIMVALS(1) = NX
         DIMVALS(2) = NY
         CALL DTA_CRVAR ('OUTPUT.MORE.JCMT.LST', 'NDF', DSTAT)
         CALL DTA_CRNAM ('OUTPUT.MORE.JCMT.LST', 'DATA_ARRAY', 2,
     :      DIMVALS, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'DOUBLE', DSTAT)
         CALL DTA_CRNAM ('OUTPUT.MORE.JCMT.LST', 'DATA_ARRAY', 0, 0,
     :      DTA_NAME, DSTAT)
         CALL DTA_MUVARD (DTA_NAME, NX*NY, ADDRESS, DSTAT)
         OLSTPTR = DYN_ELEMENT(ADDRESS)
         IF (DSTAT .NE. 0) THEN
            CALL PAR_WRUSER ('MAKEMAP - Error creating LST array',
     :        IGNORE)
            GOTO 500
         END IF

*  correct the LST for the case where the observation spans a sidereal
*  day

         NLST = C3MXP * C3NSAMPLE
         IF (STATUS .EQ. SAI__OK) THEN
            CALL JCMT_CORRLST (NLST, DYNAMIC_MEM(LSTPTR), STATUS)
         END IF

*  convert the LST from hours to radians

         DFACTOR = DDEG2R * 15
         IF (STATUS .EQ. SAI__OK) THEN
            CALL GEN_MULCAD (DYNAMIC_MEM(LSTPTR), NLST, DFACTOR,
     :         DYNAMIC_MEM(LSTPTR))
         END IF

*  sort the LST array so that it maps properly to the pixel array
*  as for the data and error arrays

         IF (STATUS .EQ. SAI__OK) THEN
            CALL JCMT_SORT_DDATA (1, C3MXP, C3NIS, C3NSAMPLE,
     :         DYNAMIC_MEM(LSTPTR), 1, DYNAMIC_MEM(XPTR),
     :         DYNAMIC_MEM(YPTR), DFLAG, XMIN, XSPACE, NX, YMIN,
     :         YSPACE, NY, DYNAMIC_MEM(OLSTPTR), STATUS)
         END IF

      END IF


** This final section aims to read in basic items needed to describe
** the observation

** .OBS section

      CALL DTA_CRNAM ('OUTPUT.MORE.JCMT', 'OBS', 0, 0, SEC_NAME,
     :   DSTAT)
      CALL DTA_CRVAR (SEC_NAME, 'STRUC', DSTAT)

*  read in object name (stored in 2 parts in GSD file), set it in the output
*  file

      IF (STATUS .EQ. SAI__OK) THEN
         STRING = ' '
         NAME = 'C1SNA1'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0C (GSDINDEX, STRING(1:GSD__SZCHAR), STATUS)
         NAME = 'C1SNA2'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0C (GSDINDEX,
     :      STRING(GSD__SZCHAR+2:2*GSD__SZCHAR+1), STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            STRING = 'UNKNOWN'
            CALL PAR_WRUSER ('MAKEMAP - Error reading name of '//
     :        'object, not important', IGNORE)
            STATUS = SAI__OK
         END IF
         CALL DSA_SET_OBJECT ('OUTPUT', STRING, STATUS)
         CALL DTA_CRNAM (SEC_NAME, 'OBJECT', 1, 64, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'OBJECT', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_WRVARC (DTA_NAME, ICH_LEN(STRING), STRING, DSTAT)
      END IF

*  same for observer 1

      IF (STATUS .EQ. SAI__OK) THEN
         STRING = ' '
         NAME = 'C1ONA1'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0C (GSDINDEX, STRING(1:GSD__SZCHAR), STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            STRING = 'UNKNOWN'
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'OBS_1', 1, 32, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'OBS_1', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_WRVARC (DTA_NAME, 32, STRING, DSTAT)
      END IF

*  and observer 2

      IF (STATUS .EQ. SAI__OK) THEN
         STRING = ' '
         NAME = 'C1ONA2'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0C (GSDINDEX, STRING(1:GSD__SZCHAR), STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            STRING = 'UNKNOWN'
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'OBS_2', 1, 32, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'OBS_2', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_WRVARC (DTA_NAME, 32, STRING, DSTAT)
      END IF

*  and project name

      IF (STATUS .EQ. SAI__OK) THEN
         STRING = ' '
         NAME = 'C1PID'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0C (GSDINDEX, STRING(1:GSD__SZCHAR), STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            STRING = 'UNKNOWN'
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'PROJECT', 1, 32, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'PROJECT', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_WRVARC (DTA_NAME, 32, STRING, DSTAT)
      END IF

*  and observation number

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C1SNO'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0I (GSDINDEX, ITEMP, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            ITEMP = IFLAG
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'NOBS', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'INT', DSTAT)
         CALL DTA_WRVARI (DTA_NAME, 1, ITEMP, DSTAT)
      END IF


** .TEL section

      CALL DTA_CRNAM ('OUTPUT.MORE.JCMT', 'TEL', 0, 0, SEC_NAME,
     :   DSTAT)
      CALL DTA_CRVAR (SEC_NAME, 'STRUC', DSTAT)

*  name

      IF (STATUS .EQ. SAI__OK) THEN
         STRING = ' '
         NAME = 'C1TEL'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0C (GSDINDEX, STRING(1:GSD__SZCHAR), STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            STRING = 'UNKNOWN'
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'NAME', 1, 32, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'NAME', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_WRVARC (DTA_NAME, 32, STRING, DSTAT)
      END IF

*  front-end

      IF (STATUS .EQ. SAI__OK) THEN
         STRING = ' '
         NAME = 'C1RCV'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0C (GSDINDEX, STRING(1:GSD__SZCHAR), STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            STRING = 'UNKNOWN'
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'FRONTEND', 1, 32, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'FRONTEND', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_WRVARC (DTA_NAME, 32, STRING, DSTAT)
      END IF

*  back-end

      IF (STATUS .EQ. SAI__OK) THEN
         STRING = ' '
         NAME = 'C1BKE'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0C (GSDINDEX, STRING(1:GSD__SZCHAR), STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            STRING = 'UNKNOWN'
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'BACKEND', 1, 32, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'BACKEND', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_WRVARC (DTA_NAME, 32, STRING, DSTAT)
      END IF

* longitude, get, convert to east longitude and radians

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C1LONG'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0D (GSDINDEX, DTEMP, STATUS)
         DTEMP = (360.0D0-DTEMP) * DDEG2R
         IF (STATUS .NE. SAI__OK) THEN
            DTEMP = DFLAG
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'LONG', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'DOUBLE', DSTAT)
         CALL DTA_WRVARD (DTA_NAME, 1, DTEMP, DSTAT)
      END IF

*  latitude, get, convert to radians

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C1LAT'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0D (GSDINDEX, DTEMP, STATUS)
         DTEMP = DTEMP * DDEG2R
         IF (STATUS .NE. SAI__OK) THEN
            DTEMP = DFLAG
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'LAT', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'DOUBLE', DSTAT)
         CALL DTA_WRVARD (DTA_NAME, 1, DTEMP, DSTAT)
      END IF

*  height, get, convert to metres

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C1HGT'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0D (GSDINDEX, DTEMP, STATUS)
         DTEMP = DTEMP * 1000.0D0
         IF (STATUS .NE. SAI__OK) THEN
            DTEMP = DFLAG
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'HEIGHT', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'DOUBLE', DSTAT)
         CALL DTA_WRVARD (DTA_NAME, 1, DTEMP, DSTAT)
      END IF

** .MAP structure

      CALL DTA_CRNAM ('OUTPUT.MORE.JCMT', 'MAP', 0, 0, SEC_NAME,
     :   DSTAT)
      CALL DTA_CRVAR (SEC_NAME, 'STRUC', DSTAT)

*  the coordinate system of the map centre

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C4CSC'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         STRING = ' '
         CALL GSD_GET0C (GSDINDEX, STRING, STATUS)
         ITEMP = ICH_FOLD (STRING)
         IF (STATUS .NE. SAI__OK) THEN
            STRING = 'UNKNOWN'
            CALL PAR_WRUSER ('MAKEMAP - Error reading coordinate '//
     :        'system of map centre, this will have to be set manually',
     :        IGNORE)
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'CENTRE_CRD', 1, 32, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'CENTRE_CRD', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_WRVARC (DTA_NAME, 32, STRING, DSTAT)
      END IF

*  the epoch of the centre coordinates

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C4EPH'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0D (GSDINDEX, DTEMP, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            DTEMP = DFLAG
            CALL PAR_WRUSER ('MAKEMAP - Error reading Epoch of '//
     :        'map centre, this will have to be set manually', IGNORE)
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'EPOCH', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'DOUBLE', DSTAT)
         CALL DTA_WRVARD (DTA_NAME, 1, DTEMP, DSTAT)
      END IF

*  the RA, Dec of the map centre, convert to radians

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C4ERA'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0D (GSDINDEX, DTEMP, STATUS)
         DTEMP = DTEMP * DDEG2R
         IF (STATUS .NE. SAI__OK) THEN
            DTEMP = DFLAG
            CALL PAR_WRUSER ('MAKEMAP - Error reading RA of '//
     :        'map centre, this will have to be set manually', IGNORE)
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'RACEN', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'DOUBLE', DSTAT)
         CALL DTA_WRVARD (DTA_NAME, 1, DTEMP, DSTAT)
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C4EDEC'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0D (GSDINDEX, DTEMP, STATUS)
         DTEMP = DTEMP * DDEG2R
         IF (STATUS .NE. SAI__OK) THEN
            DTEMP = DFLAG
            CALL PAR_WRUSER ('MAKEMAP - Error reading Dec of '//
     :        'map centre, this will have to be set manually', IGNORE)
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'DECCEN', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'DOUBLE', DSTAT)
         CALL DTA_WRVARD (DTA_NAME, 1, DTEMP, DSTAT)
      END IF

*  the coord system of the local offsets

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C4LSC'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         STRING = ' '
         CALL GSD_GET0C (GSDINDEX, STRING, STATUS)
         ITEMP = ICH_FOLD (STRING)
         IF (STATUS .NE. SAI__OK) THEN
            STRING = 'UNKNOWN'
            CALL PAR_WRUSER ('MAKEMAP - Error reading coordinate '//
     :        'system of local offsets, this will have to be set '//
     :        'manually', IGNORE)
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'LOCAL_CRD', 1, 32, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'LOCAL_CRD', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_WRVARC (DTA_NAME, 32, STRING, DSTAT)
      END IF

*  the angle between the North of the local coord system and the y
*  cell axis (anti-clockwise, degrees), convert to radians

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'CELL_V2Y'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0R (GSDINDEX, RTEMP, STATUS)
         RTEMP = RTEMP * DDEG2R
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - Error reading angle between '//
     :        'local vertical and cell y axis, this will have to be '//
     :        'set manually', IGNORE)
            RTEMP = FFLAG
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'V2Y', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
         CALL DTA_WRVARF (DTA_NAME, 1, RTEMP, DSTAT)
      END IF

*  the angle between the x-axis and the y-axis of the cells
*  (anti-clockwise, degrees), convert to radians

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C4AXY'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0R (GSDINDEX, RTEMP, STATUS)
         RTEMP = RTEMP * DDEG2R
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - Error reading angle between '//
     :        'y and x cell axes, this will have to be set manually',
     :        IGNORE)
            RTEMP = FFLAG
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'X2Y', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
         CALL DTA_WRVARF (DTA_NAME, 1, RTEMP, DSTAT)
      END IF

*  the cell sizes, read in earlier

      IF (STATUS .EQ. SAI__OK) THEN
         CALL DTA_CRNAM (SEC_NAME, 'CELL_X', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
         CALL DTA_WRVARF (DTA_NAME, 1, C6DX, DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'CELL_X_UNIT', 1, GSD__SZUNIT,
     :      DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'CELL_X_UNIT', 0, 0, DTA_NAME,
     :      DSTAT)
         CALL DTA_WRVARC (DTA_NAME, GSD__SZUNIT, CELLX_UNIT, DSTAT)
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         CALL DTA_CRNAM (SEC_NAME, 'CELL_Y', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
         CALL DTA_WRVARF (DTA_NAME, 1, C6DY, DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'CELL_Y_UNIT', 1, GSD__SZUNIT,
     :      DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'CELL_Y_UNIT', 0, 0, DTA_NAME,
     :      DSTAT)
         CALL DTA_WRVARC (DTA_NAME, GSD__SZUNIT, CELLY_UNIT, DSTAT)
      END IF

*  the on-the-fly flag

      IF (STATUS .EQ. SAI__OK) THEN
         CALL DTA_CRNAM (SEC_NAME, 'ON_FLY', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'INT', DSTAT)
         IF (C3FLY) THEN
            CALL DTA_WRVARI (DTA_NAME, 1, 1, DSTAT)
         ELSE
            CALL DTA_WRVARI (DTA_NAME, 1, 0, DSTAT)
         END IF
      END IF

*  work out integration time per pixel and write that

      IF (STATUS .EQ. SAI__OK) THEN
         IF (C3FLY) THEN
            IF (C3MXP .NE. 0) THEN
               INT_TIME  = C3SRT / C3MXP
            ELSE
               INT_TIME = 0.0
            END IF
         ELSE
            INT_TIME = C3SRT
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'INT_TIME', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
         CALL DTA_WRVARF (DTA_NAME, 1, INT_TIME, DSTAT)
      END IF

*  the scan direction

      IF (STATUS .EQ. SAI__OK) THEN
         CALL DTA_CRNAM (SEC_NAME, 'SCAN_DIR', 1, GSD__SZCHAR,
     :      DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'SCAN_DIR', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_WRVARC (DTA_NAME, GSD__SZCHAR, C6SD, DSTAT)
      END IF

*  the time of start of observation, convert to modified
*  Julian day

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C3DAT'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0D (GSDINDEX, DATE, STATUS)
         NAME = 'C3UT'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0D (GSDINDEX, HOUR, STATUS)
         IY = INT(DATE)
         IM = INT((DATE-IY)*100)
         ID = NINT(((DATE-IY)*100-IM)*100)
         CALL SLA_CLDJ (IY, IM, ID, DJM, SLASTAT)
         MJDSTART = DJM + HOUR / 24
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - error finding modified '//
     :         'Julian date of observation, this will have to be '//
     :         'set manually', IGNORE)
            STATUS = SAI__OK
            MJDSTART = DFLAG
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'MJD_START', 0, 0, DTA_NAME,
     :      DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'DOUBLE', DSTAT)
         CALL DTA_WRVARD (DTA_NAME, 1, MJDSTART, DSTAT)
      END IF

*  get the chopper function, 2-position (SQUARE) or 3-position (TRIPOS)

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C4FUN'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0C (GSDINDEX, C4FUN, STATUS)
         ITEMP = ICH_FOLD (C4FUN)
         IF (STATUS .NE. SAI__OK) THEN
            STRING = 'UNKNOWN'
            CALL PAR_WRUSER ('MAKEMAP - Error reading chopper '//
     :        'function, this will have to be set manually', IGNORE)
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'CHOP_FUN', 1, 32, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'CHOP_FUN', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_WRVARC (DTA_NAME, 32, C4FUN, DSTAT)
      END IF

*  get the beam throw in arcseconds

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C4THROW'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0R (GSDINDEX, RTEMP, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - error reading chop-throw, '//
     :         'this will have to be set manually', IGNORE)
            STATUS = SAI__OK
            RTEMP = FFLAG
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'CHOP_THRW', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
         CALL DTA_WRVARF (DTA_NAME, 1, RTEMP, DSTAT)
      END IF

*  the position angle of the chopper (radians)

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C4POSANG'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0R (GSDINDEX, RTEMP, STATUS)
         RTEMP = RTEMP * DDEG2R
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - error reading chopper '//
     :         'position angle, this will have to be set manually',
     :         IGNORE)
            STATUS = SAI__OK
            RTEMP = FFLAG
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'CHOP_PA', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
         CALL DTA_WRVARF (DTA_NAME, 1, RTEMP, DSTAT)
      END IF

*  the coordinate system of the chopper

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C4SMCO'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         STRING = ' '
         CALL GSD_GET0C (GSDINDEX, STRING, STATUS)
         ITEMP = ICH_FOLD (STRING)
         IF (STATUS .NE. SAI__OK) THEN
            STRING = 'UNKNOWN'
            CALL PAR_WRUSER ('MAKEMAP - Error reading coordinate '//
     :        'system of chopper, this will have to be set manually',
     :        IGNORE)
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'CHOP_CRD', 1, 32, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (SEC_NAME, 'CHOP_CRD', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_WRVARC (DTA_NAME, 32, STRING, DSTAT)
      END IF

*  telescope beam

      IF (STATUS .EQ. SAI__OK) THEN
         CALL PAR_SDCHAR ('TEL_BEAM', 'M', STATUS)
         CALL PAR_RDCHAR ('TEL_BEAM', 'M', STRING)
         IF (PAR_ABORT()) THEN
            FAULT = .TRUE.
            GOTO 500
         END IF
         ITEMP = ICH_FOLD (STRING)
         IF ((STRING(:ICH_LEN(STRING)) .NE. 'L') .AND.
     :       (STRING(:ICH_LEN(STRING)) .NE. 'M') .AND.
     :       (STRING(:ICH_LEN(STRING)) .NE. 'R')) THEN
             CALL PAR_WRUSER ('MAKEMAP - invalid telescope beam, '//
     :         'set to M.', IGNORE)
             STRING = 'M'
         END IF
         IF (STRING(1:1) .NE. 'M') THEN
            CALL PAR_WRUSER ('MAKEMAP - this package can only handle '//
     :        'maps made in the M beam. If you see this message '//
     :        'contact John Lightfoot (REVAD::JFL).', IGNORE)
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'TEL_BEAM', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_WRVARC (DTA_NAME, 1, STRING, DSTAT)
      END IF

*  positive beam

      IF (STATUS .EQ. SAI__OK) THEN
         IF (C4FUN .NE. 'TRIPOS') THEN
            CALL PAR_SDCHAR ('POS_BEAM', 'R', STATUS)
            CALL PAR_RDCHAR ('POS_BEAM', 'R', STRING)
            IF (PAR_ABORT()) THEN
               FAULT = .TRUE.
               GOTO 500
            END IF
            ITEMP = ICH_FOLD (STRING)
            IF ((STRING(:ICH_LEN(STRING)) .NE. 'L') .AND.
     :          (STRING(:ICH_LEN(STRING)) .NE. 'R')) THEN
                CALL PAR_WRUSER ('MAKEMAP - invalid positive beam, '//
     :            'set to L.', IGNORE)
                STRING = 'L'
            END IF
            CALL DTA_CRNAM (SEC_NAME, 'POS_BEAM', 0, 0, DTA_NAME, DSTAT)
            CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
            CALL DTA_WRVARC (DTA_NAME, 1, STRING, DSTAT)
         END IF
      END IF

*  centre observing frequency in GHz

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C12CF'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0D (GSDINDEX, DTEMP, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - error reading observation '//
     :         'frequency, this will have to be set manually', IGNORE)
            STATUS = SAI__OK
            DTEMP = DFLAG
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'FREQ', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'DOUBLE', DSTAT)
         CALL DTA_WRVARD (DTA_NAME, 1, DTEMP, DSTAT)
      END IF


**ENVIRONMENT specific parameters

      CALL DTA_CRNAM ('OUTPUT.MORE.JCMT', 'ENVRNMNT', 0, 0, SEC_NAME,
     :   DSTAT)
      CALL DTA_CRVAR (SEC_NAME, 'STRUC', DSTAT)

*  temperature

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C5AT'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0R (GSDINDEX, RTEMP, STATUS)
         RTEMP = RTEMP + 273.15
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - Error reading ambient '//
     :         'temperature, this will have to be set manually', IGNORE)
            RTEMP = FFLAG
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'AMB_TEMP', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
         CALL DTA_WRVARF (DTA_NAME, 1, RTEMP, DSTAT)
      END IF

*  pressure in mbar

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C5PRS'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0R (GSDINDEX, RTEMP, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - Error reading pressure, '//
     :         'this will have to be set manually', IGNORE)
            RTEMP = FFLAG
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'PRESSURE', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
         CALL DTA_WRVARF (DTA_NAME, 1, RTEMP, DSTAT)
      END IF

*  relative humidity (convert to fraction)

      IF (STATUS .EQ. SAI__OK) THEN
         NAME = 'C5RH'
         CALL GSD_FIND (FD, NAME, NUMBER, UNIT, TYPE, ARRAY, GSDINDEX,
     :      STATUS)
         CALL GSD_GET0R (GSDINDEX, RTEMP, STATUS)
         RTEMP = RTEMP / 100.0
         IF (STATUS .NE. SAI__OK) THEN
            CALL PAR_WRUSER ('MAKEMAP - Error reading relative '//
     :         'humidity, this will have to be set manually', IGNORE)
            RTEMP = FFLAG
            STATUS = SAI__OK
         END IF
         CALL DTA_CRNAM (SEC_NAME, 'REL_HUMID', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
         CALL DTA_WRVARF (DTA_NAME, 1, RTEMP, DSTAT)
      END IF

**Create stub for pointing correction structure

      CALL DTA_CRNAM ('OUTPUT.MORE.JCMT', 'PCORR', 0, 0, SEC_NAME,
     :   DSTAT)
      CALL DTA_CRVAR (SEC_NAME, 'STRUC', DSTAT)


**And software version record

      CALL DTA_CRNAM ('OUTPUT.MORE.JCMT', 'SOFTWARE', 0, 0, SEC_NAME,
     :   DSTAT)
      CALL DTA_CRVAR (SEC_NAME, 'STRUC', DSTAT)
      CALL DTA_CRNAM (SEC_NAME, 'MAKEMAP', 0, 0, DTA_NAME, DSTAT)
      CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
      CALL DTA_WRVARF (DTA_NAME, 1, 1.2, DSTAT)


*  tidy up

  500 CONTINUE

      IF (STATUS .NE. SAI__OK) FAULT = .TRUE.

      IF (DSTAT .NE. 0) THEN
          CALL DTA_ERROR (DSTAT, ERROR)
          CALL PAR_WRUSER (ERROR(:ICH_LEN(ERROR)), IGNORE)
          FAULT = .TRUE.
      END IF

      CALL DSA_CLOSE (STATUS)

      IF (FAULT) CALL FIG_SETERR

      END
