*  History:
*     17 Nov 1993 (hme):
*        Replace LIB$GET_VM with PSX_MALLOC.
*     23 Nov 1993 (hme):
*        In DEFINE_FLAG_TABLE, remove references to GSD.
*     01 Jan 1994 (rp):
*        Make new entry for TOTINT etc to update to VAX V6.3
*     07 Jan 1994 (rp):
*        Replace PSX_MALLOC by IGETVM, etc
*     15 Jan 1994 (rp):
*        Define table size from include file with parameter LSTMAX
*     31 Dec 1994 (rpt)
*        Support new GENLIB, make SCAN writeable, add SUBSCAN_NO
*     19 Feb 1997 (timj)
*        Increase size of GSDNAME to 32 (from 16)
*     5  Mar 1997 (timj)
*        Add new symbol MAPLIMITS to return actual limits used for greyscale
*     20 Sep 2000 (ajc)
*        Unused ierr in DEFINE_USER_TABLE
*     25 Jul 2004 (timj)
*        Be consistent in use of GSD_VAR.INC vs GSD_VAR
*-----------------------------------------------------------------------

      SUBROUTINE SPECX_INIT_TABLE

      IMPLICIT NONE

*     Define symbol table

      INTEGER   TABLE_ADDRESS      ! Location of table in memory
      INTEGER   NO_ENTRIES         ! Number of entries made in table to date
      COMMON /SYMTABS/ NO_ENTRIES, TABLE_ADDRESS

      INTEGER    LSTMAX
      PARAMETER (LSTMAX = 16384)
      INTEGER    MAX_TABLE
      PARAMETER (MAX_TABLE = 512)

*     Local variables:

      INTEGER   ISTAT

*     Functions:

      INTEGER   IGETVM

*  Ok, go...

*     Get virtual memory for variable symbol table

      ISTAT = IGETVM (LSTMAX, .FALSE., 'SPECX_INIT_TABLE',
     &                TABLE_ADDRESS)

*     Define the location of the table for GEN routines
*     Note that we want direct access to the number of entries, so keep
*       the ADDRESS of the variable containing the # entries inside GENLIB.
      CALL GEN_SET_SYMT (TABLE_ADDRESS, %LOC(NO_ENTRIES))

      NO_ENTRIES = 0

      CALL DEFINE_DATA_TABLE
      CALL DEFINE_FLAG_TABLE
      CALL DEFINE_USER_TABLE
      CALL DEFINE_USER_VARS

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE DEFINE_DATA_TABLE

*     Defines symbol table for SPECX scan header

      IMPLICIT NONE

*     Include header common block

      INCLUDE 'STACKCOMM'

*     Local variables

      INTEGER*4 IERR

      LOGICAL*4 TRUE  /.TRUE./
      LOGICAL*4 FALSE / .FALSE./
      SAVE      TRUE, FALSE

      CALL GEN_MAKESYMB ('*T',      'L4', 1, %LOC(TRUE),  IERR)
      CALL GEN_MAKESYMB ('*F',      'L4', 1, %LOC(FALSE), IERR)
      CALL GEN_MAKESYMB ('*TRUE',   'L4', 1, %LOC(TRUE),  IERR)
      CALL GEN_MAKESYMB ('*FALSE',  'L4', 1, %LOC(FALSE), IERR)
      CALL GEN_MAKESYMB ('*.TRUE.', 'L4', 1, %LOC(TRUE),  IERR)
      CALL GEN_MAKESYMB ('*.FALSE.','L4', 1, %LOC(FALSE), IERR)

      CALL GEN_MAKESYMB ('TSYS', 'R4', 8, %LOC(TSYS), IERR)

      CALL GEN_MAKESYMB ('LO_FREQ',   'R8', 8, %LOC(LOFREQ), IERR)
      CALL GEN_MAKESYMB ('IF_FREQ',   'R8', 8, %LOC(IFFREQ), IERR)
      CALL GEN_MAKESYMB ('VSL',       'R4', 1, %LOC(VSL),    IERR)
      CALL GEN_MAKESYMB ('VES',       'R4', 1, %LOC(VES),    IERR)
      CALL GEN_MAKESYMB ('VTE',       'R4', 1, %LOC(VTE),    IERR)
      CALL GEN_MAKESYMB ('VLSR',      'R4', 1, %LOC(VLSR),   IERR)
      CALL GEN_MAKESYMB ('AZIMUTH',   'R4', 1, %LOC(AZ),     IERR)
      CALL GEN_MAKESYMB ('ELEVATION', 'R4', 1, %LOC(EL),     IERR)

      CALL GEN_MAKESYMB ('F_REST', 'I4', 8, %LOC(JFREST), IERR)
      CALL GEN_MAKESYMB ('F_CEN',  'I4', 8, %LOC(JFCEN),  IERR)
      CALL GEN_MAKESYMB ('F_INC',  'I4', 8, %LOC(JFINC),  IERR)

      CALL GEN_MAKESYMB ('INT_TIME', 'I4', 1, %LOC(INTT), IERR)

      CALL GEN_MAKESYMB ('T_REC', 'I4', 8, %LOC(ITREC), IERR)
      CALL GEN_MAKESYMB ('T_SKY', 'I4', 8, %LOC(ITSKY), IERR)
      CALL GEN_MAKESYMB ('T_TEL', 'I4', 8, %LOC(ITTEL), IERR)

      CALL GEN_MAKESYMB ('NPTS', 'I4', 8, %LOC(NPTS), IERR)

      CALL GEN_MAKESYMB ('RA',  'R8', 1, %LOC(RA),  IERR)
      CALL GEN_MAKESYMB ('DEC', 'R8', 1, %LOC(DEC), IERR)

      CALL GEN_MAKESYMB ('SCAN_NO', 'I4', 1, %LOC(LSCAN), IERR)
      CALL GEN_MAKESYMB ('MODE', 'I4', 1, %LOC(IMODE), IERR)
      CALL GEN_MAKESYMB ('*NQUAD', 'I4', 1, %LOC(NQUAD), IERR)
      CALL GEN_MAKESYMB ('*BLOCK_1', 'I4', 1, %LOC(IST), IERR)
      CALL GEN_MAKESYMB ('*BLOCK_2', 'I4', 1, %LOC(IEND), IERR)
      CALL GEN_MAKESYMB ('CAL_ZD', 'I4', 1, %LOC(ICALZD), IERR)
      CALL GEN_MAKESYMB ('LSR_FLAG', 'I4', 1, %LOC(LSRFLG), IERR)
      CALL GEN_MAKESYMB ('CENTRE_QUAD', 'I4', 1, %LOC(IQCEN), IERR)
      CALL GEN_MAKESYMB ('RA_OFFSET',  'R4', 1, %LOC(DRA),  IERR)
      CALL GEN_MAKESYMB ('DEC_OFFSET', 'R4', 1, %LOC(DDEC), IERR)

      CALL GEN_MAKESYMB ('SCAN_TITLE', 'C26', 1, %LOC(ITITLE), IERR)
      CALL GEN_MAKESYMB ('SOURCE_NAME','C9',1,%LOC(ITITLE(13:13)),IERR)
      CALL GEN_MAKESYMB ('SUBSCAN_NO','C4',1,%LOC(ITITLE(6:6)),IERR)
      CALL GEN_MAKESYMB ('SCAN_DATE', 'C9', 1, %LOC(IDATE), IERR)
      CALL GEN_MAKESYMB ('UT_FLAG', 'L1', 1, %LOC(IUTFLG), IERR)
      CALL GEN_MAKESYMB ('SCAN_TIME', 'C8', 1, %LOC(ITIME), IERR)

      CALL GEN_MAKESYMB ('DATA', 'R4', 1, %LOC(DATA), IERR)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE DEFINE_FLAG_TABLE

      IMPLICIT NONE

*     Include flags common block

      INCLUDE 'FILHD'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'CUBE'
      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'GSD_VAR.INC'
      INCLUDE 'ERRORS'

*     Brute force access to baseline fits

      INTEGER*4 NXOLD_G
      INTEGER*4 NGOLD
      REAL*4    AWP
      COMMON /LINFT/ NXOLD_G, NGOLD, AWP(3,10)

      INTEGER*4 NPOLY
      INTEGER*4 NSIN
      INTEGER*4 NXOLD_S
      REAL*4    POLY_COEFF
      REAL*4    FREQ_COEFF
      REAL*4    APHI
      COMMON /SINFT/ NPOLY, NSIN, NXOLD_S,
     &               POLY_COEFF(30), FREQ_COEFF, APHI(2,14)

*     Local variables

      INTEGER*4 IERR

*     FLAGCOMM items:

      CALL GEN_MAKESYMB ('IN_FILE',     'I4', 1,  %LOC(INFILE),  IERR)
      CALL GEN_MAKESYMB ('OUT_FILE',    'I4', 1,  %LOC(OUTFILE), IERR)

      CALL GEN_MAKESYMB ('N_INTS',      'I4', 1,  %LOC(NPR),     IERR)
      CALL GEN_MAKESYMB ('BLF_INTS',    'I4', 16, %LOC(IBLP),    IERR)
      CALL GEN_MAKESYMB ('N_FSSP',      'I4', 1,  %LOC(NFSSP),   IERR)
      CALL GEN_MAKESYMB ('FSS_INTS',    'I4', 10, %LOC(IFSSP),   IERR)
      CALL GEN_MAKESYMB ('RLB_INTS',    'I4', 4,  %LOC(IBLRPT),  IERR)
      CALL GEN_MAKESYMB ('SECTOR_MASK', 'I4', 8,  %LOC(MASK),    IERR)
      CALL GEN_MAKESYMB ('*LINK',       'I4', 1,  %LOC(LINK),    IERR)
      CALL GEN_MAKESYMB ('MERGE_OFFSET','L4',1,%LOC(SECTOR_OFFSET),IERR)

      CALL GEN_MAKESYMB ('NCONT',       'I4', 1,  %LOC(NCONT),   IERR)
      CALL GEN_MAKESYMB ('CONTOUR_0',   'R4', 1,  %LOC(CONT0),   IERR)
      CALL GEN_MAKESYMB ('CONTOUR_INT', 'R4', 1,  %LOC(CONTI),   IERR)
      CALL GEN_MAKESYMB ('NCSET',       'I4', 1,  %LOC(NCSET),   IERR)
      CALL GEN_MAKESYMB ('CONTOUR_LEVS','R4', 16, %LOC(CLEVELS), IERR)
      CALL GEN_MAKESYMB ('LINTYP_NEG',  'I4', 1,  %LOC(LTNEG),   IERR)
      CALL GEN_MAKESYMB ('LINTYP_POS',  'I4', 1,  %LOC(LTPOS),   IERR)
      CALL GEN_MAKESYMB ('LINTYP_ZERO', 'I4', 1,  %LOC(LTZ),     IERR)

      CALL GEN_MAKESYMB ('PLOT_CONT',   'L4', 1,  %LOC(PLOTCONT),IERR)
      CALL GEN_MAKESYMB ('PLOT_GREY',   'L4', 1,  %LOC(PLOTGREY),IERR)
      CALL GEN_MAKESYMB ('AUTOGREY',    'L4', 1,  %LOC(AUTOGREY),IERR)
      CALL GEN_MAKESYMB ('GREYLIM',     'R4', 2,  %LOC(GREYLIM), IERR)
      CALL GEN_MAKESYMB ('MAPLIMITS',   'R4', 2,  %LOC(MAPLIMITS), IERR)
      CALL GEN_MAKESYMB ('COLOUR_TABLE','I4', 1,%LOC(COLOUR_TABLE),IERR)
      CALL GEN_MAKESYMB ('OVERLAY_CONTOURS','L4',1,%LOC(OVERCONT), IERR)
      
      CALL GEN_MAKESYMB ('MULT_FACT',   'R4', 1,  %LOC(FACT),    IERR)
      CALL GEN_MAKESYMB ('DIV_FACT',    'R4', 1,  %LOC(DIV),     IERR)
      CALL GEN_MAKESYMB ('CHAN_SHIFT',  'R4', 1,  %LOC(ACHAN),   IERR)
      CALL GEN_MAKESYMB ('OFFSET',      'R4', 1,  %LOC(OFF),     IERR)

      CALL GEN_MAKESYMB ('BEAM_FWHM',   'R4', 1,  %LOC(FWHM),    IERR)
      CALL GEN_MAKESYMB ('BEAM_EXTENT', 'R4', 1,  %LOC(WMAX),    IERR)
      CALL GEN_MAKESYMB ('VEL_FWHM',    'R4', 1,  %LOC(VWIDHM),  IERR)
      CALL GEN_MAKESYMB ('VEL_EXTENT',  'R4', 1,  %LOC(VFNMAX),  IERR)
      CALL GEN_MAKESYMB ('MAP_TOL',     'R4', 1,  %LOC(MAP_TOL), IERR)
      CALL GEN_MAKESYMB ('REPLACE',     'L4', 1, 
     &                                   %LOC(REPLACE_MAP_DATA), IERR)

      CALL GEN_MAKESYMB ('VELOCITY_FRAME','C4', 1, %LOC(VEL_REF), IERR)
      CALL GEN_MAKESYMB ('VELOCITY_LAW',  'C3', 1, %LOC(VEL_DEF), IERR)
      CALL GEN_MAKESYMB ('VELOCITY',      'R4', 1, %LOC(VELOUT),  IERR)

      CALL GEN_MAKESYMB ('X_UNITS',  'C6',  1, %LOC(XAXIS_UNITS), IERR)
      CALL GEN_MAKESYMB ('X_NAME',   'C10', 1, %LOC(XAXIS_NAME),  IERR)
      CALL GEN_MAKESYMB ('Y_UNITS',  'C16', 1, %LOC(YAXIS_UNITS), IERR)
      CALL GEN_MAKESYMB ('Y_NAME',   'C16', 1, %LOC(YAXIS_NAME),  IERR)

      CALL GEN_MAKESYMB ('HISTOGRAM',    'L4', 1, %LOC(HISTOGRAM),IERR)
      CALL GEN_MAKESYMB ('LINE_WEIGHT',  'I4', 1, %LOC(LWEIGHT),  IERR)
      CALL GEN_MAKESYMB ('LINE_TYPE',    'I4', 1, %LOC(IPEN),     IERR)
      CALL GEN_MAKESYMB ('*DEVICE',      'I4', 1, %LOC(IDEV),     IERR)
      CALL GEN_MAKESYMB ('*TERMINAL',    'L4', 1, %LOC(TERMINAL), IERR)
      CALL GEN_MAKESYMB ('AUTO_CONT',    'L4', 1, %LOC(ICAUTO),   IERR)
      CALL GEN_MAKESYMB ('SCALES_SET',   'L4', 1, %LOC(ISETSC),   IERR)
      CALL GEN_MAKESYMB ('INTERACTIVE',  'L4',1,%LOC(INTERACTIVE),IERR)
      CALL GEN_MAKESYMB ('BADPIX_VALUE', 'R4',1,%LOC(BADPIX_VAL), IERR)

      CALL GEN_MAKESYMB ('COLOR5_START', 'R4', 1, %LOC(C5START),  IERR)
      CALL GEN_MAKESYMB ('COLOR5_ROTATE','R4', 1, %LOC(C5ROTAT),  IERR)
      CALL GEN_MAKESYMB ('COLOR5_EXPONENT','R4',1,%LOC(C5EXP),    IERR)

*     Telescope/astrometric data

      CALL GEN_MAKESYMB ('LATITUDE',     'R8', 1,%LOC(ALAT),      IERR)
      CALL GEN_MAKESYMB ('LONGITUDE',    'R8', 1,%LOC(ALONG),     IERR)

*     Data from reduction operations

      CALL GEN_MAKESYMB ('TMAX',       'R4', 1,  %LOC(TMAX),      IERR)
      CALL GEN_MAKESYMB ('VMAX',       'R4', 1,  %LOC(VMAX),      IERR)
      CALL GEN_MAKESYMB ('TOTINT',     'R4', 1,  %LOC(TOTINT),    IERR)
      CALL GEN_MAKESYMB ('FSS_AV',     'R4', 1,  %LOC(FSS_AV),    IERR)
      CALL GEN_MAKESYMB ('FSS_VAR',    'R4', 1,  %LOC(FSS_VAR),   IERR)
      CALL GEN_MAKESYMB ('FSS_SD',     'R4', 1,  %LOC(FSS_SD),    IERR)
      CALL GEN_MAKESYMB ('CENTROID',   'R4', 1,  %LOC(XCHAN),     IERR)

*     X-axis frequency corrections

      CALL GEN_MAKESYMB ('FCORRECT',   'L4', 1,  %LOC(FCORRECT),  IERR)
      CALL GEN_MAKESYMB ('FRQCOEFF',   'R4', 6,  %LOC(FRQCOEFF),  IERR)

*     Data from cube?

      CALL GEN_MAKESYMB ('*CUBE_LOADED', 'L4', 1,
     &                    %LOC(CUBE_IN_MEMORY), IERR)
*     CALL GEN_MAKESYMB ('*CUBE_MODIFIED', 'L4', 1,
*    &                    %LOC(NEW_CUBE_LOADED), IERR)
      CALL GEN_MAKESYMB ('*CUBE_ROTATED', 'L4', 1,
     &                    %LOC(MAP_ROTATED), IERR)
      CALL GEN_MAKESYMB ('*CUBE_INTERPOLATED', 'L4', 1,
     &                    %LOC(MAP_INTERPOLATED), IERR)
      CALL GEN_MAKESYMB ('*CUBE_SIZE',   'I4', 1, %LOC(NCUBE), IERR)

*     Control of sin/polynomical and gaussian fits

      CALL GEN_MAKESYMB ('FIT_TOL',     'R4', 1,  %LOC(TOL),    IERR)
      CALL GEN_MAKESYMB ('MAX_ITS',     'I4', 1,  %LOC(MAXITS), IERR)
      CALL GEN_MAKESYMB ('FIT_DEBUG',   'I4', 1,  %LOC(IER),    IERR)

*     Data from sin/polynomial and gaussian fits

      CALL GEN_MAKESYMB ('N_GAUSS',      'I4', 1,  %LOC(NGOLD), IERR)
      CALL GEN_MAKESYMB ('AMP_WID_POS',  'R4', 30, %LOC(AWP),   IERR)
      CALL GEN_MAKESYMB ('N_POLY',       'I4', 1,  %LOC(NPOLY), IERR)
      CALL GEN_MAKESYMB ('N_SIN',        'I4', 1,  %LOC(NSIN),  IERR)
      CALL GEN_MAKESYMB ('AMP_PHASE',    'R4', 28, %LOC(APHI),  IERR)
      CALL GEN_MAKESYMB ('FREQ_COEFF','R4', 1,  %LOC(FREQ_COEFF), IERR)
      CALL GEN_MAKESYMB ('POLY_COEFF','R4', 30, %LOC(POLY_COEFF), IERR)

*     Data from header of currently opened map

      CALL GEN_MAKESYMB ('MAP_OPEN',    'L4', 1, %LOC(MAP_OPEN),   IERR)
      CALL GEN_MAKESYMB ('MAP_NAME',    'C40',1, %LOC(ID),         IERR)
      CALL GEN_MAKESYMB ('*NO_MAP_PTS', 'I4', 1, %LOC(NSPEC),      IERR)
      CALL GEN_MAKESYMB ('*POSN_ANGLE', 'R4', 1, %LOC(POS_ANGLE),  IERR)
      CALL GEN_MAKESYMB ('*NO_XPIX',    'I4', 1, %LOC(MSTEP),      IERR)
      CALL GEN_MAKESYMB ('*NO_YPIX',    'I4', 1, %LOC(NSTEP),      IERR)
      CALL GEN_MAKESYMB ('*XSIZ_CELL',  'R4', 1, %LOC(CELL_XSIZE), IERR)
      CALL GEN_MAKESYMB ('*YSIZ_CELL',  'R4', 1, %LOC(CELL_YSIZE), IERR)

*     Data from GSD index and INDEX-FILE

      CALL GEN_MAKESYMB ('GSD_FILENAME', 'C32', 1, %LOC(GSDNAME),  IERR)
      CALL GEN_MAKESYMB ('*NO_FILE_SPECTRA','I4', 1, %LOC(NSCAN),  IERR)
      CALL GEN_MAKESYMB ('*NO_GSD_SPECTRA', 'I4', 1, %LOC(NNSPEC), IERR)
      CALL GEN_MAKESYMB ('*NO_NEW_SPECTRA','I4',1,%LOC(NO_NEWSPEC),IERR)

*     Error parameters

      CALL GEN_MAKESYMB ('ERROR_SET',   'L4', 1, %LOC(ERROR_SET),  IERR)
      CALL GEN_MAKESYMB ('ERROR',       'I4', 1, %LOC(ERROR),      IERR)
      CALL GEN_MAKESYMB ('LAST_ERROR',  'I4', 1, %LOC(LAST_ERROR), IERR)
      CALL GEN_MAKESYMB ('SEVERITY',    'I4', 1, %LOC(SEVERITY),   IERR)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE DEFINE_USER_TABLE

      IMPLICIT NONE

*     User table definition

      INTEGER   UMEMORY_PTR
      INTEGER   UMEMORY_SIZE
      INTEGER   UMEMORY_LENGTH
      COMMON /UMEMORY/ UMEMORY_PTR, UMEMORY_SIZE,
     &                 UMEMORY_LENGTH

*     Local variables

      INTEGER   ISTAT

*     Functions:

      INTEGER   IGETVM

*     Get some virtual memory for user defined symbols

      INTEGER*4 NO_BYTES
      PARAMETER (NO_BYTES=4096)
 
      UMEMORY_SIZE = NO_BYTES
      ISTAT = IGETVM (UMEMORY_SIZE, .FALSE., 'DEFINE_USER_TABLE',
     &                UMEMORY_PTR)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE DEFINE_USER_VARS

*  Routine to allocate space in the user table for the counting variables
*  I -> N (as in FORTRAN)

      IMPLICIT  NONE
      INTEGER*4 IERR

      CALL SPECX_MAKE_VAR ('i', 'I4', IERR)
      CALL SPECX_MAKE_VAR ('j', 'I4', IERR)
      CALL SPECX_MAKE_VAR ('k', 'I4', IERR)
      CALL SPECX_MAKE_VAR ('l', 'I4', IERR)
      CALL SPECX_MAKE_VAR ('m', 'I4', IERR)
      CALL SPECX_MAKE_VAR ('n', 'I4', IERR)

      RETURN
      END

*-----------------------------------------------------------------------
