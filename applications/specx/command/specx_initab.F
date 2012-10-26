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
*     22 Aug 2005 (timj)
*        %LOC is not always available
*-----------------------------------------------------------------------

#if HAVE_CONFIG_H
# include "config.h"
#endif

* Some compilers preprocess include files. This leads to problems with
* VERSION so if we have VERSION defined, undefine it
#ifdef VERSION
# undef VERSION
#endif

      SUBROUTINE SPECX_INIT_TABLE

      IMPLICIT NONE
      INCLUDE 'CNF_PAR'

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
      INTEGER   LENGTH_ADDRESS
      LOGICAL   ISNEW

*     Functions:

      INTEGER   IGETVM

*  Ok, go...

*     Get virtual memory for variable symbol table

      ISTAT = IGETVM (LSTMAX, .FALSE., 'SPECX_INIT_TABLE',
     &                TABLE_ADDRESS)

*     Define the location of the table for GEN routines
*     Note that we want direct access to the number of entries, so keep
*       the ADDRESS of the variable containing the # entries inside GENLIB.
#if HAVE_PERCENTLOC
      LENGTH_ADDRESS = CNF_PREG( %LOC(NO_ENTRIES), ISNEW )
#elif HAVE_INTRINSIC_LOC
      LENGTH_ADDRESS = CNF_PREG( LOC(NO_ENTRIES), ISNEW )
#else
    Do not have LOC or %LOC
#endif

      CALL GEN_SET_SYMT (TABLE_ADDRESS, LENGTH_ADDRESS)

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

      INCLUDE 'CNF_PAR'
      INCLUDE 'STACKCOMM'

*     Local variables

      INTEGER*4 IERR

      LOGICAL ISNEW
      LOGICAL*4 TRUE
      LOGICAL*4 FALSE
      DATA TRUE / .TRUE. /
      DATA FALSE / .FALSE. /
      SAVE      TRUE, FALSE

      CALL GEN_MAKESYMB ('*T',      'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(TRUE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(TRUE),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*F',      'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(FALSE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(FALSE),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*TRUE',   'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(TRUE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(TRUE),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*FALSE',  'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(FALSE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(FALSE),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*.TRUE.', 'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(TRUE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(TRUE),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*.FALSE.','L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(FALSE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(FALSE),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      CALL GEN_MAKESYMB ('TSYS', 'R4', 8,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(TSYS),
#elif HAVE_INTRINSIC_LOC
     :   LOC(TSYS),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      CALL GEN_MAKESYMB ('LO_FREQ',   'R8', 8,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(LOFREQ),
#elif HAVE_INTRINSIC_LOC
     :   LOC(LOFREQ),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('IF_FREQ',   'R8', 8,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IFFREQ),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IFFREQ),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('VSL',       'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(VSL),
#elif HAVE_INTRINSIC_LOC
     :   LOC(VSL),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('VES',       'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(VES),
#elif HAVE_INTRINSIC_LOC
     :   LOC(VES),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('VTE',       'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(VTE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(VTE),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('VLSR',      'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(VLSR),
#elif HAVE_INTRINSIC_LOC
     :   LOC(VLSR),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('AZIMUTH',   'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(AZ),
#elif HAVE_INTRINSIC_LOC
     :   LOC(AZ),
#else
    Do not have LOC or %LOC
#endif
     :         ISNEW ), IERR)
      CALL GEN_MAKESYMB ('ELEVATION', 'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(EL),
#elif HAVE_INTRINSIC_LOC
     :   LOC(EL),
#else
    Do not have LOC or %LOC
#endif
     :         ISNEW ), IERR)

      CALL GEN_MAKESYMB ('F_REST', 'I4', 8,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(JFREST),
#elif HAVE_INTRINSIC_LOC
     :   LOC(JFREST),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('F_CEN',  'I4', 8,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(JFCEN),
#elif HAVE_INTRINSIC_LOC
     :   LOC(JFCEN),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('F_INC',  'I4', 8,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(JFINC),
#elif HAVE_INTRINSIC_LOC
     :   LOC(JFINC),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)

      CALL GEN_MAKESYMB ('INT_TIME', 'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(INTT),
#elif HAVE_INTRINSIC_LOC
     :   LOC(INTT),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      CALL GEN_MAKESYMB ('T_REC', 'I4', 8,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ITREC),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ITREC),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('T_SKY', 'I4', 8,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ITSKY),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ITSKY),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('T_TEL', 'I4', 8,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ITTEL),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ITTEL),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      CALL GEN_MAKESYMB ('NPTS', 'I4', 8,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NPTS),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NPTS),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      CALL GEN_MAKESYMB ('RA',  'R8', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(RA),
#elif HAVE_INTRINSIC_LOC
     :   LOC(RA),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('DEC', 'R8', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(DEC),
#elif HAVE_INTRINSIC_LOC
     :   LOC(DEC),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      CALL GEN_MAKESYMB ('SCAN_NO', 'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(LSCAN),
#elif HAVE_INTRINSIC_LOC
     :   LOC(LSCAN),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('MODE', 'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IMODE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IMODE),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*NQUAD', 'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NQUAD),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NQUAD),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*BLOCK_1', 'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IST),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IST),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*BLOCK_2', 'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IEND),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IEND),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('CAL_ZD', 'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ICALZD),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ICALZD),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('LSR_FLAG', 'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(LSRFLG),
#elif HAVE_INTRINSIC_LOC
     :   LOC(LSRFLG),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('CENTRE_QUAD', 'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IQCEN),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IQCEN),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('RA_OFFSET',  'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(DRA),
#elif HAVE_INTRINSIC_LOC
     :   LOC(DRA),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('DEC_OFFSET', 'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(DDEC),
#elif HAVE_INTRINSIC_LOC
     :   LOC(DDEC),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      CALL GEN_MAKESYMB ('SCAN_TITLE', 'C26', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ITITLE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ITITLE),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('SOURCE_NAME','C9',1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ITITLE(13:13)),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ITITLE(13:13)),
#else
    Do not have LOC or %LOC
#endif
     :   ISNEW ), IERR)
      CALL GEN_MAKESYMB ('SUBSCAN_NO','C4',1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ITITLE(6:6)),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ITITLE(6:6)),
#else
    Do not have LOC or %LOC
#endif
     :   ISNEW ), IERR)

      CALL GEN_MAKESYMB ('SCAN_DATE', 'C9', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IDATE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IDATE),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('UT_FLAG', 'L1', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IUTFLG),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IUTFLG),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('SCAN_TIME', 'C8', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ITIME),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ITIME),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      CALL GEN_MAKESYMB ('DATA', 'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(DATA),
#elif HAVE_INTRINSIC_LOC
     :   LOC(DATA),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE DEFINE_FLAG_TABLE

      IMPLICIT NONE

*     Include flags common block

      INCLUDE 'CNF_PAR'
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

      LOGICAL ISNEW
      INTEGER*4 IERR

*     FLAGCOMM items:

      CALL GEN_MAKESYMB ('IN_FILE',     'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(INFILE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(INFILE),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('OUT_FILE',    'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(OUTFILE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(OUTFILE),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      CALL GEN_MAKESYMB ('N_INTS',      'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NPR),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NPR),
#else
    Do not have LOC or %LOC
#endif
     :         ISNEW ), IERR)
      CALL GEN_MAKESYMB ('BLF_INTS',    'I4', 16,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IBLP),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IBLP),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('N_FSSP',      'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NFSSP),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NFSSP),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('FSS_INTS',    'I4', 10,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IFSSP),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IFSSP),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('RLB_INTS',    'I4', 4,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IBLRPT),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IBLRPT),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('SECTOR_MASK', 'I4', 8,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(MASK),
#elif HAVE_INTRINSIC_LOC
     :   LOC(MASK),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*LINK',       'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(LINK),
#elif HAVE_INTRINSIC_LOC
     :   LOC(LINK),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('MERGE_OFFSET','L4',1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(SECTOR_OFFSET),
#elif HAVE_INTRINSIC_LOC
     :   LOC(SECTOR_OFFSET),
#else
    Do not have LOC or %LOC
#endif
     :    ISNEW ), IERR)

      CALL GEN_MAKESYMB ('NCONT',       'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NCONT),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NCONT),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('CONTOUR_0',   'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(CONT0),
#elif HAVE_INTRINSIC_LOC
     :   LOC(CONT0),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('CONTOUR_INT', 'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(CONTI),
#elif HAVE_INTRINSIC_LOC
     :   LOC(CONTI),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('NCSET',       'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NCSET),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NCSET),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('CONTOUR_LEVS','R4', 16,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(CLEVELS),
#elif HAVE_INTRINSIC_LOC
     :   LOC(CLEVELS),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('LINTYP_NEG',  'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(LTNEG),
#elif HAVE_INTRINSIC_LOC
     :   LOC(LTNEG),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('LINTYP_POS',  'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(LTPOS),
#elif HAVE_INTRINSIC_LOC
     :   LOC(LTPOS),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('LINTYP_ZERO', 'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(LTZ),
#elif HAVE_INTRINSIC_LOC
     :   LOC(LTZ),
#else
    Do not have LOC or %LOC
#endif
     :         ISNEW ), IERR)

      CALL GEN_MAKESYMB ('PLOT_CONT',   'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(PLOTCONT),
#elif HAVE_INTRINSIC_LOC
     :   LOC(PLOTCONT),
#else
    Do not have LOC or %LOC
#endif
     :    ISNEW ), IERR)
      CALL GEN_MAKESYMB ('PLOT_GREY',   'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(PLOTGREY),
#elif HAVE_INTRINSIC_LOC
     :   LOC(PLOTGREY),
#else
    Do not have LOC or %LOC
#endif
     :    ISNEW ), IERR)
      CALL GEN_MAKESYMB ('AUTOGREY',    'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(AUTOGREY),
#elif HAVE_INTRINSIC_LOC
     :   LOC(AUTOGREY),
#else
    Do not have LOC or %LOC
#endif
     :    ISNEW ), IERR)
      CALL GEN_MAKESYMB ('GREYLIM',     'R4', 2,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(GREYLIM),
#elif HAVE_INTRINSIC_LOC
     :   LOC(GREYLIM),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('MAPLIMITS',   'R4', 2,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(MAPLIMITS),
#elif HAVE_INTRINSIC_LOC
     :   LOC(MAPLIMITS),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('COLOUR_TABLE','I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(COLOUR_TABLE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(COLOUR_TABLE),
#else
    Do not have LOC or %LOC
#endif
     :    ISNEW ), IERR)
      CALL GEN_MAKESYMB ('OVERLAY_CONTOURS','L4',1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(OVERCONT),
#elif HAVE_INTRINSIC_LOC
     :   LOC(OVERCONT),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      CALL GEN_MAKESYMB ('MULT_FACT',   'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(FACT),
#elif HAVE_INTRINSIC_LOC
     :   LOC(FACT),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('DIV_FACT',    'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(DIV),
#elif HAVE_INTRINSIC_LOC
     :   LOC(DIV),
#else
    Do not have LOC or %LOC
#endif
     :         ISNEW ), IERR)
      CALL GEN_MAKESYMB ('CHAN_SHIFT',  'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ACHAN),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ACHAN),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('OFFSET',      'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(OFF),
#elif HAVE_INTRINSIC_LOC
     :   LOC(OFF),
#else
    Do not have LOC or %LOC
#endif
     :         ISNEW ), IERR)

      CALL GEN_MAKESYMB ('BEAM_FWHM',   'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(FWHM),
#elif HAVE_INTRINSIC_LOC
     :   LOC(FWHM),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('BEAM_EXTENT', 'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(WMAX),
#elif HAVE_INTRINSIC_LOC
     :   LOC(WMAX),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('VEL_FWHM',    'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(VWIDHM),
#elif HAVE_INTRINSIC_LOC
     :   LOC(VWIDHM),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('VEL_EXTENT',  'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(VFNMAX),
#elif HAVE_INTRINSIC_LOC
     :   LOC(VFNMAX),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('MAP_TOL',     'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(MAP_TOL),
#elif HAVE_INTRINSIC_LOC
     :   LOC(MAP_TOL),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('REPLACE',     'L4', 1,
     &
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(REPLACE_MAP_DATA),
#elif HAVE_INTRINSIC_LOC
     :   LOC(REPLACE_MAP_DATA),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      CALL GEN_MAKESYMB ('VELOCITY_FRAME','C4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(VEL_REF),
#elif HAVE_INTRINSIC_LOC
     :   LOC(VEL_REF),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('VELOCITY_LAW',  'C3', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(VEL_DEF),
#elif HAVE_INTRINSIC_LOC
     :   LOC(VEL_DEF),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('VELOCITY',      'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(VELOUT),
#elif HAVE_INTRINSIC_LOC
     :   LOC(VELOUT),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)

      CALL GEN_MAKESYMB ('X_UNITS',  'C6',  1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(XAXIS_UNITS),
#elif HAVE_INTRINSIC_LOC
     :   LOC(XAXIS_UNITS),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('X_NAME',   'C10', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(XAXIS_NAME),
#elif HAVE_INTRINSIC_LOC
     :   LOC(XAXIS_NAME),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('Y_UNITS',  'C16', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(YAXIS_UNITS),
#elif HAVE_INTRINSIC_LOC
     :   LOC(YAXIS_UNITS),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('Y_NAME',   'C16', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(YAXIS_NAME),
#elif HAVE_INTRINSIC_LOC
     :   LOC(YAXIS_NAME),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)

      CALL GEN_MAKESYMB ('HISTOGRAM',    'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(HISTOGRAM),
#elif HAVE_INTRINSIC_LOC
     :   LOC(HISTOGRAM),
#else
    Do not have LOC or %LOC
#endif
     :    ISNEW ), IERR)
      CALL GEN_MAKESYMB ('LINE_WEIGHT',  'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(LWEIGHT),
#elif HAVE_INTRINSIC_LOC
     :   LOC(LWEIGHT),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('LINE_TYPE',    'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IPEN),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IPEN),
#else
    Do not have LOC or %LOC
#endif
     :         ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*DEVICE',      'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IDEV),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IDEV),
#else
    Do not have LOC or %LOC
#endif
     :         ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*TERMINAL',    'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(TERMINAL),
#elif HAVE_INTRINSIC_LOC
     :   LOC(TERMINAL),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('AUTO_CONT',    'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ICAUTO),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ICAUTO),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('SCALES_SET',   'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ISETSC),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ISETSC),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('INTERACTIVE',  'L4',1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(INTERACTIVE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(INTERACTIVE),
#else
    Do not have LOC or %LOC
#endif
     :    ISNEW ), IERR)
      CALL GEN_MAKESYMB ('BADPIX_VALUE', 'R4',1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(BADPIX_VAL),
#elif HAVE_INTRINSIC_LOC
     :   LOC(BADPIX_VAL),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

      CALL GEN_MAKESYMB ('COLOR5_START', 'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(C5START),
#elif HAVE_INTRINSIC_LOC
     :   LOC(C5START),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('COLOR5_ROTATE','R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(C5ROTAT),
#elif HAVE_INTRINSIC_LOC
     :   LOC(C5ROTAT),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('COLOR5_EXPONENT','R4',1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(C5EXP),
#elif HAVE_INTRINSIC_LOC
     :   LOC(C5EXP),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)

*     Telescope/astrometric data

      CALL GEN_MAKESYMB ('LATITUDE',     'R8', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ALAT),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ALAT),
#else
    Do not have LOC or %LOC
#endif
     :          ISNEW ), IERR)
      CALL GEN_MAKESYMB ('LONGITUDE',    'R8', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ALONG),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ALONG),
#else
    Do not have LOC or %LOC
#endif
     :         ISNEW ), IERR)

*     Data from reduction operations

      CALL GEN_MAKESYMB ('TMAX',       'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(TMAX),
#elif HAVE_INTRINSIC_LOC
     :   LOC(TMAX),
#else
    Do not have LOC or %LOC
#endif
     :          ISNEW ), IERR)
      CALL GEN_MAKESYMB ('VMAX',       'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(VMAX),
#elif HAVE_INTRINSIC_LOC
     :   LOC(VMAX),
#else
    Do not have LOC or %LOC
#endif
     :          ISNEW ), IERR)
      CALL GEN_MAKESYMB ('TOTINT',     'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(TOTINT),
#elif HAVE_INTRINSIC_LOC
     :   LOC(TOTINT),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('FSS_AV',     'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(FSS_AV),
#elif HAVE_INTRINSIC_LOC
     :   LOC(FSS_AV),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('FSS_VAR',    'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(FSS_VAR),
#elif HAVE_INTRINSIC_LOC
     :   LOC(FSS_VAR),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('FSS_SD',     'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(FSS_SD),
#elif HAVE_INTRINSIC_LOC
     :   LOC(FSS_SD),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('CENTROID',   'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(XCHAN),
#elif HAVE_INTRINSIC_LOC
     :   LOC(XCHAN),
#else
    Do not have LOC or %LOC
#endif
     :         ISNEW ), IERR)

*     X-axis frequency corrections

      CALL GEN_MAKESYMB ('FCORRECT',   'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(FCORRECT),
#elif HAVE_INTRINSIC_LOC
     :   LOC(FCORRECT),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('FRQCOEFF',   'R4', 6,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(FRQCOEFF),
#elif HAVE_INTRINSIC_LOC
     :   LOC(FRQCOEFF),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)

*     Data from cube?

      CALL GEN_MAKESYMB ('*CUBE_LOADED', 'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(CUBE_IN_MEMORY),
#elif HAVE_INTRINSIC_LOC
     :   LOC(CUBE_IN_MEMORY),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
*     CALL GEN_MAKESYMB ('*CUBE_MODIFIED', 'L4', 1,
*     :   CNF_PREG(
#if HAVE_PERCENTLOC
*     :   %LOC(NEW_CUBE_LOADED),
#elif HAVE_INTRINSIC_LOC
*     :   LOC(NEW_CUBE_LOADED),
#else
    Do not have LOC or %LOC
#endif
*     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*CUBE_ROTATED', 'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(MAP_ROTATED),
#elif HAVE_INTRINSIC_LOC
     :   LOC(MAP_ROTATED),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*CUBE_INTERPOLATED', 'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(MAP_INTERPOLATED),
#elif HAVE_INTRINSIC_LOC
     :   LOC(MAP_INTERPOLATED),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*CUBE_SIZE',   'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NCUBE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NCUBE),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

*     Control of sin/polynomical and gaussian fits

      CALL GEN_MAKESYMB ('FIT_TOL',     'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(TOL),
#elif HAVE_INTRINSIC_LOC
     :   LOC(TOL),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)
      CALL GEN_MAKESYMB ('MAX_ITS',     'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(MAXITS),
#elif HAVE_INTRINSIC_LOC
     :   LOC(MAXITS),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('FIT_DEBUG',   'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(IER),
#elif HAVE_INTRINSIC_LOC
     :   LOC(IER),
#else
    Do not have LOC or %LOC
#endif
     :        ISNEW ), IERR)

*     Data from sin/polynomial and gaussian fits

      CALL GEN_MAKESYMB ('N_GAUSS',      'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NGOLD),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NGOLD),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('AMP_WID_POS',  'R4', 30,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(AWP),
#elif HAVE_INTRINSIC_LOC
     :   LOC(AWP),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('N_POLY',       'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NPOLY),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NPOLY),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('N_SIN',        'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NSIN),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NSIN),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('AMP_PHASE',    'R4', 28,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(APHI),
#elif HAVE_INTRINSIC_LOC
     :   LOC(APHI),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('FREQ_COEFF','R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(FREQ_COEFF),
#elif HAVE_INTRINSIC_LOC
     :   LOC(FREQ_COEFF),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('POLY_COEFF','R4', 30,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(POLY_COEFF),
#elif HAVE_INTRINSIC_LOC
     :   LOC(POLY_COEFF),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

*     Data from header of currently opened map

      CALL GEN_MAKESYMB ('MAP_OPEN',    'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(MAP_OPEN),
#elif HAVE_INTRINSIC_LOC
     :   LOC(MAP_OPEN),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)
      CALL GEN_MAKESYMB ('MAP_NAME',    'C40',1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ID),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ID),
#else
    Do not have LOC or %LOC
#endif
     :             ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*NO_MAP_PTS', 'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NSPEC),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NSPEC),
#else
    Do not have LOC or %LOC
#endif
     :          ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*POSN_ANGLE', 'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(POS_ANGLE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(POS_ANGLE),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*NO_XPIX',    'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(MSTEP),
#elif HAVE_INTRINSIC_LOC
     :   LOC(MSTEP),
#else
    Do not have LOC or %LOC
#endif
     :          ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*NO_YPIX',    'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NSTEP),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NSTEP),
#else
    Do not have LOC or %LOC
#endif
     :          ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*XSIZ_CELL',  'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(CELL_XSIZE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(CELL_XSIZE),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*YSIZ_CELL',  'R4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(CELL_YSIZE),
#elif HAVE_INTRINSIC_LOC
     :   LOC(CELL_YSIZE),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)

*     Data from GSD index and INDEX-FILE

      CALL GEN_MAKESYMB ('GSD_FILENAME', 'C32', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(GSDNAME),
#elif HAVE_INTRINSIC_LOC
     :   LOC(GSDNAME),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*NO_FILE_SPECTRA','I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NSCAN),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NSCAN),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*NO_GSD_SPECTRA', 'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NNSPEC),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NNSPEC),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('*NO_NEW_SPECTRA','I4',1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(NO_NEWSPEC),
#elif HAVE_INTRINSIC_LOC
     :   LOC(NO_NEWSPEC),
#else
    Do not have LOC or %LOC
#endif
     :    ISNEW ), IERR)

*     Error parameters

      CALL GEN_MAKESYMB ('ERROR_SET',   'L4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ERROR_SET),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ERROR_SET),
#else
    Do not have LOC or %LOC
#endif
     :      ISNEW ), IERR)
      CALL GEN_MAKESYMB ('ERROR',       'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(ERROR),
#elif HAVE_INTRINSIC_LOC
     :   LOC(ERROR),
#else
    Do not have LOC or %LOC
#endif
     :          ISNEW ), IERR)
      CALL GEN_MAKESYMB ('LAST_ERROR',  'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(LAST_ERROR),
#elif HAVE_INTRINSIC_LOC
     :   LOC(LAST_ERROR),
#else
    Do not have LOC or %LOC
#endif
     :     ISNEW ), IERR)
      CALL GEN_MAKESYMB ('SEVERITY',    'I4', 1,
     :   CNF_PREG(
#if HAVE_PERCENTLOC
     :   %LOC(SEVERITY),
#elif HAVE_INTRINSIC_LOC
     :   LOC(SEVERITY),
#else
    Do not have LOC or %LOC
#endif
     :       ISNEW ), IERR)

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
