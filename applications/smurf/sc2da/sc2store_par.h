/* sc2store_par.h - constants for scuba2 storage

   History :
    13Aug2004 : original (bdk)
    17Feb2005 : add SC2_HEAT (bdk)
    02Oct2005 : add SC2STORE_FLATLEN (bdk)
    02Feb2006 : use an enum for the indices (bdk)
    09Feb2006 : make SC2STORE_NUM last item in the enum (bdk)
*/
#ifndef SC2STORE_PAR_DEFINED
#define SC2STORE_PAR_DEFINED


#define SC2STORE_FLATLEN 17  /* maximum length of flatfield algorithm name */
#define RTS_TASKS_LEN 80     /* length of string for rts report */
#define RTS_ERRS_LEN 80      /* length of string for rts report */
#define TCS_AZ_SYS_LEN 16    /* length of string for TCS report */
#define TCS_SOURCE_LEN 32    /* length of string for TCS report */
#define TCS_TR_SYS_LEN 16    /* length of string for TCS report */

#define SC2STORE__MAXFITS 256 /* Maximum number of FITS records */


/* Named indices into the per-frame header arrays */

typedef enum
{
   FTS_POS,
   POL_ANG,
   RTS_NUM,
   RTS_STEP,
   RTS_END,
   RTS_TASKS,
   RTS_ERRS,
   SC2_HEAT,
   SMU_AZ_OFF_X,
   SMU_AZ_OFF_Y,
   SMU_X,
   SMU_Y,
   SMU_Z,
   SMU_TR_OFF_X,
   SMU_TR_OFF_Y,
   TCS_AIRMASS,
   TCS_AZ_SYS,
   TCS_AZ_ANG,
   TCS_AZ_AC1,
   TCS_AZ_AC2,
   TCS_AZ_DC1,
   TCS_AZ_DC2,
   TCS_AZ_BC1,
   TCS_AZ_BC2,
   TCS_INDEX,
   TCS_SOURCE,
   TCS_TR_SYS,
   TCS_TR_ANG,
   TCS_TR_AC1,
   TCS_TR_AC2,
   TCS_TR_DC1,
   TCS_TR_DC2,
   TCS_TR_BC1,
   TCS_TR_BC2,
   WVM_TH,
   WVM_T12,
   WVM_T42,
   WVM_T78,
   WVM_TW,
   WVM_QUAL,
   WVM_TIME,
   SC2STORE_NUM
} SC2STORE_HEADLIST;

#endif /* SC2STORE_PAR_DEFINED */
