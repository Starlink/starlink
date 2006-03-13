/* sc2store_par.h - constants for scuba2 storage

   History :
    13Aug2004 : original (bdk)
    17Feb2005 : add SC2_HEAT (bdk)
    02Oct2005 : add SC2STORE_FLATLEN (bdk)
*/


#define SC2STORE_NUM 41      /* number of per-frame data items */
#define SC2STORE_FLATLEN 17  /* maximum length of flatfield algorithm name */
#define RTS_TASKS_LEN 80     /* length of string for rts report */
#define RTS_ERRS_LEN 80      /* length of string for rts report */
#define TCS_AZ_SYS_LEN 16    /* length of string for TCS report */
#define TCS_SOURCE_LEN 32    /* length of string for TCS report */
#define TCS_TR_SYS_LEN 16    /* length of string for TCS report */



/* Named indices into the per-frame header arrays */

#define FTS_POS 0
#define POL_ANG 1
#define RTS_NUM 2
#define RTS_STEP 3
#define RTS_END 4
#define RTS_TASKS 5
#define RTS_ERRS 6
#define SC2_HEAT 7
#define SMU_AZ_OFF_X 8
#define SMU_AZ_OFF_Y 9
#define SMU_X 10
#define SMU_Y 11
#define SMU_Z 12
#define SMU_TR_OFF_X 13
#define SMU_TR_OFF_Y 14
#define TCS_AIRMASS 15
#define TCS_AZ_SYS 16
#define TCS_AZ_ANG 17
#define TCS_AZ_AC1 18
#define TCS_AZ_AC2 19
#define TCS_AZ_DC1 20
#define TCS_AZ_DC2 21
#define TCS_AZ_BC1 22
#define TCS_AZ_BC2 23
#define TCS_INDEX 24
#define TCS_SOURCE 25
#define TCS_TR_SYS 26
#define TCS_TR_ANG 27
#define TCS_TR_AC1 28
#define TCS_TR_AC2 29
#define TCS_TR_DC1 30
#define TCS_TR_DC2 31
#define TCS_TR_BC1 32
#define TCS_TR_BC2 33
#define WVM_TH 34
#define WVM_T12 35
#define WVM_T42 36
#define WVM_T78 37
#define WVM_TW 38
#define WVM_QUAL 39
#define WVM_TIME 40

