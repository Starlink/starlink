/* sc2store_sys.h - data structures for sc2store library

   History :
    16Aug2004 : original (bdk)
    25Nov2005 : protect against multiple includsion and use HDSLoc for locators (timj)
    25Jul2006 : merge SCUBA-2 and ACSIS definitions
    11Jan2011 : Add sc2store_array to indicate whether jcmtstate is scalar or vector (timj)
*/

#ifndef SC2STORE_SYS_DEFINED
#define SC2STORE_SYS_DEFINED

/* HDS locators and pointers for per-frame header items */

static HDSLoc * sc2store_loc[JCMT_COMP_NUM];
static void *sc2store_ptr[JCMT_COMP_NUM];

/* indicate whether the JCMTSTATE entry is an array
   (1) or scalar (0) */
static int sc2store_array[JCMT_COMP_NUM];

#endif /* SC2STORE_SYS_DEFINED */
