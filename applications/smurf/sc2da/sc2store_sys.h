/* sc2store_sys.h - data structures for sc2store library

   History :
    16Aug2004 : original (bdk)
    25Nov2005 : protect against multiple includsion and use HDSLoc for locators (timj)
*/

#ifndef SC2STORE_SYS_DEFINED
#define SC2STORE_SYS_DEFINED

/* HDS locators and pointers for per-frame header items */

static HDSLoc * sc2store_loc[SC2STORE_NUM];
static void *sc2store_ptr[SC2STORE_NUM];

#endif /* SC2STORE_SYS_DEFINED */
