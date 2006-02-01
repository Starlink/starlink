#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat1.h"                /* Internal dat_ definitions               */

       struct LCP *dat_ga_wlq;   /* Working Locator Queue                   */
       struct LCP *dat_ga_flq;   /* Free Locator Queue                      */
       int dat_gl_wlqsize;       /* Working Locator Queue size              */

       int hds_gl_active;        /* HDS active?                             */
       int hds_gl_ntemp;         /* Counter for temporary names             */
       int hds_gl_status;        /* Global status                           */
       int hds_gl_64bit;         /* 64-bit (V4) records                     */
       unsigned int hds_gl_locseq; /* Locator sequence number               */
       struct NDR dat_gl_ndr[ DAT__MXPRM ]; /* Native data representation   */

/* Global Tuning Parameters:                                                */
/* ========================                                                 */
       int hds_gl_inalq0;        /* Default initial file allocation quantity*/
       int hds_gl_inalq;         /* Initial file allocation quantity        */
       int hds_gl_map;           /* Use file mapping if available?          */
       int hds_gl_maxwpl;        /* Maximum size of the "working page list" */
       int hds_gl_nblocks;       /* Size of the internal "transfer buffer"  */
       int hds_gl_ncomp0;        /* Default optimum number of components    */
       int hds_gl_ncomp;         /* Optimum number of structure components  */
       int hds_gl_shell;         /* Unix shell used for file name expansion */
       int hds_gl_syslck;        /* System wide lock flag                   */
       int hds_gl_wait;          /* Wait for locked files?                  */
       int hds_gl_c64bit;        /* Create new files in 64-bit format       */

       const struct RID rec_gl_ridzero = { 0, 0 }; /* Null record ID        */
       int rec_gl_active;        /* rec_ facility active?                   */
       int rec_gl_endslot;       /* Next FCV slot # to use                  */
       int rec_gl_mxslot;        /* Number of FCV slots allocated           */
       int rec_gl_wplsize;       /* Current Working Page List size          */
       struct BCP *rec_ga_fpl;   /* Free Page List                          */
       struct BCP *rec_ga_lastbcp; /* Address of last used BCP              */
       struct BCP *rec_ga_wpl;   /* Working Page List                       */
       struct FCV *rec_ga_fcv = NULL; /* File control vector                */
       struct WLD *rec_gl_wldque = NULL; /* Wild card search context queue  */

       /* Memory malloced for start of Free Page List */
       struct BCP *rec_ga_fpl_malloced = NULL;
       /* Memory malloced for start of free locator queue */
       struct LCP *dat_ga_flq_malloced = NULL;
