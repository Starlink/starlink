
/* ----------------------------------------------------------------- */
/*            Include file iii.h                                     */
/* ----------------------------------------------------------------- */

#if defined(VMS)
#define NOSHARE extern noshare
#else
#define NOSHARE extern
#endif

NOSHARE int                  sli_ind, sli_r, sli_g, sli_b;
NOSHARE int                  user;
NOSHARE int                  user_reset[MAX_DEV];
NOSHARE int                  inter_active;
NOSHARE int                  LOC_X[MAX_LOC], LOC_Y[MAX_LOC];
NOSHARE int                  int_scroll_reset[MAX_DEV];
NOSHARE int                  roi_last_active;
NOSHARE int                  roi_reset;
NOSHARE unsigned long        inter_mask[MAX_DEV];

