#include <stdio.h>
#include "nbs_typ.h"

/* External definitions   */

int nbs_gl_defining; /* Currently defining noticeboard contents? */
item_id nbs_ga_base; /* Pointer to base of noticeboard currently being
                               defined */

int nbs_ga_alloc_next;
int nbs_ga_alloc_base;
int nbs_ga_alloc_last;
int nbs_ga_alloc_data;

int nbs_gl_item_total;	/* Current total size of Item_descriptor's */
int nbs_gl_fixed_total;	/* Current total size of Fixed_info's */
int nbs_gl_shape_total;	/* Current total size of shape information */
int nbs_gl_boardinfo_total; /* Current total size of Board_info's */
int nbs_gl_data_total;	/* Current total size of primitive data */

int nbs_gl_pid;		/* PID of current process */






