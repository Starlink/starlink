/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
/*
 * draw a box: its edges, clear inclusive, clear inside, invert its inside
 */
PUBLIC void gk0xbmbox(b,flags) box b; int flags;{
	gk0xbmxbox((flags&BMWIN)?ddwin->w_bm:ddbm,b,flags&~BMWIN);
}
#ifdef FORTINTER
FORTINTER void wbmbox_(b,flags)int *b,*flags;{
	gk0xbmbox(to_box(b),*flags);
}
FORTINTER wbmxbx_(bm,b,flags)int *bm,*b,*flags;{	/* bitmap *bm; box b; int flags; */
	gk0xbmxbox((bitmap *)*bm,to_box(b),*flags);
}
#endif FORTINTER
