/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
/*
 * copy between bitmaps. box is in default bitmap ddbm.
 */
PUBLIC bitmap *gk0xbmcopy(bm,b,flags) bitmap *bm; box b; int flags;{
	bitmap *from,*to;
	box otherbox;

	if(flags&BMGET)flags |= BMTO;
	/* no else */ if(flags&BMFREE)flags |= BMFROM;
	if( ((flags&BMTO) && (flags&BMFROM)) ||
	    !(flags&(BMTO|BMFROM)) ||
	    (flags&~(BMGET|BMFREE|BMTO|BMFROM|BMWIN)) )
		wwfail("invalid flags to bmcopy",NULLPTR(bitmap));
	otherbox = gk0xboxbuild(0,0,b.b_right-b.b_left,b.b_bottom-b.b_top);
	if(flags&BMTO){
		from = (flags&BMWIN)?ddwin->w_bm:ddbm;
		if(flags&BMGET)
			bm = gk0xbmget(WIDTH(b),HEIGHT(b));
		gk0xbmxcopy(from,b,bm,otherbox,dd->d_rop);
	}else{
		to = (flags&BMWIN)?ddwin->w_bm:ddbm;
		gk0xbmxcopy(bm,otherbox,to,b,dd->d_rop);
		if(flags&BMFREE){
			gk0xbmfree(bm);
			bm = NULLPTR(bitmap);
		}
	}
	return(bm);
}
#ifdef FORTINTER
FORTINTER wbmcpy_(bm,b,flags)int *bm,*b,*flags;{
	return(int)bmcopy((bitmap *)*bm,to_box(b),*flags);
}
FORTINTER wbmxcp_(fromap,frombx,tomap,tobox,rop)int *fromap,*frombx,*tomap,*tobox,*rop;
/* bitmap *fromap,*tomap; box frombx,tobox; int rop; */
{
	bmxcopy((bitmap *)*fromap,to_box(frombx),(bitmap *)*tomap,to_box(tobox),*rop);
}
#endif FORTINTER
/*
 * move a box (inclusive) to a new top left
 */
PUBLIC void gk0xbmmove(b,x,y) box b; int x,y;{
	gk0xbmxcopy(ddbm,b,ddbm,gk0xboxshift(b,x-b.b_left,y-b.b_top),dd->d_rop);
}
#ifdef FORTINTER
FORTINTER void wbmmov_(b,x,y)int *b;int *x,*y;{
	box temp;

	temp = to_box(b);
	bmmove(temp,*x,*y);
	from_box(temp,b);
}
#endif FORTINTER
