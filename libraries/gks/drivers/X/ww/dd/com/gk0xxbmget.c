/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
/*
 * get and clear a rectangle of memory.
 * Align it etc. Could clear it with rasterop...
 * A zero width or height can be given legitimately, as for a window!
 */
PUBLIC bitmap *gk0xbmget(width,height) int width,height;{
	return(gk0xbmxget(width,height,dd->d_colours));
}
DDPUBLIC int gk0xcoltodepth(colours)int colours;{
	int i = 1;
	unsigned int col = colours-1;
	while(col >>= 1)i++;
	return(i);
}
#ifdef FORTINTER
FORTINTER int wbmget_(width,height)int *width,*height;{
	return((int)bmget(*width,*height));
}
#endif FORTINTER
#ifdef FORTINTER
FORTINTER void wbmfre_(bm)int *bm;{
	bmfree((bitmap *)*bm);
}
#endif FORTINTER
/*
 * return the clipping rectangle, and true if clipping was in force,
 * else false and the bitmap's box
 */
#ifdef FORTINTER
FORTINTER wbmclp_(bm,b,newbox)int *bm,*b,*newbox;{	/* bitmap *bm; box b,newbox; */
	from_box(bmclip((bitmap *)*bm,to_box(b)),newbox);
}
FORTINTER box to_box(vec)int *vec;{
	return boxbuild(vec[LEFT],vec[TOP],vec[RIGHT],vec[BOTTOM]);
}
FORTINTER from_box(b,vec)box b;int *vec;{
	vec[TOP]	= b.b_top;
	vec[BOTTOM]	= b.b_bottom;
	vec[LEFT]	= b.b_left;
	vec[RIGHT]	= b.b_right;
}
#endif FORTINTER
