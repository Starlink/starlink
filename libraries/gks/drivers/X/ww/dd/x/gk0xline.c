/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
PUBLIC void gk0xbmdraw(xa,ya,xb,yb,bm,rop)int xa,ya,xb,yb,rop;bitmap *bm;{
	box b;

	gk0xsetbmrop(gk0xwwlineop(rop),bm);
	setgcclip(jbm(bm)->jbm_boxclip);
	XDrawLine(WXD,jbm(bm)->jbm_drawable,WXGC,xa,ya,xb,yb);
	/* must preserve left<right etc for notescreen */
	if(xb>xa)b.b_left = xa, b.b_right = xb;
	else	b.b_left = xb, b.b_right = xa;
	if(yb>ya)b.b_top = ya, b.b_bottom = yb;
	else	b.b_top = yb, b.b_bottom = ya;
	NOTESCREEN(bm,b);	/* flush perhaps */
}
