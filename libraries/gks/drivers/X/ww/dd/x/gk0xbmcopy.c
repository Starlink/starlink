/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
/*
 * copy between bitmaps
 * The clipping done here is according to the given boxes.
 * As well as this the system clips according to the destination bitmap clip box.
 * Tiling should toregister by default with the top,left of the given destination
 * box. If WWREGISTER is used, align with the 0,0 of the dest bitmap.
 * Clipping should never affect these registrations.
 * If you (jdd->jd_flags&JUSETRICK) you seem to get "not a pixmap" from
 * bmxcopy's XSetTile if copying from the window.
 * We cannot use X's tiling copy all the time as it assumes the whole
 * source pixmap as source, not frombox of it.
 */
PUBLIC void gk0xbmxcopy(from,frombox,to,tobox,rop) bitmap *from, *to; box frombox, tobox; int rop;{
	jbitmap *jfrom,*jto;
	int fromx,fromy,tox,toy,fromh,fromw,height,width,toregister,realwindow;

	/* toregister = rop & WWREGISTER; */
	rop &= ~WWREGISTER;
	jto = jbm(to);
	jfrom = jbm(from);
	setgcrop(gk0xwwrasop(rop));
	setgcclip(jto->jbm_boxclip);
	fromx = frombox.b_left;
	fromy = frombox.b_top;
	fromh = HEIGHT(frombox);
	fromw = WIDTH(frombox);
	height = HEIGHT(tobox);
	width = WIDTH(tobox);
	toy = tobox.b_top;
	realwindow = !(jdd->jd_flags&JUSETRICK) && (jfrom->jbm_jwindow!=0);
	if(realwindow){
		/* source is a window: watch out for obscured source */
		jdd->jd_xgcvalues.graphics_exposures = TRUE;
		XChangeGC(WXD,WXGC,GCGraphicsExposures,&jdd->jd_xgcvalues);
	}
	if(fromw>=width && fromh>=height){
		XCopyArea(WXD,jfrom->jbm_drawable,jto->jbm_drawable,
			WXGC,fromx,fromy,width,height,tobox.b_left,tobox.b_top);
	}else if(BOXEQ(from->bm_box,frombox) && !realwindow){
		XSetTile(WXD,WXGC,jfrom->jbm_drawable);
		XSetFillStyle(WXD,WXGC,FillTiled);
		/* jdd->jd_xgcvalues.tile = jfrom->jbm_drawable; */
		XFillRectangle(WXD,jto->jbm_drawable,WXGC,
			tobox.b_left,tobox.b_top,width,height);
		XSetFillStyle(WXD,WXGC,FillSolid);
		/* cannot XSetTile(WXD,WXGC,None) */
	}else
	while(toy<=tobox.b_bottom){	/* do our own tiling */
		tox = tobox.b_left;
		height = fromh;
		if(toy+height>tobox.b_bottom+1){
			height = tobox.b_bottom-toy+1;	/* do our own clipping too! */
		}
		while(tox<=tobox.b_right){
			width = fromw;
			if(tox+width>tobox.b_right+1){
				width = tobox.b_right-tox+1;	/* do our own clipping too! */
			}
			XCopyArea(WXD,jfrom->jbm_drawable,jto->jbm_drawable,
				WXGC,fromx,fromy,width,height,tox,toy);
			tox += fromw;
		}
		toy += fromh;
	}
	if(realwindow){
		jdd->jd_xgcvalues.graphics_exposures = FALSE;
		XChangeGC(WXD,WXGC,GCGraphicsExposures,&jdd->jd_xgcvalues);
	}
	if(WIDTH(tobox)!=0)NOTESCREEN(to,tobox);
}
