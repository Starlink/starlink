/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
#define freeclip(jbp)	/* jbitmap *jbp;{} */
box gk0xnoclipbox = {0,0,WWNOCLIP,WWNOCLIP};
/*
 * return a bitmap all set except for the rectangle of space
 * Caller sets jbm()->jbm_memory or jbm_window
 * Hence allows use for windows or real bitmaps
 */
DDPUBLIC bitmap *gk0xwwbmget(width,height) int width,height;{
	bitmap *bm;
	jbitmap *jbp;

	bm = STRUCTMALLOC(bitmap);
	jbp = STRUCTMALLOC(jbitmap);
	if(bm == NULLPTR(bitmap) || jbp == NULLPTR(jbitmap))
		wwfail("(ww)bmget couldnt get space",NULLPTR(bitmap));
	bm->bm_junk = (int *)jbp;
	bm->bm_box = gk0xboxbuild(0,0,width-1,height-1);
	jbp->jbm_boxclip = gk0xnoclipbox;	/* must 0,0 top left for wc/sun */
	jbp->jbm_memory = 0;
	return(bm);
}
/*
 * bitmaps for user tend to have to be same depth as screen.
 * Ignore requests for depth 1 etc. (eg from bmget)
 */
PUBLIC bitmap *gk0xbmxget(width,height,colours) int width,height,colours;{
	bitmap *bm;
	bm = gk0xbmreallyget(width,height,1<<jdd->jd_depth);
	bm->bm_colours = colours;	/* lying */
	return(bm);
}
DDPUBLIC bitmap *gk0xbmreallyget(width,height,colours) int width,height,colours;{
	bitmap *bm;
	jbitmap *jbp;

	if(width<=0 || height<=0 || colours<2)wwfail("bmget with width or height <=0 or colours<2",NULLPTR(bitmap));
	bm = gk0xwwbmget(width,height);
	/* bm->bm_window = 0; */
	jbp = jbm(bm);
	jbp->jbm_memory = jbp->jbm_drawable =
		XCreatePixmap(WXD,jdd->jd_root,width,height,gk0xcoltodepth(colours));
	if(jbp->jbm_memory == 0)
		wwfail("bmget couldnt get space",NULLPTR(bitmap));
	/* must now clear the pixmap: XFillRectangle */
	bm->bm_colours = colours;
	return(bm);
}
/*
 * free a bitmap for the caller
 */
PUBLIC void gk0xbmfree(bm) bitmap *bm;{
	jbitmap *jbp;

	if(bm==NULLPTR(bitmap))return;
	if(bm->bm_window!=0)gk0xwwpanic("freeing bitmap for window");
	jbp = jbm(bm);
	freeclip(jbp);
	if(jbp->jbm_memory != 0)XFreePixmap(WXD,jbp->jbm_memory);
	free((char *)jbp);
	free((char *)bm);
}
DDPUBLIC void setgcclip(b)box b;{
	XRectangle rect;
	static box clipbox;
	if(!BOXEQ(b,clipbox)){
		if(BOXEQ(b,gk0xnoclipbox))
/*
 * XSetClipRectangles(WXD,WXGC,0,0/*origin,(XRectangle*)0,0/*num rects,Unsorted);
 * seems to set everything gets clipped!
 */
			XSetClipMask(WXD,WXGC,None);
		else{
			rect.x = b.b_left, rect.y = b.b_top;
			rect.width = WIDTH(b);
			rect.height = HEIGHT(b);
			XSetClipRectangles(WXD,WXGC,0,0/*origin*/,&rect,1/*num rects*/,Unsorted);
		}
		clipbox = b;
	}
}
