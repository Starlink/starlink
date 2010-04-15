/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
/*
 * a 13 x 22 busy hourglass that can be added to any pattern by custack(,WWPUSHBUSY)
xxxxxxxxxxxxx
x           x
x           x
x           x
 xx       xx
 xxxx   xxxx
  xxxxxxxxx
   xxxxxxx
    xxxxx
     xxx
      x
      x
     xxx
    x x x
   x  x  x
  x   x   x
 x    x    x
 x    x    x
x    xxx    x
x  xxxxxxx  x
xxxxxxxxxxxxx
xxxxxxxxxxxxx
 */
PRIVATE char busychars[] =
{ /* lines= */ 0,22, /* width= */ 0,13,
0377,0370,0200,010,
0200,010,0200,010,
0140,060,0170,0360,
077,0340,037,0300,
017,0200,07,0,
02,0,02,0,
07,0,012,0200,
022,0100,042,040,
0102,020,0102,020,
0207,010,0237,0310,
0377,0370,0377,0370}; /* ww style */
DDPUBLIC void gk0xaddbusy(wp)window *wp;{
	int xoff,yoff;
	static bitmap *busybm = NULLPTR(bitmap);
	cursor *cc;

	cc = &jwin(wp)->jw_jcursor.jc_cursor;
	if(busybm==0)
		busybm = gk0xbmdecode(busychars,ENWWSTYLE);
	xoff = cc->c_bm->bm_box.b_right+2;
	yoff = cc->c_bm->bm_box.b_bottom+2;
	/* offset to top corner of busy pic. just missing user's pic */
	if(xoff+busybm->bm_box.b_right+1>MAXCUWIDTH)
		xoff = MAXCUWIDTH-(busybm->bm_box.b_right+1);
	if(yoff+busybm->bm_box.b_bottom+1>MAXCUHEIGHT)
		yoff = MAXCUHEIGHT-(busybm->bm_box.b_bottom+1);
	gk0xbmxcopy(busybm,busybm->bm_box,
	  cc->c_bm,
	  gk0xboxshift(busybm->bm_box,xoff,yoff),
	  WWXOR);
}
/*
 * show cursor that we've stored
 */
DDPUBLIC void gk0xcudisplay(wp)window *wp;{
	static Cursor xc = 0;
	XColor fcolour,bcolour;
	cursor *cp;
	jwindow *jwp;
	int xhot,yhot;

	jwp = jwin(wp);
	cp = &jwp->jw_jcursor.jc_cursor;
	fcolour.pixel = jdd->jd_xgcvalues.foreground;
	XQueryColor(WXD,DefaultColormap(WXD,WXS),&fcolour);
	bcolour.pixel = jdd->jd_xgcvalues.background;
	XQueryColor(WXD,DefaultColormap(WXD,WXS),&bcolour);
	/* b/fcolour.pad = 0; ???? */
	if(xc!=0)XFreeCursor(WXD,xc);
	xhot = cp->c_xhot-cp->c_xoffset;
	yhot = cp->c_yhot-cp->c_yoffset;
	if(xhot<0)xhot = 0;
	else if(xhot>cp->c_bm->bm_box.b_right)xhot = cp->c_bm->bm_box.b_right;
	if(yhot<0)yhot = 0;
	else if(yhot>cp->c_bm->bm_box.b_bottom)yhot = cp->c_bm->bm_box.b_bottom;
	/* X says: source and mask cursors must be 1 plane
	 * cannot: if(cp->c_bm->bm_colours==MONOCHROME)
	 * because we cheat on bm_colours for user wanting mono */
	if(jdd->jd_depth==1){
		xc = XCreatePixmapCursor(WXD,jbm(cp->c_bm)->jbm_memory,
			cp->c_mask?jbm(cp->c_mask)->jbm_memory:jbm(cp->c_bm)->jbm_memory/*mask*/,
			&fcolour,&bcolour,xhot,yhot);
		if(jwp->jw_flags&JWSCREEN){
			setscreencursor(wp,xc);
			xc = 0;	/* dont free */
		}else XDefineCursor(WXD,jwp->jw_xwindow,xc);
	}else{
		Pixmap cplane;
		int width,height;
		width = WIDTH(cp->c_bm->bm_box);
		height = HEIGHT(cp->c_bm->bm_box);
		setgcrop(GXcopy);
		setgcclip(noclipbox);
		cplane = XCreatePixmap(WXD,jdd->jd_root,width,height,1/*depth*/);
		if(cplane==0)gk0xwwpanic("cudisplay: couldnt get space");
	/* hitech doesnt want to copyplane to a 1 plane bitmap,
	 * doesnt want to rasterop in a 1 plane bitmap either.
	 * lets try converting to an image and back again?
	 */
	if(!(jdd->jd_flags&JNOCOPYPLANE)){
		XCopyPlane(WXD,jbm(cp->c_bm)->jbm_memory,cplane,WXGC,
		  0,0/*source x,y */,width,height,
		  0,0/*destn x,y */,(unsigned long)1/* plane */);
		xc = XCreatePixmapCursor(WXD,cplane,
			(Pixmap)0/*mask*/,&fcolour,&bcolour,xhot,yhot);
		if(jwp->jw_flags&JWSCREEN){
			setscreencursor(wp,xc);
			xc = 0;	/* dont free */
		}else XDefineCursor(WXD,jwp->jw_xwindow,xc);
	}
		XFreePixmap(WXD,cplane);
	}
	/* X says: hotspot must be inside pixmap. pixmap must be depth 1 */
	/* X does not refer to pixmap again */
/*	XUndefineCursor(WXD,jwp->jw_xwindow); use parent's
	XQueryBestCursor(WXD,jdd->jd_root,w,h,&ww,&hh);
*/
}
