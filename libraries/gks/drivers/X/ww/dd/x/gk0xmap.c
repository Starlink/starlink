/* copyright (c) Mark M Martin. RAL. 1987 */
#include "dd.h"
#include "map.h"
/*
 * typedef XImage{
    int width, height;		size of image
    int xoffset;		number of pixels offset in X direction
    int format;			XYBitmap, XYPixmap, ZPixmap
    char *data;			pointer to image data
    int byte_order;		data byte order, LSBFirst, MSBFirst
    int bitmap_unit;		quant. of scanline 8, 16, 32
    int bitmap_bit_order;	LSBFirst, MSBFirst
    int bitmap_pad;		8, 16, 32 either XY or ZPixmap
    int depth;			depth of image
    int bytes_per_line;		accelarator to next line
    int bits_per_pixel;		bits per pixel (ZPixmap)
 *
 * sun X byte order 0123 4567 ...
 */
static bitmap *origbm;
static int width,height,origflags;
static box origb;
static XImage *imp;
DDPUBLIC struct layout *gk0xmaptomem(bm,b,flags)bitmap *bm;box b;{
	jbitmap *jbp = jbm(bm);
	char *dataptr;
	int fast;
	static struct layout l;

	width = WIDTH(b);
	height = HEIGHT(b);
	origflags = flags;
	origbm = bm;
	origb = b;
	if(!(flags&WWMAPGET)){
		dataptr = calloc((width+15)/16*2*height,1);
		imp = XCreateImage(WXD,&DefaultVisual(WXD,WXS),1/*depth*/,XYBitmap,
			0/*offset*/,dataptr,width,height,
			16/*xpad*/,0/*bytes per line */);
	}else{
		gk0xsetbmrop(GXcopy,bm);
		if(jdd->jd_depth==1)
		imp = XGetImage(WXD,jbp->jbm_drawable,b.b_left,b.b_top,width,height,
			(long)AllPlanes,ZPixmap/*or XYPixmap*/);
		else
		imp = XGetImage(WXD,jbp->jbm_drawable,b.b_left,b.b_top,width,height,
			(long)1/*plane*/,XYPixmap);
	}
/* on a sun:
 * the XCreateImage(,,,XYPixmap,,)  or ZPixmap seem to give XPutPixel trouble
 * in setting the bits: if 1st word was 0xffffffff in an image 14x15
 * setting to pixel 1 or 0 at x=0,y=0 would make word 0xffffff7f
 * and so on to the right till x==7, then at x==8 it clears 0xffff7fff
 * It certainly doesnt set any black bits...
 * (You cant use XYBitmap)
 * On hitech: want to XCopyPlane from source 8 planes to 1 plane pixmap
 * and them map this into memory, but XCopyPlane wont.
 */
	fast = (imp->xoffset==0 &&
		imp->byte_order==MSBFirst &&
		imp->bitmap_bit_order==MSBFirst &&
		imp->bits_per_pixel==1 &&
		imp->depth==1);
	if(!fast)gk0xwwpanic("gk0xwwcharmap: cannot map pixwin to memory");
	l.l_xoffset = -b.b_left;
	l.l_yoffset = -b.b_top;
	l.l_bytestride = imp->bytes_per_line;
	l.l_base = (unsigned char *)imp->data;
	l.l_width = width;
	l.l_height = height;
	return(&l);
}
DDPUBLIC void gk0xunmapfrommem(){
	if(origflags&WWMAPPUT){
		gk0xsetbmrop(GXcopy,origbm);
		setgcclip(jbm(origbm)->jbm_boxclip);
		XPutImage(WXD,jbm(origbm)->jbm_drawable,WXGC,imp,0,0/*source xy*/,
			origb.b_left,origb.b_top/*dest xy*/,width,height);
	}
	XDestroyImage(imp);
	imp = 0;
}
#ifdef NEVER
if not fast...

	while(x<limit){
		if(put){
			XPutPixel(imp,x,y,(*charp & mask)?jdd->jd_xgcvalues.foreground:jdd->jd_xgcvalues.background);
		}else{
			if(XGetPixel(imp,x,y)!=jdd->jd_xgcvalues.background)*charp |= mask;
		}
#endif
