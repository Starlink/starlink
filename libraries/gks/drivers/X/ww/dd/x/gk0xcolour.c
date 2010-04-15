/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
/*
 * set the colour of colour to rep (packed 3 chars rgb) in the ddbm
 */
/*
 * set or get the number of colours in the colour map. copy old to new.
 * returns max num colours in this map.
 */
PUBLIC int gk0xcosize(bm,colours)bitmap *bm;int colours;{
	jwindow *jwp;
	int newsize;

	if(bm==0)gk0xwwpanic("gk0xcorep: null bitmap");
	if(colours>bm->bm_colours)gk0xwwpanic("gk0xcosize: too many colours");
	jwp = jwin(bm->bm_window);

	if(!(jwp->jw_flags&JWWINDOW) || (jdd->jd_flags&JMONO))return(jwp->jw_colmapsize);

	newsize = (colours<=1)?2:(1<<gk0xcoltodepth(colours));
	if(newsize!=jwp->jw_colmapsize || (jwp->jw_flags&JWDEFAULTCOLOUR)){
		/* "changing default colourmap" call */
		unsigned long planemasks[8],pixels[2];
		XColor *oldrgb;
		int i,numentries;

		if(DisplayPlanes(WXD,WXS)==1)return(jwp->jw_colmapsize);
		numentries = jwp->jw_colmapsize>newsize?newsize:jwp->jw_colmapsize;
		if(jwp->jw_cmap!=0){
			oldrgb = (XColor*)malloc(numentries*sizeof(XColor));
			for(i=0;i<numentries;i++)oldrgb[i].pixel = i;
			XQueryColors(WXD,jwp->jw_cmap,oldrgb,numentries);
			if(!(jwp->jw_flags&JWDEFAULTCOLOUR))
				XFreeColormap(WXD,jwp->jw_cmap);
		}else oldrgb = 0;
		jwp->jw_cmap = XCreateColormap(WXD,jwp->jw_xwindow,DefaultVisual(WXD,WXS),AllocNone);
		/* TAW Nov 88 If we cannot allocate it may be becuase 2 colours
		   are already taken so try allocating 2 less */
		if(XAllocColorCells(WXD,jwp->jw_cmap,(Bool)1/*contiguous*/,
			planemasks,gk0xcoltodepth(newsize)/*planes*/,
			pixels,1/*colours??*/)==0)
				{ if(XAllocColorCells(WXD,jwp->jw_cmap,(Bool)1/*contiguous*/,
	                        planemasks,gk0xcoltodepth(newsize)-1/*planes*/,
	                        pixels,2/*colours??*/)==0)
					gk0xwwpanic("cannot alloc colourmap");
				}
		if(pixels[0]!=0)gk0xwwpanic("not given 0 starting pixel by X");
		if(oldrgb!=0){
			XStoreColors(WXD,jwp->jw_cmap,oldrgb,numentries);
			free((char*)oldrgb);
		}
		jwp->jw_flags &= ~JWDEFAULTCOLOUR;
		jwp->jw_colmapsize = newsize;
		XSetWindowColormap(WXD,jwp->jw_xwindow,jwp->jw_cmap);
	}
	return(jwp->jw_colmapsize);
}
PRIVATE int gk0xcosetdefault(bm)bitmap *bm;{
	static Colormap defaultcolourmap = 0;
	jwindow *jwp;
	int count;
	unsigned char *r,*g,*b;
	char *name;

	jwp = jwin(bm->bm_window);
	if(!jwp->jw_flags&JWWINDOW)return(0);
	jwp->jw_flags |= JWDEFAULTCOLOUR;
	jwp->jw_cmap = 0;
	jwp->jw_colmapsize = 2;
	if(jdd->jd_flags&JMONO)return(2);
	if(dd->d_colours==MONOCHROME)return(2);
	if(bm->bm_colours!=MONOCHROME){
		/* black red green yellow blue magenta cyan white */
		static unsigned char rr[8] = {0,255,0,255,0,255,0,255};
		static unsigned char gg[8] = {0,0,255,255,0,0,255,255};
		static unsigned char bb[8] = {0,0,0,0,255,255,255,255};
		count = 8;
		r = rr; g = gg; b = bb;
	}else{
		static unsigned char rr[2] = {0,255};
		static unsigned char gg[2] = {0,255};
		static unsigned char bb[2] = {0,255};
		count = 2;
		r = rr; g = gg; b = bb;
	}
	if(defaultcolourmap==0){	/* first call */
		if(jdd->jd_defcolsize>0 && jdd->jd_defcolsize<=count){
			int i;
			for(i=jdd->jd_defcolsize-1;i>=0;i--)
			if(jdd->jd_defaultcolour[i]&ENTRYSET){
				r[i] = jdd->jd_defaultcolour[i]>>16;
				g[i] = jdd->jd_defaultcolour[i]>>8;
				b[i] = jdd->jd_defaultcolour[i];
			}
		}
		jwp->jw_colmapsize = 0;
		gk0xcorep(bm,r,g,b,1/*step*/,0/*start*/,count,COSET);
		jwp->jw_flags |= JWDEFAULTCOLOUR;	/* restore this */
		defaultcolourmap = jwp->jw_cmap;	/* never freed */
	}
	jwp->jw_colmapsize = count;
	jwp->jw_cmap = defaultcolourmap;
	XSetWindowColormap(WXD,jwp->jw_xwindow,jwp->jw_cmap);
	if((jdd->jd_flags&JINSTALLMAP) && jwp->jw_cmap!=0)
		XInstallColormap(WXD,jwp->jw_cmap);
	return(count);
}
/*
 * return the number of entries set/got in the colour map for the window
 * and perhaps set or get all the rgb or hsl (one day) entries
 * or only one. colour map grows to accommodate more colours, up to max
 * for window.
 */
PUBLIC int gk0xcorep(bm,r,g,b,step,start,count,flags)bitmap *bm;char *r,*g,*b; int step,start,count,flags;{
	jwindow *jwp;
	int i,pix;
	XColor *rgb;

	if(bm==0)gk0xwwpanic("gk0xcorep: null bitmap");
	if(flags==COSETDEFAULT)
		return(gk0xcosetdefault(bm));
	jwp = jwin(bm->bm_window);

	if(!(jwp->jw_flags&JWWINDOW))return(0);

	if(count>bm->bm_colours || count<0)gk0xwwpanic("corep: bad count");
	if(start+count>bm->bm_colours || start<0)gk0xwwpanic("corep: bad start");
	if(count>0 && (r==0 || g==0 || b==0))gk0xwwpanic("corep: zero r g b addresses");
	if(count==0)return(0);
	if(DisplayPlanes(WXD,WXS)==1 || (jdd->jd_flags&JMONO))return(0);
	rgb = (XColor*)malloc(count*sizeof(XColor));
	switch(flags){
	case COSET:
		for(i=0;i<count;i++){
			rgb[i].pixel = i+start;
			rgb[i].red = *r<<8;	r += step;
			rgb[i].green = *g<<8;	g += step;
			rgb[i].blue = *b<<8;	b += step;
			rgb[i].flags = DoRed|DoGreen|DoBlue;
		}
		if(count+start>jwp->jw_colmapsize || (jwp->jw_flags&JWDEFAULTCOLOUR)){
		/* need new colourmap for more entries
		   or trying to set default shared colourmap
		   ensure colourmap doesnt shrink...  */
			gk0xcosize(bm,count+start>jwp->jw_colmapsize?count+start:jwp->jw_colmapsize);
		}
		XStoreColors(WXD,jwp->jw_cmap,rgb,count);
		if(jdd->jd_flags&JINSTALLMAP)
			XInstallColormap(WXD,jwp->jw_cmap);
		break;
	case COGET:
		if(count+start>jwp->jw_colmapsize)count = jwp->jw_colmapsize-start;
		for(i=0;i<count;i++)rgb[i].pixel = start+i;
		if(count>0)XQueryColors(WXD,jwp->jw_cmap,rgb,count);
		for(i=0;i<count;i++){
			pix = rgb[i].pixel-start;
			if(pix<0 || pix>=count)gk0xwwpanic("gk0xcorep: X returned pixels out of range");
			pix *= step;
			r[pix] = rgb[i].red>>8;
			g[pix] = rgb[i].green>>8;
			b[pix] = rgb[i].blue>>8;
		}
		break;
	default:gk0xwwpanic("gk0xcorep: bad flag");
	}
	free((char*)rgb);
	return(count);
}
