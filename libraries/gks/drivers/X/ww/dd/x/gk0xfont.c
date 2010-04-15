/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
#include "font.h"
PRIVATE void setgcfont(fp)fontinfo *fp;{
	Font f;
	f = jft(fp)->jf_font;
	if(f!=jdd->jd_xgcvalues.font){
		XSetFont(WXD,WXGC,f);
		jdd->jd_xgcvalues.font = f;
	}
}
/*
 * x,y is bottom left of start of string
 * if text is proportional, do printing char by char
 * Also, on whitechapel and sun, if text includes null, or is beyond end of font, do printing char by char,
 * and string must end with null for GPutString!
 */
DDPUBLIC void gk0xputbyte(x,y,string,len,fp,rop,flags)unsigned char *string;fontinfo *fp;{
	jbitmap *jbp;
	jbp = jbm(ddbm);
	gk0xsetbmrop(gk0xwwrasop(rop),ddbm);
	setgcclip(jbp->jbm_boxclip);
	setgcfont(fp);
	XDrawString(WXD,jbp->jbm_drawable,WXGC,x,y-jft(fp)->jf_offset,string,len);
	/* do we want XDrawImageString:
	 * it ignores rop fn, and fills destn rect with background, then
	 * paints text in foreground. XDrawString uses char as a mask */
}
/*
 * get the font into mem and set up struct for it.
 * Null file means get info on window's font.
 * check through all other fonts to see if any exactly same.
 * The default font (ourfont) cannot be released, so remember which it is!
 */
static fontinfo *ourfont;
static fontinfo *fontlist = NULLPTR(fontinfo);
PUBLIC fontinfo *gk0xftload(file) char *file;{
	fontinfo *fp;
	jfontinfo *jfp;
	Font fontp;

	if(file!=NULLPTR(char)){
/*
 * on sun XLoadFont returns ok even if cannot find font.
 * XQueryFont then bombs out
 */
#ifdef DOESNTWORK
		fontp = XLoadFont(WXD,file);
#else DOESNTWORK
	{	XFontStruct *xfp;
		xfp = XLoadQueryFont(WXD,file);
		fontp = xfp!=0?xfp->fid:0;
	}
#endif DOESNTWORK
		if(fontp==0){
			sprintf(dd->d_errmsg,"Error in opening font file %.20s",file);
			return(NULLPTR(fontinfo));
		}
	}else{	/* return default font */
		if(ourfont!=NULLPTR(fontinfo))return(ourfont);
#ifdef DOESNTWORK
	XFontStruct *xfp;
		xfp = XQueryFont(WXD,WXGC->gid);
		/* cannot get default font any other way? */
		/* mustnt have setgcfont to anything else */
		fontp = xfp->fid;
#else DOESNTWORK
		fontp = XLoadFont(WXD,"fixed");
#endif DOESNTWORK
	}
	fp = STRUCTMALLOC(fontinfo);
	jfp = STRUCTMALLOC(jfontinfo);
	fp->f_junk = (int *)jfp;
	jfp->jf_font = fontp;
	jfp->jf_links = 0;
	jfp->jf_flags = 0;
	if(!gk0xwwchkfont(fp)){	/* not really a font at all */
		free((char*)fontp);
		free((char*)jfp);
		free((char*)fp);
		return(NULLPTR(fontinfo));	/* err msg set already */
	}
	if(file==0){
		ourfont = fp;
		jfp->jf_flags |= DONTFREE;
		fp->f_name = NULLPTR(char);
	}else{
		fp->f_name = malloc((unsigned)(strlen(file)+1));
		(void)strcpy(fp->f_name,file);
	}
	fp->f_cursor = gk0xftcursor(fp->f_width,fp->f_height);
	gk0xaddfont(fp);
	return(fp);
}
/*
 * Check if users font is proportional.
 * return true if font valid. set width to max.
 */
PRIVATE int gk0xwwchkfont(fp)fontinfo *fp;{
	int prop,i,junk,offset;
	XCharStruct xcs;
	XFontStruct *xfp;
	jfontinfo *jfp;
	char errmsg[WWERRLEN];
	char ch;

	jfp = jft(fp);
	offset = prop = FALSE;
	fp->f_height = UNSET;
	fp->f_width = UNSET;
	ch = '\0';
	xfp = XQueryFont(WXD,jfp->jf_font);
	for(i=0;i<256;i++){
		ch = i;
		XTextExtents(xfp,&ch,1,&junk,&junk,&junk,&xcs);
		if(xcs.width!=fp->f_width){
			if(fp->f_width!=UNSET)prop = TRUE;
			if(xcs.width>fp->f_width)fp->f_width = xcs.width;
		}
		if((xcs.ascent+=xcs.descent)!=fp->f_height){
			if(fp->f_height!=UNSET)prop = TRUE;
			if(xcs.ascent>fp->f_height)fp->f_height = xcs.ascent;
		}
		if(xcs.descent!=jfp->jf_offset){
			if(offset++)prop = TRUE;
			if(xcs.descent>jfp->jf_offset)jfp->jf_offset = xcs.descent;
		}

	}
	if(fp->f_height<=0 || fp->f_width<=0){
		sprintf(errmsg,"improper font height %d or width %d",fp->f_height,fp->f_width);
		wwfail(errmsg,FALSE);
	}
	if(prop)jfp->jf_flags |= PROPORTIONAL;
	fp->f_baseline = jfp->jf_offset;
	return(TRUE);
}
/*
 * return the box enclosing the given text.
 * Box is (-1,-1,width in pixels, height in pixels)
 * no special treatment of chars at the moment
 * Must have flags==FTPROP at the moment.
 */
PUBLIC box gk0xftbox(fp,str,len,flags)fontinfo *fp;char *str;int len,flags;{
	box b,nb;
	int notfirst,old,cx,junk;
	XCharStruct xcs;
	XFontStruct *xfp;
	jfontinfo *jfp;

	if(flags&~(FTPROP|FTKERN))wwfail("unknown flag to ftbox",b);
	if(!(flags&FTPROP))wwfail("ftbox must have FTPROP flag at the moment",b);
	old = !(flags&FTKERN);
	if(len>90000)gk0xwwpanic("ftbox: len>90000");
	jfp = jft(fp);
	xfp = XQueryFont(WXD,jfp->jf_font);
	if(old){
		b = gk0xboxbuild(-1,-1,0,0);
		b.b_bottom += fp->f_height;
		while(len-->0){
			XTextExtents(xfp,str++,1,&junk,&junk,&junk,&xcs);
			b.b_right += xcs.width;
		}
		return(b);
	}
	b = gk0xboxbuild(0,0,0,0);	/* in case all null chars */
	cx = 0;
	notfirst = FALSE;
	while(len-->0){
		XTextExtents(xfp,str++,1,&junk,&junk,&junk,&xcs);
		nb = gk0xboxbuild(cx+xcs.lbearing,xcs.ascent+1,
			cx+xcs.rbearing-1,xcs.descent);
		if(notfirst++)b = gk0xboxop(b,nb,BOXENCLOSING);
		else b = nb;
		cx += xcs.width;
	}
	return(b);
}
