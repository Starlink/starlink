/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
#include "font.h"
/*
 * dynamically design text cursor for given height and width of font.
 */
#define STUB(x,y,tox,toy)	gk0xbmbox(gk0xboxbuild(x,y,tox,toy),BMNOTALL)
PUBLIC cursor *gk0xftcursor(width,height)int width,height;{
	int h,stublen,w;
	bitmap *save;
	cursor *cu;

	save = ddbm;
	if(width>MAXDIMCURSOR)width = MAXDIMCURSOR;
	else if(width<6)width = 6;
	height += 6;
	if(height>MAXDIMCURSOR)height = MAXDIMCURSOR;
	else if(height<10)height = 10;
	cu = STRUCTMALLOC(cursor);
	cu->c_bm = ddbm = gk0xbmget(width,height);
	h = (height+1)/2;
	w = (width+1)/2;
	stublen = (height+9)/10;
	STUB(0,                 0, width-1,         1);
	STUB(0,          height-2, width-1,  height-1);	/* two horiz bars */
	STUB(w-1,               2,       w, 1+stublen);
	STUB(w-1,height-2-stublen,       w,  height-3);	/* two vert bars */
	cu->c_xhot = w;
	cu->c_yhot = h;
	cu->c_xoffset = 0;
	cu->c_yoffset = 0;
	cu->c_rop = WWXOR;
	ddbm = save;
	return(cu);
}
#ifdef FORTINTER
FORTINTER int wftcsr_(width,height)int *width,*height;{
	return(int)ftcursor(*width,*height);
}
#endif FORTINTER
PRIVATE void gk0xOverstrike();
/*
 * If flags contradict use centre instead of right just or bottom just.
 * Default is top left justification.
 * The box is NOT INCLUSIVE. The chars must fit inside the box.
 */
PUBLIC void gk0xftprint(b,str,flags) box b; char *str; int flags;{
	box text;
	int len,nx,ny,lines;

	if(flags & ~(FTCENTRE|FTOVER|FTNOT|FTWIN|FTRIGHT|FTBOTTOM|FTPROP|FTCONTROL))
		wwfail("unknown flag to ftprint",);
	if(str==NULLPTR(char))len = 0;
	else	len = strlen(str);
	if(flags&FTWIN){
		gk0xbmstack(WWPUSH);
		ddbm = ddwin->w_bm;
	}
	nx = (b.b_right-b.b_left-1)/ddfont->f_width;	/* chars we can get across box width */
	lines = (nx==0)?0:(len+nx-1)/nx;				/* lines we need then */
	ny = (b.b_bottom-b.b_top-1)/ddfont->f_height;	/* lines we can fit down box ht */
	if(lines>ny)lines = ny;				/* lines we will print */
	text = gk0xboxzoom(b,-1);
	if(flags&FTBOTTOM)
		text.b_top = b.b_bottom - lines*ddfont->f_height;
	if(flags&FTVERT)
		text.b_top = (b.b_top+b.b_bottom - lines*ddfont->f_height)/2;
	if(flags&FTRIGHT)
		text.b_left = b.b_right - (len<nx?len:nx)*ddfont->f_width;
	if(flags&FTHORIZ)
		text.b_left = (b.b_left+b.b_right - (len<nx?len:nx)*ddfont->f_width)/2;
	text.b_bottom = text.b_top+ddfont->f_height-1;
	gk0xwwstack(WWPUSHOFF);
	if(!(flags&FTOVER))gk0xbmbox(b,BMCLEAR);
	while(lines-->0){
		gk0xftxprint(text.b_left,text.b_bottom,str,len>nx?nx:len,WWXOR,ddfont,ddbm,flags&(FTPROP|FTCONTROL));
		text.b_top = text.b_bottom+1;
		text.b_bottom += ddfont->f_height;
		str += nx;
		len -= nx;
	}
	if(flags&FTNOT)gk0xbmbox(b,BMNOT);
	if(flags&FTWIN)gk0xbmstack(WWPOP);
	gk0xwwstack(WWPOP);
}
#ifdef FORTINTER
FORTINTER wftpri_(b,m,flags,mlen)int *b,*flags,mlen; char *m;{
	ftprint(to_box(b),m,*flags);
}
#ifdef OLDFORTRAN
FORTINTER wftprt_(b,m,flags,length)int *b,*flags,length;char *m;{
 	char *copy = malloc((unsigned)length + 1);

 	strncpy(copy,m,length);
 	copy[length] = '\0';
	ftprint(to_box(b),copy,*flags);
	free(copy);
}
#endif OLDFORTRAN
#endif FORTINTER
#define OCT177		'\177'
#define NONSPACE	'+'	/* anything that's not a space */
#define PRINTABLE	'@'	/* makes a control char printable */
#define SPACE		' '
#define NONEWLINE	'!'	/* print this in reverse video shows no newline */

/*
 * print str with x,y bottom left corner using raster op rop
 * and font in bitmap bm.
 * Optimise out malloc most of the time
 */
#define OPTIMLEN 150	/* arbitrary value > most len calls */
PUBLIC void gk0xftxprint(x,y,str,len,rop,font,bm,flags)int x,y,len,rop,flags; char *str; fontinfo *font;bitmap *bm;{
	unsigned char optimised[OPTIMLEN];
	unsigned char *copy,*ip,*op,*end,ch;
	short hadctl,hadtopbit;
	int blobcontrol;
	bitmap *savebm;
	box b;

	if(flags&~(FTCONTROL|FTPROP))wwfail("unknown flag to ftxprint",);
	if(len<0 || len>90000)return;
	hadctl = hadtopbit = FALSE;

	blobcontrol = flags&FTCONTROL;
	savebm = ddbm;
	ddbm = bm;	/* for overstrike */

	b.b_bottom = y;
	b.b_left = x;
	b.b_top = b.b_bottom-font->f_height+1;
	b.b_right = b.b_left+len*font->f_width-1;	/* b is exactly chars */
#ifdef WWFORSUN
	gk0xwwlock(bm,b);
#endif WWFORSUN
	ip = (unsigned char *)str;
	end = ip+len;
	if(len+2<OPTIMLEN)copy = optimised;
	else copy = (unsigned char *)malloc((unsigned)(len+2));	/* +1 in case of bugs, +1 for wc null */
	op = copy;
#ifdef WWFORPNX
	while(ip<end){
		if((ch = *ip++)>OCT177){
			hadtopbit++;
			ch &= OCT177;      /* clean top bit */
		}
		if(ch<SPACE){
			ch += blobcontrol?PRINTABLE:0200;	/* make it printable or wrap on font */
			hadctl++;
		}
		*op++ = ch;
	}
#else WWFORPNX
	if(!blobcontrol)
		while(ip<end)*op++ = *ip++;
	else	while(ip<end){
		if((ch = *ip++)>OCT177){
			hadtopbit++;
			ch &= OCT177;      /* clean top bit */
		}
		if(ch<SPACE){
			ch += PRINTABLE;  /* make it printable */
			hadctl++;
		}
		*op++ = ch;
	}
	*op = '\0';	/* terminating null for GPutString or pf_text */
#endif WWFORPNX
	gk0xputbyte(x,y,copy,len,font,rop,flags);

	if(blobcontrol){
		b.b_right = b.b_left+font->f_width-1;	/* make b inclusive exactly first char of line */
		if(hadctl){     /* need to overstrike the control chars */
			ip = (unsigned char *)str;
			op = copy;
			while(ip<end){
				*op++ = ((*ip++ & OCT177)<SPACE)?NONSPACE:SPACE;
			}
			gk0xOverstrike(b,copy,len,FALSE);
		}

		if(hadtopbit){     /* need to overstrike the top bit set chars */
			ip = (unsigned char *)str;
			op = copy;
			while(ip<end)
				*op++ = (*ip++ > OCT177)?NONSPACE:SPACE;
			gk0xOverstrike(b,copy,len,TRUE);
		}
	}
	b.b_right = b.b_left+len*font->f_width-1;	/* b is exactly chars */
#ifdef WWFORSUN
	gk0xwwunlock(ddbm);
#endif WWFORSUN
	NOTESCREEN(ddbm,b);
	ddbm = savebm;
	if(copy!=optimised)free((char*)copy);
}
#ifdef FORTINTER
FORTINTER wftxp_(x,y,str,rop,font,bm,flags,len)char *str; int *x,*y,len,*rop,*font,*bm,*flags;
/* int x,y,len,rop,flags; char *str; bitmap *bm; fontinfo *font; */{
	ftxprint(*x,*y,str,len,*rop,(fontinfo *)*font,(bitmap *)*bm,*flags);
}
#ifdef OLDFORTRAN
FORTINTER wftxpr_(x,y,str,len,rop,font,bm,flags,length)
	int *x,*y,*len,*rop,*flags,length; char *str; fontinfo **font;bitmap **bm;{
	if(*len!=0)length = *len;
	ftxprint(*x,*y,str,length,*rop,*font,*bm,*flags);
}
#endif OLDFORTRAN
#endif FORTINTER
/*
 * overstrike the chars already printed.
 * The box inclusive is exactly the first single char in position.
 * Chars that are not a space are to be overstruck.
 * topbit is true for a smaller overstrike 3/4 of the way up char.
 */
PRIVATE void gk0xOverstrike(b,str,len,topbit)box b;unsigned char *str;{
	int w,x;
	unsigned char *start,*s,*end;

	w = b.b_right-b.b_left+1;
	if(topbit)
		b.b_bottom = b.b_top+(b.b_bottom-b.b_top+1)/4;	/* smaller rectangle in top 1/4 of char */
	x = b.b_left-1;	/* -1 to account for inside of box to invert */
	b.b_top--;
	b.b_bottom++;
	s = str;
	end = str+len;
	while(s<end){
		while(s<end && *s==SPACE)s++;
		if(s>=end)return;
		start = s;	/* found first non blank, first overstrike */
		while(s<end && *s!=SPACE)s++;
		/* start to s-1 are now contiguous set of chars to overstrike */
		b.b_left = x+(start-str)*w;
		b.b_right = x+(s-str)*w+1;	/* +1 cos x is outside box */
		gk0xbmbox(b,BMNOT);
		/* carry on from this blank */
	}
}
#ifdef FORTINTER
FORTINTER int wftloa_(file,len)char *file; int len;{	/* char *file; */
	return(int)gk0xftload(file);
}
#ifdef OLDFORTRAN
FORTINTER int wftlod_(file,length)char *file;int length;{
 	char *malloc(),*name = malloc((unsigned)length+1);

 	strncpy(name,file,length);
 	name[length] = '\0';
	return(int)gk0xftload(name);
}
#endif OLDFORTRAN
#endif FORTINTER
/*
 * add font to circular list of all fonts
 */
static fontinfo *fontlist = NULLPTR(fontinfo);
DDPUBLIC gk0xaddfont(fp) fontinfo *fp;{
	fontinfo *qp;
	if(fontlist==0)fp->f_next = fontlist = fp;
	else{
		for(qp = fontlist; qp->f_next!=fontlist; qp = qp->f_next);
		fp->f_next = fontlist;
		qp->f_next = fp;
	}
}
#ifdef FORTINTER
FORTINTER wftfre_(fp)int *fp;{
	ftfree((fontinfo *)*fp);
}
FORTINTER wftbox_(fp,str,len,flags,newbox,stsize)int *fp,*len,*flags,*newbox,stsize; char *str;{
/* fontinfo *fp; int len,flags; box newbox;*/
	from_box(ftbox((fontinfo *)*fp,str,*len,*flags ),newbox);
}
#endif FORTINTER
