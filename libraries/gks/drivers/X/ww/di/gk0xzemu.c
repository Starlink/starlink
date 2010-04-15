/* copyright (c) Mark M Martin. RAL. 1986 */
#include "di.h"
/*
 * Bug: ignore input if box too small to show owt. Hence will be in wrong
 * state perhaps when resized.
 *
 * Emulating part of a cif2605:
 	automatic margins on
	cursor addressing: ESC P column-as-a-char row-as-a-char (0,0 first position).
	\0 and \177	ignored
	ESC A	cursor up
	\n	cursor down
	\b	cursor left (wraps at start of line)
	ESC C	cursor right (cif2634) (wraps)
	\t	tab
	ESC 9	back tab (cif2634)
	\r	cursor to start of line
	ESC K	erase to end of line
	ESC B	erase to end of screen
	ESC J and \014 (^L)	clear screen
	ESC Q	set screen colour to black
	ESC Z	revert screen colour
	ESC N ESC L	enter standout mode (inverted characters)
	ESC M ESC O	leave standout mode
	ESC anything else ignored
(none cifer extensions)
	^x a	scroll forward
	^x b	scroll reverse
	^x c	underline start
	^x d	underline end
	^x e	insert line in front of cursor line
	^x f	delete line
	^x g	delete char
	^x h	insert mode
	^x i	end insert mode
	^x j	goto status line
	^x k	leave status line
	^x l	delete status line
	^x m	reset
	^x n	redraw if saving text. ignore standout/underline. not in termcap.
 * comment out define of burp to get check on invalid escapes, unexpected chars */
#define burp(msg,val)

#define ESC	'\033'
#define CONTROLX	'\030'

#define XXignore 255
#define XXup	1
#define XXcd	2
#define XXcl	3
#define XXce	4
#define XXcm	6
#define XXso	7
#define XXse	8
#define XXinvert	9
#define XXrevert	10
#define XXbt	11
#define XXnd	12

typedef struct{
	box jp_bcursor;		/* pixel coords of last cursor posn */
	int jp_mode;		/* holds state of which escapes/controls received. */
	int jp_scrollmax;	/* == p_ymax or p_ymax-1 if PSTATUS set */
	int jp_oldx,jp_oldy;
}jemupane;
#define jem(pp)	((jemupane *)((pp)->p_junk))

#ifndef burp
static FILE *err;
#endif burp
PRIVATE void gk0xstepy();
PRIVATE void gk0xshift();
PRIVATE void gk0xshowinvert();
PRIVATE void gk0xshowcursor();
PRIVATE void gk0xclrtobot();
PRIVATE void gk0xclrtoeol();
PRIVATE void gk0xclear();
PRIVATE void gk0xoutscroll();
PRIVATE void gk0xoutflush();
PRIVATE void gk0xoutchar();
PRIVATE void gk0xscrollregion();
PRIVATE void gk0xredraw();
PRIVATE void gk0xnotetext();
/*
 * initialise the table of chars after escape and what they mean.
 */
#define FATFONT	"/usr/lib/fonts/fat.kst"
static fontinfo *fatfont = 0;
static char emap[128];
PRIVATE void gk0xemusetup(){
	emap['9'] = XXbt;
	emap['A'] = XXup;
	emap['B'] = XXcd;
	emap['C'] = XXnd;
	emap['J'] = XXcl;
	emap['K'] = XXce;
	emap['Q'] = XXinvert;
	emap['Z'] = XXrevert;
	emap['P'] = XXcm;
	emap['N'] = XXso;
	emap['M'] = XXse;
	emap['v'] = XXignore;	/* esc-v: switch off status line */
	emap['w'] = XXignore;
	fatfont = gk0xftload(FATFONT);
#ifndef burp
	err = fopen("/dev/err","w");
	if(err==NULL)err = stdout;
#endif burp
}
/*
 * The current emupane is set in pp for all those global uses below
 * by all procs with ep as an arg.
 * box b is inclusive, but we allow a 1 pixel margin around to draw text cursor.
 */
static emupane *pp;
PUBLIC emupane *gk0xemucreate(b)box b;{
	jemupane *jpp;
	static int latercall = FALSE;

	if(!latercall++)gk0xemusetup();
	pp = STRUCTMALLOC(emupane);
	jpp = STRUCTMALLOC(jemupane);
	pp->p_junk = (int *)jpp;
/* ----	jpp->jp_mode = 0;
	pp->p_flags = 0;
	pp->p_y = pp->p_x = 0;
	pp->p_length = 0;
	pp->p_text = 0;	--------- */
	gk0xemurecreate(pp,b);
	return(pp);
}
PUBLIC void gk0xemurecreate(ep, b)emupane *ep; box b;{
	int oldx,oldy;
	pp = ep;
	pp->p_height = ddfont->f_height;
	pp->p_width = ddfont->f_width;
	pp->p_font = ddfont;
	if(fatfont && fatfont!=ddfont && fatfont->f_width==ddfont->f_width && fatfont->f_height==ddfont->f_height)
		pp->p_flags |= PFATFONT;
	else pp->p_flags &= ~PFATFONT;
	oldx = pp->p_xmax;
	oldy = pp->p_ymax;
	pp->p_box = b;
	pp->p_xmax = (b.b_right-b.b_left-1)/pp->p_width-1;	/* pixel border */
	jem(pp)->jp_scrollmax = pp->p_ymax = (b.b_bottom-b.b_top-1)/pp->p_height-1;
	if(pp->p_xmax<0 || pp->p_ymax<0)pp->p_xmax = pp->p_ymax = -1;
	else{
		if(pp->p_x<0)pp->p_x = 0;
		if(pp->p_y<0)pp->p_y = 0;
	}
	if(pp->p_x>pp->p_xmax)pp->p_x = pp->p_xmax;
	if(pp->p_y>pp->p_ymax)pp->p_y = pp->p_ymax;
	pp->p_flags &= ~(PSTATUS|PINSTATUS);
	if(pp->p_flags & PSAVE)gk0xsetscreensize(pp,oldx,oldy);
	if(pp->p_flags & PMEMORY)gk0xredraw();
	if(pp->p_xmax>=0)gk0xshowcursor(TRUE);
	pp->p_select.b_left = -1;
}
/*
 * print the given chars, interpreting control chars.
 * If screen is in invertedmode do the inversion last (and undo it on entry each time).
 * Remove cursor on entry each time.
 */
PUBLIC void gk0xemuprint(ep,cp,len)emupane *ep; char *cp;int len;{
	jemupane *jpp;
	char ch;
	pp = ep;
	jpp = jem(pp);

	if(pp->p_xmax<0)return;	/* ignore all if too small to show. tough */
	gk0xwwstack(WWPUSHOFF);
	if(pp->p_flags&PINVERTED)gk0xbmbox(pp->p_box,BMNOT);
	gk0xshowcursor(FALSE);	/* remove cursor */
	if(pp->p_select.b_left!=-1)gk0xemushow(pp,gk0xboxbuild(-1,0,0,0));

	cp--;
	while(len-->0){
	ch = *++cp & '\177';
	switch(jpp->jp_mode){
	case 0:	/* base state */
		switch(ch){
		case ESC:
			jpp->jp_mode = 1;
			break;
		case CONTROLX:
			jpp->jp_mode = 8;
			break;
		case '\007':	/* bell */
			gk0xwwnoise();
			break;
		case '\b':
			if(pp->p_x--==0){
				pp->p_x = pp->p_xmax;
				gk0xstepy(-1);
			}
			break;
		case '\014':
			gk0xclear();
			pp->p_x = pp->p_y = 0;
			break;
		case '\r':
			pp->p_x = 0;
			if(!(pp->p_flags&PCRMOD))break;
		case '\n':	/* ---> MUST FOLLOW \r */
			gk0xstepy(1);
			if(pp->p_flags&PCRMOD)pp->p_x = 0;	/* cr too */
			break;
		case '\t':
			pp->p_x = (pp->p_x+8) & ~07;
			if(pp->p_x>pp->p_xmax){
				gk0xstepy(1);
				pp->p_x = 0;
			}
			break;
		default:
			if(ch<' ')gk0xoutchar(ch|'\200');
			else gk0xoutchar(ch);
			if(pp->p_x++==pp->p_xmax){	/* auto margin */
				if(!(pp->p_flags & PINSTATUS))gk0xstepy(1);
				pp->p_x = 0;
			}
			break;
		case '\0':
		case '\177':
			break;	/* ignore */
		}
		break;
	case 1:	/* esc */
		switch(emap[ch]){
		case XXup:	/* up */
			gk0xstepy(-1);
			break;
		case XXcd:	/* clear to end display */
			gk0xclrtobot();
			break;
		case XXcl:	/* clear screen */
			gk0xclear();
			pp->p_x = pp->p_y = 0;
			break;
		case XXce:	/* clear to end line */
			gk0xclrtoeol();
			break;
		case XXinvert:	/* visible bell 'screen off or invert' */
			if(!(pp->p_flags&PINVERTED)){
				pp->p_flags |= PINVERTED;
				gk0xshowinvert();
			}
			break;
		case XXrevert:	/* visible bell end */
			if((pp->p_flags&PINVERTED)){
				pp->p_flags &= ~PINVERTED;
				gk0xshowinvert();
			}
			break;
		case XXcm:	/* cursor address */
			jpp->jp_mode = 2;
			break;
		case XXso:	/* stand out */
			jpp->jp_mode = 4;
			break;
		case XXse:	/* stand out end */
			jpp->jp_mode = 6;
			break;
		case XXbt:	/* back tab */
			if(pp->p_x==0){
				pp->p_x = pp->p_xmax+1;
				gk0xstepy(-1);
			}
			pp->p_x = (pp->p_x-1) & ~07;
			break;
		case XXnd:	/* cursor right */
			if(pp->p_x++==pp->p_xmax){	/* auto margin */
				gk0xstepy(1);
				pp->p_x = 0;
			}
			break;
		case XXignore:	/* ignore */
			break;
		case 0:	/* no entry */
		default:
			burp("esc-",(int)ch);	/* back to base mode: ignore escape char */
		}
		if(jpp->jp_mode==1)jpp->jp_mode = 0;
		break;
	case 2:	/* cursor address. %r: col first */
		pp->p_x = ch;
		if(pp->p_x>pp->p_xmax){
			burp("x",pp->p_x);
			pp->p_x = pp->p_xmax;
		}
		jpp->jp_mode++;
		break;
	case 3:	/* cursor address. %r: line 2nd */
		pp->p_y = ch;
		if(pp->p_y>jpp->jp_scrollmax){
			burp("y",pp->p_y);
			pp->p_y = jpp->jp_scrollmax;
		}
		jpp->jp_mode = 0;
		break;
	case 6:	/* stand out end : se=\EM\EO: */
	case 4:	/* stand out: so=\EN\EL */
		if(ch!=ESC)burp("so|e-",(int)ch);
		jpp->jp_mode++;
		break;
	case 5:	/* stand out: so=\EN\EL */
		if(ch!='L')burp("so-ESC-",(int)ch);
		gk0xoutflush();
		pp->p_flags |= PSTANDOUT;
		jpp->jp_mode = 0;
		break;
	case 7:	/* stand out end : se=\EM\EO: */
		if(ch!='O')burp("se-ESC-",(int)ch);
		gk0xoutflush();
		pp->p_flags &= ~PSTANDOUT;
		jpp->jp_mode = 0;
		break;
	case 8:	/* extensions: ALL FLUSH */
		gk0xoutflush();
		switch(ch){
		case 'a':	/* scroll forward */
			gk0xoutscroll(TRUE);
			break;
		case 'b':	/* scroll reverse */
			gk0xoutscroll(FALSE);
			break;
		case 'c':	/* start underline */
			pp->p_flags |= PUNDERLINE;
			break;
		case 'd':	/* end underline */
			pp->p_flags &= ~PUNDERLINE;
			break;
		case 'e':	/* insert line */
			gk0xscrollregion(pp->p_y,jpp->jp_scrollmax,-1);
			pp->p_x = 0;
			break;
		case 'f':	/* delete line */
			gk0xscrollregion(pp->p_y,jpp->jp_scrollmax,1);
			pp->p_x = 0;
			break;
		case 'g':	/* delete char */
			gk0xshift(pp->p_x,pp->p_y,-1);
			break;
		case 'h':	/* insert mode */
			pp->p_flags |= PINSERT;
			break;
		case 'i':	/* end insert mode */
			pp->p_flags &= ~PINSERT;
			break;
		case 'j':	/* status line */
			if(!(pp->p_flags & PINSTATUS)){
				if(!(pp->p_flags & PSTATUS)){
					pp->p_flags |= PSTATUS;
					jpp->jp_scrollmax--;
					if(pp->p_y==pp->p_ymax){
						pp->p_y--;
						gk0xscrollregion(0,pp->p_ymax,1);
					}
				}
				pp->p_flags |= PINSTATUS;
				jpp->jp_oldx = pp->p_x;
				jpp->jp_oldy = pp->p_y;
			}
			pp->p_x = 0;
			pp->p_y = pp->p_ymax;	/* ! */
			gk0xclrtoeol();
			break;
		case 'l':	/* delete status line */
			if(pp->p_flags & PSTATUS){
				pp->p_flags &= ~PSTATUS;
				jpp->jp_scrollmax++;
				gk0xscrollregion(pp->p_ymax,pp->p_ymax,1);
			}else break;
		case 'k':	/* leave status line: MUST FOLLOW DELETE STATUS */
			if(pp->p_flags & PINSTATUS){
				pp->p_flags &= ~PINSTATUS;
				pp->p_x = jpp->jp_oldx;
				pp->p_y = jpp->jp_oldy;
			}
			break;
		case 'm':	/* reset */
			if(pp->p_flags & PSTATUS)jpp->jp_scrollmax++;
			pp->p_flags &= ~(PSTATUS|PINSTATUS|PINVERTED|PSTANDOUT|PUNDERLINE|PINSERT);	/* all flags but PCRMOD|PFATFONT|PSAVE|PMEMORY to 0 */
			break;
		case 'n':	/* redraw if saving text */
			if(pp->p_flags & PMEMORY)gk0xredraw();
			break;
		}
		jpp->jp_mode = 0;
		break;
	}
	}
	gk0xoutflush();
	gk0xshowcursor(TRUE);
	if(pp->p_flags&PINVERTED)gk0xbmbox(pp->p_box,BMNOT);
	gk0xwwstack(WWPOP);
}
/*
 * redraw screen from saved text. no underline etc.
 */
PRIVATE void gk0xredraw(){
	int x,y;
	char *cp;
	x = pp->p_x, y = pp->p_y;
	for(pp->p_y = 0;pp->p_y<=pp->p_ymax;pp->p_y++){
		cp = &P_TEXT(pp,0,pp->p_y);
		for(pp->p_x = 0;pp->p_x<pp->p_length[pp->p_y];pp->p_x++)
			gk0xoutchar(*cp++);
	}
	pp->p_x = x, pp->p_y = y;
	gk0xoutflush();
}
/*
 * step y posn on 1 (down) or -1 (up) and
 * if were at edge of screen scroll text up or down
 */
PRIVATE void gk0xstepy(dir){
	int y;
	y = pp->p_y + dir;
	if(y>jem(pp)->jp_scrollmax || y<0)
		gk0xoutscroll(dir>0);
	else pp->p_y = y;
}
/*
 * shift text right of (incl) current posn by 'by' right +ve, left -ve.
 * dont outflush due to call from outflush
 */
PRIVATE void gk0xshift(x,y,by){
	box b;
	int to;

	if(pp->p_flags&PMEMORY){
		int i;
		char *cp;
		pp->p_length[y] += by;
		if(pp->p_length[y]<0)pp->p_length[y] = 0;
		if(pp->p_length[y]>pp->p_xmax)pp->p_length[y] = pp->p_xmax;
		i = pp->p_xmax-by-x;
		if(by>=0){
			cp = &P_TEXT(pp,pp->p_xmax-by,y);
			for(;i-->=0;cp--)cp[by] = *cp;
		}else{
			cp = &P_TEXT(pp,x,y);
/*			for(;i-->=0;cp++)*cp = cp[-by];
	compiler loop on sun!!!!!!!!!
*/
			for(;i-->=0;cp++)*cp = *(cp-by);
		}
	}
	if(by>=0)b = gk0xemucharbox(pp,x,pp->p_xmax-by,y);
	else	b = gk0xemucharbox(pp,x-by,pp->p_xmax,y);
	b = gk0xboxzoom(b,-1);
	to = b.b_left+by*pp->p_width;
	if(b.b_left<b.b_right)gk0xbmmove(b,to,b.b_top);
	if(by>=0)b.b_right = to-1;
	else	b.b_left = b.b_right + by*pp->p_width-1;
	if(b.b_left<b.b_right)gk0xbmbox(b,BMCLEARALL);
}
/*
 * show new screen state temporarily. Assume caller has pushed window off.
 */
PRIVATE void gk0xshowinvert(){
	gk0xbmbox(pp->p_box,BMNOT);
	gk0xwwstack(WWPUSHON);	/* let it be seen! */
	gk0xwwstack(WWPOP);
	gk0xbmbox(pp->p_box,BMNOT);
}
PRIVATE void gk0xshowcursor(newposn){
	int saveline;
	saveline = dd->d_line;
	dd->d_line = WWXOR;
	if(newposn)
		jem(pp)->jp_bcursor = gk0xemucharbox(pp,pp->p_x,pp->p_x,pp->p_y);
	gk0xbmbox(jem(pp)->jp_bcursor,BMEDGES);
	dd->d_line = saveline;
}
#ifndef burp
PRIVATE burp(str,val)char *str;{
	fprintf(err,"emu: cannot emulate %s0%o\n",str,val);
	fflush(err);
}
#endif burp
PRIVATE void gk0xclrtobot(){
	gk0xoutflush();
	gk0xclrtoeol();
	if(pp->p_y<jem(pp)->jp_scrollmax)
		gk0xscrollregion(pp->p_y+1,jem(pp)->jp_scrollmax,jem(pp)->jp_scrollmax-pp->p_y+1);
}
PRIVATE void gk0xclrtoeol(){
	gk0xoutflush();
	if(pp->p_flags&PMEMORY)pp->p_length[pp->p_y] = pp->p_x;
	gk0xbmbox(gk0xemucharbox(pp,pp->p_x,pp->p_xmax,pp->p_y),BMCLEAR);
}
PRIVATE void gk0xclear(){
	gk0xoutflush();
	gk0xscrollregion(0,jem(pp)->jp_scrollmax,jem(pp)->jp_scrollmax+1);
}
PRIVATE void gk0xoutscroll(upwards)int upwards;{
	gk0xscrollregion(0,jem(pp)->jp_scrollmax,upwards?1:-1);
}
/*
 * keep a buffer of chars to print. Only one buffer is needed cos it is
 * always flushed before returning to user.
 */
#define MAXSTR 50
static char str[MAXSTR],*sp = str;
static int atx,aty;
PRIVATE void gk0xoutflush(){
	box b;
	int flag;
	fontinfo *save;
	if(sp==str)return;
	if(pp->p_flags&PINSERT)gk0xshift(atx,aty,sp-str);	/* make space */
	if(pp->p_flags&PMEMORY)gk0xnotetext(atx,aty,str,sp);
	*sp = '\0';
	b = gk0xemucharbox(pp,atx,atx+sp-str-1,aty);
	sp = str;
	save = ddfont;
	ddfont = pp->p_font;
	flag = 0;
	if(pp->p_flags&PSTANDOUT){
		if(pp->p_flags&PFATFONT)ddfont = fatfont;
		else flag = FTNOT;
	}
	gk0xftprint(b,str,flag);
	ddfont = save;
	if(pp->p_flags&PUNDERLINE){
		b.b_top = b.b_bottom-2;
		gk0xbmbox(b,BMNOT);
	}
}
PRIVATE void gk0xoutchar(ch)char ch;{
	if(atx+sp-str!=pp->p_x || aty!=pp->p_y)gk0xoutflush();
	if(sp==str){
		atx = pp->p_x;
		aty = pp->p_y;
	}
	*sp++ = ch;
	if(sp==str+MAXSTR)gk0xoutflush();
}
/*
 * pad out (if nec) with spaces to x,y (not incl)
 * to account for new right max excursion of text
 * and save the text
 */
PRIVATE void gk0xnotetext(x,y,string,end)int x,y;char *string,*end;{
	int from;
	from = pp->p_length[y];
	while(from<x)P_TEXT(pp,from++,y) = ' ';
	while(string<end)P_TEXT(pp,x++,y) = *string++;
	if(pp->p_length[y]<x)pp->p_length[y] = x;
}
/*
 * 1 pixel border of edges of a char posn
 */
#define CHARTOP(l)	(pp->p_box.b_top+(l)*pp->p_height)	/* +1 as p_box not inc. -1 for non inc box */
#define CHARBOT(l)	CHARTOP(l+1)+1
#define CHARLEFT(c)	(pp->p_box.b_left+(c)*pp->p_width)
#define CHARRIGHT(c)	CHARLEFT(c+1)+1
/*
 * return box exactly surrounding by 1 pixel the chars on given line.
 * Inclusive of two x posns given, 0 is first.
 * line 0 is top line. -1 is bottom line etc
 */
PUBLIC box gk0xemucharbox(ep, fromx, tox, y)emupane *ep; int fromx,tox,y;{
	box b;
	pp = ep;
	b.b_left = CHARLEFT(fromx);
	b.b_right = CHARRIGHT(tox);
	y = y<0?pp->p_ymax+1+y:y;
	b.b_top = CHARTOP(y);
	b.b_bottom = CHARBOT(y);
	return(b);
}
/*
 * move text and length of a saved line to a new line position
 */
PRIVATE void gk0xmoveline(from,to)int from,to;{
	char *top,*fromp;
	int len;

	len = pp->p_length[to] = pp->p_length[from];
	top = &P_TEXT(pp,0,to);
	fromp = &P_TEXT(pp,0,from);
	while(len-->0)*top++ = *fromp++;
}
/*
 * scroll given inclusive region of lines by given number up or down (-ve).
 */
PRIVATE void gk0xscrollregion(from,to,by)int from,to,by;{
	box b;
	int y,ht;

	gk0xoutflush();
	ht = pp->p_height;
	b = gk0xboxzoom(gk0xemucharbox(pp,0,pp->p_xmax,from),-1);
	b.b_bottom = CHARBOT(to)-1;
	if(pp->p_flags&PMEMORY){
		if(by>=0)while(from<=to){
				if(from+by<=to)gk0xmoveline(from+by,from);
				else pp->p_length[from] = 0;
				from++;
			}
		else	while(to>=from){
				if(to+by>=from)gk0xmoveline(to+by,to);
				else pp->p_length[to] = 0;
				to--;
			}
	}
	if(by>=0){
		y = b.b_top;
		b.b_top += by*ht;
		if(b.b_top<b.b_bottom)gk0xbmmove(b,b.b_left,y);
		b.b_top = b.b_bottom - by*ht+1;
	}else{
		by = -by;
		b.b_bottom -= by*ht;
		if(b.b_top<b.b_bottom){
#ifdef RUBBISH
			/* what was I thinking of? No ripple fill with rastering! */
			bm = gk0xbmcopy(NULLPTR(bitmap),b,BMGET|BMTO);
			gk0xbmcopy(bm,boxshift(b,0,by*ht),BMFREE|BMFROM);
#else RUBBISH
			gk0xbmmove(b,b.b_left,b.b_top+by*ht);
#endif RUBBISH
		}
		b.b_bottom = b.b_top + by*ht-1;
	}
	gk0xbmbox(b,BMCLEARALL);
}
#ifdef FORTINTER
FORTINTER wemcbx_(ep,fromx,tox,y,newbox)int *ep,*fromx,*tox,*y,*newbox;{	/* emupane *ep; int fromx,tox,y; box newbox; */
	from_box(emucharbox((emupane *)*ep,*fromx,*tox,*y ),newbox);
}
FORTINTER int wemcre_(b)int *b;{	/* box b; */
	return(int)emucreate(to_box(b));
}
FORTINTER wemflw_(ep)int *ep;{	/* emupane *ep; */
	emufollow((emupane *)*ep);
}
FORTINTER wemget_(ep,b,str,stsize)int *ep,*b,stsize; char *str;{ /* emupane *ep; box b; */
	strncpy(str,emuget((emupane *)*ep,to_box(b)),stsize);
}
FORTINTER wemprt_(ep,str,len)int *ep,*len; char *str;{	/* emupane *ep; */
	emuprint((emupane *)*ep,str,*len );
}
FORTINTER wemrec_(ep,b)char *ep; int *b;{
	emurecreate((emupane *)*ep,to_box(b));
}
FORTINTER wemshw_(ep,b)int *ep; int *b;{	/* emupane *ep; box b; */
	emushow((emupane *)*ep,to_box(b));
}
FORTINTER char *wemtrm_(ep)int *ep;{	/* emupane *ep; */
	return emutermcap((emupane *)*ep );
}
#endif FORTINTER
