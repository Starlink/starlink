/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
DDPUBLIC window *gk0xwinalloc(size)box size;{
	window *newwin;
	jwindow *jwp;

	newwin = STRUCTMALLOC(window);
	jwp = STRUCTMALLOC(jwindow);
	newwin->w_junk = (int *)jwp;
	newwin->w_fnbox.b_left = newwin->w_fnbox.b_right;
	newwin->w_xrel = size.b_left;
	newwin->w_yrel = size.b_top;
	jwp->jw_cstack.s_width = sizeof(jcursor);
	newwin->w_bm = gk0xwwbmget(WIDTH(size),HEIGHT(size));
	newwin->w_bm->bm_window = newwin;
	newwin->w_bm->bm_colours = MONOCHROME;
#if defined(WWFORWC)|defined(WWFORSUN)
	jwp->jw_update.b_left = UNSET;
#endif defined(WWFORWC)|defined(WWFORSUN)
	jwp->jw_nextwindow = HEADWIN;	/* add to head of list of windows */
	HEADWIN = newwin;
	return(newwin);
}
DDPUBLIC gk0xwwseticonpic(bm)bitmap *bm;{
	static bitmap *icon = 0;
	static char defaulticon[]=
{ /* lines= */ 0,46, /* width= */ 0,55,
0,0,0,01,0300,0,0,
0,0,0,02,040,0,0,
0,0,0,04,020,0,0,
0,0,0,04,020,0,0,
0,0,017,0204,020,0,0,
0,0,020,0102,040,0,0,
0,0,040,041,0300,0,0,
0,0,0100,020,0,0,0,
0,0,0100,020,0,0,0,
0,0,0100,020,0,0,0,
0,0,0100,020,0,0,0,
0,0,0100,020,0,0,0,
0,0,040,040,0,0,0,
0,0,020,0100,0,0,0,
0,0,017,0200,07,0360,0,
0,017,0340,0,070,016,0,
0,0160,034,0,0100,01,0,
01,0200,03,01,0200,0,0300,
02,0,0,0202,0,0,040,
04,0,0,0104,0,0,020,
010,0,0,044,0,0,020,
020,0,0,030,0,01,010,
040,0,0,030,0100,01,04,
040,0,0,030,0100,01,04,
0102,0,0,024,040,02,04,
0102,0,0,0244,040,02,02,
0102,0,0,0244,040,0202,02,
0202,0,01,042,020,0202,02,
0201,04,01,042,021,0102,02,
0201,04,01,042,021,0104,02,
0201,06,02,042,021,044,02,
0200,0212,02,042,012,044,02,
0200,0211,02,022,012,024,04,
0200,0211,04,022,012,030,04,
0100,0110,0204,024,04,010,04,
0100,0110,0204,014,04,010,010,
0100,0120,0210,04,0,0,020,
040,0120,0110,014,0,0,020,
040,060,0110,012,0,0,040,
020,040,060,021,0200,0,0300,
010,0,060,040,0100,01,0,
04,0,0,0100,070,016,0,
02,0,0,0200,07,0360,0,
01,0200,03,0,0,0,0,
0,0160,034,0,0,0,0,
0,017,0340,0,0,0,0}; /* ww style */
	if(icon==0){
#ifdef WWFORSUN
		int savecols;
		savecols = dd->d_colours;
		dd->d_colours = bm->bm_colours;
#endif WWFORSUN
		icon = gk0xbmdecode(defaulticon,ENWWSTYLE);
#ifdef WWFORSUN
		dd->d_colours = savecols;
#endif WWFORSUN
	}
	gk0xbmxcopy(icon,icon->bm_box,bm,
	  gk0xboxshift(icon->bm_box,(bm->bm_box.b_right-icon->bm_box.b_right)/2,
	  	(bm->bm_box.b_bottom-icon->bm_box.b_bottom)/2),
	  WWCOPY);
}
