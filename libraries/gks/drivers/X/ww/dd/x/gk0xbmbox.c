/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
PUBLIC void gk0xbmxbox(bm,b,flags) bitmap *bm; box b; int flags;{
	jbitmap *jbp;
	int opn;

	if(bm==0)wwfail("null bitmap to bmbox",);
	if(flags & ~(BMEDGES|BMCLEAR|BMNOTALL|BMNOT|BMCLEARALL))
		wwfail("unknown flag to bmbox",);
	jbp = 	jbm(bm);
	setgcclip(jbp->jbm_boxclip);
	if(flags&(BMCLEARALL|BMNOTALL)){
		opn = (flags&(BMCLEARALL|BMNOTALL))==(BMCLEARALL|BMNOTALL) ? GXset :
			(flags&BMCLEARALL) ? GXclear : GXinvert;
		gk0xsetbmrop(opn,bm);
		if(jdd->jd_flags&JCOPYAREA)XCopyArea(WXD,jbp->jbm_drawable,jbp->jbm_drawable,WXGC,b.b_left,b.b_top,WIDTH(b),HEIGHT(b),b.b_left,b.b_top);
		else XFillRectangle(WXD,jbp->jbm_drawable,WXGC,b.b_left,b.b_top,WIDTH(b),HEIGHT(b));
	}
 	if(flags&(BMCLEAR|BMNOT)){
		opn = (flags&(BMCLEAR|BMNOT))==(BMCLEAR|BMNOT) ? GXset :
			(flags&BMCLEAR) ? GXclear : GXinvert;
		gk0xsetbmrop(opn,bm);
		if(jdd->jd_flags&JCOPYAREA)XCopyArea(WXD,jbp->jbm_drawable,jbp->jbm_drawable,WXGC,b.b_left+1,b.b_top+1,WIDTH(b)-2,HEIGHT(b)-2,b.b_left+1,b.b_top+1);
		else XFillRectangle(WXD,jbp->jbm_drawable,WXGC,b.b_left+1,b.b_top+1,WIDTH(b)-2,HEIGHT(b)-2);
	}
	if(flags&BMEDGES){
		bitmap *save;
		save = ddbm;
		ddbm = bm;
		gk0xline(b.b_left,b.b_top,LNMOVEABS);
		gk0xline(b.b_right,b.b_top,LNDRAWABS);
		gk0xline(b.b_right,b.b_bottom,LNDRAWABS);
		gk0xline(b.b_left,b.b_bottom,LNDRAWABS);
		gk0xline(b.b_left,b.b_top,LNDRAWABS);
		ddbm = save;
	}
	NOTESCREEN(bm,b);
}
