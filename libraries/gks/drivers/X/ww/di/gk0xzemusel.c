/* copyright (c) Mark M Martin. RAL. 1986 */
#include "di.h"
/*
 * handle selection of retained text of emu
 */
/*
 * alloc space, or realloc it.
 */
#define REMALL(var,type,size)	var = (type *)remall((char *)var,(unsigned)size)
PRIVATE char *remall(space,size)char *space;unsigned size;{
	if(space!=0)space = realloc(space,size);
	else space = malloc(size);
	if(space==0)gk0xwwpanic("emu couldnt get space to save text");
	return(space);
}
/*
 * If first use after setting PSAVE this will get space and init lengths to 0
 * if oldy is set to -1, oldx==p_xmax.
 */
LOCALPUBLIC void gk0xsetscreensize(pp,oldx,oldy)emupane *pp;int oldx,oldy;{
	int xmax,ymax,yline,col,minline;
	char *from,*to;

	xmax = pp->p_xmax;
	ymax = pp->p_ymax;
	if(xmax<0 || ymax<0){
		if(pp->p_text!=0)free((char*)pp->p_text);
		pp->p_text = 0;
		if(pp->p_length!=0)free((char*)pp->p_length);
		pp->p_length = 0;
		pp->p_flags &= ~PMEMORY;
		return;
	}
	pp->p_flags |= PMEMORY;
	minline = ymax<oldy?ymax:oldy;
	if(xmax<oldx){	/* getting narrower */
		from = to = pp->p_text+xmax+1;	/* start of new 2nd line */
		for(yline=1;yline<=minline;yline++){	/* not line 0 */
			from += oldx-xmax;	/* old start of line */
			for(col=0;col<=xmax;col++)
				*to++ = *from++;
		}
		for(yline=0;yline<=minline;yline++)
		if(pp->p_length[yline]>xmax+1)pp->p_length[yline] = xmax+1;
	}
	/* easy if same width, just alloc more/less space */
	if(xmax!=oldx || oldy!=ymax){
		REMALL(pp->p_text,char,(xmax+1)*(ymax+1));
		REMALL(pp->p_length,int,(sizeof(int))*(ymax+1));
		/* init new line lengths to 0 */
		while(oldy<ymax)pp->p_length[++oldy] = 0;
	}
	if(xmax>oldx){	/* grown wider. start at end of text */
		to = pp->p_text+(xmax+1)*(minline+1);
		from = pp->p_text+(oldx+1)*(minline+1);
		for(yline=minline;yline>0;yline--){	/* not line 0 */
			to += oldx-xmax;	/* old start of line */
			for(col=0;col<=oldx;col++)
				*--to = *--from;
		}
	}

}
typedef struct{
	int po_x,po_y;
}posn;
#define FLAT(a)		((a).po_y*1000+(a).po_x)
/*
 * return posn of cursor in characters. 0=> left of first, UNSET if out of box.
 * Greater than rightmost char => rightmost char+1 for assumed newline (==len of string+1)
 * unless at right margin
 */
PRIVATE posn emuposn(pp)emupane *pp;{
	int len,newline;
	posn p;

	p.po_x = (dd->d_x-pp->p_box.b_left+pp->p_width/2-1)/pp->p_width;
	p.po_y = (dd->d_y-pp->p_box.b_top)/pp->p_height;
	if(p.po_y<0 || p.po_y>pp->p_ymax)
		p.po_x = UNSET;
	else{
		len = pp->p_length[p.po_y];
		newline = len<pp->p_xmax+1;	/* to right margin: no implied newline */
		if(p.po_x<0 || p.po_x>pp->p_xmax)p.po_x = UNSET;
		else if(p.po_x>len+newline)p.po_x = len+newline;
	}
	return(p);
}
/*
 * ensure box fits within pane's constraints and is ordered
 */
PRIVATE void rationalise(pp,bp)emupane *pp;box *bp;{
	int save;
	if(bp->b_top<0)bp->b_top = bp->b_left = 0;
	else if(bp->b_top>pp->p_ymax)bp->b_top = pp->p_ymax, bp->b_left = pp->p_xmax+1;
	if(bp->b_bottom<0)bp->b_bottom = bp->b_right = 0;
	else if(bp->b_bottom>pp->p_ymax)bp->b_bottom = pp->p_ymax, bp->b_right = pp->p_xmax+1;
	if(bp->b_left<0)bp->b_left = 0;
	if(bp->b_right<0)bp->b_right = 0;
	if(bp->b_top>bp->b_bottom){	/* swop to get in order */
		save = bp->b_top;
		bp->b_top = bp->b_bottom;
		bp->b_bottom = save;
		save = bp->b_left;
		bp->b_left = bp->b_right;
		bp->b_right = save;
	}else if(bp->b_top==bp->b_bottom && bp->b_left>bp->b_right){
		save = bp->b_left;
		bp->b_left = bp->b_right;
		bp->b_right = save;
	}
}
/*
 * return the text between the given points (box corners), in char positions.
 * Caller should free(the null terminated string returned). It may be a null pointer 0, but should
 * never be a pointer to a null string (len 0).
 */
PUBLIC char *gk0xemuget(pp,b)emupane *pp;box b;{
	int len,totlen,i,newline;
	char *cp,*mem,*source;

	if(!(pp->p_flags & PSAVE)){
		pp->p_flags |= PSAVE;
		gk0xsetscreensize(pp,pp->p_xmax,-1);	/* same x, new y. get same ymax/scrollmax */
		return((char *)0);	/* just started: nowt saved */
	}
	if(!(pp->p_flags & PMEMORY)|| b.b_left==-1 || pp->p_xmax<0)return(0);
	rationalise(pp,&b);
	/* go through once to find how much to malloc */
	totlen = 0;
	for(i=b.b_top;i<=b.b_bottom;i++){
		len = pp->p_length[i];
		newline = len<pp->p_xmax+1;	/* to right margin: no implied newline */
		if(i==b.b_bottom && b.b_right>len+newline)b.b_right = len+newline;
		if(i==b.b_top && b.b_left>=len+newline)continue;
		if(b.b_top==b.b_bottom && b.b_left==b.b_right)break;
		if(i==b.b_bottom && b.b_right<=len)len = b.b_right, newline = FALSE;
		if(i==b.b_top)len -= b.b_left;
		if(len>=0)totlen += len+newline;
	}
	if(totlen==0)return((char *)0);
	mem = cp = malloc(++totlen);	/* 1 for null */
	if(mem==0)gk0xwwpanic("gk0xemuget: cannot get space");
	for(i=b.b_top;i<=b.b_bottom;i++){
		len = pp->p_length[i];
		newline = len<pp->p_xmax+1;
		if(i==b.b_top && b.b_left>=len+newline)continue;
		source = &P_TEXT(pp,0,i);
		if(i==b.b_bottom && b.b_right<=len)len = b.b_right, newline = FALSE;
		if(i==b.b_top)len -= b.b_left, source += b.b_left;
		while(len-->0)*cp++ = *source++;
		if(newline)*cp++ = '\n';
	}
	*cp++ = '\0';
	if(totlen!=cp-mem)gk0xwwpanic("gk0xemuget: didnt alloc correct space");
	return(mem);
}
/*
 * show (or remove) the selection by underline and barring ends as usual
 */
PRIVATE void gk0xshow(pp)emupane *pp;{
	box edges,b,uline;
	int y,fromx,tox;

	if(pp->p_xmax<0)return;
	b = pp->p_select;
	for(y = b.b_top;y<=b.b_bottom;y++){
		fromx = y==b.b_top?b.b_left:0;
		if(fromx>pp->p_length[y]+1)fromx = pp->p_length[y]+1;
		if(fromx>pp->p_xmax+1)fromx = pp->p_xmax+1;
		if(y==b.b_bottom){
			tox = b.b_right;	/* may be 0. => -1 in emucharbox */
			if(tox>pp->p_length[y]+1)tox = pp->p_length[y]+1;
		}else tox = pp->p_length[y]+1;
		if(tox>pp->p_xmax+1)tox = pp->p_xmax+1;
		/* beware emucharbox returns right edges of tox char so need -1.
		   wont use tox edge later on if tox==fromx (ie tox was 0) */
		uline = edges = gk0xboxzoom(gk0xemucharbox(pp,fromx,tox-1,y),-1);
		uline.b_top = uline.b_bottom;
		gk0xbmbox(uline,BMNOTALL);	/* underline */
		if(y==b.b_top){
			uline = edges;
			uline.b_right = uline.b_left+1;
			gk0xbmbox(uline,BMNOTALL);	/* left bar, or only one */
		}
		if(y==b.b_bottom && (fromx!=tox || y!=b.b_top)){
			edges.b_left = edges.b_right-1;
			gk0xbmbox(edges,BMNOTALL);	/* right bar if not of 0 len */
		}
	}
}
/*
 * set highlight, removing old if nec. box.b_left == -1 for none
 */
PUBLIC void gk0xemushow(pp,b)emupane *pp;box b;{
	int saveline;

	if(!(pp->p_flags & PMEMORY))return;
	gk0xwwstack(WWPUSHOFF);
	saveline = dd->d_line;
	dd->d_line = WWXOR;
	if(pp->p_select.b_left!=-1)gk0xshow(pp);	/* remove */
	if(b.b_left!=-1){
		rationalise(pp,&b);
		pp->p_select = b;
		gk0xshow(pp);
	}else pp->p_select.b_left = -1;
	dd->d_line = saveline;
	gk0xwwstack(WWPOP);
}
