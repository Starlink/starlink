/* copyright (c) Mark M Martin. RAL. 1986 */
#include "di.h"
/*
 * shift: move the box by x and y
 */
PUBLIC box gk0xboxshift(b,x,y) box b; int x,y;{
	b.b_top +=y;
	b.b_bottom +=y;
	b.b_left += x;
	b.b_right += x;
	return(b);
}
#ifdef FORTINTER
FORTINTER wbxshf_(b,x,y,newbox)int *x,*y;int *b,*newbox;{
	from_box(boxshift(to_box(b),*x,*y),newbox);
}
#endif FORTINTER
/*
 * inside: return true if point x,y is inside the box.
 *  The top left edges are inside the box, the bottom right are not.
 */
PUBLIC int gk0xboxinside(b,x,y) box b; int x,y;{
	return(x>=b.b_left && x<b.b_right
	    && y>=b.b_top && y<b.b_bottom);
}
#ifdef FORTINTER
FORTINTER int wbxins_(b,x,y)int *b;int *x,*y;{
	return boxinside(to_box(b),*x,*y);
}
#endif FORTINTER
/*
 * zoom: enlarge (>0) or reduce the box size equally in all dirns
 */
PUBLIC box gk0xboxzoom(b,dist) box b; int dist;{
	b.b_left -= dist;
	b.b_top -= dist;
	b.b_right += dist;
	b.b_bottom += dist;
	return(b);
}
#ifdef FORTINTER
FORTINTER wbxzom_(b,dist,newbox)int *b,*newbox;int *dist;{
	from_box(boxzoom(to_box(b),*dist),newbox);
}
#endif FORTINTER
/*
 * build: into a box.
 */
PUBLIC box gk0xboxbuild(left,top,right,bottom) int left,top,right,bottom;{
	box b;
	b.b_left = left;
	b.b_right = right;
	b.b_top = top;
	b.b_bottom = bottom;
	return(b);
}
#ifdef FORTINTER
FORTINTER wbxbld_(left,top,right,bottom,newbox)int *left,*right,*top,*bottom;int *newbox;{
	from_box(boxbuild(*left,*top,*right,*bottom),newbox);
}
#endif FORTINTER
/*
 * BOXINTERSECT: return common box between two boxes. if none return 0,0,0,0
 * BOXCENTRE:	return first box when centred in 2nd box, with reduced size if
 *		2nd box smaller
 * BOXENCLOSING: smallest box enclosing both boxes
 * BOXREDUCE:	reduce a to the size of b, unless is it smaller already.
 * BOXADD:	offset first box by 2nd box top/left.
 * BOXBOUNCE:	return first box moved so as to be inside (or=) 2nd box, reduced if nec.
 * BOX??LAP:	return 2nd box moved so as to overlap 1st as much as possible,
 *		but leaving a border so that either can be on top & picked.
 */
#define OVERLAP	10	/* num pixels not in common for BOXOVERLAP */
PUBLIC box gk0xboxop(a,b,flag)box a,b; int flag;{
	int width,height,xlap,ylap;
	switch(flag){
	case BOXINTERSECT:
		if(a.b_left < b.b_left)a.b_left = b.b_left;
		if(a.b_right > b.b_right)a.b_right = b.b_right;
		if(a.b_left>a.b_right){
			a.b_left = a.b_right = a.b_top = a.b_bottom = 0;
			break;
		}
		if(a.b_top < b.b_top)a.b_top = b.b_top;
		if(a.b_bottom > b.b_bottom)a.b_bottom = b.b_bottom;
		if(a.b_top>a.b_bottom){
			a.b_left = a.b_right = a.b_top = a.b_bottom = 0;
			break;
		}
		break;
	case BOXREDUCE:
		width = b.b_right-b.b_left;
		height = b.b_bottom-b.b_top;
		if(a.b_right-a.b_left>width)a.b_right = a.b_left+width;
		if(a.b_bottom-a.b_top>height)a.b_bottom = a.b_top+height;
		break;
	case BOXCENTRE:
		a = gk0xboxshift(a,((b.b_right+b.b_left)-(a.b_right+a.b_left))/2,
			((b.b_bottom+b.b_top)-(a.b_bottom+a.b_top))/2);
		a = gk0xboxop(a,b,BOXINTERSECT);
		break;
	case BOXENCLOSING:
		if(a.b_left > b.b_left)a.b_left = b.b_left;
		if(a.b_right < b.b_right)a.b_right = b.b_right;
		if(a.b_top > b.b_top)a.b_top = b.b_top;
		if(a.b_bottom < b.b_bottom)a.b_bottom = b.b_bottom;
		break;
	case BOXADD:
		a = gk0xboxshift(a,b.b_left,b.b_top);
		break;
	case BOXBOUNCE:
		if(a.b_right>b.b_right){
			a.b_left -= a.b_right-b.b_right;
			a.b_right = b.b_right;
			if(a.b_left<b.b_left)a.b_left = b.b_left;
		}else if(a.b_left<b.b_left){
			a.b_right -= a.b_left-b.b_left;
			a.b_left = b.b_left;
			if(a.b_right>b.b_right)a.b_right = b.b_right;
		}
		if(a.b_bottom>b.b_bottom){
			a.b_top -= a.b_bottom-b.b_bottom;
			a.b_bottom = b.b_bottom;
			if(a.b_top<b.b_top)a.b_top = b.b_top;
		}else if(a.b_top<b.b_top){
			a.b_bottom -= a.b_top-b.b_top;
			a.b_top = b.b_top;
			if(a.b_bottom>b.b_bottom)a.b_bottom = b.b_bottom;
		}
		break;
	case BOXBRLAP:
	case BOXTLLAP:
	case BOXBLLAP:
	case BOXTRLAP:
		width = b.b_right-b.b_left;
		height = b.b_bottom-b.b_top;
		xlap = (flag==BOXBRLAP || flag==BOXTRLAP)?OVERLAP:-OVERLAP;
		ylap = (flag==BOXBRLAP || flag==BOXBLLAP)?OVERLAP:-OVERLAP;
		b.b_left = a.b_right+xlap-width;
		b.b_top = a.b_bottom+ylap-height;
		/* use ?lap as a multiplier only for the sign:
		   read as: if(xlap>0)if(b.b_left<a.b_left+xlap)...; else > */
		if(0<xlap*(a.b_left+xlap-b.b_left))b.b_left = a.b_left+xlap;
		if(0<ylap*(a.b_top+ylap-b.b_top))b.b_top = a.b_top+ylap;
		b.b_right = b.b_left+width;
		b.b_bottom = b.b_top+height;
		a = b;
		break;
	default:
		gk0xwwpanic("gk0xboxop: unknown flag\n");
		break;
	}
	return(a);
}
#ifdef FORTINTER
FORTINTER wbxop_(a,b,flag,newbox)int *a,*b,*flag,*newbox;{	/* box a,b,newbox; int flag; */
	from_box(boxop(to_box(a),to_box(b),*flag),newbox );
}
#endif FORTINTER
