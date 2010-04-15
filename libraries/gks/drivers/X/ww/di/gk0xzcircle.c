/* copyright (c) Mark M Martin. RAL. 1986 */
#include "di.h"
/*
 * circle drawing. Apr 86
 * convert the points for one eighth to non-overlapping
 * rastered boxes, big horizontal ones from middle out
 * and little horiz ones, one pixel high, at top and bottom moving in
 * x always decreases by one in each call. from always=last to+1.
 * to>=from always.
 */
static int origx,origy,how,edges;
PRIVATE eachbox(l,t,r,b){
	gk0xbmbox(gk0xboxbuild(l+origx,t+origy,r+origx,b+origy),how);
}
/*
 * draw vert line from (l,t) to (l,b) and ditto for -l
 */
PRIVATE xline(l,t,b){
	gk0xline(l+origx,t+=origy,LNMOVEABS);
	gk0xline(l+origx,b+=origy,LNDRAWABS);
	gk0xline(-l+origx,t,LNMOVEABS);	/* -ve x */
	gk0xline(-l+origx,b,LNDRAWABS);
}
/*
 * draw horiz line from (l,b) to (r,b) and ditto for -b
 */
PRIVATE yline(l,r,b){
	gk0xline(l+=origx,b+origy,LNMOVEABS);
	gk0xline(r+=origx,b+origy,LNDRAWABS);
	gk0xline(l,-b+origy,LNMOVEABS);	/* -ve y */
	gk0xline(r,-b+origy,LNDRAWABS);
}
PRIVATE VertLine(x,from,to){
	if(edges){
		if(from==0){	/* first call special case */
			xline(x,-to,to);	/* +ve x */
			yline(-to,to,x);
		}else{
			xline(x,from,to);	/* +ve x +ve y */
			xline(x,-to,-from);	/* +ve x -ve y */
			if(x!=to){
				yline(from,to,x);	/* +ve x +ve y */
				yline(-to,-from,x);	/* -ve x +ve y */
			}
		}
	}else{
		if(from==0)	/* first call special case */
			eachbox(-x,-to,x,to);
		else{
			eachbox(-x,from,x,to);	/* big horiz one below centre */
			eachbox(-x,-to,x,-from);	/* above centre. mirror */
		}
		if(x!=to){
			eachbox(-to,x,to,x);
			eachbox(-to,-x,to,-x);
		}
	}
}
/*
 * rasterop a solid circle centred at x,y of given radius
 * using rasterop rop=BMNOT or BMCLEAR etc which could have BMEDGES set
 */
PUBLIC void gk0xbmcircle(bm,x,y,radius,flags)bitmap *bm;int x,y,radius,flags;{
	if(flags & ~(BMEDGES|BMCLEAR|BMNOTALL|BMNOT|BMCLEARALL))
		wwfail("unknown flag to xxcircle",);
	radius = abs(radius);
	if(radius!=0){
		window *savewin;
		bitmap *savebm;
		savebm = ddbm;
		ddbm = bm;
		origx = x, origy = y, edges = FALSE;
		if(bm->bm_window!=0){
			savewin = ddwin;
			ddwin = bm->bm_window;
			gk0xwwstack(WWPUSHOFF);
		}
		if(flags&(BMCLEARALL|BMNOTALL)){
			how = flags&(BMCLEARALL|BMNOTALL);
			Arc(radius);
		}
		if(flags&(BMCLEAR|BMNOT)){
			how = (flags&BMCLEAR)?BMCLEARALL:0;
			if(flags&BMNOT)how |= BMNOTALL;
			if(--radius!=0)Arc(radius);
		}
		if(flags&BMEDGES){
			edges = TRUE;	/* how irrelevant */
			Arc(radius);
		}
		if(bm->bm_window!=0){
			gk0xwwstack(WWPOP);
			ddwin = savewin;
		}
		ddbm = savebm;
	}
}
/*
 * Scan conversion procedures to generate the set of points
 * which approximate the respective curve. A S Williams 20 Oct 81
 * Uses Bresenham's algorithm to compute pixel coordinates
 * which approximate a circle  centre (0,0) in the first octant
 * (E to SE) ie from (Radius,0) to (Radius/Sqrt(2),Radius/Sqrt(2).
 *
 * translated to C. Mark Martin @ RAL Jan 86.
 * and made to do whole solid circle. Apr 86

		|   /\
		|  /  \   <-----generates this octant only
		| /    |
	  ------|/-----|
		|
		|
		|

 */
PRIVATE Arc(Radius){
	int s,Curx,Cury,linestart;
	Curx = Radius;
	Cury = 0;
	s = Radius-2*Curx+2;	/* +2 found empirically by mmm to give better tiny circles */
	linestart = Cury;
	while(Cury<=Curx){
		/* Point(Curx,Cury); */
		s += 2*Cury-1;
		Cury++;
		if(s>=0){
			VertLine(Curx,linestart,Cury-1);
			linestart = Cury;
			s -= 2*Curx+2;
			Curx--;
		}
	}
	if(linestart<=Curx)VertLine(Curx,linestart,Cury-1);
}
#ifdef NEVER
/*
 * Uses Bresenham's algorithm to compute the pixel coordinates which
 * approximate the straight line from (0,0) to (dx,dy). Other starting
 * positions can obviously be achieved by applying a translation
 * in the routine Point
 */
PRIVATE LineSegment(dx,dy){
	int x,y,e,i;
	x = 0;
	y = 0;
	e = dx/2;
	i = dx;
	while(i>=0){
		/* Point(x,y); */
		e = e+dy;
		x = x+1;
		if(e>dx){
			e = e-dx;
			y = y+1;
		}
		i = i-1;
	}
}
#endif NEVER
#ifdef FORTINTER
FORTINTER wbmcrc_(bm,x,y,radius,flags)int *bm,*x,*y,*radius,*flags;{	/* bitmap *bm; int x,y,radius,flags; */
	 bmcircle((bitmap *)*bm,*x,*y,*radius,*flags);
}
#endif FORTINTER
