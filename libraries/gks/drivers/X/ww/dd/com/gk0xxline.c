/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
static int presx,presy;
PUBLIC void gk0xline(x,y,flag) int x,y,flag;{
	switch(flag){
	case LNDRAWREL:
		gk0xbmdraw(presx,presy,presx+x,presy+y,ddbm,dd->d_line);
		/* no break */
	case LNMOVEREL:
		presx += x, presy += y;
		break;
	case LNDRAWABS:
		gk0xbmdraw(presx,presy,x,y,ddbm,dd->d_line);
		/* no break */
	case LNMOVEABS:
		presx = x, presy = y;
		break;
		break;
	default:
		wwfail("unknown flag to line",);
	}
}
typedef struct{
	int xx,yy;
}pair;
static stack stackline = sizeof(pair);
PUBLIC void gk0xlnstack(flag) int flag;{
	pair here;
	if(flag==WWPUSH){
		here.xx = presx, here.yy = presy;
		STACKPUSH(stackline,here);
	}else if(flag==WWPOP){
		STACKPOP(stackline,here);
		presx = here.xx, presy = here.yy;
	}
}
#ifdef FORTINTER
FORTINTER wlnstk_(flag)int *flag;{
	lnstack(*flag);
}
#endif FORTINTER
#ifdef FORTINTER
FORTINTER wline_(x,y,flag)int *x,*y,*flag;{
	line(*x,*y,*flag);
}
#endif FORTINTER
