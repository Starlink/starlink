/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
/*
 * values in jw_isoff:
 * FALSE: all rasterops to be echoed to window.
 * TRUENOCHANGES: isoff is true, but no rasterops done to window bitmap yet
 * TRUECHANGED: some changes need to be written to window.
 */
#define TRUENOCHANGES	1
#define TRUECHANGED	2
static stack stackww = sizeof(int);
PUBLIC void gk0xwwstack(flag) int flag;{
	gk0xwwxstack(ddwin,flag);
}
PUBLIC void gk0xwwxstack(win,flag)window *win; int flag;{
	jwindow *jwp;
	int state;
	if(win==0)return;
	jwp = jwin(win);
	switch(flag){
	case WWPUSH:
	case WWPUSHOFF:
	case WWPUSHON:
		STACKPUSH(stackww,jwp->jw_isoff);
		if(!(dd->d_flags&WWNOBUFFER))
			gk0xsetsc(jwp,flag!=WWPUSHON);
		else if(jwp->jw_isoff)gk0xsetsc(jwp,TRUE);
		break;
	case WWPOP:
		STACKPOP(stackww,state);
		if(!(dd->d_flags&WWNOBUFFER))
			gk0xsetsc(jwp,state);
			/* TRUECHANGED is equiv to TRUENOCHANGES */
		else if(jwp->jw_isoff)gk0xsetsc(jwp,TRUE);
		break;
	default:
		wwfail("gk0xwwxstack: unknown flag",);
	}
}
#ifdef FORTINTER
FORTINTER wwwstk_(flags)int *flags;{
	wwstack(*flags);
}
#endif FORTINTER
static stack stackwin = sizeof(window *);
PUBLIC void gk0xwinstack(flag) int flag;{
	switch(flag){
	case WWPUSH:
		STACKPUSH(stackwin,ddwin);
		break;
	case WWPOP:
		STACKPOP(stackwin,ddwin);
		break;
	default:
		wwerror("unknown flag to winstack");
	}
}
#ifdef FORTINTER
FORTINTER winsta_(flag)int *flag;{	/* int flag; */
		winstack(*flag);
}
#ifdef OLDFORTRAN
FORTINTER winstk_(win,flag)int *win,*flag;{
 	if(*flag == WWSET)
 		ddwin =(window *)*win;
	else
		winstack(*flag);
}
#endif OLDFORTRAN
#endif FORTINTER
static stack stackbm = sizeof(bitmap *);
PUBLIC void gk0xbmstack(flag) int flag;{
	switch(flag){
	case WWPUSH:
		STACKPUSH(stackbm,ddbm);
		break;
	case WWPOP:
		STACKPOP(stackbm,ddbm);
		break;
	default:
		wwfail("unknown flag to bmstack",);
	}
}
#ifdef FORTINTER
FORTINTER void wbmstk_(flags)int *flags;{
	bmstack(*flags);
}
#endif FORTINTER
/*
 * stack the font
 */
static stack stackft = sizeof(fontinfo *);
PUBLIC void gk0xftstack(flag) int flag;{
	switch(flag){
	case WWPUSH:
		STACKPUSH(stackft,ddfont);
		break;
	case WWPOP:
		STACKPOP(stackft,ddfont);
		break;
	default:
		wwfail("unknown flag to ftstack",);
	}
}
#ifdef FORTINTER
FORTINTER wftstk_(flag)int *flag;{
	ftstack(*flag);
}
#endif FORTINTER
/*
 * stack has a pointer s_top to start of last item  (ie top of stack).
 * if null then stack is empty.
 * s_memory points to start of whole of stack (got space).
 * it never shrinks, but grows for each item when full.
 * it is null at the start also.
 * s_end points to end+1 of whole of got s_memory space. (may be beyond top
 * of stack now). It is the max ever excursion+1.
 */
LOCALPUBLIC void gk0xstackpush(sp,val)stack *sp;char *val;{
	unsigned int len;
	char *to;
	if(sp->s_width==0)gk0xwwpanic("stack width 0");
	if(sp->s_top==NULLPTR(char) && sp->s_memory!=NULLPTR(char))
		sp->s_top = sp->s_memory;
	else{
		sp->s_top += sp->s_width;
		if(sp->s_top>=sp->s_end){
			len = sp->s_end - sp->s_memory + sp->s_width;
			if(sp->s_memory==0)
				sp->s_memory = malloc((unsigned)len);
			else sp->s_memory = realloc(sp->s_memory,(unsigned)len);
			if(sp->s_memory==0)
				gk0xwwpanic("stack oflo (no space)");
			sp->s_end = sp->s_memory + len;
			sp->s_top = sp->s_end - sp->s_width;
		}
	}
	for(to = sp->s_top, len = sp->s_width; len--!=0 ;)
		*to++ = *val++;
}
LOCALPUBLIC void gk0xstackpop(sp,val)stack *sp;char *val;{
	char *from;
	unsigned int len;
	if(sp->s_width==0)gk0xwwpanic("stack width 0");
	if(sp->s_top==NULLPTR(char))gk0xwwpanic("stack unflo");
	for(from = sp->s_top, len = sp->s_width; len--!=0 ;)
		*val++ = *from++;
	if(sp->s_top<=sp->s_memory)sp->s_top = NULLPTR(char);
	else	sp->s_top -= sp->s_width;
}
#if defined(WWFORPERQ2) | defined(SUNQUEUE)
/*
 * with queues, there is a circle of items.
 * s_top points to start of queue, and items are
 * added to the right (higher addresses).
 * it is null if queue empty.
 * s_bottom points to last item added to end of queue.
 * it is not to be looked at if queue is empty.
 * s_top==s_bottom when there is one item on the queue.
 * When adding, if the queue is empty simply put item at start.
 * else, put at bottom+1. But if this is ==top, then queue full.
 * So, increase memory allocation, and insert gap between bottom
 * and top pointers, shifting right. If gap would be at extreme left
 * dont bother, instead use gap already at extreme right.
 */
LOCALPUBLIC void enqueue(sp,val)stack *sp;char *val;{
	unsigned int len;
	char *to,*oldmem,*from;
	if(sp->s_width==0)gk0xwwpanic("queue width 0");
	if(sp->s_top==NULLPTR(char) && sp->s_memory!=NULLPTR(char))
		sp->s_top = sp->s_bottom = sp->s_memory;	/* empty q that's been used before */
	else{
		sp->s_bottom += sp->s_width;
		if(sp->s_bottom>=sp->s_end)
			sp->s_bottom = sp->s_memory;	/* wrap */
		if(sp->s_top==NULLPTR(char) || sp->s_top==sp->s_bottom){	/* unused or full */
			len = sp->s_end - sp->s_memory + sp->s_width;
			oldmem = sp->s_memory;
			if(sp->s_memory==NULLPTR(char))
				sp->s_memory = malloc(len);
			else sp->s_memory = realloc(sp->s_memory,len);
			if(sp->s_memory==NULLPTR(char))
				gk0xwwpanic("queue oflo (no space)");
			sp->s_end = sp->s_memory + len;
			if(oldmem==NULLPTR(char))
				sp->s_top = sp->s_bottom = sp->s_memory;
			else{
				sp->s_bottom = sp->s_top += sp->s_memory-oldmem;	/* relocate ptr */
				if(sp->s_bottom==sp->s_memory)	/* gap is at front, make it at end! */
					sp->s_bottom = sp->s_end - sp->s_width;
				else{	/* shift right to get gap between bottom and top ptrs */
					for(to = sp->s_end, from = to-sp->s_width;from>sp->s_top;)
						*--to = *--from;
					sp->s_top += sp->s_width;
				}
			}
		}
	}
	for(to = sp->s_bottom, len = sp->s_width; len--!=0 ;)
		*to++ = *val++;
}
LOCALPUBLIC void dequeue(sp,val)stack *sp;char *val;{
	char *from;
	unsigned int len;
	if(sp->s_width==0)gk0xwwpanic("queuewidth 0");
	if(sp->s_top==NULLPTR(char))gk0xwwpanic("queue unflo");
	for(from = sp->s_top, len = sp->s_width; len--!=0 ;)
		*val++ = *from++;
	if(sp->s_top==sp->s_bottom)sp->s_top = NULLPTR(char);	/* empty */
	else{
		sp->s_top += sp->s_width;	/* next */
		if(sp->s_top>=sp->s_end)sp->s_top = sp->s_memory;	/* wrap */
	}
}
#ifdef WANTUNDOQUEUE
/*
 * remove item last added to queue. not much of a queue when you can take
 * away at both ends
 */
LOCALPUBLIC void undoqueue(sp,val)stack *sp;char *val;{
	char *from;
	unsigned int len;
	if(sp->s_width==0)gk0xwwpanic("queuewidth 0");
	if(sp->s_top==NULLPTR(char))gk0xwwpanic("queue unflo");
	for(from = sp->s_bottom, len = sp->s_width; len--!=0 ;)
		*val++ = *from++;
	if(sp->s_top==sp->s_bottom)sp->s_top = NULLPTR(char);	/* empty */
	else{
		if(sp->s_bottom<=sp->s_memory)sp->s_bottom = sp->s_end;	/* wrap */
		sp->s_bottom -= sp->s_width;	/* previous */
	}
}
#endif WANTUNDOQUEUE
#endif defined(WWFORPERQ2) | defined(SUNQUEUE)
