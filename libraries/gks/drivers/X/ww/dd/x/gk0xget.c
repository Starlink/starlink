/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
PUBLIC window *gk0xwwxget(size,colours,label,flags) box size;int colours,flags; char *label;{
	window *newwin;
	jwindow *jwp;

	if(flags & ~(WWSIZEFROMUSER|WWOPENICONISED|WWINTERNALSIZE|WWREPORTKILL))gk0xwwpanic("gk0xwwxget: bad flags");
	if(!gk0xwwstartup(colours))return(0);
	newwin = gk0xwingetx(size,label,flags);
	jwp = jwin(newwin);
	if(ddwin==0)ddwin = newwin, ddbm = newwin->w_bm;
	return(newwin);
}
