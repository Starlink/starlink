/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
#include <signal.h>
PRIVATE void ipread();
PRIVATE void ipon();
PRIVATE void ipoff();
PUBLIC void gk0xipset(flag) int flag;{
	gk0xipxset(ddwin,flag);
}
PUBLIC int gk0xipwait(){
	if(dd->d_ipwait==0)gk0xwwpanic("d_ipwait 0: no ipset on?");
	if(dd->d_ipwait==gk0xipwait)gk0xwwpanic("d_ipwait==gk0xipwait. should be gk0xipxwait?");
	return ((*dd->d_ipwait)());
}
#ifdef FORTINTER
FORTINTER int wipwat_(){
	ipwait();
	return dd->d_event;
}
FORTINTER wipset_(flag)int *flag;{
	ipset(*flag);
}
#endif FORTINTER
