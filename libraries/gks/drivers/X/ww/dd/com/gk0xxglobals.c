/* copyright (c) Mark M Martin. RAL. 1988 */
#include "dd.h"
/*
 * space for externs common to all
 */
PUBLIC wwstate	*dd;
PUBLIC window	*ddwin;
PUBLIC bitmap	*ddbm;
PUBLIC fontinfo	*ddfont;
PUBLIC treeinfo *treebase;
DDPUBLIC jwwstate	*jdd;
box noclipbox = {0,0,WWNOCLIP,WWNOCLIP};
