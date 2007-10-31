/*
 * $Id: ncio.c,v 1.1 2005/07/15 21:56:39 andy Exp $
 */

#if defined(_CRAY)
#   include "ffio.c"
#else
#   include "posixio.c"
#endif
