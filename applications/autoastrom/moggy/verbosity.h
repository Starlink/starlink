/* Part of moggy
 * Copyright 2001 Council for the Central Laboratory of the Research Councils.
 * See file LICENCE for conditions.
 *
 * $Id$
 */

/* Since this is a server, normal is pretty silent anyway.
 * Leave `everything' as the last in this enum.
 */
#ifndef VERBOSITY_HEADER_READ
#define VERBOSITY_HEADER_READ 1
enum verbosities { silent, normal, debug, everything };
#endif
