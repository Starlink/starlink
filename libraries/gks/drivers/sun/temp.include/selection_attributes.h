/*	@(#)selection_attributes.h 10.5 88/02/08	*/

#ifndef	suntool_selection_attributes_DEFINED
#define	suntool_selection_attributes_DEFINED

/*
 *	Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#include <sunwindow/attr.h>
/*
 *	Common requests a client may send to a selection-holder
 */
#define ATTR_PKG_SELECTION	ATTR_PKG_SELN_BASE

#define SELN_ATTR(type, n)	ATTR(ATTR_PKG_SELECTION, type, n)

#define SELN_ATTR_LIST(list_type, type, n)	\
	ATTR(ATTR_PKG_SELECTION, ATTR_LIST_INLINE(list_type, type), n)

/*
 *	Attributes of selections
 */

typedef enum	{

    /*	Simple attributes
     */
    SELN_REQ_BYTESIZE		= SELN_ATTR(ATTR_INT,		         1),
    SELN_REQ_CONTENTS_ASCII	= SELN_ATTR_LIST(ATTR_NULL, ATTR_CHAR,   2),
    SELN_REQ_CONTENTS_PIECES	= SELN_ATTR_LIST(ATTR_NULL, ATTR_CHAR,   3),
    SELN_REQ_FIRST		= SELN_ATTR(ATTR_INT,		         4),
    SELN_REQ_FIRST_UNIT		= SELN_ATTR(ATTR_INT,		         5),
    SELN_REQ_LAST		= SELN_ATTR(ATTR_INT,		         6),
    SELN_REQ_LAST_UNIT		= SELN_ATTR(ATTR_INT,		         7),
    SELN_REQ_LEVEL		= SELN_ATTR(ATTR_INT,		         8),
    SELN_REQ_FILE_NAME		= SELN_ATTR_LIST(ATTR_NULL, ATTR_CHAR,   9),

    /* Simple commands (no parameters)
     */
    SELN_REQ_COMMIT_PENDING_DELETE
				= SELN_ATTR(ATTR_NO_VALUE,	        65),
    SELN_REQ_DELETE		= SELN_ATTR(ATTR_NO_VALUE,	        66),
    SELN_REQ_RESTORE		= SELN_ATTR(ATTR_NO_VALUE,	        67),

    /* Other commands
     */
    SELN_REQ_YIELD		= SELN_ATTR(ATTR_ENUM,		        97),
    SELN_REQ_FAKE_LEVEL		= SELN_ATTR(ATTR_INT,		        98),
    SELN_REQ_SET_LEVEL		= SELN_ATTR(ATTR_INT,		        99),

    /* Service debugging commands
     */
    SELN_TRACE_ACQUIRE		= SELN_ATTR(ATTR_BOOLEAN,	       193),
    SELN_TRACE_DONE		= SELN_ATTR(ATTR_BOOLEAN,	       194),
    SELN_TRACE_HOLD_FILE	= SELN_ATTR(ATTR_BOOLEAN,	       195),
    SELN_TRACE_INFORM		= SELN_ATTR(ATTR_BOOLEAN,	       196),
    SELN_TRACE_INQUIRE		= SELN_ATTR(ATTR_BOOLEAN,	       197),
    SELN_TRACE_YIELD		= SELN_ATTR(ATTR_BOOLEAN,	       198),
    SELN_TRACE_STOP		= SELN_ATTR(ATTR_BOOLEAN,	       199),
    SELN_TRACE_DUMP		= SELN_ATTR(ATTR_ENUM,		       200),

    /* Get / Set the style of notification on function-key up
     */
    SELN_GET_NOTIFY_SUPPRESSION	= SELN_ATTR(ATTR_INT,		       209),
    SELN_SET_NOTIFY_SUPPRESSION	= SELN_ATTR(ATTR_INT,		       210),

    /*	Close bracket so replier can terminate commands
     *	like FAKE_LEVEL which have scope
     */
    SELN_REQ_END_REQUEST	= SELN_ATTR(ATTR_NO_VALUE,	       253),

    /*	Error returnd for failed or unrecognized requests
     */
    SELN_REQ_UNKNOWN		= SELN_ATTR(ATTR_INT,		       254),
    SELN_REQ_FAILED		= SELN_ATTR(ATTR_INT,		       255)

}	Seln_attribute;

/* Meta-levels available for use with SELN_REQ_FAKE/SET_LEVEL.
 *	SELN_LEVEL_LINE is "text line bounded by newline characters,
 *			    including only the terminating newline"
 */
typedef enum {
    SELN_LEVEL_FIRST	= 0x40000001,
    SELN_LEVEL_LINE	= 0x40000101,
    SELN_LEVEL_ALL	= 0x40008001,
    SELN_LEVEL_NEXT	= 0x4000F001,
    SELN_LEVEL_PREVIOUS	= 0x4000F002
}	Seln_level;
#endif
