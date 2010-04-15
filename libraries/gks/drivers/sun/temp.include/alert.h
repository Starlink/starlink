/*	@(#)alert.h 15.15 88/03/25	*/

/* ------------------------------------------------------------------ */
/* ------ alert.h: Copyright (c) 1986 by Sun Microsystems, Inc. ----- */
/* ------------------------------------------------------------------ */

#ifndef alert_DEFINED
#define alert_DEFINED

#include <sunwindow/attr.h>

/* ------------------------------------------------------------------ */
/* ------------------------- Attributes ----------------------------- */
/* ------------------------------------------------------------------ */

#define ALERT_ATTR(type, ordinal)	ATTR(ATTR_PKG_ALERT, type, ordinal)
#define ALERT_ATTR_LIST(ltype, type, ordinal) \
	ALERT_ATTR(ATTR_LIST_INLINE((ltype), (type)), (ordinal))
#define ALERT_BUTTON_VALUE_PAIR		ATTR_INT_PAIR

typedef enum {
ALERT_NO_BEEPING	= ALERT_ATTR(ATTR_BOOLEAN,			5),
ALERT_NO_IMAGE		= ALERT_ATTR(ATTR_BOOLEAN,			10),
ALERT_IMAGE		= ALERT_ATTR(ATTR_PIXRECT_PTR,  		15),
ALERT_POSITION		= ALERT_ATTR(ATTR_INT,				7),
ALERT_MESSAGE_STRINGS	= ALERT_ATTR_LIST(ATTR_NULL, ATTR_STRING,	20),
ALERT_MESSAGE_STRINGS_ARRAY_PTR
			= ALERT_ATTR(ATTR_STRING,			22),
ALERT_MESSAGE_FONT	= ALERT_ATTR(ATTR_PIXFONT_PTR,			25),
ALERT_BUTTON_YES	= ALERT_ATTR(ATTR_STRING,			35),
ALERT_BUTTON_NO		= ALERT_ATTR(ATTR_STRING,			36),
ALERT_BUTTON		= ALERT_ATTR(ALERT_BUTTON_VALUE_PAIR,		37),
ALERT_BUTTON_FONT	= ALERT_ATTR(ATTR_PIXFONT_PTR,			40),
ALERT_TRIGGER		= ALERT_ATTR(ATTR_INT,				50),
ALERT_OPTIONAL		= ALERT_ATTR(ATTR_BOOLEAN,			55)
} Alert_attribute;

#define alert_attr_next(attr) (Alert_attribute *)attr_next((caddr_t *)attr)

/* ------------------------------------------------------------------ */
/* ------------------------useful constants ------------------------- */
/* ------------------------------------------------------------------ */

/* the following constant, ALERT_FAILED is returned if alert_prompt()
   failed for an unspecified reason.
*/

#define ALERT_YES			 1
#define ALERT_NO			 0
#define ALERT_FAILED			-1
#define ALERT_TRIGGERED			-2

/* ------------------------------------------------------------------ */
/* ------------------------type declarations ------------------------ */
/* ------------------------------------------------------------------ */

typedef enum {
	/* The following describe the options for ALERT_POSITION */
	ALERT_SCREEN_CENTERED		= 0,
	ALERT_CLIENT_CENTERED		= 1,
	ALERT_CLIENT_OFFSET		= 2
} Alert_position;

/* ------------------------------------------------------------------ */
/* -------------------- external declarations ----------------------- */
/* ------------------------------------------------------------------ */


extern int
alert_prompt(/*client_frame, event, attr_list*/);

#endif ~alert_DEFINED
