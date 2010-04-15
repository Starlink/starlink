/*	@(#)textsw.h 10.49 88/02/08 SMI	*/

/*
 * Copyright (c) 1986, 1987 by Sun Microsystems, Inc.
 */

#ifndef suntool_textsw_DEFINED

#define suntool_textsw_DEFINED

/*
 * Programmatic interface to textsw
 */

#					ifndef pr_rop
#include <pixrect/pixrect.h>
#					endif
#					ifndef rect_right
#include <sunwindow/rect.h>
#					endif
#					ifndef rl_rectoffset
#include <sunwindow/rectlist.h>
#					endif
#					ifndef pw_rop
#include <sunwindow/pixwin.h>
#					endif
#					ifndef sunwindow_attr_DEFINED
#include <sunwindow/attr.h>
#					endif
#					ifndef tool_struct_DEFINED
#include <suntool/tool_struct.h>
#					endif
#					ifndef suntool_selection_attributes_DEFINED
#include <suntool/selection_attributes.h>
#					endif

/* New window_create() defs */
#					ifndef tool_DEFINED
#include <suntool/window.h>
#define TEXTSW_TYPE ATTR_PKG_TEXTSW
#define TEXTSW textsw_window_object, WIN_COMPATIBILITY
extern caddr_t textsw_window_object();
#					endif

#if lint
	typedef void * Textsw_opaque;
#else
	typedef char * Textsw_opaque;
#endif

typedef Textsw_opaque		Textsw;
#define	TEXTSW_NULL		((Textsw)0)
typedef long int		Textsw_index;
#define	TEXTSW_INFINITY		((Textsw_index)0x77777777)
#define	TEXTSW_CANNOT_SET	((Textsw_index)0x80000000)
#define	TEXTSW_UNIT_IS_CHAR	SELN_LEVEL_FIRST
#define	TEXTSW_UNIT_IS_WORD	SELN_LEVEL_FIRST+1
#define	TEXTSW_UNIT_IS_LINE	SELN_LEVEL_LINE


extern Textsw
textsw_build( /* tool, attributes */ );
#							ifdef notdef
	struct tool	 *tool;
	char		**attributes;
#							endif

extern void
textsw_destroy( /* textsw */ );
#							ifdef notdef
	Textsw		textsw;
#							endif

extern void
textsw_destroy_view( /* textsw */ );
#							ifdef notdef
	Textsw		textsw;
#							endif

extern Textsw
textsw_first( /* textsw */ );
#							ifdef notdef
	Textsw		textsw;
#							endif

extern Textsw
textsw_init( /* windowfd, attributes */ );
#							ifdef notdef
	int		  windowfd;
	char		**attributes;
#							endif

extern Textsw
textsw_next( /* textsw */ );
#							ifdef notdef
	Textsw		textsw;
#							endif

extern void
textsw_reset( /* abstract, locx, locy */ );
#							ifdef notdef
	Textsw 		abstract;
	int		locx;
	int		locy;
#							endif

extern void
textsw_file_lines_visible( /* abstract, top, bottom */ );
#							ifdef notdef

	Textsw 		abstract;
	int 		*top;
	int 		*bottom;
#							endif

/* Status values for textsw_build and textsw_init. */
typedef enum {
	TEXTSW_STATUS_OKAY,
	TEXTSW_STATUS_OTHER_ERROR,
	TEXTSW_STATUS_CANNOT_ALLOCATE,
	TEXTSW_STATUS_CANNOT_OPEN_INPUT,
	TEXTSW_STATUS_BAD_ATTR,
	TEXTSW_STATUS_BAD_ATTR_VALUE,
	TEXTSW_STATUS_CANNOT_INSERT_FROM_FILE,
	TEXTSW_STATUS_OUT_OF_MEMORY
} Textsw_status;

/* Status values for textsw_expand. */
typedef enum {
	TEXTSW_EXPAND_OK,
	TEXTSW_EXPAND_FULL_BUF,
	TEXTSW_EXPAND_OTHER_ERROR
} Textsw_expand_status;

extern Textsw_expand_status
textsw_expand( /* abstract, start, stop_plus_one,
	          out_buf, out_buf_len, total_chars */ );
#							ifdef notdef
	Textsw			 abstract;
	Es_index		 start;	/* Entity to start expanding at */
	Es_index		 stop_plus_one; /* 1st ent not expanded */
	char			*out_buf;
	int			 out_buf_len;
	int			*total_chars;
#							endif

/* Attributes for textsw_build, textsw_init, textsw_set and textsw_get. */
#define TEXTSW_ATTR(type, ordinal)	ATTR(ATTR_PKG_TEXTSW, type, ordinal)
typedef enum {
    /* Apply to underlying sw collection */
	TEXTSW_ADJUST_IS_PENDING_DELETE	= TEXTSW_ATTR(ATTR_BOOLEAN,	  2),
	TEXTSW_AGAIN_LIMIT		= TEXTSW_ATTR(ATTR_INT,		  3),
	TEXTSW_AGAIN_RECORDING		= TEXTSW_ATTR(ATTR_BOOLEAN,	  4),
	TEXTSW_AUTO_INDENT		= TEXTSW_ATTR(ATTR_BOOLEAN,	  5),
	TEXTSW_AUTO_SCROLL_BY		= TEXTSW_ATTR(ATTR_INT,		  7),
	TEXTSW_BLINK_CARET		= TEXTSW_ATTR(ATTR_BOOLEAN,	  8),
	TEXTSW_BROWSING			= TEXTSW_ATTR(ATTR_BOOLEAN,	 11),
	TEXTSW_CHECKPOINT_FREQUENCY	= TEXTSW_ATTR(ATTR_INT,		 13),
	TEXTSW_CLIENT_DATA		= TEXTSW_ATTR(ATTR_OPAQUE,	 14),
	TEXTSW_CONFIRM_OVERWRITE	= TEXTSW_ATTR(ATTR_BOOLEAN,	 17),
	TEXTSW_CONTENTS			= TEXTSW_ATTR(ATTR_STRING,	 20),
	TEXTSW_CONTROL_CHARS_USE_FONT	= TEXTSW_ATTR(ATTR_BOOLEAN,	 21),
	TEXTSW_DESTROY_ALL_VIEWS	= TEXTSW_ATTR(ATTR_BOOLEAN,	 22),
	TEXTSW_DISABLE_CD		= TEXTSW_ATTR(ATTR_BOOLEAN,	 23),
	TEXTSW_DISABLE_LOAD		= TEXTSW_ATTR(ATTR_BOOLEAN,	 26),
	TEXTSW_EDIT_BACK_CHAR		= TEXTSW_ATTR(ATTR_CHAR,	220),
	TEXTSW_EDIT_BACK_LINE		= TEXTSW_ATTR(ATTR_CHAR,	221),
	TEXTSW_EDIT_BACK_WORD		= TEXTSW_ATTR(ATTR_CHAR,	222),
	TEXTSW_WRAPAROUND_SIZE		= TEXTSW_ATTR(ATTR_INT,		201),
	TEXTSW_EDIT_COUNT		= TEXTSW_ATTR(ATTR_INT,		 27),
	TEXTSW_ERROR_MSG		= TEXTSW_ATTR(ATTR_STRING,	 29),
	TEXTSW_ES_CREATE_PROC		= TEXTSW_ATTR(ATTR_FUNCTION_PTR, 30),
	TEXTSW_FILE			= TEXTSW_ATTR(ATTR_STRING,	 32),
	TEXTSW_FILE_CONTENTS		= TEXTSW_ATTR(ATTR_STRING,	 33),
	TEXTSW_FONT			= TEXTSW_ATTR(ATTR_PIXFONT_PTR,  35),
	TEXTSW_HEIGHT			= TEXTSW_ATTR(ATTR_Y,		 38),
	TEXTSW_HISTORY_LIMIT		= TEXTSW_ATTR(ATTR_INT,		 41),
	TEXTSW_IGNORE_LIMIT		= TEXTSW_ATTR(ATTR_INT,		 44),
	TEXTSW_INSERT_FROM_FILE		= TEXTSW_ATTR(ATTR_STRING,	 45),
	TEXTSW_INSERT_MAKES_VISIBLE	= TEXTSW_ATTR(ATTR_ENUM,	 47),
	TEXTSW_INSERT_ONLY		= TEXTSW_ATTR(ATTR_BOOLEAN,	 48),
	TEXTSW_INSERTION_POINT		= TEXTSW_ATTR(ATTR_INT,		 50),
	TEXTSW_LENGTH			= TEXTSW_ATTR(ATTR_INT,		 53),
	TEXTSW_LOAD_DIR_IS_CD		= TEXTSW_ATTR(ATTR_ENUM,	200),
	TEXTSW_LOWER_CONTEXT		= TEXTSW_ATTR(ATTR_INT,		 56),
	TEXTSW_MEMORY_MAXIMUM		= TEXTSW_ATTR(ATTR_INT,		 59),
	TEXTSW_MENU			= TEXTSW_ATTR(ATTR_OPAQUE,	 62),
	TEXTSW_MENU_STYLE		= TEXTSW_ATTR(ATTR_INT,		240),
	TEXTSW_MODIFIED			= TEXTSW_ATTR(ATTR_BOOLEAN,	 65),
	TEXTSW_MULTI_CLICK_SPACE	= TEXTSW_ATTR(ATTR_INT,		 68),
	TEXTSW_MULTI_CLICK_TIMEOUT	= TEXTSW_ATTR(ATTR_INT,		 71),
	TEXTSW_MUST_SHOW_CARET		= TEXTSW_ATTR(ATTR_BOOLEAN,	 72),
	TEXTSW_NAME			= TEXTSW_ATTR(ATTR_STRING,	 74),
	TEXTSW_NAME_TO_USE		= TEXTSW_ATTR(ATTR_STRING,	 75),
	TEXTSW_NO_REPAINT_TIL_EVENT	= TEXTSW_ATTR(ATTR_BOOLEAN,	 300),
	TEXTSW_NO_RESET_TO_SCRATCH	= TEXTSW_ATTR(ATTR_BOOLEAN,	 76),
	TEXTSW_NO_SELECTION_SERVICE	= TEXTSW_ATTR(ATTR_BOOLEAN,	 77),
	TEXTSW_NOTIFY_LEVEL		= TEXTSW_ATTR(ATTR_INT,		 80),
	TEXTSW_NOTIFY_PROC		= TEXTSW_ATTR(ATTR_FUNCTION_PTR, 83),
	TEXTSW_PIXWIN			= TEXTSW_ATTR(ATTR_PIXWIN_PTR,	 86),
	TEXTSW_READ_ONLY		= TEXTSW_ATTR(ATTR_BOOLEAN,	 89),
	TEXTSW_RESET_MODE		= TEXTSW_ATTR(ATTR_ENUM,	 92),
	TEXTSW_RESET_TO_CONTENTS	= TEXTSW_ATTR(ATTR_NO_VALUE,	 93),
	TEXTSW_STATUS			= TEXTSW_ATTR(ATTR_OPAQUE,	 95),
	TEXTSW_STORE_CHANGES_FILE 	= TEXTSW_ATTR(ATTR_BOOLEAN,	 97),
	TEXTSW_STORE_SELF_IS_SAVE	= TEXTSW_ATTR(ATTR_BOOLEAN,	 98),
	TEXTSW_TAB_WIDTH		= TEXTSW_ATTR(ATTR_INT,		101),
	TEXTSW_TEMP_FILENAME		= TEXTSW_ATTR(ATTR_STRING,	102),
	TEXTSW_TOOL			= TEXTSW_ATTR(ATTR_INT,		104),
	TEXTSW_UPPER_CONTEXT		= TEXTSW_ATTR(ATTR_INT,		107),
	TEXTSW_WIDTH			= TEXTSW_ATTR(ATTR_X,		110),
    /* Make individual view changes affect all views */
    	TEXTSW_END_ALL_VIEWS		= TEXTSW_ATTR(ATTR_NO_VALUE,	140),
    	TEXTSW_FOR_ALL_VIEWS		= TEXTSW_ATTR(ATTR_NO_VALUE,	141),
    /* Apply to individual views */
	TEXTSW_COALESCE_WITH		= TEXTSW_ATTR(ATTR_OPAQUE,	113),
    	TEXTSW_FIRST			= TEXTSW_ATTR(ATTR_INT,		170),
    	TEXTSW_FIRST_LINE		= TEXTSW_ATTR(ATTR_INT,		173),
	TEXTSW_LEFT_MARGIN		= TEXTSW_ATTR(ATTR_INT,		176),
	TEXTSW_LINE_BREAK_ACTION	= TEXTSW_ATTR(ATTR_ENUM,	179),
	TEXTSW_RIGHT_MARGIN		= TEXTSW_ATTR(ATTR_INT,		182),
	TEXTSW_SCROLLBAR		= TEXTSW_ATTR(ATTR_OPAQUE,	185),
	TEXTSW_UPDATE_SCROLLBAR		= TEXTSW_ATTR(ATTR_NO_VALUE,	188),
    /* Spares for development between updates to this enumeration */
	TEXTSW_SPARE_1			=
			TEXTSW_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_INT),
									252),
	TEXTSW_SPARE_2			=
			TEXTSW_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_INT),
									253),
#ifdef DEBUG
	TEXTSW_MALLOC_DEBUG_LEVEL	= TEXTSW_ATTR(ATTR_INT,		254),
#endif
} Textsw_attribute;

/* Following definitions are for compatibility during 3.0. */
#define	TEXTSW_NO_CD		TEXTSW_DISABLE_CD

typedef enum {
	TEXTSW_NEVER		= 0,
	    /* Additional values for TEXTSW_LOAD_DIR_IS_CD */
	TEXTSW_ALWAYS		= 1,
	TEXTSW_ONLY		= 2,
	    /* Additional values for TEXTSW_INSERT_MAKES_VISIBLE */
	TEXTSW_IF_AUTO_SCROLL,
	    /* Valid values for TEXTSW_LINE_BREAK_ACTION */
	TEXTSW_CLIP,
	TEXTSW_WRAP_AT_CHAR,
	TEXTSW_WRAP_AT_WORD,
	TEXTSW_WRAP_AT_LINE,
	    /* Style values for menu appearance */
	TEXTSW_STYLE_ORGANIZED,
	TEXTSW_STYLE_UNORGANIZED

} Textsw_enum;

	/* A special scrollbar value for TEXTSW_VIEW_SCROLLBAR */
#define	TEXTSW_DEFAULT_SCROLLBAR	((caddr_t)1)

	/* Flag values for TEXTSW_NOTIFY_LEVEL attribute. */
#define	TEXTSW_NOTIFY_NONE		0x00
#define	TEXTSW_NOTIFY_DESTROY_VIEW	0x01
#define	TEXTSW_NOTIFY_EDIT_DELETE	0x02
#define	TEXTSW_NOTIFY_EDIT_INSERT	0x04
#define	TEXTSW_NOTIFY_EDIT		(TEXTSW_NOTIFY_EDIT_DELETE | \
					 TEXTSW_NOTIFY_EDIT_INSERT)
#define	TEXTSW_NOTIFY_PAINT		0x08
#define	TEXTSW_NOTIFY_REPAINT		0x10
#define	TEXTSW_NOTIFY_SCROLL		0x20
#define	TEXTSW_NOTIFY_SPLIT_VIEW	0x40
#define	TEXTSW_NOTIFY_STANDARD		0x80
#define	TEXTSW_NOTIFY_ALL		(TEXTSW_NOTIFY_DESTROY_VIEW | \
					 TEXTSW_NOTIFY_EDIT	    | \
					 TEXTSW_NOTIFY_PAINT	    | \
					 TEXTSW_NOTIFY_REPAINT	    | \
					 TEXTSW_NOTIFY_SCROLL	    | \
					 TEXTSW_NOTIFY_SPLIT_VIEW   | \
					 TEXTSW_NOTIFY_STANDARD)

extern int
textsw_default_notify( /* textsw, attributes */ );
#							ifdef notdef
	Textsw		textsw;
	Attr_avlist	attributes;
#							endif

extern int
textsw_nop_notify( /* textsw, attributes */ );
#							ifdef notdef
	Textsw		textsw;
	Attr_avlist	attributes;
#							endif

/*
 * Following are actions defined for client provided notify_proc.
 * Two standard notify procs are textsw_default_notify and textsw_nop_notify.
 */
#define	TEXTSW_ATTR_RECT_PAIR	ATTR_TYPE(ATTR_BASE_RECT_PTR, 2)
#define	TEXTSW_ATTR_REPLACE_5	ATTR_TYPE(ATTR_BASE_INT, 5)
typedef enum {
	TEXTSW_ACTION_CAPS_LOCK		= TEXTSW_ATTR(ATTR_BOOLEAN,	 1),
	TEXTSW_ACTION_CHANGED_DIRECTORY	= TEXTSW_ATTR(ATTR_STRING,	 4),
	TEXTSW_ACTION_EDITED_FILE	= TEXTSW_ATTR(ATTR_STRING,	 7),
	TEXTSW_ACTION_EDITED_MEMORY	= TEXTSW_ATTR(ATTR_NO_VALUE,	 8),
	TEXTSW_ACTION_FILE_IS_READONLY	= TEXTSW_ATTR(ATTR_STRING,	10),
	TEXTSW_ACTION_LOADED_FILE	= TEXTSW_ATTR(ATTR_STRING,	13),
	TEXTSW_ACTION_SAVING_FILE	= TEXTSW_ATTR(ATTR_NO_VALUE,	14),
	TEXTSW_ACTION_STORING_FILE	= TEXTSW_ATTR(ATTR_STRING,	15),
	TEXTSW_ACTION_USING_MEMORY	= TEXTSW_ATTR(ATTR_NO_VALUE,	16),
	TEXTSW_ACTION_WRITE_FAILED	= TEXTSW_ATTR(ATTR_NO_VALUE,	17),
	TEXTSW_ACTION_TOOL_CLOSE	= TEXTSW_ATTR(ATTR_NO_VALUE,	19),
	TEXTSW_ACTION_TOOL_DESTROY	= TEXTSW_ATTR(ATTR_OPAQUE,	20),
	TEXTSW_ACTION_TOOL_MGR		= TEXTSW_ATTR(ATTR_OPAQUE,	22),
	TEXTSW_ACTION_TOOL_QUIT		= TEXTSW_ATTR(ATTR_OPAQUE,	31),
	TEXTSW_ACTION_REPLACED		=
				   TEXTSW_ATTR(TEXTSW_ATTR_REPLACE_5,	34),
	TEXTSW_ACTION_PAINTED		= TEXTSW_ATTR(ATTR_RECT_PTR,	37),
	TEXTSW_ACTION_SCROLLED		=
				   TEXTSW_ATTR(TEXTSW_ATTR_RECT_PAIR,	40),
	TEXTSW_ACTION_DESTROY_VIEW	= TEXTSW_ATTR(ATTR_NO_VALUE,	43),
	TEXTSW_ACTION_SPLIT_VIEW	= TEXTSW_ATTR(ATTR_OPAQUE,	46),
} Textsw_action;

/*
 * Menu command tokens
 */
typedef enum {
	TEXTSW_MENU_NO_CMD,
	TEXTSW_MENU_SAVE,
	TEXTSW_MENU_SAVE_QUIT,
	TEXTSW_MENU_SAVE_RESET,
	TEXTSW_MENU_SAVE_CLOSE,
	TEXTSW_MENU_STORE,
	TEXTSW_MENU_STORE_QUIT,
	TEXTSW_MENU_STORE_CLOSE,
	TEXTSW_MENU_CLIP_LINES,
	TEXTSW_MENU_WRAP_LINES_AT_CHAR,
	TEXTSW_MENU_WRAP_LINES_AT_WORD,
	TEXTSW_MENU_RESET,
	TEXTSW_MENU_LOAD,
	TEXTSW_MENU_CREATE_VIEW,
	TEXTSW_MENU_DESTROY_VIEW,
	TEXTSW_MENU_CD,
	TEXTSW_MENU_NORMALIZE_INSERTION,
	TEXTSW_MENU_NORMALIZE_TOP,
	TEXTSW_MENU_NORMALIZE_BOTTOM,
/* 	TEXTSW_MENU_NORMALIZE_SELECTION, */
	TEXTSW_MENU_NORMALIZE_LINE,
	TEXTSW_MENU_COUNT_TO_LINE,
	TEXTSW_MENU_FILE_STUFF,
	TEXTSW_MENU_FIND_PULLRIGHT,
	TEXTSW_MENU_FIND,
	TEXTSW_MENU_FIND_BACKWARD,
	TEXTSW_MENU_FIND_SHELF_PULLRIGHT,
	TEXTSW_MENU_FIND_SHELF,
	TEXTSW_MENU_FIND_SHELF_BACKWARD,
#ifdef _TEXTSW_FIND_RE
	TEXTSW_MENU_FIND_RE,
	TEXTSW_MENU_FIND_RE_BACKWARD,
	TEXTSW_MENU_FIND_TAG,
	TEXTSW_MENU_FIND_TAG_BACKWARD,
#endif
	TEXTSW_MENU_PUT_THEN_GET,
	TEXTSW_MENU_FIND_AND_REPLACE,
	TEXTSW_MENU_SEL_ENCLOSE_FIELD,   /* Select |> field <| in pending and delete mode */
	TEXTSW_MENU_SEL_NEXT_FIELD,
	TEXTSW_MENU_SEL_PREV_FIELD,
	TEXTSW_MENU_MATCH_FIELD,
	TEXTSW_MENU_SEL_MARK_TEXT,
	TEXTSW_MENU_AGAIN,
	TEXTSW_MENU_UNDO,
	TEXTSW_MENU_UNDO_ALL,
	TEXTSW_MENU_CUT,
	TEXTSW_MENU_COPY,
	TEXTSW_MENU_PASTE,
	TEXTSW_MENU_SHOW_CLIPBOARD_FROM_EDIT, /* actual menu */
	TEXTSW_MENU_SHOW_CLIPBOARD_FROM_FIND, /* actual menu */
	TEXTSW_MENU_SHOW_CLIPBOARD, /* pullright */
	TEXTSW_MENU_LAST_CMD
} Textsw_menu_cmd;


/* Bit flags returned by textsw_process_event */
#define	TEXTSW_PE_BUSY		0x1
#define TEXTSW_PE_READ_ONLY	0x2
#define TEXTSW_PE_USED		0x4

/* Reset actions for Load/Reset/Save/Store */
#define	TEXTSW_LRSS_CURRENT		0
#define	TEXTSW_LRSS_ENTITY_START	1
#define	TEXTSW_LRSS_LINE_START		2


/* fields flags */
#define	TEXTSW_NOT_A_FIELD		0	/* This tells the field code don't do the search, only do a match.  */
#define	TEXTSW_FIELD_FORWARD		1
#define	TEXTSW_FIELD_BACKWARD		2
#define	TEXTSW_FIELD_ENCLOSE		3
#define TEXTSW_DELIMITER_FORWARD	4
#define TEXTSW_DELIMITER_BACKWARD	5
#define TEXTSW_DELIMITER_ENCLOSE	6

/* Support for marking of positions in the textsw */
typedef Textsw_opaque		Textsw_mark;
#define	TEXTSW_NULL_MARK	((Textsw_mark)0)

	/* Flags for use with textsw_add_mark */
#define	TEXTSW_MARK_DEFAULTS		0x0
#define	TEXTSW_MARK_MOVE_AT_INSERT	0x1
#define	TEXTSW_MARK_READ_ONLY		0x2

extern Textsw_mark
textsw_add_mark( /* textsw, position, flags */ );
#ifdef notdef
	Textsw		textsw;
	Textsw_index	position;
	unsigned	flags;
#endif

extern Textsw_index
textsw_find_mark( /* textsw, mark */ );
#ifdef notdef
	Textsw		textsw;
	Textsw_mark	mark;
#endif

extern void
textsw_remove_mark( /* textsw, mark */ );
#ifdef notdef
	Textsw		textsw;
	Textsw_mark	mark;
#endif


/* The magic number, attributes, and commands for smart filters */
#define	TEXTSW_FILTER_MAGIC		0xFF012003
typedef enum {
	TEXTSW_FATTR_INPUT		=	TEXTSW_ATTR(ATTR_OPAQUE,1),
	TEXTSW_FATTR_INPUT_EVENT	=	TEXTSW_ATTR(ATTR_OPAQUE,2),
	TEXTSW_FATTR_INSERTION_POINTS	=	TEXTSW_ATTR(ATTR_OPAQUE,3),
	TEXTSW_FATTR_INSERTION_LINE	=	TEXTSW_ATTR(ATTR_OPAQUE,4),
	TEXTSW_FATTR_SELECTION_ENDPOINTS=	TEXTSW_ATTR(ATTR_OPAQUE,5)
} Textsw_filter_attribute;
typedef enum {
	TEXTSW_FILTER_DELETE_RANGE	= 0,
	TEXTSW_FILTER_INSERT		= 1,
	TEXTSW_FILTER_SEND_RANGE	= 2,
	TEXTSW_FILTER_SET_INSERTION	= 3,
	TEXTSW_FILTER_SET_SELECTION	= 4
} Textsw_filter_command;

/* Flag values for textsw_add_glyph_on_line(). */
#define	TEXTSW_GLYPH_DISPLAY	0x0000001
#define	TEXTSW_GLYPH_LINE_START	0x0000002
#define	TEXTSW_GLYPH_WORD_START	0x0000004
#define	TEXTSW_GLYPH_LINE_END	0x0000008

#endif
