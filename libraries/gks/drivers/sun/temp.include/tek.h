/*	@(#)tek.h 10.5 88/02/08 SMI	*/

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

/*******************
*
* module input routines
*
********************
*
* caddr_t
* tek_create(tdata, straps)	init emulator return magic cookie to ident.
* caddr_t tdata;		user magic cookie for output routines
* int straps;			terminal straps
*
* tek_destroy(tp)			terminate emulation
* struct tek *tp;
*
* tek_ttyinput(tp, c)		input from tty
* struct tek *tp;
* char c;
*
* tek_kbdinput(tp, c)		input from keyboard
* struct tek *tp;
* char c;
*
* tek_posinput(tp, x, y)	update current position
* struct tek *tp;
* int x, y;
*
*******************/

extern caddr_t tek_create();
extern void tek_destroy();
extern void tek_ttyinput();
extern void tek_kbdinput();
extern void tek_posinput();

/*******************
*
* user supplied module output routines
*
********************
*
* tek_move(tdata, x, y)		move current position without writing
* caddr_t tdata;
* int x,y;
*
* tek_draw(tdata, x, y, type, style)	draw from current position to x,y
* caddr_t tdata;
* int x,y;
* enum vtype type;
* enum vstyle style;		line style
*
* tek_char(tdata, c)		put a character at the current position
* caddr_t tdata;
* char c;
*
* tek_ttyoutput(tdata, c)	output to tty
* caddr_t tdata;
* char c;
*
* tek_chfont(tdata, font)	change alph fonts
* caddr_t tdata;
* int font;
*
* tek_cursormode(tdata, cmode)	change displayed cursor mode
* caddr_t tdata;		alpha cursor is put at current position
* enum cursormode cmode;
*
* tek_clearscreen(tdata)	clear the screen
* caddr_t tdata;
*
* tek_bell(tdata)		ring the bell
* caddr_t tdata;
*
********************/

extern void tek_move();
extern void tek_draw();
extern void tek_char();
extern void tek_ttyoutput();
extern void tek_chfont();
extern void tek_cursormode();
extern void tek_clearscreen();
extern void tek_bell();
extern void tek_makecopy();
extern void tek_pagefull_on();
extern void tek_pagefull_off();

/*
* tektronix 4014 constants
*/
#define TXSIZE		4096	/* bits in tektronix x axis */
#define TYSIZE		3072	/* bits in tektronix y axis */
#define NFONT		4	/* number of fonts */
#define TEKFONT0X	56	/* char width in font 0 in tekpoints */
#define TEKFONT0Y	88	/* char height in font 0 in tekpoints */
#define TEKFONT1X	51	/* char width in font 1 in tekpoints */
#define TEKFONT1Y	82	/* char height in font 1 in tekpoints */
#define TEKFONT2X	34	/* char width in font 2 in tekpoints */
#define TEKFONT2Y	53	/* char height in font 2 in tekpoints */
#define TEKFONT3X	31	/* char width in font 3 in tekpoints */
#define TEKFONT3Y	48	/* char height in font 3 in tekpoints */

/*
* tektronix 4014 straps
*/
#define LFCR	0x001			/* LF causes CR also */
#define CRLF	0x002			/* CR causes LF also */
#define DELLOY	0x004			/* DEL is LOY char */
#define AECHO	0x008			/* auto echo */
#define ACOPY	0x010			/* auto print */
#define LOCAL	0x020			/* local terminal */
#define GINTERM	0x300			/* gin terminator options */
#define GINNONE	0x000			/* no gin terminator chars */
#define GINCR	0x100			/* send CR on gin termination */
#define	GINCRE	0x200			/* send CR,EOT on gin termination */
#define MARGIN	0xC00			/* margin control */
#define MARGOFF	0x000			/* no margin control */
#define MARG1	0x400			/* full at margin 1 */
#define MARG2	0x800			/* full at margin 2 */

/*
* meta characters
*/
#define PAGE		0200	/* clear page */
#define RESET		0201	/* reset terminal */
#define COPY		0202	/* make copy */
#define RELEASE		0203	/* margin release */

/*
* ascii control characters
*/
#define NUL	000
#define SOH	001
#define STX	002
#define ETX	003
#define EOT	004
#define ENQ	005
#define ACK	006
#define BEL	007
#define BS	010
#define TAB	011
#define LF	012
#define VT	013
#define FF	014
#define CR	015
#define SO	016
#define SI	017
#define	DLE	020
#define DC1	021
#define DC2	022
#define DC3	023
#define DC4	024
#define NAK	025
#define SYN	026
#define ETB	027
#define CAN	030
#define EM	031
#define SUB	032
#define ESC	033
#define FS	034
#define GS	035
#define RS	036
#define US	037
#define SP	040

/*******************
*
* types
*
*******************/

/* cursor modes */
enum cursormode { NOCURSOR, ALPHACURSOR, GFXCURSOR };

/* vector types */
enum vtype { VT_NORMAL, VT_DEFOCUSED, VT_WRITETHRU };

/* vector style */
enum vstyle { VS_NORMAL, VS_DOTTED, VS_DOTDASH, VS_SHORTDASH, VS_LONGDASH };
