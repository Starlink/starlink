/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Include File Name:	press/h/h_comp.h
*
*   Purpose:
*	Header file for the h compress routines.
*
*   Date		: Mar 15, 1993
*
*   SCCS data		: @(#)
*	Module Name	: h_comp.h
*	Version Number	: 1.3
*	Release Number	: 1
*	Last Updated	: 30 Mar 1993
*
*   Programmer		: Norman Hill
*
*   Modification History:
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

static unsigned char code_magic[2] = { 0xDD, 0x99 };

/* allan: see h_decomp.c for routine */
/* #define	read_int( char_in, data )	char_in( (byte *) data, sizeof( int ) ) */

/* allan: see h_press.c for routine */
/*#define	write_int( char_out, data )	char_out( (byte *) data, sizeof( int ) )*/
