/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Include File Name:	press/h/ux_press.h
*
*   Purpose:
*	Private header for the unix compress routines.
*
*   Date		: Feb 23, 1993
*
*   SCCS data		: @(#)
*	Module Name	: ux_press.h
*	Version Number	: 1.1
*	Release Number	: 1
*	Last Updated	: 03/01/93
*
*   Programmer		: Norman Hill
*
*   Modification History:
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

#include <gen_msg.h>

typedef int		code_int;
typedef long int	count_int;


/*
 *  Define the bits for the third byte in the header.
 */

#define	BLOCK_MASK	0x80
#define	BIT_MASK	0x1f
#define	CHECK_GAP	50000


/*
 * the next two codes should not be changed lightly, as they must not
 * lie within the contiguous general code space.
 */

#define FIRST   257     /* first free entry */
#define CLEAR   256     /* table clear output code */


#define	BITS		16	/* Maximum number of bits.		*/
#define	INIT_BITS	9	/* Initial number of bits/code.		*/
#define	HSIZE		69001	/* Hash table size.			*/

#define htabof(i)       htab[i]
#define codetabof(i)    codetab[i]


/*
 * To save much memory, we overlay the table used by compress() with those
 * used by decompress().  The tab_prefix table is the same size and type
 * as the codetab.  The tab_suffix table needs 2**BITS characters.  We
 * get this from the beginning of htab.  The output stack uses the rest
 * of htab, and contains characters.  There is plenty of room for any
 * possible stack (stack used to be 8000 characters).
 */

#define tab_prefixof(i) 	codetabof(i)
#define tab_suffixof(i)		((byte *)(htab))[i]
#define de_stack		((byte *)&tab_suffixof(1<<BITS))
#define MAXCODE(n_bits)		((1 << (n_bits)) - 1)
