/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Include File Name:	gen/h/gen_types.h
*
*   Purpose:
*	General constants and macros.
*
*   Date		: Dec 10, 1990
*
*   SCCS data		: @(#)
*	Module Name	: gen_types.h
*	Version Number	: 1.5
*	Release Number	: 1
*	Last Updated	: 2/17/94
*
*   Programmer		: Severin Gaudet
*
*   Modification History:
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

/*
 *  Standard include files.
 */

#include <stdlib.h>
#include <stdio.h>


/*
 *  Data structures
 */

typedef	int		boolean;

typedef	unsigned char	byte;
typedef boolean		(*pfb)();
typedef int		(*pfi)();
typedef float		(*pff)();
typedef void		(*pfv)();
typedef void		*pointer;


/*
 *  Constant definitions
 */

#ifndef	FALSE
#define	FALSE			0
#define TRUE			1
#endif


/*
 *  Memory allocation macro definitions.
 */

#define gen_alloc(n)            (malloc((unsigned)(n)))
#define gen_calloc(n,s)		(calloc((unsigned)(n),(unsigned)(s)))
#define	gen_free(p)		((void) free((pointer) (p)))
#define gen_realloc(p,n)        (realloc((pointer)(p),(unsigned)(n)))
#define gen_zero(p,n)		(memset((pointer)(p),0,(int)(n)))

#define boolean_alloc(n)        (boolean *)(gen_alloc(((n)*sizeof(boolean))))
#define byte_alloc(n)		(byte *)(gen_alloc(((n) * sizeof(byte))))
#define char_alloc(n)		(gen_alloc((n) * sizeof(char)))
#define double_alloc(n)		(double *)gen_alloc((n)*sizeof(double))
#define float_alloc(n)		(float *)gen_alloc((n)*sizeof(float))
#define int_alloc(n)		((int *)gen_alloc((n)*sizeof(int)))
#define ptr_alloc(n)		((pointer *)gen_alloc((n)*sizeof(pointer)))
#define short_alloc(n)		((short *)gen_alloc((n)*sizeof(short)))


/*
 *  General macro definitions.
 */

#ifdef	ABS
#undef	ABS
#endif
#define ABS(a)		((a) < 0 ? -(a) : (a))

#define EPSILON		0.00001
#define	EVEN(x)		((0x01 & x) ? FALSE : TRUE)

#define FEQ(a,b)        (ABS((a)-(b)) <= EPSILON)	/* Equal	*/
#define FLE(a,b)        (FLT(a,b) || FEQ(a,b))	/* Less than or equal	*/
#define FLT(a,b)        ((b)-(a) > EPSILON)	/* Less than		*/
#define FGE(a,b)        (FGT(a,b) || FEQ(a,b))	/* Greater than or equal*/
#define FGT(a,b)        ((a)-(b) > EPSILON)	/* Greater than		*/
#define FNE(a,b)        (!(FEQ(a,b)))		/* Not equal		*/

#define INRANGE(x,lo,hi)	((x)>=(lo) && (x)<=(hi))
#define INBOX(x,y,l,r,b,t)	(INRANGE(x,l,r) && INRANGE(y,b,t))
#ifdef  MAX 
#undef  MAX
#endif
#define MAX(a, b)	(((a) > (b)) ? (a) : (b))

#ifdef  MIN 
#undef  MIN
#endif
#define MIN(a, b)	(((a) < (b)) ? (a) : (b))

#ifndef NULL
#define NULL 0
#endif

#define	ROUND(x)	((x) + 0.5)
#define SIGN(x)		(((x) < (0)) ? (FEQ((x),0.0) ? 1 : (-1)) : 1)
#define SWAP(a,b)	{float swpv; swpv=(a);(a)=(b);(b)=swpv;}
#define XOR(a,b)	(((a) || (b)) && !((a) && (b)))
