/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Include File Name:	gen/h/gen_str.h
*
*   Purpose:
*	Contains defines and declarations for string manipulation.
*
*   Date		: Dec 10, 1990
*
*   SCCS data		: @(#)
*	Module Name	: gen_str.h
*	Version Number	: 1.8
*	Release Number	: 1
*	Last Updated	: 12/16/92
*
*   Programmer		: Severin Gaudet
*
*   Modification History:
*	92/03/02 Norman Hill - Added strnsav.
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

/*
 *  Standard include file.
 */

#include <string.h>

/*
 *  Definitions.
 */

#ifndef STRSIZ
#define STRSIZ			256
#endif

#define streq(s1,s2)		(strcmp(s1,s2) == 0)
#define strne(s1,s2)		(strcmp(s1,s2) != 0)

/*
 *  External function declarations.
 */

extern	boolean	str2float();
extern	boolean	str2int();
extern	void	str2lower();
extern	void	str2upper();
extern	char	*strapp();
extern	char	*strext();
extern	char	*strfit();
extern	boolean	strfloat();
extern	void	strhead();
extern	boolean	strint();
extern	char	*strnapp();
extern	char	*strnsav();
extern	boolean	strpattern();
extern	void	strroot();
extern	char	*strsav();
extern	char	*strtail();
extern	void	strtokens();
