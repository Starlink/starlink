/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Include File Name:	gen/h/gen_msg.h
*
*   Purpose:
*	Defines the message structure.
*
*   Date		: Aug 23, 1991
*
*   SCCS data		: @(#)
*	Module Name	: gen_msg.h
*	Version Number	: 1.3
*	Release Number	: 1
*	Last Updated	: 8/12/92
*
*   Programmer		: Severin Gaudet
*
*   Modification History:
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

/*
 *  A huge negative number is required for the MSG_ERRNO value.
 */

#define	MSG_ERRNO	(-9999)


/*
 *  Define the maximum message size.
 */

#define	MSG_MAX_LEN	2048


typedef struct	msg
{
    int		m_status;
    char	*m_format;
} MSG;

extern	void	msg_append();
extern	void	msg_clear();
extern	void	msg_format();
