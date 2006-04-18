/*
*+
*  Name:
*     adam_defns.h

*  Purpose:
*     Include file for MESSYS layer

*  Description:
*     The sequence g(et)s(et)o(bey)c(ancel) => gsoc used as part
*     of messys protocols

*  Authors:
*     IRJ:
*     SKR:
*     AJC: Alan Chipperfield (Starlink)

*  History:
*     15-JUN-1992 (IRJ):
*        Original
*     16-JUN-1992 (IRJ):
*        Tidied
*     16-JUN-1992 (SKR):
*        Tidied
*     07-JUL-1994 (AJC):
*        Add CONTROL

*-
*/

#define NAMELEN 	32	/* length of character strings holding names */
#define SET		1	/* set a task parameter */
#define GET		2	/* get a task parameter */
#define OBEY		3	/* obey a task action */
#define CANCEL		4	/* cancel a task action in progress */
#define CONTROL         5       /* control message for task */
