/* ***************************************************************************
 *
 * 	A D A M _ D E F N S . H
 *
 *	Include file for messys layer
 *	The sequence g(et)s(et)o(bey)c(ancel) => gsoc used as part
 *	of messys protocols
 *
 *	History :	Created: irj 15/6/92
 *			Tidied : irj, skr 16/6/92
 *                      Add CONTROL : ajc 6//7/94
 *
 * **************************************************************************** */


#define NAMELEN 	32			/* length of character strings holding names */
#define SET		1			/* set a task parameter */
#define GET		2			/* get a task parameter */
#define OBEY		3			/* obey a task action */
#define CANCEL		4			/* cancel a task action in progress */
#define CONTROL         5                       /* control message for task */
