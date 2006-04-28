/*
*+
*  Name:
*     MESSYS_LEN

*  Purpose:
*     .H - Data definitions for message structure 

*  Language:
*     {routine_language}

*  Authors:
*     {original_author_entry}

*  History:
*     11-APR-1994 (BDK):
*        Remove definition of EXT_ macros
*     13-APR-1994 (BDK):
*        Remove structure definitions
*     07-JUL-1994 (AJC):
*        Renamed from MESSYS_DD
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#ifndef INT_BYTE_SIZE

#define INT_BYTE_SIZE	(sizeof(int))
#define MSG_LEN 500		/* length of message in bytes (from DDPATH) */
				/* Must be divisible by INT_BYTE_SIZE       */
				/* If this is changed, MESSDEFN must also be */
				/* changed */
#define MSG_NAME_LEN	32
#ifndef MSG_FIX_LEN
#  define MSG_FIX_LEN	(6*INT_BYTE_SIZE+MSG_NAME_LEN)	
				/* length of non-value part of non-path part */
#endif /* #ifndef MSG_FIX_LEN */

#define MSG_VAL_LEN	(MSG_LEN - MSG_FIX_LEN)
				/* length of value part of message */

#endif /* #ifndef INT_BYTE_SIZE */
