/*  MESSYS_LEN.H - Data definitions for message structure
    History :
     11Apr 1994: remove definition of EXT_ macros (BDK)
     13Apr 1994: remove structure definitions (BDK)
      7Jul 1994: renamed from MESSYS_DD (AJC)
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
