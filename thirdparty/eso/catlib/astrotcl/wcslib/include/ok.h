/* static char sccsid[] = "@(#) ST-ECF tc/h/ok.h	4.1	12/6/91"; */
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.TYPE		Header
.NAME		ok.h
.LANGUAGE	C
.AUTHOR		Francois Ochsenbein [ESO], Alan Richmond [ST-ECF].
.CATEGORY	Status definitions
.COMMENTS	
.ENVIRONMENT
.VERSION 1.0   	05-Mar-1987: Extracted from stesodef.h
.VERSION 1.1   	28-Mar-1990: Added COMMENT_CHAR
---------------------------*/

#ifndef OK_DEF
#define OK_DEF	0

#define COMMENT_CHAR	'!'		/* Character used allover	*/

#define OK		1		/* no error		*/
#define NOK		0		/* not OK		*/
#define TRUE		1		/* ST-ScI standard	*/
#define FALSE		0		/* ST-ScI standard	*/
#define SUCCESS		1		/* ST-ScI standard 	*/
#define FAILURE		0		/* ST-ScI standard	*/
#define NOT_FOUND	(-1)		/* mismatch 		*/

#define MODE_INTERACTIVE 1		/* interactive mode */
#define MODE_BATCH       2		/* batch mode       */
#define MODE_NETWORK     3 		/* network mode     */
#define MODE_OTHER       0 		/* unknown mode     */

#define RECORD_MODE	0400		/* Special option for opening files */

#endif
