/*
 *+
 *  Name:
 *    messys_par.h

 *  Purpose:
 *    Include file for messys layer - messys constants

 *  History:
 *    15-NOV-1994 (AJC):
 *       Increased MESSYS__MXPATH and __MXTRANS from 16 to 32
 *-
 */
#ifndef MESSYS__TNAME

/* Sizes for messys variables */
#define	MESSYS__MXTRANS 	32
#define	MESSYS__MXPATH 		32
#define	MESSYS__MXMACH 		16
#define	MESSYS__MNAME 		32
#define	MESSYS__TNAME 		32
#define	MESSYS__MSPNAMLEN 	20


/* Valid values of MSG_function */
#define	MESSYS__MESSAGE 	0
#define	MESSYS__INIT 		1
#define	MESSYS__ACK_INIT 	2
#define	MESSYS__DE_INIT 	3

/* Null values */
#define	MESSYS__NULL_Q 		(-1)
#define	MESSYS__NULL_M 		(-1)
#define	MESSYS__NULL_T		(-1)
#define	MESSYS__NULL_P		(-1)

/* Possible 'states' of a path.  Note that NULL_P acts as one of these */

#define	MESSYS__PART_P 		1
#define	MESSYS__FULL_P 		2

/* Timeout parameters */
#define	MESSYS__TIMEOUTID 	10001
#define	MESSYS__INIT_WAIT_TIME 	30000
#define	MESSYS__INFINITE 	-1

/* Constants associated with networking */

#define	MESSYS__MAXNET 		4
#define	MESSYS__SEPLEN 		3   /* One added here from the FORTRAN version
				       to allow for null terminator in C */
#endif /* #ifndef MESSYS__TNAME */
