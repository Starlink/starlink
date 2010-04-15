/******************************************************************************
 *
 * 	I C L _ I O . H
 *
 * Include file for the icl io subsystem
 *
 ******************************************************************************
 */

#define IOBUFFERLENGTH 256
#define INPUTBUFFERLENGTH 256
#define INPUTLINELENGTH 256
#define IOPROCESSNAME "icl_io"

struct iocommand
  {
  int fcode;	/* Message function code */
  int iarg;	/* Message sequence number or integer argument */
  int buflen;	/* buffer length (prompt + default) including NULLS */
  int dindex;   /* Index of default value string in buff - 0 if unused */
  char buff[IOBUFFERLENGTH];
  }
#define ICLPIPE 3
#define iocommandlength(a) (a.buflen + (4 * sizeof(int)))
;
#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif
#define NOTKNOWN -1


#define OUTPUTOK 0
#define OUTPUTDELAYED 1

#define IO_COMMAND_GETINTINPUT 1 /* det2 = input identity, det3 = length of prompt in buff */
#define IO_COMMAND_OUTPUT      2
#define IO_COMMAND_GETINPUT    3 /* det2 = input identity, det3 = length of prompt in buff */
#define IO_COMMAND_SETSCREEN   4 /* det2 = number of lines */
#define IO_COMMAND_LOCATE      5 /* det2 = Row * 256 + Column  */
#define IO_COMMAND_CLEAR       6 /* det2 = Row * 256 + Column  */
#define IO_COMMAND_KILL	       7
#define IO_COMMAND_SUSPEND     8

/* Screen modes	*/
#define INITIAL		0
#define	LINEMODE	1
#define	SCREENMODE	2

/* Terminal insert/overstrike flags */
#define OVERSTRIKE 1
#define INSERT     2

/* Useful NULL types */
#define CHARNIL      ((char *)0)
#define FUNCTIONNIL ((void (*)())0)
#define NOWINDOW ((WINDOW *)0)
