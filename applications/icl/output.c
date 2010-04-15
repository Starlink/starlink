/******************************************************************************
 *
 *	O U T P U T . C	 - output routines for ICL
 *
 * This module provides routines used in ICL terminal output (which is handled
 * through a separate io subsystem process) and output to Unix files.
 *
 *		Created : 	S.K. Robinson 14/5/92
 *		Tidied and reformatted:
 *				B.K. McIlwrath 22/7/93
 *              Don't initialise out_fp at compile time:
 *                              B.K. McIlwrath 7/6/99
 *              Use new-style ems function names
 *                              A.J.Chipperfield 21/06/99
 *
 ******************************************************************************
 */
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include "icl.h"
#include "output.h"
#include "icl_io.h"
#include "ems.h"
#include "ems_par.h"
#include "sae_par.h"

extern node *node_value (value v);				/* node.c  */
extern value sys_exception2(char *format, char *arg1, char *arg2);/* procs.c */
extern value store_symbol(char *name, int type, node *n);	/* symtab.c  */
extern int terminal_output(void);				/* input.c */

extern int iocommand_fd;					/* main.c */

static struct iocommand message;
static int outputcommandnumber = 1;
static struct iocommand buffout;	/* work buffer for io subsystem */
static int outbuffp = 0;		/* current position in outbuff  */

static char sbuff[IOBUFFERLENGTH];

static FILE *out_fp;
#define OUTFPSTACK 100
static FILE *outfpstack[OUTFPSTACK];
static int outfpstackcnt = 0;

/******************************************************************************
 *
 *	S E N D T O I O S U B S Y S T E M (int command, int info, char *mess)
 *
 * Sends an ICL command to the io-subsystem process using a pipe creatd when
 * the io process was fork()ed.
 *
 * 'command' is one of the IO_COMMAND_xxxxx actions.
 * 'info' is an integer value valid for that command.
 * 'mess" is a character string to be passed to the subsystem.
 *
 ******************************************************************************
 */
void
sendtoiosubsystem(int command, int info, char *mess)
{
    message.fcode = command;
    message.iarg = info;
    strcpy(message.buff, mess);
    message.buflen = strlen(message.buff) + 1;
    write(iocommand_fd, &message, iocommandlength(message));
    return;
}


/******************************************************************************
 *
 *	S T A C K A N D S E T O U T P U T F P (value vfp)
 *
 * Used to save the current ICL output file descriptor and set it to 'vfp'
 *
 ******************************************************************************
 */
int
stackandsetoutfp(value vfp)
{
    if (outfpstackcnt == OUTFPSTACK)
	return 0;
    else {
	outfpstack[outfpstackcnt] = out_fp;
	++outfpstackcnt;
	out_fp = (FILE *) (file_part(vfp));
	return 1;
    }
}

/******************************************************************************
 *
 *	R E S T O R E O U T F P (void)
 *
 * Unstack file descriptor.
 *
 ******************************************************************************
 */
int
restoreoutfp(void)
{
    if (outfpstackcnt <= 0)
	return 0;
    else {
	--outfpstackcnt;
	out_fp = outfpstack[outfpstackcnt];
	return TRUE;
    }
}

/******************************************************************************
 *
 *	I C L O P E N A S O U T F P (char *whofor, char *filename)
 *
 * This routine opens a file (name 'filename') for output for ICL routine
 * 'whofor' (diagnostic).
 *
 * Method:
 * Saves the current contents of out_fp and sets it to the opened file
 * descriptor using stackandsetoutfp().
 *
 * Returns trueval if successfull,otherwise an exception
 *
 ******************************************************************************
 */
value
iclopenasoutfp(char *whofor, char *filename)
{
    FILE *fp;

    if ((fp = fopen(filename, "w")) == NULL)
	return sys_exception2(
		"OPENERR  %s: cannot create file \"%s\"", whofor, filename);
    if (stackandsetoutfp(value_file((PORTPTR) fp)) == 0)
	return sys_exception2(
 "OPENERR  %s: Creating file \"%s\" has caused ICLSTACKOVFL", whofor, filename);
    return trueval;
}

/******************************************************************************
 *
 *	I C L C L O S E A S O U T F P (char *whofor, char *filename)
 *
 * The system has been using the an output file via the descriptor out_fp.
 * This routine closes that file and restores out_fp to its previous value
 * using restoreoutfp().
 *
 * It is expected that the caller used iclopenasoutfp() to open the file and
 * so it is a system error if restoreoutfp() fails.
 *
 * Returns trueval or exception message if fclose() fails.
 *
 ******************************************************************************
 */
value
iclcloseasoutfp(char *whofor, char *filename)
{
    FILE *fp;

    fp = out_fp;
    if (restoreoutfp() == 0) {
	fclose(fp);
	systemfail("Did not expect restoreoutfp() to fail");
	return trueval;	/* for lint */
    }
    if (fclose(fp))
	return sys_exception2("CLOSEERR  %s: cannot close file \"%s\"",
			      whofor, filename);
    else
	return trueval;
}

/******************************************************************************
 *
 *	S Y S T E M F A I L (char *mess)
 *
 * Output a system failure message (just prior to an abnormal exit)
 *
 ******************************************************************************
 */
void
systemfail(char *mess)
{
    fprintf(stderr, "\n System Fail : %s\n", mess);
/*
 * Reset signal handlers and terminate with core
 */
    signal(SIGCHLD, SIG_DFL);

    abort();
    return; /* for lint */
}

/******************************************************************************
 *
 *	O U T S T R I N G (char *mess)
 *
 * Output the string 'mess' to the terminal immediately using the ICL
 * iosubsystem (or to stderr if not in terminal_output mode)
 *
 ******************************************************************************
 */
int
outstring(char *mess)
{
    int istat;

    if (terminal_output()) {
	message.fcode = IO_COMMAND_OUTPUT;
	message.iarg = outputcommandnumber++;
	strcpy(message.buff, mess);
	message.buflen = strlen(message.buff) + 1;
	istat = write(iocommand_fd, &message, iocommandlength(message));
    } else
	istat = printf("%s", mess);
    return istat;
}

/******************************************************************************
 *
 *	B U F S T R I N G  (char *mess)
 *
 * Appends the string 'mess' to the internal output buffer 'outbuffp'.
 * If the buffer overflows it is flushed to the terminal or to stderr if
 * we are not in terminal mode.
 *
 ******************************************************************************
 */
int
bufstring(char *mess)
{
    int i, istat = 0;

    i = strlen(mess);
    if (outbuffp + i > (IOBUFFERLENGTH - 2)) {
	if (terminal_output()) {
	    buffout.fcode = IO_COMMAND_OUTPUT;
	    buffout.iarg = outputcommandnumber++;
	    buffout.buflen = outbuffp + 1;
	    istat = write(iocommand_fd, &buffout, iocommandlength(buffout));
	} else
	    istat = printf( "%s", buffout.buff);
	outbuffp = 0;
    }
    strcpy(buffout.buff + outbuffp, mess);
    outbuffp += i;
    return istat;
}

/******************************************************************************
 *
 *	B U F C H A R (char ch)
 *
 * Appends the character 'char' to the internal output buffer 'outbuffp'.
 * If the buffer overflows it is flushed to the terminal or to stderr if
 * we are not in terminal mode.
 *
 ******************************************************************************
 */
int
bufchar(char ch)
{
    int istat = 0;

    if (outbuffp + 1 > (IOBUFFERLENGTH - 2)) {
	if (terminal_output()) {
	    buffout.fcode = IO_COMMAND_OUTPUT;
	    buffout.iarg = outputcommandnumber++;
	    buffout.buflen = outbuffp + 1;
	    istat = write(iocommand_fd, &buffout, iocommandlength(buffout));
	} else
	    istat = printf( "%s", buffout.buff);
	outbuffp = 0;
    }
    buffout.buff[outbuffp++] = ch;
    buffout.buff[outbuffp] = '\0';

    return istat;
}

/******************************************************************************
 *
 *	B U F N E W L I N E (void)
 *
 * Appends a newline to the internal output buffer 'outbuffp'.
 * If the buffer overflows it is flushed to the terminal or to stderr if
 * we are not in terminal mode.
 *
 ******************************************************************************
 */
int
bufnewline(void)
{
    int istat = 0;

    if (outbuffp + 1 > (IOBUFFERLENGTH - 2)) {
	if (terminal_output()) {
	    buffout.fcode = IO_COMMAND_OUTPUT;
	    buffout.iarg = outputcommandnumber++;
	    buffout.buflen = outbuffp + 1;
	    istat = write(iocommand_fd, &buffout, iocommandlength(buffout));
	} else
	    istat = printf( "%s", buffout.buff);
	outbuffp = 0;
    }
    buffout.buff[outbuffp++] = '\n';
    buffout.buff[outbuffp] = '\0';

    return istat;
}

/******************************************************************************
 *
 *	F L S H B U F (void)
 *
 * Flushes the internal output buffer 'outbuffp' to the terminal (or to stderr
 * if we are not in terminal mode)
 *
 ******************************************************************************
 */
int
flshbuf(void)
{
    int istat = 0;

    if (outbuffp != 0) {
	if (terminal_output()) {
	    buffout.fcode = IO_COMMAND_OUTPUT;
	    buffout.iarg = outputcommandnumber++;
	    buffout.buflen = outbuffp + 1;
	    istat = write(iocommand_fd, &buffout, iocommandlength(buffout));
	} else
	    istat = printf( "%s", buffout.buff);
    }
    outbuffp = 0;
    buffout.buff[0] = '\0';

    return istat;
}

/******************************************************************************
 *
 *	O U T F P I N T (int i)
 *
 ******************************************************************************
 */
int
outfpint(int i)
{
    int fid, istat;

    fid = fileno(out_fp);
    if (terminal_output() && fid < 3) { /* we are sending to terminal */
	sprintf(sbuff, "%d", i);
	istat = bufstring(sbuff);
    } else
	istat = fprintf(out_fp, "%d", i);
    return istat;
}

/******************************************************************************
 *
 *	O U T F P R E A L (double d)
 *
 ******************************************************************************
 */
int
outfpreal(double d)
{
    int fid, istat;

    fid = fileno(out_fp);
    if (terminal_output() && fid < 3) {
	sprintf(sbuff, "%#g", d);
	istat = bufstring(sbuff);
    } else
	istat = fprintf(out_fp, "%#g", d);
    return istat;
}

/******************************************************************************
 *
 *	O U T F P S T R I N G (char *mess)
 *
 ******************************************************************************
 */
int
outfpstring(char *mess)
{
    int fid, istat;

    fid = fileno(out_fp);
    if (terminal_output() && fid < 3) /* we are sending to terminal */
	istat = bufstring(mess);
    else
	istat = fprintf(out_fp, "%s", mess);
    return istat;
}

/******************************************************************************
 *
 *	O U T F P C H A R (char ch)
 *
 ******************************************************************************
 */
int
outfpchar(char ch)
{
    int fid, istat;

    fid = fileno(out_fp);
    if (terminal_output() && fid < 3) /* we are sending to terminal */
	istat = bufchar(ch);
    else
	istat = fprintf(out_fp, "%c", ch);
    return istat;
}

/******************************************************************************
 *
 *	O U T F P F O R M A T S T R I N G (char *form, char *mess)
 *
 ******************************************************************************
 */
int
outfpformatstring(char *form, char *mess)
{
    int fid, istat;

    fid = fileno(out_fp);
    if (terminal_output() && fid < 3) { /* we are sending to terminal */
	sprintf(sbuff, form, mess);
	istat = bufstring(sbuff);
    } else
	istat = fprintf(out_fp, form, mess);
    return istat;
}

/******************************************************************************
 *
 *	I C L E M S _ F L U S H ( int *status )
 *
 * Flush EMS error messages and reset status to SAI__OK
 *
 ******************************************************************************
 */
void
iclems_flush(void)
{
    int parlen, messlen;
    char parstr[EMS__SZPAR];
    char mess[EMS__SZMSG];
    int status;

    emsStat(&status);
    if (status != SAI__OK) {
	strcpy( mess, "!! " );
	emsEload( parstr, &parlen, mess+3, &messlen, &status );
	while ( status != SAI__OK )
	{
	    bufstring(mess);
	    bufnewline();
	    emsEload( parstr, &parlen, mess+3, &messlen, &status );
	}
	flshbuf();
    }
}

/******************************************************************************
 *
 *	I N I T _ O U T P U T (void)
 *
 * Called from main() to initialise the output module
 *
 ******************************************************************************
 */
value
init_output(void)
{
    value val;

    if(!isatty(fileno(stderr)))
	return exception("SYSERR - stderr not a tty");
    out_fp = stderr;
    outfpstackcnt = 0;
    buffout.buff[0] = '\0';
    outbuffp = 0;
    if ((isexc(val = store_symbol("STDOUT", SYM_FILE,
				node_value(value_file((PORTPTR) stdout))))) ||
	(isexc(val = store_symbol("STDERR", SYM_FILE,
				node_value(value_file((PORTPTR) stderr))))))
	return val;
    else
	return trueval;
}
