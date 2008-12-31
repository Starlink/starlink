/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>

#define	import_kernel
#define	import_knames
#define import_prtype
#define	import_zfstat
#define import_spp
#include <iraf.h>

extern	int errno;		/* error code returned by the kernel	*/
#ifdef SYSV
#define	vfork	fork
#endif

/* ZFIOPR -- File i/o to a subprocess.  A "connected" subprocess is connected
 * to the parent via two IPC channels (read and write), and is analogous to a
 * streaming binary file:
 *
 *		zopcpr		open process
 *		zclcpr		close process
 *		zardpr		read from process
 *		zawrpr		write to process
 *		zawtpr		wait for i/o
 *		zsttpr		get channel/device status
 * also
 *		zintpr		interrupt process
 *
 * See also the zopdpr, zcldpr primitives, used to spawn "detached" processes
 * which do not communicate with the parent.
 */

#define	IPC_MAGIC	01120		/* First 2 bytes of IPC block	  */
#define	SZ_TTYIBUF	512		/* buffer size if reading TTY	  */
#define	SZ_TTYOBUF	2048		/* buffer size if writing TTY	  */
#define	MAX_TRYS	2		/* max interupted reads		  */

#define	mask(s)		(1 << ((s) - 1))

int	pr_ionbytes[MAXOFILES];		/* nbytes read|written on channel */
int	debug_ipc = 0;			/* print debug info on stderr	  */
int	ipc_in = 0;			/* logfile for IPC input	  */
int	ipc_out = 0;			/* logfile for IPC output	  */
int	ipc_isatty = 0;			/* set when debugging IPC at TTY  */


#if 0
/* ZOPCPR -- Open a connected subprocess.  Spawn process and open bidirectional
 * IPC channels, implemented with pipes for this version of Berkeley UNIX.
 */
ZOPCPR (osfn, inchan, outchan, pid)
PKCHAR	*osfn;			/* name of executable file		*/
XINT	*inchan, *outchan;	/* IPC channels (parent reads inchan)	*/
XINT	*pid;
{
	int	pin[2], pout[2];
	int	maxforks = 3, fd;

	if (debug_ipc)
	    fprintf (stderr, "zopcpr (`%s')", (char *)osfn);

	/* Check that the process file exists and is executable.
	 */
	if (access ((char *)osfn, 1) == ERR) {
	    *pid = XERR;
	    return;
	}

	/* Open binary IPC channels.  Clear byte counts.
	 */
	pipe (pin);
	if (pipe (pout) == ERR) {
	    *pid = XERR;
	    return;
	}

	pr_ionbytes[pin[0]] = 0;
	pr_ionbytes[pin[1]] = 0;
	pr_ionbytes[pout[0]] = 0;
	pr_ionbytes[pout[1]] = 0;

	/* Create child process.  Vfork is used to avoid necessity to copy
	 * the full address space of the parent, since we are going to overlay
	 * a new process immediately with Execl anyhow.  The child inherits
	 * the open stdio files.  The fork can fail if swap space is full or
	 * if we have too many processes.
	 */
	while ((*pid = vfork()) == ERR) {
	    if (--maxforks == 0) {
		close (pin[0]);  close (pin[1]);
		close (pout[0]); close (pout[1]);
		*pid = XERR;
		return;
	    }
	    sleep (2);
	}

	if (*pid == 0) {
	    /* New, child process.  Make child think the pipe is its stdin/out.
	     */
	    close (pin[0]);
	    close (pout[1]);
	    close (0);  dup (pout[0]);  close (pout[0]);
	    close (1);  dup (pin[1]);   close (pin[1]);

	    /* Disable SIGINT so that child process does not die when the
	     * parent process is interrupted.  Parent sends SIGTERM to
	     * interrupt a child process.
	     */
	    signal (SIGINT, SIG_IGN);

	    /* Arrange for the local file descriptors of the parent to be
	     * closed in the child if the exec succeeds.  IRAF subprocesses
	     * do not expect to inherit any file descriptors other than
	     * stdin, stdout, and stderr.
	     */
	    for (fd=3;  fd < getdtablesize();  fd++)
		fcntl (fd, F_SETFD, 1);

	    /* Exec the new process.  Will not return if successful.
	     * The "-c" flag tells the subprocess that it is a connected
	     * subprocess.
	     */
	    execl ((char *)osfn, (char *)osfn, "-c", 0);

	    /* If we get here the new process could not be executed for some
	     * reason.  Shutdown, calling _exit to avoid flushing parent's
	     * io buffers.  Parent will receive the X_IPC exception when it
	     * subsequently tries to write to the child.
	     */
	    _exit (1);

	} else {
	    /* Existing, parent process. */
	    close (pin[1]);
	    close (pout[0]);
	    *inchan  = pin[0];
	    *outchan = pout[1];

	    /* Save pid in parent's process table.  Entry cleared when
	     * pr_wait is called to wait for process to terminate.  Also save
	     * channel numbers in process table since only the pid is passed
	     * when the process is closed.
	     */
	    pr_enter (*pid, pin[0], pout[1]);

	    if (debug_ipc)
		fprintf (stderr, " [%d]\n", *pid);
	}
}


/* ZCLCPR -- Close a connected subprocess.  Wait for subprocess to terminate,
 * close the IPC channels, and return the exit status.
 */
ZCLCPR (pid, exit_status)
XINT	*pid;
XINT	*exit_status;
{
	int	inchan, outchan;

	if (pr_getipc ((int)*pid, &inchan, &outchan) == ERR)
	    *exit_status = XERR;
	else {
	    close (outchan);
	    close (inchan);
	    *exit_status = pr_wait ((int)*pid);
	}

	if (debug_ipc)
	    fprintf (stderr, "[%d] terminated, exit code %d\n",
		*pid, *exit_status);
}
#endif

/* ZARDPR -- Read next record from an IPC channel.  Since UNIX pipes are byte
 * streams we must take special measures to transmit data through a pipe in
 * records.  Each block of data is preceded by a 4-byte header consisting
 * of a 2-byte magic number (used to verify that the correct protocol is in
 * use on the channel) and a 2-byte count of the number of bytes in the block.
 * To read a block we must read the count and then issue successive read
 * requests until the entire block has been read.  The byte count (excluding the
 * 4-byte header) is saved in a static table for return to the high level code
 * in a subsequent call to ZAWTPR.  Disaster occurs if the actual block length
 * does not agree with the header, but that cannot happen since only ZAWRPR
 * writes to an IPC channel.
 */
ZARDPR (chan, buf, maxbytes, loffset)
XINT	*chan;
XCHAR	*buf;
XINT	*maxbytes;
XINT	*loffset;		/* not used */
{
	register char *op;
	register int fd, nbytes;
	int	sigmask_save, record_length, status;
	short	temp;

	fd = *chan;
	op = (char *)buf;

	if (debug_ipc)
	    fprintf (stderr,
		"[%d] initiate read for %d bytes from IPC channel %d\n",
		getpid(), *maxbytes, fd);

	/* In TTY debug mode we simulate IPC i/o but are actually talking to
	 * a terminal.  Omit the packet headers and unpack input into XCHAR.
	 * If a interrupt ocurrs while the read is pending and control returns
	 * to the do-while, try to complete the read successfully.
	 */
	if (ipc_isatty) {
	    char	ibuf[SZ_TTYIBUF], *ip;
	    XCHAR	*op;
	    int		maxch = min (SZ_TTYIBUF, *maxbytes / sizeof(XCHAR));
	    int		ntrys = MAX_TRYS;

	    do {
		errno = 0;
		if ((pr_ionbytes[fd] = nbytes = read (fd, ibuf, maxch)) > 0) {
		    for (ip=ibuf, op=buf;  --nbytes >= 0;  )
			*op++ = *ip++;
		    pr_ionbytes[fd] *= sizeof (XCHAR);
		}
	    } while (nbytes <= 0 && errno == EINTR && --ntrys >= 0);

	    return;
	}

	/* Read 2-byte magic number to verify that the channel was written to
	 * by ZAWRPR and that we are at the start of a record.
	 */
	switch (status = read (fd, &temp, 2)) {
	case 0:
	    pr_ionbytes[fd] = 0;
	    return;
	case ERR:
	    pr_ionbytes[fd] = ERR;
	    return;
	default:
	    if (status != 2 || temp != IPC_MAGIC) {
		pr_ionbytes[fd] = ERR;
		return;
	    }
	}

	if (ipc_in > 0)
	    write (ipc_in, (char *)&temp, 2);

	/* Get byte count of record.
	 */
	if (read (fd, &temp, 2) != 2) {
	    pr_ionbytes[fd] = ERR;
	    return;
	}
	record_length = temp;
	nbytes = min (record_length, *maxbytes);
	pr_ionbytes[fd] = nbytes;
	if (ipc_in > 0)
	    write (ipc_in, (char *)&temp, 2);

	/* Now read exactly nbytes of data from channel into user buffer.
	 * Return actual byte count if EOF is seen.  If ERR is seen return
	 * ERR.  If necessary multiple read requests are issued to read the
	 * entire record.  This is implemented as a critical section to
	 * prevent corruption of the IPC protocol when an interrupt occurs.
	 */
#ifdef sun
#else
	sigmask_save = sigblock (mask(SIGINT) | mask(SIGTERM));
#endif

	while (nbytes > 0)
	    switch (status = read (fd, op, nbytes)) {
	    case 0:
		pr_ionbytes[fd] -= nbytes;
		goto reenab_;
	    case ERR:
		pr_ionbytes[fd] = ERR;
		goto reenab_;
	    default:
		nbytes -= status;
		op += status;
	    }

	if (debug_ipc) {
	    fprintf (stderr, "[%d] read %d bytes from IPC channel %d:\n",
		getpid(), op - (char *)buf, fd);
	    write (2, (char *)buf, op - (char *)buf);
	}

	if (ipc_in > 0)
	    write (ipc_in, (char *)buf, op - (char *)buf);

	/* If the record is larger than maxbytes, we must read and discard
	 * the additional bytes.  The method used is inefficient but it is
	 * unlikely that we will be called to read less than a full record.
	 */
	for (nbytes = *maxbytes;  nbytes < record_length;  nbytes++)
	    if (read (fd, &temp, 1) <= 0)
		break;
reenab_:
#ifdef sun
;
#else
	sigsetmask (sigmask_save);
#endif
}


/* ZAWRPR -- Write to an IPC channel.  Write the IPC block header followed by
 * the data block.
 */
ZAWRPR (chan, buf, nbytes, loffset)
XINT	*chan;
XCHAR	*buf;
XINT	*nbytes;
XINT	*loffset;
{
	register int fd;
	short	temp;
	int	sigmask_save;

	fd = *chan;

	/* In TTY debug mode we simulate IPC i/o but are actually talking to
	 * a terminal.  Omit the packet headers and pack XCHAR output into
	 * bytes.
	 */
	if (ipc_isatty) {
	    char	obuf[SZ_TTYOBUF], *op;
	    XCHAR	*ip;
	    int		nchars, n;

	    n = nchars = min (SZ_TTYOBUF, *nbytes / sizeof(XCHAR));
	    for (ip=buf, op=obuf;  --n >= 0;  )
		*op++ = *ip++;
	    if ((pr_ionbytes[fd] = write (fd, obuf, nchars)) > 0)
		pr_ionbytes[fd] *= sizeof (XCHAR);
	    return;
	}

	/* Write IPC block header.
	 */
#ifdef sun
#else
	sigmask_save = sigblock (mask(SIGINT) | mask(SIGTERM));
#endif

	temp = IPC_MAGIC;
	write (fd, &temp, 2);
	if (ipc_out > 0)
	    write (ipc_out, &temp, 2);
	temp = *nbytes;
	write (fd, &temp, 2);
	if (ipc_out > 0)
	    write (ipc_out, &temp, 2);

	/* Write data block.
	 */
	pr_ionbytes[fd] = write (fd, (char *)buf, (int)*nbytes);
	if (ipc_out > 0)
	    write (ipc_out, (char *)buf, (int)*nbytes);

#ifdef sun
#else
	sigsetmask (sigmask_save);
#endif

	if (debug_ipc) {
	    fprintf (stderr, "[%d] wrote %d bytes to IPC channel %d:\n",
		getpid(), (int)*nbytes, fd);
	    write (2, (char *)buf, (int)*nbytes);
	}
}


/* ZAWTPR -- Wait for i/o to an IPC channel.  Since UNIX pipes are not
 * asynchronous we do not really wait, rather we return the status value
 * (byte count) from the last read or write to the channel.
 */
ZAWTPR (chan, status)
XINT	*chan;
XINT	*status;
{
	if ((*status = pr_ionbytes[*chan]) == ERR)
	    *status = XERR;
}


/* ZSTTPR -- Get binary file status for an IPC channel.  An IPC channel is a
 * streaming binary file.
 */
ZSTTPR (chan, param, lvalue)
XINT	*chan;		/* not used; all IPC channels have same status */
XINT	*param;
XINT	*lvalue;
{
	switch (*param) {
	case FSTT_BLKSIZE:
	case FSTT_FILSIZE:
	    *lvalue = 0;
	    break;
	case FSTT_OPTBUFSIZE:
	    *lvalue = PR_OPTBUFSIZE;
	    break;
	case FSTT_MAXBUFSIZE:
	    *lvalue = PR_MAXBUFSIZE;
	    break;
	default:
	    *lvalue = XERR;
	}
}
