/*GRFILEIO -- Fast low-level UNIX I/O routines
 * +
 *
 * GRFILEIO is a set of functions that makes fast, low-level Unix I/O routines
 * available to a Fortran program.
 *
 *-------
 * 2-Dec-92 - fastio.c: John L. Lillibridge, NOAA/NOS/OES Geosciences Lab
 * 11-Nov-93 - Addition of seekf and warning by Remko Scharroo, DUT/SSR&T
 * 17-May-94 - Nice manual
 * 13-Oct-94 - Bits not required by PGPLOT stripped out; routine names
 *            changed [TJP].
 * 09-Nov-94 - Tidied and ported to Cray [mcs] (untested).
 * 10-Nov-94 - Added GRFCH() routine to write FORTRAN CHARACTER sub-strings.
 * 19-Jun-95 - File name "-" means stdout.
 *-------
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef PG_PPU
#define GROFIL grofil_
#define GRWFIL grwfil_
#define GRCFIL grcfil_
#define GRWFCH grwfch_
#else
#define GROFIL grofil
#define GRWFIL grwfil
#define GRCFIL grcfil
#define GRWFCH grwfch
#endif

/*
 **&GROFIL -- Open file for writing with GRFILEIO
 *+
 *     FUNCTION GROFIL (FNAME)
 *     INTEGER GROFIL
 *     CHARACTER*(*) FNAME
 *
 * Opens file FNAME for writing.
 * GROFIL returns the file descriptor for use in subsequent calls to
 * grwfil or grcfil. If GROFIL is negative, an error occurred while
 * opening the file.
 *
 **
 * Usage:
 *
 *     FD = GROFIL ('output_file')
 *     CALL GRWFIL (FD, 4, ARRAY)
 *
 * Arguments:
 *  FNAME  (input) : File name of the input or output file
 *  GROFIL (output) : Contains the file descriptor on return. If GROFIL < 0
 *                   an error occurred while opening the file.
 *-
 */
int GROFIL(fname, fname_len)
     char *fname;
     int fname_len;
{
  char *name = fname;      /* C pointer to FORTRAN string */
  int   slen = fname_len;  /* Length of the FORTRAN string */
  char *buff=0;            /* Dynamically allocated copy of name[] */
  int fd = -1;             /* File descriptor to be returned */
/*
 * Determine how long the FORTRAN string is by searching for the last
 * non-blank character in the string.
 */
  while(slen>0 && name[slen-1]==' ')
    slen--;
/*
 * Dynamically allocate a buffer to copy the FORTRAN string into.
 */
  buff = (char *) malloc((slen+1) * sizeof(char));
  if(buff) {
/*
 * Make a C string copy of the FORTRAN string.
 */
    strncpy(buff, name, slen);
    buff[slen] = '\0';
/* 
 * Check for stdout.
 */
    if (slen == 1 && buff[0] == '-') {
      fd = 1;
    } else {
/*
 * Open the file and return its descriptor.
 */
      fd = open(buff, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    }
    free(buff);
  } else {
    fprintf(stderr, "grofil: Insufficient memory\n");
  };
  return fd;
}

/*
 **&GRCFIL -- Close file from GRFILEIO access
 *+
 *     FUNCTION GRCFIL (FD)
 *     INTEGER GRCFIL (FD)
 *
 * Closes the file with descriptor FD from GRFILEIO access. GRCFIL returns
 * 0 when properly closed. Otherwise, use PERRORF to report the error.
 * 
 * Usage:
 *      IOS = GRCFIL (FD)
 * or:
 *      CALL GRCFIL (FD)
 *
 * In the last case the return code is ignored.
 *
 * Arguments:
 *  FD      (input) : File descriptor returned by GROFIL.
 *  GRCFIL (output) : Error code or 0 on proper closing.
 *-
 */
int GRCFIL(fd)
     int *fd;
{
  if ((*fd) == 1) {
    return 0;
  } else{
    return close(*fd);
  }
}

/*
 **&GRWFIL -- GRFILEIO write routine
 *+
 *     FUNCTION GRWFIL (FD, NBYTE, BUFFER)
 *     INTEGER FD, NBYTE, GRWFIL
 *     BYTE    BUFFER(NBYTE)
 *
 * Writes NBYTE bytes into the file associated by descriptor FD (which is
 * returned by the GROFIL call. The array BUFFER contains the data that has
 * to be written, but can (of course) also be associated with any other
 * string, scalar, or n-dimensional array.
 * The function returns the number of bytes actually written in GRWFIL. If
 * GRWFIL < 0, a write error occurred.
 *
 * Arguments:
 *  FD      (input) : File descriptor returned by GROFIL
 *  NBYTE   (input) : Number of bytes to be written
 *  BUFFER  (input) : Buffer containing the bytes that have to be written
 *  GRWFIL (output) : Number of bytes written, or (if negative) error code.
 *-
 */
int GRWFIL(fd, nbytes, buf)
     int *fd, *nbytes;
     char *buf;
{
  return write(*fd, (void *) buf, *nbytes);
}

/*
 **&GRWFCH -- GRFILEIO write FORTRAN character STRING routine
 *+
 *     FUNCTION GRWFCH (FD, STRING)
 *     INTEGER FD, GRWFCH
 *     CHARACTER*(*) STRING
 *
 * Writes NBYTE bytes into the file associated by descriptor FD (which is
 * returned by the GROFIL call). The string STRING contains the data that has
 * to be written.
 * The function returns the number of bytes actually written in GRWFCH. If
 * GRWFCH < 0, a write error occurred.
 *
 * Arguments:
 *  FD      (input) : File descriptor returned by GROFIL
 *  STRING  (input) : String containing the characterst to be written
 *  GRWFCH (output) : Number of bytes written, or (if negative) error code.
 *-
 */
int GRWFCH(fd, buf, buf_len)
     int *fd;
     char *buf;
     int buf_len;
{
  return write(*fd, (void *) buf, buf_len);
}
