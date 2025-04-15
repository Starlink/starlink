/*
*+
*  Description:
*
*     This routine opens a file.  Up to four
*     attempts may be made to open the file.  If a null response is
*     supplied the file is not opened, and the flag returned indicates
*     this fact.

*  Invocation:
*
*     file = flu_asfio(pnfile, acmode, exclaim,
*        pathname, pathname_size, status);

*  Arguments:
*
*     pnfile = CHARACTER*(*)
*        Parameter name by which file is to be opened
*     acmode = CHARACTER*(*)
*        Expression giving the required access mode.
*        Valid modes are: 'WRITE', 'UPDATE', 'APPEND'..
*     exclaim = LOGICAL( WRITE )
*        If true then the user input was '!'.
*     pathname = char*
*        If not NULL, a string into which to write the pathname.
*     pathname_size = size_t
*        Size of the string to which pathname is a pointer.
*     status = INTEGER( READ, WRITE )
*        Global status value

*  Returned Value:
*
*     A FILE pointer on success, NULL otherwise.

*  Method:
*
*     Check for error on entry - return if not o.k.
*     Initialise looping flag
*     Do while no error obtaining the name and opening the output file
*       and maximum number of attempts not exceeded
*        Get file name and open file
*        If null returned then
*            Set flag so that a log file will not be created
*            Annul the error
*            Exit from the loop
*        Else if error occurred then
*            If abort requested, do so
*            Increment loop counter
*            If maximum number of attempts not exceeded then
*               Report error
*            Else
*               Set looping flag to exit
*            Endif
*            Cancel parameter used to get filename
*        Else
*            Set flag to indicate that the file has been opened
*            Set looping flag to false
*        Endif
*     Enddo
*     If error then
*        Report and abort
*     Endif
*     Return

*  Bugs:
*
*     None known.

*  Authors:
*
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)

*  History:
*
*     1989 Jul 25: Original (RL.STAR::CUR).
*     1990 Feb 20: Renamed from AIF_OPFIO (RAL::CUR).
*     1994 Mar 1: Modified to return EXCLAIM (CARDIFF::GJP).
*-
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "merswrap.h"
#include "parwrap.h"
#include "par_err.h"
#include "sae_par.h"

#include "flu.h"

FILE* flu_asfio(
        char* pnfile, char* acmode, int* exclaim,
        char* pathname, size_t pathname_size, int* status) {
    /* Local Variables */
    FILE* fd = 0;             /* File descriptor */
    int mxloop = 4;           /* Maximum number of attempts at opening a data file */
    int loop = 0;             /* Number of attempts to open the file */
    int loopag = 1;           /* Loop again to open output file. */
    char* mode;
    char path[129];
    char aline[129];
    char path2[129];
    char* directory;

    /* Check status on entry - return if not o.k. */
    if (*status != SAI__OK) return 0;

    /* Initialise */
    *exclaim = 0;

    /* If $FLUXPWD is set we use it, else we write to CWD */
    directory = getenv("FLUXPWD");
    if (directory) {
        strncpy(path, directory, sizeof(path));
    }
    else {
        getcwd(path, sizeof(path));
    }
    if (*status != SAI__OK) return 0;

    if (! strcmp(acmode, "APPEND")) {
        mode = "a";
    }
    else if (! strcmp(acmode, "WRITE")) {
        mode = "w";
    }
    else {
        mode = "w";
    }

    while (loopag) {
        /* Access the file (appending original directory name). */
        parGet0c(pnfile, aline, sizeof(aline), status);
        if (aline[0] == '/') {
            strncpy(path2, aline, sizeof(path2));
        }
        else {
            snprintf(path2, sizeof(path2), "%s/%s", path, aline);
        }
        parPut0c(pnfile, path2, status);

        if (*status == SAI__OK) {
            fd = fopen(path2, mode);
            if (! fd) {
                *status = SAI__ERROR;
            }
            else if (pathname) {
                strncpy(pathname, path2, pathname_size);
            }
        }

        /* Check the status value. */
        if (*status == PAR__NULL) {
            loopag = 0;
            *exclaim = 1;
            errAnnul(status);
        }
        else if (*status != SAI__OK) {
            if (*status == PAR__ABORT) return 0;

            /* Here if filename is not allowed or file is not opened
             * - try again
             * Need to flush error here, as not quitting routine */

            loop ++;
            if (loop <= mxloop) {
                msgSetc("FILNAM", path2);
                errRep("ERR_AIF_ASFIO_NOFI",
                        "AIF_ASFIO: Could not open file ^FILNAM - try again",
                        status);
                errFlush(status);
            }
            else {
                /* end looping as user is having serious problems */
                loopag = 0;
            }

            parCancl(pnfile, status);
        }
        else {
            /* no problem, so exit loop */
            loopag = 0;

            /* end of file-opened-successfully check */
        }
    }

    /* abort for repeated error */

    if (*status != SAI__OK) {
        errRep("ERR_AIF_ASFIO_NOOPEN",
                "AIF_ASFIO: Repeatedly unable to open a file.", status);
    }

    return fd;
}
