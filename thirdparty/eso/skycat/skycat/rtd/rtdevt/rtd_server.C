/*******************************************************************************
* E.S.O. - VLT project
* 
* "@(#) $Id$"
* 
*  rtdServer.C
* 
* who          when      what
* --------     --------  ----------------------------------------------
* pbiereic     01/03/01  Adapted from previous version
*/
static const char* const rcsId="@(#) $Id$";

/************************************************************************
*   NAME
*      rtdServer - image event dispatcher for RTD
* 
*   SYNOPSIS
*      
*      rtdServer [-v <verbose>] [-p <port number>] [-t <delay>]
*
*   DESCRIPTION
*       
*   rtdServer is the process that manages all image events from cameras
*   which are forwared to RTD client(s) which display the images.
*   "Cameras" are acquisition processes like a CCD, IRACE-DCS, etc. which readout the
*   detector image.
*
*   Clients register (and connect) to the rtdServer via the rtdInitImageEvt() call.
*
*   Connected RTD clients will receive image events from a camera if they are attached
*   to this camera. If not, then image events are simply discarded.
*
*   Cameras use the rtdSendImageInfo() call when there is a new image to be displayed.
*
*   Several RTD clients can attach to the same camera as the multicasting
*   of event notification is supported by the rtdServer. 
*   RTD clients can also attach to cameras that not have registered yet
*   as the rtdServer supports a independence between image event producer
*   and image event consumer.
*   
*   The rtdServer also implements semaphore locking of shared memory, to
*   avoid the possibility of the RTD client reading the shared memory
*   at the same time as the camera writes (this is known as "image jitter").
*
*   The rtdServer expects the camera software to lock the semaphore.
*
*   The rtdServer will then increment this semaphore by the number of RTD
*   clients less one (one was already set by the camera). If semaphores
*   are not implemented in the incoming image event, no action is taken.
*   The overall locking scheme is discussed in more detail in rtdSem(3).
*
*   CAUTIONS
*
*   o The rtdServer must not be killed when other clients are still
*     connected to it.
*   o rtdServer should not be started when there is another instance running on
*     the same machine, since there is only one standard server port to which
*     clients can connect to. If it is nevertheless started, then it will delay
*     for some seconds before terminating.
*
*   ENVIRONMENTS
*
*   The rtdServer (and RTD clients) use 5555 as the default, standard port number.
*   The port number can be changed within a user session by setting the
*   environment variable RTD_SERVER_PORT before starting rtdServer and it's
*   clients.
*
*   SEE ALSO
*   rtdInitImageEvt(3), rtdSendImageInfo(3), rtdSem(3)
*------------------------------------------------------------------------
*/

/* 
 * System Headers
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>

/* 
 * Local Headers
 */
#include "rtdSERVER.h"

#define RTD_SERVER_DELAY 5     // default time to sleep before new events are read

typedef void (*MySigFunc)(int);  // prototype cast to keep Sun cc quiet

/*
 * Globals needed for cleanup() after signals
 */
int       socketFd  = 0;
rtdSERVER *mainLoop = NULL;     // rtdSERVER object


void usage(void)
{
    printf("Usage: rtdServer ?-v -p -s?\n"
	   "  -v  verbose mode\n"
	   "  -p  port number, default %d. Set with RTD_SERVER_PORT\n"
	   "  -t  delay between image events in msec (default %d)\n",
	   RTD_FALLBACK_PORT, RTD_SERVER_DELAY);
    exit(1);
}

/*
 * cleanup resources before terminating. Note that rtdServer does not
 * create any "global resources" such as shared memory or semaphores.
 */
void cleanup(int sig=0)
{
    close(socketFd);
    if (mainLoop != NULL)
	delete mainLoop;

    if (sig >= 0)
	fprintf(stderr, "rtdServer: signal received\n");
    exit(0);
}

main(int argc, char *argv[])
{
    extern char *optarg;
    extern int  optind;
    int         portNo = 0;
    int         delay = RTD_SERVER_DELAY;
    char        c;
    int         verbose = 0;

/*
 * rtdServer is a central server for all cameras and RTD's on
 * a host machine. It only terminates after certain signals, such as 
 * an interrupt from keyboard and signals which cannot be caught.
 * See the list of signals and their action below.
 */

    // signals which are ignored:
    signal(SIGTERM, SIG_IGN);
    signal(SIGHUP,  SIG_IGN);
    signal(SIGUSR1, SIG_IGN);
    signal(SIGUSR2, SIG_IGN);

    // signals which must terminate rtdServer:
    signal(SIGINT,  (MySigFunc)cleanup);
        
    // parse command line options
    while ((c = getopt(argc, argv, "v:p:t:")) != -1) {
#ifndef SYSV
	char* optopt = argv[optind];
#endif
	switch(c) {
	case 'v':
	    verbose = 1;
	    break;
	case 'p':
	    portNo = atoi(optarg);
	    break;
	case 't':
	    delay = atoi(optarg);
	    break;
	case ':':
	    fprintf(stderr,"Option -%s requires an argument\n",(char *)optopt);
	    usage();
	case '?':
	    usage();
	case 'h':
	    usage();
	}
    }
    // Check argument parameters
    if (portNo < 0 || delay < 0)
	usage();

    rtdLOG logs = rtdLOG::rtdLOG(verbose);  // create log object

    if (getenv(RTD_SERVER_PORT) != NULL)
	portNo = atoi(getenv(RTD_SERVER_PORT));

    if (rtdInitServer(&socketFd, portNo, NULL) == RTD_ERROR) {
	fprintf(stderr, 
		"Could not initialize server (maybe it is already running ?)\n"
		"Now sleeping for 10 seconds to avoid an automatic, immediate restart\n");
	sleep(10);
	exit (1);
    }
    logs.log("rtdServer started.\n");

    mainLoop = new rtdSERVER(verbose, socketFd, delay);
    mainLoop->Loop();
    cleanup(-1);
}
