/*
 * E.S.O. - VLT project 
 * $Id: tRtd.C,v 1.8 1998/07/22 19:57:38 abrighto Exp $
 *
 * tRtd.C - test RTD real-time updates by sending image and rapid frame
 *          
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  16 Jul 96  Created
 * P.Biereichel    16 Sept 97 Use semaphores for image event synchronization
 */

#include <stdio.h>
#include <iostream.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include "rtdSem.h"
#include "error.h"
#include "config.h"
#include "rtdImageEvent.h"

extern char *optarg;

// some protos missing in SunOS
#ifdef NEED_SOCKET_PROTO
extern "C" {
    int select (int width, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, timeval *timeout);
    void bzero(char *b, int length);
	   }
#endif /* NEED_SOCKET_PROTO */

extern "C" {
    void *shmat(int shmid, void *shmaddr, int shmflg);
    int shmdt(void *shmaddr);
	   }

// structures for multibuffering/semaphore convenience routines
static rtdShm main1, rapid1;



/*
 * cleanup and exit
 */
static void cleanup(int i=0)
{
    i=1;
    sleep(1);
    rtdShmDelete(&main1);
    rtdShmDelete(&rapid1);
    exit(1);
}

/*
 * make error message and exit
 */
static void errexit(const char* msg1, const char* msg2 = "")
{
    cout << msg1 << msg2 << endl;
    cleanup(1);
}

/*
 * generate some dummy image data based on the given count
 */
static void gen_image(char* p, int width, int height, int count)
{
    int size = width*height;
    int k;
    for (int i = 0; i < width; i++) {
	for (int j = 0; j < height; j++) {
	    k = j*width+i;
	    p[k] = k+count*3;
	}
    }
}

/*
 * lock a shm buffer
 */
void lockShm(rtdShm *shmPtr, int semNum)
{
    struct timeval tm;                      // Timestamp structure for semaphore
    gettimeofday(&tm, NULL);
    shmPtr->timestamp[semNum] = tm.tv_sec + (tm.tv_usec / 1000000.);
    
    // lock the shm buffer
    struct sembuf semLock;
    memset(&semLock, '\0', sizeof(sembuf));
    semLock.sem_op = 1;                     // Increment [#] by one
    semLock.sem_flg = 0;

    semLock.sem_num = (unsigned short)semNum;
    semop(shmPtr->semId, &semLock, 1);
}

/*
 * fill image event info
 */    
void fillImageInfo(
    rtdIMAGE_INFO *imageInfo,
    int width,
    int height,
    int count, 
    int type,
    int x,
    int y,
    int frameId)
{
    imageInfo->dataType = type;
    imageInfo->frameX   = x;
    imageInfo->frameY   = y;
    imageInfo->xPixels  = width;
    imageInfo->yPixels  = height;
    imageInfo->frameId  = frameId;

    // add some dummy world coordinates for testing
    imageInfo->ra       = 49.951;
    imageInfo->dec      =  41.5117;
    imageInfo->secpix   = 1.70127;
    imageInfo->xrefpix  = width/2.0;
    imageInfo->yrefpix  = height/2.0 ;
    imageInfo->rotate   = double(count % 360);
    imageInfo->equinox  = 2000;
    imageInfo->epoch    = 1957.97;
    strcpy(imageInfo->proj, "-TAN");

#if 1
    // add some dummy detector information
    if (frameId == 0) {
	imageInfo->startX    = count;
	imageInfo->startY    = count;
	imageInfo->binningX  = 2;
	imageInfo->binningY  = 2;
	imageInfo->lowCut    = 0;
	imageInfo->highCut    = 100;
    }
#endif
}

/*
 * pause for the given time
 */
void rtdSleep(rtdIMAGE_EVT_HNDL& eventHndl, int msec)
{
    struct timeval time;
    struct fd_set readMask;
    FD_ZERO(&readMask);
    FD_SET(eventHndl.socket, &readMask);

    time.tv_sec = msec/1000;
    time.tv_usec = (msec%1000)*1000;

#ifdef HAVE_SELECT_FD_SET
    int status = select(32, (fd_set *)&readMask, 0, 0, &time);
#else
    int status = select(32, (int *)&readMask, 0, 0, &time);
#endif
}

/*
 * Cycle over all the shm buffers, checking to see if they're locked.
 */
int checkAllLocked(rtdShm *shmPtr, int *semNum, int numShm)
{
    int i, j=0;
    for (i = 0; i < numShm; i++) {
	j = (*semNum+i) % numShm;
	if (!rtdShmLocked(shmPtr, j))
	    break;
    }
    if (i == numShm)
	return 1;
    *semNum = j;
    return 0;
}

/*
 * send an image update for the given shared memory area.
 * The count is used to generate the image.
 */
static int update(
    char *frameName,
    int verbose,
    rtdIMAGE_EVT_HNDL& eventHndl, 
    rtdShm *shmPtr,
    int *semNum,
    int numShm,
    int frameId,
    int x, 
    int y,
    int width, 
    int height, 
    int *count) 
{
    // check if all shm buffers are locked
    if (checkAllLocked(shmPtr, semNum, numShm))
	return 1;

    // lock a buffer before writing data
    lockShm(shmPtr, *semNum);

    // set info in rtdIMAGE_INFO
    rtdIMAGE_INFO imageInfo;
    memset(&imageInfo, '\0', sizeof(rtdIMAGE_INFO));
    fillImageInfo(&imageInfo, width, height, *count, BYTE, x, y, frameId);
    rtdShmStruct(*semNum, &imageInfo, shmPtr);
    
    // attach to shm
    char *ptr = (char *)shmat(imageInfo.shmId, NULL, 0);
    if (ptr <= NULL)
	errexit("Unable to attach to shared memory");

    // generate some bytes
    gen_image(ptr, width, height, *count);

    // detach from shm
    shmdt(ptr);

    if (verbose)
	printf("update #%d %s frame: frameId=%d, %dx%d+%d+%d, buffer %d/%d\n", 
	       *count, frameName, frameId, width, height, x, y, *semNum+1, numShm);

    (*count)++;

    // send image event to rtdServer
    if (rtdSendImageInfo(&eventHndl, &imageInfo, NULL) != 0)
        errexit("error from rtdSendImageInfo");
    return 1;
}

/* 
 * Main:
 */
main(int argc, char** argv) 
{
    rtdIMAGE_EVT_HNDL eventHndl;

    // current semaphore numbers
    int semNumMain=0, semNumRapid=0;

    // name of camera
    char* rtd_camera = getenv("RTD_CAMERA");

    // verbose flag: if true, print messages
    int verbose = 0;

    // data type
    int dataType = BYTE;
    
    // delay between updates
    int timer = 500;

    // number of shm buffers to use
    int numShm = 2;
    
    // pos, width and height of images
    int rapid_x = 0, rapid_y = 0;
    int main_width = 255, main_height = 255;
    int rapid_width = 50, rapid_height = 50;

    // frame ids 
    int main_id = 0, rapid_id = 0;

    char* usage = "usage: tRtd ?-x x -y y -w width -h height -f rapidFrameId -v verbose -c camera -t msecs -b numShmBuffers?";
    int c;
    while ((c = getopt(argc, argv, "x:y:w:h:f:b:v:c:t:l:")) != -1) {

	switch(c) 
	{
	case 'x': 
	    rapid_x = atoi(optarg);
	    break;
	case 'y': 
	    rapid_y = atoi(optarg);
	    break;
	case 'w': 
	    rapid_width = atoi(optarg);
	    break;
	case 'h': 
	    rapid_height = atoi(optarg);
	    break;
	case 'f': 
	    rapid_id = atoi(optarg);
	    break;
	case 'v': 
	    verbose = atoi(optarg);
	    break;
	case 'c': 
	    rtd_camera = optarg; 
	    break;
	case 't': 
	    timer = atoi(optarg);
	    break;
        case 'b': 
            numShm = atoi(optarg);
            break;
	case ':':
	    cout << usage << endl;
	    exit(0);
        default:
	    cout << usage << endl;
	    exit(0);
	}
    }

    if (! rtd_camera) 
	errexit("please use '... -c camera' or 'setenv RTD_CAMERA ...' first");
    
    if (rtdInitImageEvt(rtd_camera, &eventHndl, NULL) != 0) 
	errexit("Could not initialize image event: check if rtdServer is running");
    
    // clean up shared memory on exit
    signal(SIGINT, cleanup);
    signal(SIGTERM, cleanup);
    signal(SIGHUP, cleanup);

    // create shared memory areas (main image)
    if (rtdShmCreate(numShm, &main1, main_width, main_height, dataType) == -1)
        errexit("error creating shared memory");

    // create shared memory areas (rapid frame)
    if (rtdShmCreate(numShm, &rapid1, rapid_width, rapid_height, dataType) == -1)
        errexit("error creating shared memory");

    int countMain = 0, countRapid = 0, status = 0;

    while(1) {
	if (rapid_id)
	    status = update("rapid", verbose, eventHndl, &rapid1, &semNumMain, numShm,
			   rapid_id, 0, 0, rapid_width, rapid_height, &countRapid);	    

	// do main image every 10 times through the loop
	if (countRapid % 10 == 0)
	    status = update("main", verbose, eventHndl, &main1, &semNumRapid, numShm,
			   main_id, 0, 0, main_width, main_height, &countMain);
	
	rtdSleep(eventHndl, timer);
    }
    return 0;
}

