/*
 * E.S.O. - VLT project / ESO Archive
 * "@(#) $Id: trtdRemote.c,v 1.1.1.1 2006/01/12 16:38:03 abrighto Exp $"
 *
 * trtdRemote.C - test cases for remote interface to the RTD
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  6 Mar 96  Created
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include "rtd_remote.h"


/* 
 * declare an optional error handler to print out error messages
 * automatically.
 */
static void errorHandler(char* msg)
{
    /* this part is just added for the sake of testing */
    if (strcmp(rtdRemoteGetError(), msg) != 0) {
	printf("errorHandler test failed\n");
    }

    printf("trtdRemote: %s\n", msg);
}


/*
 * this routine is used for convenience in testing below Send the command
 * to the rtdimage, then print and return the result.
 */
static char* send_rtd(char* cmd) 
{
    char* result = NULL;
    int status = rtdRemoteSend(cmd, &result); 
    printf("%s ==> %s: %s\n", cmd, (status ? "FAILED" : "OK"), result);
    return result;
}


/* 
 * return a shared memory pointer given the id
 */
static void* get_shm(int shmId)
{
    void* ptr = shmat(shmId, NULL, 0); 
    if (ptr == NULL || ptr == (void *)-1) {
	printf("Invalid shared memory id specified: %d", shmId);
	return NULL;
    }
    printf("shmId %d ==> %x\n", shmId, ptr);
    return ptr;
}

               
/* 
 * do some tests on the shared memory from the Rtd image header and data
 * The arguments should be the shared memory Ids.
 */
static void test_shm(int data_id, int header_id)
{
    void* data = get_shm(data_id);
    char* header = (char*)get_shm(header_id);
    char buf[81];
    int i, j, n, w, h, bitpix;
    char* p;
    short* rawimage;
    
    if (header) {
	printf("got image header (%d):\n", header_id);
	p = header;
	for(i = 0; i < 10; i++) {
	    for (j=0; j<80; j++) {
		printf("%c", *p++);
	    }
	    printf("\n");
	}
	printf("...\n");
    }

    if (data) {
	printf("got image data (%d)\n", data_id);
	
	/* do something to the data... */
	w = atoi(send_rtd("width"));
	h = atoi(send_rtd("height"));
	bitpix = atoi(send_rtd("bitpix"));

	if (bitpix == 16) {
	    printf("modifying 'short' raw data (mult by 2):\n");
	    rawimage = (short*)data;
	    for(i = 0; i < w; i++) {
		for (j=0; j<h; j++) {
		    rawimage[j*h+i] *= 2;
		}
	    }
	    /* cause rtd to update the display */
	    send_rtd("shm update");
	}
    }
}


main()
{
    char* result;
    int status, data_id, header_id;

    /* cause errors to be printed */
    rtdRemoteSetErrorHandler(errorHandler);
  
    /* connect to running rtd: uses default args taken from ~/.rtd-remote file */
    if (rtdRemoteConnect(0, NULL, 0) != 0) 
	exit(1);

    /* send some commands to RTD to be evaluated */
    send_rtd("wcscenter");
    send_rtd("bitpix");
    send_rtd("scale");
    send_rtd("width");
    send_rtd("height");
    send_rtd("config -file ngc1316r.fits");
    send_rtd("width");
    send_rtd("height");

    data_id = atoi(send_rtd("shm get data"));
    header_id = atoi(send_rtd("shm get header"));

    /* look at the shared memory */
    test_shm(data_id, header_id);

    exit(0);
}

