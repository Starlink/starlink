/*************************************************************************
* E.S.O. - VLT project
* "@(#) $Id: rtdCubeDisplay.c,v 1.1.1.1 2006/01/12 16:39:59 abrighto Exp $"
*  rtdClient.c
*
* who       when      what
* --------  --------  ----------------------------------------------
* T.Herlin  08/02/95  Created
* P.W. Draper 16/12/97  Modified to use fd_set as a type rather than
*                       struct.
*             12/09/01 Added UKIRT Quick Look member initialisations, should
*                      be harmless to other uses.
*/
static const char* const rcsId="@(#) $Id: rtdCubeDisplay.c,v 1.1.1.1 2006/01/12 16:39:59 abrighto Exp $";

/************************************************************************
*   NAME
*      rtdCubeDisplay - simple FITS cube display program
* 
*   SYNOPSIS
*      
*      rtdCubeDisplay -f <file name> -c <camera name> [-t <msec>] [-l] [-v]
*
*   DESCRIPTION
*
*    rtdCubeDisplay displays FITS cube images for the real-time display.
*    By specifying a FITS cube file (option -f) and a camera name (option -c)
*    the images are extracted from the file and an image event is sent to
*    the rtdServer. In order to display the image a rtd widget application
*    e.g. 'rtd' must register to the same camera name as specified above.
*    For the 'rtd' application this is done by  setting the RTD_CAMERA 
*    environment.
*
*    Options:
*
*     -f <file name>        FITS cube images
*     -c <camera name>      Camera name to identify real time source
*     -v                    Enables verbose mode
*     -t <msec>             Delay time between images (default 500msec)
*     -l                    Loop (forever)  
*
*   SEE ALSO
*     rtdServer(1)
*
*------------------------------------------------------------------------
*/

/* 
 * System Headers
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <signal.h>
#include <time.h>
#include <netdb.h>
#include <sys/ioctl.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

typedef void (*MySigFunc)(int);  /* allan: prototype cast to keep Sun cc quiet */

/* 
 * Local Headers
 */

#include "rtdImageEvent.h"

static int verbose=0;
static int shmId;

static void usage()
{
    printf("rtdCubeDisplay -f <file name> -c <camera name> [-t <msec>] [-l] [-v]\n");
    printf("-l = loop indefinetly\n");
    printf("-t = delay time between cube images (in msec) default 500 ms\n");
    printf("-v = switch on verbose\n");
    exit (1);
}
 

static int readFitsCube(FILE  *fptr,
		 int   *type,
		 int   *width,
		 int   *height,
		 int   *count,
		 float *bscale,
		 float *bzero,
		 int   *fileoffset)
{
    char  buffer[81],*ptr,*vptr;
    int   cnt=0;


    do
    {
	fgets(buffer,sizeof(buffer),fptr);
	ptr = strtok(buffer,"=");
	while(*ptr == ' ') ptr++;
	if (strncmp(ptr,"NAXIS1",6) == 0)
	{
	    vptr = strtok(NULL,"/");
	    *width = atoi(vptr);
	}
	if (strncmp(ptr,"NAXIS2",6) == 0)
	{
	    vptr = strtok(NULL,"/");
	    *height = atoi(vptr);
	}
	if (strncmp(ptr,"NAXIS3",6) == 0)
	{
	    vptr = strtok(NULL,"/");
	    cnt = atoi(vptr);
	}
	if (strncmp(ptr,"BITPIX",6) == 0)
	{
	    vptr = strtok(NULL,"/");
	    *type = atoi(vptr);
	}
	if (strncmp(ptr,"BZERO",5) == 0)
	{
	    vptr = strtok(NULL,"/");
	    *bzero = (float)atof(vptr);
	}
	
	if (strncmp(ptr,"BSCALE",6) == 0)
	{
	    vptr = strtok(NULL,"/");
	    *bscale = (float)atof(vptr);
	}
    }
    while(strncmp(ptr,"END ",4) !=0);

    *fileoffset = ((ftell(fptr)/2880L) + ((ftell(fptr)%2880)?1:0))*2880;

    if (!cnt)
    {
	printf("Warning: NAXIS3 not specified - hmm I'm not shure this is a cube !\n");
	cnt = 1;
    }
    *count = cnt;

    return RTD_OK;
}

static void cleanup()
{
    if (shmId) shmctl(shmId,IPC_RMID,NULL);
    if (verbose) printf("Exiting !\n");
    exit(0); 
}

main(int argc, char *argv[])
{
    char               c,camera[RTD_NAMELEN],reqName[RTD_NAMELEN],fileName[256];
    int                count,fileoffset,typeSize=0;
    int                loop=0,timer=500,type,i,j;
    rtdIMAGE_EVT_HNDL  eventHndl;
    rtdIMAGE_INFO      imageInfo;
    extern char        *optarg;
    extern int         optind;
    int                shmWidth,shmHeight,shmImageType=0;
    char               *shmPtr;
    FILE               *fptr;
    float              bscale = 0, bzero = 0;

    signal(SIGINT,(MySigFunc)cleanup);
    signal(SIGTERM,(MySigFunc)cleanup);
    signal(SIGHUP,(MySigFunc)cleanup);

    while ((c = getopt(argc,argv,":vlf:c:t:")) != -1) {

#ifndef SYSV
	char* optopt = argv[optind]; 
#endif
	switch(c) 
	{
	case 'v': verbose++; break;
	case 'l': loop++; break;
	case 't': timer = atoi(optarg); break;
	case 'r': strncpy(reqName,optarg,RTD_NAMELEN); break;
	case 'f': strncpy(fileName,optarg,256); break;
	case 'c': strncpy(camera,optarg,RTD_NAMELEN); break;
	case ':':
	    fprintf(stderr,"Option -%s requires an argument\n",
		    optopt);
	    usage();
	    break;
	case '?':
	    fprintf(stderr,"Invalid argument -%s \n",optopt);
	    usage();
	    break;
	}
    }
    
    if (strlen(camera) == 0)
    {
	printf("camera name not specified - unable to continue !\n");
	usage();
    }

    if (rtdInitImageEvt(camera,&eventHndl,NULL) == RTD_ERROR)
    {
	printf("Could not initialize image event !\nCheck if rtdServer is running !\n");
	usage();
    }

    if (strlen(fileName) == 0)
    {
	printf("filename not specified - unable to continue !\n");
	usage();
    }
	

    fptr = fopen(fileName,"r");
    if (fptr == NULL)
    {
	printf("invalid filename specified: %s\n",fileName);
	usage();
    }
    
    if (readFitsCube(fptr,&type,&shmWidth,&shmHeight,&count,
		     &bscale,&bzero,&fileoffset) == RTD_ERROR)

    {
	printf("Error in readFitsCube \n");
	usage();
    }
    
    if (verbose)
	printf("Filename: %s type:%d width:%d height:%d\n",
	       fileName,type,shmWidth,shmHeight);

    switch (type)
    {
    case 8:
	shmImageType = BYTE;
	typeSize = 1; break;
    case -16:
	shmImageType = USHORT;
	typeSize = 2; break;
    case 16:
	shmImageType = SHORT;
	typeSize = 2; break;
    case 32:
	shmImageType = INT;
	typeSize = 4; break;	
    case -32:
	shmImageType = FLOAT;
	typeSize = 4; break;
    }

    /* remove previous shm area */
    if (shmId) 
	shmctl(shmId,IPC_RMID,NULL);
    
    shmId    = shmget(IPC_PRIVATE,shmWidth*shmHeight*typeSize,0666); 
    
    if (verbose)
	printf("Shared Memory area created, id: %d size:%d \n",
	       shmId,shmWidth*shmHeight*typeSize);
    
    shmPtr   = (void *)shmat(shmId,NULL,0); 
    if (shmPtr != NULL && shmPtr != (void *)-1)
    {
	/* read though file */
	do
	{
	    /* set correct file pointer */
	    rewind(fptr);
	    fseek(fptr,fileoffset, SEEK_SET);
	  
	    for (i=0;i<count-1; i++)
	    {
		fread(shmPtr,1,shmWidth*shmHeight*typeSize,fptr);  
		memset(&imageInfo,'\0',sizeof(rtdIMAGE_INFO));
		imageInfo.dataType = shmImageType;
		imageInfo.shmId = shmId;
		imageInfo.xPixels  = shmWidth;
		imageInfo.yPixels  = shmHeight;

                /* Fill in UKIRT Quick Look members */
	        imageInfo.reserved[0] = 0;
	        imageInfo.reserved[1] = 0;
	        imageInfo.reserved[2] = shmWidth;
	        imageInfo.reserved[3] = shmHeight;
	        imageInfo.reserved[4] = 1024;

		if (bscale != 0)
		{
		    char  *cPtr;
		    float *fPtr;
		    int   *iPtr;
		    short *sPtr;
		    switch (shmImageType)
		    {
		    case BYTE:
			cPtr = (char *)shmPtr;
			for (j=0; j<shmWidth*shmHeight; j++)
			    cPtr[j] = (char)(bzero + bscale*(float)cPtr[j]);
			break;
		    case USHORT:
		    case SHORT:
			sPtr = (short*)shmPtr;
			for (j=0; j<shmWidth*shmHeight; j++)
			    sPtr[j] = (short)(bzero + bscale*(float)sPtr[j]);
			break;
		    case INT:
			iPtr = (int *)shmPtr;
			for (j=0; j<shmWidth*shmHeight; j++)
			    iPtr[j]  = (int)(32000 * (bzero + bscale*(float)iPtr[j]));
			break;
		    case FLOAT:
			fPtr = (float *)shmPtr;
			for (j=0; j<shmWidth*shmHeight; j++)
			    fPtr[j] = (int)(bzero + bscale*(float)fPtr[j]);
			break;
		    }
		}
		/* udate image event structure */
		/* send to server */
		if (verbose)
		{
		    printf("sending image no:%d \r",i);
		    fflush(stdout);
		}
		rtdSendImageInfo(&eventHndl,&imageInfo,NULL);
		rtdSleep(timer);
	    }
	
	}
	while (loop);
    }
    if (verbose) printf("\n");
    cleanup();

    exit(0);       
}



/*___oOo___*/
