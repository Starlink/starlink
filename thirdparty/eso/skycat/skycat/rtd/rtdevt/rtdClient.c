/*************************************************************************
* E.S.O. - VLT project
* "@(#) $Id: rtdClient.c,v 1.2 2005/02/02 01:43:03 brighton Exp $"
*  rtdClient.c
*
* who       when      what
* --------  --------  ----------------------------------------------
* T.Herlin  08/02/95  Created
* P.W. Draper 16/12/97  Modified to use fd_set as a type rather than
*                       struct.
*/

/************************************************************************
*   NAME
*      rtdClient
* 
*   SYNOPSIS
*      
*      rtdClient [-v]
*
*   DESCRIPTION
*
*   FILES
*
*   ENVIRONMENT
*
*   RETURN VALUES 
*
*   CAUTIONS
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
*
*------------------------------------------------------------------------
*/
static const char* const rcsId="@(#) $Id: rtdClient.c,v 1.2 2005/02/02 01:43:03 brighton Exp $";


/* 
 * System Headers
 */
#include <stdio.h>
#include <stdlib.h>
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

#include "config.h"
#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

/* 
 * Local Headers
 */

#include "rtdImageEvent.h"

int verbose=0;
 

void usage(void) {
    printf("rtdClient [-v]\n");
    exit(1);
}
/*  */
int main(int argc, char *argv[])
{
    char               c,*buf, camera[RTD_NAMELEN],reqName[RTD_NAMELEN];
    int                readable;
    int                status=0,attach=0,image=0;
    rtdIMAGE_EVT_HNDL    eventHndl;
    rtdPACKET          rtdPacket;
    rtdIMAGE_INFO      imageInfo;
    fd_set             readFds;
    extern char        *optarg;
    extern int         optind;

    strcpy(reqName,"rtdClient");
    
    while ((c = getopt(argc,argv,":cvsia:r:")) != -1) {
#ifndef SYSV
	char* optopt = argv[optind];
#endif
	switch(c) 
	{
	case 'v': verbose++; break;
	case 's': status++; break;
	case 'i': image++; break;
	case 'a': attach++; strcpy(camera,optarg); break;
	case 'r': strncpy(reqName,optarg,RTD_NAMELEN); break;
	case ':':
	    fprintf(stderr,"Option -%s requires an argument\n",optopt);
	    usage();
	    break;
	case '?':
	    fprintf(stderr,"Invalid argument -%s \n",optopt);
	    usage();
	    break;
	}
    }
    
    if (rtdInitImageEvt(reqName,&eventHndl,NULL) == RTD_ERROR)
    {
	printf("Could not initialize image event !\nCheck if rtdServer is running !\n");
	exit(1); 
    }
    
    if (attach)
    {
	rtdAttachImageEvt(&eventHndl,camera,NULL);
    }
    
    if (status)
    {
	rtdPacket.opcode = STATUS;
	strncpy(rtdPacket.body.data.hdr.reqName,reqName,RTD_NAMELEN);
	write(eventHndl.socket,&rtdPacket,sizeof(rtdPACKET));
    }
     
    if (image)
    {
	memset(&imageInfo,'\0',sizeof(rtdIMAGE_INFO));
	imageInfo.dataType = FLOAT;
	imageInfo.xPixels  = 512;
	imageInfo.yPixels  = 512;
	/* send to server */
	rtdSendImageInfo(&eventHndl,&imageInfo,NULL);
	sleep(2);
	exit(0);
    }
       
    FD_ZERO(&readFds);
    FD_SET(eventHndl.socket, &readFds);
    for (;;)
    {
	if (select(32,(fd_set *)&readFds, 0, 0, NULL) == -1)
	{
	    if (verbose)
		printf("Select fails\n");
	    continue;
	}
	if (FD_ISSET(eventHndl.socket, &readFds) > 0)
	{
	    if (verbose)
		printf("Input on client socket: %d\n",eventHndl.socket);
	    ioctl(eventHndl.socket,FIONREAD,&readable);
	    if (verbose)
		printf("Bytes readable: %d\n",readable);
	    if (readable)
	    {
		if (readable == sizeof(rtdPACKET))
		{
		    memset(&rtdPacket,'\0',sizeof(rtdPACKET));
		    read(eventHndl.socket,&rtdPacket,
			 sizeof(rtdPACKET));
		    if (verbose)
			printf("Packet received: %d, %s, %s\n",
			       rtdPacket.opcode,
			       rtdPacket.body.data.hdr.reqName,
			       rtdPacket.body.data.hdr.camName);
		}
		else
		{
		    buf = malloc(readable+1);
		    read(eventHndl.socket,buf,readable);
		    buf[readable] = '\0';
		    printf("Read from server:\n%s\n",buf);
		    free(buf); 
		}
	    }
	    if (status) exit(0);
	}
    }
}



/*___oOo___*/
