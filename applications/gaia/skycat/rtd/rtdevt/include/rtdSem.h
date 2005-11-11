#ifndef RTDSEM_H
#define RTDSEM_H

/*+
 * E.S.O. - VLT project 
 *
 *
 * rtdSem.h
 *
 * This header contains prototypes for the semaphore manipulation routines
 * of rtdSem.c
 *
 * See rtdSem(1) for more information.
 *
 * who         when      what
 * ---------   --------  ----------------------------------------------
 * D.Hopkinson 21/01/97  Created.
-*/

#include <stdlib.h>
#include <stdio.h>
#include <sys/sem.h>
#include "rtdImageEvent.h"

#define RTD_PERMS	 0666   /* Access permissions for shared memory */
#define RTD_SEM_TIMEOUT  20	/* Timeout period for an un-reset semaphore */

/*
 * This structure is provided for use with the multibuffering/semaphore
 * convenience routines.
 */
typedef struct rtdShm {
    int *shmId;		/* Array of shared memory Ids */
    int semId;		/* Semaphore Id */
    int num;		/* Number of shared memory buffers */
    int	shmWidth;	/* Width of image (pixels) */
    int	shmHeight;	/* Height of image (pixels) */
    int	shmImageType;	/* Type of image (BYTE, SHORT, etc) */
    double *timestamp;  /* Array of semaphore timestamps */
} rtdShm;

#ifdef __cplusplus
extern "C" {
#endif

    int  rtdShmCreate	(int num, rtdShm *shmPtr, int width, int height, int type);
    int  rtdShmFill	(int index, char *data, rtdShm *shmPtr, int verbose);
    int  rtdShmFillFirst	(char *data, rtdShm *shmPtr);
    int  rtdShmFillNext	(int index, char *data, rtdShm *shmPtr);
    int  rtdShmStruct	(int index, rtdIMAGE_INFO *imageInfo, rtdShm *shmPtr);
    int  rtdShmDelete	(rtdShm *shmPtr);
    int  rtdShmLocked	(rtdShm *shmPtr, int index);
    int  rtdSemGetVal	(int semId, int semNum);
    int  rtdSemIncrement	(int semId, int semNum, int increment);
    void rtdSemDecrement	(int semId, int semNum);
    void rtdSemReset	(int semId, int semNum);
    int  rtdSemGetVal	(int semId, int semNum);
    void rtdShmServicePacket(rtdIMAGE_INFO *imageInfo);

#ifdef __cplusplus
}
#endif   /* __cplusplus */

#endif   /* RTDSEM_H */
