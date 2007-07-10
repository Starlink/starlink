/*******************************************************************************
* E.S.O. - VLT project
* 
*  rtdSem.c
* 
* who         when      what
* --------    --------  ----------------------------------------------
* D.Hopkinson 21/01/97  Created.
* D.Hopkinson 27/02/97  Updated to include shared memory creation/
*			destruction.
* pbiereic    14/06/97  Added rtdShmFillNext()
* pbiereic    22/10/99  Bug fixed in rtdShmDelete
* pbiereic    01/03/01  Added: rtdSemIncrement(), rtdSemGetVal()
* pbiereic    28/05/01  Removed SEM_UNDO in rtdShmFill (see system parameter
*                       semaem on HP which specifies the maximum amount the value
*                       of a semaphore can be changed by an undo operation)
*
* Description:
*	This module contains several utility routines for the creation,
*	activation, and destruction of semaphores and associated shared
*	memory areas.
*/
/************************************************************************
*   NAME
*    rtdSem - utility routines for semaphores and shared memory
*
*    rtdShmCreate	- Initialise shared memory and semaphore set.
*
*    rtdShmFill		- Fill chosen piece of shared memory with data.
*
*    rtdShmStruct	- Fill image information structure prior to send.
*
*    rtdShmDelete	- Remove the shared memory/semaphore set.
*
*    rtdShmLocked	- Detect if a particular shm segment is locked.
*
*    rtdShmFillFirst    - Fill the first free segment of shared memory.
*
*    rtdShmFillNext     - Fill the next free segment of shared memory.
*
*    rtdShmServicePacket- Clear a semaphore given image event information.
*
*    rtdSemDecrement	- Decrement the chosen semaphore.
*
*    rtdSemReset	- Reset the chosen semaphore to zero.
*    
*    SYNOPSIS
*    #include "rtdSem.h"
*
*    int rtdShmCreate(int	num,
*          	      rtdShm	*shmPtr,
*                     int	width,
*		      int	height,
*		      int	type)
*
*    int rtdShmFill(int		index,
*                   char	*data,
*                   rtdShm	*shmPtr,
*		    int		verbose)
* 
*    int rtdShmStruct(int	index,
*                   rtdIMAGE_INFO *imageInfo,
*                   rtdShm	*shmPtr)
*
*    int rtdShmDelete(rtdShm 	*shmPtr)
*
*    int rtdShmLocked(rtdShm 	*shmPtr,
*		      int 	index)
*
*    int rtdShmFillFirst(char 	*data,
*			 rtdShm *shmPtr)
*
*    int rtdShmFillNext(int     index,
*                        char 	*data,
*			 rtdShm *shmPtr)
*
*    void rtdShmServicePacket(rtdIMAGE_INFO *imageInfo)
*
*    void rtdSemDecrement(int 	semId,
*			 int 	semNum)
*
*    void rtdSemReset(int	semId,
*		     int	semNum)
*
*
*   DESCRIPTION
*
*   These routines are (mostly) for the benefit of CCD software
*   developers who wish to interface with the shared memory/
*   semaphore locking implemented within the RTD. The exception
*   to this are the routines rtdSemDecrement and rtdSemReset,
*   which are currently used by the RTD and rtdServer, although
*   there may be a use for them in the future on the CCD side.
*
*   These routines all use a structure (defined in rtdSem.h):
*	typedef struct rtdShm {
*    	    int *shmId;		Array of shared memory Ids 
*    	    int semId;		Semaphore Id
*    	    int num;		Number of shared memory buffers
*    	    int	shmWidth;	Width of image (pixels)
*    	    int	shmHeight;	Height of image (pixels) 
*    	    int	shmImageType;	Type of image (BYTE, SHORT, etc)
*    	    double *timestamp;  Array of semaphore timestamps
*	} rtdShm;
*   The use of this structure should be transparent when using the
*   following convenience routines, although the fields of the
*   structure should be self-explanatory.
*
* - rtdShmCreate() allocates the required number of buffers of shared
*   memory of the required size, given the height, width, and data
*   type of the FITS image that is to be created. It also creates a
*   single semaphore set, the number of items in the set being equal
*   to the number of shared memory buffers. This means that each shared
*   memory area correspnds to, and can be locked by, a single semaphore
*   item from the set. The information is stored in the rtdShm structure,
*   shown above.
*
*   If the shared memory has already been allocated, this routine returns
*   immediately.
* 
* - rtdShmFill() is used to fill a particular piece of shared memory 
*   (specified by the index argument) with data. Before doing so, the
*   semaphore corresponding to the shared memory is set to one.
*
*   If the shared memory is currently locked, the routine returns
*   immediately. The exception to this is when the routine detects
*   that the semaphore is in a 'zombie' state, i.e. it has not been
*   set by the CCD for a period of time longer then RTD_SEM_TIMEOUT
*   (defined in rtdSem.h). In this case, the semaphore is reset and
*   the processing continues.
*
*   If the data pointer is NULL then it is assumed that the data in
*   shared memory will be filled by the camera process after successful
*   call to rtdShmFill(). This is usually done when the camera process
*   transfers huge images (or image arrays) directly to shared memory.
* 
* - rtdShmStruct() fills the image information structure with the
*   information that is specific to the shared memory/semaphore
*   locking, i.e. the shared memory ID, the semaphore ID, and the
*   number of the shared memory in the multi-buffered cycle
*   (...imageInfo->shmNum).
*
* - rtdShmDelete() removes the shared memory areas and semaphore, and
*   frees the memory associated with their storage.
*
* - rtdShmLocked() is used to detect whether or not a particular piece
*   of shared memory is currently locked, and returns the semaphore state.
*   As with rtdShmFill, if it detects a semaphore timeout it resets the
*   semaphore, and returns the new value.
*   
* - rtdShmFillFirst() cycles over the shared memory buffers and fills the
*   first free (unlocked) buffer with the data supplied in the argument.
*   The number of the filled buffer is returned.
*   
* - rtdShmFillNext() cycles over the shared memory buffers and fills the
*   next free (unlocked) buffer with the data supplied in the argument.
*   The index starts at index+1.
*   The number of the filled buffer is returned.
*
*   The following routines are only used within the RTD software at the
*   moment.
*
* - rtdShmServicePacket() processes an image event with respect to the
*   semaphore information that it holds, but does no more. This is used
*   (for example) in situations where image events may be skipped by the
*   RTD, but the skipped packets must still be serviced to free up the
*   shared memory.
*
* - rtdSemDecrement() decrements the chosen semaphore by one.
*
* - rtdSemReset() resets the chosen semaphore to zero.
*
*   RETURN VALUES
*
*   Depends on routine. See individual functions.
*
*   NOTES
*
*   The scheme that has been chosen for the shared memory locking is as
*   follows. In a multi-buffered system, each piece of shared memory is
*   made to correspond to a single item from a semaphore set. If the state
*   of this item is high, then the CCD does not write to that piece of
*   memory. The RTD is never prevented from reading memory.
*
*   When a CCD writes into shared memory, it sets the semaphore for that
*   piece of memory to one. It then passes the semaphore/shared memory
*   information as fields in the image event structure. The server adds
*   to the semaphore a value equal to the number of RTD clients minus one.
*   When a client has finished reading the shared memory, it decrements
*   the corresponding semaphore by one. Thus when all clients have finished
*   reading the shared memory, the semaphore returns to zero and the CCD
*   is free to write once again.
*
*   If the update rate of the CCD is too fast for the RTD such that the
*   RTD skips some image events, it is up to the RTD to service the skipped
*   events so that the semaphores unlock. If an RTD crashes (so leaving a
*   semaphore in a high state), a timeout mechanism resets the semaphore
*   after a certain amount of time (e.g. 20 seconds).
*
*   The RTD/rtdServer code is semaphore transparent, i.e. if the CCD
*   developer chooses not to implemented semaphores, there should be
*   no effect on the operation of the RTD.
*
*   EXAMPLE
*
*   This is the simplest possible application that could send data to the
*   server using semaphore locking. It is not necessary to use all the
*   functions offered above; the idea was simply to allow the CCD developer
*   some flexibility in their choice of functionality.
*
*   // sample application which sends locked CCD data to the server
*   #include "rtdSem.h"
*   #include "rtdImageEvent.h"
*
*   #define WIDTH 		128	// Width of image
*   #define HEIGHT 		128	// Height of image
*   #define DATASIZE 		16	// Size of pixel
*   #define NUM_BUF		5	// Number of shm buffers
*   
*   static void generate_data(char *);	// General data generation routine
*   
*   void main()
*   {
*       rtdIMAGE_EVT_HNDL  eventHndl;
*       rtdIMAGE_INFO      imageInfo;
*       rtdShm             shmInfo;	// Required for CCD library routines
*       char	       	   *data;
*       unsigned int       i = 0;
*   
*       memset(&imageInfo, '\0', sizeof(rtdIMAGE_INFO));
*   
*       if (rtdInitImageEvt("My_CCD_Camera", &eventHndl, NULL) == RTD_ERROR) {
*   	    // ... handle error ...
*       }
*   
*       if (rtdShmCreate(NUM_BUF, &shmInfo, WIDTH, HEIGHT, DATASIZE) == -1) {
*   	    // ... handle error ...
*       }
*   
*       while ( [some condition] ) {
*   	    generate_data(data);
* 
*	    // Fill up the first available (unlocked) shm buffer
*	    // (rtdShmFillFirst has the same effect, except always starts
*	    // from zero).
*   	    while (rtdShmFill(i, data, &shmInfo, 0) == -1) {
*   	       sleep(1);
*   	    }
*   
*   	    imageInfo.dataType = DATASIZE;
*   	    imageInfo.xPixels  = WIDTH;
*   	    imageInfo.yPixels  = HEIGHT;
*   	    imageInfo.frameX   = 0;
*   	    imageInfo.frameY   = 0;
*   	    imageInfo.frameId  = 0;
*
*	    // Fill up the image information fields with semaphore/shm info   
*   	    rtdShmStruct(i, &imageInfo, &shmInfo);
*   
*   	    // forward image event
*   	    rtdSendImageInfo(&eventHndl, &imageInfo, NULL);
*   
*   	    i = (i + 1) % NUM_BUF;
*       }
*
*	// Free up the semaphore/shared memory allocation
*	rtdShmDelete(&shmInfo);
*	free(data);
*   }
*
*   WARNINGS
*       If you are not using semaphore locking then set semId=-1 in the
*       image event structure (see rtdImageEvent.h). Since semId=0 is
*       a valid number it can happen that rtdServer decrements a
*       semaphore created by another process which can lead to serious
*       problems!
*
*   SEE ALSO
*
*   rtdServer(1), rtdImageEvent(3)
*
*-------------------------------------------------------------------------
*/

#include <string.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include "rtdSem.h"
#include "rtdImageEvent.h"
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifndef HAVE_UNION_SEMUN
/* argument type needed by semctl - not used here */
union semun {
    int val;              /* used for SETVAL only */
    struct semid_ds *buf; /* for IPC_STAT and IPC_SET */
    ushort *array;        /* used for GETALL and SETALL */
};
#endif
static union semun semun_;

/*
 * rtdShmCreate
 *
 * Description:
 * This routine creates a unique semaphore, along with an array of shared
 * memory areas if these do not already exist.
 *
 * Arguments:
 *	int num
 *	    The number of semaphores to create in the set (this should be
 *	    equal to the number of shared memory areas in a multi-buffered
 *	    system).
 *	rtdShm *shmPtr
 *	    Pointer to the shared memory/semaphore information structure.
 *	int width
 *	    Width of the image (pixels)
 *	int height
 *	    Height of the image (pixels)
 *	int type
 *	    Image data type (SHORT, FLOAT,...)
 *
 * Return value:
 *	The ID of the created semaphore, or -1 if an error occurred.
 */
int rtdShmCreate(int num, rtdShm *shmPtr, int width, int height, int type)
{
    int i, shmSize, shmId, semId;

    /*
     * Check if the shared memory Ids have been allocated. If so, return.
     */
    if (shmPtr->shmId)
	return 0;

    shmPtr->shmWidth     = width;
    shmPtr->shmHeight    = height;
    shmPtr->shmImageType = type;
    shmPtr->num          = num;
    shmSize = width * height * (abs(type) / 8);

    /*
     * Create new shared memory areas and allocate memory for the IDs.
     */
    if ((shmPtr->shmId = (int *)calloc(num, sizeof(int))) == NULL) {
	fprintf(stderr, "Unable to allocate memory\n");
	return -1;
    }
    for (i = 0; i < num; i++) {
	shmId = shmget(IPC_PRIVATE, shmSize,  RTD_PERMS | IPC_CREAT);
	if (shmId  == -1) {
	    perror("rtdShmCreate");
	    fprintf(stderr, "Error in creating shared memory #%d\n", i);
	    return -1;
	}
	shmPtr->shmId[i] = shmId;
    }

    /*
     * Create the set of semaphores (one for each shared memory area)
     */
    semId = semget(IPC_PRIVATE, num, RTD_PERMS | IPC_CREAT);
    if (semId == -1) {
	perror("Unable to create semaphore");
	return 0;
    }
    shmPtr->semId = semId;
    /*
     * Allocate an array of timestamps for the semaphores
     */
    if ((shmPtr->timestamp = (double *)calloc(num, sizeof(double))) == NULL) {
	fprintf(stderr, "Unable to allocate timestamp data\n");
	return -1;
    }

    return semId;
}

/*
 * This routine fills the required shared memory area with the data. The
 * shared memory is 'locked' during the fill process by implementing the
 * semaphore for the shared memory.
 *
 * Arguments:
 *	int index
 *	    The index of the shared memory area. This is not the shared
 *	    memory Id, but the position of the shared memory in the
 *	    buffering order (indices start at 0).
 *	char *data
 *	    Pointer to the data to fill the shared memory with.
 *	rtdShm *shmPtr
 *	    Structure containing the shared memory/semaphore information.
 *
 * Return value:
 *	0 if OK, -1 for error/shared memory locked.
 */
int rtdShmFill(int index, char *data, rtdShm *shmPtr, int verbose)
{
    char *ptr;		/* Pointer to shared memory area */
    int shmSize;		/* Size of shared memory area */
    struct timeval tm;	/* Timestamp structure for semaphore */

    struct sembuf semLock[2] = {
	0, 0, 0,		/* Wait for [#] to equal zero */
	0, 1, 0	/* Increment [#] by one */
    };
  
    shmSize = shmPtr->shmWidth * shmPtr->shmHeight * 
	abs(shmPtr->shmImageType) / 8;
    
    /* Check if the semaphore is locked. Return immediately if it is */
    if (rtdShmLocked(shmPtr, index)) {
        if (verbose)
            printf("Semaphore %d is already locked\n", index);
        return -1;
    }
    
/* Get the current timestamp information */
    gettimeofday(&tm, NULL);

/*
 * Set the required semaphore to one, if the semaphore was created
 * successfully. This will be reset to zero
 * when all the RTDs have finished reading the image information.
 *
 * At the same time as locking the semaphore, timestamp the operation
 * so that we can detect semaphore zombies.
 */
    semLock[0].sem_num = (unsigned short)index;
    semLock[1].sem_num = (unsigned short)index;

    if (shmPtr->semId != -1) {
	semop(shmPtr->semId, &semLock[0], 2);
	shmPtr->timestamp[index] = tm.tv_sec + (tm.tv_usec / 1000000.);

	if (verbose && rtdSemGetVal(shmPtr->semId, index)) 
	    fprintf(stderr, "Semaphore %d locked\n", (index + 1));
    }
/*
 * Fill the shared memory up. First attach to the memory, then simply
 * copy the data across.
 */
    if (data == NULL)
	return 0;

    ptr = (char *)shmat(shmPtr->shmId[index], NULL, 0);
    if (ptr != NULL && ptr != (void *)-1) {
	if (memcpy(ptr, data, shmSize) == NULL) {
	    fprintf(stderr, "Unable to copy memory for segment %d", index);
	    rtdSemReset(shmPtr->semId, index);
	    return -1;
	}
    }
    else {
	if (verbose)
	    fprintf(stderr, "Unable to attach to shared memory %d\n",
		    shmPtr->shmId[index]);
	rtdSemDecrement(shmPtr->semId, index);
	return -1;
    }

/* Finally, detach from the shared memory. */
    shmdt(ptr);

    return 0;
}

/*
 * This is a convenience routine for the CCD software; given an rtdShm
 * structure, this fills up the first available (unlocked) buffer.
 *
 * Arguments:
 *	char *data
 *	    Pointer to the data to fill the shared memory with.
 *	rtdShm *shmPtr
 *	    Structure containing the shared memory/semaphore information.
 *
 * Return value:
 *	the buffer number if OK, -1 for error/all shared memory locked.
 */
int rtdShmFillFirst(char *data, rtdShm *shmPtr)
{
    int i;		/* Index counter */
    int status = -1;	/* Return status */

/*
 * Cycle over all the buffers, chcking to see if they're locked.
 * When an unlocked buffer is found, fill it with the data.
 */
    for (i = 0; i < shmPtr->num; i++) {
	if ((status = rtdShmFill(i, data, shmPtr, 0)) == 0) 
	    break;
    }

    return (status == -1 ? status : i);
}

/*
 * This is a convenience routine for the CCD software; given an rtdShm
 * structure, this fills up the next available (unlocked) buffer.
 *
 * Arguments:
 *	int index
 *          The current index of the shared memory area.
 *	char *data
 *	    Pointer to the data to fill the shared memory with.
 *	rtdShm *shmPtr
 *	    Structure containing the shared memory/semaphore information.
 *
 * Return value:
 *	the buffer number if OK, -1 for error/all shared memory locked.
 */
int rtdShmFillNext(int index, char *data, rtdShm *shmPtr)
{
    int i, j;		/* Index counters */
    int status = -1;	/* Return status */

    /*
     * Cycle over all the buffers, checking to see if they're locked.
     * When an unlocked buffer is found, fill it with the data.
     */
    for (i = 0; i < shmPtr->num; i++) {
	j = (index+i) % shmPtr->num;
	if ((status = rtdShmFill(j, data, shmPtr, 0)) == 0) 
	    break;
    }
    return (status == -1 ? status : j);
}

/*
 * This routine fills in the image information structure with all the 
 * information pertaining to the shared memory/semaphore Ids. This should
 * be invoked before the rtdPACKET is sent to the server.
 *
 * Arguments:
 *	int index
 *	    The index of the shared memory area to send. See note in the above
 *	    routine.
 *	rtdIMAGE_INFO *imageInfo
 *	    Image information structure for send.
 *	rtdShm *shmPtr
 *	    Structure containing the shared memory/semaphore Id.
 *
 * Return value:
 *	0 if OK, -1 if error.
 */
int rtdShmStruct(int index, rtdIMAGE_INFO *imageInfo, rtdShm *shmPtr)
{
    /* Fill in the required fields of the image information structure. */
    imageInfo->shmId  = shmPtr->shmId[index];
    imageInfo->semId  = shmPtr->semId;
    imageInfo->shmNum = index;

    return 0;
}

/*
 * This routine checks if a semaphore is locked. If it is locked, and it is
 * detected that the semaphore has been locked for a time longer than
 * RTD_SEM_TIMEOUT, then it is cleared and OK is returned.
 *
 * Arguments:
 *	rtdShm *shmPtr
 *	    Structure containing shared memory/semaphore information.
 *	int index
 *	    Element of semaphore set to check.
 *
 * Return value:
 *	0 if OK, 1 if locked.
 */
int rtdShmLocked(rtdShm *shmPtr, int index)
{
    struct timeval tm;		/* Current timestamp */
    double tmStamp;		/* 'Expanded' current timestamp */
    int semval;

    gettimeofday(&tm, NULL);
    tmStamp = tm.tv_sec + (tm.tv_usec / 1000000.);

    /*
     * If the semaphore was not created successfully, then just return
     * "locked".
     */
    if (shmPtr->semId == -1) 
	return 1;

    /*
     * First check the current state of the semaphore. If it is high (1)
     * then return without doing anything. The exception to this is if
     * the semaphore appears to be a 'zombie' (no operations have been carried
     * out on the semaphore for over RTD_SEM_TIMEOUT), in which case clear
     * the semaphore and continue.
     */
    semval = rtdSemGetVal(shmPtr->semId, index);
    if (semval < 0)
	return 1;
    if (semval == 0)
	return 0;
    if (tmStamp - shmPtr->timestamp[index] > RTD_SEM_TIMEOUT) {
	while(rtdSemGetVal(shmPtr->semId, index) > 0) 
	    rtdSemDecrement(shmPtr->semId, index);
	return 0;
    }
    return 1;
}

/*
 * This routine services an rtdPacket by decrementing the appropriate
 * semaphore. This routine is used in the image event library to remove
 * (decrement) the semaphores of missed image events when the CCD update
 * rate is too fast for the RTD.
 *
 * Arguments:
 *	rtdIMAGE_INFO *imageInfo
 *	    The image information packet that requires treatment.
 *
 * Return value:
 *	None.
 */
void rtdShmServicePacket(rtdIMAGE_INFO *imageInfo)
{
    /* Simply decrement the semaphore specified in the image information */
    if (imageInfo->semId != -1) 
	rtdSemDecrement(imageInfo->semId, imageInfo->shmNum);
}

/*
 * This routine removes the semaphore and deletes the shared memory areas.
 *
 * Arguments:
 *	rtdShm *shmPtr
 *	    Structure containing shared memory/semaphore information.
 *
 * Return value:
 *	0 if OK, -1 if error.
 */
int rtdShmDelete(rtdShm *shmPtr)
{
    int i;

    if (shmPtr == NULL)
	return 0;
    if (shmPtr->num < 1) 
	return 0;

    /* Delete the shared memory first. */
    if (shmPtr->shmId) {
	for (i = 0; i < shmPtr->num; i++) 
	    shmctl(shmPtr->shmId[i], IPC_RMID, NULL);
	free(shmPtr->shmId);
	shmPtr->shmId = NULL;
    }

    /* Delete the semaphore. */
    if (shmPtr->semId != -1) {
	if (semctl(shmPtr->semId, 0, IPC_RMID, semun_) != 0)
	    return -1;
    }

    /* Delete the timestamp allocation */
    free(shmPtr->timestamp);

    return 0;
}

/*
 * This routine decrements the chosen semaphore from a given set.
 * This is presently only used by those applications that have to reset
 * semaphores (in particular, the RTDs) - however, it is included in this
 * module in case this becomes useful to CCD software.
 *
 * Arguments:
 *	int semId
 *	    The ID number of the semaphore set concerned.
 *	int semNum
 *	    The number of the semaphore set to be decremented.
 *
 * Return value:
 *	None.
 */
void rtdSemDecrement(int semId, int semNum)
{
    int cnt;
    struct sembuf semDec = {
	0, -1, IPC_NOWAIT
    };

    /* Check the semaphore was created successfully */
    if (semId == -1) 
	return;

    /* Perform the decrementation */
    semDec.sem_num = (unsigned short)semNum;
    cnt = rtdSemGetVal(semId, semNum);
    if (cnt > 0) 
	semop(semId, &semDec, 1);
}
/*
 * This routine return the current value of a semaphore
 */

int rtdSemGetVal(int semId, int semNum)
{
    if (semId == -1) 
	return -1;
    return semctl(semId, semNum, GETVAL, semun_);
}

/*
 * This routine resets the semaphore to zero.
 *
 * Arguments:
 *	int semId
 *	    The ID number of the semaphore set concerned.
 *	int semNum
 *	    The number of the semaphore set to be decremented.
 *
 * Return value:
 *	None.
 */
void rtdSemReset(int semId, int semNum)
{
    struct sembuf semDec[1] = {
	0, 0, IPC_NOWAIT | SEM_UNDO
    };

    /* Check the semaphore was created successfully */
    if (semId == -1) 
	return;

    /* Perform the reset */
    semDec[0].sem_num = (unsigned short)semNum;
    semDec[0].sem_op  = -(short)rtdSemGetVal(semId, semNum);
    semop(semId, &semDec[0], 1);
}

int rtdSemIncrement(int semId, int semNum, int increment)
{
    struct sembuf semInc;

    semInc.sem_num = 0;
    semInc.sem_op  = increment;  /* Increment [#] by increment */
    semInc.sem_flg = SEM_UNDO; 

    if (semId == -1) 
	return RTD_ERROR;
    
    if (increment != 0) {
	semInc.sem_num = (unsigned short)semNum;
	semop(semId, &semInc, 1);
    }
    return RTD_OK;
}
