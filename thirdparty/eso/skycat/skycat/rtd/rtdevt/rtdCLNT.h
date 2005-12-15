#ifndef rtdCLNT_H
#define rtdCLNT_H
/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: rtdCLNT.h,v 1.2 2005/02/02 01:43:03 brighton Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* pbiereic  01/03/01  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE 1
#endif

#include "rtdImageEvent.h"
#include "rtdSem.h"
#include "rtdLOG.h"
#include "define.h"

#include <string.h>
#include <unistd.h>
#include <string.h>
#include <netinet/tcp.h>
#include <signal.h>
#include <errno.h>
#include <netdb.h>
#include <sys/ioctl.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

class rtdCLNT : rtdLOG
{
public:
    // constructor and destructor
    rtdCLNT(int verbose, int index);
    ~rtdCLNT();

    // store/return the name of the requestor
    char *ReqName() { return reqName_; }
    void  ReqName(char *name) { strncpy (reqNameBuf_, name, RTD_NAMELEN); }
  
    // store/return the name of the camera
    char *CamName() { return camName_; }
    void  CamName(char *name) { strncpy (camNameBuf_, name, RTD_NAMELEN); }
  
    // store/return the name of the socket file descriptor
    int   Socket() { return socket_; }
    void  Socket(int socket) { socket_ = socket; }
  
    // return the semaphore params set by the requestor
    int   semId() { return semId_; }
  
    // return the number of semaphores
    int   shmNum() { return shmNum_; }

    // set the values for semId and shmNum
    void  SetSemPar(int semId, int shmNum);
   
    // store/return the index
    int   Index() { return index_; }
    void  Index(int index) { index_ = index; }
   
    // store/return the port number
    int   Port() { return port_; }
    void  Port(int port) { port_ = port; }
   
    // store/return the type number
    int   Type() { return type_; }
    void  Type(int type) { type_ = type; }
 
    // clear a buffer
    void  BufClear(char *name) { memset (name, '\0', RTD_NAMELEN); }
  
    // are we attched?
    int   Attached() { return (*reqName_ == '\0' || *camName_ == '\0'); }

    void  Attach(char* reqName, char *camName);
    void  Detach();

    int   Forward(rtdPACKET *rtdPacket);
    void  Cleanup();
    int   Accept(int listenSocket);
    int   AttachedToCamera(char *camera);
    char *TypeName();
  
private:
    char *reqName_;                    // name of requestor
    char *camName_;                    // name of camera
    char reqNameBuf_[RTD_NAMELEN];     // name of requestor
    char camNameBuf_[RTD_NAMELEN];     // name of camera
    int  socket_;                      // socket file descriptor
    int  index_;                       // socket file descriptor index
    int  port_;                        // port number
    int  type_;                        // type number
    int  semId_;                       // semaphore Id
    int  shmNum_;                      // number of semaphores
  
protected:
};

#endif /*!rtdCLNT_H*/
