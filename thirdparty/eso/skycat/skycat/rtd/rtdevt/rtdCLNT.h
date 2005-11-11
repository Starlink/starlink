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

#define _POSIX_SOURCE 1

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
    char *rtdCLNT::ReqName() { return reqName_; }
    void  rtdCLNT::ReqName(char *name) { strncpy (reqNameBuf_, name, RTD_NAMELEN); }
  
    // store/return the name of the camera
    char *rtdCLNT::CamName() { return camName_; }
    void  rtdCLNT::CamName(char *name) { strncpy (camNameBuf_, name, RTD_NAMELEN); }
  
    // store/return the name of the socket file descriptor
    int   rtdCLNT::Socket() { return socket_; }
    void  rtdCLNT::Socket(int socket) { socket_ = socket; }
  
    // return the semaphore params set by the requestor
    int   rtdCLNT::semId() { return semId_; }
  
    // return the number of semaphores
    int   rtdCLNT::shmNum() { return shmNum_; }

    // set the values for semId and shmNum
    void  rtdCLNT::SetSemPar(int semId, int shmNum);
   
    // store/return the index
    int   rtdCLNT::Index() { return index_; }
    void  rtdCLNT::Index(int index) { index_ = index; }
   
    // store/return the port number
    int   rtdCLNT::Port() { return port_; }
    void  rtdCLNT::Port(int port) { port_ = port; }
   
    // store/return the type number
    int   rtdCLNT::Type() { return type_; }
    void  rtdCLNT::Type(int type) { type_ = type; }
 
    // clear a buffer
    void  rtdCLNT::BufClear(char *name) { memset (name, '\0', RTD_NAMELEN); }
  
    // are we attched?
    int   rtdCLNT::Attached() { return (*reqName_ == '\0' || *camName_ == '\0'); }

    void  rtdCLNT::Attach(char* reqName, char *camName);
    void  rtdCLNT::Detach();

    int   rtdCLNT::Forward(rtdPACKET *rtdPacket);
    void  rtdCLNT::Cleanup();
    int   rtdCLNT::Accept(int listenSocket);
    int   rtdCLNT::AttachedToCamera(char *camera);
    char *rtdCLNT::TypeName();
  
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
