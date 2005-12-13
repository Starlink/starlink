#ifndef rtdSERVER_H
#define rtdSERVER_H
/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: rtdSERVER.h,v 1.2 2005/02/02 01:43:03 brighton Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* pbiereic  01/03/01  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#define MAX_CLNT 300  // max. number of clients which can connect

#include "rtdCLNT.h"  // includes almost all we need
#include "rtdLOG.h"
#include <time.h>

// This class is handling semaphores, not rtdCLNT

#ifndef HAVE_UNION_SEMUN
// argument type needed by semctl - not used here
union semun 
{
    int val;               // used for SETVAL only
    struct semid_ds *buf;  // for IPC_STAT and IPC_SET
    ushort *array;         // used for GETALL and SETALL
};
#endif

class rtdSERVER : public rtdLOG
{
public:
    // constructor and destructor
    rtdSERVER(int, int, int);
    ~rtdSERVER();
  
    int      rtdSERVER::Loop();
  
protected:
    rtdCLNT *rtdSERVER::GetCurrClient();
    void     rtdSERVER::ServImageCmd(rtdPACKET *rtdPacket);
    void     rtdSERVER::ServStatusCmd(int socket);
    int      rtdSERVER::IncrSem(rtdPACKET *rtdPacket, int increment);
    rtdCLNT *rtdSERVER::Accept();
    void     rtdSERVER::DisconnectClient(rtdCLNT *client);

private:
    int       socketFd_;           // listen socket
    int       delay_;              // delay in msec
    int       numClnt_;            // number of clients which are currently connected
    rtdCLNT   *clnt_[MAX_CLNT+1];  // pointer array to camera objects
    int       reqCount_;           // total number of requests
    int       reqClnt_;            // total number of clients which connected
    fd_set    *readFd_;            // read file descriptor mask
    char startTime_[256];          // start time
};

#endif /*!rtdSERVER_H*/
