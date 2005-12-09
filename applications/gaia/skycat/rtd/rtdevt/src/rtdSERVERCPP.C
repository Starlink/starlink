/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id$"
*
* who       when      what
* --------  --------  ----------------------------------------------
* pbiereic  01/03/01  created (adapted from previous rtdServer)
*/

/************************************************************************
*   NAME
*   rtdSERVER - class which manages all clients which connect to the rtdServer
* 
*   SYNOPSIS
*   #include "rtdSERVER.h"
*   rtdSERVER::rtdSERVER(int verbose, int socketFd, int delay) 
*            verbose    - verbose flag
*            socketFd   - socket file descriptor as returned by rtdInitServer()
*            delay      - delay after each client request in msec
* 
*   DESCRIPTION
*
*   rtdSERVER accepts client connections and serves requests from connected
*   clients such as forwarding image events, attach/detach etc. For each
*   client which connect to the rtdServer, a "client object" is created
*   which executes the requests from the client.
*   The main loop enables new connection requests and requests from clients
*   already connected (via a select() call). Then it processes all active requests
*   within another loop, so that all clients are served with the same priority,
*   in particular, no client can block another client.
*   Clients which use an incompatible info package structure are simply
*   disconnected.
*   rtdSERVER "knows" the number of attached RTD clients and sets the semaphore
*   accordingly. It also provides for multicasting of event notification.
*
*   PUBLIC METHODS
*
*   int rtdSERVER::Loop()
*      The main loop which accepts connections and forwards image events to
*      RTD clients which are attached to the image producer (camera). Cameras
*      use rtdSendImageInfo() for sending image events.
*      The command opcodes currently provided are:
*         ATTACH / DETACH: used by RTD to receive or block image events
*         IMAGEINFO:       used by camera clients to send image events
*
*   rtdCLNT *rtdSERVER::Accept()
*      Accept connection from RTD or camera (i.e. rtdServer clients).
*      The Accept() sets the required socket options and creates a
*      "client object" for handling all requests coming from the client
*      socket.
*
*   void rtdSERVER::DisconnectClient(rtdCLNT *client)
*      Dsiconnect a client which either sent a wrong event request or died
*
*   void rtdSERVER::ServImageCmd(rtdPACKET *rtdPacket, int numbyte)
*      This methods serves the image event. It first increments the semaphore
*      according to the number of RTD clients attached. The semaphore is released
*      by the RTD client when the image was displayed.
*      Then it calls the client object for actually forwarding the image event.
*
*   int rtdSERVER::IncrSem(rtdPACKET *rtdPacket, int increment)
*      Increment the semaphore: this is done only when the camera client has
*      implemented the semaphore (see rtdSem.c).
*      
*   rtdCLNT *rtdSERVER::GetCurrClient()
*      Return the object for a client which has sent a request.
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*    rtdInitImageEvt(3), rtdSendImageInfo(3), rtdSem(1)
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

#include "rtdSERVER.h"

static char *rcsId="@(#) $Id$"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

rtdSERVER::rtdSERVER(int verbose, int socketFd, int delay) 
    : rtdLOG(verbose),
      socketFd_(socketFd),
      delay_(delay),
      numClnt_(0),
      reqClnt_(0),
      reqCount_(0)
{
    for (int i = 0; i <= MAX_CLNT; i++)
	clnt_[i] = NULL;
}

rtdSERVER::~rtdSERVER()
{
    for (int i = 0; i < numClnt_; i++)
	delete clnt_[i];
}

int rtdSERVER::Loop()
{
    fd_set    readFd;     // file descriptor for port RTD_SERVER_PORT (default 5555)
    readFd_ = (fd_set *) &readFd;
    time_t    timeVal;
    rtdPACKET rtdPacket;  // copy of the standard info package structure
    int       packetSize = sizeof(rtdPACKET);
    rtdHEADER *hdr      = &(rtdPacket.body.data.hdr);
    int       socket;
    int       n;

    timeVal = time(NULL);
    strcpy(startTime_, ctime(&timeVal));

    log("Entering main loop and waiting for client connections...\n");
    while ( 1 ) {
	/*
	 * sleep a bit to give the RTD clients more time. A delay can
	 * be used to slow down the image event rate from ultra-fast cameras
	 * provided the camera process is using semaphore locking.
	 */
	if (delay_ > 0)
	    rtdSleep(delay_);

	FD_ZERO(readFd_);

	// enable read for connection requests
	FD_SET(socketFd_, readFd_);

	// Enable read for all clients connected.
	for (int i = 0; i < numClnt_; i++)
	    FD_SET(clnt_[i]->Socket(), readFd_);

	/////////////////////////////////////////////
	int status = select(32, readFd_, 0, 0, NULL);
	/////////////////////////////////////////////

	if (status <= 0) {
	    log("Select error !!!\n");
	    return RTD_ERROR;  // timeout or error (signals) should not happen
	}
	log("*** Handling new event (%d) ....\n", reqCount_++); // total number of requests

	// check if a client wants to connect
	if (FD_ISSET(socketFd_, readFd_) > 0) {
	    Accept();
	    continue; // accept or refuse, anyway continue the loop...
	}
	/*
	 * service clients on all active client sockets i.e. the ones which are
	 * currently in the readFd set. This ensures that all clients are
	 * serviced even when there is a client which sends events at very
	 * high speed.
	 */
	rtdCLNT *currClient = NULL;    // current client object
	while ((currClient = GetCurrClient()) != NULL) {
	    socket = currClient->Socket();
	    n = read(socket, &rtdPacket, packetSize);

	    // check if client died or sent a wrong event structure
	    if (n < 0 || n != packetSize) {
		if (n > 0)
		    log("Client sent a wrong request. Will be disconnected.\n");
		else
		    log("Client apparently closed the connection.\n");
		DisconnectClient(currClient); // Disconnect the client
		continue;
	    }
	    currClient->Type(hdr->reqType); // keep the requestor type

	    // execute command given in the event info structure
	    switch (rtdPacket.opcode) {
	    case ATTACH:
		currClient->Attach(hdr->reqName, hdr->camName);
		log("ATTACH command received from %s, %s\n", 
		    currClient->ReqName(), currClient->CamName());
		break;
	    case DETACH:
		log("DETACH command received from %s, %s\n",
		    currClient->ReqName(), currClient->CamName());
		currClient->Detach();
		break;
	    case IMAGEINFO:
		log("IMAGEINFO command received (port %d)\n", currClient->Port());
		ServImageCmd(&rtdPacket);
		break;
	    case STATUS:
		log("STATUS command received (port %d)\n", currClient->Port());
		ServStatusCmd(socket);
		break;
	    case PING:
		log("PING command received (port %d)\n", currClient->Port());
		break;
	    default:
		log("Unknown opcode received: %d. Client will be disconnected.\n",
		    rtdPacket.opcode);
		DisconnectClient(currClient); // Disconnect the client
	    } 
	}
    }
}

/*
 * Accept connection from RTD or camera
 */
rtdCLNT *rtdSERVER::Accept()
{
    rtdCLNT *client;
    reqClnt_++;  // for statistics

    // Create a new object for handling the request for this connection
    client = clnt_[numClnt_++] = new rtdCLNT(Verbose(), numClnt_);
    if (client->Accept(socketFd_) == RTD_OK && numClnt_ < MAX_CLNT)
	return client;

    if (numClnt_ >= MAX_CLNT)
	log("Too many cameras and RTD's connected to rtdServer\n");
    DisconnectClient(client);
    return NULL;
}

void rtdSERVER::DisconnectClient(rtdCLNT *client)
{
    int idx = client->Index();

    log("Closing connection (port %d)\n", client->Port());

    delete client;
    clnt_[idx] = NULL;
    numClnt_--;
    /*
     * shuffle up the clnt_[] pointer buffer to simplify programatic access.
     * The last pointer of the buffer is not used but set to NULL.
     */
    for (int i = idx; i < MAX_CLNT; i++)
	clnt_[i] = clnt_[i+1];
}

/*
 * return a client object for which a request is pending
 */
rtdCLNT *rtdSERVER::GetCurrClient()
{
    for (int i = 0; i < numClnt_; i++) {
	if (FD_ISSET(clnt_[i]->Socket(), readFd_) <= 0)
	    continue;
	FD_CLR(clnt_[i]->Socket(), readFd_);    // needed for next FD_ISSET
	clnt_[i]->Index(i);       // client  index
	return clnt_[i];
    }
    return NULL;
}

void rtdSERVER::ServImageCmd(rtdPACKET *rtdPacket)
{
    int  numClients = 0;   // number of RTD clients attached to the camera
    char *camera        = rtdPacket->body.data.hdr.reqName;
    rtdIMAGE_INFO *info = &(rtdPacket->body.data.rtdImageInfo);
    
    log("Image event received from: %s\n", camera);
    if (*camera == '\0')
	return;
    
    // Get the number of RTD clients which are currently attached to the camera
    for (int i = 0; i < numClnt_; i++) {
	if (clnt_[i]->AttachedToCamera(camera) == RTD_OK)
	    numClients++;
    }
    /*
     * Increment the shared memory semaphore with (numClients - 1). One
     * increment was already done by the camera.
     */
    if (IncrSem(rtdPacket, numClients - 1) != RTD_OK)
	return;

    if (! numClients) {
	log("No RTD clients are currently attached to '%s'\n", camera);
	return;
    }
    /*
     * Now loop over the clients to send the packets to all attached RTD's.
     */
    for (int i = 0; i < numClnt_; i++) {
	if (clnt_[i]->AttachedToCamera(camera) != RTD_OK)
	    continue;
	/*
	 * The attached RTD client object needs to cleanup semaphores
	 * when it's associated RTD terminates.
	 */
	clnt_[i]->SetSemPar(info->semId, info->shmNum);

	log("Forwarding event to: %s\n", clnt_[i]->ReqName());
	if (clnt_[i]->Forward(rtdPacket) != RTD_OK)
	    log("Forwarding event message failed\n");
    }
    return;
}

void rtdSERVER::ServStatusCmd(int socket)
{
    char buf[4096], buf2[256];

    sprintf(buf, "rtdServer info:\n"
	    "rtdServer was started: %s"
	    "Delay was set to: %d\n"
	    "Total number of connections: %d\n"
	    "Total number of requests: %d\n",
	    startTime_, delay_, reqClnt_, reqCount_);
    strcat(buf, "Current rtdServer clients:\n");
    for (int i=0; i < numClnt_; i++) {
	if (socket == clnt_[i]->Socket())
	    continue;
	sprintf(buf2, "Entry: %d \tName: %s\tCamera: %s\t Type: %s\t\n",
		i, clnt_[i]->ReqName(), clnt_[i]->CamName(), clnt_[i]->TypeName());
	if (strlen(buf) + sizeof(buf2) + 1 < sizeof(buf))
	    strcat(buf, buf2);
    }
    log(buf);
  
    write(socket, buf, strlen(buf)+1);
}

int rtdSERVER::IncrSem(rtdPACKET *rtdPacket, int increment)
{
    rtdIMAGE_INFO *rtdImageInfo = &(rtdPacket->body.data.rtdImageInfo);
    int semId  = rtdImageInfo->semId;
    int shmNum = rtdImageInfo->shmNum;
    /*
     * First thing is to check that semaphores were implemented by the camera.
     * Note that semId=0 is a valid id.
     */
    int val = rtdSemGetVal(semId, shmNum);
    if (val < 0) 
	return RTD_OK; // for applications not using semaphores

    log("Semaphores implemented: semId = %d, shmNum=%d, val = %d\n",
	semId, shmNum, val);

    /*
     * Check also that the semaphore given in the image information 
     * is set to one.
     */
    if (val != 1) {
	log("Warning: sending image event without semaphore set to 1\n");
	return RTD_OK;
    }
    /*
     * Now increment the semaphore by increment. First set the required
     * semaphore to change in the sembuf structure.
     */
    if (increment != 0) 
	rtdSemIncrement(semId, shmNum, increment);
    return RTD_OK;
}
