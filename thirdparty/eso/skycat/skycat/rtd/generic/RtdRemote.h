// -*-c++-*-
#ifndef _RtdRemote_h_
#define _RtdRemote_h_

/*
 * E.S.O. - VLT project 
 * "@(#) $Id: RtdRemote.h,v 1.4 1997/04/11 10:48:15 abrighto Exp $" 
 *
 * RtdRemote.h - class definitions for managing remote access to the RTD
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  04/03/96  Created
 */

#include <tk.h>


/*
 * Class RtdRemote
 * 
 */
class RtdRemote {
protected:
    int status_;		// status after constructor
    int port_;			// port number for communication
    int socket_;		// socket for remote commands
    int verbose_;		// flag: if true, print diagnostic messages
    Tcl_Interp* interp_;	// Tcl interp (for file events, error handling)

    enum {MAX_CLIENTS = 32};	// max number of client connections
    struct Client {
	int socket;		// client socket for sending results
	int callback_socket;	// socket used for callback operations
        Tcl_File handle;        // Tcl file handle for events
	RtdRemote* thisPtr;	// hook to get back to class from callback
    };
    Client clients_[MAX_CLIENTS]; // array of client connection sockets
    Client* clientPtr_;		// ptr to current client connection


    // create a status file with pid/port info
    int makeStatusFile(struct sockaddr_in& addr);

    // method called by fileEventProc to accept new client connection
    int fileEvent();

    // method called by clientEventProc to read from client
    int clientEvent(Client*);

    // enter client socket in clients table
    int enterClient(int sock);

    // remove the client from the table
    void removeClient(int sock);

    // command methods
    virtual int draw() {return 0;}

    // call an rtdimage command method by name (defined in a derived class)
    // (see RtdImage.C for local derived class)
    virtual int call(const char* name, int len, int argc, char* argv[]) = 0;

    // send a message to the client with "status length result"
    int sendToClient(int socket, int status, int length, const char* result);

    // evaluate the command buf as rtdimage subcommands
    int evalClientCmd(const char* cmd);

public:

    // constructor
    RtdRemote(Tcl_Interp*, int port, int verbose);
    
    // destructor 
    virtual ~RtdRemote();

    // static file handler, called by Tk file handler for new connections
    static void fileEventProc(ClientData, int mask);
    
    // static file handler, called to read client socket
    static void clientEventProc(ClientData, int mask);

    // open the current client's callback socket on the given host and port
    int setClientCallbackPort(const char* host, int port);
   
    // member access
    int status() {return status_;}
    int port() {return port_;}
};



#endif /* _RtdRemote_h_ */
