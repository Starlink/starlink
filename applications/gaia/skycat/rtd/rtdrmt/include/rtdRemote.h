#ifndef _rtdRemote_h_
#define _rtdRemote_h_

/*
 * E.S.O. - VLT project 
 * "@(#) $Id: rtdRemote.h,v 1.2 2005/02/02 01:43:03 brighton Exp $" 
 *
 * rtdRemote.h - C interface for remote access to rtdimage based
 *               widgets
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  04/03/96  Created
 */


typedef void (*RtdRemoteErrorHandler)(char* message);

/* connect to remote Rtd: if args are null, uses ~/.rtd-remote file */
int rtdRemoteConnect(int pid, char* host, int port);

/* disconnect from remote Rtd */
void rtdRemoteDisconnect();

/* read the given socket to get the result */
int rtdRemoteGetResult(int socket, char** result);

/* send the command to the remote Rtd (don't get result) */
int rtdRemoteSendOnly(char* cmd);

/* send a command to the remote Rtd and get the result */
int rtdRemoteSend(char* cmd, char** result);

/* set an error handler to be called as: void errorhandler(char* msg); */
RtdRemoteErrorHandler rtdRemoteSetErrorHandler(RtdRemoteErrorHandler);

/* return the text of the last error message */
char* rtdRemoteGetError();

#endif /*  _rtdRemote_h_ */
