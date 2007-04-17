// -*-c++-*-
#ifndef _HTTP_h_
#define _HTTP_h_

/*
 * E.S.O. - VLT project 
 * $Id: HTTP.h,v 1.1.1.1 2006/01/12 16:40:58 abrighto Exp $
 *
 * HTTP.h - utility class for communicating with the HTTP daemon
 * 
 * See the man page for a complete description.
 * 
 * who             when       what 
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 * Peter W. Draper 16 Jun 98  Added support for proxy servers
 * pbiereic        17/02/03  Added 'using namespace std'.
 */

using namespace std;
#include <cstdio>
#include <cstring>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <iostream>


/*
 * Class HTTP
 *
 */
class HTTP {
private:
    int status_;		// status after constructor
    sockaddr_in	servAddr_;	// server address 
    char hostname_[32];		// http server hostname
    char proxyname_[32];	// http proxy server hostname
    int port_;			// port number for http server
    int proxyport_;		// port number for http proxy server
    int fd_;			// file desc from last call to get(url)
    FILE* feedback_;		// optional file ptr for feedback info

    char* content_type_;	// "Content-type" field read from HTTP server or NULL
    char* content_encoding_;	// "Content-Encoding" field read from HTTP server or NULL
    int content_length_;	// "Content-length" field read from HTTP server or NULL
    char* location_;		// URL for redirect (Location keyword in HTTP header)
    char* www_auth_realm_;	// "WWW-Authenticate:" value of string after "Basic realm="

    // pointers for stepping through http results
    char* resultBuf_;		// pointer to result buffer
    char* resultGC_;		// if not null, pointer to resultBuf_ to be freed
    char* resultPtr_;		// pointer to current line in resultBuf_

    static int allowUrlExec_;   // flag: if true, allow a URL to be a command to exec.
                                // (default: false)
    
    // authorization info, set with authorize(user, passwd, ...)
    static char* auth_info_;	// base64 encoded "username:passwd" info, if needed

    // these variables have default values that should not normally need to be
    // changed
    static char* user_agent_;	// value of User-Agent string to send with requests
    static char* auth_file_;	// filename for saving HTTP authorization info

private:

    // I/O utility
    int writen(char* ptr, int nbytes);

    // open a socket to the http server on host/port
    int open(const char* hostname, int port = 80);

    // Open the given local file and return the status.
    int openFile(const char* filename);

    // run command and position fd_ at start of output
    int openCommand(const char* command);

    // Scan the first few lines of the given file for HTTP header
    int checkCommandOutput(const char* filename);

    //  check proxy server configuration
    void checkProxy( const char *host );

    // reset any previous member values before a GET or POST
    void reset();

    // Scan the given HTTP header line for any keywords that we are interested in
    void scanHeaderLine(char* buf);

    // Add an entry to the auth_file_ for the given server and realm and
    // current value of auth_info_
    static int addAuthFileEntry(const char* server, const char* realm);
 
    // Search the auth_file_, if it exists, for an entry with the given server
    // and realm and set auth_info_ to the value found there.
    static int findAuthFileEntry(const char* server, const char* realm);

    // redo the GET on the URL with username/passwd, if known, or return special
    // error message so that user interface can do it.
    int getAuthorization(const char* url);

    // copy constructor - not defined
    HTTP(const HTTP&);

public:
    // constructor
    HTTP();

    // destructor
    ~HTTP();

    // return status after constructor
    int status() const {return status_;}

    // do an HTTP GET with the given URL and position the read fd after 
    // the result HTTP header. Returns 0 if OK. Use readNext() to read
    // the result lines.
    int get(const char* url);

    // do an HTTP get on the given URL and return the result as a buffer.
    // nlines is set to the number of result lines (use getNext() to 
    // fetch the result lines from the buffer)
    char* get(const char* url, int& nlines, int freeFlag = 1);

    // do an HTTP get on the given URL and write the results to the
    // given stream
    int get(const char* url, ostream&);

    // do an HTTP POST using the given URL and data and position the read 
    // fd after the result HTTP header. 
    int post(const char* url, const char* data);

    // do an HTTP post using the given URL and data and write the results 
    // to the given stream
    int post(const char* url, const char* data, ostream&);

    // copy the results of a previous get(url) to the given stream
    int copy(ostream& os);

    // read a line of the results after a call to get(url, nlines)
    int readline(char* ptr, int maxlen);

    // return the next result line after a call to get(url)
    // or NULL if there are no more lines.
    char* getNext();
    
    // return a pointer to the result buffer from http
    const char* result() {return resultBuf_;}

    // set/get the file ptr to use for feedback during http transfers
    void feedback(FILE* f);
    FILE* feedback() const {return feedback_;}

    // return the hostname of the http server
    const char* hostname() const {return hostname_;}
    
    // return file desc from last call to get(url)
    int fd() const {return fd_;}

    // return http header values from last GET
    char* content_type() const {return content_type_;}
    char* content_encoding() const {return content_encoding_;}
    int content_length() const {return content_length_;}
    
    // if we see: WWW-Authenticate: Basic realm="somedomain" 
    // in the HTTP header, we assume a username and password are required 
    // and www_auth_realm_ is set to the "somedomain" value.
    const char* www_auth_realm() const {return www_auth_realm_;}
    
    // return true if a username/passwd is needed for the previous GET
    int authorizationRequired() {return www_auth_realm_ != NULL;}

    // set the username and password to use for authorization,
    // and, if realm and server are given, save an entry in the auth_file
    static void authorize(const char* user, const char* passwd,
			  const char* realm = NULL, const char* server = NULL);

    // take an error message in HTML format and pass it
    // on to error(), stripped of HTML syntax (<...>)
    int html_error(istream& is);
    int html_error(char* s);

    // get/set flag value to allow URL to be a local command
    static int allowUrlExec() {return allowUrlExec_;}
    static void allowUrlExec(int i) {allowUrlExec_ = i;}

    // get/set the value of the HTTP User-Agent to use with requests
    static const char* userAgent() {return user_agent_;}
    static void userAgent(const char* s);

    // get/set the value of the file used to remember authorization info
    static const char* authFile() {return auth_file_;}
    static void authFile(const char* s);
};



#endif /* _HTTP_h_ */
