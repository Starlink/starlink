 /*
 * E.S.O. - VLT project/ESO Archive 
 * $Id: HTTP.C,v 1.6 2005/02/02 01:43:00 brighton Exp $
 *
 * HTTP.C - method definitions for class HTTP
 *          (based on code from DSS:HTTP.c by Miguell Albrecht)
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 * Peter W. Draper 16 Jun 98  Added support for web proxy servers.
 *                 06 Aug 01  Added "Host:" header for interception
 *                            proxy support.
 *                 21 Jan 03  Changed to work with UNIX permissions
 *                            after gcc3 looses a non-standard
 *                            ofstream constructor. 
 * pbiereic        17/02/03   Added 'using namespace std'. Removed ::std specs.
 */
static const char* const rcsId="@(#) $Id: HTTP.C,v 1.6 2005/02/02 01:43:00 brighton Exp $";

using namespace std;
#include <cstdio>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <fstream>
#include <sstream>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <netdb.h>
#include <arpa/inet.h>
#include "error.h"
#include "util.h"
#include "base64.h"
#include "HTTP.h"


#ifdef NEED_SOCKET_PROTO
// some protos missing in SunOS
extern "C" {
    int socket(int, int, int);
    int connect(int, const void*, int);
    int strncasecmp(char*, char*, int);
}
#endif /* NEED_SOCKET_PROTO */


// this flag is made static to make it easy to turn on and off.
// If true, allow a URL to be a command to exec
int HTTP::allowUrlExec_ = 0;

// HTTP authorization info, set with authorize(user, passwd, ...)
char* HTTP::auth_info_ = NULL;	// base64 encoded "username:passwd"

// Using "User-Agent: SkyCat/1.0" is historical - this source used to be 
// part of skycat. Changing this might cause problems with existing catalog 
// or image servers (HST previews, in particular).
char* HTTP::user_agent_ = NULL;	
const char* default_user_agent_ = "SkyCat/1.0";

// This sets the default name for the file used to store authorization info
// (host, realm, encoded(user:passwd)) used to access restricted HTTP sites.
// The information is stored in this file after an HTTP GET returns an
// "Authorization Required" result and the user supplies the username and
// password. The "~" will be replaced with getenv(HOME).
char* HTTP::auth_file_ = NULL;	
const char* default_auth_file_ = "~/.http_auth";


/*
 * constructor - open a connection to the httpd server on the
 * given host/port
 */
HTTP::HTTP()
    : status_(0),
      port_(-1),
      proxyport_(-1),
      fd_(-1),
      feedback_(NULL),
      content_type_(NULL),
      content_encoding_(NULL),
      content_length_(0),
      location_(NULL),
      www_auth_realm_(NULL),
      resultBuf_(NULL),
      resultGC_(NULL),
      resultPtr_(NULL)
{
    strcpy(hostname_, "localhost");
    proxyname_[0] = '\0';
}


/*
 * destructor - close the connection
 */
HTTP::~HTTP() 
{
    if (fd_ >= 0) 
	close(fd_);
    if (resultGC_) {
	delete[] resultGC_;
        resultGC_ = NULL;
    }
    reset();
}


/*
 * take an error message in HTML format from the given stream and pass it
 * on to error(), stripped of HTML syntax (<...>)
 */
int HTTP::html_error(istream& is)
{
    char buf[1024*2];
    is.read(buf, sizeof(buf));
    int n = is.gcount();
    if (n > 0) {
	buf[n-1] = '\0';
	return html_error(buf);
    }
    return 0;
}


/*
 * Take an error message in HTML format and pass it on to error(),
 * stripped of HTML syntax (<...>).
 */
int HTTP::html_error(char* s)
{
    // other HTML error message, filter out HTML syntax
    char* p = s;
    char* q = s;
    while (*p) {
	if (*p == '<') {
	    while (*p && *p != '>')
		p++;
	}
	else if (*p == '>') {
	    p++;
	}
	else if (*p == '\r') {
	    p++;
	}
	else
	    *q++ = *p++;
    }
    *q = '\0';
    return error("HTTP error: ", s);
}


/*
 * set the file ptr to use for feedback during http transfers
 */
void HTTP::feedback(FILE* f) 
{
    feedback_ = f;
}


/*
 * open the connection to the given host and port and set the member
 * variable fd_ to the open socket descriptor.
 * 
 * Returns 0 on success, 1 for error.
 */
int HTTP::open(const char* hostname, int port)
{
    // close previous connection
    if (fd_ >= 0) 
	close(fd_);

    // see if its the same host/port as last time...
    if (port != port_ || strcmp(hostname, hostname_) != 0) {
	strncpy(hostname_, hostname, sizeof(hostname_)-1);
	port_ = port;
	
	// reset authorization info for new server
	if (auth_info_) {
	    free(auth_info_);
	    auth_info_ = NULL;
	}
	
	if (feedback_) {
	    fprintf(feedback_, "opening connection to %s:%d...\n", hostname, port);
	    fflush(feedback_);
	}

	// Fill in the structure "servAddr_" with the address of the
	// server that we want to connect with.
	memset((char *)&servAddr_, '\0', sizeof(servAddr_));

	// get host entry from hostname or IP address
	if (isdigit(hostname_[0])) {
	    servAddr_.sin_addr.s_addr = inet_addr(hostname_);
	    if ((int)servAddr_.sin_addr.s_addr == -1) 
		return sys_error("malformed IP address: ", hostname);
	}
	else {
	    hostent *host = gethostbyname(hostname);
	    if (!host)
		return error("Can't find host IP address for: ", hostname);
	    if (feedback_) {
		fprintf(feedback_, "connecting to %s:%d...\n", host->h_name, port);
		fflush(feedback_);
	    }
	    memcpy(&servAddr_.sin_addr, host->h_addr_list[0], host->h_length);
	}
	servAddr_.sin_family = AF_INET;
	servAddr_.sin_port = htons(port);
    }


    // Open a TCP socket (an Internet stream socket).
    if ((fd_ = socket(AF_INET, SOCK_STREAM, 0)) < 0) 
	return sys_error("Can't open stream socket");
     
    // Connect to the server.
    if (connect(fd_, (sockaddr *)&servAddr_, sizeof(servAddr_)) < 0) 
	return sys_error("Can't connect to HTTP server ", hostname_);

    return 0;
}



/*
 * replace blanks or tabs in url with %20 and write the output in
 * new_url[size]
 */
static void replace_blanks(const char* url, char* new_url, int size)
{
    int i = 0;
    for (const char* p = url; *p && i < size; p++, i++) {
	if (isspace(*p)) {
	    strcpy(new_url, "%20");
	    new_url += 3;
	}
	else {
	    *new_url++ = *p;
	}
    }
    *new_url = '\0';
}

/*
 * Open the given local file and return the status.
 * Sets fd_ to the file desc.
 */
int HTTP::openFile(const char* filename)
{
    if (fd_ >= 0) 
	close(fd_);
    fd_ = ::open(filename, O_RDONLY);
    if (fd_ < 0)
	return sys_error("can't open file: ", filename);
    return 0;
}


/*
 * Run the given command and set fd_ to be positioned at the beginning of
 * the output. Return the status of the command (0 if ok).
 *
 * We run the command and put the output in a temp file, then open the
 * temp file so we can read it in the same way as the other URLS.  We
 * could read it from a pipe, but that makes error handling more
 * complicated (system() returns the exit status of the command).
 */
int HTTP::openCommand(const char* command)
{
    char cmd[2048];
    char tmpfile[80];
    strcpy(tmpfile, "/tmp/httpXXXXXX"); 
    mkstemp(tmpfile);
    sprintf(cmd, "%s > %s", command, tmpfile);
    if (system(cmd) != 0) {
	error("error executing command: ", command);
	unlink(tmpfile);
	return 1;
    }

    // The command may or may not output an HTTP type header,
    // so we just peek at the first few lines to see if there
    // is one there and save the values. The return value is
    // the number of lines in the header, or 0 if there was
    // none.
    int nHeaderLines = checkCommandOutput(tmpfile);
    
    // open the file descriptor as for the HTTP GET socket...
    int status = openFile(tmpfile);
    unlink(tmpfile);  // remove on close

    // skip over header lines, if any
    char buf[80];
    for(int i = 0; i < nHeaderLines; i++) {
	readline(buf, sizeof(buf));
    }

    return status;
}


/*
 * Scan the first few lines of the given file for HTTP header
 * info: Content-type, etc., and set the corresponding member 
 * vars. 
 * The return value is the number of lines in the header, 
 * or 0 if there was none.
 * Note that we are not sure in this case whether the file contains
 * any header lines or not.
 */
int HTTP::checkCommandOutput(const char* filename)
{
    ifstream is(filename);
    if (! is)
	return 0;
    
    char buf[80];
    int nHeaderLines = 0;
    for(int i = 0; i < 5; i++) {
	if (is.getline(buf, sizeof(buf))) {
	    if (strlen(buf) <= 2) {
		if (nHeaderLines > 0)
		    nHeaderLines++;
		break;
	    }
	    if (strncasecmp(buf, "Content-Length:", 15) == 0) {
		nHeaderLines++;
		if (sscanf(buf+15, "%d", &content_length_) == 1) {
		    if (feedback_) {
			fprintf(feedback_, "total length: %d bytes\n", content_length_);
			fflush(feedback_);
		    }
		}
	    }
	    else if (strncasecmp(buf, "Content-type:", 13) == 0) {
		nHeaderLines++;
		// save the content-type for later reference
		content_type_ = strdup(stripWhiteSpace(buf+13));
	    }
	    else if (strncasecmp(buf, "Content-Encoding:", 17) == 0) {
		nHeaderLines++;
		// save the content-type for later reference
		content_encoding_ = strdup(stripWhiteSpace(buf+17));
	    }
	    else if (nHeaderLines == 0) {
		// ignore unknown header lines only if we have a header...
		break;
	    }
	}
    }

    return nHeaderLines;
}


/*
 * Scan the given HTTP header line for any keywords that we are interested
 * in and save the values in member variables.
 */
void HTTP::scanHeaderLine(char* buf) 
{	
    if (strncasecmp(buf, "Content-Length:", 15) == 0) {
	if (sscanf(buf+15, "%d", &content_length_) == 1) {
	    if (feedback_) {
		fprintf(feedback_, "total length: %d bytes\n", content_length_);
		fflush(feedback_);
	    }
	}
    }
    else if (strncasecmp(buf, "Content-type:", 13) == 0) {
	// save the content-type for later reference
	content_type_ = strdup(stripWhiteSpace(buf+13));
    }
    else if (strncasecmp(buf, "Content-Encoding:", 17) == 0) {
	// save the content-type for later reference
	content_encoding_ = strdup(stripWhiteSpace(buf+17));
    }
    else if (strncasecmp(buf, "Location:", 9) == 0) {
	// Redirect to a new URL location
	location_ = strdup(stripWhiteSpace(buf+9));
    }
    else if (strncasecmp(buf, "WWW-Authenticate: Basic realm=\"", 31) == 0) {
	// Save the "realm" value (passwd domain) for later reference.
	// This also is an indication to the caller that a passwd is required.
	www_auth_realm_ = strdup(stripWhiteSpace(buf+31));
	int n = strlen(www_auth_realm_)-1;
	if (n > 0)
	    www_auth_realm_[n] = '\0';  // remove closing quote
    }
}


/*
 * reset any previous member values before a GET or POST
 */
void HTTP::reset() 
{
    if (content_type_) {
	free(content_type_);
	content_type_ = NULL;
    }
    if (content_encoding_) {
	free(content_encoding_);
	content_encoding_ = NULL;
    }
    if (www_auth_realm_) {
	free(www_auth_realm_);
	www_auth_realm_ = NULL;
    }
    if (location_) {
	free(location_);
	location_ = NULL;
    }
    content_length_ = 0;
}


/*
 * Set the username and password to use for authorization and, if realm
 * and server are given, save an entry in the auth_file for later
 * reference.
 *
 * This is normally called after an HTTP GET returned "401 Authorization
 * Required" and the user interface asked the user to type in a username
 * and password, but could also be called at other times, if the
 * necessary information is available.
 *
 * The server name can be obtained via the hostname() method after
 * calling get(). The realm string is returned by the method
 * www_auth_realm() after a get(). These values are also included in the
 * error message generated when the "401 Authorization Required" HTTP
 * result is received.
 */
void HTTP::authorize(const char* username, const char* passwd,
		     const char* realm, const char* server)
{
    // encode the username and passwd
    if (auth_info_) {
	free(auth_info_);
	auth_info_ = NULL;
    }
    char auth_info[1024];
    sprintf(auth_info, "%s:%s", username, passwd);
    auth_info_ = encode_base64(auth_info);  // encoded result is allocated
    
    // if realm and server are specified, save an entry to the auth_file_
    if (realm && server) 
	addAuthFileEntry(server, realm);
}


/*
 * Set the name of the file to use to store and look for HTTP
 * authorization info. '~' is translated into $HOME in the pathname.
 */
void HTTP::authFile(const char* s)
{
    if (auth_file_) {
	free(auth_file_);
	auth_file_ = NULL;
    }

    // replace '~' in auth_file_ if needed
    char filename[1024];
    if (s[0] == '~') {
	char* home = getenv("HOME");
	if (home)
	    strcpy(filename, home);
	strcat(filename, s+1);
	auth_file_ = strdup(filename);
    }
    else
	auth_file_ = strdup(s);
}


/*
 * Set the value of the HTTP User-Agent to use with requests
 */
void HTTP::userAgent(const char* s)
{
    if (user_agent_) 
	free(user_agent_);
    user_agent_ = strdup(s);
}


/*
 * Add an entry to the auth_file_ for the given server and realm and the
 * the current value of auth_info_. The file has the format:
 * 
 *  server:realm:auth_info
* 
 * Where: server is the HTTP server hostname
 *        realm is a string returned from the server (in the auth request)
 *        auth_info is the base64 encoded "username:passwd"
 */ 
int HTTP::addAuthFileEntry(const char* server, const char* realm) 
{
    if (!auth_file_)
	authFile(default_auth_file_);

    ifstream is(auth_file_);
    ostringstream os;
    char newentry[1024];
    sprintf(newentry, "%s:%s:%s", server, realm, auth_info_);
    char buf[1024];
    int n = strlen(server) + strlen(realm) + 1;
    while(is.getline(buf, sizeof(buf))) {
	if (strncmp(buf, newentry, n) != 0)
	    os << buf << endl;
    }
    is.close();
    os << newentry << endl;
    
    // create the auth file with -rw------- perms
    //ofstream f(auth_file_, ios::out, 0600);
    ofstream f(auth_file_, ios::out);
    chmod(auth_file_, 0600);
    if (f) 
	f << os.str();
    f.close();

    return 0;
}


/*
 * Search the auth_file_, if it exists, for an entry with the given server
 * and realm and set auth_info_ to the value found there.
 * Returns 0 if found, otherwise 1.
 */
int HTTP::findAuthFileEntry(const char* server, const char* realm) 
{
    if (!auth_file_)
	authFile(default_auth_file_);

    ifstream is(auth_file_);
    char entry[1024];
    sprintf(entry, "%s:%s:", server, realm);
    int n = strlen(entry);
    char buf[1024];
    while(is.getline(buf, sizeof(buf))) {
	if (strncmp(buf, entry, n) == 0) {
	    char* new_auth_info = buf+n;
	    if (auth_info_) {
		// if we find the same user/passwd information twice, it
		// probably means that the server rejected it and asked
		// for the password again. We have to return an error to
		// avoid an endless loop...
		if (strcmp(auth_info_, new_auth_info) == 0)
		    return 1;
		free(auth_info_);
	    }
	    auth_info_ = strdup(new_auth_info);
	    return 0;  // found
	}
    }
    return 1;  // not found
}


/*
 * This method is called when we receive a HTTP server request for
 * authorization (401). If we know the username and password, retry the
 * request with the encoded authorization info, otherwise, return a
 * special error message: "Authorization Required for <realm> at <host>"
 * so that the user interface can pop up a password dialog and then
 * eventually call HTTP::authorize() with the username and password.
 */
int HTTP::getAuthorization(const char* url)
{
    if (findAuthFileEntry(hostname_, www_auth_realm_) == 0) // found
	return get(url);

    return fmt_error("Authorization Required for %s at %s",
		     www_auth_realm_, hostname_);
}


/*
 * Get the value of the given URL and position the read fd after the
 * http header.
 *
 * This method accepts 3 types of URL:
 *
 * - http://host/path - URL: do an HTTP get
 *
 * - file:/path   - get the file
 *
 * - /path - command: exec the command and read the stdout
 *           (must be turned on explicitly with HTTP::allowUrlExec(int))
 *
 * Note that for http we have to open the connection once for each GET,
 * since HTTP automatically closes the connection after each GET.
 *
 * If an error occurs, 1 is returned, otherwise 0. Use readline() to
 * fetch the results.
 */
int HTTP::get(const char* url)
{
    // reset any previous values
    reset();

    // look for local file URL: "file:/..."
    if (strncmp(url, "file:", 5) == 0) {
	char filename[1024]; 
	if (sscanf(url, "file:%1023s", filename) == 1) {
	    if (openFile(filename) != 0)
		return 1;
	    return 0;
	}
	return error("can't parse URL: %s", url);
    }
    
    // If its not a HTTP URL, it should be a local command to exec
    if (strncmp(url, "http:", 5) != 0) {
	if (allowUrlExec_) {
	    return openCommand(url);
	}
	return error("invalid HTTP URL: ", url);
    }

    // look for URL: "http://host:port/args" or "http://host/args"
    char host[32];		// http host name
    int port = 80;		// http server port on host
    char args[1024];		// part of URL after host:port
    char req[2048];		// request sent to http

    // replace blanks or tabs in request with %20
    char new_url[1024];
    replace_blanks(url, new_url, sizeof(new_url));
    if (feedback_) {
	fprintf(feedback_, "url: %s\n", new_url); // mainly for debugging info
 	fflush(feedback_);
    }

    if (sscanf(new_url, "http://%31[^:/]:%d%1000s", host, &port, args) != 3 && 
	sscanf(new_url, "http://%31[^/]%1000s", host, args) != 2) {
	return error("bad URL format: ", new_url);
    }

    // open a connection to the http server on the given host/port,
    // make this the proxy server if necessary. Note we check the
    // proxy everytime so that it can be reconfigured on the fly (by
    // setting the http_proxy environment variable). This call also
    // checks if the host is in a domain that we do not want to proxy.
    checkProxy( host );
    if ( proxyport_ == -1 ) {
        if (open(host, port) != 0)
	    return 1;		// error
    }
    else {
        if (open(proxyname_, proxyport_) != 0)
            return 1;		// error

        // Request to proxy needs the fully qualified URL.
        strncpy( args, new_url, 1024 );

        // The apparent hostname and port are now wrong. Change these
        // to values that make sense in the feedback messages.
        strncpy( hostname_, host, 32 );
        port_ = port;
    }

    if (feedback_) {
	fprintf(feedback_, "sending request to %s...\n", hostname_);
	fflush(feedback_);
    }

    // generate the request
    ostringstream os;
    os << "GET " << args << " HTTP/1.0" << endl;

    // PWD: add the Host: header, this is required by some
    // interception proxy servers (these are transparent servers that
    // sniff port 80 traffic and don't require any client
    // configuration, this is a HTTP/1.1 standard header that does no
    // harm for 1.0).
    os << "Host: " << hostname_ << endl;

    // add the user-agent
    if (! user_agent_)
	userAgent(default_user_agent_);
    os << "User-Agent: " << user_agent_ << endl;

    // If we have authorization info (encoded username:passwd), include it
    // in the request
    if (auth_info_ != NULL) 
	os << "Authorization: Basic " << auth_info_ << endl;

    // add newline after request and null terminate
    os << endl;
    strncpy(req, os.str().c_str(), sizeof(req));
    
    // send the request
    int n = strlen(req);
    if (writen(req, n) != n) {
	char buf[255];
	sprintf(buf, "could not contact http server on %s:%d\n", hostname_, port_);
	if (feedback_) {
	    fprintf(feedback_, "%s", buf);
	    fflush(feedback_);
	}
	close(fd_);
	fd_ = -1;
	return sys_error(buf);;
    }

    if (feedback_) {
	fprintf(feedback_, "waiting for result from %s...\n", hostname_);
	fflush(feedback_);
    }

    // Read the result and position after the HTTP header, which ends with a blank line 
    char buf[1024];
    while (readline(buf, sizeof(buf)) > 2) {
	scanHeaderLine(buf);
    }
    if (location_) {
	// Redirect to a new URL location
	char* newurl = location_;
	location_ = NULL;  // don't want to overwrite url in reset()
	int status = get(newurl);
	free(newurl);
	return status;
    }
    if (authorizationRequired()) {
	// Server requested a username and password
	return getAuthorization(url);
    }

    return 0;
}


/*
 * do an HTTP get on the given URL and return a pointer to the result
 * of NULL if an error occurred.
 *
 * nlines is set to the number of lines in the result (not counting the
 * http header) or -1 for errors.
 *
 * If freeFlag is true, the return buffer is only valid until the next
 * call to this method or when this object is deleted, at which time the
 * memory for it is freed.
 */
char* HTTP::get(const char* url, int& nlines, int freeFlag)
{
    if (resultGC_) {
	delete[] resultGC_;
	resultGC_ = resultBuf_ = resultPtr_ = NULL;
    }

    if (get(url) != 0) {
	nlines = -1;
	return NULL;		// error
    }
	
    // read the data into a buffer
    ostringstream os;
    char buf[8*1024];
    nlines = 0;
    int n;
    if (feedback_) {
	int tot = 0;
	while ((n = read(fd_, buf, sizeof(buf))) > 0) {
	    fprintf(feedback_, "read %d bytes from %s\n", tot += n, hostname_);
	    fflush(feedback_);
	    os.write(buf, n);
	}
    }
    else {
	while ((n = read(fd_, buf, sizeof(buf))) > 0) {
	    os.write(buf, n);
	}
    }
    resultPtr_ = resultBuf_ = strdup(os.str().c_str());

    // count the lines
    // and remove "end of data" marker 
    char* p = resultBuf_;
    char* q = p;
    int errs = 0;
    for (p = resultBuf_; *p; p++) {
	if (*p == '\n') {
	    if (strncmp(q, "[EOD]", 5) == 0) {
		*q = '\0';
		break;
	    }
	    if (strncmp(q, "***", 3) == 0) {
		*p = '\0';
		error(q);
		if (feedback_) {
		    fprintf(feedback_, "%s\n", q);
		    fflush(feedback_);
		}
		errs++;
		break;
	    }
	    nlines++;
	    q = p+1;
	}
    }

    close(fd_);
    fd_ = -1;
    if (freeFlag)
	resultGC_ = resultBuf_;
    
    if (errs) {
	nlines = -1;
	return NULL;
    }

    if (feedback_) {
	fprintf(feedback_, "done: read %d lines from %s\n", nlines, hostname_);
	fflush(feedback_);
    }
	
    return resultBuf_;
}


/*
 * do an HTTP GET on the given URL and write the results to the
 * given stream.
 *
 * Returns 0 if successful, otherwise 1
 */
int HTTP::get(const char* url, ostream& os)
{
    if (get(url) != 0) {
	return 1;		// error
    }
    return copy(os);
}


/*
 * Do an HTTP POST using the given URL and data and position the read fd
 * after the http header of the result.
 *
 * This method accepts only standard http://... type URLs.
 *
 * If an error occurs, 1 is returned, otherwise 0. Use readline() to
 * fetch the results.
 *
 * Note: authorization is not implemented for POST yet, but could be added
 * easily. I'm not sure if we need it yet...
 */
int HTTP::post(const char* url, const char* data)
{
    // reset any previous values
    reset();

    // For now, only support http://... URLs
    if (strncmp(url, "http:", 5) != 0) {
	return error("Invalid URL for HTTP POST method");
    }

    // look for URL: "http://host:port/args" or "http://host/args"
    char host[32];		// http host name
    int port = 80;		// http server port on host
    char args[1024];		// part of URL after host:port
    char req[1024];		// request sent to http

    if (sscanf(url, "http://%31[^:/]:%d%1000s", host, &port, args) != 3 && 
	sscanf(url, "http://%31[^/]%1000s", host, args) != 2) {
	return error("bad URL format: ", url);
    }

    // open a connection to the http server on the given host/port,
    // make this the proxy server if necessary. Note we check the
    // proxy everytime so that it can be reconfigured on the fly (by
    // setting the http_proxy environment variable). This call also
    // checks if the host is in a domain that we do not want to proxy.
    checkProxy(host);
    if (proxyport_ == -1) {
        if (open(host, port) != 0)
	    return 1;		// error
    }
    else {
        if (open(proxyname_, proxyport_) != 0)
            return 1;		// error

        // Request to proxy needs the fully qualified URL.
        strncpy(args, url, 1024);

        // The apparent hostname and port are now wrong. Change these
        // to values that make sense in the feedback messages.
        strncpy(hostname_, host, 32);
        port_ = port;
    }

    if (feedback_) {
	fprintf(feedback_, "sending request to %s...\n", hostname_);
	fflush(feedback_);
    }

    // generate the HTTP POST command
    sprintf(req, "POST %s HTTP/1.0\nContent-type: text/plain\nContent-length: %d\n\n%s", 
	    args, strlen(data), data);

    int n = strlen(req);
    if (writen(req, n) != n) {
	char buf[255];
	sprintf(buf, "could not contact http server on %s:%d\n", hostname_, port_);
	if (feedback_) {
	    fprintf(feedback_, "%s", buf);
	    fflush(feedback_);
	}
	close(fd_);
	fd_ = -1;
	return sys_error(buf);;
    }

    if (feedback_) {
	fprintf(feedback_, "waiting for result from %s...\n", hostname_);
	fflush(feedback_);
    }

    // skip the HTTP header: ends with a blank line 
    char buf[1024];
    while (readline(buf, sizeof(buf)) > 2) {
	scanHeaderLine(buf);
    }
    if (location_) {
	// Redirect to a new URL location
	char* newurl = location_;
	location_ = NULL;  // don't want to overwrite url in reset()
	int status = post(newurl, data);
	free(newurl);
	return status;
    }

    return 0;
}


/*
 * do an HTTP POST using the given URL and data and write the 
 * results to the given stream.
 *
 * Returns 0 if successful, otherwise 1
 */
int HTTP::post(const char* url, const char* data, ostream& os)
{
    if (post(url, data) != 0) {
	return 1;		// error
    }
    return copy(os);
}


/*
 * copy the results of a previous get(url) to the given stream.
 *
 * Assumes that the connection is open and the header has been read,
 * i.e., get(url) was called.
 *
 * Returns 0 if successful, otherwise 1
 */
int HTTP::copy(ostream& os)
{
    char buf[8*1024];
    int n;
    if (feedback_) {
	int tot = 0;
	while((n = read(fd_, buf, sizeof(buf))) > 0) {
	    os.write(buf, n);
	    fprintf(feedback_, "read %d bytes from %s\n", tot += n, hostname_);
	    fflush(feedback_);
	}
    }
    else {
	while((n = read(fd_, buf, sizeof(buf))) > 0) {
	    os.write(buf, n);
	}
    }
    return 0;
}


/*
 * Return a pointer to the next line in the results of the previous call
 * to get(url, nlines). A null char is inserted in place of the newline
 * so that the line can be treated as a single string.
 *
 * A NULL is returned when there are no more lines in the results.
 */
char* HTTP::getNext()
{
    if (resultPtr_) {
	char* q = resultPtr_;
	char* p = strchr(q, '\n');
	if (p) {
	    *p++ = '\0';
	    resultPtr_ = p;
	    return q;
	}
    }
    return NULL;
}


/*
 * Read the line one byte at a time, looking for the newline.  We store
 * the newline in the buffer, then follow it with a null (the same as
 * fgets(3)).  Not very efficient but usefull for sockets.
 *
 * Returns the number of characters up to, but not including, the null
 * (the same as strlen(3)) or < 0 upon errors.
 *
 * Taken from Stevens (readline()), "Unix Network Programming"
 */
int HTTP::readline(char* ptr, int maxlen)
{
    int	n, rc;
    char	c;

    for (n = 1; n < maxlen; n++) {
	if ( (rc = read(fd_, &c, 1)) == 1) {
	    *ptr++ = c;
	    if (c == '\n')
		break;
	} else if (rc == 0) {
	    if (n == 1)
		return(0);	// EOF, no data read 
	    else
		break;		// EOF, some data was read
	} else
	    return(-1);		// error
    }

    *ptr = 0;
    return(n);
}


/*
 * Write "n" bytes to a descriptor.  Use in place of write() when fd is a
 * stream socket.
 *
 * Returns the number of characters written or <0 upon error.
 *
 * Taken from Stevens, "Unix Network Programming".
 */
int HTTP::writen(char* ptr, int nbytes)
{
    int	nleft, nwritten;

    nleft = nbytes;
    while (nleft > 0) {
	nwritten = write(fd_, ptr, nleft);
	if (nwritten <= 0)
	    return(nwritten);	// error 

	nleft -= nwritten;
	ptr   += nwritten;
    }
    return(nbytes - nleft);
}


/*
 *  Check if a proxy server is available and required to access the
 *  given host machine.
 *
 *  The proxy server, if available, is defined by the "http_proxy"
 *  environment variable. This should have the format:
 *
 *     http_proxy = "http://wwwcache.some.domain:port/"
 *
 *  A list of domains that do not require to be proxied (i.e. local
 *  machines etc.) is defined by the variable "http_noproxy". This
 *  should be a list of comma separated names, i.e.:
 *
 *     http_noproxy = "local.domain,national.domain"
 *
 *  If the given host is in one of these domains no proxy will be set
 *  up otherwise the member variables "proxyname_" and "proxyport_"
 *  will be defined (specifically proxyport_ will be set to -1 for no
 *  proxy). 
 *
 */
void HTTP::checkProxy( const char *host )
{
    //  Check if a proxy is available.
    proxyport_ = -1;
    char *proxy = getenv( "http_proxy" );
    if ( proxy != NULL ) {

        //  Parse the string into a hostname and port number. This
        //  should be in the form :
        //  "http://host:port/" or "http://host/"
        if ( sscanf( proxy, "http://%31[^:/]:%d", proxyname_, &proxyport_ ) == 2 ||
             sscanf( proxy, "http://%31[^/]", proxyname_ ) == 1 ) {
          
            //  Succeeded. Make sure port is valid.
            if ( proxyport_ == -1 ) {
                proxyport_ = 80;
            }

            //  Now see if we really need to use this for the given
            //  host. Note need a copy of variable as strtok modifies it.
            char *ptr = getenv( "http_noproxy" );
            if ( ptr != NULL ) {
                char *hostdomain = (char *) strchr(host, '.');
                if (hostdomain != NULL) {
                    hostdomain++;
		    // make a copy of the http_noproxy string for strtok
		    char buf[1024];
		    strncpy(buf, ptr, sizeof(buf)-1);
                    ptr = NULL;
		    char* noproxy = buf;
                    while ((ptr = strtok(noproxy, ", ")) != NULL) {
                        noproxy = NULL;
                        if (strcmp(hostdomain, ptr) == 0) {
                            proxyname_[0] = '\0';
                            proxyport_ = -1;
                            break;
                        }
                    }
                }
            }
        } 
	else {
            //  Scan failed to find valid proxy type address.
            proxyname_[0] = '\0';
            proxyport_ = -1;
        }
    } 
    else {
        //  No http_proxy variable. So nothing to do.
        proxyname_[0] = '\0';
        proxyport_ = -1;
    }

    //  Feedback for proxy if found/needed.
    if (feedback_) {
        if ( proxyport_ != -1 ) {
            fprintf(feedback_, "using proxy server %s:%d\n", proxyname_, proxyport_);
            fflush(feedback_);
        }
    }
}

