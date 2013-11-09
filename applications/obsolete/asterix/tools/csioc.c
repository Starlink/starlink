/*******************************************************************************

    Client/Server Data Input/Output Client (CSIOC) routines.

    AUTHOR: Song Yom (HSTX)
    DATE:   10/93

    MODIFICATION HISTORY:
    DATE    PROGRAMMER      DESCRIPTION
    ----    ----------      -----------

*******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>

/*
    Status values
*/

#define CSIO_FATAL -1
#define CSIO_ERROR 0
#define CSIO_SUCCESS 1

/*
    Maximum size of communication data buffer
*/

#define CSIO_SIZE 4096

/*
    Signals, data types, and macros
*/

#define SSTAT "1"
#define SSIGN "2"
#define MFLAG 1
#define DFLAG 2
#define MSIG "<m>"
#define DSIG "<d>"
#define LSIG 3
#define LLEN 12
#define IS_M(s) (s && *s && !strncmp(s,MSIG,LSIG) ? 1 : 0)
#define IS_D(s) (s && *s && strlen(s) && !strncmp(s,DSIG,LSIG) ? 1 : 0)
#define MINVAL(A,B) (A > B ? B : A)

/*
    Status messages
*/

#define CONNERROR "unable to connect to server"
#define SERVERROR "server has terminated"

/*
    Internal message buffer
*/

static char mbuffer[CSIO_SIZE] = {NULL};

/*
    Function prototypes (internal)
*/

#ifdef sun
static int csioc_send();
static int csioc_recv();
#else
static int csioc_send(int,char *,int,int);
static int csioc_recv(int,char *,int *,int *);
#endif

/*
    Function prototype (external)
*/

#ifdef sun
extern void csioc_putstatus();
extern char *itoa();
#else
extern void csioc_putstatus(char *);
extern char *itoa(int);
#endif

/*
    EXTERNAL: Establishes a data communication connection via TCP/IP stream
              type network socket with the data server running on a specified
              host.
*/

#ifdef sun
int csioc_connect(host,port,sd)
    char *host;
    int port;
    int *sd;
#else
int csioc_connect(char *host,int port,int *sd)
#endif
{
    int status = CSIO_ERROR;
    int addr;
    struct hostent h;
    struct hostent *hp;
    struct sockaddr_in sin;

    addr = inet_addr(host);
    if ((hp = gethostbyname(host)) ||
    (hp = gethostbyaddr((char *) &addr,sizeof(addr),AF_INET)))
    {
        bcopy((char *) hp,(char *) &h,sizeof(h));
        hp = &h;
        if ((*sd = socket(AF_INET,SOCK_STREAM,0)) != -1 &&
        fcntl(*sd,F_SETFL,O_SYNC) != -1)
        {
            bzero((char *) &sin,sizeof(sin));
            bcopy(hp->h_addr,(char *) &sin.sin_addr,hp->h_length);
            sin.sin_port = port;
            sin.sin_family = hp->h_addrtype;
            if (connect(*sd, (struct sockaddr *) &sin,sizeof(sin)) != -1)
                status = CSIO_SUCCESS;
        }
    }
    if (status != CSIO_SUCCESS)
        csioc_putstatus(CONNERROR);

    return status;
}

/*
    EXTERNAL: Terminates a previously established data communication
              connection with the data server.
*/

#ifdef sun
void csioc_disconnect(sd)
    int sd;
#else
void csioc_disconnect(int sd)
#endif
{
    close(sd);
    return;
}

/*
    EXTERNAL: Reads data from the data communication channel (blocked mode).
*/

#ifdef sun
int csioc_input(sd,buffer,length)
    int sd;
    char *buffer;
    int *length;
#else
int csioc_input(int sd,char *buffer,int *length)
#endif
{
    int status;
    int flag;

    if ((status = csioc_recv(sd,buffer,length,&flag)) == CSIO_SUCCESS &&
    (status = csioc_send(sd,SSIGN,sizeof(SSIGN),MFLAG)) == CSIO_SUCCESS)
    {
        if (flag == MFLAG)
            status = CSIO_ERROR;
    }
    else if (status == CSIO_FATAL)
        csioc_putstatus(SERVERROR);

    return status;
}

/*
    EXTERNAL: Writes data to the data communication channel (blocked mode).
*/

#ifdef sun
int csioc_output(sd,buffer,length)
    int sd;
    char *buffer;
    int length;
#else
int csioc_output(int sd,char *buffer,int length)
#endif
{
    int status;
    int flag;
    int ilength;
    char ibuffer[CSIO_SIZE];

    if ((status = csioc_recv(sd,ibuffer,&ilength,&flag)) == CSIO_SUCCESS)
    {
        if (flag == DFLAG)
        {
            if ((status = csioc_send(sd,SSIGN,sizeof(SSIGN),DFLAG)) ==
            CSIO_SUCCESS)
                status = csioc_recv(sd,ibuffer,&ilength,&flag);
        }
        if (status == CSIO_SUCCESS)
            status = csioc_send(sd,buffer,length,DFLAG);
    }

    if (status == CSIO_FATAL)
        csioc_putstatus(SERVERROR);

    return status;
}

/*
    EXTERNAL: Reads a status message from the data communication channel
              (blocked mode).
*/

#ifdef sun
int csioc_status(sd)
    int sd;
#else
int csioc_status(int sd)
#endif
{
    int status;
    int flag;
    int ilength;
    char ibuffer[CSIO_SIZE];

    if ((status = csioc_recv(sd,ibuffer,&ilength,&flag)) == CSIO_SUCCESS &&
    (status = csioc_send(sd,SSIGN,sizeof(SSIGN),MFLAG)) == CSIO_SUCCESS)
    {
        if (flag == DFLAG || strcmp(ibuffer,SSTAT))
        {
            status = CSIO_ERROR;
            csioc_putstatus(ibuffer);
        }
    }
    else if (status == CSIO_FATAL)
        csioc_putstatus(SERVERROR);

    return status;
}

/*
    EXTERNAL: Writes a status message to the internal message buffer.
*/

#ifdef sun
void csioc_putstatus(buffer)
    char *buffer;
#else
void csioc_putstatus(char *buffer)
#endif
{
    strncpy(mbuffer,buffer,MINVAL(CSIO_SIZE,strlen(buffer) + 1));

    return;
}

/*
    EXTERNAL: Returns the pointer to the internal message buffer.
*/

#ifdef sun
char *csioc_getstatus()
#else
char *csioc_getstatus(void)
#endif
{
    char *message;

    return message = *mbuffer ? mbuffer : NULL;
}

/*
    INTERNAL: Writes either data or message buffer to the communication
              channel.
*/
 
#ifdef sun
static int csioc_send(sd,buffer,length,flag)
    int sd;
    char *buffer;
    int length;
    int flag;
#else
static int csioc_send(int sd,char *buffer,int length,int flag)
#endif
{
    int status;
    int dlength;
    int ilength;
    char ibuffer[CSIO_SIZE + LSIG + LLEN];

    if (flag == MFLAG || flag == DFLAG)
    {
        if (flag == MFLAG)
            strcpy(ibuffer,MSIG);
        else
            strcpy(ibuffer,DSIG);
        dlength = MINVAL(length,CSIO_SIZE);
        ilength = dlength + LSIG + LLEN;
        memcpy(ibuffer + LSIG,itoa(dlength),LLEN);
        memcpy(ibuffer + LSIG + LLEN,buffer,dlength);
        if (write(sd,ibuffer,ilength) != -1)
            status = CSIO_SUCCESS;
        else
            status = CSIO_FATAL;
    }
    else
        status = CSIO_ERROR;

    return status;
}

/*
    INTERNAL: Reads either data or message buffer from the communication
              channel.
*/

#ifdef sun
static int csioc_recv(sd,buffer,length,flag)
    int sd;
    char *buffer;
    int *length;
    int *flag;
#else
static int csioc_recv(int sd,char *buffer,int *length,int *flag)
#endif
{
    int status;
    int dlength;
    int ilength;
    char ibuffer[CSIO_SIZE + LSIG + LLEN];

    if ((ilength = read(sd,ibuffer,CSIO_SIZE + LSIG + LLEN)) > 0)
    {
        *flag = IS_M(ibuffer) ? MFLAG : IS_D(ibuffer) ? DFLAG : 0;
        if (*flag)
        {
            *length = atoi(ibuffer + LSIG);
            dlength = ilength - LSIG - LLEN;
            memcpy(buffer,ibuffer + LSIG + LLEN,dlength);
            while (dlength < *length &&
            (ilength = read(sd,ibuffer,CSIO_SIZE - dlength)) > 0)
            {
                memcpy(buffer + dlength,ibuffer,ilength);
                dlength += ilength;
            }
            if (dlength == *length)
                status = CSIO_SUCCESS;
            else
                status = CSIO_ERROR;
        }
        else
            status = CSIO_ERROR;
    }
    else
        status = CSIO_FATAL;

    return status;
}

/*******************************************************************************

    This function converts integer to ascii.

    AUTHOR: Song Yom (HSTX)
    DATE:   03/93

    MODIFICATION HISTORY:
    DATE    PROGRAMMER      DESCRIPTION
    ----    ----------      -----------

*******************************************************************************/

#ifdef sun
extern void strrev();
#else
extern void strrev(char *);
#endif

#ifdef sun
char *itoa(n)
    int n;
#else
char *itoa(int n)
#endif
{
    int i = 0;
    int negative;
    static char buffer[12];
    char *t = buffer;

/*
    Set the sign, convert the number to ascii, assign the sign, and reverse
    the buffer.
*/

    if ((negative = (n < 0) ? 1 : 0))
        n = -n;
    do
    {
        *(t+i) = n % 10 + '0';
        ++i;
    } while ((n /= 10) > 0);
    if (negative)
        *(t+i++) = '-';
    *(t+i) = NULL;
    strrev(t);

    return buffer;
}

/*******************************************************************************

    This function reverses the order of a null-terminated character string.

    AUTHOR: Song Yom (HSTX)
    DATE:   03/93

    MODIFICATION HISTORY:
    DATE    PROGRAMMER      DESCRIPTION
    ----    ----------      -----------

*******************************************************************************/

#ifdef sun
void strrev(s)
    char *s;
#else
void strrev(char *s)
#endif
{
    int i;
    int j;
    int c;

    for (i = 0,j = strlen(s) - 1; i < j;++i,--j)
    {
        c = *(s+i);
        *(s+i) = *(s+j);
        *(s+j) = c;
    }

    return;
}
