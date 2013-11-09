/*******************************************************************************

    Client/Server Data Input/Output Client (CSIOC) header.
 
    AUTHOR: Song Yom (HSTX)
    DATE:   10/93

    MODIFICATION HISTORY:
    DATE    PROGRAMMER      DESCRIPTION
    ----    ----------      -----------

*******************************************************************************/

/*
    Status values
*/

#define CSIOC_FATAL   -1
#define CSIOC_ERROR   0
#define CSIOC_SUCCESS 1

/*
    Maximum size of communication data buffer
*/

#define CSIOC_SIZE 4096

/*
    Function prototypes
*/

#ifdef sun
extern int csioc_connect();
extern void csioc_disconnect();
extern int csioc_input();
extern int csioc_output();
extern int csioc_status();
extern void csioc_putstatus();
extern char *csioc_getstatus();
#else
extern int csioc_connect(char *,int,int *);
extern void csioc_disconnect(int);
extern int csioc_input(int,char *,int *);
extern int csioc_output(int,char *,int);
extern int csioc_status(int);
extern void csioc_putstatus(char *);
extern char *csioc_getstatus(void);
#endif
