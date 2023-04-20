/*
    Adam message system extensions to tcl.

     Authors :
      B.D.Kelly (ROE)
      D.L.Terrett (RAL)
      P.W.Draper (JAC, Durham University)

     History :
      16 March 1995 First stable version.
       7 July  1998 Avoid calling ams_init if message system is already running
      13 Sept  1999 Stop writing to interp->result directly & fix memory leak
      14 Sept  1999 Remove redundant Tcladam_Exit function, sort out exit
                    handler problems & enable stubs.
       5 Oct   2000 Retain local copies of the message status hash entries. (DSB)
      27 Jan   2006 Really remove Tcladam_Exit (PWD).
       6 Feb   2006 Add location of init.tcl script as part of package
                    start up. Also defines startcl_library.
      29 May   2007 Use tcl_findLibrary to locate startup script. More robust.
*/
#include "config.h"                   /* Local version */

#include <stdio.h>
#include <time.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>

#include "tcl.h"
#include "tk.h"

#include "sae_par.h"
#include "ems.h"
#include "ems_par.h"
#include "adam_defns.h"
#include "messys_len.h"
#include "dtask_err.h"

#include "messys_err.h"
#include "messys_par.h"

#include "ams.h"

#include "tclAdam.h"

#include "tclAdamInit.h"

/*
    Prototypes for static functions defined in this file.
*/
static int Tcladam_Getreply ( ClientData clientData, Tcl_Interp *interp,
    int argc, char *argv[]);
static int Tcladam_Receive ( ClientData clientData, Tcl_Interp *interp,
    int argc, char *argv[]);
static int Tcladam_Reply ( ClientData clientData, Tcl_Interp *interp, int argc,
    char *argv[]);
static int Tcladam_Send ( ClientData clientData, Tcl_Interp *interp, int argc,
    char *argv[]);
static int Tcladam_Start ( ClientData clientData, Tcl_Interp *interp, int argc,
    char *argv[]);
static int Tcladam_Path ( ClientData clientData, Tcl_Interp *interp, int argc,
    char *argv[]);
static void Tcladam_Strtocont ( char *contextname, int *context, int *status);
static void Tcladam_Strtostatus ( char *msg_status_name, int *msg_status,
    int *status);
static int Tcladam_ProcessMessage ( Tcl_Interp *interp, int path, int messid,
    int inmsg_status, int inmsg_context, char *inmsg_name, int inmsg_length,
    char *inmsg_value);
static void Tcladam_AppendStatus(Tcl_Interp *interp, int status);


/*  Buffer for translating Adam status's into messages */

static char status_buf[EMS__SZMSG+1];
static Tcl_HashTable codeTable;

/*+   TKADAM_PROCESSMESSAGE - process a message */

static int Tcladam_ProcessMessage
(
Tcl_Interp *interp,
int path,
int messid,
int inmsg_status,
int inmsg_context,
char *inmsg_name,
int inmsg_length,
char *inmsg_value
)

/*   Method :
      Determine the command name and set the tcl result to the
      various fields in the message.
*/

{
   int status;                    /* adam status */
   char command[32];              /* constructed tcl command */
   char task[32];                 /* task name */
   char number[10];               /* text representation of an integer */
   int tcl_status;                /* status for return to tcl */
   int length;                    /* length of message name */
   int j;                         /* loop counter */
   char *prompt = 0;
   char cstat[10];
   char *strPtr;
   Tcl_HashEntry *entryPtr;
   int new;
   Tcl_DString ds;
   char *facility, *ident, *text;

   status = SAI__OK;

/*  Look up the task name for this path */
   ams_plookup ( path, task, &status );
   if ( status != SAI__OK )
   {
      Tcl_AppendResult ( interp, "error looking up path name\n    ",
          (char *) NULL );
      Tcladam_AppendStatus( interp, status);
      tcl_status = TCL_ERROR;
   }
   else
   {
      if ( ( inmsg_context == OBEY ) || ( inmsg_context == CANCEL ) )
      {
         if ( inmsg_status == DTASK__ACTSTART )
         {
            strcpy ( command, "actstart" );
         }
         else if ( inmsg_status == MESSYS__PARAMREQ )
         {
            strcpy ( command, "paramreq" );
            prompt = inmsg_value;
         }
         else if ( inmsg_status == MESSYS__PARAMREP )
         {
            strcpy ( command, "paramrep" );
         }
         else if ( inmsg_status == MESSYS__INFORM )
         {
            strcpy ( command, "inform" );
         }
         else if ( inmsg_status == MESSYS__SYNC )
         {
            strcpy ( command, "sync" );
         }
         else if ( inmsg_status == MESSYS__TRIGGER )
         {
            strcpy ( command, "trigger" );
         }
         else if ( inmsg_status == SAI__OK )
         {
            strcpy ( command, "startmsg" );
         }
         else
         {
            strcpy ( command, "endmsg" );
         }

      }
      else if ( inmsg_context == GET )
      {
         strcpy ( command, "getresponse" );
      }
      else if ( inmsg_context == SET )
      {
         strcpy ( command, "setresponse" );
      }
      else if ( inmsg_context == CONTROL )
      {
         strcpy ( command, "controlresponse" );
      }
      else
      {
         if ( inmsg_status == MESSYS__PARAMREQ )
         {
            strcpy ( command, "paramreq" );
            prompt = inmsg_value;
         }
         else if ( inmsg_status == MESSYS__PARAMREP )
         {
            strcpy ( command, "paramrep" );
         }
         else if ( inmsg_status == MESSYS__INFORM )
         {
            strcpy ( command, "inform" );
         }
         else if ( inmsg_status == MESSYS__SYNC )
         {
            strcpy ( command, "sync" );
         }
         else
         {
            status = SAI__ERROR;
         }
      }

      if ( status == SAI__OK )
      {

         Tcl_AppendElement( interp, command);
         Tcl_AppendElement( interp, task);
         length = strlen ( inmsg_name );
         if ( ( length <= 0 ) || ( length >= MSG_NAME_LEN ) )
         {
            strcpy ( inmsg_name, "noname" );
         }
         Tcl_AppendElement( interp, inmsg_name);
         sprintf( number, "%d", path);
         Tcl_AppendElement( interp, number);
         sprintf( number, "%d", messid);
         Tcl_AppendElement( interp, number);

/*  Convert the status field to a list of the symbolic code and the
    message text */
         entryPtr = Tcl_CreateHashEntry(&codeTable, (char*)inmsg_status, &new);
         if (new)
         {
	    ems1_get_facility_error(inmsg_status, &facility, &ident, &text);
            Tcl_DStringInit(&ds);
	    if (strcmp(facility, "FACERR") != 0)
	    {
		Tcl_DStringAppend( &ds, " {", -1);
                Tcl_DStringAppendElement( &ds, facility);
                Tcl_DStringAppend( &ds, "__", -1);
                Tcl_DStringAppend( &ds, ident, -1);
                Tcl_DStringAppendElement( &ds, text);
                Tcl_DStringAppend( &ds, "}", -1);
	    }
            else
            {
               Tcl_DStringAppend( &ds, " {", -1);
               sprintf(cstat, "%d", inmsg_status);
               Tcl_DStringAppendElement( &ds, cstat);
               Tcl_DStringAppendElement( &ds, status_buf);
               Tcl_DStringAppend( &ds, "}", -1);
            }

/* Note, the hashtable only stores a pointer to the string, not a copy of
   the string. Therefore take a copy of the string so that we can free the
   DString. This copy is never freed and so constitutes an acceptable
   one-off memory leak (i.e. the amount of leaked memory will be limited to
   a small amount and will not increase indefinitely. */
            strPtr = malloc(Tcl_DStringLength(&ds)+1);
            strcpy(strPtr, Tcl_DStringValue(&ds));
            Tcl_DStringFree(&ds);
            Tcl_SetHashValue(entryPtr, strPtr);
         }

         Tcl_AppendResult(interp, Tcl_GetHashValue(entryPtr), (char*)NULL);

         if (prompt)
         {
            Tcl_AppendResult( interp, " {", (char*)NULL);
            Tcl_AppendElement( interp, prompt);
            prompt = prompt + strlen(prompt) + 1;
            Tcl_AppendElement( interp, prompt);
            prompt = prompt + strlen(prompt) + 1;
            Tcl_AppendElement( interp, prompt);
            prompt = prompt + strlen(prompt) + 1;
            Tcl_AppendElement( interp, prompt);
            prompt = prompt + strlen(prompt) + 1;
            Tcl_AppendElement( interp, prompt);
            Tcl_AppendResult( interp, "}", (char*)NULL);
         }
         else
         {

/*   Terminate and trim trailing blanks from the value string */

            if ( inmsg_length < (int)MSG_VAL_LEN )
            {
               inmsg_value[inmsg_length] = '\0';
            }
            else
            {
               inmsg_value[MSG_VAL_LEN-1] = '\0';
            }
            for ( j=strlen(inmsg_value)-1; j>0; j-- )
            {
               if ( inmsg_value[j] != ' ' )
               {
                  break;
               }
            }
            inmsg_value[j+1] = '\0';
         }

         Tcl_AppendElement( interp, inmsg_value);
         tcl_status = TCL_OK;
      }
      else
      {
          Tcl_AppendResult ( interp,
              "bad message context: message name was \"", inmsg_name,
               "\", message value was \"", inmsg_value, "\"", (char *) NULL );
          tcl_status = TCL_ERROR;
      }
   }
   return tcl_status;
}

/*+   TKADAM_GETREPLY - receive a message on specified path and messid */

static int Tcladam_Getreply
(
ClientData clientData,
Tcl_Interp *interp,
int argc,
char *argv[]
)

/*   Method :
      Wait for an adam message to arrive on the give path and messid.

        adam_getreply timeout path messid

      The form of the returned string is

        command task inmsg_name path messid inmsg_status inmsg_value
*/

{
   int path;                      /* path of received message */
   int messid;                    /* messid of received message */
   int timeout;                   /* timeout in millisec */
   int inmsg_status;              /* status of received message */
   int inmsg_context;             /* context of received message */
   char inmsg_name[MSG_NAME_LEN]; /* name of received message */
   int inmsg_length;              /* length of received message */
   char inmsg_value[MSG_VAL_LEN]; /* message value string */
   int status;                    /* adam status */
   int tcl_status;                /* status for return to tcl */

   status = SAI__OK;


   if ( argc == 4 )
   {

/*   Convert the arguments into integers */

      timeout = atoi ( argv[1] );
      path = atoi ( argv[2] );
      messid = atoi ( argv[3] );

/*   receive an adam message */

      ams_getreply ( timeout, path, messid, MSG_NAME_LEN, MSG_VAL_LEN,
        &inmsg_status, &inmsg_context, inmsg_name, &inmsg_length,
        inmsg_value, &status );

      if ( status != SAI__OK ) {
         Tcl_AppendResult ( interp, "error reading adam message\n    ",
            (char *) NULL );
         Tcladam_AppendStatus( interp, status);
         tcl_status = TCL_ERROR;
      }
      else
      {

/*     Extract the message contents into the tcl result. */
         tcl_status = Tcladam_ProcessMessage(interp, path, messid,
            inmsg_status, inmsg_context, inmsg_name, inmsg_length,
            inmsg_value);
      }
   }
   else
   {
      Tcl_AppendResult ( interp, "wrong # args:  should be \"", argv[0],
          " timeout  path messid\"", (char *) NULL );
      tcl_status = TCL_ERROR;
   }
   return tcl_status;
}


/*+   TCLADAM_INIT - initialise tcladam */

int Tcladam_Init (
Tcl_Interp *interp   /* tcl interpreter pointer */
)

/*   Method :
      Initialise stubs interface and adam_start command.
      Define the builtin variable startcl_library and locate the init script.
*/

{
   int result;
   const char *libDir;
   if ( Tcl_InitStubs( interp, "8.0", 0 ) == NULL ) return TCL_ERROR;

   Tcl_CreateCommand ( interp, "adam_start",
                       (Tcl_CmdProc *)Tcladam_Start,
                       (ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL );

   /* Locate the init.tcl script and arrange for scripts be autoloaded */
   result = Tcl_Eval( interp, initScript );

   return result;
}


/*+   TKADAM_RECEIVE - receive an adam message */

static int Tcladam_Receive
(
ClientData clientData,
Tcl_Interp *interp,
int argc,
char *argv[]
)

/*   Method :
      Wait for any adam message to arrive.

        command task inmsg_name path messid inmsg_status inmsg_value
*/

{
   int path;                      /* path of received message */
   int messid;                    /* messid of received message */
   int inmsg_status;              /* status of received message */
   int inmsg_context;             /* context of received message */
   char inmsg_name[MSG_NAME_LEN]; /* name of received message */
   int inmsg_length;              /* length of received message */
   char inmsg_value[MSG_VAL_LEN]; /* message value string */
   int status;                    /* adam status */
   int tcl_status;                /* status for return to tcl */

   status = SAI__OK;


/*   receive an adam message and lookup the name of the task it came from */

   ams_receive ( MESSYS__INFINITE, MSG_NAME_LEN, MSG_VAL_LEN,
     &inmsg_status, &inmsg_context, inmsg_name, &inmsg_length,
     inmsg_value, &path, &messid, &status );
   if ( status != SAI__OK ) {
      Tcl_AppendResult ( interp, "error reading adam message\n    ",
         (char *) NULL );
      Tcladam_AppendStatus( interp, status);
      tcl_status = TCL_ERROR;
      return tcl_status;
   }
   else
   {

/*  Copy the message contents to the tcl result. */
      tcl_status = Tcladam_ProcessMessage(interp, path, messid,
         inmsg_status, inmsg_context, inmsg_name, inmsg_length,
         inmsg_value);
   }

   return tcl_status;
}


/*+   TKADAM_REPLY - send an adam reply message */

static int Tcladam_Reply
(
ClientData clientData,
Tcl_Interp *interp,
int argc,
char *argv[]
)

/*   Method :
      Construct an adam message from the arguments to the Tcl command and
      send it. The Tcl command is

        adam_reply path messid status msg_name {value}
*/

{
   int path;                     /* path for reply */
   int messid;                   /* messid for reply */
   int msg_status;               /* message status */
   int status;                   /* adam status */
   int tcl_status;               /* status for return to tcl */

   status = SAI__OK;

   if ( argc == 6 )
   {
      path = atoi ( argv[1] );
      messid = atoi ( argv[2] );
      Tcladam_Strtostatus( argv[3], &msg_status, &status);
      ams_reply ( path, messid, MESSYS__MESSAGE, msg_status, OBEY,
        argv[4], strlen(argv[5]), argv[5], &status );
      if ( status != SAI__OK )
      {
         Tcl_AppendResult ( interp, "failed to send adam message\n    ",
             (char *) NULL );
         Tcladam_AppendStatus( interp, status);
         tcl_status = TCL_ERROR;
      }
      else
      {
         tcl_status = TCL_OK;
      }
   }
   else
   {
      Tcl_AppendResult ( interp, "wrong # args:  should be \"", argv[0],
          " path messid msg_status msg_name value\"", (char *) NULL );
      tcl_status = TCL_ERROR;
   }

   return tcl_status;
}


/*+   TKADAM_SEND - send an adam message */

static int Tcladam_Send
(
ClientData clientData,
Tcl_Interp *interp,
int argc,
char *argv[]
)

/*   Method :
      Construct an adam message from the arguments to the Tcl command and
      send it to the indicated task, returning the path and messid for
      the message.
      The Tcl command is

        adam_send task msg_name context {value}

      the returned string is

        {path messid}
*/

{
   int path;                     /* path for message */
   int messid;                   /* messid returned by ams_send */
   int context;                  /* context of message */
   int status;                   /* adam status */
   int tcl_status;               /* status for return to tcl */
   char path_spec[64];           /* formated path specification */


   status = SAI__OK;

   if ( argc == 5 )
   {

/*   get the communications path */

      ams_path ( argv[1], &path, &status );

      if ( status == SAI__OK )
      {
         Tcladam_Strtocont ( argv[3], &context, &status );
         if ( status == SAI__OK )
         {
            ams_send ( path, MESSYS__MESSAGE, SAI__OK, context, argv[2],
              strlen(argv[4]), argv[4], &messid, &status );
            if ( status == SAI__OK )
            {
               sprintf ( path_spec, "%d %d", path, messid );
               Tcl_SetResult( interp, path_spec, TCL_VOLATILE);
               tcl_status = TCL_OK;
            }
            else
            {
               Tcl_AppendResult ( interp,
                   "failed to send adam message to \"", argv[1],
                   "\"\n      ", (char *) NULL );
               Tcladam_AppendStatus(interp, status);
               tcl_status = TCL_ERROR;
            }
         }
         else
         {
            Tcl_AppendResult ( interp, "bad context \"", argv[3],
               "\": should be get, set, obey, or cancel",
                (char *) NULL );
            tcl_status = TCL_ERROR;
         }
      }
      else
      {
         Tcl_AppendResult ( interp,
            "failed to  get path to task \"", argv[1], "\"\n    ",
            (char *) NULL );
         Tcladam_AppendStatus( interp, status);
         tcl_status = TCL_ERROR;
      }
   }
   else
   {
      Tcl_AppendResult ( interp, "wrong # args:  should be \"", argv[0],
          " task msg_name context value\"", (char *) NULL );
      tcl_status = TCL_ERROR;
   }

   return tcl_status;
}


/*+   TKADAM_START - initialise tkadam */

static int Tcladam_Start
(
ClientData clientData,
Tcl_Interp *interp,
int argc,
char *argv[]
)

/*   Method :
      Initialise into the adam message system.
      The Tcl command is

        adam_start my_name

      where my_name is the name of this adam process.
*/

{
   int status;
   int tcl_status;

   status = SAI__OK;

   if ( argc == 2 )
   {
      ams_init ( argv[1], &status );
      if ( status == SAI__OK )
      {
         tcl_status = TCL_OK;
      }
      else
      {
         Tcl_AppendResult ( interp,
            "failed to open Adam message system\n    ", (char *) NULL );
         Tcladam_AppendStatus( interp, status);
         tcl_status = TCL_ERROR;
      }
   }
   else if ( argc > 2 )
   {
      Tcl_AppendResult ( interp, "wrong # args:  should be \"", argv[0],
          " name\"", (char *) NULL );
      tcl_status = TCL_ERROR;
   }

   Tcl_CreateCommand ( interp, "adam_receive",
                       (Tcl_CmdProc *)Tcladam_Receive,
                       (ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL );

   Tcl_CreateCommand ( interp, "adam_getreply",
                       (Tcl_CmdProc *)Tcladam_Getreply,
                       (ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL );

   Tcl_CreateCommand ( interp, "adam_reply",
                       (Tcl_CmdProc *)Tcladam_Reply,
                       (ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL );

   Tcl_CreateCommand ( interp, "adam_send",
                       (Tcl_CmdProc *)Tcladam_Send,
                       (ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL );

   Tcl_CreateCommand ( interp, "adam_path",
                       (Tcl_CmdProc *)Tcladam_Path,
                       (ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL );

   Tcl_InitHashTable( &codeTable, TCL_ONE_WORD_KEYS);

   return tcl_status;
}


/*+   TKADAM_PATH - get a path to a task */

static int Tcladam_Path
(
ClientData clientData,
Tcl_Interp *interp,
int argc,
char *argv[]
)

/*   Method :
      Get a path to an adam task
      The Tcl command is

        adam_path name

      where name is the name of an adam process. The result is set to 1
      if a path is successfully obtained and 0 if not.
*/

{
   int status;
   int path;
   int tcl_status;

   status = SAI__OK;

   if ( argc == 2 )
   {
      ams_path ( argv[1], &path, &status );
      if ( status == SAI__OK )
      {
         Tcl_SetResult(interp, "1", TCL_STATIC);
      }
      else
      {
         Tcl_SetResult(interp, "0", TCL_STATIC);
      }
      tcl_status = TCL_OK;
   }
   else
   {
      Tcl_AppendResult ( interp, "wrong # args:  should be \"", argv[0],
          " name\"", (char *) NULL );
      tcl_status = TCL_ERROR;
   }

   return tcl_status;
}


/*+   TKADAM_STRTOCONT - convert a string to a context number */

static void Tcladam_Strtocont
(
char *contextname,      /* string GET, SET, OBEY or CANCEL (given) */
int *context,           /* integer context flag (returned) */
int *status             /* global status (given and returned) */
)

/*   Method :
      The given string string is matched with the possible contexts and
      the corresponding adam flag returned.
*/

{
   char upcon[16];           /* uppercase version of given string */
   int length;               /* length of given string */
   int j;                    /* loop counter */

   if ( *status != SAI__OK ) return;

   length = strlen(contextname);
   if ( length < 16 )
   {
      for ( j=0; j<length+1; j++ )
      {
         upcon[j] = (char) toupper ( (int)contextname[j] );
      }

      if ( strcmp ( upcon, "GET" ) == 0 )
      {
         *context = GET;
      }
      else if ( strcmp ( upcon, "SET" ) == 0 )
      {
         *context = SET;
      }
      else if ( strcmp ( upcon, "OBEY" ) == 0 )
      {
         *context = OBEY;
      }
      else if ( strcmp ( upcon, "CANCEL" ) == 0 )
      {
         *context = CANCEL;
      }
      else if ( strcmp ( upcon, "CONTROL" ) == 0 )
      {
         *context = CONTROL;
      }
      else
      {
         *status = SAI__ERROR;
      }
   }
   else
   {
      *status = SAI__ERROR;
   }

}
/*+   TCLADAM_STRTOSTATUS - convert a string to a status number */

static void Tcladam_Strtostatus
(
char *msg_status_name,  /* string (given) */
int *msg_status,        /* integer message status (returned) */
int *status             /* global status (given and returned) */
)

/*   Method :
      The given string string is matched with the possible statuses and
      the corresponding adam flag returned.
*/

{
   char upcon[16];           /* uppercase version of given string */
   int length;               /* length of given string */
   int j;                    /* loop counter */


   if ( *status != SAI__OK ) return;

   length = strlen(msg_status_name);
   if ( length < 16 )
   {
      for ( j=0; j<length+1; j++ )
      {
         upcon[j] = (char) toupper ( (int)msg_status_name[j] );
      }
      if ( strcmp ( upcon, "ACTSTART" ) == 0 )
      {
         *msg_status = DTASK__ACTSTART;
      }
      else if ( strcmp ( upcon, "ACTCOMPLETE" ) == 0 )
      {
         *msg_status = DTASK__ACTCOMPLETE;
      }
      else if ( strcmp ( upcon, "PARAMREP" ) == 0 )
      {
         *msg_status = MESSYS__PARAMREP;
      }
      else if ( strcmp ( upcon, "PARAMREQ" ) == 0 )
      {
         *msg_status = MESSYS__PARAMREQ;
      }
      else if ( strcmp ( upcon, "INFORM" ) == 0 )
      {
         *msg_status = MESSYS__INFORM;
      }
      else if ( strcmp ( upcon, "SYNC" ) == 0 )
      {
         *msg_status = MESSYS__SYNC;
      }
      else if ( strcmp ( upcon, "SYNCREP" ) == 0 )
      {
         *msg_status = MESSYS__SYNCREP;
      }
      else if ( strcmp ( upcon, "TRIGGER" ) == 0 )
      {
         *msg_status = MESSYS__TRIGGER;
      }
      else
      {
         *status = SAI__ERROR;
      }
   }
   else
   {
      *status = SAI__ERROR;
   }
}


static void Tcladam_AppendStatus
(
Tcl_Interp *interp,       /* Tcl interpreter */
int istat                /* Adam status value */
)
/*   Method :
        The adam status is converted to a text string and the message
        part appended to the Tcl result.
*/
{
   char *facility, *ident, *text;

   ems1_get_facility_error(istat, &facility, &ident, &text);
   Tcl_AppendResult( interp,  text, (char*)NULL);
}
