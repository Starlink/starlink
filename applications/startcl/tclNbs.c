#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>

#include "tcl.h"
#include "tclNbs.h"
#include "ems.h"
#include "nbs_typ.h"
#include "nbs.h"


#define TKNBS__SZNAM 48

/*   structure for details of each mapped noticeboard */

typedef struct nbs_entry_type {
                                 char nbs_name[TKNBS__SZNAM];
                                 int nbs_mapped;
                                 int nbs_topid;
                                 struct nbs_entry_type *next;
                              } nbs_entry_type;


static int tclnbs_cmd(ClientData, Tcl_Interp*, int, char*[]);
static void tclnbs_findnbs(char*, int, int*, int*, char*, int*, int*);
static void tclnbs_get(char*, Tcl_Interp*, int*);
static void tclnbs_getnbs(Tcl_Interp*, int, char*, Tcl_DString*, int*);
static void tclnbs_mapnbs(char*, int*, int*, int*);
static void tclnbs_put(char*, char*, Tcl_Interp*, int*);
static void tclnbs_info(char*, Tcl_Interp*, int*);
static void tclnbs_putnbs(Tcl_Interp*, int, char*, char*, int*);

/*   structure for details of each nbs item to be monitored */

typedef struct mon_entry_type {
                                 Tcl_Interp *interp;
                                 char nbs_name[TKNBS__SZNAM];
                                 char nbs_type[16];
                                 int nbs_mapped;
                                 int nbs_topid;
                                 int nbs_found;
                                 int nbs_id;
                                 Tcl_DString tcl_name;
                                 struct mon_entry_type *next;
                              } mon_entry_type;

typedef struct timer_struct {
                 Tcl_Interp *interp;
                 Tcl_TimerToken timertoken;     /* identifier for tcl timer */
                 int timer_set;                 /* timer set flag */
                 int milliseconds;              /* timer interval */
                            } timer_struct;

static void tclnbs_monitor(char*, char*, Tcl_Interp*, int*);
static void tclnbs_moninfo(Tcl_Interp*, int*);
static void tclnbs_clear(Tcl_Interp*);
static void tclnbs_scan(ClientData);
static void tclnbs_start(char*, Tcl_Interp*, timer_struct*, int*);
static void tclnbs_stop (ClientData);

static nbs_entry_type *nbslist = 0;
static mon_entry_type *entrylist = 0;


/*+   TKNBS_INIT - initialise nbs commands */

int Tclnbs_Init
(
Tcl_Interp *interp
)

/*   Method :
      Define the nbs command.
     Author :
      B.D.Kelly (ROE)
      D.L.Terrett (RAL)
     History :
      22June1994: original (BDK)
      13September1999: Set dims to null to remove compiler warning (DLT)
*/

{
   timer_struct* timer;

/*  Allocate a timer structure */
   timer = (timer_struct*)malloc(sizeof (timer_struct));

   Tcl_CreateCommand ( interp, "nbs", (Tcl_CmdProc *)tclnbs_cmd,
                       (ClientData)timer, (Tcl_CmdDeleteProc *)tclnbs_stop );
   return TCL_OK;
}



/*=   TKNBS_CMD - execute the tclnbs command */

static int tclnbs_cmd
(
ClientData clientData,
Tcl_Interp *interp,
int argc,
char *argv[]
)

/*   Method :
      Implement the tcl command "nbs". There are seven nbs
      operations:

        nbs get <nbs_name>
           return the named NBS item as a string
        nbs put <nbs_name> <value>
           put the given string into the NBS item
        nbs monitor <nbs_name> <tcl_name>
           add to the list of NBS items being monitored
        nbs monitor
           return a list of NBS items being monitored
        nbs clear
           empty the list of items being monitored
        nbs start <millisec>
           start monitoring at the given loop time
        nbs stop
           stop monitoring
        nbs info <nbs_name>
           returns information about an the NBS item
     Author :
      B.D.Kelly (ROE)
     History :
      22June1994: original (BDK)
      23Nov 1994: add get and put, and change the facility name (BDK)
*/

{

   int tcl_status;          /* status to be returned */
   int istat;

   emsMark();

   tcl_status = TCL_OK;

   if ( argc < 2 )
   {
      Tcl_AppendResult(interp, "wrong # args:  should be \"",
         argv[0], " option ?arg ...?\"", (char *) NULL);
      tcl_status = TCL_ERROR;
   }
   else
   {

      if ( strcmp ( argv[1], "get" ) == 0 )
      {
         if ( argc == 3 )
         {
            tclnbs_get ( argv[2], interp, &tcl_status );
         }
         else
         {
            Tcl_AppendResult(interp, "wrong # args:  should be \"",
               argv[0], " ", argv[1], " item\"", (char *) NULL);
            tcl_status = TCL_ERROR;
         }
      }
      else if ( strcmp ( argv[1], "put" ) == 0 )
      {
         if ( argc == 4 )
         {
            tclnbs_put ( argv[2], argv[3], interp, &tcl_status );
         }
         else
         {
            Tcl_AppendResult(interp, "wrong # args:  should be \"",
               argv[0], " ", argv[1], " item value\"", (char *) NULL);
            tcl_status = TCL_ERROR;
         }
      }
      else if ( strcmp ( argv[1], "info" ) == 0 )
      {
         if ( argc == 3 )
         {
            tclnbs_info ( argv[2], interp, &tcl_status );
         }
         else
         {
            Tcl_AppendResult(interp, "wrong # args:  should be \"",
               argv[0], " ", argv[1], " item \"", (char *) NULL);
            tcl_status = TCL_ERROR;
         }
      }
      else if ( strcmp ( argv[1], "monitor" ) == 0 )
      {
         if ( argc == 2 )
         {
            tclnbs_moninfo ( interp, &tcl_status );
         }
         else if ( argc == 4 )
         {
            tclnbs_monitor ( argv[2], argv[3], interp, &tcl_status );
         }
         else
         {
            Tcl_AppendResult(interp, "wrong # args:  should be \"",
               argv[0], " ", argv[1], " name variable\"", (char *) NULL);
            tcl_status = TCL_ERROR;
         }
      }
      else if ( strcmp ( argv[1], "clear" ) == 0 )
      {
         if ( argc == 2 )
         {
            tclnbs_clear ( interp );
         }
         else
         {
            Tcl_AppendResult(interp, "wrong # args:  should be \"",
               argv[0], " ", argv[1], "\"", (char *) NULL);
            tcl_status = TCL_ERROR;
         }
      }
      else if ( strcmp ( argv[1], "start" ) == 0 )
      {
         if ( argc == 3 )
         {
            tclnbs_start ( argv[2], interp, (timer_struct*)clientData,
               &tcl_status );
         }
         else
         {
            Tcl_AppendResult(interp, "wrong # args:  should be \"",
               argv[0], " ", argv[1], " interval\"", (char *) NULL);
            tcl_status = TCL_ERROR;
         }
      }
      else if ( strcmp ( argv[1], "stop" ) == 0 )
      {
         if ( argc == 2 )
         {
            tclnbs_stop (clientData);
         }
         else
         {
            Tcl_AppendResult(interp, "wrong # args:  should be \"",
               argv[0], " ", argv[1], "\"", (char *) NULL);
            tcl_status = TCL_ERROR;
         }
      }
      else
      {
         Tcl_AppendResult(interp, "bad option \"", argv[1],
            "\":  should be put, get, monitor, start, stop or info",
            (char *) NULL);
         tcl_status = TCL_ERROR;
      }

      emsAnnul(&istat);
      emsRlse();
   }
   return tcl_status;
}


/*=  TKNBS_FINDNBS - find an item in a noticeboard */

static void tclnbs_findnbs
(
char *nbs_name,       /* full name of item (given) */
int nbs_topid,        /* noticeboard id (given) */
int *nbs_id,          /* nbs id of the item (returned) */
int *prim,            /* item is primitive (returned) */
char *nbs_type,       /* type of the item (returned) */
int *nbs_found,       /* flag indicating item found (returned) */
int *status           /* global status (given and returned) */
)

/*   Method :
      Find the named item in a noticeboard.
     Author :
      B.D.Kelly (ROE)
      D.L.Terrett (RAL)
     History :
      30June1994: original (BDK)
      29Nov 1994: fix one-off in splitting nested structures (BDK)
      01Mar 1995: add primitive argument (DLT)
*/

{
   char name[TKNBS__SZNAM];   /* name of item */
   char *startname;            /* pointer to start of name component */
   char *endname;              /* pointer to end of name component */
   int temptopid;              /* temporary nbs id */
   int tempid;                 /* temporary nbs id */

   if ( *status != TCL_OK ) return;

   strcpy ( name, nbs_name );
   temptopid = nbs_topid;
   startname = strchr ( name, '.' );
   if ( startname != 0 )
   {
      for ( ; ; )
      {
         startname++;
         endname = strchr ( startname, '.' );
         if ( endname != 0 )
         {
            *endname = '\0';
            nbc_find_item ( temptopid, startname, &tempid, status );
            if ( *status != TCL_OK )
            {
               break;
            }
            startname = endname;
            temptopid = tempid;
         }
         else
         {
            nbc_find_item ( temptopid, startname, &tempid, status );
            nbc_get_primitive( tempid, prim, status );
            if (*prim) nbc_get_type ( tempid, nbs_type, status );
            if ( *status == TCL_OK )
            {
               *nbs_id = tempid;
               *nbs_found = 1;
            }
            break;
         }
      }
   }
}


/*=   TKNBS_GET - copy data from NBS to tcl string */

static void tclnbs_get
(
char *nbsitem,          /* full name of NBS item (given) */
Tcl_Interp *interp,     /* pointer to interpreter structure (given) */
int *tcl_status         /* global status (given and returned) */
)

/*   Method :
      Copy data from NBS, convert to ascii and put into tcl return string.
     Author :
      B.D.Kelly (ROE)
     History :
      23Nov1994: original (BDK)
*/

{
   Tcl_DString string;     /* Dynamic string */
   int status;             /* status */
   int nbsid;              /* NBS id of item */
   int topid;              /* id of noticeboard */
   int mapped;             /* flag for whether noticeboard mapped */
   char nbstype[16];       /* type of item in noticeboard */
   int found;              /* flag for item found */
   int prim;               /* item is primitive */


   if ( *tcl_status != TCL_OK ) return;

   status = TCL_OK;

/*   Get id of noticeboard */

   tclnbs_mapnbs ( nbsitem, &topid, &mapped, &status );

   if ( mapped == 1 )
   {

/*   Find the item within the noticeboard */

      tclnbs_findnbs ( nbsitem, topid, &nbsid, &prim, nbstype, &found, &status );
      if ( found == 1 )
      {
         if (prim)
         {

/*   Get the value as a character string from NBS */

            Tcl_DStringInit(&string);
            tclnbs_getnbs ( interp, nbsid, nbstype, &string, &status );
            if ( status == TCL_OK )
            {
               Tcl_DStringResult(interp, &string);
            }
            else
            {
               Tcl_AppendResult(interp, "failed to get NBS item \"",
                  nbsitem, "\"", (char *) NULL);
               *tcl_status = TCL_ERROR;
            }
            Tcl_DStringFree(&string);
         }
         else
         {
            Tcl_AppendResult(interp,
               "cannot get value of structured nbs item \"", nbsitem, "\"",
               (char *) NULL);
            *tcl_status = TCL_ERROR;
         }
      }
      else
      {
         Tcl_AppendResult(interp, "nbs item \"",
            nbsitem, "\" not found", (char *) NULL);
         *tcl_status = TCL_ERROR;
      }
   }
   else
   {
      Tcl_AppendResult(interp, "cannot map noticeboard for nbs item \"",
            nbsitem, "\"", (char *) NULL);
      *tcl_status = TCL_ERROR;
   }

}


/*=  TKNBS_GETNBS - get a value from NBS */

static void tclnbs_getnbs
(
Tcl_Interp *interp,      /* tcl interpreter */
int nbs_id,              /* nbs item identifier (given) */
char *nbs_type,          /* type of nbs item (given) */
Tcl_DString *tcl_value,  /* value of item in ASCII (returned) */
int *status              /* global status (given and returned) */
)

/*   Method :
      Get the value from nbs into the correct type, and then convert to
      ASCII.
     Author :
      B.D.Kelly (ROE)
     History :
      22June1994: original (BDK)
*/

{
   int actbytes;          /* actual number of bytes returned */
   int maxbytes;          /* maximum number of bytes */
   int ival;              /* integer value */
   float fval;            /* float value */
   double dval;           /* double value */
   int i;                 /* loop index */
   int items;             /* number of elements in item */
   char cval[16];        /* string replresentation of value */

   if ( *status != TCL_OK ) return;

   nbc_get_size( nbs_id, &maxbytes, &actbytes, status);

   if ( strcmp ( nbs_type, "_INTEGER" ) == 0 )
   {
      items = actbytes/4;
      for (i=0; i < items; i++)
      {
         nbc_get_value ( nbs_id, i, 4, &ival, &actbytes, status );
         sprintf ( cval, "%d", ival );
         Tcl_DStringAppendElement ( tcl_value, cval );
      }
   }
   else if ( strcmp ( nbs_type, "_REAL" ) == 0 )
   {
      items = actbytes/4;
      for (i=0; i < items; i++)
      {
         nbc_get_value ( nbs_id, i, 4, &fval, &actbytes, status );
         sprintf ( cval, "%d", ival );
         Tcl_PrintDouble( interp, (double)fval, cval);
         Tcl_DStringAppendElement ( tcl_value, cval );
      }
   }
   else if ( strcmp ( nbs_type, "_DOUBLE" ) == 0 )
   {
      items = actbytes/8;
      for (i=0; i < items; i++)
      {
         nbc_get_value ( nbs_id, i, 8, &dval, &actbytes, status );
         sprintf ( cval, "%d", ival );
         Tcl_PrintDouble( interp, dval, cval);
         Tcl_DStringAppendElement ( tcl_value, cval );
      }
   }
   else if ( strcmp ( nbs_type, "_LOGICAL" ) == 0 )
   {
      items = actbytes/4;
      for (i=0; i < items; i++)
      {
         nbc_get_value ( nbs_id, i, 4, &ival, &actbytes, status );
         if (ival)
         {
            Tcl_DStringAppendElement ( tcl_value, "1" );
         }
         else
         {
            Tcl_DStringAppendElement ( tcl_value, "0" );
         }
      }
   }
   else
   {
      Tcl_DStringSetLength(tcl_value, actbytes);
      nbc_get_value ( nbs_id, 0, actbytes, Tcl_DStringValue(tcl_value),
         &actbytes, status );
      Tcl_DStringSetLength(tcl_value, actbytes);
   }
}


/*=  TKNBS_MAPNBS - map a noticeboard */

static void tclnbs_mapnbs
(
char *nbs_name,        /* full name of noticeboard item (given) */
int *nbs_topid,        /* noticeboard id (returned) */
int *nbs_mapped,       /* flag indicating noticeboard is mapped
                          (returned) */
int *status            /* global status (given and returned) */
)
/*   Method :
      Return an id for the noticeboard implied by the nbs name.
     Author :
      B.D.Kelly (ROE)
     History :
      30June1994: original (BDK)
      23Nov 1994: return values when noticeboard newly mapped (BDK)
*/

{
   char noticeboard[TKNBS__SZNAM]; /* name of noticeboard */
   char *endname;                  /* pointer to end of noticeboard name */
   nbs_entry_type *tempnbs;        /* pointer to noticeboard details */
   int topid;                      /* id for newly-mapped noticeboard */

   if ( *status != TCL_OK ) return;


   *nbs_mapped = 0;

/*   separate the noticeboard component of the name */

   strcpy ( noticeboard, nbs_name );
   endname = strchr ( noticeboard, '.' );
   if ( endname != 0 )
   {
      *endname = '\0';
   }
   tempnbs = nbslist;

/*   search for an entry for the noticeboard */

   for ( ; ; )
   {
      if ( tempnbs == 0 )
      {
         break;
      }
      if ( strcmp ( tempnbs->nbs_name, noticeboard ) == 0 )
      {
         *nbs_topid = tempnbs->nbs_topid;
         *nbs_mapped = tempnbs->nbs_mapped;
         break;
      }
      else
      {
         tempnbs = tempnbs->next;
      }
   }

   if ( tempnbs == 0 )
   {

/*   noticeboard has not been mapped. Try to map it */

      nbc_find_noticeboard ( noticeboard, &topid, status );
      if ( *status == TCL_OK )
      {

/*   create an entry in the list */

         tempnbs = (nbs_entry_type *) malloc ( sizeof ( nbs_entry_type ) );
         if ( tempnbs != 0 )
         {
            strcpy ( tempnbs->nbs_name, noticeboard );
            tempnbs->nbs_topid = topid;
            tempnbs->nbs_mapped = 1;
            tempnbs->next = nbslist;
            nbslist = tempnbs;
            *nbs_topid = tempnbs->nbs_topid;
            *nbs_mapped = tempnbs->nbs_mapped;
         }
         else
         {
            *status = TCL_ERROR;
         }
      }
      else
      {
         *status = TCL_ERROR;
      }
   }
}


/*=   TKNBS_PUT - copy data into NBS */

static void tclnbs_put
(
char *nbsitem,          /* full name of NBS item (given) */
char *value,            /* value to be put (given) */
Tcl_Interp *interp,     /* pointer to interpreter structure (given) */
int *tcl_status         /* global status (given and returned) */
)

/*   Method :
      Locate the NBS item and its type and put the value.
     Author :
      B.D.Kelly (ROE)
     History :
      23Nov1994: original (BDK)
*/

{

   int status;             /* status */
   int nbsid;              /* NBS id of item */
   int topid;              /* id of noticeboard */
   int mapped;             /* flag for whether noticeboard mapped */
   char nbstype[16];       /* type of item in noticeboard */
   int found;              /* flag for item found */
   int prim;               /* item is primitive */


   if ( *tcl_status != TCL_OK ) return;

   status = TCL_OK;

/*   Get id of noticeboard */

   tclnbs_mapnbs ( nbsitem, &topid, &mapped, &status );

   if ( mapped == 1 )
   {

/*   Find the item within the noticeboard */

      tclnbs_findnbs ( nbsitem, topid, &nbsid, &prim, nbstype, &found, &status );
      if ( found == 1 )
      {
         if (prim)
         {

/*   Put the value into NBS with type conversion */

            tclnbs_putnbs ( interp, nbsid, nbstype, value, &status );
            if ( status != TCL_OK )
            {
               Tcl_AppendResult(interp, "\n failed to put nbs item \"",
                   nbsitem, "\"", (char *) NULL);
               *tcl_status = TCL_ERROR;
            }
         }
         else
         {
            Tcl_AppendResult(interp,
               "cannot put value of structured nbs item ", nbsitem,
               (char *) NULL);
            *tcl_status = TCL_ERROR;
         }
      }
      else
      {
         Tcl_AppendResult(interp, "failed to find nbs item \"", nbsitem,
            "\"", (char *) NULL);
         *tcl_status = TCL_ERROR;
      }
   }
   else
   {
      Tcl_AppendResult(interp, "failed to map nbs noticeboard for item \"",
         nbsitem, "\"", (char *) NULL);
      *tcl_status = TCL_ERROR;
   }

}


/*=  TKNBS_PUTNBS - put a value into NBS */

static void tclnbs_putnbs
(
Tcl_Interp *interp,      /* interpreter structure (given and returned) */
int nbs_id,              /* nbs item identifier (given) */
char *nbs_type,          /* type of nbs item (given) */
char *tcl_value,         /* value of item in ASCII (given) */
int *status              /* global status (given and returned) */
)

/*   Method :
      Convert from ASCII into the correct type and then put the value
      into NBS.
     Author :
      B.D.Kelly (ROE)
     History :
      23Nov1994: original (BDK)
*/

{
   int ival;              /* integer value */
   float fval;            /* float value */
   double dval;           /* double value */
   int oldtune;           /* old NBS write flag */
   int maxbytes;          /* max size of nbs item */
   int actbytes;          /* size of nbs item */
   int argc;              /* count of list elements */
   char** argv;           /* pointer to list of elements */
   int i;                 /* loop index */
   char cint[16];         /* string representation of integer */


   if ( *status != TCL_OK ) return;

/*   Set NBS to world write */

   nbc_tune ( "WORLD WRITE", 1, &oldtune, status );

/*   Get the size of the item */
   nbc_get_size( nbs_id, &maxbytes, &actbytes, status);

   if ( strcmp ( nbs_type, "_CHAR" ) == 0 )
   {
      if (strlen(tcl_value) > maxbytes)
      {
         sprintf(cint, "%d", maxbytes);
         Tcl_AppendResult( interp, "string \"", tcl_value,
            "\" is longer than nbs item: should be ", cint, (char *)NULL);
         *status = TCL_ERROR;
      }
      else
      {
         nbc_put_value ( nbs_id, 0, strlen(tcl_value), tcl_value, status );
         for (i=strlen(tcl_value); i < actbytes; i++)
         {
             nbc_put_value( nbs_id, i, 1, " ", status);
         }
      }
   }
   else if ( strcmp ( nbs_type, "_INTEGER" ) == 0 ||
      strcmp ( nbs_type, "_LOGICAL" ) == 0 ||
      strcmp ( nbs_type, "_REAL" ) == 0 ||
      strcmp ( nbs_type, "_DOUBLE" ) == 0 )
   {
      if (Tcl_SplitList(interp, tcl_value, &argc, &argv) == TCL_OK)
      {
         if ( strcmp ( nbs_type, "_DOUBLE" ) == 0 )
         {
            if (argc != actbytes/8)
            {
               sprintf(cint, "%d", actbytes);
               Tcl_AppendResult( interp, "wrong number of values: should be ",
                   cint, (char*)NULL);
               *status = TCL_ERROR;
            }
            else
            {
               for (i=0; i < argc; i++)
               {
                  if (Tcl_GetDouble(interp, argv[i], &dval) == TCL_OK)
                  {
                     nbc_put_value ( nbs_id, i, 8, &dval, status );
                  }
                  else
                  {
                     *status = TCL_ERROR;
                     break;
                  }
               }
            }
         }
         else
         {
            if (argc != actbytes/4)
            {
               sprintf(cint, "%d", actbytes);
               Tcl_AppendResult( interp, "wrong number of values: should be ",
                   cint, (char*)NULL);
               *status = TCL_ERROR;
            }
            else
            {
               if ( strcmp ( nbs_type, "_INTEGER" ) == 0 )
               {
                  for (i=0; i < argc; i++)
                  {
                     if (Tcl_GetInt(interp, argv[i], &ival) == TCL_OK)
                     {
                        nbc_put_value ( nbs_id, i, 4, &ival, status );
                     }
                     else
                     {
                        *status = TCL_ERROR;
                        break;
                     }
                  }
               }
               else if ( strcmp ( nbs_type, "_REAL" ) == 0 )
               {
                  for (i=0; i < argc; i++)
                  {
                     if (Tcl_GetDouble(interp, argv[i], &dval) == TCL_OK)
                     {
                        fval = (float)dval;
                        nbc_put_value ( nbs_id, i, 4, &fval, status );
                     }
                     else
                     {
                        *status = TCL_ERROR;
                        break;
                     }
                  }
               }
               else if ( strcmp ( nbs_type, "_LOGICAL" ) == 0 )
               {
                  for (i=0; i < argc; i++)
                  {
                     if (Tcl_GetBoolean(interp, argv[i], &ival) == TCL_OK)
                     {
                        nbc_put_value ( nbs_id, i, 4, &ival, status );
                     }
                     else
                     {
                        *status = TCL_ERROR;
                        break;
                     }
                  }
               }
            }
         }
         free ((char*) argv);
      }
   }
   else
   {
      if (strlen(tcl_value) != actbytes)
      {
         sprintf(cint, "%d", actbytes);
         Tcl_AppendResult( interp, "length of string \"", tcl_value,
            "\" is does not match nbs item: should be ",cint, (char *)NULL);
         *status = TCL_ERROR;
      }
      else
      {
         nbc_put_value ( nbs_id, 0, actbytes, tcl_value, status );
      }
   }
}

/*=   TKNBS_INFO - get info on nbs item */

static void tclnbs_info
(
char *nbsitem,          /* full name of NBS item (given) */
Tcl_Interp *interp,     /* pointer to interpreter structure (given) */
int *tcl_status         /* global status (given and returned) */
)

/*   Method :
      Find noticeboard item and rest the tcl result to either a list
      of its children or its size and dimensions.
     Author :
      D.L.Terrett (RAL)
     History :
      01Mar1995: original (DLT)
*/
{
   int status;             /* status */
   int nbsid;              /* NBS id of item */
   int topid;              /* id of noticeboard */
   int mapped;             /* flag for whether noticeboard mapped */
   char nbstype[16];       /* type of item in noticeboard */
   int found;              /* flag for item found */
   int prim;               /* item is primitive */
   char cint[16];          /* buffer for formatted integer */
   int nchild;             /* number of children */
   int childid;            /* nbs id of child */
   char name[17];          /* child element name */
   char expname[33];       /* expanded child element name */
   int maxdims;
   int actdims;
   int *dims;
   int flags;
   int i;
   int maxbytes;
   int actbytes;

   status = TCL_OK;

/*   Get id of noticeboard */

   tclnbs_mapnbs ( nbsitem, &topid, &mapped, &status );

   if ( mapped == 1 )
   {

/*   Find the item within the noticeboard */

      if (strchr(nbsitem, '.') == 0)
      {
         nbsid = topid;
         found = 1;
         prim = 0;
      }
      else
      {
         tclnbs_findnbs ( nbsitem, topid, &nbsid, &prim, nbstype, &found,
           &status );
      }
      if ( found == 1 )
      {

         if (prim)
         {
            Tcl_AppendResult(interp, "yes", (char *) NULL);
         }
         else
         {
            Tcl_AppendResult(interp, "no", (char *) NULL);
         }
         if (!prim) {

/*   Get the number of children */
            nbc_get_children( nbsid, &nchild, &status);

/*   Get the names of the children */
            for (i=1; i<=nchild; i++)
            {
                nbc_find_nth_item(nbsid, i, &childid, &status);
                nbc_get_name(childid, name, &status);
                if (status == TCL_OK)
                {
                   Tcl_ScanElement(name, &flags);
                   Tcl_ConvertElement(name, expname, flags);
                   Tcl_AppendResult(interp, " ", expname, (char *) NULL);
                }
            }
         }
         else
         {

/*   Get the dimensions etc. */
            maxdims = 0;
            dims = NULL;
            nbc_get_size(nbsid, &maxbytes, &actbytes, &status);
            nbc_get_shape(nbsid, &maxdims, dims, &actdims, &status);
            if (status == TCL_OK)
            {
               if (actdims > 0)
               {
                  dims = malloc(sizeof(int)*actdims);
                  maxdims = actdims;
                  nbc_get_shape(nbsid, &maxdims, dims, &actdims, &status);
               }
            }
            if (status == TCL_OK)
            {
               if ( ( strcmp ( nbstype, "_INTEGER" ) == 0 ) ||
                    ( strcmp ( nbstype, "_LOGICAL" ) == 0 ) ||
                    ( strcmp ( nbstype, "_REAL" ) == 0 ) )
               {
                  actbytes = actbytes/4;
               }
               else if ( strcmp ( nbstype, "_DOUBLE" ) == 0 )
               {
                  actbytes = actbytes/8;
               }

               Tcl_ScanElement(nbstype, &flags);
               Tcl_ConvertElement(nbstype, expname, flags);
               sprintf(cint, " %d {", actbytes);
               Tcl_AppendResult(interp, " ", expname, cint, (char *) NULL);
               if (actdims > 0) {
                  for (i=0; i<actdims; i++)
                  {
                     sprintf(cint, " %d", dims[i]);
                     Tcl_AppendResult(interp, cint, (char *) NULL);
                  }
                  free (dims);
                }
                Tcl_AppendResult(interp, "}", (char *) NULL);
            }
         }
      }
      else
      {
         Tcl_AppendResult(interp, "failed to find NBS item \"",
            nbsitem, "\"", (char *) NULL);
         *tcl_status = TCL_ERROR;
      }
   }
   else
   {
      Tcl_AppendResult(interp, "failed to map noticeboard for \"",
            nbsitem, "\"", (char *) NULL);
      *tcl_status = TCL_ERROR;
   }
}

/*=   TKNBS_CLEAR - clear the monitor list */

static void tclnbs_clear
(
Tcl_Interp *interp
)

/*   Method :
      Empty the monitor list and free memory.
     Author :
      B.D.Kelly (ROE)
     History :
      22June1994: original (BDK)
*/

{
   mon_entry_type **ptr;        /* pointer to new entry */
   mon_entry_type *next_ptr;   /* pointer to next entry */

   ptr = &entrylist;
   for ( ; ; )
   {
      if ( *ptr == 0 )
      {
         break;
      }
      if ((*ptr)->interp == interp)
      {
         Tcl_DStringFree(&((*ptr)->tcl_name));
         next_ptr = (*ptr)->next;
         free ( *ptr );
         *ptr = next_ptr;
      }
      else
      {
         ptr = &((*ptr)->next);
      }
   }
}

/*=   TKNBS_MONITOR - add an item to the monitor list */

static void tclnbs_monitor
(
char *nbs_name,           /* name of noticeboard entry (given) */
char *tcl_name,           /* name of tcl variable (given) */
Tcl_Interp *interp,       /* interpreter structure (given and returned) */
int *tcl_status           /* tcl status (returned) */
)

/*   Method :
      Create space for new entry, put it at the front of the entry list
      and initialise it.
     Author :
      B.D.Kelly (ROE)
     History :
      22June1994: original (BDK)
*/

{
   mon_entry_type *ptr;    /* pointer to new entry */


   if ( *tcl_status != TCL_OK ) return;


/*   Get space for new entry */

   ptr = (mon_entry_type *) malloc ( sizeof ( mon_entry_type ) );
   if ( ptr != 0 )
   {
      ptr->next = entrylist;
      entrylist = ptr;
      strcpy ( ptr->nbs_name, nbs_name );
      Tcl_DStringInit(&(ptr->tcl_name));
      Tcl_DStringAppend(&(ptr->tcl_name), tcl_name, -1);
      ptr->nbs_mapped = 0;
      ptr->nbs_found = 0;
      ptr->interp = interp;
   }
   else
   {
      Tcl_AppendResult(interp,
               "monitor can't allocate space for new entry", (char *) NULL);
      *tcl_status = TCL_ERROR;
   }

}

/*=   TKNBS_MONINFO - return the monitor list */

static void tclnbs_moninfo
(
Tcl_Interp *interp,       /* interpreter structure (given and returned) */
int *tcl_status           /* tcl status (returned) */
)
{
   mon_entry_type *ptr = entrylist;    /* pointer to entry */

   for ( ; ; )
   {
      if ( ptr == 0 ) break;
      if ( ptr->nbs_name[0] == '\0' || ptr->interp != interp)
      {
         ptr = ptr->next;
         continue;
      }
      Tcl_AppendResult( interp, " {", (char *)NULL);
      Tcl_AppendElement( interp, ptr->nbs_name);
      Tcl_AppendElement( interp, Tcl_DStringValue(&(ptr->tcl_name)));
      Tcl_AppendResult( interp, "}", (char *)NULL);
      ptr = ptr->next;
   }
   *tcl_status = TCL_OK;
}

/*=   TKNBS_SCAN - copy data from NBS to tcl variables */

static void tclnbs_scan
(
ClientData clientData   /* tcl clienta data (given) */
)

/*   Method :
      Copy data from NBS, convert to ascii and put into tcl variables.
      Restart the tcl timer.
     Author :
      B.D.Kelly (ROE)
     History :
      22June1994: original (BDK)
*/

{

   timer_struct *timer = (timer_struct*)clientData;  /* timer structure */
   int status;             /* status */
   mon_entry_type **ptr;      /* pointer to new entry */
   mon_entry_type *next_ptr;  /* pointer to next entry */
   Tcl_DString tempval;    /* value got from NBS */
   int updated = 0;
   int istat;
   int prim;
   char *c;


   status = TCL_OK;
   emsMark();

   ptr = &entrylist;

   for ( ; ; )
   {
      if ( *ptr == 0 ) break;
      if ( (*ptr)->interp != timer->interp)
      {
         ptr = &((*ptr)->next);
         continue;
      }

      if ( (*ptr)->nbs_mapped == 0 )
      {

/*   Map the noticeboard if necessary. Ignore errors - these probably
     just mean the requested noticeboard hasn't been created yet */

         tclnbs_mapnbs ( (*ptr)->nbs_name, &((*ptr)->nbs_topid),
           &((*ptr)->nbs_mapped), &status );
         status = TCL_OK;
      }

      if ( (*ptr)->nbs_mapped == 1 )
      {
         if ( (*ptr)->nbs_found == 0 )
         {
            tclnbs_findnbs ( (*ptr)->nbs_name, (*ptr)->nbs_topid,
              &((*ptr)->nbs_id), &prim, (*ptr)->nbs_type, &((*ptr)->nbs_found),
              &status );
            updated = 1;
         }
         if ( (*ptr)->nbs_found == 1 )
         {

/*   See if the item has been updated since it was last read */
            if (!updated)
            {
               nbc_get_updated((*ptr)->nbs_id, &updated, &status);
            }

/*   Get the value as a character string from NBS. */

            if (updated)
            {
               Tcl_DStringInit( &tempval );
               tclnbs_getnbs ( timer->interp, (*ptr)->nbs_id, (*ptr)->nbs_type,
                  &tempval, &status );
               if (status == TCL_OK)
               {
                  c = Tcl_SetVar ( timer->interp,
                    Tcl_DStringValue(&((*ptr)->tcl_name)),
                    Tcl_DStringValue(&tempval),
                       TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG );
                  if (c == 0)
                  {
                     status = TCL_ERROR;
                  }
               }
               Tcl_DStringFree(&tempval);
            }
         }
         else
         {
            Tcl_AppendResult( timer->interp, "failed to get nbs item \"",
              (*ptr)->nbs_name, "\"", (char *) NULL);
         }

         if (status != TCL_OK)
         {
	    Tcl_AddErrorInfo( timer->interp, "\n    (during noticeboard scan)");
            Tcl_BackgroundError(timer->interp);
            Tcl_DStringFree(&((*ptr)->tcl_name));
            next_ptr = (*ptr)->next;
            free ( *ptr );
            *ptr = next_ptr;
         }
         else
         {
            ptr = &((*ptr)->next);
         }
      }
   }

   timer->timertoken = Tcl_CreateTimerHandler ( timer->milliseconds, tclnbs_scan,
     clientData );
   timer->timer_set = 1;

   emsAnnul(&istat);
   emsRlse();
}


/*=   TKNBS_START - start monitoring */

static void tclnbs_start
(
char *new_time,           /* monitor update time in millisec (given) */
Tcl_Interp *interp,       /* interpreter structure (given and returned) */
timer_struct *timer,      /* timer structure */
int *tcl_status           /* tcl status (returned) */
)

/*   Method :
      Cancel any current monitoring and resume with the new interval
     Author :
      B.D.Kelly (ROE)
     History :
      22June1994: original (BDK)
*/

{
   int msec;             /* new interval requested */
   int j;                /* loop counter */


   if ( *tcl_status != TCL_OK ) return;

   for ( j=0; j<strlen(new_time); j++ )
   {
      if ( isdigit(new_time[j]) == 0 )
      {
         *tcl_status = TCL_ERROR;
      }
   }


   if ( *tcl_status == TCL_OK )
   {
      msec = atoi ( new_time );
      if ( msec > 0 )
      {
         if ( timer->timer_set == 1 )
         {
            Tcl_DeleteTimerHandler ( timer->timertoken );
         }
         timer->milliseconds = msec;
         timer->interp = interp;
         timer->timertoken = Tcl_CreateTimerHandler ( timer->milliseconds,
           tclnbs_scan, (ClientData)timer );
         timer->timer_set = 1;
      }
      else
      {
         Tcl_AppendResult(interp,
            "invalid monitoring interval requested", (char *) NULL);
         *tcl_status = TCL_ERROR;
      }
    }
   else
   {
      Tcl_AppendResult(interp,
         "invalid monitoring interval requested", (char *) NULL);
   }

}



/*=   TKNBS_STOP - stop monitoring */

static void tclnbs_stop
(
ClientData clientData
)

/*   Method :
      Cancel any current monitoring.
     Author :
      B.D.Kelly (ROE)
     History :
      22June1994: original (BDK)
*/

{
   timer_struct *timer = (timer_struct*)clientData;      /* timer structure */

   if ( timer->timer_set == 1 )
   {
      Tcl_DeleteTimerHandler ( timer->timertoken );
   }

}
