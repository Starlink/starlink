#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#ifdef __cplusplus
}
#endif

/* Starlink include files */

#include <adam_defns.h>
#include <ams.h>
#include <ems_par.h>
#include <messys_par.h>
#include <messys_len.h>
#include <messys_err.h>
#include <sae_par.h>
#include <dtask_err.h>

/* Pre-allocated space */

#define FCHAR 512       /* Size of Fortran character string */
static char str1[FCHAR];
static char str2[FCHAR];


/* Routine to change nulls into newline characters */
/* Need this since the paramreq values are null separated */

adam_rmnull (char *c, int len) {
  int i;

  if (len==0) { return;}  /* Do nothing */
 
  /* Remove all spurious \0  */
  /* and replace them with a new line */  
  i = 0;
  while (i<len-1) {
   if(*(c+i) == '\0') { *(c+i) = '\n';}
   i++;
  }
  
  /* Find end of string */
  i = len + 0;
 
  while ((*(c+i-1)==' '||*(c+i-1)=='\0') && i>=0) {
    i--;
  }
 
  if (i<0) {i=0;}
 
  /* Null it */
  *(c+i) = '\0';
}


/* Autoloaded parameters */

static int
not_here(s)
char *s;
{
    croak("%s not implemented on this architecture", s);
    return -1;
}

static double
constant(name, arg)
char *name;
int arg;
{
    errno = 0;
    switch (*name) {

    case 'C':
      if (strEQ(name, "CANCEL")) return ((double)CANCEL);
      if (strEQ(name, "CONTROL")) return ((double)CONTROL);

      break;

    case 'D':
      if (strEQ(name, "DTASK__ACTSTART"))  return ((double)DTASK__ACTSTART);
      if (strEQ(name, "DTASK__ACTCOMPLETE"))  return ((double)DTASK__ACTCOMPLETE);

      break;

    case 'G':
      if (strEQ(name, "GET")) return ((double)GET);

      break;

    case 'M':
      if (strEQ(name, "MESSYS__INFINITE")) return ((double)MESSYS__INFINITE);
      if (strEQ(name, "MESSYS__PARAMREQ")) return ((double)MESSYS__PARAMREQ);
      if (strEQ(name, "MESSYS__PARAMREP")) return ((double)MESSYS__PARAMREP);
      if (strEQ(name, "MESSYS__INFORM"))   return ((double)MESSYS__INFORM);
      if (strEQ(name, "MESSYS__SYNC"))     return ((double)MESSYS__SYNC);
      if (strEQ(name, "MESSYS__SYNCREP"))  return ((double)MESSYS__SYNCREP);
      if (strEQ(name, "MESSYS__TRIGGER"))  return ((double)MESSYS__TRIGGER);
      if (strEQ(name, "MESSYS__MESSAGE"))  return ((double)MESSYS__MESSAGE);

      if (strEQ(name, "MSG_NAME_LEN"))     return ((double)MSG_NAME_LEN);
      if (strEQ(name, "MSG_VAL_LEN"))      return ((double)MSG_VAL_LEN);

      break;

    case 'O':
      if (strEQ(name, "OBEY"))  return ((double)OBEY);
      
      break;

    case 'S':
      if (strEQ(name, "SAI__OK"))    return ((double)SAI__OK);
      if (strEQ(name, "SAI__WARN"))  return ((double)SAI__WARN);
      if (strEQ(name, "SAI__ERROR")) return ((double)SAI__ERROR);

      if (strEQ(name, "SET")) return ((double)SET);

      break;

    }
    errno = EINVAL;
    return 0;

not_there:
    errno = ENOENT;
    return 0;
}


MODULE = Starlink::ADAM		PACKAGE = Starlink::ADAM		


double
constant(name,arg)
	char *		name
	int		arg


void
ams_astint(status)
	int status
 CODE:
  ams_astint(&status);
 OUTPUT:
  status

void
ams_astmsg(name, length, value, status)
	char * name  
	int length	
	char * value	
	int  status	
 CODE:
  ams_astmsg(name, length, value, &status);
 OUTPUT:
  status

void
ams_exit()

void
ams_extint(status)
	int status
 CODE:
  ams_extint(&status);
 OUTPUT:
  status


void
ams_getreply(timeout, path, messid, message_name_s, message_value_s, message_status, message_context, message_name, message_length, message_value, status)
	int timeout	
	int path	
	int messid	
	int message_name_s
	int message_value_s
	int message_status = NO_INIT
	int message_context = NO_INIT
	char * message_name = NO_INIT
	int message_length = NO_INIT
	char * message_value = NO_INIT
	int status
 CODE:
  message_name = str1;
  message_value = str2;
  ams_getreply(timeout, path, messid, message_name_s, message_value_s,
	       &message_status, &message_context, message_name, 
	       &message_length, message_value, &status);
  adam_rmnull(message_value, message_length);
 OUTPUT:
  message_status
  message_context
  message_name
  message_length
  message_value
  status


void
ams_init(own_name, status)
	char * own_name	
	int status	
 CODE:
  ams_init(own_name, &status);
 OUTPUT:
  status

#void
#ams_kick(arg0, arg1, arg2, arg3)
#	char * name	arg0
#	int length	arg1
#	char * value	arg2
#	int * status	arg3

void
ams_path(other_task_name, path, status)
	char * other_task_name
	int path     = NO_INIT
	int status	
 CODE:
  ams_path(other_task_name, &path, &status);
 OUTPUT:
  path
  status

void
ams_plookup(path, name, status)
	int path 
	char * name = NO_INIT	
	int status	
 CODE:
  name = str1;
  ams_plookup(path, name, &status);
 OUTPUT:
  name
  status

void
ams_receive(timeout, message_name_s, message_value_s, message_status, message_context, message_name, message_length, message_value, path, messid, status)
	int timeout	
	int message_name_s
	int message_value_s
	int message_status = NO_INIT
	int message_context = NO_INIT
	char * message_name = NO_INIT
	int message_length = NO_INIT
	char * message_value = NO_INIT
	int path = NO_INIT
	int messid = NO_INIT
	int status
 CODE:
  message_name = str1;
  message_value = str2;
  ams_receive(timeout, message_name_s, message_value_s, &message_status,
	      &message_context, message_name, &message_length, message_value,
	      &path, &messid, &status);
  adam_rmnull(message_value, message_length);
 OUTPUT:
  message_status
  message_context
  message_name
  message_length
  message_value
  path
  messid
  status


void
ams_reply(path, messid, message_function, message_status, message_context, message_name, message_length, message_value, status)
	int path	
	int messid	
	int message_function
	int message_status
	int message_context
	char * message_name
	int message_length
	char * message_value
	int status
 CODE:
  ams_reply(path, messid, message_function, message_status, message_context,
	    message_name, message_length, message_value, &status);
 OUTPUT:
  status

#void
#ams_resmsg(arg0, arg1, arg2)
#	int length	arg0
#	char * value	arg1
#	int * status	arg2

void
ams_send(path, message_function, message_status, message_context, message_name, message_length, message_value, messid, status)
	int path	
	int message_function
	int message_status  
	int message_context 
	char * message_name 
	int message_length  
	char * message_value
	int messid	= NO_INIT
	int status	
 CODE:
  ams_send(path, message_function, message_status, message_context, message_name, message_length, message_value, &messid, &status);
 OUTPUT:
  messid
  status
