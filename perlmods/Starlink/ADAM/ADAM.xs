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

/* Routine to change nulls into newline characters */
/* Need this since the paramreq values are null separated */

adam_rmnull (char *c, int len) {
  int i;
  if (c==NULL) return;
  if (len<=0) return;  /* Do nothing */

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

static int
constant(name)
char *name;
{
    errno = 0;
    switch (*name) {

    case 'C':
      if (strEQ(name, "CANCEL")) return ((int)CANCEL);
      if (strEQ(name, "CONTROL")) return ((int)CONTROL);

      break;

    case 'D':
      if (strEQ(name, "DTASK__ACTSTART"))  return ((int)DTASK__ACTSTART);
      if (strEQ(name, "DTASK__ACTCOMPLETE"))  return ((int)DTASK__ACTCOMPLETE);

      break;

    case 'G':
      if (strEQ(name, "GET")) return ((int)GET);

      break;

    case 'M':
      if (strEQ(name, "MESSYS__INFINITE")) return ((int)MESSYS__INFINITE);
      if (strEQ(name, "MESSYS__PARAMREQ")) return ((int)MESSYS__PARAMREQ);
      if (strEQ(name, "MESSYS__PARAMREP")) return ((int)MESSYS__PARAMREP);
      if (strEQ(name, "MESSYS__INFORM"))   return ((int)MESSYS__INFORM);
      if (strEQ(name, "MESSYS__SYNC"))     return ((int)MESSYS__SYNC);
      if (strEQ(name, "MESSYS__SYNCREP"))  return ((int)MESSYS__SYNCREP);
      if (strEQ(name, "MESSYS__TRIGGER"))  return ((int)MESSYS__TRIGGER);
      if (strEQ(name, "MESSYS__MESSAGE"))  return ((int)MESSYS__MESSAGE);

      if (strEQ(name, "MSG_NAME_LEN"))     return ((int)MSG_NAME_LEN);
      if (strEQ(name, "MSG_VAL_LEN"))      return ((int)MSG_VAL_LEN);

      break;

    case 'O':
      if (strEQ(name, "OBEY"))  return ((int)OBEY);

      break;

    case 'S':
      if (strEQ(name, "SAI__OK"))    return ((int)SAI__OK);
      if (strEQ(name, "SAI__WARN"))  return ((int)SAI__WARN);
      if (strEQ(name, "SAI__ERROR")) return ((int)SAI__ERROR);

      if (strEQ(name, "SET")) return ((int)SET);

      break;

    }
    errno = EINVAL;
    return 0;

not_there:
    errno = ENOENT;
    return 0;
}


MODULE = Starlink::ADAM		PACKAGE = Starlink::ADAM


# Supply a real function for SAI__OK since this is the routine
# used most often. This is about 30% faster than an autoloaded
# constant but 4 times slower than a perl constant (use constant)


int
pSAI__OK()
 PROTOTYPE:
 CODE:
  RETVAL = SAI__OK;
 OUTPUT:
  RETVAL


int
constant(name)
	char *		name



void
ams_astint(status)
	int status
 PROTOTYPE: $
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
 PROTOTYPE: $$$$
 CODE:
  ams_astmsg(name, length, value, &status);
 OUTPUT:
  status

void
ams_exit()

void
ams_extint(status)
	int status
 PROTOTYPE: $
 CODE:
  ams_extint(&status);
 OUTPUT:
  status


void
ams_getreply(timeout, path, messid, message_status, message_context, message_name, message_length, message_value, status)
	int timeout
	int path
	int messid
	int message_status = NO_INIT
	int message_context = NO_INIT
	char * message_name = NO_INIT
	int message_length = NO_INIT
	char * message_value = NO_INIT
	int status
 PROTOTYPE: $$$$$$$$$
 PREINIT:
  char name_len[MSG_NAME_LEN];
  char msg_len[MSG_VAL_LEN];
 CODE:
  *msg_len = '\0';
  *name_len = '\0';
  message_length = 0;
  message_name = name_len;
  message_value = msg_len;
  ams_getreply(timeout, path, messid, MSG_NAME_LEN, MSG_VAL_LEN,
	       &message_status, &message_context, message_name,
	       &message_length, message_value, &status);
  if ( status == SAI__OK ) {
    adam_rmnull(message_value, message_length);
  }
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
 PROTOTYPE: $$
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
 PROTOTYPE: $$$
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
 PROTOTYPE: $$$
 PREINIT:
  char str1[512]; /* Not sure how big to make this so over do it */
 CODE:
  name = str1;
  ams_plookup(path, name, &status);
 OUTPUT:
  name
  status

void
ams_receive(timeout, message_status, message_context, message_name, message_length, message_value, path, messid, status)
	int timeout
	int message_status = NO_INIT
	int message_context = NO_INIT
	char * message_name = NO_INIT
	int message_length = NO_INIT
	char * message_value = NO_INIT
	int path = NO_INIT
	int messid = NO_INIT
	int status
 PROTOTYPE: $$$$$$$$$
 PREINIT:
  char name_len[MSG_NAME_LEN];
  char msg_len[MSG_VAL_LEN];
 CODE:
  message_length = 0;
  *name_len = '\0';
  *msg_len = '\0';
  message_name = name_len;
  message_value = msg_len;
  ams_receive(timeout, MSG_NAME_LEN, MSG_VAL_LEN, &message_status,
	      &message_context, message_name, &message_length, message_value,
	      &path, &messid, &status);
  if (status == SAI__OK) {
    adam_rmnull(message_value, message_length);
  }
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
 PROTOTYPE: $$$$$$$$
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
 PROTOTYPE: $$$$$$$$$
 CODE:
  ams_send(path, message_function, message_status, message_context, message_name, message_length, message_value, &messid, &status);
 OUTPUT:
  messid
  status
