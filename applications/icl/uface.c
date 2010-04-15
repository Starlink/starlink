/******************************************************************************
 *
 * This file contains implementations of the UFACE procedures used by icl
 *
 * History:
 *   10-DEC-1993 (AJC):
 *      Extracted from ADAM.C
 *      Added UFACE_SPLITVAL   (previously SUBPAR_SPLITVAL)
 *      UFACE_IPUT, UFACE_OPUT, UFACE_NAMETR for use by HLP_HELP
 *    3-FEB-1994 (AJC):
 *      Correct for one-line help
 *   10-AUG-1995 (AJC):
 *      Correct UFACE_OPUT paging bug
 *   11-FEB-2000 (AJC):
 *      Un-comment out handling CONTROL context in UFACE_MSGINFO
 *        and trim only message length - not MSG_VAL_LEN
 *
 ******************************************************************************
 */
#include <stdio.h>
#include <ctype.h>
#include "sae_par.h"
#include "f77.h"
#include "cnf.h"
#include "icl.h"
#include "dtask_err.h"
#include "utils.h"
#include "ems.h"
#include "output.h"

#include "ams.h"
#include "messys_par.h"
#include "messys_err.h"
#include "messys_len.h"
#include "adam_defns.h"

static int NOCONTEXT = 0;
static char NONAME[MESSYS__TNAME] = "\0";

extern struct {
    F77_INTEGER_TYPE ufacepgsz, ufacelcnt;
    } F77_NAMED_COMMON(ufaceg06);				/* hlp common */

extern value icl_gets  (int interruptable,
			char *whofor,
			char *buffer,
			int buffersize,
			char *prompt,
			char *dflt);				/* input.c */
extern F77_SUBROUTINE(uface_pwhlp)(CHARACTER(topic),
				   CHARACTER(library),
				   INTEGER(flag),
				   INTEGER(status)
				   TRAIL(topic) TRAIL(library) );

F77_INTEGER_FUNCTION(uface_iput)( CHARACTER(string), CHARACTER(prompt),
                                  INTEGER(len) TRAIL(string) TRAIL(prompt) );

/******************************************************************************
 *
 *  U F A C E _ S E L H E L P
 *
 *  Purpose:
 *     Select the required parameter help.
 *
 *  Language:
 *     C
 *
 *  Description:
 *     Depending on the help specifiers given and the response to the
 *     prompt, the appropriate returns are made in HELPLB, STRING and
 *     LBSRCH.
 *     If one-line help is to be output, return
 *        HELPLB=' ', STRING=' '.
 *     If help library module to be output, return
 *        HELPLB=library name, STRING=key string.
 *     If an error message is to be output, return
 *        HELPLB=' ', STRING=error message.
 *     LBSRCH is intended to indicate whether or not the help system
 *     should prompt for more keys before returning to the parameter
 *     prompt. It is set TRUE if a help library module is to be output
 *     in response to '??'.
 *
 *     The response in user_val is assumed to be either ? or ??.
 *
 *  Algorithm:
 *     The help specifiers will have been extracted from the task's
 *     interface file 'help' and 'helplib'/'helpkey' fields for the
 *     parameter.
 *          '?'  => if 'help' is specified, return blank HELPLB
 *                    and STRING unless it begins with % in which case
 *                    interpret the remainder as for 'helpkey'
 *                   if there is no 'help' specifier but there is a
 *                   'helpkey' specifer, use that.
 *          '??' => if 'helpkey' is specified, use it.
 *                  if there is no 'helpkey' specifier, use 'help'
 *                  specifier, if any.
 *     If multi-line help is indicated in response to a single '?', set
 *     LBSRCH false. If it is indicated in response to '??', set LBSRCH
 *     true to indicate that a search of the library is required.
 *
 *  Authors:
 *     AJC: A J Chipperfield (STARLINK)
 *     {enter_new_authors_here}
 *
 *  History:
 *     10-DEC-1993 (AJC):
 *        Original C version - from the Fortran
 *     {enter_further_changes_here}
 *
 ******************************************************************************
 */
static void uface_selhelp
(
char *param,		/* parameter for which help required (given) */
char *user_val,		/* user response (given) */
char *hlp_txt,		/* single line help text (given) */
char *hlp_key,		/* multi line help key (given) */
char helplb[],		/* help library name (returned) */
char string[],		/* topic name or error message (returned) */
int *lbsrch,		/* set to true if library search required (returned) */
int *status		/* global status */
)
{
/*  Local Variables: */
    int liblen;
    int help;         /* Whether 'help' is specified */
    int flhelp;       /* Whether 'helpkey' is specified */
    int pcent;        /* Whether 'help' specifier begins with % */
    int single;        /* Whether to output single-line help */
    int full;          /* Whether to output multi-line help */
    char thelp[132];

/* Set flags to indicate presence or absence of specifiers */

    if ( ( strlen(hlp_txt) == 1 ) && ( hlp_txt[0] == ' ' ) ) {
	pcent = 0;
	help = 0;
    } else if( hlp_txt[0] == '%' ) {
	pcent = 1;
	help = 0;
    } else {
         pcent = 0;
         help = 1;
    }

/* Set flhelp to indicate if there is a HELPKEY field */
    if ( ( strlen(hlp_key) == 1 ) && ( hlp_key[0] == ' ' ) )
	flhelp = 0;
    else
        flhelp = 1;

/* Now decide what type of help to output.
 *      Set single true if single-line help is required: otherwise false
 *      Set full true if full help is required: otherwise false
 *      Set lbsrch = 1  if it is required to stay and search the library
 *                 = 0 if to return to the parameter prompt immediately
 */
    if ( !help && !flhelp && !pcent ) {
	helplb[0] = '\0';
	strcpy( string, "Sorry, help is not specified for parameter ");
	strcat( string, param );
	full = 0;
	single = 0;
    } else if ( user_val[1] != '?' ) {
	*lbsrch = 0;
        if ( help ) {
	    single = 1;
	    full = 0;
	} else {
	    single = 0;
	    full = 1;
	}
    } else {
	*lbsrch = 1;
	if ( flhelp ) {
	    single = 0;
            pcent = 0;
            full = 1;
         } else if ( pcent ) {
            single = 0;
            full = 1;
         } else {
/*       Full help is not available, use one-line */
            single = 1;
            full = 0;
            *lbsrch = 0;
         }
      }

/*   Now output the required type of help */
      if ( single )
      {
         helplb[0] = '\0';
         string[0] = '\0';

      } else if ( full ) {
/*      Full help required.
 *           PCENT is true if it is due to % form of single line help
 *           LBSRCH is set if library searching is required.
 */
         if ( pcent )
            strcpy(thelp, hlp_txt+1);
         else
            strcpy(thelp, hlp_key );

/* Set the library name - assuming it may be followed by keys separated
 * by spaces */
         liblen = strcspn(thelp," ");
         strncpy( helplb, thelp, liblen );
         helplb[liblen] = '\0';

         if ( liblen != strlen(thelp) )
/*         There are keys following */
            strcpy(string, thelp+liblen+1);
         else
/*         no keys */
            string[0] = '\0';
      }
}

/******************************************************************************
 *
 *  U F A C E _ O P U T
 *
 *  Purpose:
 *     For use by hlp_help to output a string
 *
 *  Language:
 *     C
 *
 *  Description:
 *     See SUN/124
 *
 *  Algorithm:
 *     The routine is called from Fortran so uses F77 and cnf to maintain
 *     portability.
 *     It handles paging with the aid of uface_iput and the external
 *     structure (Fortran COMMON block) ufaceg06
 *     Eventually it calls outstring
 *
 *  Authors:
 *     AJC: A J Chipperfield (STARLINK)
 *     {enter_new_authors_here}
 *
 *  History:
 *     10-DEC-1993 (AJC):
 *        Original
 *      8-FEB-1994 (AJC):
 *        Add paging
 *     10-AUG-1995 (AJC):
 *        Correct paging bug and avoid first line blank on page
 *     {enter_further_changes_here}
 *
 ******************************************************************************
 */
F77_INTEGER_FUNCTION(uface_oput) ( CHARACTER(string) TRAIL(string) )
{
    GENPTR_CHARACTER(string)
    DECLARE_CHARACTER(reply,  10);
    DECLARE_CHARACTER(prompt, 30);
    DECLARE_INTEGER(replen);
    char *string_c;
    int istat;

    string_c = cnf_creim( string, string_length );

    /* Check if paging required */
    istat = 1;
    if ( F77_NAMED_COMMON(ufaceg06).ufacelcnt > 0 ) {
    /* Count down UFACELCNT */
	if ( F77_NAMED_COMMON(ufaceg06).ufacelcnt-- == 2 ){
        /*    Room for 2 more lines
         *    - output the prompt. uface_iput will reset UFACELCNT to UFACEPGSZ
         *    but here we must allow for the current line being output.
         */
	    outstring("\n");
	    cnf_exprt("Press RETURN to continue...", prompt, prompt_length);
	    istat=F77_CALL(uface_iput)
		( CHARACTER_ARG(reply), CHARACTER_ARG(prompt),
		  INTEGER_ARG(&replen) TRAIL_ARG(reply)
	          TRAIL_ARG(prompt) );
   	    F77_NAMED_COMMON(ufaceg06).ufacelcnt--;
	}
    }
    /* If all OK, output the line
     * unless it is the first line on the page and blank
     */
    if ( istat == 1 ) {
       if ( ( F77_NAMED_COMMON(ufaceg06).ufacelcnt !=
              ( F77_NAMED_COMMON(ufaceg06).ufacepgsz - 1 ) )
            || *string_c ) {
 	  outstring( string_c );
          outstring("\n");
       } else {
          F77_NAMED_COMMON(ufaceg06).ufacelcnt++;
       }
    }
    cnf_free(string_c);

    return istat;
}

/******************************************************************************
 *
 *  U F A C E _ I P U T
 *
 *  Purpose:
 *     For use by hlp_help to prompt for further topics
 *
 *  Language:
 *     C
 *
 *  Description:
 *     See SUN/124
 *
 *  Algorithm:
 *     The routine is called from Fortran so uses F77 and cnf to maintain
 *     portability.
 *     Eventually it calles icl_gets
 *     If a value is returned it removes any trailing \n
 *     and returns 1.
 *     If it fails to get a value, it returns 0
 *     The routine also resets the line count, UFACELCNT, to UFACEPGSZ.
 *     Both are integers in tne external structure (Fortran COMMON block)
 *     ufaceg06 and allow a paging system.
 *
 *  Authors:
 *     AJC: A J Chipperfield (STARLINK)
 *     {enter_new_authors_here}
 *
 *  History:
 *     10-DEC-1993 (AJC):
 *        Original
 *      8-FEB-1994 (AJC):
 *        Add paging
 *     {enter_further_changes_here}
 *
 ******************************************************************************
 */
F77_INTEGER_FUNCTION(uface_iput)( CHARACTER(string), CHARACTER(prompt),
                                  INTEGER(len) TRAIL(string) TRAIL(prompt) )
{
    GENPTR_CHARACTER(string)
    GENPTR_INTEGER(len)
    GENPTR_CHARACTER(prompt)

    int got;
    int i;
    char *string_c;
    char *prompt_c;
    value val;

    got  = 0;
    string_c = cnf_creat( string_length + 2 );
    prompt_c = cnf_creib( prompt, prompt_length );

    if ( ! isexc( val =
	icl_gets( 0, "uface_iput", string_c, string_length, prompt_c,
		CHARNIL) ) )
    {
    /* Value got OK */
	got = 1;
	*len = strlen(string_c);
       /* remove the last character if it's \n */
	if( string_c[*len-1] == '\n' )
	    string_c[--*len] = '\0';
	/* export the returned string */
	cnf_exprt( string_c, string, string_length );
    }
    cnf_free(string_c);
    cnf_free(prompt_c);
    F77_NAMED_COMMON(ufaceg06).ufacelcnt =
	F77_NAMED_COMMON(ufaceg06).ufacepgsz;
    return got;
}

/******************************************************************************
 *
 *  U F A C E _ N A M E T R
 *
 *  Purpose:
 *     For use by hlp_help to translate help file names
 *
 *  Language:
 *     C
 *
 *  Description:
 *     See SUN/124
 *
 *  Algorithm:
 *     The routine is called from Fortran so uses F77 and cnf to maintain
 *     portability.
 *     The given filename is checked to see if it is a vMS form (LOGNAM:FILE)
 *     If it is, the LOGNAM: is converted to $LOGNAM/ (in upper case) and
 *     the remainder of the name, if any, is converted to lower case.
 *
 *     The resulting string sent to csh to be echoed so that environment
 *     variables and ~ are translated. If the resultant filename does not
 *     have extension .shl,  .shl is appended
 *
 *  Authors:
 *     AJC: A J Chipperfield (STARLINK)
 *     {enter_new_authors_here}
 *
 *  History:
 *     10-DEC-1993 (AJC):
 *        Original
 *     24-JAN-1994 (AJC):
 *        Make uface_nametr use expand_name
 *     17-FEB-1993 (AJC):
 *        Cope with VMS-style names LOGNAME:FILE
 *     {enter_further_changes_here}
 *
 ******************************************************************************
 */
F77_SUBROUTINE(uface_nametr)( INTEGER(cmd), CHARACTER(instr),
CHARACTER(outstr), INTEGER(jstat) TRAIL(instr) TRAIL(outstr) )
{
    GENPTR_INTEGER(cmd)
    GENPTR_CHARACTER(instr)
    GENPTR_CHARACTER(outstr)
    GENPTR_INTEGER(jstat)
    char *instr_c;
    char *eln, *inpt1, *inpt2;
    char *instr_c2;
    char *outstr_c;
    char *buffer;

    inpt1 = instr_c = cnf_creim( instr, instr_length );

/* Change any VMS-style logical names to Unix environment variables */
    if ( ( eln = strchr( instr_c, ':' ) ) != NULL ) {
/*   There is a VMS-style logical name */
       inpt2 = instr_c2 = (char *) malloc( instr_length + 2 );
       *inpt2++ = '$';
       while ( inpt1 < eln )
          *inpt2++ = toupper( *inpt1++ );
/* If there is a remaining part of the name, insert /
 * and append the name in lower case */
       if ( *++inpt1 != '\0' ) {
          *inpt2++ = '/';
          while ( inpt1 < instr_c + strlen(instr_c) )
             *inpt2++ = tolower( *inpt1++ );
       }
       *inpt2 = '\0';

       expand_name(&buffer, instr_c2 );
       free( instr_c2 );
    } else {
/*    Expand environment variable names etc. */
       expand_name(&buffer, instr_c );
    }
    free( instr_c );

/* Append .shl if necessary */
    if ( buffer == CHARNIL )
    {
	*jstat = -17;	/* Other error - SUN/124 */
	return;
    }
    if ( (int)strlen(buffer) > 4 )
	if ( strcmp( buffer + strlen(buffer) - 4, ".shl") )
	{
	    cnf_exprt( instr_c=strconcat(buffer,".shl"), outstr,
		outstr_length );
	    free( instr_c );
	} else
	    cnf_exprt( buffer, outstr, outstr_length );
	else {
	    cnf_exprt( instr_c=strconcat(buffer,".shl"), outstr,
		outstr_length );
	    free( instr_c );
	}
    *jstat = 0;
    return;
}

/******************************************************************************
 *
 *	U F A C E _ I N F O R M (int path, char *svalue, int *status)
 *
 *  Purpose:
 *     To output an information message to the user
 *
 *  Language:
 *     C
 *
 *  Description:
 *     The supplied string is terminated correctly, then output via the
 *     normal ICL output mechanism.
 *
 *  Authors:
 *     BKM: B K McIlwrath (STARLINK)
 *     {enter_new_authors_here}
 *
 *  History:
 *     10-DEC-1993 (AJC):
 *        Original removed from adam.c
 *     {enter_further_changes_here}
 *
 ******************************************************************************
 */
extern void
uface_inform(int path, char *svalue, int svlen, int *status)
{
    char infostr[MSG_VAL_LEN + 1];

    if (*status != SAI__OK)
	return;
    if (svlen != 0) {
	strcpy(infostr, "\0");
	strncat(infostr, svalue, svlen);
	strcat(infostr, "\n");
    } else
	strcpy(infostr, "\n");
    outstring(infostr);
}

/******************************************************************************
 *
 *	U F A C E _ M S G I N F O (int path, char *mname, char *mvalue,
 *                                 int mvlen, int messid,  int messtatus,
 *                                 int *status)
 *
 *  Purpose:
 *     To decode incoming messages
 *
 *  Language:
 *     C
 *
 *  Description:
 *
 *  Authors:
 *     SKR: S K Robinson (INFOMATICS)
 *     AJC: A J Chipperfield (STARLINK)
 *     {enter_new_authors_here}
 *
 *  History:
 *     10-DEC-1993 (AJC):
 *        Original removed from adam.c
 *     {enter_further_changes_here}
 *
 * ****************************************************************************
 ******************************************************************************
 */
static void uface_msginfo
(
int path,		/* path number (given) */
int message_context,	/* message context (given) */
char *message_name,	/* message name (given) */
char *message_value,    /* message value (given) */
int message_len,	/* length of value (given) */
int messid,		/* message number (given) */
int message_status,	/* message status (given) */
int *status		/* global status (given and returned) */
)
{
    char taskname[MSG_NAME_LEN];
    char context_text[30],	/* textual message context */
	 status_text[80];	/* textual message status  */

    if (*status != SAI__OK)
	return;

    ams_plookup(path, taskname, status);

    switch (message_context) {
      case GET:
	strcpy(context_text, "*GET*");
	break;
      case SET:
	strcpy(context_text, "*SET*");
	break;
      case OBEY:
	strcpy(context_text, "*OBEY*");
	break;
      case CANCEL:
	strcpy(context_text, "*CANCEL*");
	break;
      case CONTROL:
	strcpy(context_text, "*CONTROL*");
	break;
      default:
	strcpy(context_text, "*Unknown context*");
	break;
    }

    switch (message_status) {
      case SAI__OK:
	strcpy(status_text, "status SAI__OK");
	break;
      case DTASK__ACTSTART:
	strcpy(status_text, "action started");
	message_value[0] = '\0';
	break;
      case DTASK__ACTCOMPLETE:
	strcpy(status_text, "action complete");
	message_value[0] = '\0';
	break;
      case DTASK__ACTCANCEL:
	strcpy(status_text, "action cancelled");
	break;
      case DTASK__ACTINFORM:
	strcpy(status_text, "action complete - text:");
	break;
      case DTASK__ACTUNIMP:
	strcpy(status_text,
	       "error: action not implemented");
	break;
      case DTASK__REJECTED:
	strcpy(status_text,
	       "rejected: action already in progress");
	break;
      case DTASK__NOTACTIVE:
	strcpy(status_text, "rejected: no obey active");
	break;
      case DTASK__ACTNOTCANCEL:
	strcpy(status_text, "error: stage/wait/end status returned from ACT");
	break;
      default:
        strcpy(status_text, string_part(adam_exception("",
						message_status)));
	break;
    }
    bufstring(" - from ");
    bufstring(taskname);
    bufchar(' ');
    bufstring(context_text);
    bufchar(' ');
    bufstring(message_name);
    bufstring(" - ");
    bufstring(status_text);
    strtrim(message_value, message_len);
    if (message_value[0] != '\0') {
        bufnewline();
	bufstring(" - return value string = \"");
	bufstring(message_value);
	bufstring("\"");
	bufnewline();
    }
    bufnewline();
    flshbuf();

    return;
}

/******************************************************************************
 *
 *	U F A C E _ S P L I T V A L (char *valu, char param[], int *parlen,
 *			char prstr[], int *prlen,
 *			char dfault[],int * deflen,char hlptxt[], int *hlplen,
 *			char hlpkey[], int *hkylen, char errmes[], int *errlen,
 *                      int *status)
 *
 *  Purpose:
 *     To split a parameter request message into its constituent parts
 *
 *  Language:
 *     C
 *
 *  Description:
 *     The value string contains substrings separated by nulls as set up by
 *     SUBPAR_PROMPTL.  The substrings are the parameter name, its prompt,
 *     its default value, its one-line help info, and an error message if an
 *     earlier attempt to get its value failed.  This information is put into
 *     variables which can be used by uface_askparam().
 *
 *  Authors:
 *     SKR: S K Robinson (INFOMATICS)
 *     AJC: A J Chipperfield (STARLINK)
 *     {enter_new_authors_here}
 *
 *  History:
 *     10-DEC-1993 (AJC):
 *        Original moved from adam.c
 *     26-JUL-1994 (BKM):
 *        Simplified logic
 *     {enter_further_changes_here}
 *
 ******************************************************************************
 */
static void uface_splitval
(
char *message_value,	/* message value (given) */
char **param_name,	/* address of parameter name (returned) */
char **prompt_str,	/* address of prompt string (returned) */
char **default_value,	/* address of parameter default value (returned) */
char **help_txt,	/* address of single line help (returned) */
char **help_key,	/* address of key for multi-line help (returned) */
char **error_mess,	/* address of error message (returned) */
int *status		/* global status (given and returned) */
)
{
    char *p;

    if (*status != SAI__OK)
	return;

    p = message_value;
    *param_name = p;
    p += strlen(*param_name);
    *prompt_str = ++p;
    p += strlen(*prompt_str);
    *default_value = ++p;
    p += strlen(*default_value);
    *help_txt = ++p;
    p += strlen(*help_txt);
    *help_key = ++p;
    p += strlen(*help_key);
    *error_mess = ++p;

    return;
}

/******************************************************************************
 *
 *	U F A C E _ A S K P A R A M (int path, char *svalue,
 *			             int messid, int *status)
 *
 *  Purpose:
 *     To handle a parameter request message from an ADAM task.
 *
 *  Language:
 *     C
 *
 *  Description:
 *     The request message is split into its constitiuent part by a call
 *     to UFACE_SPLITVAL. The required prompt is then constructed and
 *     displayed to the user.
 *     The user's response is checked for any special characters (? etc)
 *     and those are handled if necessary.
 *     When a normal response is received, it is returned to the task.
 *
 *  Authors:
 *     SKR: S K Robinson (INFOMATICS)
 *     AJC: A J Chipperfield (STARLINK)
 *     {enter_new_authors_here}
 *
 *  History:
 *     10-DEC-1993 (AJC):
 *        Original moved from adam.c
 *     {enter_further_changes_here}
 *
 ******************************************************************************
 */
void uface_askparam
(
int path,		/* the path number (given) */
char *message_value,	/* message value (given) */
int messid,		/* message number (given) */
int *status		/* global status (given and returned) */
)
{
    char *param, *prompt, *param_dflt, *hlp_txt, *hlp_key, *errmess;
    char user_prompt[132], taskname[MSG_NAME_LEN], user_value[MSG_VAL_LEN];
    int gotval, user_vlen = 0;
    value val;

/* Help system variables */
    char helplb[132];	/* Help library name */
    char topic[132];	/* Help library topic */
    int libsrch;	/* Flag to search library */
    DECLARE_CHARACTER( topic_f, 132 );
    DECLARE_CHARACTER( helplb_f, 132 );
    DECLARE_INTEGER( jstat );
    DECLARE_INTEGER( kmd );

    if (*status != SAI__OK)
	return;

    ams_plookup(path, taskname, status); /* Get task name from path */

    uface_splitval(message_value, &param, &prompt, &param_dflt, &hlp_txt,
		   &hlp_key, &errmess,  status);
/*
 * Construct user prompt
 */
    strcpy(user_prompt, param);
    if( strlen(prompt) != 0 ) {
	strcat(user_prompt, " - ");
	strcat(user_prompt, prompt);
    }
    if( strlen(param_dflt) != 0 ) {
	strcat(user_prompt, " /");
	strcat(user_prompt, param_dflt);
	strcat(user_prompt, "/ > ");
    } else
	strcat(user_prompt, " > ");

    if( strcmp(errmess, " ") != 0) {
	bufstring("!! ");
	bufstring(errmess);
	bufnewline();
	flshbuf();
    }
    gotval = 0;
    while (!gotval) {
	*status = SAI__OK;
	if (isexc(val = icl_gets(0, "uface_askparam", user_value, MSG_VAL_LEN,
		user_prompt, param_dflt))) {
	    gotval = 1;
	    strcpy(user_value, "!!");
	    strcat(user_value, "\n");
            user_vlen = 2;
	} else if (user_value[0] == '?') {
		/* Select required help */
            uface_selhelp( param, user_value, hlp_txt, hlp_key,
                           helplb, topic, &libsrch, status );

            if( ( strlen(helplb)==0 ) && ( strlen(topic)==0 ) ) {
               bufstring(hlp_txt);
               bufnewline();
               flshbuf();

            } else {
               cnf_exprt( topic, topic_f, 132 );
               cnf_exprt( helplb, helplb_f, 132 );
               F77_CALL(uface_pwhlp)( CHARACTER_ARG(topic_f),
                                   CHARACTER_ARG(helplb_f),
                                   INTEGER_ARG(&libsrch), INTEGER_ARG(status)
                                   TRAIL_ARG(topic_f) TRAIL_ARG(helplb_f) );
            }

	} else if( user_value[0] == '\n' ) {
	    if( strlen(param_dflt) != 0 ) {
		strcpy(user_value, param_dflt);
		strcat(user_value, "\n");
		gotval = 1;
		user_vlen = strlen(user_value) - 1;
	    }
	} else {
	    gotval = 1;
	    user_vlen = strlen(user_value) - 1;	/* remove newline added by
						 * io system */
	}
    }
    ams_reply(path, messid,
	      MESSYS__MESSAGE,	/* message function */
	      MESSYS__PARAMREP, /* message status   */
	      NOCONTEXT, NONAME,
	      user_vlen, user_value,
	      status);
    return;
}

/******************************************************************************
 *
 *	U F A C E _ S T A R T O B E Y (char *taskname, char *actionname,
 *                                      char valu[], int *status)
 *  Purpose:
 *     Send OBEY with action 'actionname' and value 'value' to task 'taskname'
 *     Process parameter system interaction until the action starts (indicated
 *     by a DTASK__ACTSTART message).
 *
 *  Language:
 *     C
 *
 *  Description:
 *     The required OBEY message is sent to the task and a reply awaited.
 *
 *     On return status is SAI__OK or indicative of some error which may be
 *     qualified by string returned in value[] (no qualification will have
 *     value set to the null string).
 *
 *  Authors:
 *     BKM: B K McIlwrath (STARLINK)
 *     {enter_new_authors_here}
 *
 *  History:
 *     12-SEP-1994 (BKM):
 *        Original extracted from uface_obeyw()
 *     {enter_further_changes_here}
 *
 ******************************************************************************
 */
void uface_startobey
(
char *taskname,		/* taskname to send to (given) */
char *actionname,	/* action within task to obey (given) */
char *tvalue,		/* task value string (given and returned) */
int  value_len,		/* length of value string (given and returned) */
int *path,		/* path to task (returned) */
int *messid,		/* message system id (returned) */
int *status		/* global status (given and returned) */
)
{
    int message_context, message_status, rvalue_len;
    int finished;
    char rvalue[MSG_VAL_LEN], replyaction[MSG_NAME_LEN];

    if (*status != SAI__OK)
	return;

    ams_path(taskname, path, status);
    if (*status == MESSYS__PATHOPEN)
	*status = SAI__OK;
    message_context = OBEY;

    /* send  OBEY message to task passing input "value string" */
    ams_send(*path,
	    MESSYS__MESSAGE,	/* message function */
	    SAI__OK,		/* message status   */
	    message_context, actionname,
	    value_len-1, tvalue, messid, status);
    /* Process messages from parameter system before initial acknowledgement */
    finished = 0;
    while (!finished && *status == SAI__OK) {
	ams_getreply(20000, *path, *messid, MSG_NAME_LEN,
		     MSG_VAL_LEN, &message_status, &message_context,
		     actionname, &rvalue_len, rvalue, status);
        if (*status != SAI__OK)
	    return;

	switch (message_status) {

	  case DTASK__ACTSTART:
	    finished = 1;
	    break;

	  case MESSYS__PARAMREQ:
	    uface_askparam(*path, rvalue, *messid, status);
	    finished = (*status != SAI__OK);
	    break;

	  case MESSYS__INFORM:
	    uface_inform(*path, rvalue, rvalue_len, status);
	    break;

	  default:	/* Unexpected message status */
	    memcpy(tvalue, rvalue, MSG_VAL_LEN);
	    *status = message_status;
	    break;
	}
    }
    return;
}

/******************************************************************************
 *
 *	U F A C E _ E N D O B E Y  ( int path, int messid, char valu[],
 *                                   int *status)
 *
 *  Purpose:
 *     To wait for completion of an OBEY and handle its return status.
 *
 *  Language:
 *     C
 *
 *  Description:
 *     SEND OBEY has sent an OBEY command to the task on the end of path
 *     'path', transaction 'messid'.  We now wait for its completion.
 *     On return status is SAI__OK or indicative of some error which may be
 *     qualified by returned 'value[]' (no qualification will have value set
 *     to the null string).
 *
 *  Authors:
 *     SKR: S K Robinson (INFOMATICS)
 *     AJC: A J Chipperfield (STARLINK)
 *     {enter_new_authors_here}
 *
 *  History:
 *     10-DEC-1993 (AJC):
 *        Original moved from adam.c
 *     {enter_further_changes_here}
 *
 ******************************************************************************
 */
void uface_endobey(
int path,		/* path number (given) */
int messid,		/* message number (given) */
char *tvalue,
int *status		/* global status (given and returned) */
)
{
    int message_status, message_context, rvalue_len;
    int finished = 0;
    char rvalue[MSG_VAL_LEN],
	 replyaction[MSG_NAME_LEN];

    if (*status != SAI__OK)
	return;

    while (!finished && *status == SAI__OK) {
	ams_getreply(MESSYS__INFINITE, path, messid,
		     MSG_NAME_LEN, MSG_VAL_LEN,
		     &message_status, &message_context,
		     replyaction, &rvalue_len, rvalue, status);
	switch (message_status) {

	  case DTASK__ACTCOMPLETE:
fprintf(stderr, "Complete\n");
	    finished = 1;
	    tvalue[0] = '\0';
	    break;

	  case MESSYS__PARAMREQ:
fprintf(stderr,"Paramreq\n");
	    uface_askparam(path, rvalue, messid, status);
	    finished = (status != SAI__OK);
	    break;

	  case MESSYS__INFORM:
fprintf(stderr, "Inform\n");
	    uface_inform(path, rvalue, rvalue_len, status);
	    rvalue[0] = '\0';
	    break;

	  case MESSYS__SYNC:
	    ams_reply( path, messid,
		       MESSYS__MESSAGE,		/* message function */
		       MESSYS__SYNCREP,		/* message status   */
		       message_context, replyaction,
		       rvalue_len, rvalue, status);
	    break;

	  default:
fprintf(stderr, "default\n");
	    finished = 1;
	    memcpy(tvalue, rvalue, MSG_VAL_LEN);
            *status = message_status;
	    break;
	}
    }
fprintf(stderr, "Return\n");
    return;
}


/******************************************************************************
 *
 *	U F A C E _ O B E Y W (char *taskname, char *actionname, char valu[],
 *                             int *status)
 *  Purpose:
 *     Send OBEY with action 'actionname' and value 'value' to task 'taskname'
 *     and wait for completion.
 *
 *  Language:
 *     C
 *
 *  Description:
 *     The required OBEY message is sent to the task and a reply awaited.
 *
 *     On return status is SAI__OK or indicative of some error which may be
 *     qualified by string returned in value[] (no qualification will have
 *     value set to the null string).
 *
 *  Authors:
 *     SKR: S K Robinson (INFOMATICS)
 *     AJC: A J Chipperfield (STARLINK)
 *     {enter_new_authors_here}
 *
 *  History:
 *     10-DEC-1993 (AJC):
 *        Original moved from adam.c
 *     09-SEP-1994 (BKM):
 *        Correct logic to allow parameter system interaction and use
 *        uface_startobey()
 *     {enter_further_changes_here}
 *
 ******************************************************************************
 */
void uface_obeyw
(
char *taskname,		/* taskname to send to (given) */
char *actionname,	/* action within task to obey (given) */
char *tvalue,		/* task value string (given and returned) */
int  value_len,		/* length of value string (given and returned) */
int *status		/* global status (given and returned) */
)
{
    int path, messid, message_context, message_status, rvalue_len;
    int finished;
    char rvalue[MSG_VAL_LEN], replyaction[MSG_NAME_LEN];

    if (*status != SAI__OK)
	return;

/* Use uface_startobey to send the obey and process the task interaction up to
 * the DTASK__ACTSTART point */

    uface_startobey(taskname, actionname, tvalue, value_len,
		    &path, &messid, status);

    /* Main task OBEY processing loop */
    finished = 0;
    while (!finished && *status == SAI__OK) {
	ams_getreply(MESSYS__INFINITE, path, messid, MSG_NAME_LEN, MSG_VAL_LEN,
		     &message_status, &message_context, actionname,
		     &rvalue_len, rvalue, status);
	if (*status != SAI__OK)
	    break;
/*
 * The reply is expected to be either from the task on the transaction queue or
 * a keyboard interrupt (CNTL/C) which will generate a MESSYS__EXTINT status
 */
	switch (message_status) {

	  case DTASK__ACTCOMPLETE:
	    finished = 1;
	    tvalue[0] = '\0';
	    break;

	  case MESSYS__PARAMREQ:
	    uface_askparam(path, rvalue, messid, status);
	    finished = (*status != SAI__OK);
	    break;

	  case MESSYS__INFORM:
	    uface_inform(path, rvalue, rvalue_len, status);
	    break;

	  case MESSYS__SYNC:
	    ams_reply(path, messid, MESSYS__MESSAGE, MESSYS__SYNCREP,
		     message_context, replyaction, rvalue_len, rvalue, status);
	    break;

	  case MESSYS__EXTINT:
	  default:
	/* Interrupt or unexpected message status - report it back to caller */
	    finished = 1;
	    memcpy(tvalue, rvalue, MSG_VAL_LEN);
	    *status = message_status;
	    break;
	}
    }
    return;
}


/******************************************************************************
 *
 *      U F A C E _ G E T C O M (char * line, int *status)
 *
 *  Purpose:
 *     Waits for and processes incoming messages to ICL.
 *
 *  Language:
 *     C
 *
 *  Description:
 *     This function loops waiting for messages to arrive. These messages
 *     may be from other ADAM tasks or from the io-subsystem containing ICL
 *     command input. The status of each message received determines the
 *     action to be taken and the appropriate function is called.
 *
 *     This function will return only when an io-subsystem message is received
 *     or in the event of an error
 *
 *  Authors:
 *     SKR: S K Robinson (INFOMATICS)
 *     AJC: A J Chipperfield (STARLINK)
 *     BKM: B K McIlwrath (STARLINK)
 *     {enter_new_authors_here}
 *
 *  History:
 *     10-DEC-1993 (AJC):
 *        Original moved from adam.c
 *     26-JUL-1994 (BKM):
 *        Use ams and extensively revised
 *     {enter_further_changes_here}
 *
 ******************************************************************************
 */
void uface_getcom
(
char line[],	/* icl command (returned) */
int *status	/* global status (given and returned) */
)
{
    int path, messid, message_context, message_status, message_vlen;
    int iclcmd, istat;
    char message_name[MSG_NAME_LEN],
	 message_value[MSG_VAL_LEN],
	 *error_text;
    char *facility, *ident, *text;

    if (*status != SAI__OK)
	return;

    iclcmd = 0;
    while (!iclcmd &&
	   *status == SAI__OK) {
/*
 * Note: the ams_receive() return status is used to indicate the queue
 * the message arrived on (in addition to reporting errors)
 */
	ams_receive(MESSYS__INFINITE, MSG_NAME_LEN, MSG_VAL_LEN,
		    &message_status, &message_context, message_name,
		    &message_vlen, message_value, &path, &messid, status);
        if (*status != SAI__OK) {
	    switch(*status) {
	      case MESSYS__ASTINT:
		memcpy(line, message_value, message_vlen);
		iclcmd = 1;
	        *status = SAI__OK;
		break;

	      default:	/* Genuine error or a message on an invalid queue */
		return;
	    }
	} else {
	    istat = SAI__OK;
	    switch(message_status) {

	      case MESSYS__TRIGGER:
		memcpy(line, message_value, message_vlen);
		iclcmd = 1;
		break;

	      case MESSYS__EXTINT:
		/* Interrupt message */
	        *status = MESSYS__EXTINT;
		break;

	      case MESSYS__PARAMREQ:
		uface_askparam(path, message_value, messid, &istat);
		if (istat != SAI__OK)
		    error_text = "PARAMREQ";
		break;

	      case MESSYS__INFORM:
		uface_inform(path, message_value, message_vlen, &istat);
		if (istat != SAI__OK)
		    error_text = "INFORM";
		break;

	      case MESSYS__SYNC:
		ams_reply(path, messid,
			  MESSYS__MESSAGE,		/* message function */
			  MESSYS__SYNCREP,		/* message status   */
			  message_context, message_name,
			  message_vlen, message_value, &istat);
		if (istat != SAI__OK)
		    error_text = "SYNC";
		break;
	      default:
		uface_msginfo(path, message_context, message_name,
			      message_value, message_vlen, messid,
			      message_status, &istat);
		if (istat != SAI__OK) {
		    ems1_get_facility_error(istat, &facility, &ident, &text);
		    bufstring("%");
		    bufstring(facility);
		    bufstring("__");
		    bufstring(ident);
		    bufstring(", ");
		    bufstring(text);
		    bufnewline();
		    flshbuf();
		    istat = SAI__OK;
		}
	    }
	    if (istat != SAI__OK) {
		    bufstring("uface_getcom() - failed to process ");
		    bufstring(error_text);
		    bufstring(" message - error code ");
		    outfpint(istat);
		    bufnewline();
		    flshbuf();
	    }
	} /* *status if */
    } /* while loop */

    return;
}


/******************************************************************************
 *
 *	U F A C E _ I N T E R R U P T (void)
 *
 *  Purpose:
 *     Check for an user interface interrupt.
 *
 *  Method:
 *     The io-subsystem will send an "interrupt" message to our
 *	"external-interrupt" messys queue. We could check for the arrival of
 *	this message quite easily but it is presently ignored (see
 *	uface_getcom()). Instead we return the state of the interrupt flag
 *	set by the SIGINT handler in main()
 *
 ******************************************************************************
 */
int
uface_interrupt(void)
{
    extern int sigint_flag;	/* flag - set by cntlc_handler in main.c */

    if(sigint_flag) {
	sigint_flag = 0;
	return TRUE;
    } else
	return FALSE;
}
