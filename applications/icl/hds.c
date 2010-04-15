/******************************************************************************
 *
 *	H D S . C
 *
 * This module handles the interface between ICL and the Starlink Hierarchical
 * Data System (HDS).
 *
 * History:
 *   01-DEC-1993 (BKM):
 *      Created this module - moved routines from elsewhere in ICL
 *   13-OCT-1993 (AJC):
 *      Added (to adam.c) GETPAR and SETPAR
 *            GETGLOBAL, SETGLOBAL and CREATEGLOBAL
 *   02-DEC-1993 (BKM):
 *	Moved GETPAR etc. here from adam.c and tidied
 *   21-JAN-1994 (AJC):
 *      Reset node to name_interpret
 *      Use assign_helper to reload variables (copes with parameters also)
 *   14-JUN-1995 (BKM)
 *      Correct EMS usage
 *    1-FEB-1996 (AJC):
 *      Use correct status to release file after SETPAR
 *    2-FEB-1996 (AJC):
 *      Rationalize SETPAR, GETPAR, SETGLOBAL, GETGLOBAL and CREATEGLOBAL.
 *      Correct logic to handle switch between names and values etc.
 *      Create GLOBAL file on CREATEGLOBAL if necessary.
 *    5-JUN-1996 (AJC):
 *      Avoid nargs_should_be in argument checking
 *      Improve prologues etc
 *      Lengthen cval in reload_vars_hds and move }
 *   21-JUN-1999 (AJC):
 *      Change to new-style ems names emsAnnul etc.
 *      Correct type to ftype in call emsSetc
 *   22-NOV-2005 (TIMJ):
 *      Use modern HDS API with HDSLoc*
 *   15-SEP-2008 (TIMJ):
*       3 arg emsSetc is deprecated.
 ******************************************************************************
 */
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "icl.h"
#include "node.h"
#include "interp.h"
#include "symtab.h"
#include "sae_par.h"
#include "ems.h"
#include "ems_par.h"
#include "cnf.h"
#include "f77.h"
#include "star/hds.h"
#include "dat_err.h"
#include "messys_len.h"
#include "messys_par.h"
#include "messys_err.h"
#include "iclhds.h"
extern value name_interpret		  ( node *n, int op );	/* expr.c */
extern value paren_interpret              ( node *n, int op );  /* expr.c */

/******************************************************************************
 *
 * Directory in which to create HDS files (hds_path). This will either be the
 * the translation of the environment variable ADAM_USER or the default of
 * $HOME/adam and is initialised by init_hds()
 *
 * Name of the ICL HDS variable file and structure locators to the top level
 * component (hds_tloc) and the structure component (<pid>.VARIABLES)
 * which is used to pass internal ICL variable to and from ADAM tasks.
 *
 ******************************************************************************
 */
char *hds_path;
char *icl_hdscomp;
HDSLoc * hds_tloc = NULL;
HDSLoc * hds_vloc = NULL;

/******************************************************************************
 *
 * Flag set if the HDS variable file has been used
 *
 ******************************************************************************
 */
static int hds_vars_used = 0;

/******************************************************************************
 *
 *	H D S _ F L U S H
 *
 * Flush the memory resident buffers to disk - simply need to call HDS_FREE
 * to do this.
 *
 ******************************************************************************
 */
int
hds_flush(void)
{
    int status = SAI__OK;
    hdsFree( hds_tloc, &status );
    return status;
}

/******************************************************************************
 *
 *	I N I T _ H D S F I L E (void)
 *
 * Create (or connect to an existing) HDS file used to communicate ICL
 * variables between ICL and ADAM tasks.
 *
 * Author: B.K. McIlwrath 25-NOV-1993
 *
 * The following HDS component structure is created:
 *
 *	ICLVARS			- top level
 *	   <PID>        	- structure name based on the current ICL pid.
 *		VARIABLES       - Structure containing ICL variables.
 *
 * Returns the inherited status code from the HDS routines - SAI__OK if this
 * routine succeeds
 *
 ******************************************************************************
 */
static int
init_hdsfile(void)
{
    HDSLoc* tloc = NULL;
    int ndims = 0;
    hdsdim dims[1];
    int status;
    int there;

    char *file;
    char ascpid[10];
/*
 * Only obey this routine once
 */
    if (hds_vars_used)
	return SAI__OK;
    else
	hds_vars_used = 1;

    sprintf(ascpid,"%d", getpid() );
/*
 * Work out the HDS file name and the component path for ICL variables
 */
    if ( getenv("ADAM_USER") != NULL)
	icl_hdscomp = strconcat( "@$ADAM_USER", "/icl.");
    else
	icl_hdscomp = strconcat( "@$HOME", "/adam/icl.");
    icl_hdscomp = strconcat( icl_hdscomp, ascpid);
    icl_hdscomp = strconcat( icl_hdscomp, ".VARIABLES.");
    file = strconcat( hds_path, "icl");
/*
 * See if we have an existing HDS file
 */
    emsMark();

    ndims = 0;
    status = SAI__OK;
    hdsOpen(file, "UPDATE", &hds_tloc, &status);

    if (status == DAT__FILNF) {	/* Need to create a new file */
        emsAnnul(&status);
        hds_tloc = NULL;
	hdsNew(file, "ICLVARS", "ICLVARS", ndims, dims, &hds_tloc, &status );
    }
    emsRlse();
/*
 * Create the top level component from the pid of this incarnation of ICL
 */
    datThere(hds_tloc, ascpid, &there, &status );

    if( !there ) datNew( hds_tloc, ascpid, "ICLPID", ndims, dims, &status );

    datFind( hds_tloc, ascpid, &tloc, &status );

/*
 * Obtain the file locators for the "VARIABLES" component
 */
    datThere( tloc, "VARIABLES", &there, &status );

    if (!there) datNew( tloc, "VARIABLES", "VARS", ndims, dims, &status );

    datFind( tloc, "VARIABLES", &hds_vloc, &status );

/*
 * 'hds_vloc' is the globally accessible locator we will use to read and
 * write variables. Tidy up and return.
 */
    datAnnul( &tloc, &status );
    hdsFree( hds_tloc, &status );

    return status;
}

/******************************************************************************
 *
 *	D E I N I T _ H D S F I L E (void)
 *
 * Close down HDS variable file - called during ICL termination.
 *
 * Author: B.K. McIlwrath 01-DEC-1993
 *
 * Returns the inherited status code from the HDS routines - SAI__OK if this
 * routine succeeds
 *
 ******************************************************************************
 */
int
deinit_hdsfile(void)
{
    int status = SAI__OK;
    char ascpid[10];

    if (hds_vars_used) {

        datAnnul( &hds_vloc, &status );

	sprintf(ascpid,"%d", getpid() );
	datErase( hds_tloc, ascpid, &status );
	hdsClose( &hds_tloc, &status );

    }

    hdsStop( &status );

    return status;
}

/******************************************************************************
 *
 *	N A M E _ I N T E R P R E T _ H D S (node *n, int op)
 *
 ******************************************************************************
 */
value
name_interpret_hds(node *n, int op)
{
    extern value  name_interpret(node *n, int op);		/* expr.c */
    extern char	  *currentproc(void);				/* main.c */

    int ndims = 0;
    int status = SAI__OK;
    hdsdim dims[1];
    int there;

    HDSLoc *comp_loc = NULL;
    char ctype[DAT__SZTYP+1];
    char curtype[DAT__SZTYP+1];

    value val;
    char *str;

    if (op != OP_INTERPRET)
	return (name_interpret(n, op));

/*
 * Restore node to name_interpret so name_interpret nodes within
 * procedures will be counted on each invocation.
 */
    n->interpret = name_interpret;

    if( isexc(val = lookup_variable_value(string_part(n->val))) )
	return val;

    if (val.type == TYPE_STRING && val.u.string[0] == '@')
	return val;
/*
 * Initialise the HDS file used for ICL variable and setup global locators
 */
    if (!hds_vars_used)
	if( init_hdsfile() != SAI__OK )
	    return exception("HDSERR: failed to initialise ICL variable file");
/*
 * We use the global structure locator "hds_vloc" which points to a component
 * "<pid>.VARIABLES" in the HDS file to pass the ICL variable. We write
 * the variable and return a name pointer to the variable to be send to the
 * task as part of its "value string".
 */
    switch (val.type) {
      case TYPE_REAL:
	strcpy( ctype, "_DOUBLE" );
	break;
      case TYPE_INTEGER:
	strcpy( ctype, "_INTEGER" );
	break;
      case TYPE_LOGICAL:
	strcpy( ctype, "_LOGICAL" );
	break;
      case TYPE_STRING:
	strcpy( ctype, "_CHAR*256" );
    }
    str = string_part(n->val);
    str = strconcat(icl_hdscomp, str);

    datThere( hds_vloc, string_part(n->val), &there, &status );

    if (there) {
      datFind( hds_vloc, string_part(n->val), &comp_loc, &status );
      datType( comp_loc, curtype, &status );

      if (strncmp( ctype, curtype, DAT__SZTYP ) != 0 ) {
	datAnnul( &comp_loc, &status );
	datErase( hds_vloc, string_part(n->val), &status );
	there = 0;
      }

    }

    if (!there) {
      datNew( hds_vloc, string_part(n->val), ctype, ndims, dims, &status );
      datFind( hds_vloc, string_part(n->val), &comp_loc, &status );
    }


    switch (val.type) {
      case TYPE_REAL:
	datPut0D( comp_loc, real_part(val), &status );
	break;
      case TYPE_INTEGER:
	datPut0I( comp_loc, integer_part(val), &status );
	break;
      case TYPE_LOGICAL:
	datPut0L( comp_loc, logical_part(val), &status );
	break;
      case TYPE_STRING:
	datPut0C( comp_loc, string_part(val), &status );
    }
    datAnnul( &comp_loc, &status );

    return value_string(str);
}

/******************************************************************************
 *
 *	 R E L O A D _ V A R S _ H D S (void)
 *
 ******************************************************************************
 */
value
reload_vars_hds(void)
{
    extern node *node_value   (value v );			/* node.c */

    HDSLoc * comp_loc = NULL;

    float rval;
    int ival;
    int lval;

    int status = SAI__OK;
    int ncomp = 0;
    int index;
    char name[DAT__SZNAM+1];
    char type[DAT__SZTYP+1];
    char cval[257];

    value val;
    node *node;
    char ctmp[256];

    datNcomp( hds_vloc, &ncomp, &status );
    if (ncomp == 0)
	return noval;
    index = 1;
    for( ; ncomp != 0; ncomp-- ) {
        datIndex( hds_vloc, index, &comp_loc, &status );
        datName( comp_loc, name, &status );
        datType( comp_loc, type, &status );

	/*
	 * Now get the value, in the appropriate type
	 */
	if ( ( !strncmp( type, "_CHAR*", 6 ) ) ||
	     ( !strcmp( type, "ADAM_PARNAME" ) ) ) {

	    datGet0C( comp_loc, ctmp, sizeof(ctmp), &status );

	    if ( status == SAI__OK ) {
	    /* If it is a name,add the @ */
		if (!strcmp( type, "ADAM_PARNAME" )) {
		    strcpy( cval, "@" );
                } else {
		    strcpy( cval, "" );
		}
		strncat( cval, ctmp, 255 );
	        val = value_string( cval );
	    }

	} else if ( ( !strcmp( type, "_REAL" ) ) ||
		    ( !strcmp( type, "_DOUBLE" ) ) ) {

	    datGet0R( comp_loc, &rval, &status );
	    if ( status == SAI__OK )
		val = value_real( rval );

	} else if  ( !strcmp( type, "_INTEGER" ) ) {

	    datGet0I( comp_loc, &ival, &status );
	    if ( status == SAI__OK )
		val = value_integer( ival );

	} else if  ( !strcmp( type, "_LOGICAL" ) ) {
	    datGet0L( comp_loc, &lval, &status );
	    if ( status == SAI__OK )
		val = value_logical( lval );
	} else {
	    status = SAI__ERROR;
	    emsSetc( "TYPE", type );
	    emsRep(" ",
		      "HDS : Non-standard type, ^TYPE", &status);
	    break;
	}
        if( status  == SAI__OK) {
/*
 * Store the variable for use by ICL
 * assign_helper copes with variables and parameters.
 */
        if (( node = node0( name_interpret, value_string(name)) ) == NODENIL )
            return exception(
                        "SYSERR  memory exhausted in reload_vars_hds");
        else
            if (isexc( val=assign_helper1( node, val )))
               return val;
/*
 * delete the component from HDS
 */
	datAnnul( &comp_loc, &status );
	datErase( hds_vloc, name, &status );
        }
    } /* for */
/*
 * Update HDS file on disk
 */
    status = SAI__OK;
    hdsFree( hds_tloc, &status );

    return noval;
}

/*****************************************************************************
 * OPENPAR
 *
 * Purpose:
 *    Obtain a locator for a task parameter file.
 *
 * Arguments:
 *    taskname = *char (Given)
 *       The task name
 *    action = *char (Given)
 *       The action name
 *    mode = *char (Given)
 *       The mode in which the file is to be opened ( see HDS_OPEN ).
 *    loc = HDSLoc*  (Given)
 *       A locator for the top level of the file
 *    status = *int (Given and Returned)
 *       The global status
 *
 * Description:
 *    The filename is assembled by appending GLOBAL to hds_path which has
 *    been set in hds_init.
 *    If the file does not exist, and it is to be opened for writing, it is
 *    created.
 *
 * Author:
 *    AJC: A.J. Chipperfield (STARLINK)
 *
 *****************************************************************************
 */
void openpar( char *taskname, char *actname, char *mode,
              HDSLoc **floc, int *status )
{
  HDSLoc * loc = NULL;
  char *filename;
  char type[DAT__SZTYP+1];
  int index;
  int there;
  int true;

/*
 * Construct the filename
 */
    filename = strconcat(hds_path, taskname);
/*
 * Set an error reporting context
 */
    emsMark();
/*
 * Now find the parameter file
 */
    hdsOpen(filename, mode, &loc, status );
    free(filename);
    if (*status == SAI__OK)
    {
    /* Is it a monolith? Look at the first component to see if it has
     * type PROGRAM if so, assume it is a monolith
     */
	index = 1;
	datIndex( loc, index, floc, status );
	datType( *floc, type, status );
	datAnnul( floc, status );

	if (!strncmp(type, "PROGRAM", 7))
	{
        /* It is a monolith - find action component */
	    datThere( loc, actname, &there, status );
            if ( there )
	      datFind( loc, actname, floc, status );
            else
            {
                *status = SAI__ERROR;
                emsSetc( "NAME", actname );
                emsRep( " ",
                    "Failed to find parameter file component for action ^NAME",
                    status );
            }

	} else
        /*
         *  It is not a monolith - clone the file locator as action locator
         *  and make it primary
         */
         datClone( loc, floc, status );


/* Make floc a primary locator */
 	 datPrmry( 1, floc, &true, status );
/* Close the file (It will remain open till floc is annulled) */
	 datAnnul( &loc, status );
    }
/*
 * Now release the error context
 */
    emsRlse();
}

/*
 ******************************************************************************
 *
 * P R O C _ G E T P A R ( node *n )
 *
 * GETPAR taskname parametername (variable)
 *
 * Get the value of a task's parameter from the task's parameter file
 *
 * Author: A J Chipperfield
 *
 * History:
 *   13-OCT-1993 (AJC)
 *      Original version
 *   14-JUN-1995 (BKM)
 *      Correct EMS usage
 *
 ******************************************************************************
 */
static value
proc_getpar( node *n)
{
    value val;

    float rval;
    int ival;
    int lval;
    int there;

    char type[DAT__SZTYP+1];
    int status = SAI__OK;
    HDSLoc * floc = NULL;
    HDSLoc * ploc = NULL;
    HDSLoc * cloc = NULL;

    char *command, *parameter, *taskname, *actname;
    char fcval[257];
    char cval[257];
    node *nw, *variable;
/*
 * Check number of parameters
 */
    if ( nargs < 3 )
        return exception("TOOFEWPARS  Insufficient parameters for GETPAR");
    if ( nargs > 3 )
        return exception("TOOMANYPARS  Too many parameters for GETPAR");
/*
 * Get and check "command" parameter
 */
    if (isexc(val = interpret(arglist[0])))
	return val;
    if (!isstringtype(val))
	return (exception("GETPAR : First parameter must be a command name"));
    command = string_part(val);
    if (strlen(command) >= (size_t) MESSYS__TNAME)
	return exception("GETPAR : Command name too long");
/*
 * Get and check "parameter" parameter
 */
    if (isexc(val = interpret(arglist[1])))
	return val;
    if (!isstringtype(val))
	return (exception(
		"GETPAR : Second parameter must be a task parameter"));
    parameter = string_part(val);
    if (strlen(parameter) >= (size_t) MESSYS__TNAME)
	return exception("GETPAR : Parameter name too long");
/*
 * Get and check "variable" parameter
 */
    variable = arglist[2];
    if (variable->interpret != paren_interpret)
	return exception
	("GETPAR : Assignment to parameter which is not a variable");
/*
 * Find the name_interpret node
 */
    while (variable->interpret == paren_interpret)
	variable = variable->sub[0];
    if (variable->interpret != name_interpret)
	return exception
	("GETPAR : Assignment to parameter which is not a variable");
/*
 * Look up command in global symbol table
 * This gets the taskname, including any path, into taskname
 */
    if ((nw = lookup_symbol(command, SYM_DEFINED)) == NODENIL)
	return exception("GETPAR : Specified command is not defined");
    taskname = strip_path(string_part(nw->val));
    if (strlen(taskname) >= (size_t) MESSYS__TNAME)
	return exception("TASKERR  Taskname too long");
/*
 * Now get the action name
 */
    actname = string_part(nw->sub[0]->val);
    if (strlen(actname) >= (size_t) MESSYS__TNAME)
	return exception("GETPAR : Action name too long");

    status = SAI__OK;
    (void) openpar( taskname, actname, "READ",
                    &floc, &status );
	/*
         * If all OK, Find parameter component
         */
	if (status == SAI__OK)
	{
	    datThere( floc, parameter, &there, &status );
            if ( there )
	      datFind( floc, parameter, &ploc, &status );
            else
            {
                status = SAI__ERROR;
                emsSetc( "NAME", parameter );
                emsRep( " ",
		    "GETPAR : Failed to find HDS component for parameter ^name",
                    &status );
            }
	    if (status == SAI__OK)
	    {
            /* Find type of parameter component */
	      datType( ploc, type, &status );
            /*
             * Check if it is a 'name' structure
             */
	        if ( !strncmp( type, "ADAM_PARNAME", 12) )
                {
                /*
		 * It is a name - find the NAMEPTR component
		 */
		    datFind( ploc, "NAMEPTR", &cloc, &status );
	        } else
                /*
		 * It is not a 'name' structure - clone the parameter locator
                 */
		    datClone( ploc, &cloc, &status );
            /*
	     * Now get the value, in the appropriate type
             */
                if ( ( !strncmp( type, "_CHAR*", 6 ) ) ||
                     ( !strncmp( type, "ADAM_PARNAME", 12) ) )
                {
		  datGet0C( cloc, fcval, sizeof(fcval), &status );
                    if ( status == SAI__OK )
                    {
                    /* If it is a name,add the @ */
                        if (!strncmp( type, "ADAM_PARNAME", 12 ))
                        {
			  strcpy( cval, "@" );
                        } else
			  strcpy( cval, "" );

			strncat( cval, fcval, 256 );
                        val = value_string( cval );
                    }

                } else if ( ( !strncmp( type, "_REAL", 5 ) ) ||
                            ( !strncmp( type, "_DOUBLE", 7) ) )
                    {
		      datGet0R( cloc, &rval, &status );
                    if ( status == SAI__OK )
                        val = value_real( rval );
                    else ;

                } else if  ( !strncmp( type, "_INTEGER", 8) )
		  {
		    datGet0I( cloc, &ival, &status );
                    if ( status == SAI__OK )
                        val = value_integer( ival );
                    else ;

                } else if  ( !strncmp( type, "_LOGICAL", 8) )
                    {
		      datGet0L( cloc, &lval, &status );
                    if ( status == SAI__OK )
                        val = value_logical( lval );
                    else ;

                } else
                    {
                    status = SAI__ERROR;
                    emsSetc( "TYPE", type );
                    emsRep(" ",
			"GETPAR : Non-standard HDS component type, ^TYPE",
			&status);
                    }

		if (status == SAI__OK)
                    assign_helper( variable, val );
		else ;
	   /* Annul the parameter bottom component locator */
		datAnnul( &cloc, &status );
 	   /*
            * Annul the parameter component locator
            */
		datAnnul( &ploc, &status );

	    } else ;
	/*
         * Annul the action component locator - this should close the file.
         */
	    datAnnul( &floc, &status );

    } else
        emsRep( " ",
	    "GETPAR : Failed to open parameter file", &status );
/*
 * Now return appropriate status
 */
    emsRlse();

    if (status == SAI__OK)
	return trueval;
    else
	return adam_exception("ADAMERR", status);
}

/*****************************************************************************
 * OPENGLOBAL
 *
 * Purpose:
 *    Obtain a locator for the GLOBAL parameter file - creating it if
 *    necessary.
 *
 * Arguments:
 *    mode = *char (Given)
 *       The mode in which the file is to be opened ( see HDS_OPEN ).
 *    loc = *char  (Given)
 *       A locator for the top level of the file.
 *    loc_length = int (Given)
 *       Length of loc
 *    status = *int (Given and Returned)
 *       The global status
 *
 * Description:
 *    The filename is assembled by appending GLOBAL to hds_path which has
 *    been set in hds_init.
 *    If the file does not exist, and it is to be opened for writing, it is
 *    created.
 *
 * Author:
 *    AJC: A.J. Chipperfield (STARLINK)
 *
 *****************************************************************************
 */
void openglobal( char *mode, HDSLoc **loc, int *status )
{

    int ndims;
    hdsdim dims[1];
    char *filename;
/*
 * Set an error reporting context
 */
    emsMark();
/*
 * Now find the parameter file
 * First, construct filename
 */
    filename = strconcat(hds_path, "GLOBAL");
    hdsOpen( filename, mode, loc, status );

    if ( (*status == DAT__FILNF) && strcmp( mode, "READ" ) ) {
    /* Need to create a new file */
        emsAnnul(status);
	ndims = 0;
        *loc = NULL;
	hdsNew( filename, "GLOBAL", "STRUC", ndims, dims, loc, status );
    }
    free(filename);
    emsRlse();
}

/*
 *****************************************************************************
 * ENSURE_EXISTS
 *
 * Purpose:
 *    To ensure that a component of the required type exists - creating it
 *    if necessary.
 *
 * Arguments:
 *    loc = *HDSLoc  (Given)
 *       A locator for the structure in which the named component is to exist
 *    parname = *char (Given)
 *       The name of the component which must exist
 *    type = *char (Given)
 *       The required type
 *    btype = *char (Given)
 *       The type to be created if type is "?" and the component does not
 *       exist.
 *    ploc = **HDSLoc (Returned)
 *       A locator for the named component.
 *    status = *int (Given and Returned)
 *       The global status
 *
 * Description:
 *    If the named component exists and is of the right type, a locator to
 *    it is returned. If the required type is given as "?", the type of the
 *    component may be any primitive type.
 *    If the component exists, and is of the wrong type, it is deleted and
 *    a component of the right type created.
 *    If the named component does not exist, a component of the right type
 *    created. If type is specified as "?",  a component of the type given
 *    by the btype argument is created.
 *    In all cases a locator for the named component is returned in ploc.
 *
 * Author:
 *    AJC: A.J. Chipperfield (STARLINK)
 *
 *****************************************************************************
 */
void ensure_exists( HDSLoc *loc,
                    char *parameter, char *type, char *btype,
                    HDSLoc **ploc,
                    int *status )
{

HDSLoc * cloc = NULL;
int ndims;
hdsdim dims[1];
int there;
int new;
char hdstype[DAT__SZTYP+1];

        datThere( loc, parameter, &there, status );

/*  See if we need to create a new component */
	new = ( !there ? 1 : 0 );
        if ( !new ) {
/* If there is one, get a locator to it and check it's OK */
	  datFind( loc, parameter, ploc, status );
	  datType( *ploc, hdstype, status );

/* See if the existing component is OK.
 * If type is not "?" check the type against the existing component type
 */
            if ( !( type[0] == '?' ) || !strcasecmp( hdstype, "ADAM_PARNAME" ) ) {
               if ( strcasecmp( hdstype, type ) )  new = 1;

/* Else may be any primitive type (hdstype starts with "_") */
            } else if ( !(hdstype[0] == '_') ) new = 1;

/* If we need a new component, delete the old one */
            if ( new ) {
	      datAnnul( ploc, status );
	      datErase( loc, parameter, status );
            }
         }

/* Now set hdstype to the required type of the component
 * or backup type if type is "?"
 */
         type[0]=='?'?strcpy( hdstype, btype ): strcpy( hdstype, type );

/* Now create a new component of necessary */
        if ( new ) {
   /* Create the component */
           ndims = 0;
           if ( !strcasecmp(hdstype,"ADAM_PARNAME") ) {
      /* It is a name */
      /* Create a NAME parameter component */
	     datNew( loc, parameter, "ADAM_PARNAME", ndims, dims, status );

      /* and get a locator to it */
	     datFind( loc, parameter, ploc, status );

      /* create the NAMEPTR component */
	     datNew( *ploc, "NAMEPTR", "_CHAR*132", ndims, dims, status );

           } else {
   /*
    * Not a name - create a component of the required type
    */
	     datNew( loc, parameter, hdstype, ndims, dims, status );

                /* Get locator to newly created component */
	     datFind( loc, parameter, ploc, status );

           }
        }

/* If the component is an ADAM_PARNAME, set the returned locator to
 * the NAMPTR component
 */

        if ( !strcasecmp( hdstype, "ADAM_PARNAME" ) ) {
	  datFind( *ploc, "NAMEPTR", &cloc, status );

      /* Annul the intermediate locator */
	  datAnnul( ploc, status );

      /* and clone the new one and annul it */
	  datClone( cloc, ploc, status );
	  datAnnul( &cloc, status );
       }
}

/*
 ******************************************************************************
 *
 * P R O C _ S E T P A R ( node *n )
 *
 * SETPAR taskname parametername value
 *
 * Set the value of a task's parameter in the task's parameter file
 * The specifed parameter file must exist and have the specified action
 * component in it. However, if a component for the specified parameter
 * does not exist, one will be created of type _CHAR*132 unless the
 * value starts with @, in which case it is assumed to be a name and a
 * `name' component will be created.
 *
 * Author: A J Chipperfield
 *
 * History:
 *   13-OCT-1993 (AJC)
 *      Original version
 *   14-JUN-1995 (BKM)
 *      Correct EMS usage
 *
 ******************************************************************************
 */
static value
proc_setpar(node *n)
{
    value val;

    int status = SAI__OK;
    HDSLoc * floc = NULL;
    HDSLoc * ploc = NULL;

    char *command, *parameter, *taskname, *actname, *value;
    char type[DAT__SZTYP+1];
    int hname;
    node *nw;
/*
 * Check number of parameters
 */
    if ( nargs < 3 )
        return exception("TOOFEWPARS  Insufficient parameters for SETPAR");
    if ( nargs > 3 )
        return exception("TOOMANYPARS  Too many parameters for SETPAR");
/*
 * Get and check "command" parameter
 */
    if (isexc(val = interpret(arglist[0])))
	return val;
    if (!isstringtype(val))
	return (exception("SETPAR : First parameter must be a command name"));
    command = string_part(val);
    if (strlen(command) >= (size_t) MESSYS__TNAME)
	return exception("TASKERR: Command name too long");
/*
 * Get and check "parameter" parameter
 */
    if (isexc(val = interpret(arglist[1])))
	return val;
    if (!isstringtype(val))
       return (exception("SETPAR : Second parameter must be a task parameter"));
    parameter = string_part(val);
    if (strlen(parameter) >= (size_t) MESSYS__TNAME)
	return exception("TASKERR: Parameter name too long");
/*
 * Get and check "value" parameter
 */
    if (isexc(val = interpret_to_string(arglist[2])))
	return val;
    if (!isstringtype(val))
	return (exception("SETPAR : Third parameter must be a value"));
    value = string_part(val);
/*
 * Find if value is a name
 */
    hname = (value[0]=='@') ? 1: 0;
/*
 * Look up command in global symbol table
 * This gets the taskname, including any path, into taskname
 */
    if ((nw = lookup_symbol(command, SYM_DEFINED)) == NODENIL)
	return exception( "SETPAR : Specified command is not defined" );
    taskname = strip_path(string_part(nw->val));
    if (strlen(taskname) >= (size_t) MESSYS__TNAME)
	return exception("TASKERR  Taskname too long");
/*
 * Now get the action name
 */
    actname = string_part(nw->sub[0]->val);	/* the action */
    if (strlen(actname) >= (size_t) MESSYS__TNAME)
	return exception("TASKERR: Parameter name too long");


/* Now open the parameter file */
    status = SAI__OK;
    (void) openpar( taskname, actname, "UPDATE",
                    &floc, &status );

/*
 * If all OK, Find parameter component
 */
    if (status == SAI__OK) {

        hname?strcpy(type, "ADAM_PARNAME"): strcpy(type, "?");

/*   Ensure a component of the right type exists */
        (void) ensure_exists( floc, parameter,
                              type, "_CHAR*132", &ploc,
                              &status );
   /*
    * Now write the value into the appropriate component
    *  skipping the first character (@) if hname is 1
    */

   /* Write a normal value */
	datPut0C( ploc, (value + hname ), &status );

    /* Annul the parameter component locator */
	datAnnul( &ploc, &status );

    /* Annul the action component locator
     * this should close the file */
	datAnnul( &floc, &status );

    } else
        emsRep( " ",
	    "SETPAR : Failed to open parameter file", &status );

    if (status == SAI__OK)
	return trueval;
    else
	return adam_exception("ADAMERR", status);
}

/******************************************************************************
 *
 *	P R O C _ G E T G L O B A L ( node *n )
 *
 * GETGLOBAL parametername (variable)
 *
 * Get the value of a GLOBAL parameter from the GLOBAL parameter file
 *
 * Author: A J Chipperfield
 *
 * History:
 *   13-OCT-1993 (AJC)
 *      Original version
 *   14-JUN-1995 (BKM)
 *      Correct EMS usage
 *
 *********************************************************************
 */
static value
proc_getglobal( node *n)
{
    value val;

    float rval;
    int   ival;
    int   lval;

    char type[DAT__SZTYP+1];

    int there;
    int istat;
    int status = SAI__OK;
    HDSLoc * loc = NULL;
    HDSLoc * ploc = NULL;
    HDSLoc * cloc = NULL;

    char *parameter;
    char cval[257];
    char ctmp[258];
    node *variable;
/*
 * Check number of parameters
 */
    if ( nargs < 2 )
        return exception("TOOFEWPARS  Insufficient parameters for GETGLOBAL");
    if ( nargs > 2 )
        return exception("TOOMANYPARS  Too many parameters for GETGLOBAL");
/*
 * Get and check "parameter" parameter
 */
    if (isexc(val = interpret(arglist[0])))
	return val;
    if (!isstringtype(val))
	return
            (exception("GETGLOBAL : First parameter must be a parameter name"));
    parameter = string_part(val);
    if (strlen(parameter) >= (size_t) MESSYS__TNAME)
	return exception("GETGLOBAL : Parameter name too long");
/*
 * Get and check "variable" parameter
 */
    variable = arglist[1];
    if (variable->interpret != paren_interpret)	return exception
     ("GETGLOBAL : Assignment to parameter which is not a variable");
/*
 * Find the name_interpret node
 */
    while (variable->interpret == paren_interpret)
	variable = variable->sub[0];
    if (variable->interpret != name_interpret)
	return exception
	("GETGLOBAL : Assignment to parameter which is not a variable");

/* Now open the GLOBAL file */
    status = SAI__OK;
    (void) openglobal( "READ", &loc, &status );

    if (status == SAI__OK)
    {
/* If all OK, Find parameter component */
      datThere( loc, parameter, &there, &status );
        if ( there )
	  datFind( loc, parameter, &ploc, &status );
        else
        {
            status = SAI__ERROR;
            emsSetc( "NAME", parameter );
            emsRep( " ",
                "GETGLOBAL : Failed to find HDS component for parameter ^name",
                &status );
        }
	if (status == SAI__OK)
	{
        /* Find type of parameter component */
	  datType( ploc, type, &status );

        /*
         * If it is a 'name' type, find the actual name component
         */
	    if ( !strncmp( type, "ADAM_PARNAME", 12) )
            {
            /* It is a name - find the NAMEPTR component */
	      datFind( ploc, "NAMEPTR", &cloc, &status );
	    } else
                /*
                 * It is not a 'name' structure - clone the parameter locator
                 */
	      datClone(ploc, &cloc, &status );

            if ( status == SAI__OK )
            {
            /* Now get the value, in the appropriate type */
                if ( ( !strncmp( type, "_CHAR*", 6 ) ) ||
                     ( !strncmp( type, "ADAM_PARNAME", 12) ) )
                {
		  datGet0C( cloc, cval, sizeof(cval), &status );
                    if ( status == SAI__OK )
                    {
                    /* If it is a name, add the @ */
                        if ( !strncmp( type, "ADAM_PARNAME", 12) )
                        {
			  strcpy( ctmp, "@" );
                        } else
			  strcpy( ctmp, "" );
			strncat( ctmp, cval, 256 );
                        val = value_string( ctmp );
                    } else ;

                } else if ( ( !strncmp( type, "_REAL", 5) ) ||
                            ( !strncmp( type, "_DOUBLE", 7) ) )
                    {
		      datGet0R( cloc, &rval, &status );
                    if ( status == SAI__OK )
                        val = value_real( rval );
                    else ;

                } else if  ( !strncmp( type, "_INTEGER", 8) )
                    {
		      datGet0I( cloc, &ival, &status );
                    if ( status == SAI__OK )
                        val = value_integer( ival );
                    else ;

                } else if  ( !strncmp( type, "_LOGICAL", 8) )
                    {
		      datGet0L( cloc, &lval, &status );
                    if ( status == SAI__OK )
                        val = value_logical( lval );
                    else ;

                } else
                    {
                    status = SAI__ERROR;
                    emsSetc( "TYPE", type);
                    emsRep(" ",
			"GETGLOBAL : Non-standard HDS component type ^TYPE",
			&status);
                    }

		if (status == SAI__OK)
                    assign_helper( variable, val );
		else ;

	   /* Annul the parameter bottom component locator */
		datAnnul( &cloc, &status );
	    } else ;

 	/* Annul the parameter component locator */
	    datAnnul( &ploc, &status );
	} else ;

    /* close the file - ensuring good status is given */
        istat = SAI__OK;
	hdsClose( &loc, &istat );
    } else
        emsRep( " ",
	    "GETGLOBAL : Failed to open GLOBAL parameter file", &status );
/*
 * Now return appropriate status
 */
    emsRlse();

    if (status == SAI__OK)
	return trueval;
    else
	return adam_exception("ADAMERR", status);
}

/*
 ******************************************************************************
 *
 *	P R O C _ S E T G L O B A L ( node *n )
 *
 * SETGLOBAL parametername value
 *
 * Set the value of a GLOBAL parameter in the GLOBAL parameter file
 * If a component for the specified parameter does not exist, one will
 * be created of type _CHAR*132 unless the value starts with @, in
 * which case it is assumed to be a name and a `name' component will be
 * created.
 *
 * Author: A J Chipperfield
 *
 * History:
 *   13-OCT-1993 (AJC)
 *      Original version
 *   14-JUN-1995 (BKM)
 *      Correct EMS usage
 *
 ******************************************************************************
 */
static value
proc_setglobal(node *n)
{

    int istat;
    int status = SAI__OK;
    HDSLoc *loc = NULL;
    HDSLoc *ploc = NULL;
    value val;
    int hname;
    char type[DAT__SZTYP];
    char *parameter, *value;

/*
 * Check number of parameters
 */
    if ( nargs < 2 )
        return exception("TOOFEWPARS  Insufficient parameters for SETGLOBAL");
    if ( nargs > 2 )
        return exception("TOOMANYPARS  Too many parameters for SETGLOBAL");
/*
 * Get and check "parameter" parameter
 */
    if (isexc(val = interpret(arglist[0])))
	return val;
    if (!isstringtype(val))
	return
           (exception("SETGLOBAL : First parameter must be a parameter name"));
    parameter = string_part(val);
    if (strlen(parameter) >= (size_t) MESSYS__TNAME)
	return exception("TASKERR: Parameter name too long");
/*
 * Get and check "value" parameter
 */
    if (isexc(val = interpret_to_string(arglist[1])))
	return val;
    if (!isstringtype(val))
	return (exception("SETGLOBAL : Second parameter must be a value"));
    value = string_part(val);
/*
 * Find if value is a name
 */
    hname = (value[0]=='@') ? 1: 0;

/* Open GLOBAL file */
    status = SAI__OK;
    openglobal( "UPDATE", &loc, &status );

/* If all OK, Find parameter component */
    if (status == SAI__OK) {

       hname? strcpy(type, "ADAM_PARNAME"): strcpy(type, "?");

       (void) ensure_exists( loc, parameter,
                             type, "_CHAR*132", &ploc,
                             &status );
       /*
        * Now write the value into the appropriate component
        *  skipping the first character (@) if hname is 1
        */
            /* Write a normal value */
	datPut0C( ploc, (value+hname), &status );

        /* Annul the parameter component locator */
	datAnnul( &ploc, &status);

    /* close the file - ensuring good status is given */
        istat = SAI__OK;
	hdsClose( &loc, &istat);

    } else
        emsRep( " ",
	    "SETGLOBAL : Failed to open GLOBAL parameter file", &status );
/*
 * Now return appropriate status
 */
    emsRlse();

    if (status == SAI__OK)
	return trueval;
    else
	return adam_exception("ADAMERR", status);
}

/******************************************************************************
 *
 *	P R O C _ C R E A T E G L O B A L ( node *n )
 *
 * CREATEGLOBAL parametername type
 *
 * Set the value of a GLOBAL parameter in the GLOBAL parameter file.
 * If a component for the specified parameter already exists, it will
 * deleted and one of the specified type created.
 * If type is ADAM_PARNAME, a 'name' structure will be created.
 *
 * Author: A J Chipperfield
 *
 * History:
 *   13-OCT-1993 (AJC)
 *      Original version
 *   14-JUN-1995 (BKM)
 *      Correct EMS usage
 *
 ******************************************************************************
 */
static value
proc_createglobal(node *n)
{
    int istat;
    int status = SAI__OK;
    HDSLoc *loc = NULL;
    HDSLoc *ploc = NULL;
    value val;
    char *parameter, *type;

/*
 * Check number of parameters
 */
    if ( nargs < 2 )
       return exception("TOOFEWPARS  Insufficient parameters for CREATEGLOBAL");
    if ( nargs > 2 )
       return exception("TOOMANYPARS  Too many parameters for CREATEGLOBAL");
/*
 * Get and check "parameter" parameter
 */
    if (isexc(val = interpret(arglist[0])))
	return val;
    if (!isstringtype(val))
	return
          (exception(
		    "CREATEGLOBAL : First parameter must be a parameter name"));
    parameter = string_part(val);
    if (strlen(parameter) >= (size_t) MESSYS__TNAME)
	return exception("TASKERR: Parameter name too long");
/*
 * Get and check "type" parameter
 */
    if (isexc(val = interpret(arglist[1])))
	return val;
    if (!isstringtype(val))
	return
          (exception("CREATEGLOBAL : Second parameter must be a `type'"));
    type = string_part(val);
    if (strlen(type) >= (size_t) MESSYS__TNAME)
	return exception("TASKERR: `type' too long");

/* Now open the GLOBAL file, creating it if necessary */
    status = SAI__OK;
    openglobal( "UPDATE", &loc, &status );

/* If file opened OK */
    if (status == SAI__OK) {
    /*
     * Now create component of the right type
     */
        if ( !strncasecmp( type, "_CHAR*", 6 ) ||
             !strcasecmp( type, "_DOUBLE" ) ||
             !strcasecmp( type, "_INTEGER" ) ||
             !strcasecmp( type, "_REAL" ) ||
             !strcasecmp( type, "_LOGICAL" ) ||
             !strcasecmp( type, "ADAM_PARNAME" ) ) {

            (void) ensure_exists( loc, parameter,
                                  type, type, &ploc,
                                  &status );
            if ( status == SAI__OK ) {
      /* Reset the component in case it already existed */
	      datReset( ploc, &status );

      /* Annul the locator */
	      datAnnul( &ploc, &status );
            }

	} else {
            status = SAI__ERROR;
            emsSetnc( "TYPE", type, DAT__SZTYP);
            emsRep(" ",
		"CREATEGLOBAL : Illegal HDS component type, ^TYPE", &status);
        }

    /* close the file - ensuring good status is given */
        istat = SAI__OK;
	hdsClose( &loc, &istat );

    } else
        emsRep( " ",
	    "CREATEGLOBAL : Failed to open GLOBAL parameter file", &status );
/*
 * Now return appropriate status
 */
    emsRlse();

    if (status == SAI__OK)
	return trueval;
    else
	return adam_exception("ADAMERR", status);
}

/******************************************************************************
 *
 *	I N I T _ H D S (void)
 *
 * Initialise ICL's use of HDS - called during ICL initialisation.
 *
 * Author: B.K. McIlwrath 25-NOV-1993
 *
 * Returns the inherited status code from the HDS routines - SAI__OK if this
 * routine succeeds
 *
 ******************************************************************************
 */
value
init_hds(void)
{
    extern node *node_builtin(value (*fn)() );			 /* node.c   */

    value val;
/*
 * Determine the HDS directory
 */
    if ( (hds_path = getenv("ADAM_USER")) != NULL)
	hds_path = strconcat( hds_path, "/");
    else
	hds_path = strconcat( getenv("HOME"), "/adam/" );
/*
 * Start HDS - no longer required
 */

/*
 * Set up symbol table extries for calling the HDS related routines
 */
    if ((isexc(val = store_symbol(
	  "GETPAR", SYM_BUILTIN, node_builtin(proc_getpar))))	||
	(isexc(val = store_symbol(
	  "SETPAR", SYM_BUILTIN, node_builtin(proc_setpar))))	||
	(isexc(val = store_symbol(
	  "SETGLOBAL", SYM_BUILTIN, node_builtin(proc_setglobal))))	||
	(isexc(val = store_symbol(
	  "GETGLOBAL", SYM_BUILTIN, node_builtin(proc_getglobal))))	||
	(isexc(val = store_symbol(
	  "CREATEGLOBAL", SYM_BUILTIN, node_builtin(proc_createglobal)))) )
	return val;
    else
	return trueval;
}
