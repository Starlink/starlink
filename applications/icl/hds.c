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
 *
 ******************************************************************************
 */
#include <string.h>
#include "icl.h"
#include "node.h"
#include "interp.h"
#include "symtab.h"
#include "sae_par.h"
#include "ems.h"
#include "ems_par.h"
#include "cnf.h"
#include "f77.h"
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
DECLARE_CHARACTER(hds_tloc, DAT__SZLOC);
DECLARE_CHARACTER(hds_vloc, DAT__SZLOC);

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
    DECLARE_INTEGER(status);

    status = SAI__OK;
    F77_CALL(hds_free) (CHARACTER_ARG(hds_tloc), INTEGER_ARG(&status)
	TRAIL_ARG(hds_tloc) );

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
    DECLARE_CHARACTER(tloc, DAT__SZLOC);
    DECLARE_CHARACTER(fcname, DAT__SZNAM);
    DECLARE_CHARACTER(fmode, DAT__SZMOD);
    DECLARE_CHARACTER(fctype, DAT__SZTYP);
    DECLARE_CHARACTER(ffilename, 64);
    DECLARE_INTEGER(ndims);
    DECLARE_INTEGER_ARRAY(dims,1);
    DECLARE_INTEGER(status);
    DECLARE_LOGICAL(there);
  
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
    cnf_exprt(file, ffilename, ffilename_length);
    cnf_exprt("update", fmode, fmode_length);
    F77_CALL(hds_open) (CHARACTER_ARG(ffilename), CHARACTER_ARG(fmode),
	CHARACTER_ARG(hds_tloc), INTEGER_ARG(&status) TRAIL_ARG(ffilename)
	TRAIL_ARG(fmode) TRAIL_ARG(hds_tloc));

    if (status == HDS_I_FNF) {	/* Need to create a new file */
        emsAnnul(&status);
	cnf_exprt("ICLVARS", fcname, fcname_length);
	cnf_exprt("ICLVARS", fctype, fctype_length);
	F77_CALL(hds_new) (CHARACTER_ARG(ffilename), CHARACTER_ARG(fcname),
	    CHARACTER_ARG(fctype), INTEGER_ARG(&ndims), INTEGER_ARRAY_ARG(dims),
	    CHARACTER_ARG(hds_tloc), INTEGER_ARG(&status) TRAIL_ARG(ffilename)
	    TRAIL_ARG(fcname) TRAIL_ARG(fctype) TRAIL_ARG(hds_tloc) );
    }
    emsRlse();
/* 
 * Create the top level component from the pid of this incarnation of ICL 
 */
    cnf_exprt(ascpid,  fcname, fcname_length);
    cnf_exprt("ICLPID", fctype, fctype_length);
    F77_CALL(dat_there) (CHARACTER_ARG(hds_tloc), CHARACTER_ARG(fcname),
	    LOGICAL_ARG(&there), INTEGER_ARG(&status) TRAIL_ARG(hds_tloc)
	    TRAIL_ARG(fcname) );

    if( F77_ISFALSE(there) )
	F77_CALL(dat_new) (CHARACTER_ARG(hds_tloc), CHARACTER_ARG(fcname),
	    CHARACTER_ARG(fctype), INTEGER_ARG(&ndims), INTEGER_ARRAY_ARG(dims),
	    INTEGER_ARG(&status) TRAIL_ARG(hds_tloc) TRAIL_ARG(fcname)
	    TRAIL_ARG(fctype));

    F77_CALL(dat_find) (CHARACTER_ARG(hds_tloc),CHARACTER_ARG(fcname), 
	CHARACTER_ARG(tloc), INTEGER_ARG(&status) TRAIL_ARG(hds_tloc)
	TRAIL_ARG(fcname) TRAIL_ARG(tloc));
/*
 * Obtain the file locators for the "VARIABLES" component
 */ 
    cnf_exprt("VARIABLES", fcname, fcname_length);
    cnf_exprt("VARS", fctype, fctype_length);
    F77_CALL(dat_there) (CHARACTER_ARG(tloc), CHARACTER_ARG(fcname),
	    LOGICAL_ARG(&there), INTEGER_ARG(&status) TRAIL_ARG(tloc)
	    TRAIL_ARG(fcname) );
    if( F77_ISFALSE(there) )
	F77_CALL(dat_new) (CHARACTER_ARG(tloc), CHARACTER_ARG(fcname),
	    CHARACTER_ARG(fctype), INTEGER_ARG(&ndims), INTEGER_ARRAY_ARG(dims),
	    INTEGER_ARG(&status) TRAIL_ARG(tloc) TRAIL_ARG(fcname)
	    TRAIL_ARG(fctype));
    F77_CALL(dat_find) (CHARACTER_ARG(tloc),CHARACTER_ARG(fcname), 
	CHARACTER_ARG(hds_vloc), INTEGER_ARG(&status) TRAIL_ARG(tloc)
	TRAIL_ARG(fcname) TRAIL_ARG(hds_vloc));
/*
 * 'hds_vloc' is the globally accessible locator we will use to read and
 * write variables. Tidy up and return.
 */
    F77_CALL(dat_annul) (CHARACTER_ARG(tloc), INTEGER_ARG(&status)
	TRAIL_ARG(tloc) );
    F77_CALL(hds_free) (CHARACTER_ARG(hds_tloc), INTEGER_ARG(&status)
	TRAIL_ARG(hds_tloc) );

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
    DECLARE_CHARACTER(fcname, DAT__SZNAM);
    DECLARE_INTEGER(status);
    char ascpid[10];
   
    status = SAI__OK;     

    if (hds_vars_used) {
	F77_CALL(dat_annul) (CHARACTER_ARG(hds_vloc), INTEGER_ARG(&status)
	    TRAIL_ARG(hds_vloc) );

	sprintf(ascpid,"%d", getpid() );
        cnf_exprt(ascpid, fcname, fcname_length);
	F77_CALL(dat_erase) (CHARACTER_ARG(hds_tloc), CHARACTER_ARG(fcname),
	    INTEGER_ARG(&status) TRAIL_ARG(hds_tloc) TRAIL_ARG(fcname) );

	F77_CALL(hds_close) (CHARACTER_ARG(hds_tloc), INTEGER_ARG(&status)
	    TRAIL_ARG(hds_tloc) );
    }
    F77_CALL(hds_stop) (INTEGER_ARG(&status) );

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

    DECLARE_CHARACTER(comp_loc, DAT__SZLOC);
    DECLARE_CHARACTER(fcname,   DAT__SZNAM);
    DECLARE_CHARACTER(fctype,   DAT__SZTYP);
    DECLARE_CHARACTER(curtype,  DAT__SZTYP);
    DECLARE_INTEGER(ndims);
    DECLARE_INTEGER_ARRAY(dims,1);
    DECLARE_INTEGER(status);
    DECLARE_LOGICAL(there);
    DECLARE_CHARACTER(fcval, 256);
    DECLARE_DOUBLE(fdval);
    DECLARE_LOGICAL(flval);
    DECLARE_INTEGER(fival);

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
	cnf_exprt("_DOUBLE",   fctype, DAT__SZTYP);
	break;
      case TYPE_INTEGER:
	cnf_exprt("_INTEGER",  fctype, DAT__SZTYP);
	break;
      case TYPE_LOGICAL:
	cnf_exprt("_LOGICAL",  fctype, DAT__SZTYP);
	break;
      case TYPE_STRING:        
	cnf_exprt("_CHAR*256", fctype, DAT__SZTYP);
    }
    cnf_exprt( (str = string_part(n->val)), fcname, DAT__SZNAM);
    str = strconcat(icl_hdscomp, str);
    status = 0;	/* SAI__OK */
    ndims =  0;

    F77_CALL(dat_there) (CHARACTER_ARG(hds_vloc), CHARACTER_ARG(fcname),
	LOGICAL_ARG(&there), INTEGER_ARG(&status) TRAIL_ARG(hds_vloc)
	TRAIL_ARG(fcname) );

    if ( F77_ISTRUE(there) ) {
	F77_CALL(dat_find) (CHARACTER_ARG(hds_vloc), CHARACTER_ARG(fcname),
	    CHARACTER_ARG(comp_loc), INTEGER_ARG(&status) TRAIL_ARG(hds_vloc)
	    TRAIL_ARG(fcname) TRAIL_ARG(comp_loc) );
	F77_CALL(dat_type)  (CHARACTER_ARG(comp_loc), CHARACTER_ARG(curtype),
	    INTEGER_ARG(&status) TRAIL_ARG(comp_loc) TRAIL_ARG(curtype));
        if ( memcmp(fctype, curtype, DAT__SZTYP) != 0) {
	    F77_CALL(dat_annul) (CHARACTER_ARG(comp_loc), INTEGER_ARG(&status)
		TRAIL_ARG(comp_loc) );
	    F77_CALL(dat_erase) (CHARACTER_ARG(hds_vloc),
		CHARACTER_ARG(fcname), INTEGER_ARG(&status)
		TRAIL_ARG(hds_vloc) TRAIL_ARG(fcname) );
	    there = F77_FALSE;
	}
    }
    if ( F77_ISFALSE(there) ) {
	F77_CALL(dat_new) (CHARACTER_ARG(hds_vloc), CHARACTER_ARG(fcname),
	    CHARACTER_ARG(fctype), INTEGER_ARG(&ndims),
	    INTEGER_ARRAY_ARG(dims), INTEGER_ARG(&status) TRAIL_ARG(hds_vloc)
	    TRAIL_ARG(fcname) TRAIL_ARG(fctype));
	F77_CALL(dat_find) (CHARACTER_ARG(hds_vloc), CHARACTER_ARG(fcname),
	    CHARACTER_ARG(comp_loc), INTEGER_ARG(&status) TRAIL_ARG(hds_vloc)
	    TRAIL_ARG(fcname) TRAIL_ARG(comp_loc) );
    }

    switch (val.type) {
      case TYPE_REAL:
	fdval = real_part(val);
	F77_CALL(dat_put0d) (CHARACTER_ARG(comp_loc), DOUBLE_ARG(&fdval),
	    INTEGER_ARG(&status) TRAIL_ARG(comp_loc) );
	break;
      case TYPE_INTEGER:
	fival = integer_part(val);
	F77_CALL(dat_put0i) (CHARACTER_ARG(comp_loc), INTEGER_ARG(&fival),
	    INTEGER_ARG(&status) TRAIL_ARG(comp_loc) );
	break;
      case TYPE_LOGICAL:
	flval = (logical_part(val)? F77_TRUE : F77_FALSE);
	F77_CALL(dat_put0l) (CHARACTER_ARG(comp_loc), LOGICAL_ARG(&flval),
	    INTEGER_ARG(&status) TRAIL_ARG(comp_loc) );
	break;
      case TYPE_STRING:        
	cnf_exprt(string_part(val), fcval, fcval_length);
	F77_CALL(dat_put0c) (CHARACTER_ARG(comp_loc), CHARACTER_ARG(fcval),
	    INTEGER_ARG(&status) TRAIL_ARG(comp_loc) TRAIL_ARG(fcval) );
    }
    F77_CALL(dat_annul) (CHARACTER_ARG(comp_loc), INTEGER_ARG(&status)
	TRAIL_ARG(comp_loc) );

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

    DECLARE_CHARACTER(comp_loc, DAT__SZLOC);
    DECLARE_CHARACTER(fcname,   DAT__SZNAM);
    DECLARE_CHARACTER(fctype,   DAT__SZTYP);
    DECLARE_INTEGER(index);
    DECLARE_INTEGER(ncomp);
    DECLARE_INTEGER(status);
    DECLARE_REAL(rval);
    DECLARE_INTEGER(ival);
    DECLARE_LOGICAL(lval);
    DECLARE_LOGICAL(there);
    DECLARE_CHARACTER(fcval, 256);

    value val, val1;
    node *node;
    char cval[257], name[DAT__SZNAM + 1], type[DAT__SZTYP + 1];
    int cvind;

    status = SAI__OK;
    F77_CALL(dat_ncomp) (CHARACTER_ARG(hds_vloc), INTEGER_ARG(&ncomp),
	INTEGER_ARG(&status) TRAIL_ARG(hds_vloc) );
    if (ncomp == 0)
	return noval;
    index = 1;
    for( ; ncomp != 0; ncomp-- ) {
	F77_CALL(dat_index) (CHARACTER_ARG(hds_vloc), INTEGER_ARG(&index),
	    CHARACTER_ARG(comp_loc), INTEGER_ARG(&status) TRAIL_ARG(hds_vloc)
	    TRAIL_ARG(comp_loc) );
	F77_CALL(dat_name) (CHARACTER_ARG(comp_loc), CHARACTER_ARG(fcname),
	    INTEGER_ARG(&status) TRAIL_ARG(comp_loc) TRAIL_ARG(fcname) );
	F77_CALL(dat_type) (CHARACTER_ARG(comp_loc), CHARACTER_ARG(fctype),
	    INTEGER_ARG(&status) TRAIL_ARG(comp_loc) TRAIL_ARG(fctype) );
	/*
	 * Now get the value, in the appropriate type
	 */
	cnf_impn(fctype, fctype_length, fctype_length, type);
	if ( ( !strncmp( type, "_CHAR*", 6 ) ) ||
	     ( !strcmp( type, "ADAM_PARNAME" ) ) ) {

	    F77_CALL(dat_get0c) (CHARACTER_ARG(comp_loc),
		CHARACTER_ARG(fcval), INTEGER_ARG(&status)
		TRAIL_ARG(comp_loc) TRAIL_ARG(fcval));
	    if ( status == SAI__OK ) {
	    /* If it is a name,add the @ */
		if (!strcmp( type, "ADAM_PARNAME" )) {
		    cval[0] = '@';
		    cvind = 1;
		} else {
		    cvind = 0;                            
                }
	        cnf_impn(fcval, fcval_length, fcval_length, cval + cvind );
	        val = value_string( cval );
	    }

	} else if ( ( !strcmp( type, "_REAL" ) ) || 
		    ( !strcmp( type, "_DOUBLE" ) ) ) {
	    F77_CALL(dat_get0r) (CHARACTER_ARG(comp_loc), REAL_ARG(&rval),
		INTEGER_ARG(&status) TRAIL_ARG(comp_loc) );
	    if ( status == SAI__OK )
		val = value_real( rval );                    

	} else if  ( !strcmp( type, "_INTEGER" ) ) {
 	    F77_CALL(dat_get0i) (CHARACTER_ARG(comp_loc),
		INTEGER_ARG(&ival), INTEGER_ARG(&status)
		TRAIL_ARG(comp_loc) );
	    if ( status == SAI__OK )
		val = value_integer( ival );                    

	} else if  ( !strcmp( type, "_LOGICAL" ) ) {
	    F77_CALL(dat_get0l) (CHARACTER_ARG(comp_loc),
		LOGICAL_ARG(&lval), LOGICAL_ARG(&status)
		TRAIL_ARG(comp_loc) );
	    if ( status == SAI__OK )
		val = value_logical( lval );                    
	} else {
	    status = SAI__ERROR;
	    emsSetc( "TYPE", fctype, fctype_length);
	    emsRep(" ",
		      "HDS : Non-standard type, ^TYPE", &status);
	    break;
	}
        if( status  == SAI__OK) {
/*
 * Store the variable for use by ICL
 * assign_helper copes with variables and parameters.
 */
	cnf_impn(fcname, fcname_length, fcname_length, name);	
        if (( node = node0( name_interpret, value_string(name)) ) == NODENIL )
            return exception(
                        "SYSERR  memory exhausted in reload_vars_hds");
        else
            if (isexc( val=assign_helper1( node, val )))
               return val;
/* 
 * delete the component from HDS
 */
	F77_CALL(dat_annul) (CHARACTER_ARG(comp_loc), INTEGER_ARG(&status)
	    TRAIL_ARG(comp_loc) );
	F77_CALL(dat_erase) (CHARACTER_ARG(hds_vloc), CHARACTER_ARG(fcname),
	    INTEGER_ARG(&status) TRAIL_ARG(hds_vloc) TRAIL_ARG(fcname) );
        }
    } /* for */
/*
 * Update HDS file on disk
 */
    status = SAI__OK;
    F77_CALL(hds_free) (CHARACTER_ARG(hds_tloc), INTEGER_ARG(&status)
	TRAIL_ARG(hds_tloc) );

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
 *    loc = *char  (Given)
 *       A locator for the top level of the file
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
void openpar( char *taskname, char *actname, char *mode, 
              char *floc, int floc_length, int *status )
{
DECLARE_CHARACTER(loc, DAT__SZLOC);
DECLARE_CHARACTER(factname, MSG_NAME_LEN);
DECLARE_CHARACTER(ffilename, 200);
DECLARE_CHARACTER(fmode, DAT__SZMOD);
DECLARE_CHARACTER(ftype, DAT__SZTYP);
DECLARE_INTEGER(index);
DECLARE_LOGICAL(there);
DECLARE_LOGICAL(true);
char *filename;
char type[DAT__SZTYP];
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
    cnf_exprt(mode, fmode, fmode_length);
    cnf_exprt(filename, ffilename, ffilename_length);
    F77_CALL(hds_open) (CHARACTER_ARG(ffilename), CHARACTER_ARG(fmode),
	CHARACTER_ARG(loc), INTEGER_ARG(status) TRAIL_ARG(ffilename)
	TRAIL_ARG(fmode) TRAIL_ARG(loc));
    if (*status == SAI__OK)
    {
    /* Is it a monolith? Look at the first component to see if it has
     * type PROGRAM if so, assume it is a monolith
     */
	index = 1;
        F77_CALL(dat_index) (CHARACTER_ARG(loc), INTEGER_ARG(&index),
	    CHARACTER_ARG(floc), INTEGER_ARG(status) TRAIL_ARG(loc)
	    TRAIL_ARG(floc));
	F77_CALL(dat_type)  (CHARACTER_ARG(floc), CHARACTER_ARG(ftype),
	    INTEGER_ARG(status) TRAIL_ARG(floc) TRAIL_ARG(ftype));
	F77_CALL(dat_annul) (CHARACTER_ARG(floc), INTEGER_ARG(status)
	    TRAIL_ARG(floc));
        cnf_imprt(ftype, ftype_length, type );
	if (!strncmp(type, "PROGRAM", 7))
	{
        /* It is a monolith - find action component */
            cnf_exprt(actname, factname, factname_length);
	    F77_CALL(dat_there) (CHARACTER_ARG(loc), CHARACTER_ARG(factname), 
		LOGICAL_ARG(&there), INTEGER_ARG(status) TRAIL_ARG(loc)
		TRAIL_ARG(factname) );
            if ( there )
	       F77_CALL(dat_find) (CHARACTER_ARG(loc), CHARACTER_ARG(factname), 
		    CHARACTER_ARG(floc), INTEGER_ARG(status) 
		    TRAIL_ARG(loc) TRAIL_ARG(factname) TRAIL_ARG(floc));
            else
            {
                *status = SAI__ERROR;
                emsSetc( "NAME", actname, factname_length );
                emsRep( " ",
                    "Failed to find parameter file component for action ^NAME", 
                    status );
            }

	} else
        /*
         *  It is not a monolith - clone the file locator as action locator
         *  and make it primary
         */
	    F77_CALL(dat_clone) (CHARACTER_ARG(loc), CHARACTER_ARG(floc),
		INTEGER_ARG(status) TRAIL_ARG(loc) TRAIL_ARG(floc));
            
/* Make floc a primary locator */
         true = F77_TRUE;
         F77_CALL(dat_prmry)(LOGICAL_ARG(&true), CHARACTER_ARG(floc),
                                LOGICAL_ARG(&true), INTEGER_ARG(status)
                                TRAIL_ARG(floc) );
/* Close the file (It will remain open till floc is annulled) */
         F77_CALL(dat_annul)(CHARACTER_ARG(loc), INTEGER_ARG(status)
                             TRAIL_ARG(loc) );
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
    int cvind;

    DECLARE_REAL(rval);
    DECLARE_INTEGER(ival);
    DECLARE_LOGICAL(lval);
    DECLARE_LOGICAL(there);
    DECLARE_CHARACTER(fparameter, MSG_NAME_LEN);
    DECLARE_CHARACTER(fcommand, MSG_NAME_LEN);
    DECLARE_CHARACTER(factname, MSG_NAME_LEN);
    DECLARE_CHARACTER(fcname, DAT__SZNAM);
    DECLARE_CHARACTER(ffilename, 200);
    DECLARE_CHARACTER(fcval, 256);
    DECLARE_CHARACTER(fmode, DAT__SZMOD);
    DECLARE_CHARACTER(ftype, DAT__SZTYP);
    DECLARE_INTEGER(index);
    DECLARE_INTEGER(status);
    DECLARE_INTEGER(istat);
    DECLARE_CHARACTER(loc, DAT__SZLOC);
    DECLARE_CHARACTER(floc, DAT__SZLOC);
    DECLARE_CHARACTER(ploc, DAT__SZLOC);
    DECLARE_CHARACTER(cloc, DAT__SZLOC);
    char *filename, *command, *parameter, *varname, *taskname, *actname;
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
                    floc, floc_length, &status );
	/* 
         * If all OK, Find parameter component
         */
	if (status == SAI__OK)
	{
	    cnf_exprt(parameter, fparameter, fparameter_length);
	    F77_CALL(dat_there) (CHARACTER_ARG(floc), CHARACTER_ARG(fparameter), 
		LOGICAL_ARG(&there), INTEGER_ARG(&status) TRAIL_ARG(floc) 
		TRAIL_ARG(fparameter) );
            if ( there )
	        F77_CALL(dat_find) (CHARACTER_ARG(floc),
		    CHARACTER_ARG(fparameter), CHARACTER_ARG(ploc),
		    INTEGER_ARG(&status) TRAIL_ARG(floc) TRAIL_ARG(fparameter)
		    TRAIL_ARG(ploc));
            else
            {
                status = SAI__ERROR;
                emsSetc( "NAME", parameter, fparameter_length );
                emsRep( " ",
		    "GETPAR : Failed to find HDS component for parameter ^name",
                    &status );
            }
	    if (status == SAI__OK)
	    {
            /* Find type of parameter component */
	        F77_CALL(dat_type) (CHARACTER_ARG(ploc), CHARACTER_ARG(ftype),
		    INTEGER_ARG(&status) TRAIL_ARG(ploc) TRAIL_ARG(ftype));
            /* 
             * Check if it is a 'name' structure
             */
	        if ( !strncmp( ftype, "ADAM_PARNAME", 12) )
                {
                /*
		 * It is a name - find the NAMEPTR component
		 */
		    cnf_exprt("NAMEPTR", fcname, fcname_length);
	            F77_CALL(dat_find) (CHARACTER_ARG(ploc),
			CHARACTER_ARG(fcname), CHARACTER_ARG(cloc),
			INTEGER_ARG(&status) TRAIL_ARG(ploc) TRAIL_ARG(fcname)
			TRAIL_ARG(cloc));
	        } else
                /*
		 * It is not a 'name' structure - clone the parameter locator
                 */
	            F77_CALL(dat_clone) (CHARACTER_ARG(ploc),
			CHARACTER_ARG(cloc), INTEGER_ARG(&status)
			TRAIL_ARG(ploc) TRAIL_ARG(cloc));
            /*
	     * Now get the value, in the appropriate type
             */
                if ( ( !strncmp( ftype, "_CHAR*", 6 ) ) ||
                     ( !strncmp( ftype, "ADAM_PARNAME", 12) ) )
                {
		    F77_CALL(dat_get0c) (CHARACTER_ARG(cloc),
			CHARACTER_ARG(fcval), INTEGER_ARG(&status)
			TRAIL_ARG(cloc) TRAIL_ARG(fcval));
                    if ( status == SAI__OK )                    
                    {
                    /* If it is a name,add the @ */
                        if (!strncmp( ftype, "ADAM_PARNAME", 12 ))
                        {
                            cval[0] = '@';
                            cvind = 1;
                        } else
                            cvind = 0;                            
		        cnf_impn(fcval, fcval_length, fcval_length,
                                 cval + cvind );
                        val = value_string( cval );
                    }

                } else if ( ( !strncmp( ftype, "_REAL", 5 ) ) || 
                            ( !strncmp( ftype, "_DOUBLE", 7) ) )
                    {
                    F77_CALL(dat_get0r) (CHARACTER_ARG(cloc), REAL_ARG(&rval),
			INTEGER_ARG(&status) TRAIL_ARG(cloc) );
                    if ( status == SAI__OK )
                        val = value_real( rval );                    
                    else ;

                } else if  ( !strncmp( ftype, "_INTEGER", 8) )
                    {
                    F77_CALL(dat_get0i) (CHARACTER_ARG(cloc),
			INTEGER_ARG(&ival), INTEGER_ARG(&status)
			TRAIL_ARG(cloc) );
                    if ( status == SAI__OK )
                        val = value_integer( ival );                    
                    else ;

                } else if  ( !strncmp( ftype, "_LOGICAL", 8) )
                    {
                    F77_CALL(dat_get0l) (CHARACTER_ARG(cloc),
			LOGICAL_ARG(&lval), LOGICAL_ARG(&status)
			TRAIL_ARG(cloc) );
                    if ( status == SAI__OK )
                        val = value_logical( lval );                    
                    else ;

                } else
                    {
                    status = SAI__ERROR;
                    emsSetc( "TYPE", ftype, ftype_length);
                    emsRep(" ",
			"GETPAR : Non-standard HDS component type, ^TYPE",
			&status);
                    }

		if (status == SAI__OK)
                    assign_helper( variable, val );
		else ;
	   /* Annul the parameter bottom component locator */
		F77_CALL(dat_annul) (CHARACTER_ARG(cloc), INTEGER_ARG(&status)
		    TRAIL_ARG(cloc));
 	   /*
            * Annul the parameter component locator
            */
  		F77_CALL(dat_annul) (CHARACTER_ARG(ploc), INTEGER_ARG(&status)
		    TRAIL_ARG(ploc));

	    } else ;
	/*
         * Annul the action component locator - this should close the file.
         */
	    F77_CALL(dat_annul) (CHARACTER_ARG(floc), INTEGER_ARG(&status)
		TRAIL_ARG(floc));

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
void openglobal( char *mode, char *loc, int loc_length, int *status )
{
    DECLARE_CHARACTER(ffilename, 200);
    DECLARE_CHARACTER(fcname, 20);
    DECLARE_CHARACTER(fmode, DAT__SZMOD);
    DECLARE_CHARACTER(ftype, DAT__SZTYP);
    DECLARE_INTEGER(ndims);
    DECLARE_INTEGER_ARRAY(dims,1);
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
    cnf_exprt(filename, ffilename, ffilename_length);
    free(filename);
    cnf_exprt(mode, fmode, fmode_length);
    F77_CALL(hds_open) (CHARACTER_ARG(ffilename), CHARACTER_ARG(fmode),
	CHARACTER_ARG(loc), INTEGER_ARG(status) TRAIL_ARG(ffilename)
	TRAIL_ARG(fmode) TRAIL_ARG(loc));

    if ( (*status == HDS_I_FNF) && strcmp( mode, "READ" ) ) {
    /* Need to create a new file */
        emsAnnul(status);
	ndims = 0;
	cnf_exprt("GLOBAL", fcname, fcname_length);
	cnf_exprt("STRUC" , ftype,  ftype_length);
	F77_CALL(hds_new) (CHARACTER_ARG(ffilename), CHARACTER_ARG(fcname),
	    CHARACTER_ARG(ftype), INTEGER_ARG(&ndims), INTEGER_ARRAY_ARG(dims),
	    CHARACTER_ARG(loc), INTEGER_ARG(status) TRAIL_ARG(ffilename)
	    TRAIL_ARG(fcname) TRAIL_ARG(ftype) TRAIL_ARG(loc) );
    }
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
 *    loc = *char  (Given)
 *       A locator for the structure in which the named component is to exist
 *    loc_length = int (Given)
 *       Length of loc
 *    parname = *char (Given)
 *       The name of the component which must exist
 *    type = *char (Given)
 *       The required type 
 *    btype = *char (Given)
 *       The type to be created if type is "?" and the component does not
 *       exist.
 *    ploc = *char (Returned)
 *       A locator for the named component.
 *    ploc_length = int (Given)
 *       Length of ploc
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
void ensure_exists( char *loc, int loc_length,
                    char *parameter, char *type, char *btype,
                    char *ploc, int ploc_length, 
                    int *status )
{
DECLARE_CHARACTER(fparameter, MSG_NAME_LEN);
DECLARE_CHARACTER(fcname, DAT__SZNAM);
DECLARE_CHARACTER(cloc, DAT__SZLOC);
DECLARE_CHARACTER(fhdstype, DAT__SZTYP);
DECLARE_LOGICAL(there);
DECLARE_INTEGER(ndims);
DECLARE_INTEGER_ARRAY(dims,1);
int new;
char hdstype[DAT__SZTYP+1];

	cnf_exprt(parameter, fparameter, fparameter_length);
	F77_CALL(dat_there) (CHARACTER_ARG(loc), CHARACTER_ARG(fparameter), 
	    LOGICAL_ARG(&there), INTEGER_ARG(status) TRAIL_ARG(loc)
	    TRAIL_ARG(fparameter) );

/*  See if we need to create a new component */
        if ( !(new = F77_ISFALSE(there)) ) {
/* If there is one, get a locator to it and check it's OK */
            F77_CALL(dat_find)(CHARACTER_ARG(loc),
		CHARACTER_ARG(fparameter), CHARACTER_ARG(ploc),
		INTEGER_ARG(status) TRAIL_ARG(loc)
		TRAIL_ARG(fparameter) TRAIL_ARG(ploc));

            F77_CALL(dat_type)(CHARACTER_ARG(ploc), CHARACTER_ARG(fhdstype),
                INTEGER_ARG(status) TRAIL_ARG(ploc) TRAIL_ARG(fhdstype) );
            cnf_imprt(fhdstype, fhdstype_length, hdstype );
           
/* See if the existing component is OK.
 * If type is not "?" check the type against the existing component type
 */
            if ( !( type[0] == '?' ) || !strcasecmp( hdstype, "ADAM_PARNAME" ) ) {
               if ( strcasecmp( hdstype, type ) )  new = 1;

/* Else may be any primitive type (hdstype starts with "_") */
            } else if ( !(hdstype[0] == '_') ) new = 1;
               
/* If we need a new component, delete the old one */
            if ( new ) {
               F77_CALL(dat_annul)(CHARACTER_ARG(ploc), 
                  INTEGER_ARG(status) TRAIL_ARG(ploc) );
               F77_CALL(dat_erase) (CHARACTER_ARG(loc), 
                  CHARACTER_ARG(fparameter), INTEGER_ARG(status) 
                  TRAIL_ARG(loc) TRAIL_ARG(fparameter) );
            }
         }

/* Now set hdstype to the required type of the component 
 * or backup type if type is "?"
 */
         type[0]=='?'?strcpy( hdstype, btype ): strcpy( hdstype, type );
         cnf_exprt(hdstype, fhdstype, fhdstype_length);

/* Now create a new component of necessary */
        if ( new ) {
   /* Create the component */
           ndims = 0;
           if ( !strcasecmp(hdstype,"ADAM_PARNAME") ) {
      /* It is a name */
      /* Create a NAME parameter component */
              cnf_exprt( "ADAM_PARNAME", fhdstype, fhdstype_length);
              F77_CALL(dat_new) (CHARACTER_ARG(loc),
		CHARACTER_ARG(fparameter), CHARACTER_ARG(fhdstype),
		INTEGER_ARG(&ndims), INTEGER_ARRAY_ARG(dims),
		INTEGER_ARG(status) TRAIL_ARG(loc)
		TRAIL_ARG(fparameter) TRAIL_ARG(fhdstype));

      /* and get a locator to it */
              F77_CALL(dat_find)(CHARACTER_ARG(loc),
		CHARACTER_ARG(fparameter), CHARACTER_ARG(ploc),
		INTEGER_ARG(status) TRAIL_ARG(loc)
		TRAIL_ARG(fparameter) TRAIL_ARG(ploc));

      /* create the NAMEPTR component */
                cnf_exprt("NAMEPTR", fcname, fcname_length);
                cnf_exprt("_CHAR*132", fhdstype, fhdstype_length);
              F77_CALL(dat_new) (CHARACTER_ARG(ploc),
		CHARACTER_ARG(fcname), CHARACTER_ARG(fhdstype),
		INTEGER_ARG(&ndims), INTEGER_ARRAY_ARG(dims),
		INTEGER_ARG(status) TRAIL_ARG(ploc) TRAIL_ARG(fcname)
		TRAIL_ARG(fhdstype));

           } else {
   /* 
    * Not a name - create a component of the required type
    */
              F77_CALL(dat_new) (CHARACTER_ARG(loc), 
		CHARACTER_ARG(fparameter), CHARACTER_ARG(fhdstype),
		INTEGER_ARG(&ndims), INTEGER_ARRAY_ARG(dims),
		INTEGER_ARG(status) TRAIL_ARG(loc)
		TRAIL_ARG(fparameter) TRAIL_ARG(fhdstype));

                /* Get locator to newly created component */
  	      F77_CALL(dat_find) (CHARACTER_ARG(loc), 
                CHARACTER_ARG(fparameter), CHARACTER_ARG(ploc), 
                INTEGER_ARG(status) TRAIL_ARG(loc)
	        TRAIL_ARG(fparameter) TRAIL_ARG(ploc));
           }
        }

/* If the component is an ADAM_PARNAME, set the returned locator to
 * the NAMPTR component
 */
        cnf_exprt("NAMEPTR", fcname, fcname_length);
        if ( !strcasecmp( hdstype, "ADAM_PARNAME" ) ) {
	   F77_CALL(dat_find) (CHARACTER_ARG(ploc), 
              CHARACTER_ARG(fcname), CHARACTER_ARG(cloc), 
              INTEGER_ARG(status) TRAIL_ARG(ploc)
              TRAIL_ARG(fcname) TRAIL_ARG(cloc));

      /* Annul the intermediate locator */
           F77_CALL(dat_annul)(CHARACTER_ARG(ploc), INTEGER_ARG(status)
              TRAIL_ARG(ploc) );
      /* and clone the new one and annul it */
           F77_CALL(dat_clone)(CHARACTER_ARG(cloc), CHARACTER_ARG(ploc),
              INTEGER_ARG(status) TRAIL_ARG(cloc) TRAIL_ARG(ploc) );
           F77_CALL(dat_annul)(CHARACTER_ARG(cloc), INTEGER_ARG(status)
              TRAIL_ARG(cloc) );
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
    DECLARE_CHARACTER(fvalue, 256);
    DECLARE_INTEGER(status);
    DECLARE_CHARACTER(floc, DAT__SZLOC);
    DECLARE_CHARACTER(ploc, DAT__SZLOC);
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
                    floc, floc_length, &status );

/*
 * If all OK, Find parameter component
 */
    if (status == SAI__OK) {

        hname?strcpy(type, "ADAM_PARNAME"): strcpy(type, "?");

/*   Ensure a component of the right type exists */
        (void) ensure_exists( floc, floc_length, parameter, 
                              type, "_CHAR*132", ploc, ploc_length,
                              &status );
   /* 
    * Now write the value into the appropriate component 
    *  skipping the first character (@) if hname is 1
    */
        cnf_exprt( value + hname, fvalue, fvalue_length );
   /* Write a normal value */
	F77_CALL(dat_put0c) (CHARACTER_ARG(ploc), CHARACTER_ARG(fvalue),
  	         INTEGER_ARG(&status) TRAIL_ARG(ploc) TRAIL_ARG(fvalue));

    /* Annul the parameter component locator */
        F77_CALL(dat_annul) (CHARACTER_ARG(ploc), INTEGER_ARG(&status)
                             TRAIL_ARG(ploc));

    /* Annul the action component locator
     * this should close the file */
        F77_CALL(dat_annul) (CHARACTER_ARG(floc), INTEGER_ARG(&status)
               		     TRAIL_ARG(floc));

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
    int cvind;
    DECLARE_REAL(rval);
    DECLARE_INTEGER(ival);
    DECLARE_LOGICAL(lval);
    DECLARE_LOGICAL(there);
    DECLARE_CHARACTER(fparameter, MSG_NAME_LEN);
    DECLARE_CHARACTER(fcname, DAT__SZNAM);
    DECLARE_CHARACTER(ffilename, 200);
    DECLARE_CHARACTER(fcval, 256);
    DECLARE_CHARACTER(fmode, DAT__SZMOD);
    DECLARE_CHARACTER(ftype, DAT__SZTYP);
    DECLARE_INTEGER(status);
    DECLARE_INTEGER(istat);
    DECLARE_CHARACTER(loc, DAT__SZLOC);
    DECLARE_CHARACTER(ploc, DAT__SZLOC);
    DECLARE_CHARACTER(cloc, DAT__SZLOC);
    char *filename, *parameter, *type;
    char cval[257];
    node *nw, *variable;
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
    (void) openglobal( "READ", loc, loc_length, &status );

    if (status == SAI__OK)
    {
/* If all OK, Find parameter component */
	cnf_exprt(parameter, fparameter, fparameter_length);
	F77_CALL(dat_there) (CHARACTER_ARG(loc), CHARACTER_ARG(fparameter), 
	    LOGICAL_ARG(&there), INTEGER_ARG(&status) TRAIL_ARG(loc)
	    TRAIL_ARG(fparameter) );
        if ( there )
	    F77_CALL(dat_find) (CHARACTER_ARG(loc), CHARACTER_ARG(fparameter), 
		CHARACTER_ARG(ploc), INTEGER_ARG(&status) TRAIL_ARG(loc)
		TRAIL_ARG(fparameter)
				TRAIL_ARG(ploc));
        else
        {
            status = SAI__ERROR;
            emsSetc( "NAME", parameter, fparameter_length );
            emsRep( " ",
                "GETGLOBAL : Failed to find HDS component for parameter ^name", 
                &status );
        }
	if (status == SAI__OK)
	{
        /* Find type of parameter component */
	    F77_CALL(dat_type) (CHARACTER_ARG(ploc), CHARACTER_ARG(ftype),
		INTEGER_ARG(&status) TRAIL_ARG(ploc) TRAIL_ARG(ftype));
        /*
         * If it is a 'name' type, find the actual name component
         */
	    if ( !strncmp( ftype, "ADAM_PARNAME", 12) )
            {
            /* It is a name - find the NAMEPTR component */
                cnf_exprt("NAMEPTR", fcname, fcname_length);
	        F77_CALL(dat_find) (CHARACTER_ARG(ploc), CHARACTER_ARG(fcname), 
		    CHARACTER_ARG(cloc), INTEGER_ARG(&status) TRAIL_ARG(ploc)
		    TRAIL_ARG(fcname) TRAIL_ARG(cloc));
	    } else
                /*
                 * It is not a 'name' structure - clone the parameter locator
                 */
	        F77_CALL(dat_clone) (CHARACTER_ARG(ploc), CHARACTER_ARG(cloc), 
		    INTEGER_ARG(&status) TRAIL_ARG(ploc) TRAIL_ARG(cloc));
                   
            if ( status == SAI__OK )
            {
            /* Now get the value, in the appropriate type */
                if ( ( !strncmp( ftype, "_CHAR*", 6 ) ) ||
                     ( !strncmp( ftype, "ADAM_PARNAME", 12) ) )
                {
		    F77_CALL(dat_get0c) (CHARACTER_ARG(cloc),
			CHARACTER_ARG(fcval), INTEGER_ARG(&status)
			TRAIL_ARG(cloc) TRAIL_ARG(fcval));
                    if ( status == SAI__OK )                    
                    {
                    /* If it is a name, add the @ */
                        if ( !strncmp( ftype, "ADAM_PARNAME", 12) )
                        {
                            cval[0] = '@';
                            cvind = 1;
                        } else
                            cvind = 0;
		        cnf_impn(fcval, fcval_length, fcval_length, 
                                 cval + cvind );
                        val = value_string( cval );
                    } else ;

                } else if ( ( !strncmp( ftype, "_REAL", 5) ) || 
                            ( !strncmp( ftype, "_DOUBLE", 7) ) )
                    {
                    F77_CALL(dat_get0r) (CHARACTER_ARG(cloc), REAL_ARG(&rval),
			INTEGER_ARG(&status) TRAIL_ARG(cloc) );
                    if ( status == SAI__OK )
                        val = value_real( rval );                    
                    else ;

                } else if  ( !strncmp( ftype, "_INTEGER", 8) )
                    {
                    F77_CALL(dat_get0i) (CHARACTER_ARG(cloc),INTEGER_ARG(&ival),
			INTEGER_ARG(&status) TRAIL_ARG(cloc) );
                    if ( status == SAI__OK )
                        val = value_integer( ival );                    
                    else ;

                } else if  ( !strncmp( ftype, "_LOGICAL", 8) )
                    {
                    F77_CALL(dat_get0l) (CHARACTER_ARG(cloc),LOGICAL_ARG(&lval),
			LOGICAL_ARG(&status) TRAIL_ARG(cloc) );
                    if ( status == SAI__OK )
                        val = value_logical( lval );                    
                    else ;

                } else
                    {
                    status = SAI__ERROR;
                    emsSetc( "TYPE", ftype, ftype_length);
                    emsRep(" ", 
			"GETGLOBAL : Non-standard HDS component type ^TYPE",
			&status);
                    }

		if (status == SAI__OK)
                    assign_helper( variable, val );
		else ;

	   /* Annul the parameter bottom component locator */
		F77_CALL(dat_annul) (CHARACTER_ARG(cloc), INTEGER_ARG(&status)
		    TRAIL_ARG(cloc));
	    } else ;

 	/* Annul the parameter component locator */
            F77_CALL(dat_annul) (CHARACTER_ARG(ploc), INTEGER_ARG(&status)
		TRAIL_ARG(ploc));
	} else ;

    /* close the file - ensuring good status is given */
        istat = SAI__OK;
	F77_CALL(hds_close) (CHARACTER_ARG(loc), INTEGER_ARG(&istat)
	    TRAIL_ARG(loc));
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
    DECLARE_CHARACTER(fvalue, 256);
    DECLARE_INTEGER(status);
    DECLARE_INTEGER(istat);
    DECLARE_CHARACTER(loc, DAT__SZLOC);
    DECLARE_CHARACTER(ploc, DAT__SZLOC);
    DECLARE_LOGICAL(there);
    value val;
    int hname;
    int new;
    char type[DAT__SZTYP];
    char *vp, *parameter, *value;
    node *nw;
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
    openglobal( "UPDATE", loc, loc_length, &status );

/* If all OK, Find parameter component */
    if (status == SAI__OK) {

       hname? strcpy(type, "ADAM_PARNAME"): strcpy(type, "?");

       (void) ensure_exists( loc, loc_length, parameter, 
                             type, "_CHAR*132", ploc, ploc_length,
                             &status );
       /* 
        * Now write the value into the appropriate component 
        *  skipping the first character (@) if hname is 1
        */
        cnf_exprt( value + hname, fvalue, fvalue_length );
            /* Write a normal value */
        F77_CALL(dat_put0c) (CHARACTER_ARG(ploc), CHARACTER_ARG(fvalue),
	      INTEGER_ARG(&status) TRAIL_ARG(ploc) TRAIL_ARG(fvalue));

        /* Annul the parameter component locator */
        F77_CALL(dat_annul) (CHARACTER_ARG(ploc), INTEGER_ARG(&status) 
          TRAIL_ARG(ploc));

    /* close the file - ensuring good status is given */
        istat = SAI__OK;
        F77_CALL(hds_close) (CHARACTER_ARG(loc), INTEGER_ARG(&istat)
          TRAIL_ARG(loc));

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
    DECLARE_INTEGER(status);
    DECLARE_INTEGER(istat);
    DECLARE_CHARACTER(loc, DAT__SZLOC);
    DECLARE_CHARACTER(ploc, DAT__SZLOC);
    value val;
    int hname;
    char *vp, *filename, *parameter, *type;
    node *nw;
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
    openglobal( "UPDATE", loc, loc_length, &status );

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

            (void) ensure_exists( loc, loc_length, parameter, 
                                  type, type, ploc, ploc_length,
                                  &status );
            if ( status == SAI__OK ) {
      /* Reset the component in case it already existed */
               F77_CALL(dat_reset)( CHARACTER_ARG(ploc), INTEGER_ARG(&status)
                  TRAIL_ARG(ploc) );

      /* Annul the locator */
               F77_CALL(dat_annul)(CHARACTER_ARG(ploc), INTEGER_ARG(&status)
                  TRAIL_ARG(ploc) );
            
            }

	} else {
            status = SAI__ERROR;
            emsSetc( "TYPE", type, DAT__SZTYP);
            emsRep(" ",
		"CREATEGLOBAL : Illegal HDS component type, ^TYPE", &status);
        }

    /* close the file - ensuring good status is given */
        istat = SAI__OK;
	F77_CALL(hds_close) (CHARACTER_ARG(loc), INTEGER_ARG(&istat)
	    TRAIL_ARG(loc));

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

    DECLARE_INTEGER(status);
    value val;
/*
 * Determine the HDS directory
 */
    if ( (hds_path = getenv("ADAM_USER")) != NULL)
	hds_path = strconcat( hds_path, "/");
    else
	hds_path = strconcat( getenv("HOME"), "/adam/" );
/*
 * Start HDS
 */
    status = SAI__OK;
    F77_CALL(hds_start) (INTEGER_ARG(&status));
    if (status != SAI__OK)
	return exception("hds_start failed in init_hds()");
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
