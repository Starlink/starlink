#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <limits.h>

#include "ems.h"
#include "asterix.h"

#include "aditypes.h"
#include "adikrnl.h"
#include "adimem.h"
#include "adilist.h"
#include "adistrng.h"
#include "adiparse.h"
#include "adicface.h"
#include "aditable.h"
#include "adierror.h"
#include "adisyms.h"

#include "adifsys.h"

ADIobj DsysADIbase      = ADI__nullid;
ADIclassDef *DsysFileObjTdef  = NULL;

/* Globals for holding information about representations
 *
 */
static int      ADI_G_nrep = 0;
ADIobj   ADI_G_replist = ADI__nullid;
ADIobj		ADI_G_exten_rep_alist = ADI__nullid;


/* Globals holding useful string names */

ADIobj	DnameFileCommit = ADI__nullid;
ADIobj	DnameFileClone = ADI__nullid;
ADIobj	DnameFileClose = ADI__nullid;


void adix_base_null1( ADIobj id, ADIstatus status )
  {
  ;
  }

void adix_base_NewLink( ADIobj id, ADIobj lid, ADIstatus status )
  {
  adic_cputref( id, "ADIlink", lid, status );
  }

ADIobj adix_base_SetLink( int narg, ADIobj args[], ADIstatus status )
  {
  adic_cputref( args[0], "ADIlink", args[1], status );

  return ADI__nullid;
  }

void adix_base_UnLink( ADIobj id, ADIobj lid, ADIstatus status )
  {
  adic_cerase( id, "ADIlink", status );
  }

void adix_newlnk( ADIobj id, ADIobj lid, ADIstatus status )
  {
  ADIobj        args[2];

  args[0] = id; args[1] = lid;

  adix_execi( DnameNewLink, 2, args, status );
  }

ADIobj adix_setlnk( ADIobj id, ADIobj lid, ADIstatus status )
  {
  ADIobj        args[2];
  ADIclassDef	*otdef;
  ADIobj	rval;

  _chk_init_err; _chk_stat_ret(ADI__nullid);

  args[0] = id; args[1] = lid;

  rval = adix_execi( DnameSetLink, 2, args, status );

/* Did the SetLink method return an alternative left hand data object? */
  if ( _valid_q(rval) ) {

/* The alternative supplied must be be derived from our original l.h.s */
    otdef = _DTDEF(id);
    if ( adix_chkder( _DTDEF(rval), otdef, status ) ) {

/* Destroy the original L.H.S, and return the new object */
      adic_erase( &id, status );
      }
    else {
      adic_setecs( SAI__ERROR,
      "The SetLink method returned an object of a class which is not derived from the required class %s",
      status, otdef->name );

      rval = ADI__nullid;
      }
    }
  else
    rval = id;

  return rval;
  }

void adix_unlnk( ADIobj id, ADIobj lid, ADIstatus status )
  {
  ADIobj        args[2];

  _chk_init_err; _chk_stat;

  args[0] = id; args[1] = lid;

  adix_execi( DnameUnLink, 2, args, status );
  }


ADIobj adix_do_fclone( ADIobj id, ADIobj name, ADIstatus status )
  {
  return adix_execi2( DnameFileClone, id, name, status );
  }

void adix_do_fclose( ADIobj id, ADIstatus status )
  {
  _chk_stat;
  adix_execi( DnameFileClose, 1, &id, status );
  }

void adix_do_fcomit( ADIobj id, ADIstatus status )
  {
  _chk_stat;
  adix_execi( DnameFileCommit, 1, &id, status );
  }


ADIlogical adix_isfile( ADIobj id, ADIstatus status )
  {
  ADIclassDef *tdef = _DTDEF(id);

  return adix_chkder( tdef, DsysFileObjTdef, status );
  }


ADIobj adix_getlink( ADIobj id, ADIstatus status )
  {
  ADIobj	rid;

  _chk_init_err; _chk_stat_ret(ADI__nullid);

  adic_cgetref( id, "ADIlink", &rid, status );
  if ( *status == ADI__NOTSET ) {
    adic_erranl( status );
    rid = ADI__nullid;
    }

  return rid;
  }


void adix_getfile( ADIobj id, ADIobj *root, ADIstatus status )
  {
  ADIlogical    found = ADI__false;
  ADIobj        lid = id;

  _chk_init_err; _chk_stat;

  while ( (lid!=ADI__nullid) && _ok(status) && ! found )
    {
    if ( adix_isfile(lid,status) )
      found = ADI__true;
    else
      lid = adix_getlink( lid, status );
    }

  if ( found )
    *root = lid;
  else
    {
    *root = id;
    adic_setecs( SAI__ERROR, "Unable to locate FileObject", status );
    }
  }


void adix_getpath( ADIobj id, ADIlogical nulterm, int mxlen, char *path,
		   int *lpath, ADIstatus status )
  {
  int   actlen = 0;
  char  lbit[30];
  char  *lcbit;
  int lcbitl;
  char lc;
  ADIobj rep;
  ADIobj lid = id;
  ADIclassDef *tdef;

  _chk_init_err; _chk_stat;

  while ( (lid!=ADI__nullid) && _ok(status) ) {
    tdef = _DTDEF(lid);

    if ( tdef == DsysFileObjTdef ) {
      lc = '%';
      adic_cget0i( lid, "REP", (ADIinteger *) &rep, status );
      adic_cget0c( rep, "NAME", 30, lbit, status );
      lcbit = lbit;
      }
    else {
      lc = '>';
      lcbit = tdef->name;
      }

    lcbitl = strlen(lcbit);

    if ( (actlen+1) < mxlen )
      {
      if ( actlen )
	path[actlen++] = lc;

      memcpy( path + actlen, lcbit, lcbitl );
      actlen += lcbitl;
      }

    lid = adix_getlink( lid, status );
    }

  if ( actlen < mxlen )
    {
    if ( nulterm )
      path[actlen] = 0;
    else
      memset( path + actlen, ' ', mxlen - actlen );
    }

  *lpath = actlen;                      /* Return length used */
  }

/*
 * Data system routines
 */


/*
 * Pass down the object chain, committing changes and closing if required
 */
void adix_comit_or_close( ADIobj id, ADIlogical close, ADIstatus status )
  {
  ADIobj	cid = id;		/* Cursor down object chain */
  char		cmode[7];		/* The file access mode */
  ADIobj	lid;			/* Next object in chain */
  ADIobj        repid;

/* Check inherited status on entry, and that system is initialised */
  _chk_init_err; _chk_stat;

/* Unhook all leading linked objects down to the FileObject derived object */
/* at the end of the chain */
  while ( _valid_q(cid) && _ok(status) ) {

/* Is this a FileObject? */
    if ( adix_isfile( cid, status ) ) {

/* Extract representation id from file object */
      adic_cget0i( cid, "REP", &repid, status );

/* If the mode was WRITE or UPDATE, commit changes to disk */
      adic_cget0c( cid, "MODE", 7, cmode, status );
      if ( (cmode[0] != 'r') && (cmode[0] != 'R') ) {
        adix_do_fcomit( cid, status );
        }

/* Close the file if required */
      if ( close )
        adix_do_fclose( cid, status );
      }

/* Get next object in chain */
    lid = adix_getlink( cid, status );

/* Close? */
    if ( close ) {

/* Break the link between these objects */
      if ( _valid_q(lid) )
        adix_unlnk( id, lid, status );

/* Erase the current object */
      adic_erase( &cid, status );
      }

/* Next object down the chain */
    cid = lid;
    }
  }



void ADIfsysFileClose( ADIobj id, ADIstatus status )
  {
/* Check inherited status on entry, and that system is initialised */
  _chk_init_err; _chk_stat;

/* Invoke worker routine in close mode */
  adix_comit_or_close( id, ADI__true, status );
  }


void ADIfsysFileComit( ADIobj id, ADIstatus status )
  {
/* Check inherited status on entry, and that system is initialised */
  _chk_init_err; _chk_stat;

/* Invoke worker routine in commit mode */
  adix_comit_or_close( id, ADI__false, status );
  }



void adix_fcreat_int( ADIobj rtn, ADIobj fspec, ADIobj id, ADIobj *fileid,
                      ADIstatus status )
  {
  if ( _ok(status) ) {
    if ( _eprc_c(rtn) ) 			/* C routine? */
      ((ADIcCreatRCB) _eprc_prc(rtn))( fspec, id, fileid, status );
    else					/* Fortran routine */
      ((ADIfCreatRCB) _eprc_prc(rtn))( &fspec, &id, fileid, status );
    }
  }

void adix_fcreat( char *fspec, int flen, ADIobj id, ADIobj *fileid,
                  ADIstatus status )
  {
  ADIobj	fid;			/* ADI version of fspec */
  ADIlogical	found = ADI__false;	/* Located the representation? */
  ADIobj	ortn;			/* Create routine */
  char		*ppos;
  ADIobj	rid = ADI__nullid;	/* Representation chosen */
  int		rlen;

/* Check inherited global status, and that the system is initialised */
  _chk_init; _chk_stat;

/* Import strings resolving lengths */
  _GET_NAME(fspec,flen);

/* Construct ADI strings */
  adic_newv0c_n( fspec, flen, &fid, status );

/* Look for representation delimiter */
  ppos = strstr( fspec, "%" );

/* User specified a representation? */
  if ( ppos ) {

/* Length of representation code */
    rlen = flen - (ppos-fspec) - 1;

/* Look for representation */
    adix_locrep( ppos+1, rlen, &rid, status );

/* Locate the file creation routine */
    adix_locrcb( rid, "CreatRtn", _CSM, &ortn, status );

/* Try to create the file */
    adix_fcreat_int( ortn, fid, id, fileid, status );

/* Opened ok? */
    found = _ok(status);
    }
  else {
    ADIobj	curp = ADI_G_replist;

    while ( _valid_q(curp) && ! found )	/* Loop over representations */
      {
      ADIobj		cdr;
      ADIlogical	there=ADI__false;

      _GET_CARCDR(rid,cdr,curp);

      adic_there( rid, "CreatRtn", &there, status );

      if ( there ) {
	adix_locrcb( rid, "CreatRtn",	/* Locate the opening routine */
		     _CSM, &ortn, status );

/* Try to create the file */
	adix_fcreat_int( ortn, fid, id, fileid, status );

	if ( _ok(status) )		/* Did it work? */
	  found = ADI__true;
	else
	  adic_erranl( status );
	}

      if ( ! found )			/* Next one */
	curp = cdr;
      }

    }

  if ( ! found ) {			/* Not found? */
    ADIstatype	istat = *status;
    *status = SAI__ERROR;

    adic_erase( &fid, status );		/* Release strings created */

    *status = istat;

    if ( ! ppos )
      adic_setecs( ADI__INVARG, "File cannot be created", status );
    }

/* Created ok? If so, write in details of representation and access mode */
  if ( _ok(status) ) {
    adic_cput0i( *fileid, "REP", rid, status );
    adic_cput0c( *fileid, "MODE", "WRITE", status );
    }

/* Link user object if created ok */
  if ( _ok(status) && ! _null_q(id) ) {

/* Link user object to file object if required */
    adix_newlnk( id, *fileid, status );
    }

  }


/*
 * Construct an object chain with the named class at the head, and the
 * input object specified by the first argument at the tail. Build any
 * intervening links required to construct the chain.
 */
ADIobj adix_link_efile( ADIobj id, char *cls, int clen, ADIstatus status )
  {
  ADIlogical		found = ADI__false;
  ADIobj		rval = id;

/* Check inherited global status. Return input argument if bad */
  _chk_stat_ret(id);

/* If the user has specified wildcard type then we're done */
  if ( *cls != '*' ) {

    ADIobj	ocls;
    ADIlogical  atend;
    int		icp,jcp,fjcp;

/* Import the class name string */
    _GET_NAME(cls,clen);

/* Class name of opened object */
    ocls = _DTDEF(id)->aname;

/* Loop over class list supplied to see if we have a match */
    icp = 0;
    while ( (icp < clen) && _ok(status) && ! found ) {

/* Find end of this class name */
      jcp = icp;
      atend = ADI__false;
      while ( (jcp<clen) && ! atend ) {
        if ( cls[jcp] == '|' )
          atend = ADI__true;
        else
          jcp++;
        }

/* Store end of first word for later */
      if ( ! icp ) fjcp = jcp;

/* Doesn't match what we've got? Advance to next class name */
      if ( strx_cmpc( cls + icp, jcp - icp, ocls ) )
        icp = jcp + 1;
      else
        found = ADI__true;
      }

/* We haven't matched any of the requested classes? */
    if ( ! found ) {
      ADIobj newid;

/* Loop over supplied classes trying to create the link */
      icp = 0;
      found = ADI__false;
      while ( (icp < clen) && _ok(status) && ! found ) {

/*   Find end of this class name */
        jcp = icp;
        atend = ADI__false;
        while ( (jcp<clen) && ! atend ) {
          if ( cls[jcp] == '|' )
            atend = ADI__true;
          else
            jcp++;
          }

/*   Store end of first word for later */
        if ( ! icp ) fjcp = jcp;

/*   Create instance of class */
        adix_newn( ADI__nullid, NULL, 0, cls, jcp-icp, 0, NULL,
		   &newid, status );

/*   Try to link them */
        newid = adix_setlnk( newid, id, status );

/*   Linkage worked? Return new object */
        if ( _ok(status) ) {
          found = ADI__true;
          rval = newid;
          }

/*   Status was no applicable method, in which case we try the next list */
        else if ( *status == ADI__NOMTH ) {

/*   Cancel the bad status */
          adic_erranl( status );

/*   Destroy the object we don't want */
          adic_erase( &newid, status );

/*   Next class */
          icp = jcp + 1;
          }

        }
      }

/* Report error if no linkage */
    if ( ! found )
      adic_setecs( ADI__NOMTH, "Unable to link object of class %S to any of %*s",
                   status, ocls, clen, cls );
    }

/* Set return object */
  return rval;
  }


void adix_fclone( ADIobj id, char *fspec, int flen, char *cls, int clen,
	          ADIobj *fileid, ADIstatus status )
  {
  ADIobj	ifd;			/* File object at end of id chain */
  ADIobj	fid;			/* ADI version of fspec */
  ADIobj	rid = ADI__nullid;	/* Representation chosen */

/* Check inherited global status, and that the system is initialised */
  _chk_init; _chk_stat;

/* Import strings resolving lengths */
  _GET_NAME(fspec,flen);

/* Locate file object at base of input chain */
  adix_getfile( id, &ifd, status );

/* Construct ADI strings */
  adic_newv0c_n( fspec, flen, &fid, status );

/* Try to clone the file */
  *fileid = adix_do_fclone( ifd, fid, status );

/* Cloned ok? If so, write in details of representation and access mode */
  if ( _ok(status) ) {
    adic_cget0i( ifd, "REP", &rid, status );
    adic_cput0i( *fileid, "REP", rid, status );
    adic_cput0c( *fileid, "MODE", "WRITE", status );
    }

/* Release file name string */
  adic_erase( &fid, status );

/* Make link to requested object class */
  *fileid = adix_link_efile( *fileid, cls, clen, status );
  }


void adix_fopen_int( ADIobj rtn, ADIobj fspec, ADIobj mode, ADIobj *id,
                     ADIstatus status )
  {
  if ( _eprc_c(rtn) ) 			/* C routine? */
    ((ADIcOpenRCB) _eprc_prc(rtn))( fspec, mode, id, status );
  else					/* Fortran routine */
    ((ADIfOpenRCB) _eprc_prc(rtn))( &fspec, &mode, id, status );
  }


void adix_fopen( char *fspec, int flen, char *cls, int clen,
                 char *mode, int mlen, ADIobj *id, ADIstatus status )
  {
  ADIobj	fid;			/* ADI version of fspec */
  ADIlogical	found = ADI__false;	/* Located the representation? */
  ADIobj	mid;			/* ADI version of mode */
  ADIobj	ortn;			/* Open routine */
  char		*ppos;
  ADIobj	rid = ADI__nullid;	/* Representation chosen */
  int		rlen;

  _chk_init; _chk_stat;			/* Check status on entry */

/* Import strings resolving lengths */
  _GET_NAME(fspec,flen);
  _GET_STRING(mode,mlen);

/* Construct ADI strings */
  adic_newv0c_n( fspec, flen, &fid, status );
  adic_newv0c_n( mode, mlen, &mid, status );

/* Look for representation delimiter */
  ppos = strstr( fspec, "%" );

/* User specified a representation? */
  if ( ppos ) {

/* Length of representation code */
    rlen = flen - (ppos-fspec) - 1;

/* Look for representation */
    adix_locrep( ppos+1, rlen, &rid, status );

/* Locate the open routine */
    adix_locrcb( rid, "OpenRtn", _CSM, &ortn, status );

/* Try to open file */
    adix_fopen_int( ortn, fid, mid, id, status );

/* Opened ok? */
    found = _ok(status);
    }
  else {
    ADIobj	curp = ADI_G_replist;

    while ( _valid_q(curp) && ! found )	/* Loop over representations */
      {
      ADIobj		cdr;
      ADIlogical	there=ADI__false;

      _GET_CARCDR(rid,cdr,curp);

      adic_there( rid, "OpenRtn", &there, status );

      if ( there ) {

/* Locate the opening routine */
	adix_locrcb( rid, "OpenRtn", _CSM, &ortn, status );

/* Try to open file */
	adix_fopen_int( ortn, fid, mid, id, status );

	if ( _ok(status) )		/* Did it work? */
	  found = ADI__true;
	else
	  adic_erranl( status );
	}

      if ( ! found )			/* Next one */
	curp = cdr;
      }

    }

  if ( ! found ) {			/* Not found? */
    ADIstatype	istat = *status;
    *status = SAI__ERROR;

    adic_erase( &fid, status );		/* Release strings created */
    adic_erase( &mid, status );

    *status = istat;

    if ( ! ppos )
      adic_setecs( ADI__INVARG, "File %*s cannot be opened", status,
                   flen, fspec );
    }

/* Opened ok? If so, write in details of representation and access mode */
  if ( _ok(status) ) {
    adic_cput0i( *id, "REP", rid, status );
    adic_cputid( *id, "MODE", mid, status );
    }

/* Make link to requested object class */
  *id = adix_link_efile( *id, cls, clen, status );
  }


/*  Locate a file representation object by name
 *
 */
void adix_locrep( char *name, int nlen, ADIobj *id, ADIstatus status )
  {
  ADIobj        cid;                    /* Class description id */
  ADIobj        curp = ADI_G_replist;   /* Cursor over representations */
  ADIlogical    found = ADI__false;     /* Found the representation yet? */
  ADIobj        nid;                    /* NAME member address */

  _chk_init; _chk_stat;                 /* Check status on entry */

/* Initialise */
  *id = ADI__nullid;

  _GET_NAME(name,nlen);               	/* Import string */

/* Loop until found or finished */
  while ( (curp!=ADI__nullid) && _ok(status) && ! found ) {

/* Locate class definition object and advance cursor */
    _GET_CARCDR( cid, curp, curp );

/* Find NAME member insertion */
    adic_find( cid, "NAME", &nid, status );

/* Found the one we want? */
    if ( ! strx_cmpic( name, nlen, nid ) ) {
      found = ADI__true;
      *id = cid;
      }

/*  Release NAME member */
    adic_erase( &nid, status );
    }

  if ( ! found )
    adic_setecs( ADI__INVARG, "File representation /%*s/ not known",
		 status, nlen, name );
  }


/*  Define a new file representation object
 *
 */
void adix_defrep( char *name, int nlen, ADIobj *id, ADIstatus status )
  {
  ADIobj        newid;
  ADIobj        nid;

  _chk_init; _chk_stat;                 /* Check status on entry */

  _GET_NAME(name,nlen);               	/* Import string */

/* See if representation already defined */
  adix_locrep( name, nlen, &nid, status );

  if ( _valid_q(nid) )
    adic_setecs( ADI__EXISTS, "File representation /%*s/ already defined",
	status, nlen, name );

  else {
    ems_annul_c( status );

    adic_new0( "FileRepresentation", &newid, status );

/* Create object holding name */
    adic_newv0c_n( name, nlen, &nid, status );

/* Set the representation name */
    adic_cputid( newid, "NAME", nid, status );

    ADI_G_nrep++;                         /* Increment count */

    ADI_G_replist = lstx_append(          /* Add new representation to list */
	ADI_G_replist,
	lstx_cell( newid, ADI__nullid,
		   status ),
	status );

    *id = newid;                          /* Set return value */
    }
  }


/* Define a FileRepresentation callback function
 *
 */
void adix_defrcb( ADIobj rid, char *name, int nlen,
		  ADIobj rtn, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

/* Store rtn in the appropriate member */
  adix_putid( rid, name, nlen, rtn, status );
  }


void adix_locrcb( ADIobj rid, char *name, int nlen,
		  ADIobj *rtn, ADIstatus status )
  {
  _chk_init_err; _chk_stat;

/* Find member data identifier */
  *rtn = adix_find( rid, name, nlen, status );
  }


void ADIfsysInit( ADIstatus status )
  {
  DEFINE_CSTR_TABLE(stringtable)
    CSTR_TENTRY(DnameAround, "Around"),
    CSTR_TENTRY(DnameAfter,  "After"),
    CSTR_TENTRY(DnameBefore, "Before"),
    CSTR_TENTRY(DnameFileCommit,"FileCommit"),
    CSTR_TENTRY(DnameFileClone,"FileClone"),
    CSTR_TENTRY(DnameFileClose,"FileClose"),
    CSTR_TENTRY(DnamePrimary,"Primary"),
    CSTR_TENTRY(DnameNewLink,"NewLink"),
    CSTR_TENTRY(DnameSetLink,"SetLink"),
    CSTR_TENTRY(DnameUnLink, "UnLink"),
  END_CSTR_TABLE;

  DEFINE_GNRC_TABLE(gnrctable)
    GNRC_TENTRY("FileClose(file)",  adix_cdsp_vo,	adix_fdsp_vo),
    GNRC_TENTRY("FileCommit(file)",  adix_cdsp_vo,	adix_fdsp_vo),
    GNRC_TENTRY("NewLink(lhs,rhs)", adix_cdsp_voo,	adix_fdsp_voo),
    GNRC_TENTRY("UnLink(lhs,rhs)",  adix_cdsp_voo,	adix_fdsp_voo),
  END_GNRC_TABLE;

  DEFINE_MTHD_TABLE(mthdtable)
    MTHD_TENTRY( "FileCommit(_ADIbase)",        adix_base_null1 ),
    MTHD_TENTRY( "FileClose(_ADIbase)",        adix_base_null1 ),
    MTHD_TENTRY( "NewLink(_ADIbase,_ADIbase)", adix_base_NewLink ),
/*    MTHD_TENTRY( "SetLink(_ADIbase,_ADIbase)", adix_base_SetLink ), */
    MTHD_TENTRY( "UnLink(_ADIbase,_ADIbase)",  adix_base_UnLink ),
  END_MTHD_TABLE;

  _chk_stat;

/* Add our common strings to the system */
  ADIkrnlAddCommonStrings( stringtable, status );

/* Load file system package */
  adic_reqpkg( "filesys", status );

/* Locate class object */
  DsysFileObjTdef = ADIkrnlFindClsC( "FileObject", _CSM, status );

  adic_reqpkg( "dsmodels", status );

/* Install generics from table */
  ADIkrnlAddGenerics( gnrctable, status );

/* Install methods from table */
  ADIkrnlAddMethods( mthdtable, status );

#ifndef NOHDS
  F77_EXTERNAL_NAME(adi1_init)( status );
#endif
#ifndef NOFITS
  F77_EXTERNAL_NAME(adi2_init)( status );
#endif
  }
