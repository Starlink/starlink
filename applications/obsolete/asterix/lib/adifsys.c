#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <limits.h>

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
ADIobj DsysFileRep      = ADI__nullid;
ADIobj DsysFileObj      = ADI__nullid;
ADIobj DsysScalar       = ADI__nullid;
ADIobj DsysArray        = ADI__nullid;
ADIobj DsysBinDS        = ADI__nullid;
ADIobj DsysSpectrum     = ADI__nullid;

/* Globals for holding information about representations
 *
 */
static int      ADI_G_nrep = 0;
ADIobj   ADI_G_replist = ADI__nullid;
ADIobj		ADI_G_exten_rep_alist = ADI__nullid;

void adix_base_NewLink( ADIobj id, ADIobj lid, ADIstatus status )
  {
printf("Calling base Newlink\n" );
  adic_cput0i( id, "ADIlink", (ADIinteger) lid, status );
  }

void adix_base_SetLink( ADIobj id, ADIobj lid, ADIstatus status )
  {
  adic_cput0i( id, "ADIlink", (ADIinteger) lid, status );
  }

void adix_base_UnLink( ADIobj id, ADIstatus status )
  {
  adic_cput0i( id, "ADIlink", 0, status );
  }

void adix_newlnk( ADIobj id, ADIobj lid, ADIstatus status )
  {
  ADIobj        args[2];

  args[0] = id; args[1] = lid;

  adix_execi( DnameNewLink, 2, args, status );
  }

void adix_setlnk( ADIobj id, ADIobj lid, ADIstatus status )
  {
  ADIobj        args[2];

  args[0] = id; args[1] = lid;

  adix_execi( DnameSetLink, 2, args, status );
  }

void adix_unlnk( ADIobj id, ADIstatus status )
  {
  adix_execi( DnameUnLink, 1, &id, status );
  }

ADIlogical adix_isfile( ADIobj id, ADIstatus status )
  {
  ADIclassDefPtr tdef = _DTDEF(id);

  return adix_chkder( tdef, _cdef_data(DsysFileObj), status );
  }


ADIobj adix_getlink( ADIobj id, ADIstatus status )
  {
  ADIobj lid;

  adic_cget0i( id, "ADIlink", &lid, status );

  return lid;
  }


void adix_getfile( ADIobj id, ADIobj *root, ADIstatus status )
  {
  ADIlogical    found = ADI__false;
  ADIobj        lid = id;

  _chk_stat;

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
  ADIclassDefPtr tdef;

  _chk_stat;

  while ( (lid!=ADI__nullid) && _ok(status) )
    {
    tdef = _DTDEF(lid);

    if ( tdef->selfid == DsysFileObj )
      {
      lc = '%';
      adic_cget0i( lid, "REP", (ADIinteger *) &rep, status );
      adic_cget0c( rep, "NAME", 30, lbit, status );
      lcbit = lbit;
      }
    else
      {
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

void adix_fclose_int( ADIobj rtn, ADIobj id, ADIstatus status )
  {
  if ( _eprc_c(rtn) ) 			/* C routine? */
    ((ADIoCB) _eprc_prc(rtn))( id, status );
  else					/* Fortran routine */
    ((ADIfoCB) _eprc_prc(rtn))( &id, status );
  }

void ADIfsysFileClose( ADIobj id, ADIstatus status )
  {
  ADIobj	fid;			/* File object at end of chain */
  ADIobj	ortn;			/* Close routine */
  ADIobj        repid;
  ADIlogical	there;

  _chk_stat;

/* Get file object */
  adix_getfile( id, &fid, status );

/* Extract representation id from file object */
  adic_cget0i( fid, "REP", &repid, status );

/* Representation has supplied a closure routine? */
  adic_there( repid, "CLOSE_RTN", &there, status );

  if ( there ) {
    adix_locrcb( repid, "CLOSE_RTN",	/* Locate the opening routine */
              _CSM, &ortn, status );

/* Try to close the file */
    adix_fclose_int( ortn, fid, status );
    }

  }

void adix_fcreat_int( ADIobj rtn, ADIobj fspec, ADIobj id, ADIobj *fileid,
                      ADIstatus status )
  {
  _chk_stat;

  if ( _eprc_c(rtn) ) 			/* C routine? */
    ((ADIcCreatRCB) _eprc_prc(rtn))( fspec, id, fileid, status );
  else					/* Fortran routine */
    ((ADIfCreatRCB) _eprc_prc(rtn))( &fspec, &id, fileid, status );
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

  _chk_stat;				/* Check status on entry */

  _GET_STRING(fspec,flen);		/* Import strings resolving lengths */

  adic_newv0c_n( fspec, flen, &fid,	/* Construct ADI strings */
                          status );

  ppos = strstr( fspec, "%" );		/* Look for representation delimiter */

  if ( ppos ) { 			/* User specified a representation? */
    rlen = flen - (ppos-fspec) - 1;	/* Length of representation code */

    adix_locrep( ppos+1, rlen, &rid,	/* Look for representation */
		 status );

    if ( _null_q(rid) )
      adic_setecs( ADI__INVARG, "File representation /^REP/ not known",
                   status );
    else {

/* Locate the file creation routine */
      adix_locrcb( rid, "CREAT_RTN", _CSM, &ortn, status );

/* Try to create the file */
      adix_fcreat_int( ortn, fid, id, fileid, status );

      found = _ok(status);		/* Opened ok? */
      }
    }
  else {
    ADIobj	curp = ADI_G_replist;

    while ( _valid_q(curp) && ! found )	/* Loop over representations */
      {
      ADIlogical	there=ADI__false;

      rid = _CAR(curp);

      adic_there( rid, "CREAT_RTN", &there, status );

      if ( there ) {
        adix_locrcb( rid, "CREAT_RTN",	/* Locate the opening routine */
                     _CSM, &ortn, status );

/* Try to create the file */
        adix_fcreat_int( ortn, fid, id, fileid, status );

        if ( _ok(status) )		/* Did it work? */
	  found = ADI__true;
        else
          adix_errcnl( status );
        }

      if ( ! found )			/* Next one */
        curp = _CDR(curp);
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

  _chk_stat;				/* Check status on entry */

  _GET_STRING(fspec,flen);		/* Import strings resolving lengths */
  _GET_STRING(cls,clen);
  _GET_STRING(mode,mlen);

  adic_newv0c_n( fspec, flen, &fid,	/* Construct ADI strings */
                          status );
  adic_newv0c_n( mode, mlen, &mid,
                          status );

  ppos = strstr( fspec, "%" );		/* Look for representation delimiter */

  if ( ppos ) { 			/* User specified a representation? */
    rlen = flen - (ppos-fspec) - 1;	/* Length of representation code */

    adix_locrep( ppos+1, rlen, &rid,	/* Look for representation */
		 status );

    if ( _null_q(rid) )
      adic_setecs( ADI__INVARG, "File representation /^REP/ not known",
                   status );
    else {
      adix_locrcb( rid, "OPEN_RTN",	/* Locate the open routine */
		   _CSM,
		   &ortn, status );

      adix_fopen_int( ortn, fid, mid,	/* Try to open file */
                        id, status );

      found = _ok(status);		/* Opened ok? */
      }
    }
  else {
    ADIobj	curp = ADI_G_replist;

    while ( _valid_q(curp) && ! found )	/* Loop over representations */
      {
      ADIlogical	there=ADI__false;

      rid = _CAR(curp);

      adic_there( rid, "OPEN_RTN", &there, status );

      if ( there ) {
        adix_locrcb( rid, "OPEN_RTN",	/* Locate the opening routine */
                     _CSM, &ortn, status );

        adix_fopen_int( ortn, fid, mid,	/* Try to open file */
                        id, status );

        if ( _ok(status) )		/* Did it work? */
	  found = ADI__true;
        else
          adix_errcnl( status );
        }

      if ( ! found )			/* Next one */
        curp = _CDR(curp);
      }

    }

  if ( ! found ) {			/* Not found? */
    ADIstatype	istat = *status;
    *status = SAI__ERROR;

    adic_erase( &fid, status );		/* Release strings created */
    adic_erase( &mid, status );

    *status = istat;

    if ( ! ppos )
      adic_setecs( ADI__INVARG, "File cannot be opened", status );
    }

/* Opened ok? If so, write in details of representation and access mode */
  if ( _ok(status) ) {
    adic_cput0i( *id, "REP", rid, status );
    adic_cputid( *id, "MODE", mid, status );
    }

  if ( _ok(status) && (*cls!='*') ) {	/* We've opened the file ok? */
    ADIobj	ocls;

    ocls = _DTDEF(*id)->aname;		/* Class name of opened object */

    if ( strx_cmpc( cls, clen, ocls)) {	/* They're different! */
      ADIobj newid;

      adix_newn( ADI__nullid, NULL, 0,	/* Create requested class object */
		 cls, clen, 0, NULL,
		 &newid, status );

      adix_setlnk( newid, *id,		/* Try to link them */
                     status );

      if ( _ok(status) )
        *id = newid;
      }
    }

  }



/*  Locate a file representation object by name
 *
 */
void adix_locrep( char *name, int nlen, ADIobj *id, ADIstatus status )
  {
  ADIobj        cid;                    /* Class description id */
  ADIobj        curp = ADI_G_replist;   /* Cursor over representations */
  ADIlogical    found = ADI__false;     /* Found the representation yet? */
  ADIobj        *nid;                   /* NAME member address */

  _chk_stat;                            /* Check status on entry */

  _GET_NAME(name,nlen);               	/* Import string */

  while ( (curp!=ADI__nullid) &&        /* Loop until found or finished */
	  _ok(status) &&
	  ! found )
    {
    cid = _CAR(curp);                   /* Locate class definition object */

    adix_findmem( cid, "NAME", 4,       /* Find NAME member insertion */
		  &nid, NULL, status );

    if ( ! strx_cmpc( name, nlen,       /* Found the one we want? */
		      *nid ) )
      {
      found = ADI__true;
      *id = cid;
      }
    else
      curp = _CDR(curp);
    }
  }


/*  Define a new file representation object
 *
 */
void adix_defrep( char *name, int nlen, ADIobj *id, ADIstatus status )
  {
  ADIobj        newid;
  ADIobj        nid;

  _chk_stat;                            /* Check status on entry */

  _GET_NAME(name,nlen);               	/* Import string */

  adic_new0( "FileRepresentation", &newid, status );

  adic_newv0c_n( name, nlen,            /* Create object holding name */
		 &nid, status );

  adic_cputid( newid, "NAME",           /* Set the representation name */
	       nid, status );

  ADI_G_nrep++;                         /* Increment count */

  ADI_G_replist = lstx_append(          /* Add new representation to list */
	ADI_G_replist,
	lstx_cell( newid, ADI__nullid,
		   status ),
	status );

  *id = newid;                          /* Set return value */
  }


/* Define a FileRepresentation callback function
 *
 */
void adix_defrcb( ADIobj rid, char *name, int nlen,
		  ADIobj rtn, ADIstatus status )
  {
  _chk_stat;

  adix_cputid( rid, name, nlen,		/* Store rtn in the appropriate member */
	       rtn, status );
  }


void adix_locrcb( ADIobj rid, char *name, int nlen,
		  ADIobj *rtn, ADIstatus status )
  {
  _chk_stat;

  *rtn = adix_find( rid, name, nlen,   	/* Find member data identifier */
                    status );
  }


void ADIfsysInit( ADIstatus status )
  {
  DEFINE_CSTR_TABLE(stringtable)
    DEFINE_CSTR_TABLE_ENTRY(DnameAround, "Around"),
    DEFINE_CSTR_TABLE_ENTRY(DnameAfter,	 "After"),
    DEFINE_CSTR_TABLE_ENTRY(DnameBefore, "Before"),
    DEFINE_CSTR_TABLE_ENTRY(DnamePrimary,"Primary"),
    DEFINE_CSTR_TABLE_ENTRY(DnameNewLink,"NewLink"),
    DEFINE_CSTR_TABLE_ENTRY(DnameSetLink,"SetLink"),
    DEFINE_CSTR_TABLE_ENTRY(DnameUnLink, "UnLink"),
  END_CSTR_TABLE;

  static struct
    {
    char        *spec;
    ADIcGenericDispatchCB       cdisp;
    ADIfGenericDispatchCB       fdisp;
    }
  gtable[] =
    {
    {"NewLink(lhs,rhs)",       adix_cdsp_voo,	adix_fdsp_voo},
    {"SetLink(lhs,rhs)",       adix_cdsp_voo,	adix_fdsp_voo},
    {"UnLink(lhs,rhs)",        adix_cdsp_vo,	adix_fdsp_vo},
    {NULL,NULL,NULL}};

  static struct
    {
    char        *spec;
    ADIcMethodCB       exec;
    }
  mtable[] =
    {
    {"NewLink(ADIbase,ADIbase)",       (ADIcMethodCB) adix_base_NewLink},
    {"SetLink(ADIbase,ADIbase)",       (ADIcMethodCB) adix_base_SetLink},
    {"UnLink(ADIbase,ADIbase)",        (ADIcMethodCB) adix_base_UnLink},
    {NULL,NULL}};

  int i;

  _chk_stat;

/* Add our common strings to the system */
  ADIkrnlAddCommonStrings( stringtable, status );

  adic_defcls( "FileRepresentation",
	       "", "NAME,OPEN_RTN,CREAT_RTN,NATRL_RTN,CLOSE_RTN",
	       &DsysFileRep, status );

  adic_defcls( "FileObject",
	       "ADIbase", "REP,MODE",
	       &DsysFileObj, status );

  adic_defcls( "Scalar",
	       "ADIbase", "TYPE,Value*",
	       &DsysScalar, status );

  adic_defcls( "Array",
	       "ADIbase", "TYPE,SHAPE,Origin,Values*,SpacedValues",
	       &DsysArray, status );

  adic_defcls( "BinDS",
	       "ADIbase", "TYPE,SHAPE,Data,Variance,Quality,QualityMask",
	       &DsysBinDS, status );

  adic_defcls( "Spectrum",
	       "BinDS", "",
	       &DsysSpectrum, status );

  for( i=0; gtable[i].spec; i++ ) {     /* Install methods from table */
    ADIobj	gid;

    adic_defgen( gtable[i].spec,        /* Define generic and C dispatch */
		 "", gtable[i].cdisp,
		 &gid, status );

    if ( gtable[i].fdisp )		/* Fortran dispatch defined? */
      adix_defgdp( gid,
		   adix_neweprc( ADI__false, (ADICB) gtable[i].fdisp, status ),
		   status );
    }

  for( i=0; mtable[i].spec; i++ )       /* Install methods from table */
    adic_defmth( mtable[i].spec,        /* Ignore returned id */
		 mtable[i].exec,
		 NULL, status );

#ifndef NOHDS
  F77_EXTERNAL_NAME(adi1_init)( status );
#endif
#ifndef NOFITS
  F77_EXTERNAL_NAME(adi2_init)( status );
#endif
  }
