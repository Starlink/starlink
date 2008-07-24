/*
*+
*  Name:
*     err.c

*  Purpose:
*     C wrapper around Fortran interface.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     AJC: Alan Chipperfield (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     16-APR-2006 (TIMJ):
*        Add prolog.

*-
*/

#include <string.h>
#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

void errFioer( const char *token,
               int iostat ) {

DECLARE_CHARACTER_DYN(ftoken);
DECLARE_INTEGER(fiostat);

   F77_CREATE_CHARACTER(ftoken,strlen( token ));
   F77_EXPORT_CHARACTER(token,ftoken,ftoken_length);
   F77_EXPORT_INTEGER(iostat,fiostat);

   F77_CALL(err_fioer)( CHARACTER_ARG(ftoken),
                        INTEGER_ARG(&fiostat)
                        TRAIL_ARG(ftoken) );

   F77_FREE_CHARACTER(ftoken);

   return;
}

void errFlbel( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_CALL(err_flbel)( INTEGER_ARG(&fstatus) );

   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}

void errFlush( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_CALL(err_flush)( INTEGER_ARG(&fstatus) );

   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}

void errLoad( char *param,
              int param_length,
              int *parlen,
              char *opstr,
              int opstr_length,
              int *oplen,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fparlen);
DECLARE_CHARACTER_DYN(fopstr);
DECLARE_INTEGER(foplen);
DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER(fparam,param_length-1);
   F77_CREATE_CHARACTER(fopstr,opstr_length-1);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(err_load)( CHARACTER_ARG(fparam),
                       INTEGER_ARG(&fparlen),
                       CHARACTER_ARG(fopstr),
                       INTEGER_ARG(&foplen),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam)
                       TRAIL_ARG(fopstr) );

   F77_IMPORT_CHARACTER(fparam,fparam_length,param);
   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fparlen,*parlen);
   F77_IMPORT_CHARACTER(fopstr,fopstr_length,opstr);
   F77_FREE_CHARACTER(fopstr);
   F77_IMPORT_INTEGER(foplen,*oplen);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}

void errOut( const char *param,
             const char *text,
             int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(ftext);
DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER(fparam,strlen( param ));
   F77_EXPORT_CHARACTER(param,fparam,fparam_length);
   F77_CREATE_CHARACTER(ftext,strlen( text ));
   F77_EXPORT_CHARACTER(text,ftext,ftext_length);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(err_out)( CHARACTER_ARG(fparam),
                      CHARACTER_ARG(ftext),
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(fparam)
                      TRAIL_ARG(ftext) );

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(ftext);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}

void errRep( const char *param,
             const char *text,
             int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(ftext);
DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER(fparam,strlen( param ));
   F77_EXPORT_CHARACTER(param,fparam,fparam_length);
   F77_CREATE_CHARACTER(ftext,strlen( text ));
   F77_EXPORT_CHARACTER(text,ftext,ftext_length);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(err_rep)( CHARACTER_ARG(fparam),
                      CHARACTER_ARG(ftext),
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(fparam)
                      TRAIL_ARG(ftext) );

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(ftext);

   return;
}

void errTune( const char *param,
              int value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER(fparam,strlen( param ));
   F77_EXPORT_CHARACTER(param,fparam,fparam_length);
   F77_EXPORT_INTEGER(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(err_tune)( CHARACTER_ARG(fparam),
                       INTEGER_ARG(&fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam) );

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}

void msgBlank( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(msg_blank)( INTEGER_ARG(&fstatus) );

   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}

void msgFmtc( const char *token,
              const char *format,
              const char *cvalue ) {

DECLARE_CHARACTER_DYN(ftoken);
DECLARE_CHARACTER_DYN(fformat);
DECLARE_CHARACTER_DYN(fcvalue);

   F77_CREATE_CHARACTER(ftoken,strlen( token ));
   F77_EXPORT_CHARACTER(token,ftoken,ftoken_length);
   F77_CREATE_CHARACTER(fformat,strlen( format ));
   F77_EXPORT_CHARACTER(format,fformat,fformat_length);
   F77_CREATE_CHARACTER(fcvalue,strlen( cvalue ));
   F77_EXPORT_CHARACTER(cvalue,fcvalue,fcvalue_length);

   F77_CALL(msg_fmtc)( CHARACTER_ARG(ftoken),
                       CHARACTER_ARG(fformat),
                       CHARACTER_ARG(fcvalue)
                       TRAIL_ARG(ftoken)
                       TRAIL_ARG(fformat)
                       TRAIL_ARG(fcvalue) );

   F77_FREE_CHARACTER(ftoken);
   F77_FREE_CHARACTER(fformat);
   F77_FREE_CHARACTER(fcvalue);

   return;
}

void msgFmtd( const char *token,
              const char *format,
              double dvalue ) {

DECLARE_CHARACTER_DYN(ftoken);
DECLARE_CHARACTER_DYN(fformat);
DECLARE_DOUBLE(fdvalue);

   F77_CREATE_CHARACTER(ftoken,strlen( token ));
   F77_EXPORT_CHARACTER(token,ftoken,ftoken_length);
   F77_CREATE_CHARACTER(fformat,strlen( format ));
   F77_EXPORT_CHARACTER(format,fformat,fformat_length);
   F77_EXPORT_DOUBLE(dvalue,fdvalue);

   F77_CALL(msg_fmtd)( CHARACTER_ARG(ftoken),
                       CHARACTER_ARG(fformat),
                       DOUBLE_ARG(&fdvalue)
                       TRAIL_ARG(ftoken)
                       TRAIL_ARG(fformat) );

   F77_FREE_CHARACTER(ftoken);
   F77_FREE_CHARACTER(fformat);

   return;
}

void msgFmti( const char *token,
              const char *format,
              int ivalue ) {

DECLARE_CHARACTER_DYN(ftoken);
DECLARE_CHARACTER_DYN(fformat);
DECLARE_INTEGER(fivalue);

   F77_CREATE_CHARACTER(ftoken,strlen( token ));
   F77_EXPORT_CHARACTER(token,ftoken,ftoken_length);
   F77_CREATE_CHARACTER(fformat,strlen( format ));
   F77_EXPORT_CHARACTER(format,fformat,fformat_length);
   F77_EXPORT_INTEGER(ivalue,fivalue);

   F77_CALL(msg_fmti)( CHARACTER_ARG(ftoken),
                       CHARACTER_ARG(fformat),
                       INTEGER_ARG(&fivalue)
                       TRAIL_ARG(ftoken)
                       TRAIL_ARG(fformat) );

   F77_FREE_CHARACTER(ftoken);
   F77_FREE_CHARACTER(fformat);

   return;
}

void msgFmtl( const char *token,
              const char *format,
              int lvalue ) {

DECLARE_CHARACTER_DYN(ftoken);
DECLARE_CHARACTER_DYN(fformat);
DECLARE_LOGICAL(flvalue);

   F77_CREATE_CHARACTER(ftoken,strlen( token ));
   F77_EXPORT_CHARACTER(token,ftoken,ftoken_length);
   F77_CREATE_CHARACTER(fformat,strlen( format ));
   F77_EXPORT_CHARACTER(format,fformat,fformat_length);
   F77_EXPORT_LOGICAL(lvalue,flvalue);

   F77_CALL(msg_fmtl)( CHARACTER_ARG(ftoken),
                       CHARACTER_ARG(fformat),
                       LOGICAL_ARG(&flvalue)
                       TRAIL_ARG(ftoken)
                       TRAIL_ARG(fformat) );

   F77_FREE_CHARACTER(ftoken);
   F77_FREE_CHARACTER(fformat);

   return;
}

void msgFmtr( const char *token,
              const char *format,
              float rvalue ) {

DECLARE_CHARACTER_DYN(ftoken);
DECLARE_CHARACTER_DYN(fformat);
DECLARE_REAL(frvalue);

   F77_CREATE_CHARACTER(ftoken,strlen( token ));
   F77_EXPORT_CHARACTER(token,ftoken,ftoken_length);
   F77_CREATE_CHARACTER(fformat,strlen( format ));
   F77_EXPORT_CHARACTER(format,fformat,fformat_length);
   F77_EXPORT_REAL(rvalue,frvalue);

   F77_CALL(msg_fmtr)( CHARACTER_ARG(ftoken),
                       CHARACTER_ARG(fformat),
                       REAL_ARG(&frvalue)
                       TRAIL_ARG(ftoken)
                       TRAIL_ARG(fformat) );

   F77_FREE_CHARACTER(ftoken);
   F77_FREE_CHARACTER(fformat);

   return;
}

void msgIflev( int *filter ) {

DECLARE_INTEGER(ffilter);

   F77_CALL(msg_iflev)( INTEGER_ARG(&ffilter) );

   F77_IMPORT_INTEGER(ffilter,*filter);

   return;
}

void msgIfset( int filter,
               int *status ) {

DECLARE_INTEGER(ffilter);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER(filter,ffilter);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(msg_ifset)( INTEGER_ARG(&ffilter),
                        INTEGER_ARG(&fstatus) );

   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}

void msgLoad( const char *param,
              const char *text,
              char *opstr,
              int opstr_length,
              int *oplen,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(ftext);
DECLARE_CHARACTER_DYN(fopstr);
DECLARE_INTEGER(foplen);
DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER(fparam,strlen( param ));
   F77_EXPORT_CHARACTER(param,fparam,fparam_length);
   F77_CREATE_CHARACTER(ftext,strlen( text ));
   F77_EXPORT_CHARACTER(text,ftext,ftext_length);
   F77_CREATE_CHARACTER(fopstr,opstr_length-1);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(msg_load)( CHARACTER_ARG(fparam),
                       CHARACTER_ARG(ftext),
                       CHARACTER_ARG(fopstr),
                       INTEGER_ARG(&foplen),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam)
                       TRAIL_ARG(ftext)
                       TRAIL_ARG(fopstr) );

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(ftext);
   F77_IMPORT_CHARACTER(fopstr,fopstr_length,opstr);
   F77_FREE_CHARACTER(fopstr);
   F77_IMPORT_INTEGER(foplen,*oplen);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}

void msgOut( const char *param,
             const char *text,
             int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(ftext);
DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER(fparam,strlen( param ));
   F77_EXPORT_CHARACTER(param,fparam,fparam_length);
   F77_CREATE_CHARACTER(ftext,strlen( text ));
   F77_EXPORT_CHARACTER(text,ftext,ftext_length);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(msg_out)( CHARACTER_ARG(fparam),
                      CHARACTER_ARG(ftext),
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(fparam)
                      TRAIL_ARG(ftext) );

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(ftext);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}

void msgOutif( int prior,
               const char *param,
               const char *text,
               int *status ) {

DECLARE_INTEGER(fprior);
DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(ftext);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER(prior,fprior);
   F77_CREATE_CHARACTER(fparam,strlen( param ));
   F77_EXPORT_CHARACTER(param,fparam,fparam_length);
   F77_CREATE_CHARACTER(ftext,strlen( text ));
   F77_EXPORT_CHARACTER(text,ftext,ftext_length);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(msg_outif)( INTEGER_ARG(&fprior),
                        CHARACTER_ARG(fparam),
                        CHARACTER_ARG(ftext),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(ftext) );

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(ftext);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}

void msgRenew( void ) {

   F77_CALL(msg_renew)(  );

   return;
}

void msgTune( const char *param,
              int value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER(fparam,strlen( param ));
   F77_EXPORT_CHARACTER(param,fparam,fparam_length);
   F77_EXPORT_INTEGER(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(msg_tune)( CHARACTER_ARG(fparam),
                       INTEGER_ARG(&fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam) );

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
