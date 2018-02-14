/*
*+
*  Name:
*     hdsTest

*  Purpose:
*     Test the C interface to HDS

*  Language:
*     Starlink ANSI C

*  Description:
*     This program tests some of the C API to HDS. It is not meant
*     to be an exhaustive test of all the API (at least not initially).

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     04-NOV-2005 (TIMJ):
*        Original.
*     20-DEC-2005 (TIMJ):
*        No longer requires FC_MAIN
*     25-JAN-2006 (TIMJ):
*        Add hdsShow/hdsInfoI
*     {enter_further_changes_here}

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <string.h>
#include "sae_par.h"
#include "hds.h"
#include <stdlib.h>
#include "ems.h"
#include "dat_err.h"
#include <stdio.h>
#include <inttypes.h>

static void traceme (const HDSLoc * loc, int *status);
static void cmpprec ( const HDSLoc * loc1, const char * name, int * status );


int main (void) {

  /*  Local Variables: */
  const char path[] = "hds_ctest";
  int status = SAI__OK;
  hdsdim dim[] = { 10, 20 };
  hdsdim dimd[1];
  const char * chararr[] = { "TEST1", "TEST2", "Longish String" };
  char *retchararr[4];
  char buffer[1024];  /* plenty large enough */
  double darr[] = { 4.5, 2.5 };
  double retdarr[2];
  void *mapv;    /* Mapped void* */
  double *mapd;  /* Mapped _DOUBLE */
  float  *mapf;  /* Mapped _REAL */
  int *mapi;     /* Mapped _INTEGER */
  int64_t *mapi64; /* Mapped _INT64 */
  HDSLoc * loc1 = NULL;
  HDSLoc * loc2 = NULL;
  HDSLoc * loc3 = NULL;
  size_t actval;
  size_t nel;
  size_t nelt;
  size_t nbytes;
  size_t i;
  int n;
  double sumd;
  int sumi;
  int64_t sumi64;
  int64_t test64;
  int64_t testin64;
  const int64_t VAL__BADK = (-9223372036854775807 - 1);

  emsBegin(&status);

  /* Force 64-bit mode */
  hdsTune( "64BIT", 1, &status );

  /* Create a new container file */
  hdsNew( path, "HDS_TEST", "NDF", 0, dim, &loc1, &status );

  {
    int gndim;
    int i;
    hdsdim celldims[] = {1};
    hdsdim gdims[DAT__MXDIM];
    datShape(loc1, DAT__MXDIM, gdims, &gndim, &status);
    if (status == SAI__OK) {
      printf("GROUP NDIMS: %d\n", gndim);
      for (i=0; i<gndim; i++) {
        printf(" Dim[%d] = %zu\n", i, (size_t)gdims[i]);
      }
    }
    datVec( loc1, &loc2, &status);
    datShape(loc2, DAT__MXDIM, gdims, &gndim, &status);
    if (status == SAI__OK) {
      printf("GROUP NDIMS: %d\n", gndim);
      for (i=0; i<gndim; i++) {
        printf(" Dim[%d] = %zu\n", i, (size_t)gdims[i]);
      }
    }
    datCell( loc2, 1, celldims, &loc3, &status );
    datAnnul( &loc3, &status );
    datAnnul( &loc2, &status );
  }

  /* Some components */
  datNew( loc1, "DATA_ARRAY", "_INTEGER", 2, dim, &status );
  datNew1C( loc1, "ONEDCHAR", 14, 3, &status );
  datNew1D( loc1, "ONEDD", 2, &status );
  datNew0K( loc1, "TESTI64", &status );
  datNew0K( loc1, "TESTBADI64", &status );
  datNew1L( loc1, "BOOLEAN", 3, &status );

  /* Create structure array */
  {
    hdsdim histdim[] = { 5, 2 };
    hdsdim subs[] = { 3, 2 };
    char namestr[DAT__SZNAM+1];
    char opstr[2048];
    HDSLoc * loc4 = NULL;
    datNew( loc1, "RECORDS", "HIST_REC", 2, histdim, &status );
    datFind( loc1, "RECORDS", &loc2, &status );
    datCell( loc2, 2, subs, &loc3, &status );
    datNew0I( loc3, "INTINCELL", &status );
    datFind( loc3, "INTINCELL", &loc4, &status );
    datName( loc2, namestr, &status );
    printf("Parent name '%s'\n", namestr );
    datName( loc3, namestr, &status );
    printf("Cell name '%s'\n", namestr );
    /* Now for datRef */
    datRef( loc2, opstr, sizeof(opstr), &status);
    printf("datRef structure array: %s\n", opstr);
    datRef( loc3, opstr, sizeof(opstr), &status);
    printf("datRef cell: %s\n", opstr);
    traceme(loc3, &status);
    traceme(loc4, &status);
    datAnnul( &loc4, &status );
    datAnnul( &loc3, &status );
    datAnnul( &loc2, &status );
  }

  { /* Create structure and then put a
       component of each type in it */

//    datNew0B( loc1, "BYTE", &status);
//    datNew0UB( loc1, "UBYTE", &status);
    datNew0W( loc1, "WORD", &status);
    datNew0UW( loc1, "UWORD", &status);
    datNew0I( loc1, "INTEGER", &status);
    datNew0K( loc1, "INT64", &status);
    datNew0L( loc1, "LOGICAL", &status);
    datNew0R( loc1, "REAL", &status);
    datNew0D( loc1, "DOUBLE", &status);
    datNew0C( loc1, "CHAR", 12, &status );

//    cmpprec( loc1, "BYTE", &status );
//    cmpprec( loc1, "UBYTE", &status );
    cmpprec( loc1, "WORD", &status );
    cmpprec( loc1, "UWORD", &status );
    cmpprec( loc1, "INTEGER", &status );
    cmpprec( loc1, "INT64", &status );
    cmpprec( loc1, "LOGICAL", &status );
    cmpprec( loc1, "REAL", &status );
    cmpprec( loc1, "DOUBLE", &status );
    cmpprec( loc1, "CHAR", &status );

  }



  /* Populate */
  testin64 = 9223372036854775800;
  datFind( loc1, "TESTI64", &loc2, &status );
  datPut0K( loc2, testin64, &status );
  datGet0K( loc2, &test64, &status );
  datAnnul( &loc2, &status );
  if (status == SAI__OK) {
    if ( test64 != testin64 ) {
      status = DAT__FATAL;
      emsRepf( "TESTI64", "Test _INT64 value %" PRIi64 " did not match expected %"PRIi64,
               &status, test64, testin64 );
    }
  }

  datFind( loc1, "TESTBADI64", &loc2, &status );
  datPut0K( loc2, VAL__BADK, &status );
  datGet0K( loc2, &test64, &status );
  datAnnul( &loc2, &status );
  if (status == SAI__OK) {
    if ( test64 != VAL__BADK ) {
      status = DAT__FATAL;
      emsRepf( "TESTBADI64", "Test _INT64 value %" PRIi64 " did not match expected VAL__BADK",
               &status, test64 );
    }
  }

  datFind( loc1, "ONEDCHAR", &loc2, &status );
  datPutVC( loc2, 3, chararr, &status );

  traceme(loc2, &status);

 if (status == SAI__OK) {
    /* Do not use MERS in test. We create an error message
       with EMS and then extract it as text */
    int lstat = DAT__FATAL;
    char param[10];
    char opstr[2048];
    int oplen;
    int parlen;
    emsMark();
    datMsg("OBJ", loc2 );
    emsRep("", "^OBJ", &lstat);
    emsEload( param, &parlen, opstr, &oplen, &lstat);
    printf("datMsg: %s\n", opstr);
    emsAnnul(&lstat);
    emsRlse();

    /* Now for datRef */
    datRef( loc2, opstr, sizeof(opstr), &status);
    printf("datRef: %s\n", opstr);
  }


  /* Check contents */
  datGetVC(loc2, 3, 1024, buffer, retchararr, &actval, &status);
  if (status == SAI__OK) {
    if (actval == 3) {
      for (i = 0; i < 3; i++ ) {
	if (strncmp( chararr[i], retchararr[i], strlen(chararr[i]) ) ) {
           status = DAT__DIMIN;
	   emsSetc( "IN", chararr[i]);
	   emsSetc( "OUT", retchararr[i] );
           emsRep( "GET1C","Values from Get1C differ (^IN != ^OUT)", &status);
           break;
         }
      }
    } else {
      status = DAT__DIMIN;
      emsRep( "GET1C","Did not get back as many strings as put in", &status);
    }
  }

  datAnnul( &loc2, &status );

  datFind( loc1, "BOOLEAN", &loc2, &status );
  {
    int boolarr[] = { 1, 0, 1};
    datPutVL( loc2, 3, boolarr, &status );
  }
  datAnnul( &loc2, &status );

  datFind( loc1, "ONEDD", &loc2, &status );
  datPutVD( loc2, 2, darr, &status );

  /* Check contents */
  datGetVD( loc2, 2, retdarr, &actval, &status);
  if (status == SAI__OK) {
    if (actval == 2) {
      for (i = 0; i < 2; i++ ) {
         if (darr[i] != retdarr[i]) {
           status = DAT__DIMIN;
           emsRep( "GETVD","Values from getVD differ", &status);
           break;
         }
      }
    } else {
      status = DAT__DIMIN;
      emsRep( "GETVD","Did not get back as many values as put in", &status);
    }
  }

  /* Try mapping - _DOUBLE */
  dimd[0] = 2;
  datMapD(loc2, "READ", 1, dimd, &mapd, &status);
  if (status == SAI__OK) {
      for (i = 0; i < 2; i++ ) {
         if (darr[i] != mapd[i]) {
           status = DAT__DIMIN;
           emsRep( "MAPD","Values from MapD differ", &status);
           break;
         }
      }
  }
  datUnmap(loc2, &status);

  /* Try mapping - _FLOAT */
  datMapR(loc2, "READ", 1, dimd, &mapf, &status);
  if (status == SAI__OK) {
      for (i = 0; i < 2; i++ ) {
         if ( (float)darr[i] != mapf[i]) {
           status = DAT__DIMIN;
           emsRep( "MAPR","Values from MapR differ", &status);
           break;
         }
      }
  }
  datUnmap(loc2, &status);

  /* Annul */
  datAnnul( &loc2, &status );

  /* Find and map DATA_ARRAY */
  datFind( loc1, "DATA_ARRAY", &loc2, &status );
  datMapV( loc2, "_REAL", "WRITE", &mapv, &nel, &status );
  mapf = mapv;
  if (status == SAI__OK) {
    nelt = dim[0] * dim[1];
    if ( nelt != nel) {
      status = DAT__FATAL;
      emsSeti( "NEL", (int)nel );
      emsSeti( "NORI", (int)nelt );
      emsRep( "SIZE","Number of elements originally (^NORI) not the same as now (^NEL)", &status);
    }
  }
  sumd = 0.0;
  for (i = 1; i <= nel; i++) {
    mapf[i-1] = (float)i;
    sumd += (double)i;
  }
  datUnmap( loc2, &status );
  datAnnul( &loc2, &status );
  hdsClose( &loc1, &status );

  /* Re-open */
  hdsOpen( path, "UPDATE", &loc1, &status );

  /* Look for the data array and map it */
  datFind( loc1, "DATA_ARRAY", &loc2, &status );
  datVec( loc2, &loc3, &status );
  datSize( loc3, &nel, &status);
  if (status == SAI__OK) {
    nelt = dim[0] * dim[1];
    if ( nelt != nel) {
      status = DAT__FATAL;
      emsSeti( "NEL", (int)nel );
      emsSeti( "NORI", (int)nelt );
      emsRep( "SIZE","Number of elements before (^NORI) not the same as now (^NEL)", &status);
    }
  }

  datPrec( loc3, &nbytes, &status );
  if (status == SAI__OK) {
    if ( nbytes != 4) {
      status = DAT__FATAL;
      emsSeti( "NB", nbytes );
      emsRep( "PREC","Precision for _REAL not 4 bytes but ^NB", &status);
    }
  }
  datLen( loc3, &nbytes, &status );
  if (status == SAI__OK) {
    if ( nbytes != 4) {
      status = DAT__FATAL;
      emsSeti( "NB", nbytes );
      emsRep( "PREC","Precision for _REAL not 4 bytes but ^NB", &status);
    }
  }

  /* Try hdsShow */
  //hdsShow("LOCATORS", &status);
  //hdsShow("FILES", &status);
  //hdsInfoI(NULL, "LOCATORS", "!HDS_TEST.,YYY", &n, &status );
  //hdsInfoI(NULL, "FILES", NULL, &n, &status );

  datAnnul( &loc3, &status );

  datMapV( loc2, "_INTEGER", "READ", &mapv, &nel, &status );
  mapi = mapv;
  if (status == SAI__OK) {
    nelt = dim[0] * dim[1];
    if ( nelt != nel) {
      status = DAT__FATAL;
      emsSeti( "NEL", (int)nel );
      emsSeti( "NORI", (int)nelt );
      emsRep( "SIZE","Number of elements originally (^NORI) not the same as now (^NEL)", &status);
    }
  }
  sumi = 0;
  for (i = 0; i < nel; i++) {
    sumi += mapi[i];
  }
  datUnmap( loc2, &status );

  if (status == SAI__OK) {
    if (sumi != (int)sumd) {
      status = DAT__FATAL;
      emsSeti( "I", sumi );
      emsSeti( "D", (int)sumd );
      emsRep("SUM","Sum was not correct. Got ^I rather than ^D", &status );
    }
  }

  /* _INT64 test */
  datMapV( loc2, "_INT64", "READ", &mapv, &nel, &status );
  mapi64 = mapv;
  if (status == SAI__OK) {
    nelt = dim[0] * dim[1];
    if ( nelt != nel) {
      status = DAT__FATAL;
      emsSeti( "NEL", (int)nel );
      emsSeti( "NORI", (int)nelt );
      emsRep( "SIZE","Number of elements originally (^NORI) not the same as now (^NEL)", &status);
    }
  }
  sumi64 = 0;
  for (i = 0; i < nel; i++) {
    sumi64 += mapi64[i];
  }
  datUnmap( loc2, &status );

  if (status == SAI__OK) {
    if (sumi64 != (int)sumd) {
      status = DAT__FATAL;
      emsSeti( "I", (int)sumi64 );
      emsSeti( "D", (int)sumd );
      emsRep("SUM","Sum was not correct. Got ^I rather than ^D", &status );
    }
  }


  /* Tidy up and close */
  //hdsErase( &loc1, &status );

  if (status == SAI__OK) {
    printf("HDS C installation test succeeded\n");
    emsEnd(&status);
    return EXIT_SUCCESS;
  } else {
    printf("HDS C installation test failed\n");
    emsEnd(&status);
    return EXIT_FAILURE;
  }


}


static void cmpprec ( const HDSLoc * loc1, const char * name, int * status ) {
    HDSLoc * locator = NULL;
    size_t complen = 0;
    size_t compprec = 0;

    datFind( loc1, name, &locator, status);
    datPrec( locator, &compprec, status);
    datLen( locator, &complen, status);
    printf("%s precision: %zu length: %zu\n", name, compprec, complen);
    datAnnul(&locator, status );
}

static void traceme (const HDSLoc * loc, int *status) {
    char path_str[1024];
    char file_str[2048];
    int nlev;
    hdsTrace( loc, &nlev, path_str, file_str,
              status, sizeof(path_str),
              sizeof(file_str));
    if (*status == SAI__OK) {
      printf("File: '%s' Path: '%s' Level = %d\n", file_str,
             path_str, nlev);
    }
  }

