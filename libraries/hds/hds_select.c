/* Protect against multiple inclusion */
#ifndef STAR_HDS_H_INCLUDED
#define STAR_HDS_H_INCLUDED

#include "dat1.h"
#include "dat_par.h"
#include "hds_types.h"

#include <stdlib.h>
#include <stdio.h>
#include "sae_par.h"
#include "ems.h"
#include "hds.h"
#include "dat_err.h"
#include "star/hds_v4.h"
#include "star/hds_v5.h"
#define ISHDSv5(loc) ((loc) && (loc)->hds_version >= 5)

/*=================================*/
/* datAlter - Alter size of object */
/*=================================*/

int
datAlter(const HDSLoc *locator, int ndim, const hdsdim dims[], int *status) {

  if (ISHDSv5(locator)) return datAlter_v5(locator, ndim, dims, status);
  return datAlter_v4(locator, ndim, dims, status);

}

/*==========================*/
/* datAnnul - Annul locator */
/*==========================*/

int
datAnnul(HDSLoc **locator, int *status) {

  if (ISHDSv5(*locator)) return datAnnul_v5(locator, status);
  return datAnnul_v4(locator, status);

}

/*==============================================*/
/* datBasic - Map data (in basic machine units) */
/*==============================================*/

int
datBasic(const HDSLoc *locator, const char *mode_c, unsigned char **pntr, size_t *len, int *status) {

  if (ISHDSv5(locator)) return datBasic_v5(locator, mode_c, pntr, len, status);
  return datBasic_v4(locator, mode_c, pntr, len, status);

}

/*=====================================*/
/* datCcopy - copy one structure level */
/*=====================================*/

int
datCcopy(const HDSLoc *locator1, const HDSLoc *locator2, const char *name, HDSLoc **locator3, int *status) {
  /* Requires special code */
  if (ISHDSv5(locator1) && ISHDSv5(locator2)) {
    /* Just call the v5 code */
    datCcopy_v5( locator1, locator2, name, locator3, status );
  } else if ( !ISHDSv5(locator1) && !ISHDSv5(locator2) ) {
    datCcopy_v4( locator1, locator2, name, locator3, status );
  } else {
    printf("Aborting. datCcopy: Special code required for copy across different versions of files.\n");
    abort();
  }
  return *status;
}

/*=======================================*/
/* datCctyp - construct _CHAR*nnn string */
/*=======================================*/

void
datCctyp(size_t size, char type[DAT__SZTYP+1]) {
  return datCctyp_v5(size, type);
}


/*===========================================*/
/* datCell - Locate a "cell" (array element) */
/*===========================================*/

int
datCell(const HDSLoc *locator1, int ndim, const hdsdim subs[], HDSLoc **locator2, int *status) {

  if (ISHDSv5(locator1)) return datCell_v5(locator1, ndim, subs, locator2, status);
  return datCell_v4(locator1, ndim, subs, locator2, status);

}

/*=================================================*/
/* datChscn - validate the supplied component name */
/*=================================================*/

int
datChscn(const char * name, int *status) {
  return datChscn_v5(name, status);
}

/*==========================================*/
/* datClen - Obtain character string length */
/*==========================================*/

int
datClen(const HDSLoc *locator, size_t *clen, int *status) {

  if (ISHDSv5(locator)) return datClen_v5(locator, clen, status);
  return datClen_v4(locator, clen, status);

}

/*===========================*/
/* datClone - clone locator */
/*===========================*/

int
datClone(const HDSLoc *locator1, HDSLoc **locator2, int *status) {

  if (ISHDSv5(locator1)) return datClone_v5(locator1, locator2, status);
  return datClone_v4(locator1, locator2, status);

}

/*================================*/
/* datCoerc - coerce object shape */
/*================================*/

int
datCoerc(const HDSLoc *locator1, int ndim, HDSLoc **locator2, int *status) {

  if (ISHDSv5(locator1)) return datCoerc_v5(locator1, ndim, locator2, status);
  return datCoerc_v4(locator1, ndim, locator2, status);

}

/*=======================*/
/* datCopy - copy object */
/*=======================*/

int
datCopy(const HDSLoc *locator1, const HDSLoc *locator2, const char *name_c, int *status) {
  /* Requires special code */
  if (ISHDSv5(locator1) && ISHDSv5(locator2)) {
    /* Just call the v5 code */
    datCopy_v5( locator1, locator2, name_c, status );
  } else if ( !ISHDSv5(locator1) && !ISHDSv5(locator2) ) {
    datCopy_v4( locator1, locator2, name_c, status );
  } else {
    printf("Aborting. datCopy: Special code required for copy across different versions of files.\n");
    abort();
  }
  return *status;
}

/*============================================================*/
/* datDrep - Obtain primitive data representation information */
/*============================================================*/

int
datDrep(const HDSLoc *locator, char **format_str, char **order_str, int *status) {

  if (ISHDSv5(locator)) return datDrep_v5(locator, format_str, order_str, status);
  return datDrep_v4(locator, format_str, order_str, status);

}

/*========================================*/
/* datErase - Erase object                */
/*========================================*/

int
datErase(const HDSLoc *locator, const char *name_str, int *status) {

  if (ISHDSv5(locator)) return datErase_v5(locator, name_str, status);
  return datErase_v4(locator, name_str, status);

}

/*===========================================================*/
/* datErmsg - Translate a status value into an error message */
/*===========================================================*/

int
datErmsg(int status, size_t *len, char *msg_str) {
  return datErmsg_v5(status, len, msg_str);
}

/*================================*/
/* datFind - Find named component */
/*================================*/

int
datFind(const HDSLoc *locator1, const char *name_str, HDSLoc **locator2, int *status) {

  if (ISHDSv5(locator1)) return datFind_v5(locator1, name_str, locator2, status);
  return datFind_v4(locator1, name_str, locator2, status);

}

/*============================*/
/* datGet - Read primitive(s) */
/*============================*/

int
datGet(const HDSLoc *locator, const char *type_str, int ndim, const hdsdim dims[], void *values, int *status) {

  if (ISHDSv5(locator)) return datGet_v5(locator, type_str, ndim, dims, values, status);
  return datGet_v4(locator, type_str, ndim, dims, values, status);

}

/*===================================*/
/* datGetC - Read _CHAR primitive(s) */
/*===================================*/

int
datGetC(const HDSLoc *locator, const int ndim, const hdsdim dims[], char values[], size_t char_len, int *status) {

  if (ISHDSv5(locator)) return datGetC_v5(locator, ndim, dims, values, char_len, status);
  return datGetC_v4(locator, ndim, dims, values, char_len, status);

}

/*=====================================*/
/* datGetD - Read _DOUBLE primitive(s) */
/*=====================================*/

int
datGetD(const HDSLoc *locator, int ndim, const hdsdim dims[], double values[], int *status) {

  if (ISHDSv5(locator)) return datGetD_v5(locator, ndim, dims, values, status);
  return datGetD_v4(locator, ndim, dims, values, status);

}

/*======================================*/
/* datGetI - Read _INTEGER primitive(s) */
/*======================================*/

int
datGetI(const HDSLoc *locator, int ndim, const hdsdim dims[], int values[], int *status) {

  if (ISHDSv5(locator)) return datGetI_v5(locator, ndim, dims, values, status);
  return datGetI_v4(locator, ndim, dims, values, status);

}

/*======================================*/
/* datGetK - Read _INT64 primitive(s) */
/*======================================*/

int
datGetK(const HDSLoc *locator, int ndim, const hdsdim dims[], int64_t values[], int *status) {

  if (ISHDSv5(locator)) return datGetK_v5(locator, ndim, dims, values, status);
  return datGetK_v4(locator, ndim, dims, values, status);

}

/*===================================*/
/* datGetW - Read _WORD primitive(s) */
/*===================================*/

int
datGetW(const HDSLoc *locator, int ndim, const hdsdim dims[], short values[], int *status) {

  if (ISHDSv5(locator)) return datGetW_v5(locator, ndim, dims, values, status);
  return datGetW_v4(locator, ndim, dims, values, status);

}

/*===================================*/
/* datGetUW - Read _UWORD primitive(s) */
/*===================================*/

int
datGetUW(const HDSLoc *locator, int ndim, const hdsdim dims[], unsigned short values[], int *status) {

  if (ISHDSv5(locator)) return datGetUW_v5(locator, ndim, dims, values, status);
  return datGetUW_v4(locator, ndim, dims, values, status);

}

/*======================================*/
/* datGetL - Read _LOGICAL primitive(s) */
/*======================================*/

int
datGetL(const HDSLoc *locator, int ndim, const hdsdim dims[], hdsbool_t values[], int *status) {

  if (ISHDSv5(locator)) return datGetL_v5(locator, ndim, dims, values, status);
  return datGetL_v4(locator, ndim, dims, values, status);

}

/*===================================*/
/* datGetR - Read _REAL primitive(s) */
/*===================================*/

int
datGetR(const HDSLoc *locator, int ndim, const hdsdim dims[], float values[], int *status) {

  if (ISHDSv5(locator)) return datGetR_v5(locator, ndim, dims, values, status);
  return datGetR_v4(locator, ndim, dims, values, status);

}

/*======================================*/
/* datGet0C - Read scalar string value  */
/*======================================*/

int
datGet0C(const HDSLoc * locator, char * value, size_t len, int * status) {

  if (ISHDSv5(locator)) return datGet0C_v5(locator, value, len, status);
  return datGet0C_v4(locator, value, len, status);

}

/*======================================*/
/* datGet0D - Read scalar double value  */
/*======================================*/

int
datGet0D(const HDSLoc * locator, double * value, int * status) {

  if (ISHDSv5(locator)) return datGet0D_v5(locator, value, status);
  return datGet0D_v4(locator, value, status);

}

/*=====================================*/
/* datGet0R - Read scalar float value  */
/*=====================================*/

int
datGet0R(const HDSLoc * locator, float * value, int * status) {

  if (ISHDSv5(locator)) return datGet0R_v5(locator, value, status);
  return datGet0R_v4(locator, value, status);

}

/*=======================================*/
/* datGet0I - Read scalar integer value  */
/*=======================================*/

int
datGet0I(const HDSLoc * locator, int * value, int * status) {

  if (ISHDSv5(locator)) return datGet0I_v5(locator, value, status);
  return datGet0I_v4(locator, value, status);

}

/*================================================*/
/* datGet0K - Read scalar 64-bit integer value  */
/*================================================*/

int
datGet0K(const HDSLoc * locator, int64_t * value, int * status) {

  if (ISHDSv5(locator)) return datGet0K_v5(locator, value, status);
  return datGet0K_v4(locator, value, status);

}

/*=============================================*/
/* datGet0W - Read scalar short integer value  */
/*=============================================*/

int
datGet0W(const HDSLoc * locator, short * value, int * status) {

  if (ISHDSv5(locator)) return datGet0W_v5(locator, value, status);
  return datGet0W_v4(locator, value, status);

}

/*=============================================*/
/* datGet0UW - Read scalar unsigned short integer value  */
/*=============================================*/

int
datGet0UW(const HDSLoc * locator, unsigned short * value, int * status) {

  if (ISHDSv5(locator)) return datGet0UW_v5(locator, value, status);
  return datGet0UW_v4(locator, value, status);

}

/*=======================================*/
/* datGet0L - Read scalar logical value  */
/*=======================================*/

int
datGet0L(const HDSLoc * locator, hdsbool_t * value, int * status) {

  if (ISHDSv5(locator)) return datGet0L_v5(locator, value, status);
  return datGet0L_v4(locator, value, status);

}

/*==================================*/
/* DAT_GET1C - Read 1D string array */
/*==================================*/

int
datGet1C(const HDSLoc * locator, size_t maxval, size_t bufsize, char *buffer, char *pntrs[], size_t * actval, int * status) {

  if (ISHDSv5(locator)) return datGet1C_v5(locator, maxval, bufsize, buffer, pntrs, actval, status);
  return datGet1C_v4(locator, maxval, bufsize, buffer, pntrs, actval, status);

}

/*==================================*/
/* DAT_GET1D - Read 1D Double array */
/*==================================*/

int
datGet1D(const HDSLoc * locator, size_t maxval, double values[], size_t *actval, int * status) {

  if (ISHDSv5(locator)) return datGet1D_v5(locator, maxval, values, actval, status);
  return datGet1D_v4(locator, maxval, values, actval, status);

}

/*==================================*/
/* DAT_GET1I - Read 1D Integer array */
/*==================================*/

int
datGet1I(const HDSLoc * locator, size_t maxval, int values[], size_t *actval, int * status) {

  if (ISHDSv5(locator)) return datGet1I_v5(locator, maxval, values, actval, status);
  return datGet1I_v4(locator, maxval, values, actval, status);

}

/*============================================*/
/* DAT_GET1K - Read 1D 64-bit Integer array */
/*============================================*/

int
datGet1K(const HDSLoc * locator, size_t maxval, int64_t values[], size_t *actval, int * status) {

  if (ISHDSv5(locator)) return datGet1K_v5(locator, maxval, values, actval, status);
  return datGet1K_v4(locator, maxval, values, actval, status);

}

/*=========================================*/
/* DAT_GET1W - Read 1D Short Integer array */
/*=========================================*/

int
datGet1W(const HDSLoc * locator, size_t maxval, short values[], size_t *actval, int * status) {

  if (ISHDSv5(locator)) return datGet1W_v5(locator, maxval, values, actval, status);
  return datGet1W_v4(locator, maxval, values, actval, status);

}

/*===================================================*/
/* DAT_GET1UW - Read 1D Unsigned Short Integer array */
/*===================================================*/

int
datGet1UW(const HDSLoc * locator, size_t maxval, unsigned short values[], size_t *actval, int * status) {

  if (ISHDSv5(locator)) return datGet1UW_v5(locator, maxval, values, actval, status);
  return datGet1UW_v4(locator, maxval, values, actval, status);

}

/*==================================*/
/* DAT_GET1R - Read 1D REAL array */
/*==================================*/

int
datGet1R(const HDSLoc * locator, size_t maxval, float values[], size_t *actval, int * status) {

  if (ISHDSv5(locator)) return datGet1R_v5(locator, maxval, values, actval, status);
  return datGet1R_v4(locator, maxval, values, actval, status);

}

/*==================================*/
/* DAT_GET1L - Read 1D Logical array */
/*==================================*/

int
datGet1L(const HDSLoc * locator, size_t maxval, hdsbool_t values[], size_t *actval, int * status) {

  if (ISHDSv5(locator)) return datGet1L_v5(locator, maxval, values, actval, status);
  return datGet1L_v4(locator, maxval, values, actval, status);

}

/*==================================*/
/* DAT_GETVC - Read vectorized 1D string array */
/*==================================*/

int
datGetVC(const HDSLoc * locator, size_t maxval, size_t bufsize, char *buffer, char *pntrs[], size_t * actval, int * status) {

  if (ISHDSv5(locator)) return datGetVC_v5(locator, maxval, bufsize, buffer, pntrs, actval, status);
  return datGetVC_v4(locator, maxval, bufsize, buffer, pntrs, actval, status);

}


/*==========================================*/
/* DAT_GETVD - Read vectorized Double array */
/*==========================================*/

int
datGetVD(const HDSLoc * locator, size_t maxval, double values[], size_t *actval, int * status) {

  if (ISHDSv5(locator)) return datGetVD_v5(locator, maxval, values, actval, status);
  return datGetVD_v4(locator, maxval, values, actval, status);

}

/*==========================================*/
/* DAT_GETVI - Read vectorized Integer array */
/*==========================================*/

int
datGetVI(const HDSLoc * locator, size_t maxval, int values[], size_t *actval, int * status) {

  if (ISHDSv5(locator)) return datGetVI_v5(locator, maxval, values, actval, status);
  return datGetVI_v4(locator, maxval, values, actval, status);

}

/*==========================================*/
/* DAT_GETVK - Read vectorized Int64 array */
/*==========================================*/

int
datGetVK(const HDSLoc * locator, size_t maxval, int64_t values[], size_t *actval, int * status) {

  if (ISHDSv5(locator)) return datGetVK_v5(locator, maxval, values, actval, status);
  return datGetVK_v4(locator, maxval, values, actval, status);

}

/*==========================================*/
/* DAT_GETVR - Read vectorized REAL array */
/*==========================================*/

int
datGetVR(const HDSLoc * locator, size_t maxval, float values[], size_t *actval, int * status) {

  if (ISHDSv5(locator)) return datGetVR_v5(locator, maxval, values, actval, status);
  return datGetVR_v4(locator, maxval, values, actval, status);

}

/*==========================================*/
/* DAT_GETVL - Read vectorized Logical array */
/*==========================================*/

int
datGetVL(const HDSLoc * locator, size_t maxval, hdsbool_t values[], size_t *actval, int * status) {

  if (ISHDSv5(locator)) return datGetVL_v5(locator, maxval, values, actval, status);
  return datGetVL_v4(locator, maxval, values, actval, status);

}


/*======================================*/
/* datIndex - Index into component list */
/*======================================*/

int
datIndex(const HDSLoc *locator1, int index, HDSLoc **locator2, int *status) {

  if (ISHDSv5(locator1)) return datIndex_v5(locator1, index, locator2, status);
  return datIndex_v4(locator1, index, locator2, status);

}

/*===================================*/
/* datLen - Inquire primitive length */
/*===================================*/

int
datLen(const HDSLoc *locator, size_t *len, int *status) {

  if (ISHDSv5(locator)) return datLen_v5(locator, len, status);
  return datLen_v4(locator, len, status);

}

/*===========================*/
/* datMap - Map primitive(s) */
/*===========================*/

int
datMap(const HDSLoc *locator, const char *type_str, const char *mode_str, int ndim, const hdsdim dims[], void **pntr, int *status) {

  if (ISHDSv5(locator)) return datMap_v5(locator, type_str, mode_str, ndim, dims, pntr, status);
  return datMap_v4(locator, type_str, mode_str, ndim, dims, pntr, status);

}

/*==================================*/
/* datMapC - Map _CHAR primitive(s) */
/*==================================*/

int
datMapC(const HDSLoc *locator, const char *mode_str, int ndim, const hdsdim dims[], unsigned char **pntr, int *status) {

  if (ISHDSv5(locator)) return datMapC_v5(locator, mode_str, ndim, dims, pntr, status);
  return datMapC_v4(locator, mode_str, ndim, dims, pntr, status);

}

/*====================================*/
/* datMapD - Map _DOUBLE primitive(s) */
/*====================================*/

int
datMapD(const HDSLoc *locator, const char *mode_str, int ndim, const hdsdim dims[], double **pntr, int *status) {

  if (ISHDSv5(locator)) return datMapD_v5(locator, mode_str, ndim, dims, pntr, status);
  return datMapD_v4(locator, mode_str, ndim, dims, pntr, status);

}

/*=====================================*/
/* datMapI - Map _INTEGER primitive(s) */
/*=====================================*/

int
datMapI(const HDSLoc *locator, const char *mode_str, int ndim, const hdsdim dims[], int **pntr, int *status) {

  if (ISHDSv5(locator)) return datMapI_v5(locator, mode_str, ndim, dims, pntr, status);
  return datMapI_v4(locator, mode_str, ndim, dims, pntr, status);

}

/*=====================================*/
/* datMapK - Map _INT64 primitive(s) */
/*=====================================*/

int
datMapK(const HDSLoc *locator, const char *mode_str, int ndim, const hdsdim dims[], int **pntr, int *status) {

  if (ISHDSv5(locator)) return datMapK_v5(locator, mode_str, ndim, dims, pntr, status);
  return datMapK_v4(locator, mode_str, ndim, dims, pntr, status);

}

/*=====================================*/
/* datMapL - Map _LOGICAL primitive(s) */
/*=====================================*/

int
datMapL(const HDSLoc *locator, const char *mode_str, int ndim, const hdsdim dims[], hdsbool_t **pntr, int *status) {

  if (ISHDSv5(locator)) return datMapL_v5(locator, mode_str, ndim, dims, pntr, status);
  return datMapL_v4(locator, mode_str, ndim, dims, pntr, status);

}

/*==================================*/
/* datMapR - Map _REAL primitive(s) */
/*==================================*/

int
datMapR(const HDSLoc *locator, const char *mode_str, int ndim, const hdsdim dims[], float **pntr, int *status) {

  if (ISHDSv5(locator)) return datMapR_v5(locator, mode_str, ndim, dims, pntr, status);
  return datMapR_v4(locator, mode_str, ndim, dims, pntr, status);

}


/*========================================*/
/* datMapN - Map primitive as N-dim array */
/*========================================*/

int
datMapN(const HDSLoc *locator, const char *type_str, const char *mode_str, int ndim, void **pntr, hdsdim dims[], int *status) {

  if (ISHDSv5(locator)) return datMapN_v5(locator, type_str, mode_str, ndim, pntr, dims, status);
  return datMapN_v4(locator, type_str, mode_str, ndim, pntr, dims, status);

}

/*==================================*/
/* datMapV - Map vectorized primitive(s) */
/*==================================*/

int
datMapV(const HDSLoc *locator, const char *type_str, const char *mode_str, void **pntr, size_t *actval, int *status) {

  if (ISHDSv5(locator)) return datMapV_v5(locator, type_str, mode_str, pntr, actval, status);
  return datMapV_v4(locator, type_str, mode_str, pntr, actval, status);

}


/*==================================*/
/* datMould - Alter shape of object */
/*==================================*/

int
datMould(const HDSLoc *locator, int ndim, const hdsdim dims[], int *status) {

  if (ISHDSv5(locator)) return datMould_v5(locator, ndim, dims, status);
  return datMould_v4(locator, ndim, dims, status);

}

/*=======================*/
/* datMove - Move object */
/*=======================*/

int
datMove(HDSLoc **locator1, const HDSLoc *locator2, const char *name_str, int *status) {
  /* Requires special code */
  if (ISHDSv5(*locator1) && ISHDSv5(locator2)) {
    /* Just call the v5 code */
    datMove_v5( locator1, locator2, name_str, status );
  } else if ( !ISHDSv5(*locator1) && !ISHDSv5(locator2) ) {
    datMove_v4( locator1, locator2, name_str, status );
  } else {
    printf("Aborting. datMove: Special code required for copy across different versions of files.\n");
    abort();
  }
  return *status;
}

/*======================================*/
/* datMsg - store filename in EMS token */
/*======================================*/

void
datMsg(const char * token, const HDSLoc * locator) {

  if (ISHDSv5(locator)) return datMsg_v5(token, locator);
  return datMsg_v4(token, locator);

}

/*===============================*/
/* datName - Enquire object name */
/*===============================*/

int
datName(const HDSLoc *locator, char name_str[DAT__SZNAM+1], int *status) {

  if (ISHDSv5(locator)) return datName_v5(locator, name_str, status);
  return datName_v4(locator, name_str, status);

}

/*=========================================*/
/* datNcomp - Inquire number of components */
/*=========================================*/

int
datNcomp(const HDSLoc *locator, int *ncomp, int *status) {

  if (ISHDSv5(locator)) return datNcomp_v5(locator, ncomp, status);
  return datNcomp_v4(locator, ncomp, status);

}

/*===============================*/
/* datNew - Create new component */
/*===============================*/

int
datNew(const HDSLoc *locator, const char *name_str, const char *type_str, int ndim, const hdsdim dims[], int *status) {

  if (ISHDSv5(locator)) return datNew_v5(locator, name_str, type_str, ndim, dims, status);
  return datNew_v4(locator, name_str, type_str, ndim, dims, status);

}

/*============================================*/
/* datNewC - Create new _CHAR type component */
/*============================================*/

int
datNewC(const HDSLoc *locator, const char *name_str, size_t len, int ndim, const hdsdim dims[], int *status) {

  if (ISHDSv5(locator)) return datNewC_v5(locator, name_str, len, ndim, dims, status);
  return datNewC_v4(locator, name_str, len, ndim, dims, status);

}

/*=======================================*/
/* datNew0 - Create new scalar component */
/*=======================================*/

int
datNew0(const HDSLoc *locator, const char *name_str, const char *type_str, int *status) {

  if (ISHDSv5(locator)) return datNew0_v5(locator, name_str, type_str, status);
  return datNew0_v4(locator, name_str, type_str, status);

}

/*===============================================*/
/* datNew0D - Create new scalar double component */
/*===============================================*/

int
datNew0D(const HDSLoc *locator, const char *name_str, int *status) {

  if (ISHDSv5(locator)) return datNew0D_v5(locator, name_str, status);
  return datNew0D_v4(locator, name_str, status);

}

/*================================================*/
/* datNew0I - Create new scalar integer component */
/*================================================*/

int
datNew0I(const HDSLoc *locator, const char *name_str, int *status) {

  if (ISHDSv5(locator)) return datNew0I_v5(locator, name_str, status);
  return datNew0I_v4(locator, name_str, status);

}

/*=========================================================*/
/* datNew0K - Create new scalar 64-bit integer component */
/*=========================================================*/

int
datNew0K(const HDSLoc *locator, const char *name_str, int *status) {

  if (ISHDSv5(locator)) return datNew0K_v5(locator, name_str, status);
  return datNew0K_v4(locator, name_str, status);

}

/*======================================================*/
/* datNew0W - Create new scalar short integer component */
/*======================================================*/

int
datNew0W(const HDSLoc *locator, const char *name_str, int *status) {

  if (ISHDSv5(locator)) return datNew0W_v5(locator, name_str, status);
  return datNew0W_v4(locator, name_str, status);

}

/*================================================================*/
/* datNew0UW - Create new scalar unsigned short integer component */
/*================================================================*/

int
datNew0UW(const HDSLoc *locator, const char *name_str, int *status) {

  if (ISHDSv5(locator)) return datNew0UW_v5(locator, name_str, status);
  return datNew0UW_v4(locator, name_str, status);

}

/*=============================================*/
/* datNew0R - Create new scalar real component */
/*=============================================*/

int
datNew0R(const HDSLoc *locator, const char *name_str, int *status) {

  if (ISHDSv5(locator)) return datNew0R_v5(locator, name_str, status);
  return datNew0R_v4(locator, name_str, status);

}

/*================================================*/
/* datNew0L - Create new scalar logical component */
/*================================================*/

int
datNew0L(const HDSLoc *locator, const char *name_str, int *status) {

  if (ISHDSv5(locator)) return datNew0L_v5(locator, name_str, status);
  return datNew0L_v4(locator, name_str, status);

}

/*================================================*/
/* datNew0L - Create new scalar logical component */
/*================================================*/

int
datNew0C(const HDSLoc *locator, const char *name_str, size_t len, int *status) {

  if (ISHDSv5(locator)) return datNew0C_v5(locator, name_str, len, status);
  return datNew0C_v4(locator, name_str, len, status);

}



/*=======================================*/
/* datNew1 - Create new vector component */
/*=======================================*/

int
datNew1(const HDSLoc *locator, const char *name_str, const char *type_str, size_t len, int *status) {

  if (ISHDSv5(locator)) return datNew1_v5(locator, name_str, type_str, len, status);
  return datNew1_v4(locator, name_str, type_str, len, status);

}

/*=======================================*/
/* datNew1C - Create new vector string  */
/*=======================================*/

int
datNew1C(const HDSLoc *locator, const char *name_str, size_t len, size_t nelem, int *status) {

  if (ISHDSv5(locator)) return datNew1C_v5(locator, name_str, len, nelem, status);
  return datNew1C_v4(locator, name_str, len, nelem, status);

}

/*=======================================*/
/* datNew1d - Create new vector double   */
/*=======================================*/

int
datNew1D(const HDSLoc *locator, const char *name_str, size_t len, int *status) {

  if (ISHDSv5(locator)) return datNew1D_v5(locator, name_str, len, status);
  return datNew1D_v4(locator, name_str, len, status);

}

/*=======================================*/
/* datNew1I - Create new vector integer  */
/*=======================================*/

int
datNew1I(const HDSLoc *locator, const char *name_str, size_t len, int *status) {

  if (ISHDSv5(locator)) return datNew1I_v5(locator, name_str, len, status);
  return datNew1I_v4(locator, name_str, len, status);

}

/*================================================*/
/* datNew1K - Create new vector 64-bit integer  */
/*================================================*/

int
datNew1K(const HDSLoc *locator, const char *name_str, size_t len, int *status) {

  if (ISHDSv5(locator)) return datNew1K_v5(locator, name_str, len, status);
  return datNew1K_v4(locator, name_str, len, status);

}

/*=============================================*/
/* datNew1W - Create new vector short integer  */
/*=============================================*/

int
datNew1W(const HDSLoc *locator, const char *name_str, size_t len, int *status) {

  if (ISHDSv5(locator)) return datNew1W_v5(locator, name_str, len, status);
  return datNew1W_v4(locator, name_str, len, status);

}

/*=======================================================*/
/* datNew1UW - Create new vector unsigned short integer  */
/*=======================================================*/

int
datNew1UW(const HDSLoc *locator, const char *name_str, size_t len, int *status) {

  if (ISHDSv5(locator)) return datNew1UW_v5(locator, name_str, len, status);
  return datNew1UW_v4(locator, name_str, len, status);

}

/*=======================================*/
/* datNew1L - Create new vector logical   */
/*=======================================*/

int
datNew1L(const HDSLoc *locator, const char *name_str, size_t len, int *status) {

  if (ISHDSv5(locator)) return datNew1L_v5(locator, name_str, len, status);
  return datNew1L_v4(locator, name_str, len, status);

}

/*=======================================*/
/* datNew1R - Create new vector float   */
/*=======================================*/

int
datNew1R(const HDSLoc *locator, const char *name_str, size_t len, int *status) {

  if (ISHDSv5(locator)) return datNew1R_v5(locator, name_str, len, status);
  return datNew1R_v4(locator, name_str, len, status);

}

/*====================================*/
/* datParen - Locate parent structure */
/*====================================*/

int
datParen(const HDSLoc *locator1, HDSLoc **locator2, int *status) {

  if (ISHDSv5(locator1)) return datParen_v5(locator1, locator2, status);
  return datParen_v4(locator1, locator2, status);

}


/*=====================================*/
/* datPrec - Enquire storage precision */
/*=====================================*/

int
datPrec(const HDSLoc *locator, size_t *nbytes, int *status) {

  if (ISHDSv5(locator)) return datPrec_v5(locator, nbytes, status);
  return datPrec_v4(locator, nbytes, status);

}

/*====================================*/
/* datPrim - Enquire object primitive */
/*====================================*/

int
datPrim(const HDSLoc *locator, hdsbool_t *prim, int *status) {

  if (ISHDSv5(locator)) return datPrim_v5(locator, prim, status);
  return datPrim_v4(locator, prim, status);

}

/*=========================================================*/
/* datPrmry - Set/Enquire primary/secondary locator status */
/*=========================================================*/

int
datPrmry(int set, HDSLoc **locator, hdsbool_t *prmry, int *status) {

  if (ISHDSv5(*locator)) return datPrmry_v5(set, locator, prmry, status);
  return datPrmry_v4(set, locator, prmry, status);

}

/*==================================*/
/* datPutC - Write _CHAR primitive */
/*==================================*/

int
datPutC(const HDSLoc *locator, int ndim, const hdsdim dims[], const char string[], size_t string_length, int *status) {

  if (ISHDSv5(locator)) return datPutC_v5(locator, ndim, dims, string, string_length, status);
  return datPutC_v4(locator, ndim, dims, string, string_length, status);

}

/*====================================*/
/* datPutD - Write _DOUBLE primitives */
/*====================================*/

int
datPutD(const HDSLoc *locator, int ndim, const hdsdim dims[], const double values[], int *status) {

  if (ISHDSv5(locator)) return datPutD_v5(locator, ndim, dims, values, status);
  return datPutD_v4(locator, ndim, dims, values, status);

}

/*=====================================*/
/* datPutI - Write _INTEGER primitives */
/*=====================================*/

int
datPutI(const HDSLoc *locator, int ndim, const hdsdim dims[], const int values[], int *status) {

  if (ISHDSv5(locator)) return datPutI_v5(locator, ndim, dims, values, status);
  return datPutI_v4(locator, ndim, dims, values, status);

}

/*=====================================*/
/* datPutK - Write _INT64 primitives */
/*=====================================*/

int
datPutK(const HDSLoc *locator, int ndim, const hdsdim dims[], const int64_t values[], int *status) {

  if (ISHDSv5(locator)) return datPutK_v5(locator, ndim, dims, values, status);
  return datPutK_v4(locator, ndim, dims, values, status);

}

/*=====================================*/
/* datPutW - Write _WORD primitives */
/*=====================================*/

int
datPutW(const HDSLoc *locator, int ndim, const hdsdim dims[], const short values[], int *status) {

  if (ISHDSv5(locator)) return datPutW_v5(locator, ndim, dims, values, status);
  return datPutW_v4(locator, ndim, dims, values, status);

}

/*====================================*/
/* datPutUW - Write _UWORD primitives */
/*====================================*/

int
datPutUW(const HDSLoc *locator, int ndim, const hdsdim dims[], const unsigned short values[], int *status) {

  if (ISHDSv5(locator)) return datPutUW_v5(locator, ndim, dims, values, status);
  return datPutUW_v4(locator, ndim, dims, values, status);

}

/*==================================*/
/* datPutR - Write _REAL primitives */
/*==================================*/

int
datPutR(const HDSLoc *locator, int ndim, const hdsdim dims[], const float values[], int *status) {

  if (ISHDSv5(locator)) return datPutR_v5(locator, ndim, dims, values, status);
  return datPutR_v4(locator, ndim, dims, values, status);

}

/*=====================================*/
/* datPutL - Write _LOGICAL primitives */
/*=====================================*/

int
datPutL(const HDSLoc *locator, int ndim, const hdsdim dims[], const hdsbool_t values[], int *status) {

  if (ISHDSv5(locator)) return datPutL_v5(locator, ndim, dims, values, status);
  return datPutL_v4(locator, ndim, dims, values, status);

}

/*==========================*/
/* datPut - Write primitive */
/*==========================*/

int
datPut(const HDSLoc *locator, const char *type_str, int ndim, const hdsdim dims[], const void *values, int *status) {

  if (ISHDSv5(locator)) return datPut_v5(locator, type_str, ndim, dims, values, status);
  return datPut_v4(locator, type_str, ndim, dims, values, status);

}

/*=======================================*/
/* datPut0C - Write scalar string value  */
/*=======================================*/

int
datPut0C(const HDSLoc * locator, const char * value, int * status) {

  if (ISHDSv5(locator)) return datPut0C_v5(locator, value, status);
  return datPut0C_v4(locator, value, status);

}

/*=======================================*/
/* datPut0D - Write scalar double value  */
/*=======================================*/

int
datPut0D(const HDSLoc * locator, double value, int * status) {

  if (ISHDSv5(locator)) return datPut0D_v5(locator, value, status);
  return datPut0D_v4(locator, value, status);

}

/*======================================*/
/* datPut0R - Write scalar float value  */
/*======================================*/

int
datPut0R(const HDSLoc * locator, float value, int * status) {

  if (ISHDSv5(locator)) return datPut0R_v5(locator, value, status);
  return datPut0R_v4(locator, value, status);

}

/*========================================*/
/* datPut0I - Write scalar integer value  */
/*========================================*/

int
datPut0I(const HDSLoc * locator, int value, int * status) {

  if (ISHDSv5(locator)) return datPut0I_v5(locator, value, status);
  return datPut0I_v4(locator, value, status);

}

/*========================================*/
/* datPut0I - Write scalar 64-bit integer value  */
/*========================================*/

int
datPut0K(const HDSLoc * locator, int64_t value, int * status) {

  if (ISHDSv5(locator)) return datPut0K_v5(locator, value, status);
  return datPut0K_v4(locator, value, status);

}

/*==============================================*/
/* datPut0W - Write scalar short integer value  */
/*===============================================*/

int
datPut0W(const HDSLoc * locator, short value, int * status) {

  if (ISHDSv5(locator)) return datPut0W_v5(locator, value, status);
  return datPut0W_v4(locator, value, status);

}

/*========================================================*/
/* datPut0UW - Write scalar unsigned short integer value  */
/*========================================================*/

int
datPut0UW(const HDSLoc * locator, unsigned short value, int * status) {

  if (ISHDSv5(locator)) return datPut0UW_v5(locator, value, status);
  return datPut0UW_v4(locator, value, status);

}

/*========================================*/
/* datPut0L - Write scalar logical value  */
/*========================================*/

int
datPut0L(const HDSLoc * locator, hdsbool_t value, int * status) {

  if (ISHDSv5(locator)) return datPut0L_v5(locator, value, status);
  return datPut0L_v4(locator, value, status);

}

/*========================================*/
/* datPut1C - Write 1D character array       */
/*========================================*/

int
datPut1C(const HDSLoc * locator, size_t nval, const char *values[], int * status) {

  if (ISHDSv5(locator)) return datPut1C_v5(locator, nval, values, status);
  return datPut1C_v4(locator, nval, values, status);

}

/*========================================*/
/* datPut1D - Write 1D double array       */
/*========================================*/

int
datPut1D(const HDSLoc * locator, size_t nval, const double values[], int * status) {

  if (ISHDSv5(locator)) return datPut1D_v5(locator, nval, values, status);
  return datPut1D_v4(locator, nval, values, status);

}

/*========================================*/
/* datPut1I - Write 1D int array       */
/*========================================*/

int
datPut1I(const HDSLoc * locator, size_t nval, const int values[], int * status) {

  if (ISHDSv5(locator)) return datPut1I_v5(locator, nval, values, status);
  return datPut1I_v4(locator, nval, values, status);

}

/*========================================*/
/* datPut1K - Write 1D 64-bit int array */
/*========================================*/

int
datPut1K(const HDSLoc * locator, size_t nval, const int64_t values[], int * status) {

  if (ISHDSv5(locator)) return datPut1K_v5(locator, nval, values, status);
  return datPut1K_v4(locator, nval, values, status);

}

/*===========================================*/
/* datPut1W - Write 1D short int array       */
/*===========================================*/

int
datPut1W(const HDSLoc * locator, size_t nval, const short values[], int * status) {

  if (ISHDSv5(locator)) return datPut1W_v5(locator, nval, values, status);
  return datPut1W_v4(locator, nval, values, status);

}

/*===============================================*/
/* datPut1UW - Write 1D unsigned short int array */
/*===============================================*/

int
datPut1UW(const HDSLoc * locator, size_t nval, const unsigned short values[], int * status) {

  if (ISHDSv5(locator)) return datPut1UW_v5(locator, nval, values, status);
  return datPut1UW_v4(locator, nval, values, status);

}

/*========================================*/
/* datPut1R - Write 1D double array       */
/*========================================*/

int
datPut1R(const HDSLoc * locator, size_t nval, const float values[], int * status) {

  if (ISHDSv5(locator)) return datPut1R_v5(locator, nval, values, status);
  return datPut1R_v4(locator, nval, values, status);

}

/*========================================*/
/* datPut1L - Write 1D Logical/int array       */
/*========================================*/

int
datPut1L(const HDSLoc * locator, size_t nval, const hdsbool_t values[], int * status) {

  if (ISHDSv5(locator)) return datPut1L_v5(locator, nval, values, status);
  return datPut1L_v4(locator, nval, values, status);

}

/*================================================*/
/* datPutVD - Write vectorized double array       */
/*================================================*/

int
datPutVD(const HDSLoc * locator, size_t nval, const double values[], int * status) {

  if (ISHDSv5(locator)) return datPutVD_v5(locator, nval, values, status);
  return datPutVD_v4(locator, nval, values, status);

}

/*================================================*/
/* datPutVI - Write vectorized int array       */
/*================================================*/

int
datPutVI(const HDSLoc * locator, size_t nval, const int values[], int * status) {

  if (ISHDSv5(locator)) return datPutVI_v5(locator, nval, values, status);
  return datPutVI_v4(locator, nval, values, status);

}

/*================================================*/
/* datPutVI - Write vectorized int64 array       */
/*================================================*/

int
datPutVK(const HDSLoc * locator, size_t nval, const int64_t values[], int * status) {

  if (ISHDSv5(locator)) return datPutVK_v5(locator, nval, values, status);
  return datPutVK_v4(locator, nval, values, status);

}

/*================================================*/
/* datPutVR - Write vectorized REAL/float array       */
/*================================================*/

int
datPutVR(const HDSLoc * locator, size_t nval, const float values[], int * status) {

  if (ISHDSv5(locator)) return datPutVR_v5(locator, nval, values, status);
  return datPutVR_v4(locator, nval, values, status);

}

/*================================================*/
/* datPutVL - Write vectorized Logical array       */
/*================================================*/

int
datPutVL(const HDSLoc * locator, size_t nval, const hdsbool_t values[], int * status) {

  if (ISHDSv5(locator)) return datPutVL_v5(locator, nval, values, status);
  return datPutVL_v4(locator, nval, values, status);

}

/*================================================*/
/* datPutVC - Write vectorized character array       */
/*================================================*/

int
datPutVC(const HDSLoc * locator, size_t nval, const char *values[], int * status) {

  if (ISHDSv5(locator)) return datPutVC_v5(locator, nval, values, status);
  return datPutVC_v4(locator, nval, values, status);

}


/*========================================*/
/* datRef - Enquire object reference name */
/*========================================*/

int
datRef(const HDSLoc * locator, char * ref, size_t reflen, int *status) {

  if (ISHDSv5(locator)) return datRef_v5(locator, ref, reflen, status);
  return datRef_v4(locator, ref, reflen, status);

}

/*===================================================*/
/* datRefct - Enquire container file reference count */
/*===================================================*/

int
datRefct(const HDSLoc *locator, int *refct, int *status) {

  if (ISHDSv5(locator)) return datRefct_v5(locator, refct, status);
  return datRefct_v4(locator, refct, status);

}

/*=============================*/
/* datRenam - Rename an object */
/*=============================*/

int
datRenam(const HDSLoc *locator, const char *name_str, int *status) {

  if (ISHDSv5(locator)) return datRenam_v5(locator, name_str, status);
  return datRenam_v4(locator, name_str, status);

}

/*================================*/
/* datReset - Reset object state */
/*================================*/

int
datReset(const HDSLoc *locator, int *status) {

  if (ISHDSv5(locator)) return datReset_v5(locator, status);
  return datReset_v4(locator, status);

}

/*================================*/
/* datRetyp - Change object type */
/*================================*/

int
datRetyp(const HDSLoc *locator, const char *type_str, int *status) {

  if (ISHDSv5(locator)) return datRetyp_v5(locator, type_str, status);
  return datRetyp_v4(locator, type_str, status);

}

/*=================================*/
/* datShape - Enquire object shape */
/*=================================*/

int
datShape(const HDSLoc *locator, int maxdim, hdsdim dims[], int *actdim, int *status) {

  if (ISHDSv5(locator)) return datShape_v5(locator, maxdim, dims, actdim, status);
  return datShape_v4(locator, maxdim, dims, actdim, status);

}

/*===============================*/
/* datSize - Enquire object size */
/*===============================*/

int
datSize(const HDSLoc *locator, size_t *size, int *status) {

  if (ISHDSv5(locator)) return datSize_v5(locator, size, status);
  return datSize_v4(locator, size, status);

}

/*================================*/
/* datSlice - Locate object slice */
/*================================*/

int
datSlice(const HDSLoc *locator1, int ndim, const hdsdim lower[], const hdsdim upper[], HDSLoc **locator2, int *status) {

  if (ISHDSv5(locator1)) return datSlice_v5(locator1, ndim, lower, upper, locator2, status);
  return datSlice_v4(locator1, ndim, lower, upper, locator2, status);

}

/*=================================*/
/* datState - Enquire object state */
/*=================================*/

int
datState(const HDSLoc *locator, hdsbool_t *state, int *status) {

  if (ISHDSv5(locator)) return datState_v5(locator, state, status);
  return datState_v4(locator, state, status);

}

/*=====================================*/
/* datStruc - Enquire object structure */
/*=====================================*/

int
datStruc(const HDSLoc *locator, hdsbool_t *struc, int *status) {

  if (ISHDSv5(locator)) return datStruc_v5(locator, struc, status);
  return datStruc_v4(locator, struc, status);

}

/*===================================*/
/* datTemp - Create temporary object */
/*===================================*/

int
datTemp(const char *type_str, int ndim, const hdsdim dims[], HDSLoc **locator, int *status) {
  return datTemp_v5(type_str, ndim, dims, locator, status);
}

/*=========================================*/
/* datThere - Enquire component existence */
/*=========================================*/

int
datThere(const HDSLoc *locator, const char *name_c, hdsbool_t *there, int *status) {

  if (ISHDSv5(locator)) return datThere_v5(locator, name_c, there, status);
  return datThere_v4(locator, name_c, there, status);

}

/*===============================*/
/* datType - Enquire object type */
/*===============================*/

int
datType(const HDSLoc *locator, char type_str[DAT__SZTYP + 1], int *status) {

  if (ISHDSv5(locator)) return datType_v5(locator, type_str, status);
  return datType_v4(locator, type_str, status);

}

/*=========================*/
/* datUnmap - Unmap object */
/*=========================*/

int
datUnmap(const HDSLoc *locator, int *status) {

  if (ISHDSv5(locator)) return datUnmap_v5(locator, status);
  return datUnmap_v4(locator, status);

}

/*==================================*/
/* datValid - Enquire locator valid */
/*==================================*/

int
datValid(const HDSLoc *locator, hdsbool_t *valid, int *status) {

  if (ISHDSv5(locator)) return datValid_v5(locator, valid, status);
  return datValid_v4(locator, valid, status);

}

/*===========================*/
/* datVec - Vectorise object */
/*===========================*/

int
datVec(const HDSLoc *locator1, HDSLoc **locator2, int *status) {

  if (ISHDSv5(locator1)) return datVec_v5(locator1, locator2, status);
  return datVec_v4(locator1, locator2, status);

}

/*================================================*/
/* datWhere - Find primitive position in HDS file */
/*            Currently not part of the public    */
/*            C API                               */
/*================================================*/

/*==================================================*/
/* hdsCopy - Copy an object to a new container file */
/*==================================================*/

int
hdsCopy(const HDSLoc *locator, const char *file_str, const char name_str[DAT__SZNAM], int *status) {

  if (ISHDSv5(locator)) return hdsCopy_v5(locator, file_str, name_str, status);
  return hdsCopy_v4(locator, file_str, name_str, status);

}

/*=================================*/
/* hdsErase - Erase container file */
/*=================================*/

int
hdsErase(HDSLoc **locator, int *status) {

  if (ISHDSv5(*locator)) return hdsErase_v5(locator, status);
  return hdsErase_v4(locator, status);

}

/*===============================================================*/
/* hdsEwild - End a wild card search for HDS container files     */
/*===============================================================*/

int
hdsEwild(int *iwld, int *status) {
  /* Requires special code */
  printf("Aborting. Special code required in: %s\n", "hdsEwild(iwld, status);");
  abort();
  return *status;
}

/*================================*/
/* hdsFlush - Flush locator group */
/*=================================*/

int
hdsFlush(const char *group_str, int *status) {
  /* We are only allowed to flush a group that actually exists */
  if (*status != SAI__OK) return *status;

  /* We need a new API that will let us query whether a group
     exists before we try to flush it. _v5 triggers an error
     if the group doesn't exist but v4 does not trigger such an error.
     For now we catch the specific error from v5 and assume that means
     v4 will deal with it. */
  hdsFlush_v5(group_str, status);
  if (*status == DAT__GRPIN) emsAnnul(status);
  hdsFlush_v4(group_str, status);

  return *status;
}

/*===============================*/
/* hdsFree - Free container file */
/*===============================*/

int
hdsFree(const HDSLoc *locator, int *status) {

  if (ISHDSv5(locator)) return hdsFree_v5(locator, status);
  return hdsFree_v4(locator, status);

}

/*==================================*/
/* hdsGroup - Enquire locator group */
/*==================================*/

int
hdsGroup(const HDSLoc *locator, char group_str[DAT__SZGRP+1], int *status) {

  if (ISHDSv5(locator)) return hdsGroup_v5(locator, group_str, status);
  return hdsGroup_v4(locator, group_str, status);

}

/*=========================================*/
/* hdsGtune - Get HDS tuning parameter     */
/*=========================================*/

int
hdsGtune(const char *param_str, int *value, int *status) {
  if (*status != SAI__OK) return *status;
  hdsGtune_v4(param_str, value, status);
  hdsGtune_v5(param_str, value, status);
  if (*status != SAI__OK) {
    emsRepf("hdsGtune_wrap", "hdsGtune: Error obtaining value of tuning parameter '%s'",
            status, param_str);
  }
  return *status;
}

/*=========================================*/
/* hdsGtune - Get HDS status integers      */
/*=========================================*/

int
hdsInfoI(const HDSLoc* locator, const char *topic_str, const char *extra, int *result, int *status) {

  if (ISHDSv5(locator)) return hdsInfoI_v5(locator, topic_str, extra, result, status);
  return hdsInfoI_v4(locator, topic_str, extra, result, status);

}

/*=================================*/
/* hdsLink - Link locator to group */
/*=================================*/

int
hdsLink(const HDSLoc *locator, const char *group_str, int *status) {

  if (ISHDSv5(locator)) return hdsLink_v5(locator, group_str, status);
  return hdsLink_v4(locator, group_str, status);

}

/*================================*/
/* hdsLock - Lock container file */
/*================================*/

int
hdsLock(const HDSLoc *locator, int *status) {

  if (ISHDSv5(locator)) return hdsLock_v5(locator, status);
  return hdsLock_v4(locator, status);

}

/*====================================*/
/* hdsNew - Create new container file */
/*====================================*/

int
hdsNew(const char *file_str, const char *name_str, const char *type_str, int ndim, const hdsdim dims[], HDSLoc **locator, int *status) {
  return hdsNew_v5(file_str, name_str, type_str, ndim, dims, locator, status);
}

/*========================================*/
/* hdsOpen - Open existing container file */
/*========================================*/

int
hdsOpen(const char *file_str, const char *mode_str, HDSLoc **locator, int *status) {
  if (*status != SAI__OK) return *status;
  hdsOpen_v5(file_str, mode_str, locator, status);
  if (*status != SAI__OK) {
    emsAnnul(status);
    hdsOpen_v4(file_str, mode_str, locator, status);
  }
  return *status;
}

/*===============================*/
/* hdsShow - Show HDS statistics */
/*===============================*/

int
hdsShow(const char *topic_str, int *status) {
  /* Requires special code */
  printf("Aborting. Special code required in: %s\n", "hdsShow(topic_str, status);");
  abort();
  return *status;
}

/*===============================================*/
/* hdsState - Enquire the current state of HDS   */
/*===============================================*/

int
hdsState(hdsbool_t *state, int *status) {
  hdsState_v4(state, status);
  return hdsState_v5(state, status);
}

/*============================*/
/* hdsStop - Close down HDS   */
/*============================*/

int
hdsStop(int *status) {
  hdsStop_v4(status);
  return hdsStop_v5(status);
}

/*==============================*/
/* hdsTrace - Trace object path */
/*==============================*/

int
hdsTrace(const HDSLoc *locator, int *nlev, char *path_str, char *file_str, int *status, size_t path_length, size_t file_length) {

  if (ISHDSv5(locator)) return hdsTrace_v5(locator, nlev, path_str, file_str, status, path_length, file_length);
  return hdsTrace_v4(locator, nlev, path_str, file_str, status, path_length, file_length);

}

/*========================================*/
/* hdsTune - Set HDS tuning parameter     */
/*========================================*/

int
hdsTune(const char *param_str, int value, int *status) {
  hdsTune_v4(param_str, value, status);
  return hdsTune_v5(param_str, value, status);
}

/*=================================================================*/
/* hdsWild - Perform a wild-card search for HDS container files   */
/*=================================================================*/

int
hdsWild(const char *fspec, const char *mode, int *iwld, HDSLoc **locator, int *status) {
  /* Requires special code */
  printf("Aborting. Special code required in: %s\n", "hdsWild(fspec, mode, iwld, locator, status);");
  abort();
  return *status;
}

/*=================================================================*/
/*  Deprecated routines!                                           */
/*=================================================================*/

/*========================================*/
/* datConv - Enquire conversion possible? */
/*========================================*/

int
datConv(const HDSLoc *locator, const char *type_str, hdsbool_t *conv, int *status) {

  if (ISHDSv5(locator)) return datConv_v5(locator, type_str, conv, status);
  return datConv_v4(locator, type_str, conv, status);

}

/*=====================================================*/
/* hdsClose - Close container file (Obsolete routine!) */
/*=====================================================*/

int
hdsClose(HDSLoc **locator, int *status) {

  if (ISHDSv5(*locator)) return hdsClose_v5(locator, status);
  return hdsClose_v4(locator, status);

}


/*===================================================================*/
/* hdsFind - Find an object (Fortran routine, requires hdsf library) */
/*===================================================================*/
void
hdsFind(const HDSLoc *locator1, const char *name, const char *mode, HDSLoc **locator2, int *status) {

  if (ISHDSv5(locator1)) return hdsFind_v5(locator1, name, mode, locator2, status);
  return hdsFind_v4(locator1, name, mode, locator2, status);

}


/* STAR_HDS_H_INCLUDED */
#endif
