/*
 *  Name:
 *     patchCmd
 *
 *  Purpose:
 *     Interface from RTD to RPATCH routine.
 *
 *  Description:
 *     This routine is designed to be called from RTD, the Real Time
 *     Display tool, as a foreign method. It accepts an info structure
 *     that gives direct access to the displayed image and an unparsed
 *     sequence of arguments.
 *
 *     The arguments string must contain either:
 *
 *        -new "ard.file"
 *        -fit "ard.file"
 *
 *     or:
 *
 *        -undo
 *
 *     And may optionally contain some of the following:
 *
 *        -keep x1 y1 x2 xy2
 *        -scale value
 *        -order n
 *        -region x1 y1 x2 y2
 *        -usevar (1|0)
 *        -release
 *
 *     The new ARD file specifies the part of the image to be replaced
 *     and the fit the part of the image to use when determining the
 *     background value.
 *
 *     The keep region specifies part of the image (in pixel indices) to
 *     retain (i.e. copy) before patching. This can be restored by invoking
 *     this routine with just the "-undo" argument. Only one undo region
 *     may be kept at any time.
 *
 *     Region if given should include all the parts of the image that
 *     will be used (i.e. the fit and new regions). This can improve
 *     the performance by limiting the region of the image that must
 *     be processed by ARD.
 *
 *     Usevar should be set to 1 if the image variance component
 *     should be used.
 *
 *     Release should be used when the application no longer wants to
 *     keep any undo information.
 *
 *  Arguments:
 *     StarImageInfo = struct * (Given)
 *        Pointer to an ImageInfo structure.
 *     args = char * (Given)
 *        Pointer to any arguments.
 *     errStr = char ** (Returned)
 *        Pointer to pointer to a string that contains an error
 *        message if appropriate. Only set if return is 0.
 *
 *  Copyright:
 *     Copyright (C) 1998 Central Laboratory of the Research Councils
 *
 *  Authors:
 *     PWD: Peter W. Draper (STARLINK - Durham University)
 *
 *  History:
 *     17-JUN-1996 (PWD):
 *        Added argument processing.
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "StarRtdForeignCmds.h"
#include "f77.h"
#include "cnf.h"
#include "sae_par.h"
#include "ems.h"
#include "ems_par.h"
#define MAXFILE 132
#define FRACTION 0.05
#define MAX(a,b) ( (a) > (b) ) ? (a) : (b)
#define MIN(a,b) ( (a) < (b) ) ? (a) : (b)
#define SWAP(a,b,t) ( (t) = (a), (a) = (b), (b) = (t) )
typedef unsigned char byte;

/*  Undo structure */
static struct unDo {
  void *imagePtr;
  byte *qualPtr;
  enum ImageDataType type;
  int x1;
  int y1;
  int x2;
  int y2;
  int nx;
  int ny;
} undoInfo = {
  (void *) NULL, (byte *) NULL, FLOAT_IMAGE, 1, 1, 1, 1
};

/*  Function prototypes */
extern F77_SUBROUTINE(rpatch)( INTEGER(ndfId), CHARACTER(type),
                               REAL(scale), INTEGER(nFit),
                               CHARACTER(fitFile),
                               CHARACTER(newFile), LOGICAL(usevar),
                               INTEGER(nx), INTEGER(ny),
                               INTEGER(xs1), INTEGER(ys1),
                               INTEGER(xs2), INTEGER(ys2),
                               LOGICAL(haveQual), POINTER(q),
                               POINTER(f), INTEGER(status)
                               TRAIL(type) TRAIL(fitFile)
                               TRAIL(newFile) );

extern F77_SUBROUTINE(rtd1_aqual)( INTEGER(ndfId), LOGICAL(grab),
                                   POINTER(q), LOGICAL(haveQual));

static void *copyImagetoWork( enum ImageDataType type,
                              void *imagePtr,
                              int nx, int x1, int y1, int x2, int y2);
static void copyWorktoImage( enum ImageDataType type,
                             void *imagePtr,
                             void *workPtr,
                             int nx, int x1, int y1, int x2, int y2);

int patchCmd( struct StarImageInfo *info, char *args, char **errStr )
{

  /* Local declarations (Fortran versions)                           */
  DECLARE_CHARACTER(fitFile, MAXFILE);     /* ARD description of background area */
  DECLARE_CHARACTER(newFile, MAXFILE);     /* ARD description of fit area */
  DECLARE_CHARACTER(type, 10);             /* HDS type of data */
  DECLARE_INTEGER(nFit);                   /* Order of surface fit */
  DECLARE_INTEGER(ndfId);                  /* NDF identifier */
  DECLARE_INTEGER(nx);                     /* Size of image in X */
  DECLARE_INTEGER(ny);                     /* Size of image in Y */
  DECLARE_INTEGER(status);                 /* Starlink STATUS */
  DECLARE_INTEGER(xs1);                    /* Bound of processed region */
  DECLARE_INTEGER(xs2);                    /* Bound of processed region */
  DECLARE_INTEGER(ys1);                    /* Bound of processed region */
  DECLARE_INTEGER(ys2);                    /* Bound of processed region */
  DECLARE_LOGICAL(haveQual);               /* True when NDF has quality */
  DECLARE_LOGICAL(usevar);                 /* Use variances if available? */
  DECLARE_LOGICAL(grab);                   /* Grab quality */
  DECLARE_POINTER(f);                      /* Pointer to image data. */
  DECLARE_POINTER(q);                      /* Pointer to quality data. */
  DECLARE_POINTER(qualPtr);                /* Pointer to quality data */
  DECLARE_REAL(scale);                     /* Scaling factor for noise */

  /* Local variables: */
  int i, j;
  int undo;
  int release;
  char *ptr;
  char *atPtr;
  int keep, x1, y1, x2, y2;
  int size;
  int usepart;
  char *opStr;
  char *opPtr;
  char param[EMS__SZPAR];
  int used;
  int result;
  int variance;

#ifdef _DEBUG_
  printf( "Called patchCmd \n");
#endif

  /* Runtime initialisations. */
  opStr = (char *)NULL;
  result = 1;
  scale = 1.0f;
  nFit = 3;
  undo = 0;
  release = 0;
  keep = 0;
  usevar = F77_FALSE;
  x1 = y1 = x2 = y2 = 0;
  xs1 = ys1 = xs2 = ys2 = 0;
  usepart = 0;
  haveQual = F77_FALSE;

  /*  Parse the input arguments extracting the parameters which have
      been set. */
  atPtr = args;
  while ( ( ptr = strtok( atPtr, " " ) ) != NULL ) {
    atPtr = (char *) NULL;

    if ( strcmp( ptr,  "-new" ) == 0 ) {
      ptr = strtok( atPtr, " " );
      cnf_exprt( ptr, (char *)newFile, MAXFILE );

    } else if ( strcmp( ptr,  "-fit" ) == 0 ) {
      ptr = strtok( atPtr, " " );
      cnf_exprt( ptr, (char *)fitFile, MAXFILE );

    } else if ( strcmp( ptr,  "-undo" ) == 0 ) {
      undo = 1;

    } else if ( strcmp( ptr,  "-usevar" ) == 0 ) {
      ptr = strtok( atPtr, " " );
      variance = atoi( ptr );
      if ( variance  ) {
        usevar = F77_TRUE;
      } else {
        usevar = F77_FALSE;
      }

    } else if ( strcmp( ptr,  "-keep" ) == 0 ) {
      ptr = strtok( atPtr, " " );
      x1 = atoi( ptr );
      ptr = strtok( atPtr, " " );
      y1 = atoi( ptr );
      ptr = strtok( atPtr, " " );
      x2 = atoi( ptr );
      ptr = strtok( atPtr, " " );
      y2 = atoi( ptr );
      if ( x1 > x2 ) {
        SWAP( x1, x2, i );
      }
      if ( y1 > y2 ) {
        SWAP( y1, y2, i );
      }
      if ( x1 != 0 && y1 != 0 && x2 != 0 && y2 != 0 ) {
        keep = 1;
      }

    } else if ( strcmp( ptr,  "-region" ) == 0 ) {
      ptr = strtok( atPtr, " " );
      xs1 = atoi( ptr );
      ptr = strtok( atPtr, " " );
      ys1 = atoi( ptr );
      ptr = strtok( atPtr, " " );
      xs2 = atoi( ptr );
      ptr = strtok( atPtr, " " );
      ys2 = atoi( ptr );
      if ( xs1 > xs2 ) {
        SWAP( xs1, xs2, i );
      }
      if ( ys1 > ys2 ) {
        SWAP( ys1, ys2, i );
      }

    } else if ( strcmp( ptr,  "-scale" ) == 0 ) {
      ptr = strtok( atPtr, " " );
      scale = atof( ptr );

    } else if ( strcmp( ptr,  "-order" ) == 0 ) {
      ptr = strtok( atPtr, " " );
      nFit = atoi( ptr );

    } else if ( strcmp( ptr,  "-release" ) == 0 ) {
      release = 1;
    }
  }
  if ( !undo && !release ) {

    /*  Decode info structure into something more useful for calling
        Fortran and determine HDS type of image. */
    nx = info->nx;
    ny = info->ny;
    ndfId = info->NDFid;
    switch ( info->type ) {
       case  BYTE_IMAGE:
        cnf_exprt( "_BYTE", type, 10 );
        break;
      case  X_IMAGE:
        cnf_exprt( "_UBYTE", type, 10 );
        break;
      case  SHORT_IMAGE:
        cnf_exprt( "_WORD", type, 10 );
        break;
      case  USHORT_IMAGE:
        cnf_exprt( "_UWORD", type, 10 );
        break;
      case  LONG_IMAGE:
        cnf_exprt( "_INTEGER", type, 10 );
        break;
      case  FLOAT_IMAGE:
        cnf_exprt( "_REAL", type, 10 );
        break;
    }

    /*  Access the quality information associated with this NDF (this
     *  is a copy not the actual values and needs to be added to any
     *  output NDFs. */
    if ( ndfId != 0 ) { 
      grab = F77_TRUE;
      F77_CALL(rtd1_aqual)( INTEGER_ARG(&ndfId), LOGICAL_ARG(&grab),
                            POINTER_ARG(&qualPtr),
                            LOGICAL_ARG(&haveQual));
    } else {
      haveQual = F77_FALSE;
    }

    /*  If keep is true then record the contents of the image at the
        given bounds (this is assumed to be a bounding box of the
        region to be replaced). */
    if ( keep ) {
      if ( undoInfo.imagePtr != (void *) NULL ) {
        free( (void *)undoInfo.imagePtr );
        undoInfo.imagePtr = (void *) NULL;
      }
      undoInfo.type = info->type;
      x1--, y1--;
      x1 = undoInfo.x1 = MAX( x1, 0 );
      y1 = undoInfo.y1 = MAX( y1, 0 );
      x2 = undoInfo.x2 = MIN( x2, nx );
      y2 = undoInfo.y2 = MIN( y2, ny );
      undoInfo.nx = nx;
      undoInfo.ny = ny;
      size = ( x2 - x1 + 1 ) * ( y2 - y1 + 1 );
      if ( size > 0 ) {
        undoInfo.imagePtr = copyImagetoWork( info->type, info->imageData,
                                             nx, x1, y1, x2, y2 );

        /* Do the same for the quality component if necessary */
        if ( haveQual == F77_TRUE ) {
          undoInfo.qualPtr = copyImagetoWork( BYTE_IMAGE, (void *)qualPtr,
                                              nx, x1, y1, x2, y2 );
        }
      } else {
        undoInfo.imagePtr = (void *) NULL;
        undoInfo.qualPtr = (byte *) NULL;
      }
    }

    /*  Now decide if we want to process just a part of the image
        rather than the whole as an efficiency saving, only do this
        if region of image to use is less than a given fraction
        of the whole. */
    if ( xs1 == 0 && ys1 == 0 && xs2 == 0 && ys2 == 0 )  {
      usepart = 0;
    } else {
      size = ( xs2 - xs1 + 1 ) * ( ys2 - ys1 + 1 );
      if ( size * FRACTION < nx * ny && size > 0 ) {

        /*  Copy part of image into workspace. */
        xs1--, ys1--;
        xs1 = MAX( xs1, 0 );
        ys1 = MAX( ys1, 0 );
        xs2 = MIN( xs2, nx );
        ys2 = MIN( ys2, ny );
        f = (F77_POINTER_TYPE) copyImagetoWork( info->type,
                                                info->imageData,
                                                nx, xs1, ys1, xs2, ys2 );
        
        /* Do the same for the quality component if necessary */
        if ( haveQual == F77_TRUE ) {
          q = (F77_POINTER_TYPE) copyImagetoWork( BYTE_IMAGE,
                                                  (void *)qualPtr,
                                                  nx, xs1, ys1, xs2,
                                                  ys2 );
        } else {
	  q = (F77_POINTER_TYPE) NULL;
	}

        /* Work out new image size and increment ranges to Fortran
           style (start at 1). */
        xs1++, ys1++;
        xs1 = MAX( xs1, 1 );
        ys1 = MAX( ys1, 1 );
        xs2 = MIN( xs2, nx );
        ys2 = MIN( ys2, ny );
        nx = xs2 - xs1 + 1;
        ny = ys2 - ys1 + 1;
        usepart = 1;
      } else {
        usepart = 0;
      }
    }
    if ( !usepart ) {
      f = (F77_POINTER_TYPE)info->imageData;
      q = (F77_POINTER_TYPE)qualPtr;
      xs1 = 1, ys1 = 1, xs2 = nx, ys2 = ny;
    }

    /*  Call Fortran routine to do the work of replacement. */
    ems_mark_c();
    F77_CALL(rpatch)( INTEGER_ARG(&ndfId), CHARACTER_ARG(type),
                      REAL_ARG(&scale), INTEGER_ARG(&nFit),
                      CHARACTER_ARG(fitFile),
                      CHARACTER_ARG(newFile), LOGICAL_ARG(&usevar),
                      INTEGER_ARG(&nx), INTEGER_ARG(&ny),
                      INTEGER_ARG(&xs1), INTEGER_ARG(&ys1),
                      INTEGER_ARG(&xs2), INTEGER_ARG(&ys2),
                      LOGICAL_ARG(&haveQual), POINTER_ARG(&q),
                      POINTER_ARG(&f), INTEGER_ARG(&status)
                      TRAIL_ARG(type) TRAIL_ARG(fitFile)
                      TRAIL_ARG(newFile) );

    /*  Copy modified image part back, if necessary. */
    if ( usepart && status == SAI__OK ) {
      xs1--, ys1--;
      xs1 = MAX( xs1, 0 );
      ys1 = MAX( ys1, 0 );
      copyWorktoImage( info->type, info->imageData, (void *) f,
                       info->nx, xs1, ys1, xs2, ys2 );

      /* Do the same for the quality component if necessary */
      if ( haveQual == F77_TRUE ) {
        copyWorktoImage( BYTE_IMAGE, (void *)qualPtr, (void *) q,
                         info->nx, xs1, ys1, xs2, ys2 );
      }
    }

    if ( status != SAI__OK ) {

      /*  Get the error from EMS and return it as errStr. */
      opStr = (char *) NULL;
      used = 0;
      while ( status != SAI__OK ) {
        opStr = realloc( (void *)opStr, (size_t) EMS__SZMSG * sizeof(char) );
        opPtr = opStr + used;
        ems_stat_c( &status );
        ems_eload_c( param, &i, opPtr, &j, &status);
        used += j;
        opStr[used++] ='\n';
      }
      opStr[used] = '\0';
      *errStr = opStr;

      /*  Set success of routine to false. */
      result = 0;
    }
    ems_rlse_c();

  } else if ( undo ) {

    /*  Request to undo. Only possible if image has been stored and
        the current image is the same size and type as before */
    if ( undoInfo.imagePtr != (void *)NULL &&
         ( info->type == undoInfo.type ) &&
         ( info->nx == undoInfo.nx ) &&
         ( info->ny == undoInfo.ny )
         ) {
      nx = info->nx;
      ny = info->ny;
      x1 = undoInfo.x1;
      y1 = undoInfo.y1;
      x2 = undoInfo.x2;
      y2 = undoInfo.y2;
      copyWorktoImage( info->type, info->imageData, undoInfo.imagePtr,
                       nx, x1, y1, x2, y2 );
      undoInfo.imagePtr = (void *) NULL;
      if ( undoInfo.qualPtr != (byte *)NULL ) {
        grab = F77_FALSE;
        F77_CALL(rtd1_aqual)( INTEGER_ARG(&info->NDFid),
                              LOGICAL_ARG(&grab),
                              POINTER_ARG(&qualPtr),
                              LOGICAL_ARG(&haveQual));

        /* Do the same for the quality component if necessary */
        if ( haveQual == F77_TRUE ) {
          copyWorktoImage( BYTE_IMAGE, (void *)qualPtr, 
                           (void *)undoInfo.qualPtr,
                           nx, x1, y1, x2, y2 );
          undoInfo.qualPtr = (void *) NULL;
        }
      }
    }
  } else if ( release )  {

    /*  Request to release any resources stored by this routine. */
    if ( undoInfo.imagePtr != (void *) NULL ) {
      free( (void *)undoInfo.imagePtr );
      undoInfo.imagePtr = (void *) NULL;

      /* Do the same for the quality component if necessary */
      if ( undoInfo.qualPtr != (byte *) NULL ) {
        free( (void *)undoInfo.qualPtr );
        undoInfo.qualPtr = (void *) NULL;
      }
    }
  }
  return result;
}

/*
 *  Copy a part of an image into workspace
 */

void *copyImagetoWork( enum ImageDataType type,
                       void *imagePtr,
                       int nx, int x1, int y1, int x2, int y2)
{
  signed char *scPtr, *scArr;
  unsigned char *ucPtr, *ucArr;
  short *sPtr, *sArr;
  unsigned short *usPtr, *usArr;
  int *iPtr, *iArr;
  float *fPtr, *fArr;
  void *sectPtr;
  int i, j;
  int size;

  size = ( x2 - x1 + 1 ) * ( y2 - y1 + 1 );

  switch ( type ) {
  case  BYTE_IMAGE:
    scArr = (signed char *) imagePtr;
    sectPtr = (void *) malloc( size * sizeof(signed char) );
    scPtr = (signed char *) sectPtr;
    for( j=y1; j < y2; j++ ) {
      for( i=x1; i < x2; i++ ) {
        *scPtr++ = scArr[nx*j+i];
      }
    }
    break;
  case  X_IMAGE:
    ucArr = (unsigned char *) imagePtr;
    sectPtr = (void *) malloc( size * sizeof(unsigned char) );
    ucPtr = (unsigned char *) sectPtr;
    for( j=y1; j < y2; j++ ) {
      for( i=x1; i < x2; i++ ) {
        *ucPtr++ = ucArr[nx*j+i];
      }
    }
    break;
  case  SHORT_IMAGE:
    sArr = (short *) imagePtr;
    sectPtr = (void *) malloc( size * sizeof(short) );
    sPtr = (short *) sectPtr;
    for( j=y1; j < y2; j++ ) {
      for( i=x1; i < x2; i++ ) {
        *sPtr++ = sArr[nx*j+i];
      }
    }
    break;
  case  USHORT_IMAGE:
    usArr = (unsigned short *) imagePtr;
    sectPtr = (void *) malloc( size * sizeof(unsigned short) );
    usPtr = (unsigned short *) sectPtr;
    for( j=y1; j < y2; j++ ) {
      for( i=x1; i < x2; i++ ) {
        *usPtr++ = usArr[nx*j+i];
      }
    }
    break;
  case  LONG_IMAGE:
    iArr = (int *) imagePtr;
    sectPtr = (void *) malloc( size * sizeof(int) );
    iPtr = (int *) sectPtr;
    for( j=y1; j < y2; j++ ) {
      for( i=x1; i < x2; i++ ) {
        *iPtr++ = iArr[nx*j+i];
      }
    }
    break;
  case  FLOAT_IMAGE:
    fArr = (float *) imagePtr;
    sectPtr = (void *) malloc( size * sizeof(float) );
    fPtr = (float *) sectPtr;
    for( j=y1; j < y2; j++ ) {
      for( i=x1; i < x2; i++ ) {
        *fPtr++ = fArr[nx*j+i];
      }
    }
    break;
  }
  return sectPtr;
}

/*
 *  Copy workspace back into an image.
 */

void copyWorktoImage( enum ImageDataType type,
                      void *imagePtr,
                      void *workPtr,
                      int nx, int x1, int y1, int x2, int y2)
{
  signed char *scPtr, *scArr;
  unsigned char *ucPtr, *ucArr;
  short *sPtr, *sArr;
  unsigned short *usPtr, *usArr;
  int *iPtr, *iArr;
  float *fPtr, *fArr;
  int i, j;

  switch ( type ) {
  case  BYTE_IMAGE:
    scArr = (signed char *) imagePtr;
    scPtr = (signed char *) workPtr;
    for( j=y1; j < y2; j++ ) {
      for( i=x1; i < x2; i++ ) {
        scArr[nx*j+i] = *scPtr++;
      }
    }
    break;
  case  X_IMAGE:
    ucArr = (unsigned char *) imagePtr;
    ucPtr = (unsigned char *) workPtr;
    for( j=y1; j < y2; j++ ) {
      for( i=x1; i < x2; i++ ) {
        ucArr[nx*j+i] = *ucPtr++;
      }
    }
    break;
  case  SHORT_IMAGE:
    sArr = (short *) imagePtr;
    sPtr = (short *) workPtr;
    for( j=y1; j < y2; j++ ) {
      for( i=x1; i < x2; i++ ) {
              sArr[nx*j+i] = *sPtr++;
      }
    }
    break;
  case  USHORT_IMAGE:
    usArr = (unsigned short *) imagePtr;
    usPtr = (unsigned short *) workPtr;
    for( j=y1; j < y2; j++ ) {
      for( i=x1; i < x2; i++ ) {
        usArr[nx*j+i] = *usPtr++;
      }
    }
    break;
  case  LONG_IMAGE:
    iArr = (int *) imagePtr;
    iPtr = (int *) workPtr;
    for( j=y1; j < y2; j++ ) {
      for( i=x1; i < x2; i++ ) {
        iArr[nx*j+i] = *iPtr++;
      }
    }
    break;
  case  FLOAT_IMAGE:
    fArr = (float *) imagePtr;
    fPtr = (float *) workPtr;
    for( j=y1; j < y2; j++ ) {
      for( i=x1; i < x2; i++ ) {
        fArr[nx*j+i] = *fPtr++;
      }
    }
    break;
  }

  /*  Finally free the workspace. */
  free( workPtr );
}
