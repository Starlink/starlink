/*
 *  Name:
 *     patchCmd
 *
 *  Purpose:
 *     Interface from GAIA to RPATCH routine.
 *
 *  Description:
 *     This routine is designed to be called from GAIA as a foreign method.
 *     It accepts an info structure that gives direct access to the displayed
 *     image and an unparsed sequence of arguments.
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
 *     Copyright (C) 1998-2004 Central Laboratory of the Research Councils
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *
 *  Authors:
 *     PWD: Peter W. Draper (STARLINK - Durham University)
 *
 *  History:
 *     17-JUN-1996 (PWD):
 *        Added argument processing.
 *     20-JAN-2000 (PWD):
 *        Added byte swapping changes.
 *     01-JUN-2001 (PWD):
 *        Now supports double precision images.
 *     03-SEP-2004 (PWD):
 *        Converted to pass CNF pointers to and from Fortran.
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "StarRtdForeignCmds.h"
#include "f77.h"
#include "cnf.h"
#include "sae_par.h"
#include "ems.h"
#include "ems_par.h"
#include "gaiaUtils.h"

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
                              void *imagePtr, int swap,
                              int nx, int x1, int y1, int x2, int y2);
static void copyWorktoImage( enum ImageDataType type,
                             void *imagePtr,
                             void *workPtr, int swap,
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
    DECLARE_POINTER(fimage);                 /* Pointer to image data. */
    DECLARE_POINTER(fqual);                  /* Pointer to quality data. */
    DECLARE_POINTER(fqualPtr);               /* Pointer to quality data */
    DECLARE_REAL(scale);                     /* Scaling factor for noise */

    /* Local C variables: */
    byte *qualPtr;
    char *atPtr;
    char *ptr;
    int i;
    int keep, x1, y1, x2, y2;
    int release;
    int result;
    int size;
    int undo;
    int usepart;
    int variance;
    void *image;
    void *qual;

#ifdef _DEBUG_
    printf( "Called patchCmd \n");
#endif

    /* Runtime initialisations. */
    qualPtr = (byte *)NULL;
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
            cnf_exprt( "_UBYTE", type, 10 );
            break;
        case  X_IMAGE:
            cnf_exprt( "_BYTE", type, 10 );
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
        case  DOUBLE_IMAGE:
            cnf_exprt( "_DOUBLE", type, 10 );
            break;
        }

        /*  Access the quality information associated with this NDF (this is a
         *  copy not the actual mapped data and needs to be added to any output
         *  NDFs.
         */
        if ( ndfId != 0 ) {
            grab = F77_TRUE;
            F77_CALL(rtd1_aqual)( INTEGER_ARG(&ndfId), LOGICAL_ARG(&grab),
                                  POINTER_ARG(&fqualPtr),
                                  LOGICAL_ARG(&haveQual));
            qualPtr = (void *)NULL;
            if ( F77_ISTRUE( haveQual ) ) {
                F77_IMPORT_POINTER( fqualPtr, qualPtr );
            }
        } else {
            haveQual = F77_FALSE;
        }

        /*  If keep is true then record the contents of the image at the given
         *  bounds (this is assumed to be a bounding box of the region to be
         *  replaced).
         */
        if ( keep ) {
            if ( undoInfo.imagePtr != (void *) NULL ) {
                cnfFree( (void *)undoInfo.imagePtr );
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
                undoInfo.imagePtr = copyImagetoWork( info->type,
                                                     info->imageData,
                                                     info->swap,
                                                     nx, x1, y1, x2, y2 );

                /* Do the same for the quality component if necessary */
                if ( F77_ISTRUE( haveQual ) ) {
                    undoInfo.qualPtr = copyImagetoWork( BYTE_IMAGE, qualPtr, 0,
                                                        nx, x1, y1, x2, y2 );
                }
            } else {
                undoInfo.imagePtr = (void *) NULL;
                undoInfo.qualPtr = (byte *) NULL;
            }
        }

        /*  Now decide if we want to process just a part of the image rather
         *  than the whole as an efficiency saving, only do this if region of
         *  image to use is less than a given fraction of the whole.
         */
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
                image = copyImagetoWork( info->type, info->imageData,
                                         info->swap, nx, xs1, ys1, xs2, ys2 );
                F77_EXPORT_POINTER( image, fimage );

                /* Do the same for the quality component if necessary */
                if ( F77_ISTRUE( haveQual ) ) {
                    qual = copyImagetoWork( BYTE_IMAGE, (void *) qualPtr, 0,
                                            nx, xs1, ys1, xs2, ys2 );
                    F77_EXPORT_POINTER( qual, fqual );
                } else {
                    fqual = (F77_POINTER_TYPE) NULL;
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
            F77_EXPORT_POINTER( info->imageData, fimage );
            F77_EXPORT_POINTER( qualPtr, fqual );
            xs1 = 1, ys1 = 1, xs2 = nx, ys2 = ny;
        }

        /*  Call Fortran routine to do the work of replacement. */
        emsMark();
        F77_CALL(rpatch)( INTEGER_ARG(&ndfId), CHARACTER_ARG(type),
                          REAL_ARG(&scale), INTEGER_ARG(&nFit),
                          CHARACTER_ARG(fitFile),
                          CHARACTER_ARG(newFile), LOGICAL_ARG(&usevar),
                          INTEGER_ARG(&nx), INTEGER_ARG(&ny),
                          INTEGER_ARG(&xs1), INTEGER_ARG(&ys1),
                          INTEGER_ARG(&xs2), INTEGER_ARG(&ys2),
                          LOGICAL_ARG(&haveQual),
                          POINTER_ARG(&fqual),
                          POINTER_ARG(&fimage), INTEGER_ARG(&status)
                          TRAIL_ARG(type) TRAIL_ARG(fitFile)
                          TRAIL_ARG(newFile) );

        /*  Copy modified image part back, if necessary. */
        if ( usepart && status == SAI__OK ) {
            xs1--, ys1--;
            xs1 = MAX( xs1, 0 );
            ys1 = MAX( ys1, 0 );
            F77_IMPORT_POINTER( fimage, image );
            copyWorktoImage( info->type, info->imageData, (void *) image,
                             info->swap,
                             info->nx, xs1, ys1, xs2, ys2 );

            /* Do the same for the quality component if necessary */
            if ( F77_ISTRUE( haveQual ) ) {
                F77_IMPORT_POINTER( fqual, qual );
                copyWorktoImage( BYTE_IMAGE, (void *)qualPtr,
                                 (void *) qual, 0, info->nx,
                                 xs1, ys1, xs2, ys2 );
            }
        }

        if ( status != SAI__OK ) {

            /*  Get the error from EMS and return it as errStr. */
            *errStr = gaiaUtilsErrMessage();

            /*  Set success of routine to false. */
            result = 0;
        }
        emsRlse();

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
                             info->swap,
                             nx, x1, y1, x2, y2 );
            undoInfo.imagePtr = (void *) NULL;
            if ( undoInfo.qualPtr != (byte *)NULL ) {

                /* Also restore quality if necessary */
                if ( F77_ISTRUE( haveQual ) ) {
                    copyWorktoImage( BYTE_IMAGE, (void *)qualPtr,
                                     (void *)undoInfo.qualPtr, 0,
                                     nx, x1, y1, x2, y2 );
                    undoInfo.qualPtr = (void *) NULL;
                }

                /*  Free the quality we're holding from the last call to
                 *  rtd1_aqual.
                 */
                if ( qualPtr != NULL ) {
                    grab = F77_FALSE;
                    F77_CALL(rtd1_aqual)( INTEGER_ARG(&info->NDFid),
                                          LOGICAL_ARG(&grab),
                                          POINTER_ARG(&fqualPtr),
                                          LOGICAL_ARG(&haveQual) );
                }
            }
        }
    } else if ( release )  {

        /*  Request to release any resources stored by this routine. */
        if ( undoInfo.imagePtr != (void *) NULL ) {
            cnfFree( (void *)undoInfo.imagePtr );
            undoInfo.imagePtr = (void *) NULL;

            /* Do the same for the quality component if necessary */
            if ( undoInfo.qualPtr != (byte *) NULL ) {
                cnfFree( (void *)undoInfo.qualPtr );
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
                       void *imagePtr, int swap,
                       int nx, int x1, int y1, int x2, int y2)
{
    unsigned char *scPtr, *scArr;
    unsigned short *sPtr, *sArr;
    unsigned int *iPtr, *iArr;
    void *sectPtr;
    int i, j;
    int size;

    size = ( x2 - x1 + 1 ) * ( y2 - y1 + 1 );

    switch ( type ) {
       case BYTE_IMAGE:
       case X_IMAGE:
           scArr = (unsigned char *) imagePtr;
           sectPtr = (void *) cnfMalloc( size * sizeof(unsigned char) );
           scPtr = (unsigned char *) sectPtr;
           for( j=y1; j < y2; j++ ) {
               for( i=x1; i < x2; i++ ) {
                   *scPtr++ = scArr[nx*j+i];
               }
           }
           break;
       case SHORT_IMAGE:
       case USHORT_IMAGE:
           sArr = (unsigned short *) imagePtr;
           sectPtr = (void *) cnfMalloc( size * sizeof(unsigned short) );
           sPtr = (unsigned short *) sectPtr;
           if ( swap ) {
               for( j=y1; j < y2; j++ ) {
                   for( i=x1; i < x2; i++ ) {
                       *sPtr++ = ntohs( sArr[nx*j+i] );
                   }
               }
           } else {
               for( j=y1; j < y2; j++ ) {
                   for( i=x1; i < x2; i++ ) {
                       *sPtr++ = sArr[nx*j+i];
                   }
               }
           }
           break;
       case LONG_IMAGE:
       case FLOAT_IMAGE:
           iArr = (unsigned int *) imagePtr;
           sectPtr = (void *) cnfMalloc( size * sizeof(unsigned int) );
           iPtr = (unsigned int *) sectPtr;
           if ( swap ) {
               for( j=y1; j < y2; j++ ) {
                   for( i=x1; i < x2; i++ ) {
                       *iPtr++ = ntohl( iArr[nx*j+i] );
                   }
               }
           } else {
               for( j=y1; j < y2; j++ ) {
                   for( i=x1; i < x2; i++ ) {
                       *iPtr++ = iArr[nx*j+i];
                   }
               }
           }
           break;
       case DOUBLE_IMAGE: {
           double *iArr = (double *) imagePtr;
           double *iPtr;
           union { unsigned int halves[2]; double value; } joined;
           unsigned int tmp;
           sectPtr = (void *) cnfMalloc( size * sizeof(double) );
           iPtr = (double *) sectPtr;
           if ( swap ) {
               for( j=y1; j < y2; j++ ) {
                   for( i=x1; i < x2; i++ ) {
                       /*  Need to swap two sets of four bytes. */
                       joined.value = iArr[nx*j+i];
                       tmp = joined.halves[0];
                       joined.halves[0] = ntohl( joined.halves[1] );
                       joined.halves[1] = ntohl( tmp );
                       *iPtr++ = joined.value;
                   }
               }
           } else {
               for( j=y1; j < y2; j++ ) {
                   for( i=x1; i < x2; i++ ) {
                       *iPtr++ = iArr[nx*j+i];
                   }
               }
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
                      int swap,
                      int nx, int x1, int y1, int x2, int y2 )
{
    signed char *scPtr, *scArr;
    unsigned short *usPtr, *usArr;
    unsigned int *iPtr, *iArr;
    int i, j;

    switch ( type ) {
       case BYTE_IMAGE:
       case X_IMAGE:
           scArr = (signed char *) imagePtr;
           scPtr = (signed char *) workPtr;
           for( j=y1; j < y2; j++ ) {
               for( i=x1; i < x2; i++ ) {
                   scArr[nx*j+i] = *scPtr++;
               }
           }
           break;
       case SHORT_IMAGE:
       case USHORT_IMAGE:
           usArr = (unsigned short *) imagePtr;
           usPtr = (unsigned short *) workPtr;
           if ( swap ) {
               for( j=y1; j < y2; j++ ) {
                   for( i=x1; i < x2; i++ ) {
                       usArr[nx*j+i] = ntohs( *usPtr );
                       usPtr++;  /* ntohs could be macro */
                   }
               }
           } else {
               for( j=y1; j < y2; j++ ) {
                   for( i=x1; i < x2; i++ ) {
                       usArr[nx*j+i] = *usPtr++;
                   }
               }
           }
           break;
       case LONG_IMAGE:
       case FLOAT_IMAGE:
           iArr = (unsigned int *) imagePtr;
           iPtr = (unsigned int *) workPtr;
           if ( swap ) {
               for( j=y1; j < y2; j++ ) {
                   for( i=x1; i < x2; i++ ) {
                       iArr[nx*j+i] = ntohl( *iPtr );
                       iPtr++;
                   }
               }
           } else {
               for( j=y1; j < y2; j++ ) {
                   for( i=x1; i < x2; i++ ) {
                       iArr[nx*j+i] = *iPtr++;
                   }
               }
           }
           break;
       case DOUBLE_IMAGE:
       {
           double *iArr = (double *) imagePtr;
           double *iPtr = (double *) workPtr;
           union { unsigned int halves[2]; double value; } joined;
           unsigned int tmp;
           if ( swap ) {
               for( j=y1; j < y2; j++ ) {
                   for( i=x1; i < x2; i++ ) {
                       /*  Need to swap two sets of four bytes.*/
                       joined.value = *iPtr;
                       tmp = joined.halves[0];
                       joined.halves[0] = ntohl( joined.halves[1] );
                       joined.halves[1] = ntohl( tmp );
                       iArr[nx*j+i] = joined.value;
                       iPtr++;
                   }
               }
           } else {
               for( j=y1; j < y2; j++ ) {
                   for( i=x1; i < x2; i++ ) {
                       iArr[nx*j+i] = *iPtr++;
                   }
               }
         }
       }
       break;
    }

    /*  Finally free the workspace. */
    cnfFree( workPtr );
}
