/*  This file, modkey.c, contains routines that modify, insert, or update  */
/*  keywords in a FITS header.                                             */

/*  The FITSIO software was written by William Pence at the High Energy    */
/*  Astrophysic Science Archive Research Center (HEASARC) at the NASA      */
/*  Goddard Space Flight Center.  Users shall not, without prior written   */
/*  permission of the U.S. Government,  establish a claim to statutory     */
/*  copyright.  The Government and others acting on its behalf, shall have */
/*  a royalty-free, non-exclusive, irrevocable,  worldwide license for     */
/*  Government purposes to publish, distribute, translate, copy, exhibit,  */
/*  and perform such material.                                             */

#include <string.h>
/* stddef.h is apparently needed to define size_t */
#include <stddef.h>
#include "fitsio2.h"
/*--------------------------------------------------------------------------*/
int ffuky( fitsfile *fptr,     /* I - FITS file pointer        */
           int  datatype,      /* I - datatype of the value    */
           char *keyname,      /* I - name of keyword to write */
           void *value,        /* I - keyword value            */
           char *comm,         /* I - keyword comment          */
           int  *status)       /* IO - error status            */
/*
  Update the keyword, value and comment in the FITS header.
  The datatype is specified by the 2nd argument.
*/
{
    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (datatype == TSTRING)
    {
        ffukys(fptr, keyname, (char *) value, comm, status);
    }
    else if (datatype == TBYTE)
    {
        ffukyj(fptr, keyname, (long) *(unsigned char *) value, comm, status);
    }
    else if (datatype == TUSHORT)
    {
        ffukyj(fptr, keyname, (long) *(unsigned short *) value, comm, status);
    }
    else if (datatype == TSHORT)
    {
        ffukyj(fptr, keyname, (long) *(short *) value, comm, status);
    }
    else if (datatype == TINT)
    {
        ffukyj(fptr, keyname, (long) *(int *) value, comm, status);
    }
    else if (datatype == TLOGICAL)
    {
        ffukyl(fptr, keyname, *(int *) value, comm, status);
    }
    else if (datatype == TULONG)
    {
        ffukyg(fptr, keyname, (double) *(unsigned long *) value, 0,
               comm, status);
    }
    else if (datatype == TLONG)
    {
        ffukyj(fptr, keyname, *(long *) value, comm, status);
    }
    else if (datatype == TFLOAT)
    {
        ffukye(fptr, keyname, *(float *) value, 6, comm, status);
    }
    else if (datatype == TDOUBLE)
    {
        ffukyd(fptr, keyname, *(double *) value, 14, comm, status);
    }
    else if (datatype == TCOMPLEX)
    {
        ffukyc(fptr, keyname, (float *) value, 6, comm, status);
    }
    else if (datatype == TDBLCOMPLEX)
    {
        ffukym(fptr, keyname, (double *) value, 14, comm, status);
    }
    else
        *status = BAD_DATATYPE;

    return(*status);
} 
/*--------------------------------------------------------------------------*/
int ffukyu(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmkyu(fptr, keyname, comm, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffpkyu(fptr, keyname, comm, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffukys(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           char *value,       /* I - keyword value      */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */ 
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmkys(fptr, keyname, value, comm, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffpkls(fptr, keyname, value, comm, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffukyl(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           int value,         /* I - keyword value      */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmkyl(fptr, keyname, value, comm, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffpkyl(fptr, keyname, value, comm, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffukyj(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           long value,        /* I - keyword value      */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmkyj(fptr, keyname, value, comm, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffpkyj(fptr, keyname, value, comm, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffukyf(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           float value,       /* I - keyword value      */
           int decim,         /* I - no of decimals     */         
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmkyf(fptr, keyname, value, decim, comm, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffpkyf(fptr, keyname, value, decim, comm, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffukye(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           float value,       /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmkye(fptr, keyname, value, decim, comm, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffpkye(fptr, keyname, value, decim, comm, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffukyg(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           double value,      /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmkyg(fptr, keyname, value, decim, comm, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffpkyg(fptr, keyname, value, decim, comm, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffukyd(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           double value,      /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmkyd(fptr, keyname, value, decim, comm, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffpkyd(fptr, keyname, value, decim, comm, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffukfc(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           float *value,      /* I - keyword value      */
           int decim,         /* I - no of decimals     */         
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmkfc(fptr, keyname, value, decim, comm, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffpkfc(fptr, keyname, value, decim, comm, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffukyc(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           float *value,      /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmkyc(fptr, keyname, value, decim, comm, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffpkyc(fptr, keyname, value, decim, comm, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffukfm(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           double *value,     /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmkfm(fptr, keyname, value, decim, comm, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffpkfm(fptr, keyname, value, decim, comm, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffukym(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           double *value,     /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmkym(fptr, keyname, value, decim, comm, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffpkym(fptr, keyname, value, decim, comm, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffucrd(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           char *card,        /* I - card string value  */
           int *status)       /* IO - error status      */
{
    int tstatus;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    tstatus = *status;

    if (ffmcrd(fptr, keyname, card, status) == KEY_NO_EXIST)
    {
        *status = tstatus;
        ffprec(fptr, card, status);
    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmrec(fitsfile *fptr,    /* I - FITS file pointer               */
           int nkey,          /* I - number of the keyword to modify */
           char *card,        /* I - card string value               */
           int *status)       /* IO - error status                   */
{
    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    ffmaky(fptr, nkey+1, status);
    ffmkey(fptr, card, status);
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmcrd(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           char *card,        /* I - card string value  */
           int *status)       /* IO - error status      */
{
    char tcard[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgcrd(fptr, keyname, tcard, status) > 0)
        return(*status);

    ffmkey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmnam(fitsfile *fptr,    /* I - FITS file pointer     */
           char *oldname,     /* I - existing keyword name */
           char *newname,     /* I - new name for keyword  */
           int *status)       /* IO - error status         */
{
    char comm[FLEN_COMMENT];
    char value[FLEN_VALUE];
    char card[FLEN_CARD];
 
    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, oldname, value, comm, status) > 0)
        return(*status);

    ffmkky(newname, value, comm, card);  /* construct the card */
    ffmkey(fptr, card, status);  /* rewrite with new name */

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmcom(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char oldcomm[FLEN_COMMENT];
    char value[FLEN_VALUE];
    char card[FLEN_CARD];
 
    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, value, oldcomm, status) > 0)
        return(*status);

    ffmkky(keyname, value, comm, card);  /* construct the card */
    ffmkey(fptr, card, status);  /* rewrite with new comment */

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffpunt(fitsfile *fptr,    /* I - FITS file pointer   */
           char *keyname,     /* I - keyword name        */
           char *unit,        /* I - keyword unit string */
           int *status)       /* IO - error status       */
/*
    Write (put) the units string into the comment field of the existing
    keyword. This routine uses a local FITS convention (not defined in the
    official FITS standard) in which the units are enclosed in 
    square brackets following the '/' comment field delimiter, e.g.:

    KEYWORD =                   12 / [kpc] comment string goes here
*/
{
    char oldcomm[FLEN_COMMENT];
    char newcomm[FLEN_COMMENT];
    char value[FLEN_VALUE];
    char card[FLEN_CARD];
    char *loc;
    size_t len;
 
    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, value, oldcomm, status) > 0)
        return(*status);

    /* copy the units string to the new comment string if not null */
    if (*unit)
    {
        strcpy(newcomm, "[");
        strncat(newcomm, unit, 45);  /* max allowed length is about 45 chars */
        strcat(newcomm, "] ");
        len = strlen(newcomm);  
        len = FLEN_COMMENT - len - 1;  /* amount of space left in the field */
    }
    else
    {
        newcomm[0] = '\0';
        len = FLEN_COMMENT - 1;
    }

    if (oldcomm[0] == '[')  /* check for existing units field */
    {
        loc = strchr(oldcomm, ']');  /* look for the closing bracket */
        if (loc)
        {
            loc++;
            while (*loc == ' ')   /* skip any blank spaces */
               loc++;

            strncat(newcomm, loc, len);  /* concat remainder of comment */
        }
        else
        {
            strncat(newcomm, oldcomm, len);  /* append old comment onto new */
        }
    }
    else
    {
        strncat(newcomm, oldcomm, len);
    }

    ffmkky(keyname, value, newcomm, card);  /* construct the card */
    ffmkey(fptr, card, status);  /* rewrite with new units string */

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmkyu(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char oldcomm[FLEN_COMMENT];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, valstring, oldcomm, status) > 0)
        return(*status);                               /* get old comment */

    strcpy(valstring,"0");  /* create a dummy value string */

    if (!comm || comm[0] == '&')  /* preserve the current comment string */
        ffmkky(keyname, valstring, oldcomm, card);
    else
        ffmkky(keyname, valstring, comm, card);

    card[29] = ' ';        /* reset the dummy value string to a blank */
    ffmkey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmkys(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           char *value,       /* I - keyword value      */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
  /* NOTE: This routine does not support long continued strings */

    char oldval[FLEN_VALUE],valstring[FLEN_VALUE];
    char oldcomm[FLEN_COMMENT];
    char card[FLEN_CARD];
    int len, keypos;

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, oldval, oldcomm, status) > 0)
        return(*status);                               /* get old comment */

    ffs2c(value, valstring, status);   /* convert value to a string */

    if (!comm || comm[0] == '&')  /* preserve the current comment string */
        ffmkky(keyname, valstring, oldcomm, card);
    else
        ffmkky(keyname, valstring, comm, card);

    ffmkey(fptr, card, status); /* overwrite the previous keyword */

    keypos = ((((fptr->Fptr)->nextkey) - ((fptr->Fptr)->headstart[(fptr->Fptr)->curhdu])) / 80) + 1;

    /* check if old string value was continued over multiple keywords */
    ffc2s(oldval, valstring, status); /* remove quotes and trailing spaces */
    len = strlen(valstring);

    while (valstring[len - 1] == '&')  /* ampersand is continuation char */
    {
        ffgcnt(fptr, valstring, status);
        if (valstring)
        {
            ffdrec(fptr, keypos, status);  /* delete the continuation */
            len = strlen(valstring);
        }
        else   /* a null valstring indicates no continuation */
            len = 1;
    }

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmkyl(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           int value,         /* I - keyword value      */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char oldcomm[FLEN_COMMENT];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, valstring, oldcomm, status) > 0)
        return(*status);                               /* get old comment */

    ffl2c(value, valstring, status);   /* convert value to a string */

    if (!comm || comm[0] == '&')  /* preserve the current comment string */
        ffmkky(keyname, valstring, oldcomm, card);
    else
        ffmkky(keyname, valstring, comm, card);

    ffmkey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmkyj(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           long value,        /* I - keyword value      */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char oldcomm[FLEN_COMMENT];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, valstring, oldcomm, status) > 0)
        return(*status);                               /* get old comment */

    ffi2c(value, valstring, status);   /* convert value to a string */

    if (!comm || comm[0] == '&')  /* preserve the current comment string */
        ffmkky(keyname, valstring, oldcomm, card);
    else
        ffmkky(keyname, valstring, comm, card);

    ffmkey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmkyf(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           float value,       /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char oldcomm[FLEN_COMMENT];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, valstring, oldcomm, status) > 0)
        return(*status);                               /* get old comment */

    ffr2f(value, decim, valstring, status);   /* convert value to a string */

    if (!comm || comm[0] == '&')  /* preserve the current comment string */
        ffmkky(keyname, valstring, oldcomm, card);
    else
        ffmkky(keyname, valstring, comm, card);

    ffmkey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmkye(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           float value,       /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char oldcomm[FLEN_COMMENT];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, valstring, oldcomm, status) > 0)
        return(*status);                               /* get old comment */

    ffr2e(value, decim, valstring, status);   /* convert value to a string */

    if (!comm || comm[0] == '&')  /* preserve the current comment string */
        ffmkky(keyname, valstring, oldcomm, card);
    else
        ffmkky(keyname, valstring, comm, card);

    ffmkey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmkyg(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           double value,      /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char oldcomm[FLEN_COMMENT];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, valstring, oldcomm, status) > 0)
        return(*status);                               /* get old comment */

    ffd2f(value, decim, valstring, status);   /* convert value to a string */

    if (!comm || comm[0] == '&')  /* preserve the current comment string */
        ffmkky(keyname, valstring, oldcomm, card);
    else
        ffmkky(keyname, valstring, comm, card);

    ffmkey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmkyd(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           double value,      /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char oldcomm[FLEN_COMMENT];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, valstring, oldcomm, status) > 0)
        return(*status);                               /* get old comment */

    ffd2e(value, decim, valstring, status);   /* convert value to a string */

    if (!comm || comm[0] == '&')  /* preserve the current comment string */
        ffmkky(keyname, valstring, oldcomm, card);
    else
        ffmkky(keyname, valstring, comm, card);

    ffmkey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmkfc(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           float *value,      /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE], tmpstring[FLEN_VALUE];
    char oldcomm[FLEN_COMMENT];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, valstring, oldcomm, status) > 0)
        return(*status);                               /* get old comment */

    strcpy(valstring, "(" );
    ffr2f(value[0], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ", ");
    ffr2f(value[1], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ")");

    if (!comm || comm[0] == '&')  /* preserve the current comment string */
        ffmkky(keyname, valstring, oldcomm, card);
    else
        ffmkky(keyname, valstring, comm, card);

    ffmkey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmkyc(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           float *value,      /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE], tmpstring[FLEN_VALUE];
    char oldcomm[FLEN_COMMENT];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, valstring, oldcomm, status) > 0)
        return(*status);                               /* get old comment */

    strcpy(valstring, "(" );
    ffr2e(value[0], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ", ");
    ffr2e(value[1], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ")");

    if (!comm || comm[0] == '&')  /* preserve the current comment string */
        ffmkky(keyname, valstring, oldcomm, card);
    else
        ffmkky(keyname, valstring, comm, card);

    ffmkey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmkfm(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           double *value,     /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE], tmpstring[FLEN_VALUE];
    char oldcomm[FLEN_COMMENT];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, valstring, oldcomm, status) > 0)
        return(*status);                               /* get old comment */

    strcpy(valstring, "(" );
    ffd2f(value[0], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ", ");
    ffd2f(value[1], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ")");

    if (!comm || comm[0] == '&')  /* preserve the current comment string */
        ffmkky(keyname, valstring, oldcomm, card);
    else
        ffmkky(keyname, valstring, comm, card);

    ffmkey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffmkym(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           double *value,     /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE], tmpstring[FLEN_VALUE];
    char oldcomm[FLEN_COMMENT];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, valstring, oldcomm, status) > 0)
        return(*status);                               /* get old comment */

    strcpy(valstring, "(" );
    ffd2e(value[0], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ", ");
    ffd2e(value[1], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ")");

    if (!comm || comm[0] == '&')  /* preserve the current comment string */
        ffmkky(keyname, valstring, oldcomm, card);
    else
        ffmkky(keyname, valstring, comm, card);

    ffmkey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikyu(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
/*
  Insert a null-valued keyword and comment into the FITS header.  
*/
{
    char valstring[FLEN_VALUE];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    strcpy(valstring,"0");  /* create a dummy value string */
    ffmkky(keyname, valstring, comm, card);  /* construct the keyword*/
    card[29] = ' ';        /* reset the dummy value string to a blank */
    ffikey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikys(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           char *value,       /* I - keyword value      */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    ffs2c(value, valstring, status);   /* put quotes around the string */
    ffmkky(keyname, valstring, comm, card);  /* construct the keyword*/
    ffikey(fptr, card, status);

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikyl(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           int value,         /* I - keyword value      */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    ffl2c(value, valstring, status);   /* convert logical to 'T' or 'F' */
    ffmkky(keyname, valstring, comm, card);  /* construct the keyword*/
    ffikey(fptr, card, status);  /* write the keyword*/

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikyj(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           long value,        /* I - keyword value      */
           char *comm,        /* I - keyword comment    */
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    ffi2c(value, valstring, status);   /* convert to formatted string */
    ffmkky(keyname, valstring, comm, card);  /* construct the keyword*/
    ffikey(fptr, card, status);  /* write the keyword*/

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikyf(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           float value,       /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */ 
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    ffr2f(value, decim, valstring, status);   /* convert to formatted string */
    ffmkky(keyname, valstring, comm, card);  /* construct the keyword*/
    ffikey(fptr, card, status);  /* write the keyword*/

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikye(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           float value,       /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */ 
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    ffr2e(value, decim, valstring, status);   /* convert to formatted string */
    ffmkky(keyname, valstring, comm, card);  /* construct the keyword*/
    ffikey(fptr, card, status);  /* write the keyword*/

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikyg(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           double value,      /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */ 
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    ffd2f(value, decim, valstring, status);   /* convert to formatted string */
    ffmkky(keyname, valstring, comm, card);  /* construct the keyword*/
    ffikey(fptr, card, status);  /* write the keyword*/

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikyd(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           double value,      /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */ 
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    ffd2e(value, decim, valstring, status);   /* convert to formatted string */
    ffmkky(keyname, valstring, comm, card);  /* construct the keyword*/
    ffikey(fptr, card, status);  /* write the keyword*/

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikfc(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           float *value,      /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */ 
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE], tmpstring[FLEN_VALUE];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    strcpy(valstring, "(" );
    ffr2f(value[0], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ", ");
    ffr2f(value[1], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ")");

    ffmkky(keyname, valstring, comm, card);  /* construct the keyword*/
    ffikey(fptr, card, status);  /* write the keyword*/

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikyc(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           float *value,      /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */ 
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE], tmpstring[FLEN_VALUE];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    strcpy(valstring, "(" );
    ffr2e(value[0], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ", ");
    ffr2e(value[1], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ")");

    ffmkky(keyname, valstring, comm, card);  /* construct the keyword*/
    ffikey(fptr, card, status);  /* write the keyword*/

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikfm(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           double *value,     /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */ 
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE], tmpstring[FLEN_VALUE];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);


    strcpy(valstring, "(" );
    ffd2f(value[0], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ", ");
    ffd2f(value[1], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ")");

    ffmkky(keyname, valstring, comm, card);  /* construct the keyword*/
    ffikey(fptr, card, status);  /* write the keyword*/

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikym(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           double *value,     /* I - keyword value      */
           int decim,         /* I - no of decimals     */
           char *comm,        /* I - keyword comment    */ 
           int *status)       /* IO - error status      */
{
    char valstring[FLEN_VALUE], tmpstring[FLEN_VALUE];
    char card[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    strcpy(valstring, "(" );
    ffd2e(value[0], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ", ");
    ffd2e(value[1], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ")");

    ffmkky(keyname, valstring, comm, card);  /* construct the keyword*/
    ffikey(fptr, card, status);  /* write the keyword*/

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffirec(fitsfile *fptr,    /* I - FITS file pointer              */
           int nkey,          /* I - position to insert new keyword */
           char *card,        /* I - card string value              */
           int *status)       /* IO - error status                  */
{
    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    ffmaky(fptr, nkey, status);  /* move to insert position */
    ffikey(fptr, card, status);  /* insert the keyword card */

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffikey(fitsfile *fptr,    /* I - FITS file pointer  */
           char *card,        /* I - card string value  */
           int *status)       /* IO - error status      */
/*
  insert a keyword at the position of (fptr->Fptr)->nextkey
*/
{
    int ii, len, nshift;
    long nblocks, bytepos;
    char *inbuff, *outbuff, *tmpbuff, buff1[FLEN_CARD], buff2[FLEN_CARD];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    /* reset position to the correct HDU if necessary */
    if (fptr->HDUposition != (fptr->Fptr)->curhdu)
        ffmahd(fptr, (fptr->HDUposition) + 1, NULL, status);

    if ( ((fptr->Fptr)->datastart - (fptr->Fptr)->headend) == 80) /* only room for END card */
    {
        nblocks = 1;
        if (ffiblk(fptr, nblocks, 0, status) > 0) /* add new 2880-byte block*/
            return(*status);  
    }

    nshift=( (fptr->Fptr)->headend - (fptr->Fptr)->nextkey ) / 80; /* no. keywords to shift */

    strncpy(buff2, card, 80);     /* copy card to output buffer */

    len = strlen(buff2);
    for (ii=len; ii < 80; ii++)   /* fill buffer with spaces if necessary */
        buff2[ii] = ' ';

    inbuff = buff1;
    outbuff = buff2;

    bytepos = (fptr->Fptr)->nextkey;           /* pointer to next keyword in header */
    ffmbyt(fptr, bytepos, REPORT_EOF, status);

    for (ii = 0; ii < nshift; ii++) /* shift each keyword down one position */
    {
        ffgbyt(fptr, 80, inbuff, status);   /* read the current keyword */

        ffmbyt(fptr, bytepos, REPORT_EOF, status); /* move back */
        ffpbyt(fptr, 80, outbuff, status);  /* overwrite with other buffer */

        tmpbuff = inbuff;   /* swap input and output buffers */
        inbuff = outbuff;
        outbuff = tmpbuff;

        bytepos += 80;
    }

    ffpbyt(fptr, 80, outbuff, status);  /* write the final keyword */

    (fptr->Fptr)->headend += 80; /* increment the position of the END keyword */
    (fptr->Fptr)->nextkey += 80; /* increment the pointer to next keyword */

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffdkey(fitsfile *fptr,    /* I - FITS file pointer  */
           char *keyname,     /* I - keyword name       */
           int *status)       /* IO - error status      */
/*
  delete a specified header keyword
*/
{
    int keypos, len;
    char valstring[FLEN_VALUE], comm[FLEN_COMMENT], value[FLEN_VALUE];
    char message[FLEN_ERRMSG];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (ffgkey(fptr, keyname, valstring, comm, status) > 0) /* read keyword */
    {
        sprintf(message, "Could not find the %s keyword to delete (ffdkey)",
                keyname);
        ffpmsg(message);
        return(*status);
    }

    /* calc position of keyword in header */
    keypos = (((fptr->Fptr)->nextkey) - ((fptr->Fptr)->headstart[(fptr->Fptr)->curhdu])) / 80;

    ffdrec(fptr, keypos, status);  /* delete the keyword */

    /* check for string value which may be continued over multiple keywords */
    ffc2s(valstring, value, status);   /* remove quotes and trailing spaces */
    len = strlen(value);

    while (value[len - 1] == '&')  /* ampersand used as continuation char */
    {
        ffgcnt(fptr, value, status);
        if (value)
        {
            ffdrec(fptr, keypos, status);  /* delete the keyword */
            len = strlen(value);
        }
        else   /* a null valstring indicates no continuation */
            len = 1;

    }
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffdrec(fitsfile *fptr,   /* I - FITS file pointer  */
           int keypos,       /* I - position in header of keyword to delete */
           int *status)      /* IO - error status      */
/*
  Delete a header keyword at position keypos. The 1st keyword is at keypos=1.
*/
{
    int ii, nshift;
    long bytepos;
    char *inbuff, *outbuff, *tmpbuff, buff1[81], buff2[81];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    /* reset position to the correct HDU if necessary */
    if (fptr->HDUposition != (fptr->Fptr)->curhdu)
        ffmahd(fptr, (fptr->HDUposition) + 1, NULL, status);

    if (keypos < 1 ||
        keypos > (fptr->Fptr)->headend - (fptr->Fptr)->headstart[(fptr->Fptr)->curhdu] / 80 )
        return(*status = KEY_OUT_BOUNDS);

    (fptr->Fptr)->nextkey = (fptr->Fptr)->headstart[(fptr->Fptr)->curhdu] + (keypos - 1) * 80;

    nshift=( (fptr->Fptr)->headend - (fptr->Fptr)->nextkey ) / 80; /* no. keywords to shift */

    bytepos = (fptr->Fptr)->headend - 80;  /* last keyword in header */  

    /* construct a blank keyword */
    strcpy(buff2, "                                        ");
    strcat(buff2, "                                        ");
    inbuff  = buff1;
    outbuff = buff2;

    for (ii = 0; ii < nshift; ii++) /* shift each keyword up one position */
    {

        ffmbyt(fptr, bytepos, REPORT_EOF, status);
        ffgbyt(fptr, 80, inbuff, status);   /* read the current keyword */

        ffmbyt(fptr, bytepos, REPORT_EOF, status);
        ffpbyt(fptr, 80, outbuff, status);  /* overwrite with next keyword */

        tmpbuff = inbuff;   /* swap input and output buffers */
        inbuff = outbuff;
        outbuff = tmpbuff;

        bytepos -= 80;
    }

    (fptr->Fptr)->headend -= 80; /* decrement the position of the END keyword */
    return(*status);
}

