
/*  This file, getcol.c, contains routines that read data elements from    */
/*  a FITS image or table.  There are generic datatype routines.           */

/*  The FITSIO software was written by William Pence at the High Energy    */
/*  Astrophysic Science Archive Research Center (HEASARC) at the NASA      */
/*  Goddard Space Flight Center.  Users shall not, without prior written   */
/*  permission of the U.S. Government,  establish a claim to statutory     */
/*  copyright.  The Government and others acting on its behalf, shall have */
/*  a royalty-free, non-exclusive, irrevocable,  worldwide license for     */
/*  Government purposes to publish, distribute, translate, copy, exhibit,  */
/*  and perform such material.                                             */

#include "fitsio2.h"
/*--------------------------------------------------------------------------*/
int ffgpv(  fitsfile *fptr,   /* I - FITS file pointer                       */
            int  datatype,    /* I - datatype of the value                   */
            long firstelem,   /* I - first vector element to read (1 = 1st)  */
            long nelem,       /* I - number of values to read                */
            void *nulval,     /* I - value for undefined pixels              */
            void *array,      /* O - array of values that are returned       */
            int  *anynul,     /* O - set to 1 if any values are null; else 0 */
            int  *status)     /* IO - error status                           */
/*
  Read an array of values from the primary array. The datatype of the
  input array is defined by the 2nd argument.  Data conversion
  and scaling will be performed if necessary (e.g, if the datatype of
  the FITS array is not the same as the array being read).
  Undefined elements will be set equal to NULVAL, unless NULVAL=0
  in which case no checking for undefined values will be performed.
  ANYNUL is returned with a value of .true. if any pixels are undefined.
*/
{
    long row = 1;
    char cdummy;

    if (*status > 0 || nelem == 0)   /* inherit input status value if > 0 */
        return(*status);

    /*
      the primary array is represented as a binary table:
      each group of the primary array is a row in the table,
      where the first column contains the group parameters
      and the second column contains the image itself.
    */

    if (datatype == TBYTE)
    {
      if (nulval == 0)
        ffgclb(fptr, 2, row, firstelem, nelem, 1, 1, 0,
              (unsigned char *) array, &cdummy, anynul, status);
      else
        ffgclb(fptr, 2, row, firstelem, nelem, 1, 1, *(unsigned char *) nulval,
              (unsigned char *) array, &cdummy, anynul, status);
    }
    else if (datatype == TUSHORT)
    {
      if (nulval == 0)
        ffgclui(fptr, 2, row, firstelem, nelem, 1, 1, 0,
               (unsigned short *) array, &cdummy, anynul, status);
      else
        ffgclui(fptr, 2, row, firstelem, nelem, 1, 1,
               *(unsigned short *) nulval,
               (unsigned short *) array, &cdummy, anynul, status);
    }
    else if (datatype == TSHORT)
    {
      if (nulval == 0)
        ffgcli(fptr, 2, row, firstelem, nelem, 1, 1, 0,
              (short *) array, &cdummy, anynul, status);
      else
        ffgcli(fptr, 2, row, firstelem, nelem, 1, 1, *(short *) nulval,
              (short *) array, &cdummy, anynul, status);
    }
    else if (datatype == TUINT)
    {
      if (nulval == 0)
        ffgcluk(fptr, 2, row, firstelem, nelem, 1, 1, 0,
              (unsigned int *) array, &cdummy, anynul, status);
      else
        ffgcluk(fptr, 2, row, firstelem, nelem, 1, 1, *(unsigned int *) nulval,
              (unsigned int *) array, &cdummy, anynul, status);
    }
    else if (datatype == TINT)
    {
      if (nulval == 0)
        ffgclk(fptr, 2, row, firstelem, nelem, 1, 1, 0,
              (int *) array, &cdummy, anynul, status);
      else
        ffgclk(fptr, 2, row, firstelem, nelem, 1, 1, *(int *) nulval,
              (int *) array, &cdummy, anynul, status);
    }
    else if (datatype == TULONG)
    {
      if (nulval == 0)
        ffgcluj(fptr, 2, row, firstelem, nelem, 1, 1, 0,
              (unsigned long *) array, &cdummy, anynul, status);
      else
        ffgcluj(fptr, 2, row, firstelem, nelem, 1, 1, *(unsigned long *) nulval,
              (unsigned long *) array, &cdummy, anynul, status);
    }
    else if (datatype == TLONG)
    {
      if (nulval == 0)
        ffgclj(fptr, 2, row, firstelem, nelem, 1, 1, 0,
              (long *) array, &cdummy, anynul, status);
      else
        ffgclj(fptr, 2, row, firstelem, nelem, 1, 1, *(long *) nulval,
              (long *) array, &cdummy, anynul, status);
    }
    else if (datatype == TFLOAT)
    {
      if (nulval == 0)
        ffgcle(fptr, 2, row, firstelem, nelem, 1, 1, 0.,
              (float *) array, &cdummy, anynul, status);
      else
        ffgcle(fptr, 2, row, firstelem, nelem, 1, 1, *(float *) nulval,
              (float *) array, &cdummy, anynul, status);
    }
    else if (datatype == TDOUBLE)
    {
      if (nulval == 0)
        ffgcld(fptr, 2, row, firstelem, nelem, 1, 1, 0.,
              (double *) array, &cdummy, anynul, status);
      else
        ffgcld(fptr, 2, row, firstelem, nelem, 1, 1, *(double *) nulval,
              (double *) array, &cdummy, anynul, status);
    }
    else
      *status = BAD_DATATYPE;

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffgcv(  fitsfile *fptr,   /* I - FITS file pointer                       */
            int  datatype,    /* I - datatype of the value                   */
            int  colnum,      /* I - number of column to write (1 = 1st col) */
            long  firstrow,   /* I - first row to write (1 = 1st row)        */
            long  firstelem,  /* I - first vector element to read (1 = 1st)  */
            long nelem,       /* I - number of values to read                */
            void *nulval,     /* I - value for undefined pixels              */
            void *array,      /* O - array of values that are returned       */
            int  *anynul,     /* O - set to 1 if any values are null; else 0 */
            int  *status)     /* IO - error status                           */
/*
  Read an array of values from a table column. The datatype of the
  input array is defined by the 2nd argument.  Data conversion
  and scaling will be performed if necessary (e.g, if the datatype of
  the FITS array is not the same as the array being read).
  Undefined elements will be set equal to NULVAL, unless NULVAL=0
  in which case no checking for undefined values will be performed.
  ANYNUL is returned with a value of true if any pixels are undefined.
*/
{
    char cdummy[2];

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    if (datatype == TBIT)
    {
      ffgcx(fptr, colnum, firstrow, firstelem, nelem, (char *) array, status);
    }
    else if (datatype == TBYTE)
    {
      if (nulval == 0)
        ffgclb(fptr, colnum, firstrow, firstelem, nelem, 1, 1, 0,
              (unsigned char *) array, cdummy, anynul, status);
      else
       ffgclb(fptr, colnum, firstrow, firstelem, nelem, 1, 1, *(unsigned char *)
              nulval, (unsigned char *) array, cdummy, anynul, status);
    }
    else if (datatype == TUSHORT)
    {
      if (nulval == 0)
        ffgclui(fptr, colnum, firstrow, firstelem, nelem, 1, 1, 0,
               (unsigned short *) array, cdummy, anynul, status);
      else
        ffgclui(fptr, colnum, firstrow, firstelem, nelem, 1, 1,
               *(unsigned short *) nulval,
               (unsigned short *) array, cdummy, anynul, status);
    }
    else if (datatype == TSHORT)
    {
      if (nulval == 0)
        ffgcli(fptr, colnum, firstrow, firstelem, nelem, 1, 1, 0,
              (short *) array, cdummy, anynul, status);
      else
        ffgcli(fptr, colnum, firstrow, firstelem, nelem, 1, 1, *(short *)
              nulval, (short *) array, cdummy, anynul, status);
    }
    else if (datatype == TUINT)
    {
      if (nulval == 0)
        ffgcluk(fptr, colnum, firstrow, firstelem, nelem, 1, 1, 0,
              (unsigned int *) array, cdummy, anynul, status);
      else
        ffgcluk(fptr, colnum, firstrow, firstelem, nelem, 1, 1,
         *(unsigned int *) nulval, (unsigned int *) array, cdummy, anynul,
         status);
    }
    else if (datatype == TINT)
    {
      if (nulval == 0)
        ffgclk(fptr, colnum, firstrow, firstelem, nelem, 1, 1, 0,
              (int *) array, cdummy, anynul, status);
      else
        ffgclk(fptr, colnum, firstrow, firstelem, nelem, 1, 1, *(int *)
            nulval, (int *) array, cdummy, anynul, status);
    }
    else if (datatype == TULONG)
    {
      if (nulval == 0)
        ffgcluj(fptr, colnum, firstrow, firstelem, nelem, 1, 1, 0,
               (unsigned long *) array, cdummy, anynul, status);
      else
        ffgcluj(fptr, colnum, firstrow, firstelem, nelem, 1, 1,
               *(unsigned long *) nulval, 
               (unsigned long *) array, cdummy, anynul, status);
    }
    else if (datatype == TLONG)
    {
      if (nulval == 0)
        ffgclj(fptr, colnum, firstrow, firstelem, nelem, 1, 1, 0,
              (long *) array, cdummy, anynul, status);
      else
        ffgclj(fptr, colnum, firstrow, firstelem, nelem, 1, 1, *(long *)
              nulval, (long *) array, cdummy, anynul, status);
    }
    else if (datatype == TFLOAT)
    {
      if (nulval == 0)
        ffgcle(fptr, colnum, firstrow, firstelem, nelem, 1, 1, 0.,
              (float *) array, cdummy, anynul, status);
      else
      ffgcle(fptr, colnum, firstrow, firstelem, nelem, 1, 1, *(float *)
               nulval,(float *) array, cdummy, anynul, status);
    }
    else if (datatype == TDOUBLE)
    {
      if (nulval == 0)
        ffgcld(fptr, colnum, firstrow, firstelem, nelem, 1, 1, 0.,
              (double *) array, cdummy, anynul, status);
      else
        ffgcld(fptr, colnum, firstrow, firstelem, nelem, 1, 1, *(double *)
              nulval, (double *) array, cdummy, anynul, status);
    }
    else if (datatype == TCOMPLEX)
    {
      if (nulval == 0)
        ffgcle(fptr, colnum, firstrow, (firstelem - 1) * 2 + 1, nelem * 2,
           1, 1, 0., (float *) array, cdummy, anynul, status);
      else
        ffgcle(fptr, colnum, firstrow, (firstelem - 1) * 2 + 1, nelem * 2,
           1, 1, *(float *) nulval, (float *) array, cdummy, anynul, status);
    }
    else if (datatype == TDBLCOMPLEX)
    {
      if (nulval == 0)
        ffgcld(fptr, colnum, firstrow, (firstelem - 1) * 2 + 1, nelem * 2, 
         1, 1, 0., (double *) array, cdummy, anynul, status);
      else
        ffgcld(fptr, colnum, firstrow, (firstelem - 1) * 2 + 1, nelem * 2, 
         1, 1, *(double *) nulval, (double *) array, cdummy, anynul, status);
    }

    else if (datatype == TLOGICAL)
    {
      if (nulval == 0)
        ffgcll(fptr, colnum, firstrow, firstelem, nelem, 1, 0,
          (char *) array, cdummy, anynul, status);
      else
        ffgcll(fptr, colnum, firstrow, firstelem, nelem, 1, *(char *) nulval,
          (char *) array, cdummy, anynul, status);
    }
    else if (datatype == TSTRING)
    {
      if (nulval == 0)
      {
        cdummy[0] = '\0';
        ffgcls(fptr, colnum, firstrow, firstelem, nelem, 1, 
             cdummy, (char **) array, cdummy, anynul, status);
      }
      else
        ffgcls(fptr, colnum, firstrow, firstelem, nelem, 1, (char *)
             nulval, (char **) array, cdummy, anynul, status);
    }
    else
      *status = BAD_DATATYPE;

    return(*status);
}

