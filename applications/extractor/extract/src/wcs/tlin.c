/*============================================================================
*
*   WCSLIB - an implementation of the FITS WCS proposal.
*   Copyright (C) 1995,1996 Mark Calabretta
*
*   This program is free software; you can redistribute it and/or modify it
*   under the terms of the GNU General Public License as published by the
*   Free Software Foundation; either version 2 of the License, or (at your
*   option) any later version.
*
*   This program is distributed in the hope that it will be useful, but
*   WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*   General Public License for more details.
*
*   You should have received a copy of the GNU General Public License along
*   with this library; if not, write to the Free Software Foundation, Inc.,
*   675 Mass Ave, Cambridge, MA 02139, USA.
*
*   Correspondence concerning WCSLIB may be directed to:
*      Internet email: mcalabre@atnf.csiro.au
*      Postal address: Dr. Mark Calabretta,
*                      Australia Telescope National Facility,
*                      P.O. Box 76,
*                      Epping, NSW, 2121,
*                      AUSTRALIA
*
*=============================================================================
*
*   tlin tests the linear transformation routines supplied with WCSLIB.
*
*   $Id: tlin.c,v 2.1 1996/05/07 20:27:51 mcalabre Exp $
*---------------------------------------------------------------------------*/

#include <stdio.h>
#include "lin.h"

double crpix[5] =  {256.0, 256.0,  64.0, 128.0,   1.0};
double pc[5][5] = {{  1.0,   0.5,   0.0,   0.0,   0.0},
                   {  0.5,   1.0,   0.0,   0.0,   0.0},
                   {  0.0,   0.0,   1.0,   0.0,   0.0},
                   {  0.0,   0.0,   0.0,   1.0,   0.0},
                   {  0.0,   0.0,   0.0,   0.0,   1.0}};
double cdelt[5] =  {  1.2,   2.3,   3.4,   4.5,   5.6};
double pix[5] =    {303.0, 265.0, 112.4, 144.5,  28.2};
double img[5];

main()

{
   int err, j;
   struct linprm lin;

 
   printf("\nTesting WCSLIB linear transformation routines\n");
   printf("---------------------------------------------\n");

   /* List error messages. */
   printf("\nList of linset error codes:\n");
   for (j = 1; j <=2 ; j++) {
      printf("   %d: %s.\n", j, linset_errmsg[j]);
   }
 
   printf("\nList of linfwd error codes:\n");
   for (j = 1; j <=2 ; j++) {
      printf("   %d: %s.\n", j, linfwd_errmsg[j]);
   }
 
   printf("\nList of linrev error codes:\n");
   for (j = 1; j <=2 ; j++) {
      printf("   %d: %s.\n", j, linrev_errmsg[j]);
   }

   printf("\n\n");


   lin.flag  = 0;
   lin.naxis = 5;
   lin.crpix = crpix;
   lin.pc    = (double*)pc;
   lin.cdelt = cdelt;

   printf("pix:");
   for (j = 0; j < 5; j++) {
      printf("%14.8f", pix[j]);
   }
   printf("\n");

   if (err = linrev(pix, &lin, img)) {
      printf("linrev error %d\n",err);
      return 1;
   }

   printf("img:");
   for (j = 0; j < 5; j++) {
      printf("%14.8f", img[j]);
   }
   printf("\n");

   if (err = linfwd(img, &lin, pix)) {
      printf("linfwd error %d\n",err);
      return 1;
   }

   printf("pix:");
   for (j = 0; j < 5; j++) {
      printf("%14.8f", pix[j]);
   }
   printf("\n");

   if (err = linrev(pix, &lin, img)) {
      printf("linrev error %d\n",err);
      return 1;
   }

   printf("img:");
   for (j = 0; j < 5; j++) {
      printf("%14.8f", img[j]);
   }
   printf("\n");

   return 0;
}
