/*
;+
;	W B S W A P
;
;	SUBROUTINE WBSWAP (ARRAY,LWORDS)
;
;	Exchanges the byte order in an array of 32 bit longwords
;       so that it matches the convention used by non-DEC
;	machines.  This is based on an MTPCKG routine written
;	by Bill Sebok.
;
;	Parameters -    (">" input, "!" modified, "<" output)
;
;	(!) ARRAY    (Integer*4 array ARRAY(LWORDS)) The array
;		     to be swapped.
;	(>) LWORDS   (Integer) The number of longwords in ARRAY
;
;	Subroutines / functions used - None
;
;	Modified:
;
;	30th April 1986  KS / AAO.  Psect name changed.
;       21st Nov   1988. CKL / CIT. Converted to C for Sun.
;        8th April 1990. SNS / CIT. NULL version added for VMS.
;        5th Feb   1993. BDC / UNSW DECstation-specific version.
;	 4th Mar   1993. KS / AAO.  Now compiles for any machine, using
:                        the 'byteswap' flag.
;        3th May   1999. RPT / JAC  Rip-off Figaro GEN_WBSWAP: strip
;                        preprocessor code since SPECX allows optional
;                        swapping.
;+
*/

void wbswap_(array,lwords)
unsigned long int array[];
long int *lwords;
{

   /* #ifdef byteswap */

   int i;

   for (i = 0; i < *lwords; i++)
      array[i] = (array[i] << 24 ) | (array[i] >> 24) |
                 ((array[i] & 0x00FF00) << 8) |
                 ((array[i] & 0xFF0000) >> 8);

   /* #endif */

}
