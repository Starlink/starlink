/*
                                  fitscheck.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*       Part of:        The LDAC Tools
*
*       Author:         E.BERTIN (IAP)
*
*       Contents:       Functions related to file integrity
*
*       Last modify:    15/08/2003
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef HAVE_CONFIG_H
#include	"config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fitscat_defs.h"
#include "fitscat.h"

#define	ENCODE_OFFSET	0x30
unsigned int	exclude[13] = {0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x40,
				0x5b, 0x5c, 0x5d, 0x5e, 0x5f, 0x60};

/****** encode_checksum *****************************************************
PROTO	void encode_checksum(unsigned int sum, char *str)
PURPOSE	Encode a checksum to ASCII
INPUT	Checksum,
	Destination string.
OUTPUT	-.
NOTES	Straightforward copy of  Seaman & Pence 1995
	(ftp://iraf.noao.edu/misc/checksum/).
AUTHOR	E. Bertin (IAP)
VERSION	08/05/2001
 ***/
void	encode_checksum(unsigned int sum, char *str)

  {
   int	ch[4],
	i,j,k, byte, check;

  for (i=0; i<4; i++)
    {
/*-- Each byte becomes four */
    byte = (sum << 8*i) >> 24;
    ch[0] = (ch[1] = ch[2] = ch[3] = byte/4 + ENCODE_OFFSET) + (byte%4);
    for (check=1; check;)		/* avoid ASCII punctuation */
      for (check=k=0; k<13; k++)
        {
        if (ch[0]==exclude[k] || ch[1]==exclude[k])
          {
          ch[0]++;
          ch[1]--;
          check++;
          }
        if (ch[2]==exclude[k] || ch[3]==exclude[k])
          {
          ch[2]++;
          ch[3]--;
          check++;
          }
        }
    for (j=0; j<4; j++)			/* assign the bytes */
      str[(4*j+i+1)%16] = ch[j];	/* permute the bytes for FITS */
    }
  str[16] = 0;

  return;
  }


/****** decode_checksum *****************************************************
PROTO	unsigned int decode_checksum(char *str)
PURPOSE	Decode an ASCII checksum
INPUT	Checksum string.
OUTPUT	Checksum.
NOTES	Straightforward copy of  Seaman & Pence 1995
	(ftp://iraf.noao.edu/misc/checksum/).
AUTHOR	E. Bertin (IAP)
VERSION	08/05/2001
 ***/
unsigned int	decode_checksum(char *str)

  {
   char			cbuf[16];
   unsigned short	*sbuf,
			los,his;
   unsigned int		hi,lo, hicarry,locarry;
   int			i;

/* Remove the permuted FITS byte alignment and the ASCII 0 offset */
  for (i=0; i<16; i++)
    cbuf[i] = str[(i+1)%16] - 0x30;
  sbuf = (unsigned short *)cbuf;
  hi = lo = 0;
  if (bswapflag)
    for (i=4; i--;)
      {
      his = *(sbuf++);
      los = *(sbuf++);
      hi += (*((unsigned char *)&his)<<8) + *((unsigned char *)&his+1);
      lo += (*((unsigned char *)&los)<<8) + *((unsigned char *)&los+1);
      }
  else
    for (i=4; i--;)
      {
      hi += *(sbuf++);
      lo += *(sbuf++);
      }

  hicarry = hi>>16;
  locarry = lo>>16;
  while (hicarry || locarry)
    {
    hi = (hi & 0xffff) + locarry;
    lo = (lo & 0xffff) + hicarry;
    hicarry = hi >> 16;
    locarry = lo >> 16;
    }

  return (hi<<16) + lo;
  }


/****** compute_blocksum *****************************************************
PROTO	unsigned int compute_blocksum(char *buf, unsigned int sum)
PURPOSE	Compute the checksum of a FITS block (2880 bytes)
INPUT	Pointer to the block,
	The previous checksum.
OUTPUT	The new computed checksum.
NOTES	From Seaman & Pence 1995 (ftp://iraf.noao.edu/misc/checksum/). But
	contrarily to what is stated by the authors, the original algorithm
	depends on the endianity of the machine. The routine below adds
	support for ix386-like processors (non-IEEE).
AUTHOR	E. Bertin (IAP)
VERSION	08/05/2001
 ***/
unsigned int	compute_blocksum(char *buf, unsigned int sum)
  {
   unsigned short	*sbuf,
			his,los;
   unsigned int		hi,lo, hicarry,locarry;
   int			i;

  sbuf = (unsigned short *)buf;
  hi = (sum >> 16);
  lo = (sum << 16) >> 16;
  if (bswapflag)
     for (i=FBSIZE/4; i--;)
      {
      his = *(sbuf++);
      los = *(sbuf++);
      hi += (*((unsigned char *)&his)<<8) + *((unsigned char *)&his+1);
      lo += (*((unsigned char *)&los)<<8) + *((unsigned char *)&los+1);
      }
  else
    for (i=FBSIZE/4; i--;)
      {
      hi += *(sbuf++);
      lo += *(sbuf++);
      }

  hicarry = hi>>16;     /* fold carry bits in */
  locarry = lo>>16;
  while (hicarry || locarry)
    {
    hi = (hi & 0xFFFF) + locarry;
    lo = (lo & 0xFFFF) + hicarry;
    hicarry = hi >> 16;
    locarry = lo >> 16;
    }

  return (hi << 16) + lo;
  }


/****** compute_bodysum *****************************************************
PROTO	unsigned int compute_bodysum(tabstruct *tab, unsigned int sum)
PURPOSE	Compute the checksum of a FITS body
INPUT	Pointer to the tab,
	Checksum from a previous iteration.
OUTPUT	The computed checksum.
NOTES	-.
AUTHOR	E. Bertin (IAP)
VERSION	15/08/2003
 ***/
unsigned int	compute_bodysum(tabstruct *tab, unsigned int sum)
  {
   catstruct	*cat;
   char		*buf;
   KINGSIZE_T	size;
   int		n, nblock;

/* FITS data are generally padded */
  nblock = (tab->tabsize+FBSIZE-1)/FBSIZE;
/* 2 cases: either the data are in memory or still on disk */
  if (tab->bodybuf)
    {
/*-- In memory: they are probably not padded */
    buf = (char *)tab->bodybuf;
    for (n=nblock-1; n--; buf+=FBSIZE)
      sum = compute_blocksum(buf, sum);
    if ((size=PADEXTRA(tab->tabsize)))
      {
      QCALLOC(buf, char, FBSIZE);
      size = FBSIZE-size;
      memcpy(buf, (char *)tab->bodybuf+tab->tabsize-size, size);
      sum = compute_blocksum(buf, sum);
      free(buf);
      }
    }
  else
    {
/*-- On disk: they are padded */
/*-- We open the file (nothing is done if already open) */
    if (!(cat=tab->cat))
      {
      warning("Cannot access file while computing the checksum in HDU ",
		tab->extname);
      return 0;
      }
    open_cat(cat, READ_ONLY);
    QFSEEK(cat->file, tab->bodypos, SEEK_SET, cat->filename);
    QMALLOC(buf, char, FBSIZE);
    for (n=nblock; n--;)
      {
      QFREAD(buf, FBSIZE, cat->file, cat->filename);
/*---- No need to swap bytes */
      sum = compute_blocksum(buf, sum);
      }
    }

  return sum;
  }


/****** write_checksum *****************************************************
PROTO	void write_checksum(tabstruct *tab)
PURPOSE	Compute and write the checksum to a FITS table
INPUT	Pointer to the tab.
OUTPUT	-.
NOTES	-.
AUTHOR	E. Bertin (IAP)
VERSION	04/06/2001
 ***/
void	write_checksum(tabstruct *tab)

  {
   char		str[32],
		*buf;
   unsigned int	sum;
   int		i;

/* Keep some margin */
  QREALLOC(tab->headbuf, char, 80*(tab->headnblock*36+3));
/* Add or update keywords in the header */
  fitsadd(tab->headbuf, "CHECKSUM", "ASCII 1's complement checksum");
  fitswrite(tab->headbuf, "CHECKSUM", "0000000000000000",
	H_STRING, T_STRING);
  fitsadd(tab->headbuf, "DATASUM ", "Checksum of data records");
  fitswrite(tab->headbuf, "DATASUM ", "0", H_STRING, T_STRING);
  fitsadd(tab->headbuf, "CHECKVER", "Checksum version ID");
  fitswrite(tab->headbuf, "CHECKVER", "COMPLEMENT", H_STRING, T_STRING);
/* Keep only what's necessary */
  tab->headnblock = ((fitsfind(tab->headbuf, "END     ")+36)*80)/FBSIZE;
  QREALLOC(tab->headbuf, char, tab->headnblock*FBSIZE);
/* First: the data */
  tab->bodysum = sum = compute_bodysum(tab, 0);
  sprintf(str, "%u", sum);
  fitswrite(tab->headbuf, "DATASUM ", str, H_STRING, T_STRING);


/* Now the header */
  buf = tab->headbuf;
  for (i=tab->headnblock; i--; buf+=FBSIZE)
    sum = compute_blocksum(buf, sum);

/* Complement to 1 */
  encode_checksum(~sum, str);
  fitswrite(tab->headbuf, "CHECKSUM", str, H_STRING, T_STRING);

  return;
  }


/****** verify_checksum *****************************************************
PROTO	int verify_checksum(tabstruct *tab)
PURPOSE	Compute and check the checksum of a FITS table
INPUT	Pointer to the tab.
OUTPUT	RETURN_OK if the checksum is correct, RETURN_ERROR if it is
	incorrect, or RETURN_FATAL_ERROR if no checksum found.
NOTES	-.
AUTHOR	E. Bertin (IAP)
VERSION	07/05/2001
 ***/
int	verify_checksum(tabstruct *tab)

  {
   char		*buf;
   unsigned int	sum;
   int		i;

  if (fitsfind(tab->headbuf, "CHECKSUM")==RETURN_ERROR)
    return RETURN_FATAL_ERROR;

/* First: the data */
  sum = compute_bodysum(tab, 0);
/* Now the header */
  buf = tab->headbuf;
  for (i=tab->headnblock; i--; buf+=FBSIZE)
    sum = compute_blocksum(buf, sum);
/* The result should sum to 0 */
  sum = ~sum;

  return sum? RETURN_ERROR : RETURN_OK;
  }

