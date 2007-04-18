/******************************************************************************
 * $Id: tif_ovrcache.h,v 1.1.1.1 2006/01/16 18:12:31 abrighto Exp $
 *
 * Project:  TIFF Overview Builder
 * Purpose:  Library functions to maintain two rows of tiles or two strips
 *           of data for output overviews as an output cache. 
 * Author:   Frank Warmerdam, warmerda@home.com
 *
 * This code could potentially be used by other applications wanting to
 * manage a once-through write cache. 
 *
 ******************************************************************************
 * Copyright (c) 2000, Frank Warmerdam
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 ******************************************************************************
 *
 * $Log: tif_ovrcache.h,v $
 * Revision 1.1.1.1  2006/01/16 18:12:31  abrighto
 * Imported sources
 *
 * Revision 1.1  2004/08/13 22:02:09  andreas_kupries
 *
 * 	* libtiff:     Updated libtiff from version 3.5.5 to 3.6.1.
 * 	* libtiff/tcl: Updated to handle the new version of libtiff.
 *
 * Revision 1.1  2000/01/28 15:03:32  warmerda
 * New
 *
 */

#ifndef TIF_OVRCACHE_H_INCLUDED
#define TIF_OVRCACHE_H_INCLUDED

typedef struct 
{
    uint32	nXSize;
    uint32	nYSize;

    uint32	nBlockXSize;
    uint32	nBlockYSize;
    uint16	nBitsPerPixel;
    uint16	nSamples;
    int		nBytesPerBlock;

    int		nBlocksPerRow;
    int		nBlocksPerColumn;

    int	        nBlockOffset; /* what block is the first in papabyBlocks? */
    unsigned char *pabyRow1Blocks;
    unsigned char *pabyRow2Blocks;

    int		nDirOffset;
    TIFF	*hTIFF;
    int		bTiled;
    
} TIFFOvrCache;

TIFFOvrCache *TIFFCreateOvrCache( TIFF *hTIFF, int nDirOffset );
unsigned char *TIFFGetOvrBlock( TIFFOvrCache *, int, int, int );
void           TIFFDestroyOvrCache( TIFFOvrCache * );

#endif /* ndef TIF_OVRCACHE_H_INCLUDED */

