/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: NativeImageData.C,v 1.2 2005/02/02 01:43:02 brighton Exp $" 
 *
 * NativeImageData.C - cpp template definitions to support native byte
 *                     order images on byte swapped machines (i386, vax, etc.)
 *
 * This file uses some #defines to declare native versions of the
 * ImageData subclasses for use on byte swapped machines. On other
 * machines, this has no effect and no new classes are created.
 *
 * The purpose is to allow derived packages to use image types that are
 * either in native byte order already or have been read in and
 * byte-swapped already by a library routine. This can be controlled by a
 * flag set in a class derived from the ImageIO class (astrotcl/imageio).
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  20/03/98  Created
 * Peter W. Draper 07/04/05  Added __x86_64
 * pbiereic        17/02/03  Native byte order routines revised
 */

#define NTOH(x) x
#define ShortImageData NativeShortImageData
#include "ShortImageData.C"
#undef ShortImageData
#undef NTOH

#define NTOH(x) x
#define UShortImageData NativeUShortImageData
#include "UShortImageData.C"
#undef UShortImageData
#undef NTOH

#define NTOH(x) x
#define LongImageData NativeLongImageData
#include "LongImageData.C"
#undef LongImageData
#undef NTOH

#define NTOH(x) x
#define FloatImageData NativeFloatImageData
#include "FloatImageData.C"
#undef FloatImageData
#undef NTOH

#define NTOH(x) x
#define DoubleImageData NativeDoubleImageData
#include "DoubleImageData.C"
#undef DoubleImageData
#undef NTOH
