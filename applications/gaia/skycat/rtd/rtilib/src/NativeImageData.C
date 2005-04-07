/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: NativeImageData.C,v 1.2 1998/07/28 21:23:47 abrighto Exp $" 
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
 */

#if defined(i386) || defined(__i386__) || defined(__x86_64) || \
     defined(__alpha) || \
     defined(vax) || defined(__vax__) || (defined(mips) && defined(MIPSEL))

// byte swapping needed, make copies of existing classes, but with swapping disabled
#define NTOH(x) x
#include "NativeImageData.h"

#define ShortImageData NativeShortImageData
#include "ShortImageData.C"
#undef ShortImageData
#undef CLASS_NAME
#undef DATA_TYPE
#undef ISNAN

#define UShortImageData NativeUShortImageData
#include "UShortImageData.C"
#undef UShortImageData
#undef CLASS_NAME
#undef DATA_TYPE
#undef ISNAN

#define LongImageData NativeLongImageData
#include "LongImageData.C"
#undef LongImageData
#undef CLASS_NAME
#undef DATA_TYPE
#undef ISNAN

#define FloatImageData NativeFloatImageData
#include "FloatImageData.C"
#undef FloatImageData
#undef CLASS_NAME
#undef DATA_TYPE
#undef ISNAN

#define DoubleImageData NativeDoubleImageData
#include "DoubleImageData.C"
#undef DoubleImageData
#undef CLASS_NAME
#undef DATA_TYPE
#undef ISNAN

#endif /* defined(i386) ... */

// Dummy routine to keep some linkers happy.
static void dummy() {
  // Do nothing.
}
