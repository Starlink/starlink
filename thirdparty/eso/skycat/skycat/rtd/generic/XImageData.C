/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: XImageData.C,v 1.5 1997/04/11 10:52:36 abrighto Exp $" 
 *
 * XImageData.C - member functions for class XImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */
static const char* const rcsId="@(#) $Id: XImageData.C,v 1.5 1997/04/11 10:52:36 abrighto Exp $";


#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <assert.h>
#include <math.h>
#include <define.h>
#include "XImageData.h"


/*
 * Include some standard methods as (cpp macro) templates:
 * These are methods that are the same for all derived classes of ImageData,
 * except that they work on a different raw data type
 */
#define CLASS_NAME XImageData
#define DATA_TYPE byte
#define NTOH(x) (x)
#include "ImageTemplates.C"


