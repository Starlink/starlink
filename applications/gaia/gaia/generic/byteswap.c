/*
 *   Name:
 *      byteswapnew
 *
 *   Purpose:
 *      Returns a byte swapped copy of an array. 
 *
 *   Usage:
 *      #include "byteswap.h"
 *      void *ptr = byteswapnew( inptr, size, nbyte );
 *
 *   Arguments:
 *      void *inptr = pointer to the array of data to byte swap.
 *      int size    = number of elements in data array.
 *      nbyte       = number pf bytes per data element of array.
 *
 *   Return:
 *      Pointer to byte swapped copy of data, or NULL if fails.
 */

void *byteswapnew( void *inptr, int size, int nbyte ) 
{
   /*  Get dynamic memory required for copy. */
   void *outptr = malloc( (size_t) ( size * nbyte ) );
   if ( copy != NULL ) {
      byteswapcopy( inptr, size, nbyte, outptr );
   }
   return outptr;
}

/*
 *   Name:
 *      byteswapcopy
 *
 *   Purpose:
 *      Copy and byte swap data between arrays. 
 *
 *   Usage:
 *      #include "byteswap.h"
 *      byteswapcopy( inptr, size, nbyte, outptr );
 *
 *   Arguments:
 *      void *inptr  = pointer to the array of data to byte swap.
 *      int size     = number of elements in data array.
 *      nbyte        = number pf bytes per data element of array.
 *      void *outptr = pointer to memory to contain copy.
 */

void byteswapcopy( void *ptr, int size, int nbyte, void *outptr ) 
{
   int n = size;

   /*  Copy the data and swap bytes */
   if ( nbyte == 1 ) {

      /*  Single byte so no swapping necessary */
      memcpy( outptr, inptr, (size_t) size );

   } else if ( nbyte == 2 ) {
         
      /* Copy shorts. */
      unsigned short *from = (unsigned short *) inptr;
      unsigned short *to = (unsigned short *) outptr; 
      while( n-- ) {
         *to++ = ntohs( *from );
         from++;               /* Increment here as ntohs is macro.*/
      }

   } else if ( nbyte == 4 ) {
      
      if ( sizeof( unsigned long ) == 8 ) {
         
         /* Use int for 4 bytes */
         unsigned int* from = (unsigned int *) inptr;
         unsigned int* to = (unsigned int *) outptr; 
         while( n-- ) {
            *to++ = ntohl( *from );
            from++;
         }
      } else {

         /* Use long for 4 bytes */
         unsigned long* from = (unsigned long *) inptr;
         unsigned long* to = (unsigned long *) outptr; 
         while( n-- ) {
            *to++ = ntohl( *from );
            from++;
         }
      }
   } else if ( nbyte == 8 ) {
      
      /*  Use double for pointer arith */
      
      if ( sizeof( unsigned long ) == 8 ) {
         
         /*  Use int for 4 bytes */
         union { unsigned int raw[2]; double typed; } ret;
         double* from = (double *) inptr;
         double* to = (double int *) outptr; 
         unsigned int tmp;
         while( n-- ) {
            ret.typed = *from++;
            tmp = ret.raw[0];
            ret.raw[0] = ntohl( ret.raw[1] );
            ret.raw[1] = ntohl( tmp );
            *to++ = ret.typed;
         }
      } else {
         
         /*  Use long for 4 bytes */
         union { unsigned long raw[2]; double typed; } ret;
         double* from = (double *) inptr;
         double* to = (double int *) outptr; 
         unsigned long tmp;
         while( n-- ) {
            ret.typed = *from++;
            tmp = ret.raw[0];
            ret.raw[0] = ntohl(ret.raw[1]);
            ret.raw[1] = ntohl(tmp);
            *to++ = ret.typed;
         }
      }
   }
}
