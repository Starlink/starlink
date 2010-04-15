package edu.hawaii.jach.gsd;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/**
 * Conversion routines for converting data stored in VAX format (the
 * format used in the GSD file format) to Java native types. The
 * fundamental operations are:
 * <p>
 * <table>
 * <tr><td>VAX/GSD Type</td><td>Java type</td><td>Note</td></tr>
 * <tr><td>BYTE</td> <td>byte</td><td></td></tr>
 * <tr><td>LOGICAL</td><td>boolean</td><td>Single byte</td></tr>
 * <tr><td>SHORT</td><td>short</td><td>Byte swap</td></tr>
 * <tr><td>INTEGER</td><td>int</td><td>Byte swap</td></tr>
 * <tr><td>REAL</td><td>float</td><td>VAX float -&gt; IEEE float </td></tr>
 * <tr><td>DOUBLE</td><td>double</td><td>VAX float -&gt; IEEE float </td></tr>
 * <tr><td>CHARACTER*16</td><td>String[16]</td><td></td></tr>
 * </table>
 * <p>
 * i.e. Integer types must be converted from little-endian to big-endian and
 * floating point numbers must be converted from VAX floating point format
 * to IEEE floating point format. In GSD files strings are stored using a
 * fixed length and are space padded. Note that GSD bad values are not
 * understood at this time.
 *
 * @author  Tim Jenness (JAC)
 * @version $Id$
 */

// Note that this class should only be visible in the gsd package
// and is not part of the public API.
final class GSDVAXBytes {

	// These constants specify the sizes of the fundamental data types
	// as seen from the VAX side
	/**
	 * Constant specifying the number of bytes used to represent
	 * a VAX byte (clearly this is always 1).
	 */
	public static final int VAX__SZBYTE = 1;
	/**
	 * Constant specifying the number of bytes used to represent
	 * a VAX logical.
	 */
	public static final int VAX__SZLOGICAL = 1;
	/**
	 * Constant specifying the number of bytes used to represent
	 * a VAX word.
	 */
	public static final int VAX__SZWORD = 2;
	/**
	 * Constant specifying the number of bytes used to represent
	 * a VAX integer.
	 */
	public static final int VAX__SZINTEGER = 4;
	/**
	 * Constant specifying the number of bytes used to represent
	 * a VAX floating point real.
	 */
	public static final int VAX__SZREAL = 4;
	/**
	 * Constant specifying the number of bytes used to represent
	 * a VAX double precision floating point number.
	 */
	public static final int VAX__SZDOUBLE = 8;

	// Have a private constructor since this class does not export
	// any instance methods
	private GSDVAXBytes() {
		// empty
	}

	/**
	 * Convert 4 bytes in VAX floating point format to a Java float.
	 * This routine is a clone of gsd2_nativr in the C GSD library
	 * written by Horst Meyerdierks and Remo Tilanus.
	 *
	 * @param bytes Array of 4 bytes in VAX sequence
	 * @return IEEE float
	 */

	public static float tofloat(byte[] bytes) {
		// Floating point numbers are in VAX format. We have to convert to
		// little endian IEEE

		// Take this from gsd2_nativr.c as written by Horst and Remo

		// Extract the exponent
		int e = ((bytes[1] << 1) & 0xfe) | ((bytes[0] >> 7) & 0x1);

		/*    If the (biased) exponent is greater than 2, then the
		 *    VAXF number can be represented in IEEE_S form as a
		 *    normalised number. Decrement the exponent by 2. This
		 *    allows for a difference of 1 between the exponent bias
		 *    values of the two number formats and a further
		 *    difference of one in the assumed position of the binary
		 *    radix point.
		 */

		int tmp[] = new int[4];

		if (e > 2) {
			e -= 2;

			/*       Construct the resulting IEEE_S number, using the
			 *       appropriate bytes of the VAXF number but
			 *       replacing the exponent field with its modified
			 *       value.
			 */
			tmp[0] = (bytes[1] & 0x80) | ((e >> 1) & 0x7f);
			tmp[1] = (bytes[0] & 0x7f) | ((e << 7) & 0x80);
			tmp[2] = bytes[3];
			tmp[3] = bytes[2];

		} else if (e == 0) {

			/*    If the (biased) VAXF exponent is zero, then the
			 *    resulting IEEE_S value is zero (or we have a VAX
			 *    reserved operand, but we assume that can't happen).
			 */

			for (int i = 0; i < 4; i++) {
				tmp[i] = 0;
			}

		} else {

			/*    Otherwise, if the (biased) exponent is 2 or less,
			 *    then the IEEE_S equivalent will be a denormalised
			 *    number, so the fraction field must be modified.
			 *    Extract all the bits of the VAXF fraction field into
			 *    a single integer (remember we can't assume what
			 *    order the integer's bytes are stored in).  Also add
			 *    the (normally omitted) leading 1.
			 */

			int f =
				bytes[2]
					| (bytes[3] << 8)
					| ((bytes[0] & 0x7f) << 16)
					| (0x1 << 23);

			/*       Shift the fraction bits to account for the
			 *       limited range of the exponent. Then pack the
			 *       fraction field into the IEEE_S number. Retain the
			 *       VAXF sign bit, but set the exponent field to zero
			 *       (indicating a denormalised number).
			 */

			f = f >> (3 - e);
			tmp[0] = bytes[1] & 0x80;
			tmp[1] = (f >> 16) & 0x7f;
			tmp[2] = (f >> 8) & 0xff;
			tmp[3] = f & 0xff;

		}

		/* Need to convert the integer array back to a byte array */
		byte retval[] = new byte[4];
		for (int i = 0; i < 4; i++) {
			retval[i] = (byte) tmp[i];
		}

		/* We know we are always BIG endian so just convert the byte buffer
		   to a float */
		return ByteBuffer.wrap(retval, 0, VAX__SZREAL).getFloat();
	}

	/**
	 * Convert a VAX little-endian 4 byte sequence to a Java int.
	 *
	 * @param bytes Array of bytes representing a VAX integer.
	 * @return Java int.
	 */
	public static int toint(byte[] bytes) {
		return ByteBuffer
			.wrap(bytes, 0, VAX__SZINTEGER)
			.order(ByteOrder.LITTLE_ENDIAN)
			.getInt();
	}

	/**
	 * Convert a VAX little-endian 2 byte sequence to a Java short
	 *
	 * @param bytes Array of bytes representing a VAX word.
	 * @return Java short.
	 */
	public static short toshort(byte[] bytes) {
		return ByteBuffer
			.wrap(bytes, 0, VAX__SZWORD)
			.order(ByteOrder.LITTLE_ENDIAN)
			.getShort();
	}

	/**
	 * Convert a VAX byte to a java byte. This method exists for interface
	 * completeness and simply returns the first element of the input
	 * byte array.
	 *
	 * @param bytes Array of bytes (only first element is used)
	 * @return The first element of the input array
	 */
	public static byte tobyte(byte[] bytes) {
		return bytes[0];
	}

	/**
	 * Convert a VAX logical to a boolean.
	 *
	 * @param bytes Array of bytes representing a VAX logical
	 * @return Java boolean
	 */
	public static boolean tobool(byte[] bytes) {
		bytes[0] &= 1;
		boolean retval;
		if (bytes[0] != 0) {
			retval = true;
		} else {
			retval = false;
		}
		return retval;
	}

	/**
	 * Convert 8 bytes in VAX floating point format to a Java double.
	 * This routine is a clone of gsd2_nativd in the C GSD library
	 * written by Horst Meyerdierks and Remo Tilanus.
	 *
	 * @param bytes Array of 8 bytes in VAX sequence
	 * @return IEEE double precision float
	 */

	public static double todouble(byte[] bytes) {
		// GSD stores data in VAX floating point format. Must be
		// converted to IEEE little endian
		// Direct clone of gsd2_nativd as written by Horst and Remo

		//    Extract the exponent.
		int e = ((bytes[1] << 1) & 0xfe) | ((bytes[0] >> 7) & 0x1);

		/*    If the (biased) exponent is non-zero, then the VAXD
		 *    number can be represented in IEEE_D form as a normalised
		 *    number. Increment the exponent by 894. This allows for a
		 *    difference of 895 between the exponent bias values of
		 *    the two number formats and a further difference of one
		 *    (acting in the opposite direction) in the assumed
		 *    position of the binary radix point.
		 */

		int tmp[] = new int[8];

		if (e != 0) {
			e += 894;

			/*       Construct the resulting IEEE_D number, using the
			 *       appropriate bytes of the VAXD number but
			 *       replacing the exponent field with its modified
			 *       value. The input fraction bytes have to be split
			 *       between the output bytes with a displacement of 3
			 *       bits and the final 3 least significant input bits
			 *       are lost (note they are simply truncated; no
			 *       rounding is attempted).
			 */
			tmp[0] = (bytes[1] & 0x80) | ((e >> 4) & 0x7f);
			tmp[1] = ((bytes[0] >> 3) & 0xf) | ((e << 4) & 0xf0);
			tmp[2] = ((bytes[0] << 5) & 0xe0) | ((bytes[3] >> 3) & 0x1f);
			tmp[3] = ((bytes[3] << 5) & 0xe0) | ((bytes[2] >> 3) & 0x1f);
			tmp[4] = ((bytes[2] << 5) & 0xe0) | ((bytes[5] >> 3) & 0x1f);
			tmp[5] = ((bytes[5] << 5) & 0xe0) | ((bytes[4] >> 3) & 0x1f);
			tmp[6] = ((bytes[4] << 5) & 0xe0) | ((bytes[7] >> 3) & 0x1f);
			tmp[7] = ((bytes[7] << 5) & 0xe0) | ((bytes[6] >> 3) & 0x1f);
		}

		/*    If the (biased) VAXD exponent is zero, then the
		 *    resulting IEEE_D value is zero (or we have a VAX
		 *    reserved operand, but we assume that can't happen).
		 */
		else {
			for (int i = 0; i < 8; i++)
				tmp[i] = 0;
		}

		/* Need to convert the integer array back to a byte array */
		byte retval[] = new byte[8];
		for (int i = 0; i < 8; i++) {
			retval[i] = (byte) tmp[i];
		}

		/* We know we are always BIG endian so just convert the byte buffer
		   to a float */
		return ByteBuffer.wrap(retval, 0, VAX__SZDOUBLE).getDouble();

	}

	/**
	 * Converts a Fortran-style string into a Java string.
	 * The characters in the byte array are assumed to represent
	 * the US-ASCII character set. A single byte per character.
	 *
	 * @param bytes Array of bytes assumed to represent characters. All the
	 *              bytes are converted to characters in the output string.
	 * @return Java string
	 * @throws IllegalStateException if the character set used by this
	 * routine is no longer supported by the String constructor. This
	 * should never happen (unless US-ASCII is suddenly deprecated).
	 */
	public static String tostring(byte[] bytes) {
		String buf;
		String charset = "US-ASCII";
		try {
			buf = new String(bytes, charset);
		} catch (UnsupportedEncodingException e) {
			throw new IllegalStateException(
				"Somehow char set "
					+ charset
					+ " is no longer supported by the String constructor");
		}
		return buf;
	}

	// Variants with offset specified ---------------------------

	/**
	 * Convert a sequence of bytes into a Java float, starting at the
	 * specified offset into the supplied byte stream.
	 *
	 * @param bytes A bytes array containing some VAX bytes
	 * @param offset Start position to begin reading the input bytes array.
	 * @return A single Java float
	 */
	public static float tofloat(byte[] bytes, int offset) {
		// Copy buffer
		return tofloat(bytecopy(bytes, offset, VAX__SZREAL));
	}

	/**
	 * Convert a sequence of bytes into a Java double, starting at the
	 * specified offset into the supplied byte stream.
	 *
	 * @param bytes A bytes array containing some VAX bytes
	 * @param offset Start position to begin reading the input bytes array.
	 * @return A single Java double
	 */
	public static double todouble(byte[] bytes, int offset) {
		// Copy buffer
		return todouble(bytecopy(bytes, offset, VAX__SZDOUBLE));
	}

	/**
	 * Convert a sequence of bytes into a Java short, starting at the
	 * specified offset into the supplied byte stream.
	 *
	 * @param bytes A bytes array containing some VAX bytes
	 * @param offset Start position to begin reading the input bytes array.
	 * @return A single Java short
	 */
	public static short toshort(byte[] bytes, int offset) {
		// Copy buffer
		return toshort(bytecopy(bytes, offset, VAX__SZWORD));
	}

	/**
	 * Convert a sequence of bytes into a Java int, starting at the
	 * specified offset into the supplied byte stream.
	 *
	 * @param bytes A bytes array containing some VAX bytes
	 * @param offset Start position to begin reading the input bytes array.
	 * @return A single Java int
	 */
	public static int toint(byte[] bytes, int offset) {
		// Copy buffer
		return toint(bytecopy(bytes, offset, VAX__SZINTEGER));
	}

	/**
	 * Convert a sequence of bytes into a Java boolean, starting at the
	 * specified offset into the supplied byte stream.
	 *
	 * @param bytes A bytes array containing some VAX bytes
	 * @param offset Start position to begin reading the input bytes array.
	 * @return A single Java boolean
	 */
	public static boolean tobool(byte[] bytes, int offset) {
		return tobool(bytecopy(bytes, offset, VAX__SZLOGICAL));
	}

	/**
	 * Return the byte corresponding to the offset.
	 * This method exists for interface completeness.
	 *
	 * @param bytes A bytes array containing some VAX bytes
	 * @param offset Start position to begin reading the input bytes array.
	 * @return A single byte
	 */
	public static byte tobyte(byte[] bytes, int offset) {
		return tobyte(bytecopy(bytes, offset, VAX__SZBYTE));
	}

	/**
	 * Convert a sequence of bytes into a Java String, starting at the
	 * specified offset into the supplied byte stream.  Needs an
	 * argument specifying the length since we do not know the length
	 * of the required string and can not assume we can read all the
	 * bytes.
	 *
	 * @param bytes A bytes array containing some VAX bytes
	 * @param offset Start position to begin reading the input bytes array.
	 * @param nchars Number of characters to read from the array
	 * @return A single Java string.
	 */
	public static String tostring(byte[] bytes, int offset, int nchars) {
		return tostring(bytecopy(bytes, offset, nchars));
	}

	/**
	 * Convert a byte array to a byte array. Simply returns the slice
	 * into the input array as specified by the arguments. This method
	 * exists for completeness.
	 *
	 * @param bytes Input byte array to convert.
	 * @param offset Start position to begin reading the input bytes array.
	 * @param nelem Number of bytes to copy.
	 * @return The input byte array but sliced.
	 */
	public static byte[] tobyteArray(byte[] bytes, int offset, int nelem) {
		// do not even bother to copy
		return bytecopy(bytes, offset, nelem);
	}

	/**
	 * Convert a VAX byte array into an array of booleans.
	 *
	 * @param bytes Input byte array to convert.
	 * @param offset Start position to begin reading the input bytes array.
	 * @param nelem Number of elements to copy.
	 * @return array of booleans.
	 */
	public static boolean[] toboolArray(byte[] bytes, int offset, int nelem) {

		// array to receive
		boolean[] output = new boolean[nelem];

		for (int i = 0; i < nelem; i++) {

			output[i] = tobool(bytes, offset);

			// Increment offset into byte array
			offset += VAX__SZLOGICAL;
		}

		return output;
	}

	/**
	 * Convert a VAX byte array into an array of shorts.
	 *
	 * @param bytes Input byte array to convert.
	 * @param offset Start position to begin reading the input bytes array.
	 * @param nelem Number of elements to copy.
	 * @return Array of shorts.
	 */
	public static short[] toshortArray(byte[] bytes, int offset, int nelem) {

		// array to receive
		short[] output = new short[nelem];

		for (int i = 0; i < nelem; i++) {

			output[i] = toshort(bytes, offset);

			// Increment offset into byte array
			offset += VAX__SZWORD;
		}

		return output;
	}

	/**
	 * Convert a VAX byte array into an array of ints.
	 *
	 * @param bytes Input byte array to convert.
	 * @param offset Start position to begin reading the input bytes array.
	 * @param nelem Number of elements to copy.
	 * @return Array of ints.
	 */
	public static int[] tointArray(byte[] bytes, int offset, int nelem) {

		// array to receive
		int[] output = new int[nelem];

		for (int i = 0; i < nelem; i++) {

			output[i] = toint(bytes, offset);

			// Increment offset into byte array
			offset += VAX__SZINTEGER;
		}

		return output;
	}

	/**
	 * Convert a VAX byte array into an array of floats.
	 *
	 * @param bytes Input byte array to convert.
	 * @param offset Start position to begin reading the input bytes array.
	 * @param nelem Number of elements to copy.
	 * @return Array of floats.
	 */
	public static float[] tofloatArray(byte[] bytes, int offset, int nelem) {

		// array to receive
		float[] output = new float[nelem];

		for (int i = 0; i < nelem; i++) {

			output[i] = tofloat(bytes, offset);

			// Increment offset into byte array
			offset += VAX__SZREAL;
		}

		return output;
	}

	/**
	 * Convert a VAX byte array into an array of doubles.
	 *
	 * @param bytes Input byte array to convert.
	 * @param offset Start position to begin reading the input bytes array.
	 * @param nelem Number of elements to copy.
	 * @return Array of doubles.
	 */
	public static double[] todoubleArray(byte[] bytes, int offset, int nelem) {

		// array to receive
		double[] output = new double[nelem];

		for (int i = 0; i < nelem; i++) {

			output[i] = todouble(bytes, offset);

			// Increment offset into byte array
			offset += VAX__SZDOUBLE;
		}

		return output;
	}

	/**
	 * Convert a VAX byte array into an array of strings.
	 * The strings are assumed to represent a US-ASCII character set.
	 *
	 * @param bytes Input byte array to convert.
	 * @param offset Start position to begin reading the input bytes array.
	 * @param nelem Number of elements to copy.
	 * @param nchars Size of each string in the array (in bytes).
	 * @return Array of strings.
	 */
	public static String[] tostringArray(
		byte[] bytes,
		int offset,
		int nelem,
		int nchars) {

		// array to receive
		String[] output = new String[nelem];

		for (int i = 0; i < nelem; i++) {

			output[i] = tostring(bytes, offset, nchars);

			// Increment offset into byte array
			offset += nchars;
		}

		return output;
	}

	/**
	 * This method converts bytes from a ByteBuffer rather than from
	 * an array of bytes. The bytes are extracted from the ByteBuffer
	 * for each float in turn rather than creating a large intermediate
	 * array of bytes. For this reason, this method is probably slower
	 * than the byte array equivalent but will use far less memory
	 * especially if the ByteBuffer is obtained from memory mapping
	 * of a file.
	 *
	 * @param buffer An ByteBuffer containing vax bytes
	 * @param offset Start position to begin reading the input buffer.
	 * @param nelem Number of elements to copy.
	 * @return An array of floats.
	 */
	public static float[] tofloatArray(
		ByteBuffer buffer,
		int offset,
		int nelem) {

		// array to receive
		float[] output = new float[nelem];

		// Array to store a float at a time
		byte[] temp = new byte[VAX__SZREAL];

		// Force the buffer offset to be at the correct position
		buffer.position(offset);

		for (int i = 0; i < nelem; i++) {

			// Read some bytes from the buffer
			buffer.get(temp);

			output[i] = tofloat(temp);

		}

		return output;
	}

	/**
	 * This method converts bytes from a ByteBuffer rather than from
	 * an array of bytes. The bytes are extracted from the ByteBuffer
	 * for each integer in turn rather than creating a large intermediate
	 * array of bytes. For this reason, this method is probably slower
	 * than the byte array equivalent but will use far less memory
	 * especially if the ByteBuffer is obtained from memory mapping
	 * of a file.
	 *
	 * @param buffer An ByteBuffer containing vax bytes
	 * @param offset Start position to begin reading the input buffer.
	 * @param nelem Number of elements to copy.
	 * @return An array of integers.
	 */
	public static int[] tointArray(ByteBuffer buffer, int offset, int nelem) {

		// array to receive
		int[] output = new int[nelem];

		// Array to store a float at a time
		byte[] temp = new byte[VAX__SZINTEGER];

		// Force the buffer offset to be at the correct position
		buffer.position(offset);

		for (int i = 0; i < nelem; i++) {

			// Read some bytes from the buffer
			buffer.get(temp);

			output[i] = toint(temp);

		}

		return output;
	}

	/**
	 * This method converts bytes from a ByteBuffer rather than from
	 * an array of bytes. The bytes are extracted from the ByteBuffer
	 * for each short in turn rather than creating a large intermediate
	 * array of bytes. For this reason, this method is probably slower
	 * than the byte array equivalent but will use far less memory
	 * especially if the ByteBuffer is obtained from memory mapping
	 * of a file.
	 *
	 * @param buffer An ByteBuffer containing vax bytes
	 * @param offset Start position to begin reading the input buffer.
	 * @param nelem Number of elements to copy.
	 * @return An array of shorts.
	 */
	public static short[] toshortArray(
		ByteBuffer buffer,
		int offset,
		int nelem) {

		// array to receive
		short[] output = new short[nelem];

		// Array to store a float at a time
		byte[] temp = new byte[VAX__SZWORD];

		// Force the buffer offset to be at the correct position
		buffer.position(offset);

		for (int i = 0; i < nelem; i++) {

			// Read some bytes from the buffer
			buffer.get(temp);

			output[i] = toshort(temp);

		}

		return output;
	}

	/**
	 * This method converts bytes from a ByteBuffer rather than from
	 * an array of bytes. The bytes are extracted from the ByteBuffer
	 * for each boolean in turn rather than creating a large intermediate
	 * array of bytes. For this reason, this method is probably slower
	 * than the byte array equivalent but will use far less memory
	 * especially if the ByteBuffer is obtained from memory mapping
	 * of a file.
	 *
	 * @param buffer An ByteBuffer containing vax bytes
	 * @param offset Start position to begin reading the input buffer.
	 * @param nelem Number of elements to copy.
	 * @return An array of booleans.
	 */
	public static boolean[] toboolArray(
		ByteBuffer buffer,
		int offset,
		int nelem) {

		// array to receive
		boolean[] output = new boolean[nelem];

		// Array to store a float at a time
		byte[] temp = new byte[VAX__SZLOGICAL];

		// Force the buffer offset to be at the correct position
		buffer.position(offset);

		for (int i = 0; i < nelem; i++) {

			// Read some bytes from the buffer
			buffer.get(temp);

			output[i] = tobool(temp);

		}

		return output;
	}

	/**
	 * This method converts bytes from a ByteBuffer rather than from
	 * an array of bytes. The bytes are extracted from the ByteBuffer
	 * for each double in turn rather than creating a large intermediate
	 * array of bytes. For this reason, this method is probably slower
	 * than the byte array equivalent but will use far less memory
	 * especially if the ByteBuffer is obtained from memory mapping
	 * of a file.
	 *
	 * @param buffer An ByteBuffer containing vax bytes
	 * @param offset Start position to begin reading the input buffer.
	 * @param nelem Number of elements to copy.
	 * @return An array of doubles.
	 */
	public static double[] todoubleArray(
		ByteBuffer buffer,
		int offset,
		int nelem) {

		// array to receive
		double[] output = new double[nelem];

		// Array to store a float at a time
		byte[] temp = new byte[VAX__SZDOUBLE];

		// Force the buffer offset to be at the correct position
		buffer.position(offset);

		for (int i = 0; i < nelem; i++) {

			// Read some bytes from the buffer
			buffer.get(temp);

			output[i] = todouble(temp);

		}

		return output;
	}

	/**
	 * This method converts bytes from a ByteBuffer rather than from
	 * an array of bytes. The bytes are extracted from the ByteBuffer
	 * for each string in turn rather than creating a large intermediate
	 * array of bytes. For this reason, this method is probably slower
	 * than the byte array equivalent but will use far less memory
	 * especially if the ByteBuffer is obtained from memory mapping
	 * of a file.
	 *
	 * @param buffer An ByteBuffer containing vax bytes
	 * @param offset Start position to begin reading the input buffer.
	 * @param nelem Number of elements to copy.
	 * @return An array of strings.
	 */
	public static String[] tostringArray(
		ByteBuffer buffer,
		int offset,
		int nelem,
		int nchar) {

		// array to receive
		String[] output = new String[nelem];

		// Array to store a float at a time
		byte[] temp = new byte[nchar];

		// Force the buffer offset to be at the correct position
		buffer.position(offset);

		for (int i = 0; i < nelem; i++) {

			// Read some bytes from the buffer
			buffer.get(temp);

			output[i] = tostring(temp);

		}

		return output;
	}

	/**
	 * This method converts bytes from a ByteBuffer rather than from
	 * an array of bytes. The bytes are extracted from the ByteBuffer
	 * and simply copied to an output array of the correct size without
	 * modification.
	 *
	 * @param buffer An ByteBuffer containing simple bytes
	 * @param offset Start position to begin reading the input buffer.
	 * @param nelem Number of elements to copy.
	 * @return An array of bytes.
	 */
	public static byte[] tobyteArray(
		ByteBuffer buffer,
		int offset,
		int nelem) {

		// array to receive
		byte[] output = new byte[nelem];

		// Force the buffer offset to be at the correct position
		buffer.position(offset);

		// Read some bytes from the buffer
		buffer.get(output);

		return output;
	}

	// Routines to do array copy from bytes but using the number of bytes
	// rather than the number of elements

	/**
	 * Similar to tointArray() except the length of the byte stream
	 * is specified rather than the number of elements.
	 *
	 * @param buffer A ByteBuffer containing the VAX bytes.
	 * @param offset Offset into the ByteBuffer.
	 * @param nbytes Number of bytes to read from the buffer.
	 * @return Array of integers.
	 */
	public static int[] tointArrayNB(
		ByteBuffer buffer,
		int offset,
		int nbytes) {
		return tointArray(buffer, offset, nbytes / VAX__SZINTEGER);
	}

	/**
	 * Similar to toshortArray() except the length of the byte stream
	 * is specified rather than the number of elements.
	 *
	 * @param buffer A ByteBuffer containing the VAX bytes.
	 * @param offset Offset into the ByteBuffer.
	 * @param nbytes Number of bytes to read from the buffer.
	 * @return Array of shorts.
	 */
	public static short[] toshortArrayNB(
		ByteBuffer buffer,
		int offset,
		int nbytes) {
		return toshortArray(buffer, offset, nbytes / VAX__SZWORD);
	}

	/**
	 * Similar to tofloatArray() except the length of the byte stream
	 * is specified rather than the number of elements.
	 *
	 * @param buffer A ByteBuffer containing the VAX bytes.
	 * @param offset Offset into the ByteBuffer.
	 * @param nbytes Number of bytes to read from the buffer.
	 * @return Array of floats.
	 */
	public static float[] tofloatArrayNB(
		ByteBuffer buffer,
		int offset,
		int nbytes) {
		return tofloatArray(buffer, offset, nbytes / VAX__SZREAL);
	}

	/**
	 * Similar to todoubleArray() except the length of the byte stream
	 * is specified rather than the number of elements.
	 *
	 * @param buffer A ByteBuffer containing the VAX bytes.
	 * @param offset Offset into the ByteBuffer.
	 * @param nbytes Number of bytes to read from the buffer.
	 * @return Array of doubles.
	 */
	public static double[] todoubleArrayNB(
		ByteBuffer buffer,
		int offset,
		int nbytes) {
		return todoubleArray(buffer, offset, nbytes / VAX__SZDOUBLE);
	}

	/**
	 * Similar to toboolArray() except the length of the byte stream
	 * is specified rather than the number of elements.
	 *
	 * @param buffer A ByteBuffer containing the VAX bytes.
	 * @param offset Offset into the ByteBuffer.
	 * @param nbytes Number of bytes to read from the buffer.
	 * @return Array of booleans.
	 */
	public static boolean[] toboolArrayNB(
		ByteBuffer buffer,
		int offset,
		int nbytes) {
		return toboolArray(buffer, offset, nbytes / VAX__SZLOGICAL);
	}

	/**
	 * Similar to tobyteArray() except the length of the byte stream
	 * is specified rather than the number of elements.
	 *
	 * @param buffer A ByteBuffer containing the VAX bytes.
	 * @param offset Offset into the ByteBuffer.
	 * @param nbytes Number of bytes to read from the buffer.
	 * @return Array of bytes.
	 */
	public static byte[] tobyteArrayNB(
		ByteBuffer buffer,
		int offset,
		int nbytes) {
		return tobyteArray(buffer, offset, nbytes / VAX__SZBYTE);
	}

	/**
	 * Similar to tostringArray() except the length of the byte stream
	 * is specified rather than the number of elements.
	 *
	 * @param buffer A ByteBuffer containing the VAX bytes.
	 * @param offset Offset into the ByteBuffer.
	 * @param nbytes Number of bytes to read from the buffer.
	 * @param nchar  Number of characters per element
	 * @return Array of strings.
	 */
	public static String[] tostringArrayNB(
		ByteBuffer buffer,
		int offset,
		int nbytes,
		int nchar) {
		return tostringArray(buffer, offset, nbytes / nchar, nchar);
	}

	// Generic byte copy. Just a wrapper for System.arraycopy()
	private static byte[] bytecopy(byte[] bytes, int offset, int nbytes) {
		byte[] buf = new byte[nbytes];
		System.arraycopy(bytes, offset, buf, 0, nbytes);
		return buf;
	}

}
