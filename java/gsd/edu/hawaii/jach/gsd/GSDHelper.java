/*
 * Created on Apr 8, 2003
 *
 * To change the template for this generated file go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
package edu.hawaii.jach.gsd;

/**
 * @author Tim Jenness (JAC)
 *
 * Simple routines helpful for both GSDObject and GSDItem.
 * (at least whilst I am offline and can not look at the standard
 * class libraries)
 */
final class GSDHelper {

    /**
     * Pads (or truncates) a string so that it is forced to be the
     * specified length. Space padded.
     *
     * @param input String to be truncated.
     * @param outputLength Length of the return string.
     * @return
     */

    static String padString( String input, int outputLength) {
        String outstr;
        if (input.length() == outputLength) return input;
        if (input.length() > outputLength ) {
           outstr = input.substring(0,outputLength);
        } else {
            // Append the correct number of spaces. There must be
            // a better way. At the very least should be using
            // StringBuffer or allocating a single extra string of
            // spaces rather than instantiating an entirely new string
            // each time around the loop.
            outstr = input;
            for (int i = input.length(); i < outputLength; i++) {
                outstr = outstr + " ";
            }
        }
        return outstr;
    }


}
