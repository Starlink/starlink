package edu.hawaii.jach.gsd;

/**
 * Thrown to indicate that there has been an GSD error of some description.
 */
public class GSDException extends Exception {
    public GSDException( String message ) {
        super( message );
    }
}
