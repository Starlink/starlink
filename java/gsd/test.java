
import java.io.*;
import edu.hawaii.jach.gsd.*;


class TestGSD {

    public static void main( String[] args) {
	int item = 2;
	try {
	    GSDObject gsd = new GSDObject( "/stardev/bin/specx/obs_das_0011.dat" );

	    gsd.print();
            System.out.println( gsd );
            System.out.println( gsd.itemByName("C1PID") );

	} catch ( edu.hawaii.jach.gsd.GSDException e ) {
	    System.out.println(e);
	} catch ( java.io.FileNotFoundException e ) {
	    System.out.println(e);
	} catch ( java.io.IOException e ) {
	    System.out.println(e);	
	}
    }

}
