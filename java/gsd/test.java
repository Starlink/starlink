import edu.hawaii.jach.gsd.GSDItem;
import edu.hawaii.jach.gsd.GSDObject;
import java.io.File;
import java.util.Iterator;

class TestGSD {

	public static void main(String[] args) {
		int item = 2;
		try {
			GSDObject gsd =
               // new GSDObject("/stardev/bin/specx/obs_das_0011.dat");
               new GSDObject(new File("/home/timj/data/jcmt/jcmtdr/rxa_146.dat"));

			gsd.print();
			System.out.println(gsd);
			System.out.println(gsd.itemByName("SCAN_VARS2"));

			// Get the items
			GSDItem[] allitems = gsd.items();
			System.out.println(allitems[20]);


			Iterator all = gsd.getItemIterator();
			while ( all.hasNext() ) {
			    GSDItem next = (GSDItem)all.next();
			    System.out.println(next);
			}

            // This is designed to cause a problem
			// System.out.println(gsd.itemByNum(0));

		} catch (edu.hawaii.jach.gsd.GSDException e) {
			System.out.println(e);
		} catch (java.io.FileNotFoundException e) {
			System.out.println(e);
		} catch (java.io.IOException e) {
			System.out.println(e);
		}
	}

}
