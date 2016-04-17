/*
  __  __    U  ___ u  _   _       _      ____    ____
U|' \/ '|u   \/"_ \/ | \ |"|  U  /"\  u |  _"\  / __"| u
\| |\/| |/   | | | |<|  \| |>  \/ _ \/ /| | | |<\___ \/
 |_|  |_| \_)-\___/  |_| \_|  /_/   \_\ |____/ u|____/>>
<<,-,,-.       \\    ||   \\,-.\\    >>  |||_    )(  (__)
 (./  \.)     (__)   (_")  (_/(__)  (__)(__)_)  (__)
Christophe.Scholliers@UGent.be
*/
package edu.ugent.twist.topl.util;
import edu.ugent.twist.topl.monads.ListMonad;
import java.util.ArrayList;
import java.util.List;

// Creates a range
public class Range extends ArrayList<Integer> {
	public static ListMonad<Integer> make(Integer a, Integer b) {
		return new ListMonad<Integer>(new Range(a,b));
	}

	public Range(Integer from,Integer to) {
		while(from <= to) {
			this.add(from++);
		}
	}
}
