/*
  __  __    U  ___ u  _   _       _      ____    ____
U|' \/ '|u   \/"_ \/ | \ |"|  U  /"\  u |  _"\  / __"| u
\| |\/| |/   | | | |<|  \| |>  \/ _ \/ /| | | |<\___ \/
 |_|  |_| \_)-\___/  |_| \_|  /_/   \_\ |____/ u|____/>>
<<,-,,-.       \\    ||   \\,-.\\    >>  |||_    )(  (__)
 (./  \.)     (__)   (_")  (_/(__)  (__)(__)_)  (__)
Christophe.Scholliers@UGent.be
*/
package edu.ugent.twist.topl.test;
import edu.ugent.twist.topl.monads.*;
import edu.ugent.twist.topl.util.*;
import java.util.List;

class TestMonad {
	public static void main(String[] args) {
		int max = 5;
		Monad Mp =     Range.make(0,max)     .Bind
                     ((a)  ->  Range.make(0,max)     .Bind
                     ((b)  ->  ListMonad.guard(a<b)  .Bind
                     ((x)  ->  new ListMonad<Pair<Integer,Integer>>(new Pair(a,b)))));
		System.out.println(Mp);
	}
}
