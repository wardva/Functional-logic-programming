/*
  __  __    U  ___ u  _   _       _      ____    ____
U|' \/ '|u   \/"_ \/ | \ |"|  U  /"\  u |  _"\  / __"| u
\| |\/| |/   | | | |<|  \| |>  \/ _ \/ /| | | |<\___ \/
 |_|  |_| \_)-\___/  |_| \_|  /_/   \_\ |____/ u|____/>>
<<,-,,-.       \\    ||   \\,-.\\    >>  |||_    )(  (__)
 (./  \.)     (__)   (_")  (_/(__)  (__)(__)_)  (__)
Christophe.Scholliers@UGent.be
*/
package edu.ugent.twist.topl.monads;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

// The list monad implements the monad interface
/*
instance Monad [] where 
  return x = [ x ] 
  m >>= k  = [ y | x <- m, y <- k x]
*/
public class ListMonad<A> implements Monad<List,A> {
	public List<A> list       = new ArrayList<A>();
	public ListMonad()          {                      }
	public ListMonad(A a)       { this.list.add(a);    }
	public ListMonad(List<A> a) { this.list.addAll(a); }
	//TODO
	public static ListMonad<Integer> guard(Boolean b) {
		return null;
	}
	//TODO
	public ListMonad<A> Return (A a) {
		return null;
	}
	//TODO
	public <B> Monad<List,B> Bind(Transformer<A,List,B> t) {
		return null;
	}
       
	public String toString() {
		String s  = "[";
		for(A a : list) s += a + " "; 
		return s+"]";
	}
}
