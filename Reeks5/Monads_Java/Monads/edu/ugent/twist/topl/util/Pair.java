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

// Class of pairs
public class Pair<A,B> {
	private A a;
	private B b;
	public Pair(A a, B b) {
		this.a = a;
		this.b = b;
	}
	public A first()  { return this.a; };
	public B second() { return this.b; };
	public String toString() {
		return "("+a+","+b+")";
	}
}
