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
// The monad interface
public interface Monad<M,A> {
	// M a -> a-> M b -> M b
  // Note that M a is implicit in the signature here.
  // M a is the class which implements the monad interface
	public <B> Monad<M,B> Bind(Transformer<A,M,B> t);
	// a -> M a
	public Monad<M,A> Return (A a);
}
