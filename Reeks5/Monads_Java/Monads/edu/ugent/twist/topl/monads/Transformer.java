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

@FunctionalInterface
public interface Transformer<A,M,B> {
	public Monad<M,B> apply(A a);
}
