// Generated from /Users/lorenzobacchiani/Desktop/session-subtyping-tool/SessionType.g4 by ANTLR 4.10.1
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link SessionTypeParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface SessionTypeVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link SessionTypeParser#start}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStart(SessionTypeParser.StartContext ctx);
	/**
	 * Visit a parse tree produced by {@link SessionTypeParser#stype}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStype(SessionTypeParser.StypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link SessionTypeParser#guarded}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGuarded(SessionTypeParser.GuardedContext ctx);
}