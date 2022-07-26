// Generated from /Users/lorenzobacchiani/Desktop/session-subtyping-tool/SessionType.g4 by ANTLR 4.10.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link SessionTypeParser}.
 */
public interface SessionTypeListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link SessionTypeParser#start}.
	 * @param ctx the parse tree
	 */
	void enterStart(SessionTypeParser.StartContext ctx);
	/**
	 * Exit a parse tree produced by {@link SessionTypeParser#start}.
	 * @param ctx the parse tree
	 */
	void exitStart(SessionTypeParser.StartContext ctx);
	/**
	 * Enter a parse tree produced by {@link SessionTypeParser#stype}.
	 * @param ctx the parse tree
	 */
	void enterStype(SessionTypeParser.StypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link SessionTypeParser#stype}.
	 * @param ctx the parse tree
	 */
	void exitStype(SessionTypeParser.StypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link SessionTypeParser#guarded}.
	 * @param ctx the parse tree
	 */
	void enterGuarded(SessionTypeParser.GuardedContext ctx);
	/**
	 * Exit a parse tree produced by {@link SessionTypeParser#guarded}.
	 * @param ctx the parse tree
	 */
	void exitGuarded(SessionTypeParser.GuardedContext ctx);
}