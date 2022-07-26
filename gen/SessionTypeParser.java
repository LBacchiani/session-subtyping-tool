// Generated from /Users/lorenzobacchiani/Desktop/session-subtyping-tool/SessionType.g4 by ANTLR 4.10.1
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class SessionTypeParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.10.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		PLUS=1, CLPAR=2, CRPAR=3, SLPAR=4, SRPAR=5, SEMIC=6, COMMA=7, DOT=8, AND=9, 
		REC=10, END=11, OUT=12, IN=13, ID=14, WHITESP=15;
	public static final int
		RULE_start = 0, RULE_stype = 1, RULE_guarded = 2;
	private static String[] makeRuleNames() {
		return new String[] {
			"start", "stype", "guarded"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'+'", "'{'", "'}'", "'['", "']'", "';'", "','", "'.'", "'&'", 
			"'rec'", "'end'", "'!'", "'?'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "PLUS", "CLPAR", "CRPAR", "SLPAR", "SRPAR", "SEMIC", "COMMA", "DOT", 
			"AND", "REC", "END", "OUT", "IN", "ID", "WHITESP"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "SessionType.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public SessionTypeParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	public static class StartContext extends ParserRuleContext {
		public  type;
		public StypeContext s;
		public TerminalNode EOF() { return getToken(SessionTypeParser.EOF, 0); }
		public StypeContext stype() {
			return getRuleContext(StypeContext.class,0);
		}
		public StartContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_start; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SessionTypeListener ) ((SessionTypeListener)listener).enterStart(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SessionTypeListener ) ((SessionTypeListener)listener).exitStart(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof SessionTypeVisitor ) return ((SessionTypeVisitor<? extends T>)visitor).visitStart(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StartContext start() throws RecognitionException {
		StartContext _localctx = new StartContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_start);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(6);
			((StartContext)_localctx).s = stype();
			_localctx.type = ((StartContext)_localctx).s.type if ((StartContext)_localctx).s.type is not None else ""
			setState(8);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StypeContext extends ParserRuleContext {
		public  type;
		public Token i;
		public Token s;
		public StypeContext st;
		public Token c;
		public Token i2;
		public Token s2;
		public StypeContext st2;
		public Token r;
		public Token d;
		public GuardedContext g;
		public Token e;
		public Token o;
		public Token sl;
		public Token o2;
		public Token sr;
		public Token n;
		public Token n2;
		public TerminalNode PLUS() { return getToken(SessionTypeParser.PLUS, 0); }
		public TerminalNode CLPAR() { return getToken(SessionTypeParser.CLPAR, 0); }
		public TerminalNode CRPAR() { return getToken(SessionTypeParser.CRPAR, 0); }
		public List<TerminalNode> ID() { return getTokens(SessionTypeParser.ID); }
		public TerminalNode ID(int i) {
			return getToken(SessionTypeParser.ID, i);
		}
		public List<TerminalNode> SEMIC() { return getTokens(SessionTypeParser.SEMIC); }
		public TerminalNode SEMIC(int i) {
			return getToken(SessionTypeParser.SEMIC, i);
		}
		public List<StypeContext> stype() {
			return getRuleContexts(StypeContext.class);
		}
		public StypeContext stype(int i) {
			return getRuleContext(StypeContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(SessionTypeParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(SessionTypeParser.COMMA, i);
		}
		public TerminalNode AND() { return getToken(SessionTypeParser.AND, 0); }
		public TerminalNode REC() { return getToken(SessionTypeParser.REC, 0); }
		public TerminalNode DOT() { return getToken(SessionTypeParser.DOT, 0); }
		public GuardedContext guarded() {
			return getRuleContext(GuardedContext.class,0);
		}
		public TerminalNode END() { return getToken(SessionTypeParser.END, 0); }
		public List<TerminalNode> OUT() { return getTokens(SessionTypeParser.OUT); }
		public TerminalNode OUT(int i) {
			return getToken(SessionTypeParser.OUT, i);
		}
		public TerminalNode SLPAR() { return getToken(SessionTypeParser.SLPAR, 0); }
		public TerminalNode SRPAR() { return getToken(SessionTypeParser.SRPAR, 0); }
		public List<TerminalNode> IN() { return getTokens(SessionTypeParser.IN); }
		public TerminalNode IN(int i) {
			return getToken(SessionTypeParser.IN, i);
		}
		public StypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stype; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SessionTypeListener ) ((SessionTypeListener)listener).enterStype(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SessionTypeListener ) ((SessionTypeListener)listener).exitStype(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof SessionTypeVisitor ) return ((SessionTypeVisitor<? extends T>)visitor).visitStype(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StypeContext stype() throws RecognitionException {
		StypeContext _localctx = new StypeContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_stype);
		int _la;
		try {
			setState(116);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,4,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(10);
				match(PLUS);
				setState(11);
				match(CLPAR);
				_localctx.type = "["
				setState(13);
				((StypeContext)_localctx).i = match(ID);
				setState(14);
				((StypeContext)_localctx).s = match(SEMIC);
				setState(15);
				((StypeContext)_localctx).st = stype();
				_localctx.type += "!" + (((StypeContext)_localctx).i!=null?((StypeContext)_localctx).i.getText():null) + (((StypeContext)_localctx).s!=null?((StypeContext)_localctx).s.getText():null) + (((StypeContext)_localctx).st.type if not ((StypeContext)_localctx).st.type == None else "") 
				setState(25);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(17);
					((StypeContext)_localctx).c = match(COMMA);
					setState(18);
					((StypeContext)_localctx).i2 = match(ID);
					setState(19);
					((StypeContext)_localctx).s2 = match(SEMIC);
					setState(20);
					((StypeContext)_localctx).st2 = stype();
					_localctx.type += (((StypeContext)_localctx).c!=null?((StypeContext)_localctx).c.getText():null) + "!" + (((StypeContext)_localctx).i2!=null?((StypeContext)_localctx).i2.getText():null) + (((StypeContext)_localctx).s2!=null?((StypeContext)_localctx).s2.getText():null) + (((StypeContext)_localctx).st2.type if not ((StypeContext)_localctx).st2.type == None else "")
					}
					}
					setState(27);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				_localctx.type += "]"
				setState(29);
				match(CRPAR);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(31);
				match(AND);
				setState(32);
				match(CLPAR);
				_localctx.type = "["
				setState(34);
				((StypeContext)_localctx).i = match(ID);
				setState(35);
				((StypeContext)_localctx).s = match(SEMIC);
				setState(36);
				((StypeContext)_localctx).st = stype();
				_localctx.type += "?" + (((StypeContext)_localctx).i!=null?((StypeContext)_localctx).i.getText():null) + (((StypeContext)_localctx).s!=null?((StypeContext)_localctx).s.getText():null) + (((StypeContext)_localctx).st.type if not ((StypeContext)_localctx).st.type == None else "") 
				setState(46);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(38);
					((StypeContext)_localctx).c = match(COMMA);
					setState(39);
					((StypeContext)_localctx).i2 = match(ID);
					setState(40);
					((StypeContext)_localctx).s2 = match(SEMIC);
					setState(41);
					((StypeContext)_localctx).st2 = stype();
					_localctx.type += (((StypeContext)_localctx).c!=null?((StypeContext)_localctx).c.getText():null) + "?" + (((StypeContext)_localctx).i2!=null?((StypeContext)_localctx).i2.getText():null) + (((StypeContext)_localctx).s2!=null?((StypeContext)_localctx).s2.getText():null) + (((StypeContext)_localctx).st2.type if not ((StypeContext)_localctx).st2.type == None else "")
					}
					}
					setState(48);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				_localctx.type += "]"
				setState(50);
				match(CRPAR);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(52);
				((StypeContext)_localctx).r = match(REC);
				setState(53);
				((StypeContext)_localctx).i = match(ID);
				setState(54);
				((StypeContext)_localctx).d = match(DOT);
				setState(55);
				((StypeContext)_localctx).g = guarded();
				_localctx.type = (((StypeContext)_localctx).r!=null?((StypeContext)_localctx).r.getText():null) + " V" + (((StypeContext)_localctx).i!=null?((StypeContext)_localctx).i.getText():null) + " " + (((StypeContext)_localctx).d!=null?((StypeContext)_localctx).d.getText():null) + (((StypeContext)_localctx).g.type if not ((StypeContext)_localctx).g.type == None else "")
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(58);
				((StypeContext)_localctx).i = match(ID);
				_localctx.type = "V" + (((StypeContext)_localctx).i!=null?((StypeContext)_localctx).i.getText():null)
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(60);
				((StypeContext)_localctx).e = match(END);
				_localctx.type = (((StypeContext)_localctx).e!=null?((StypeContext)_localctx).e.getText():null)
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(62);
				((StypeContext)_localctx).o = match(OUT);
				setState(63);
				((StypeContext)_localctx).i = match(ID);
				setState(64);
				((StypeContext)_localctx).s = match(SEMIC);
				setState(65);
				((StypeContext)_localctx).st = stype();
				_localctx.type = (((StypeContext)_localctx).o!=null?((StypeContext)_localctx).o.getText():null) + (((StypeContext)_localctx).i!=null?((StypeContext)_localctx).i.getText():null) + (((StypeContext)_localctx).s!=null?((StypeContext)_localctx).s.getText():null) + (((StypeContext)_localctx).st.type if not ((StypeContext)_localctx).st.type == None else "") 
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(68);
				((StypeContext)_localctx).sl = match(SLPAR);
				setState(69);
				((StypeContext)_localctx).o = match(OUT);
				setState(70);
				((StypeContext)_localctx).i = match(ID);
				setState(71);
				((StypeContext)_localctx).s = match(SEMIC);
				setState(72);
				((StypeContext)_localctx).st = stype();
				_localctx.type = (((StypeContext)_localctx).sl!=null?((StypeContext)_localctx).sl.getText():null) + (((StypeContext)_localctx).o!=null?((StypeContext)_localctx).o.getText():null) + (((StypeContext)_localctx).i!=null?((StypeContext)_localctx).i.getText():null) + (((StypeContext)_localctx).s!=null?((StypeContext)_localctx).s.getText():null) + (((StypeContext)_localctx).st.type if not ((StypeContext)_localctx).st.type == None else "") 
				setState(83);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(74);
					((StypeContext)_localctx).c = match(COMMA);
					setState(75);
					((StypeContext)_localctx).o2 = match(OUT);
					setState(76);
					((StypeContext)_localctx).i2 = match(ID);
					setState(77);
					((StypeContext)_localctx).s2 = match(SEMIC);
					setState(78);
					((StypeContext)_localctx).st2 = stype();
					_localctx.type += (((StypeContext)_localctx).c!=null?((StypeContext)_localctx).c.getText():null) + (((StypeContext)_localctx).o2!=null?((StypeContext)_localctx).o2.getText():null) + (((StypeContext)_localctx).i2!=null?((StypeContext)_localctx).i2.getText():null) + (((StypeContext)_localctx).s2!=null?((StypeContext)_localctx).s2.getText():null) + (((StypeContext)_localctx).st2.type if not ((StypeContext)_localctx).st2.type == None else "")
					}
					}
					setState(85);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(86);
				((StypeContext)_localctx).sr = match(SRPAR);
				_localctx.type += (((StypeContext)_localctx).sr!=null?((StypeContext)_localctx).sr.getText():null)
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(89);
				((StypeContext)_localctx).n = match(IN);
				setState(90);
				((StypeContext)_localctx).i = match(ID);
				setState(91);
				((StypeContext)_localctx).s = match(SEMIC);
				setState(92);
				((StypeContext)_localctx).st = stype();
				_localctx.type = (((StypeContext)_localctx).n!=null?((StypeContext)_localctx).n.getText():null) + (((StypeContext)_localctx).i!=null?((StypeContext)_localctx).i.getText():null) + (((StypeContext)_localctx).s!=null?((StypeContext)_localctx).s.getText():null) + (((StypeContext)_localctx).st.type if not ((StypeContext)_localctx).st.type == None else "") 
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(95);
				((StypeContext)_localctx).sl = match(SLPAR);
				setState(96);
				((StypeContext)_localctx).n = match(IN);
				setState(97);
				((StypeContext)_localctx).i = match(ID);
				setState(98);
				((StypeContext)_localctx).s = match(SEMIC);
				setState(99);
				((StypeContext)_localctx).st = stype();
				_localctx.type = (((StypeContext)_localctx).sl!=null?((StypeContext)_localctx).sl.getText():null) + (((StypeContext)_localctx).n!=null?((StypeContext)_localctx).n.getText():null) + (((StypeContext)_localctx).i!=null?((StypeContext)_localctx).i.getText():null) + (((StypeContext)_localctx).s!=null?((StypeContext)_localctx).s.getText():null) + (((StypeContext)_localctx).st.type if not ((StypeContext)_localctx).st.type == None else "") 
				setState(110);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(101);
					((StypeContext)_localctx).c = match(COMMA);
					setState(102);
					((StypeContext)_localctx).n2 = match(IN);
					setState(103);
					((StypeContext)_localctx).i2 = match(ID);
					setState(104);
					((StypeContext)_localctx).s2 = match(SEMIC);
					setState(105);
					((StypeContext)_localctx).st2 = stype();
					_localctx.type += (((StypeContext)_localctx).c!=null?((StypeContext)_localctx).c.getText():null) + (((StypeContext)_localctx).n2!=null?((StypeContext)_localctx).n2.getText():null) + (((StypeContext)_localctx).i2!=null?((StypeContext)_localctx).i2.getText():null) + (((StypeContext)_localctx).s2!=null?((StypeContext)_localctx).s2.getText():null) + (((StypeContext)_localctx).st2.type if not ((StypeContext)_localctx).st2.type == None else "")
					}
					}
					setState(112);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(113);
				((StypeContext)_localctx).sr = match(SRPAR);
				_localctx.type += (((StypeContext)_localctx).sr!=null?((StypeContext)_localctx).sr.getText():null)
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class GuardedContext extends ParserRuleContext {
		public  type;
		public Token i;
		public Token s;
		public StypeContext st;
		public Token c;
		public Token i2;
		public Token s2;
		public StypeContext st2;
		public Token r;
		public Token d;
		public GuardedContext g;
		public Token e;
		public Token o;
		public Token sl;
		public Token o2;
		public Token sr;
		public Token n;
		public Token n2;
		public TerminalNode PLUS() { return getToken(SessionTypeParser.PLUS, 0); }
		public TerminalNode CLPAR() { return getToken(SessionTypeParser.CLPAR, 0); }
		public TerminalNode CRPAR() { return getToken(SessionTypeParser.CRPAR, 0); }
		public List<TerminalNode> ID() { return getTokens(SessionTypeParser.ID); }
		public TerminalNode ID(int i) {
			return getToken(SessionTypeParser.ID, i);
		}
		public List<TerminalNode> SEMIC() { return getTokens(SessionTypeParser.SEMIC); }
		public TerminalNode SEMIC(int i) {
			return getToken(SessionTypeParser.SEMIC, i);
		}
		public List<StypeContext> stype() {
			return getRuleContexts(StypeContext.class);
		}
		public StypeContext stype(int i) {
			return getRuleContext(StypeContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(SessionTypeParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(SessionTypeParser.COMMA, i);
		}
		public TerminalNode AND() { return getToken(SessionTypeParser.AND, 0); }
		public TerminalNode REC() { return getToken(SessionTypeParser.REC, 0); }
		public TerminalNode DOT() { return getToken(SessionTypeParser.DOT, 0); }
		public GuardedContext guarded() {
			return getRuleContext(GuardedContext.class,0);
		}
		public TerminalNode END() { return getToken(SessionTypeParser.END, 0); }
		public List<TerminalNode> OUT() { return getTokens(SessionTypeParser.OUT); }
		public TerminalNode OUT(int i) {
			return getToken(SessionTypeParser.OUT, i);
		}
		public TerminalNode SLPAR() { return getToken(SessionTypeParser.SLPAR, 0); }
		public TerminalNode SRPAR() { return getToken(SessionTypeParser.SRPAR, 0); }
		public List<TerminalNode> IN() { return getTokens(SessionTypeParser.IN); }
		public TerminalNode IN(int i) {
			return getToken(SessionTypeParser.IN, i);
		}
		public GuardedContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_guarded; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SessionTypeListener ) ((SessionTypeListener)listener).enterGuarded(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SessionTypeListener ) ((SessionTypeListener)listener).exitGuarded(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof SessionTypeVisitor ) return ((SessionTypeVisitor<? extends T>)visitor).visitGuarded(this);
			else return visitor.visitChildren(this);
		}
	}

	public final GuardedContext guarded() throws RecognitionException {
		GuardedContext _localctx = new GuardedContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_guarded);
		int _la;
		try {
			setState(222);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,9,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(118);
				match(PLUS);
				setState(119);
				match(CLPAR);
				_localctx.type = "["
				setState(121);
				((GuardedContext)_localctx).i = match(ID);
				setState(122);
				((GuardedContext)_localctx).s = match(SEMIC);
				setState(123);
				((GuardedContext)_localctx).st = stype();
				_localctx.type += "!" + (((GuardedContext)_localctx).i!=null?((GuardedContext)_localctx).i.getText():null) + (((GuardedContext)_localctx).s!=null?((GuardedContext)_localctx).s.getText():null) + (((GuardedContext)_localctx).st.type if not ((GuardedContext)_localctx).st.type == None else "") 
				setState(133);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(125);
					((GuardedContext)_localctx).c = match(COMMA);
					setState(126);
					((GuardedContext)_localctx).i2 = match(ID);
					setState(127);
					((GuardedContext)_localctx).s2 = match(SEMIC);
					setState(128);
					((GuardedContext)_localctx).st2 = stype();
					_localctx.type += (((GuardedContext)_localctx).c!=null?((GuardedContext)_localctx).c.getText():null) + "!" + (((GuardedContext)_localctx).i2!=null?((GuardedContext)_localctx).i2.getText():null) + (((GuardedContext)_localctx).s2!=null?((GuardedContext)_localctx).s2.getText():null) + (((GuardedContext)_localctx).st2.type if not ((GuardedContext)_localctx).st2.type == None else "")
					}
					}
					setState(135);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				_localctx.type += "]"
				setState(137);
				match(CRPAR);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(139);
				match(AND);
				setState(140);
				match(CLPAR);
				_localctx.type = "["
				setState(142);
				((GuardedContext)_localctx).i = match(ID);
				setState(143);
				((GuardedContext)_localctx).s = match(SEMIC);
				setState(144);
				((GuardedContext)_localctx).st = stype();
				_localctx.type += "?" + (((GuardedContext)_localctx).i!=null?((GuardedContext)_localctx).i.getText():null) + (((GuardedContext)_localctx).s!=null?((GuardedContext)_localctx).s.getText():null) + (((GuardedContext)_localctx).st.type if not ((GuardedContext)_localctx).st.type == None else "") 
				setState(154);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(146);
					((GuardedContext)_localctx).c = match(COMMA);
					setState(147);
					((GuardedContext)_localctx).i2 = match(ID);
					setState(148);
					((GuardedContext)_localctx).s2 = match(SEMIC);
					setState(149);
					((GuardedContext)_localctx).st2 = stype();
					_localctx.type += (((GuardedContext)_localctx).c!=null?((GuardedContext)_localctx).c.getText():null) + "?" + (((GuardedContext)_localctx).i2!=null?((GuardedContext)_localctx).i2.getText():null) + (((GuardedContext)_localctx).s2!=null?((GuardedContext)_localctx).s2.getText():null) + (((GuardedContext)_localctx).st2.type if not ((GuardedContext)_localctx).st2.type == None else "")
					}
					}
					setState(156);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				_localctx.type += "]"
				setState(158);
				match(CRPAR);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(160);
				((GuardedContext)_localctx).r = match(REC);
				setState(161);
				((GuardedContext)_localctx).i = match(ID);
				setState(162);
				((GuardedContext)_localctx).d = match(DOT);
				setState(163);
				((GuardedContext)_localctx).g = guarded();
				_localctx.type = (((GuardedContext)_localctx).r!=null?((GuardedContext)_localctx).r.getText():null) + " V" + (((GuardedContext)_localctx).i!=null?((GuardedContext)_localctx).i.getText():null) + " " + (((GuardedContext)_localctx).d!=null?((GuardedContext)_localctx).d.getText():null) + (((GuardedContext)_localctx).g.type if not ((GuardedContext)_localctx).g.type == None else "")
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(166);
				((GuardedContext)_localctx).e = match(END);
				_localctx.type = (((GuardedContext)_localctx).e!=null?((GuardedContext)_localctx).e.getText():null)
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(168);
				((GuardedContext)_localctx).o = match(OUT);
				setState(169);
				((GuardedContext)_localctx).i = match(ID);
				setState(170);
				((GuardedContext)_localctx).s = match(SEMIC);
				setState(171);
				((GuardedContext)_localctx).st = stype();
				_localctx.type = (((GuardedContext)_localctx).o!=null?((GuardedContext)_localctx).o.getText():null) + (((GuardedContext)_localctx).i!=null?((GuardedContext)_localctx).i.getText():null) + (((GuardedContext)_localctx).s!=null?((GuardedContext)_localctx).s.getText():null) + (((GuardedContext)_localctx).st.type if not ((GuardedContext)_localctx).st.type == None else "") 
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(174);
				((GuardedContext)_localctx).sl = match(SLPAR);
				setState(175);
				((GuardedContext)_localctx).o = match(OUT);
				setState(176);
				((GuardedContext)_localctx).i = match(ID);
				setState(177);
				((GuardedContext)_localctx).s = match(SEMIC);
				setState(178);
				((GuardedContext)_localctx).st = stype();
				_localctx.type = (((GuardedContext)_localctx).sl!=null?((GuardedContext)_localctx).sl.getText():null) + (((GuardedContext)_localctx).o!=null?((GuardedContext)_localctx).o.getText():null) + (((GuardedContext)_localctx).i!=null?((GuardedContext)_localctx).i.getText():null) + (((GuardedContext)_localctx).s!=null?((GuardedContext)_localctx).s.getText():null) + (((GuardedContext)_localctx).st.type if not ((GuardedContext)_localctx).st.type == None else "") 
				setState(189);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(180);
					((GuardedContext)_localctx).c = match(COMMA);
					setState(181);
					((GuardedContext)_localctx).o2 = match(OUT);
					setState(182);
					((GuardedContext)_localctx).i2 = match(ID);
					setState(183);
					((GuardedContext)_localctx).s2 = match(SEMIC);
					setState(184);
					((GuardedContext)_localctx).st2 = stype();
					_localctx.type += (((GuardedContext)_localctx).c!=null?((GuardedContext)_localctx).c.getText():null) + (((GuardedContext)_localctx).o2!=null?((GuardedContext)_localctx).o2.getText():null) + (((GuardedContext)_localctx).i2!=null?((GuardedContext)_localctx).i2.getText():null) + (((GuardedContext)_localctx).s2!=null?((GuardedContext)_localctx).s2.getText():null) + (((GuardedContext)_localctx).st2.type if not ((GuardedContext)_localctx).st2.type == None else "")
					}
					}
					setState(191);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(192);
				((GuardedContext)_localctx).sr = match(SRPAR);
				_localctx.type += (((GuardedContext)_localctx).sr!=null?((GuardedContext)_localctx).sr.getText():null)
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(195);
				((GuardedContext)_localctx).n = match(IN);
				setState(196);
				((GuardedContext)_localctx).i = match(ID);
				setState(197);
				((GuardedContext)_localctx).s = match(SEMIC);
				setState(198);
				((GuardedContext)_localctx).st = stype();
				_localctx.type = (((GuardedContext)_localctx).n!=null?((GuardedContext)_localctx).n.getText():null) + (((GuardedContext)_localctx).i!=null?((GuardedContext)_localctx).i.getText():null) + (((GuardedContext)_localctx).s!=null?((GuardedContext)_localctx).s.getText():null) + (((GuardedContext)_localctx).st.type if not ((GuardedContext)_localctx).st.type == None else "") 
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(201);
				((GuardedContext)_localctx).sl = match(SLPAR);
				setState(202);
				((GuardedContext)_localctx).n = match(IN);
				setState(203);
				((GuardedContext)_localctx).i = match(ID);
				setState(204);
				((GuardedContext)_localctx).s = match(SEMIC);
				setState(205);
				((GuardedContext)_localctx).st = stype();
				_localctx.type = (((GuardedContext)_localctx).sl!=null?((GuardedContext)_localctx).sl.getText():null) + (((GuardedContext)_localctx).n!=null?((GuardedContext)_localctx).n.getText():null) + (((GuardedContext)_localctx).i!=null?((GuardedContext)_localctx).i.getText():null) + (((GuardedContext)_localctx).s!=null?((GuardedContext)_localctx).s.getText():null) + (((GuardedContext)_localctx).st.type if not ((GuardedContext)_localctx).st.type == None else "") 
				setState(216);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(207);
					((GuardedContext)_localctx).c = match(COMMA);
					setState(208);
					((GuardedContext)_localctx).n2 = match(IN);
					setState(209);
					((GuardedContext)_localctx).i2 = match(ID);
					setState(210);
					((GuardedContext)_localctx).s2 = match(SEMIC);
					setState(211);
					((GuardedContext)_localctx).st2 = stype();
					_localctx.type += (((GuardedContext)_localctx).c!=null?((GuardedContext)_localctx).c.getText():null) + (((GuardedContext)_localctx).n2!=null?((GuardedContext)_localctx).n2.getText():null) + (((GuardedContext)_localctx).i2!=null?((GuardedContext)_localctx).i2.getText():null) + (((GuardedContext)_localctx).s2!=null?((GuardedContext)_localctx).s2.getText():null) + (((GuardedContext)_localctx).st2.type if not ((GuardedContext)_localctx).st2.type == None else "")
					}
					}
					setState(218);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(219);
				((GuardedContext)_localctx).sr = match(SRPAR);
				_localctx.type += (((GuardedContext)_localctx).sr!=null?((GuardedContext)_localctx).sr.getText():null)
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\u0004\u0001\u000f\u00e1\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001"+
		"\u0002\u0002\u0007\u0002\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0005\u0001\u0018\b\u0001\n\u0001\f\u0001\u001b\t\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0005\u0001-\b\u0001\n\u0001"+
		"\f\u00010\t\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0005\u0001R\b\u0001\n\u0001"+
		"\f\u0001U\t\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0005"+
		"\u0001m\b\u0001\n\u0001\f\u0001p\t\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0003\u0001u\b\u0001\u0001\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0005\u0002\u0084\b\u0002\n"+
		"\u0002\f\u0002\u0087\t\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0005\u0002\u0099\b\u0002\n\u0002\f\u0002\u009c\t\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0005\u0002"+
		"\u00bc\b\u0002\n\u0002\f\u0002\u00bf\t\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0005\u0002\u00d7\b\u0002\n\u0002\f\u0002\u00da\t\u0002"+
		"\u0001\u0002\u0001\u0002\u0001\u0002\u0003\u0002\u00df\b\u0002\u0001\u0002"+
		"\u0000\u0000\u0003\u0000\u0002\u0004\u0000\u0000\u00f4\u0000\u0006\u0001"+
		"\u0000\u0000\u0000\u0002t\u0001\u0000\u0000\u0000\u0004\u00de\u0001\u0000"+
		"\u0000\u0000\u0006\u0007\u0003\u0002\u0001\u0000\u0007\b\u0006\u0000\uffff"+
		"\uffff\u0000\b\t\u0005\u0000\u0000\u0001\t\u0001\u0001\u0000\u0000\u0000"+
		"\n\u000b\u0005\u0001\u0000\u0000\u000b\f\u0005\u0002\u0000\u0000\f\r\u0006"+
		"\u0001\uffff\uffff\u0000\r\u000e\u0005\u000e\u0000\u0000\u000e\u000f\u0005"+
		"\u0006\u0000\u0000\u000f\u0010\u0003\u0002\u0001\u0000\u0010\u0019\u0006"+
		"\u0001\uffff\uffff\u0000\u0011\u0012\u0005\u0007\u0000\u0000\u0012\u0013"+
		"\u0005\u000e\u0000\u0000\u0013\u0014\u0005\u0006\u0000\u0000\u0014\u0015"+
		"\u0003\u0002\u0001\u0000\u0015\u0016\u0006\u0001\uffff\uffff\u0000\u0016"+
		"\u0018\u0001\u0000\u0000\u0000\u0017\u0011\u0001\u0000\u0000\u0000\u0018"+
		"\u001b\u0001\u0000\u0000\u0000\u0019\u0017\u0001\u0000\u0000\u0000\u0019"+
		"\u001a\u0001\u0000\u0000\u0000\u001a\u001c\u0001\u0000\u0000\u0000\u001b"+
		"\u0019\u0001\u0000\u0000\u0000\u001c\u001d\u0006\u0001\uffff\uffff\u0000"+
		"\u001d\u001e\u0005\u0003\u0000\u0000\u001eu\u0001\u0000\u0000\u0000\u001f"+
		" \u0005\t\u0000\u0000 !\u0005\u0002\u0000\u0000!\"\u0006\u0001\uffff\uffff"+
		"\u0000\"#\u0005\u000e\u0000\u0000#$\u0005\u0006\u0000\u0000$%\u0003\u0002"+
		"\u0001\u0000%.\u0006\u0001\uffff\uffff\u0000&\'\u0005\u0007\u0000\u0000"+
		"\'(\u0005\u000e\u0000\u0000()\u0005\u0006\u0000\u0000)*\u0003\u0002\u0001"+
		"\u0000*+\u0006\u0001\uffff\uffff\u0000+-\u0001\u0000\u0000\u0000,&\u0001"+
		"\u0000\u0000\u0000-0\u0001\u0000\u0000\u0000.,\u0001\u0000\u0000\u0000"+
		"./\u0001\u0000\u0000\u0000/1\u0001\u0000\u0000\u00000.\u0001\u0000\u0000"+
		"\u000012\u0006\u0001\uffff\uffff\u000023\u0005\u0003\u0000\u00003u\u0001"+
		"\u0000\u0000\u000045\u0005\n\u0000\u000056\u0005\u000e\u0000\u000067\u0005"+
		"\b\u0000\u000078\u0003\u0004\u0002\u000089\u0006\u0001\uffff\uffff\u0000"+
		"9u\u0001\u0000\u0000\u0000:;\u0005\u000e\u0000\u0000;u\u0006\u0001\uffff"+
		"\uffff\u0000<=\u0005\u000b\u0000\u0000=u\u0006\u0001\uffff\uffff\u0000"+
		">?\u0005\f\u0000\u0000?@\u0005\u000e\u0000\u0000@A\u0005\u0006\u0000\u0000"+
		"AB\u0003\u0002\u0001\u0000BC\u0006\u0001\uffff\uffff\u0000Cu\u0001\u0000"+
		"\u0000\u0000DE\u0005\u0004\u0000\u0000EF\u0005\f\u0000\u0000FG\u0005\u000e"+
		"\u0000\u0000GH\u0005\u0006\u0000\u0000HI\u0003\u0002\u0001\u0000IS\u0006"+
		"\u0001\uffff\uffff\u0000JK\u0005\u0007\u0000\u0000KL\u0005\f\u0000\u0000"+
		"LM\u0005\u000e\u0000\u0000MN\u0005\u0006\u0000\u0000NO\u0003\u0002\u0001"+
		"\u0000OP\u0006\u0001\uffff\uffff\u0000PR\u0001\u0000\u0000\u0000QJ\u0001"+
		"\u0000\u0000\u0000RU\u0001\u0000\u0000\u0000SQ\u0001\u0000\u0000\u0000"+
		"ST\u0001\u0000\u0000\u0000TV\u0001\u0000\u0000\u0000US\u0001\u0000\u0000"+
		"\u0000VW\u0005\u0005\u0000\u0000WX\u0006\u0001\uffff\uffff\u0000Xu\u0001"+
		"\u0000\u0000\u0000YZ\u0005\r\u0000\u0000Z[\u0005\u000e\u0000\u0000[\\"+
		"\u0005\u0006\u0000\u0000\\]\u0003\u0002\u0001\u0000]^\u0006\u0001\uffff"+
		"\uffff\u0000^u\u0001\u0000\u0000\u0000_`\u0005\u0004\u0000\u0000`a\u0005"+
		"\r\u0000\u0000ab\u0005\u000e\u0000\u0000bc\u0005\u0006\u0000\u0000cd\u0003"+
		"\u0002\u0001\u0000dn\u0006\u0001\uffff\uffff\u0000ef\u0005\u0007\u0000"+
		"\u0000fg\u0005\r\u0000\u0000gh\u0005\u000e\u0000\u0000hi\u0005\u0006\u0000"+
		"\u0000ij\u0003\u0002\u0001\u0000jk\u0006\u0001\uffff\uffff\u0000km\u0001"+
		"\u0000\u0000\u0000le\u0001\u0000\u0000\u0000mp\u0001\u0000\u0000\u0000"+
		"nl\u0001\u0000\u0000\u0000no\u0001\u0000\u0000\u0000oq\u0001\u0000\u0000"+
		"\u0000pn\u0001\u0000\u0000\u0000qr\u0005\u0005\u0000\u0000rs\u0006\u0001"+
		"\uffff\uffff\u0000su\u0001\u0000\u0000\u0000t\n\u0001\u0000\u0000\u0000"+
		"t\u001f\u0001\u0000\u0000\u0000t4\u0001\u0000\u0000\u0000t:\u0001\u0000"+
		"\u0000\u0000t<\u0001\u0000\u0000\u0000t>\u0001\u0000\u0000\u0000tD\u0001"+
		"\u0000\u0000\u0000tY\u0001\u0000\u0000\u0000t_\u0001\u0000\u0000\u0000"+
		"u\u0003\u0001\u0000\u0000\u0000vw\u0005\u0001\u0000\u0000wx\u0005\u0002"+
		"\u0000\u0000xy\u0006\u0002\uffff\uffff\u0000yz\u0005\u000e\u0000\u0000"+
		"z{\u0005\u0006\u0000\u0000{|\u0003\u0002\u0001\u0000|\u0085\u0006\u0002"+
		"\uffff\uffff\u0000}~\u0005\u0007\u0000\u0000~\u007f\u0005\u000e\u0000"+
		"\u0000\u007f\u0080\u0005\u0006\u0000\u0000\u0080\u0081\u0003\u0002\u0001"+
		"\u0000\u0081\u0082\u0006\u0002\uffff\uffff\u0000\u0082\u0084\u0001\u0000"+
		"\u0000\u0000\u0083}\u0001\u0000\u0000\u0000\u0084\u0087\u0001\u0000\u0000"+
		"\u0000\u0085\u0083\u0001\u0000\u0000\u0000\u0085\u0086\u0001\u0000\u0000"+
		"\u0000\u0086\u0088\u0001\u0000\u0000\u0000\u0087\u0085\u0001\u0000\u0000"+
		"\u0000\u0088\u0089\u0006\u0002\uffff\uffff\u0000\u0089\u008a\u0005\u0003"+
		"\u0000\u0000\u008a\u00df\u0001\u0000\u0000\u0000\u008b\u008c\u0005\t\u0000"+
		"\u0000\u008c\u008d\u0005\u0002\u0000\u0000\u008d\u008e\u0006\u0002\uffff"+
		"\uffff\u0000\u008e\u008f\u0005\u000e\u0000\u0000\u008f\u0090\u0005\u0006"+
		"\u0000\u0000\u0090\u0091\u0003\u0002\u0001\u0000\u0091\u009a\u0006\u0002"+
		"\uffff\uffff\u0000\u0092\u0093\u0005\u0007\u0000\u0000\u0093\u0094\u0005"+
		"\u000e\u0000\u0000\u0094\u0095\u0005\u0006\u0000\u0000\u0095\u0096\u0003"+
		"\u0002\u0001\u0000\u0096\u0097\u0006\u0002\uffff\uffff\u0000\u0097\u0099"+
		"\u0001\u0000\u0000\u0000\u0098\u0092\u0001\u0000\u0000\u0000\u0099\u009c"+
		"\u0001\u0000\u0000\u0000\u009a\u0098\u0001\u0000\u0000\u0000\u009a\u009b"+
		"\u0001\u0000\u0000\u0000\u009b\u009d\u0001\u0000\u0000\u0000\u009c\u009a"+
		"\u0001\u0000\u0000\u0000\u009d\u009e\u0006\u0002\uffff\uffff\u0000\u009e"+
		"\u009f\u0005\u0003\u0000\u0000\u009f\u00df\u0001\u0000\u0000\u0000\u00a0"+
		"\u00a1\u0005\n\u0000\u0000\u00a1\u00a2\u0005\u000e\u0000\u0000\u00a2\u00a3"+
		"\u0005\b\u0000\u0000\u00a3\u00a4\u0003\u0004\u0002\u0000\u00a4\u00a5\u0006"+
		"\u0002\uffff\uffff\u0000\u00a5\u00df\u0001\u0000\u0000\u0000\u00a6\u00a7"+
		"\u0005\u000b\u0000\u0000\u00a7\u00df\u0006\u0002\uffff\uffff\u0000\u00a8"+
		"\u00a9\u0005\f\u0000\u0000\u00a9\u00aa\u0005\u000e\u0000\u0000\u00aa\u00ab"+
		"\u0005\u0006\u0000\u0000\u00ab\u00ac\u0003\u0002\u0001\u0000\u00ac\u00ad"+
		"\u0006\u0002\uffff\uffff\u0000\u00ad\u00df\u0001\u0000\u0000\u0000\u00ae"+
		"\u00af\u0005\u0004\u0000\u0000\u00af\u00b0\u0005\f\u0000\u0000\u00b0\u00b1"+
		"\u0005\u000e\u0000\u0000\u00b1\u00b2\u0005\u0006\u0000\u0000\u00b2\u00b3"+
		"\u0003\u0002\u0001\u0000\u00b3\u00bd\u0006\u0002\uffff\uffff\u0000\u00b4"+
		"\u00b5\u0005\u0007\u0000\u0000\u00b5\u00b6\u0005\f\u0000\u0000\u00b6\u00b7"+
		"\u0005\u000e\u0000\u0000\u00b7\u00b8\u0005\u0006\u0000\u0000\u00b8\u00b9"+
		"\u0003\u0002\u0001\u0000\u00b9\u00ba\u0006\u0002\uffff\uffff\u0000\u00ba"+
		"\u00bc\u0001\u0000\u0000\u0000\u00bb\u00b4\u0001\u0000\u0000\u0000\u00bc"+
		"\u00bf\u0001\u0000\u0000\u0000\u00bd\u00bb\u0001\u0000\u0000\u0000\u00bd"+
		"\u00be\u0001\u0000\u0000\u0000\u00be\u00c0\u0001\u0000\u0000\u0000\u00bf"+
		"\u00bd\u0001\u0000\u0000\u0000\u00c0\u00c1\u0005\u0005\u0000\u0000\u00c1"+
		"\u00c2\u0006\u0002\uffff\uffff\u0000\u00c2\u00df\u0001\u0000\u0000\u0000"+
		"\u00c3\u00c4\u0005\r\u0000\u0000\u00c4\u00c5\u0005\u000e\u0000\u0000\u00c5"+
		"\u00c6\u0005\u0006\u0000\u0000\u00c6\u00c7\u0003\u0002\u0001\u0000\u00c7"+
		"\u00c8\u0006\u0002\uffff\uffff\u0000\u00c8\u00df\u0001\u0000\u0000\u0000"+
		"\u00c9\u00ca\u0005\u0004\u0000\u0000\u00ca\u00cb\u0005\r\u0000\u0000\u00cb"+
		"\u00cc\u0005\u000e\u0000\u0000\u00cc\u00cd\u0005\u0006\u0000\u0000\u00cd"+
		"\u00ce\u0003\u0002\u0001\u0000\u00ce\u00d8\u0006\u0002\uffff\uffff\u0000"+
		"\u00cf\u00d0\u0005\u0007\u0000\u0000\u00d0\u00d1\u0005\r\u0000\u0000\u00d1"+
		"\u00d2\u0005\u000e\u0000\u0000\u00d2\u00d3\u0005\u0006\u0000\u0000\u00d3"+
		"\u00d4\u0003\u0002\u0001\u0000\u00d4\u00d5\u0006\u0002\uffff\uffff\u0000"+
		"\u00d5\u00d7\u0001\u0000\u0000\u0000\u00d6\u00cf\u0001\u0000\u0000\u0000"+
		"\u00d7\u00da\u0001\u0000\u0000\u0000\u00d8\u00d6\u0001\u0000\u0000\u0000"+
		"\u00d8\u00d9\u0001\u0000\u0000\u0000\u00d9\u00db\u0001\u0000\u0000\u0000"+
		"\u00da\u00d8\u0001\u0000\u0000\u0000\u00db\u00dc\u0005\u0005\u0000\u0000"+
		"\u00dc\u00dd\u0006\u0002\uffff\uffff\u0000\u00dd\u00df\u0001\u0000\u0000"+
		"\u0000\u00dev\u0001\u0000\u0000\u0000\u00de\u008b\u0001\u0000\u0000\u0000"+
		"\u00de\u00a0\u0001\u0000\u0000\u0000\u00de\u00a6\u0001\u0000\u0000\u0000"+
		"\u00de\u00a8\u0001\u0000\u0000\u0000\u00de\u00ae\u0001\u0000\u0000\u0000"+
		"\u00de\u00c3\u0001\u0000\u0000\u0000\u00de\u00c9\u0001\u0000\u0000\u0000"+
		"\u00df\u0005\u0001\u0000\u0000\u0000\n\u0019.Snt\u0085\u009a\u00bd\u00d8"+
		"\u00de";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}