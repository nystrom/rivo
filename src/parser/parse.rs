use num::bigint::BigInt;
use num::rational::BigRational;

use syntax::loc::*;
use syntax::trees::*;

use parser::tokens::Token;
use parser::lex::Lexer;

#[macro_export]
macro_rules! located {
    ($parser: expr, $located: expr) => {
        {
            let first_token = $parser.lookahead();
            let v = $located;
            let last_token = &$parser.last_token;
            Located {
                loc: Loc {
                    start: first_token.loc.start.clone(),
                    end: last_token.loc.end.clone(),
                    source: first_token.loc.source.clone(),
                },
                value: v
            }
        }
    };
}

pub struct Parser<'a> {
    pub lex: Lexer<'a>,
    pub last_token: Located<Token>,
    pub errors: Vec<Located<String>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, input: &'a str) -> Parser<'a> {
        Parser {
            lex: Lexer::new(source, input),
            last_token: Located {
                loc: NO_LOC,
                value: Token::EOF
            },
            errors: vec![],
        }
    }

    pub fn new_from_lexer(lex: Lexer<'a>) -> Parser<'a> {
        Parser {
            lex,
            last_token: Located {
                loc: NO_LOC,
                value: Token::EOF
            },
            errors: vec![],
        }
    }

    fn lookahead(&mut self) -> Located<Token> {
        let t = self.lex.peek_token();
        t
    }

    fn error(&mut self, loc: Loc, msg: &str) {
        self.errors.push(Located { loc: loc, value: String::from(msg) });
    }

    fn eat(&mut self, expected: Token) {
        let t = self.lookahead();
        let loc = t.loc.clone();
        match *t {
            Token::BadInt =>
                self.error(loc, "bad integer literal"),
            Token::BadChar =>
                self.error(loc, "bad character literal"),
            Token::BadString =>
                self.error(loc, "bad string literal"),
            Token::BadRat =>
                self.error(loc, "bad rational literal"),
            Token::BadComment =>
                self.error(loc, "bad comment"),
            Token::UnexpectedChar(ch) =>
                self.error(loc, "unexpected character"),
            ref q => {
                if *q == expected {
                    self.last_token = self.lex.next_token();
                }
                else {
                    self.error(loc, "unexpected token");
                }
            },
        }
    }

    fn eat_anything(&mut self) {
        let t = self.lookahead();
        let loc = t.loc.clone();
        match *t {
            Token::BadInt =>
                self.error(loc, "bad integer literal"),
            Token::BadChar =>
                self.error(loc, "bad character literal"),
            Token::BadString =>
                self.error(loc, "bad string literal"),
            Token::BadRat =>
                self.error(loc, "bad rational literal"),
            Token::BadComment =>
                self.error(loc, "bad comment"),
            Token::UnexpectedChar(ch) =>
                self.error(loc, "unexpected character"),
            _ => {
                self.last_token = self.lex.next_token();
            },
        }
    }

    pub fn parse_bundle(&mut self) -> Located<Root> {
        let mut cs = Vec::new();

        located!(self, {
            loop {
                match *self.lookahead() {
                    Token::EOF => {
                        break;
                    },
                    Token::Semi => {
                        // nothing
                        self.eat_anything();
                    },
                    _ => {
                        let c = self.parse_cmd();
                        cs.push(c);
                    },
                }
            }

            println!("commands {:?}", cs);
            Root::Parsed { cmds: cs }
        })
    }

    fn parse_cmd(&mut self) -> Located<Cmd> {
        let e = Cmd::Exp(Exp::Literal { lit: Lit::Nothing });
        located!(self, {
            loop {
                match *self.lookahead() {
                    Token::EOF => break,
                    Token::Semi => break,
                    _ => self.eat_anything(),
                }
            }
            e
        })
    }
}

mod tests {
    use parser::parse::Parser;
    use syntax::trees::*;

    #[test]
    fn test_empty_bundle_1() {
        let mut p = Parser::new("foo.ivo", "");
        let t = p.parse_bundle();
        assert_eq!(*t, Root::Parsed { cmds: vec![] })
    }

    #[test]
    fn test_empty_bundle_2() {
        let mut p = Parser::new("foo.ivo", ";;;;");
        let t = p.parse_bundle();
        assert_eq!(*t, Root::Parsed { cmds: vec![] })
    }

    #[test]
    fn test_empty_bundle_3() {
        let mut p = Parser::new("foo.ivo", ";;11;;");
        let t = p.parse_bundle();
        assert_eq!(*t, Root::Parsed { cmds: vec![] })
    }
}

/*
  var la: Token = scanner.nextToken

  def eat(kind: Kind) = {
    if (la.kind == kind) {
      la = scanner.nextToken
    }
    else {
      error(s"Unexpected token $la, expected $kind")
    }
  }

  def root: L[AmbBundle] = {
    val p = program
    eat(EOF)
    p
  }

  val lexer = scanner

  def positioned[A](a: => A): L[A] = {
    val start = lexer.position
    val b = a
    val end = lexer.position
    Located(span(start, end, lexer.source), b)
  }

  def program: L[AmbBundle] = {
    optSemis
    positioned {
      AmbBundle(cmds)
    }
  }

  def cmds: LL[Cmd] = {
    val c = cmd
    if (la.kind == SEMI) {
      semis
      if (lexer.canStartStatement(la.kind)) {
        val cs = cmds
        c::cs
      }
    }
    c::Nil
  }

  def cmd = {
    la.kind match {
      case FUN => funDef
      case VAL => valDef
      case VAR => varDef
      case IMPORT => importDef
      case TRAIT => traitDef
      case _ => exp
    }
  }

  def importDef = {
    positioned {
      eat(IMPORT)

      val e = select

      def mkImport(e: Tree): LL[P.Import] = e match {
        case t @ TupleExp(es) =>
          es flatMap mkImport
        case t @ Union(es) =>
          es flatMap mkImport
        case t @ AmbSelect(e1, TupleExp(es)) =>
          es flatMap {
            case e2 => mkImport(AmbSelect(e1, e2))
          }
        case t @ AmbSelect(e1, Union(es)) =>
          es flatMap {
            case e2 => mkImport(AmbSelect(e1, e2))
          }
        case t @ Nothing() =>
          Located(pos, ImportNone(Located(pos, AmbFrame())))::Nil
        case t @ AmbName(x: SimpleOrAtomicName) =>
          Located(pos, ImportIncluding(Located(pos, AmbFrame()), fo(x)))::Nil

        case t @ Select(e1, x: SimpleOrAtomicName) =>
          Located(pos, ImportIncluding(f(e1), fo(x)))::Nil
        case t @ AmbSelect(e1, AmbName(x: SimpleOrAtomicName)) =>
          Located(pos, ImportIncluding(f(e1), fo(x)))::Nil
        case t @ AmbSelect(e1, Wildcard()) =>
          Located(pos, ImportAll(f(e1)))::Nil
        case t @ AmbSelect(e1, Nothing()) =>
          Located(pos, ImportNone(f(e1)))::Nil
        case t @ AmbSelect(e1, Arrow(e2, AmbName(y: SimpleOrAtomicName))) =>
          mkImport(AmbSelect(e1, e2)) collect {
            case Located(pos, ImportIncluding(e1, x)) => Located(pos, ImportRenaming(e1, x, fo(y)))
          }
        case t @ AmbSelect(e1, Arrow(e2, Nothing())) =>
          mkImport(AmbSelect(e1, e2)) collect {
            case Located(pos, ImportIncluding(e1, x)) => Located(pos, ImportExcluding(e1, x))
          }
        case _ => Nil
      }

      AmbImportDef(mkImport(e)::Nil)
    }
  }

  def valDef = {
    positioned {
      eat(VAL)
      FormulaDef(FlagVal(), exp)
    }
  }

  def varDef = {
    positioned {
      eat(VAR)
      FormulaDef(FlagVar(), exp)
    }
  }

  def mixfixName(args: List[MixfixElement]): SimpleName = {
    val parts = args collect {
      case Left(x: Op) => x
      case Left(x: Id) => x
      case Right(x) => PlaceholderIn() pos x
    }

    parts match {
      case (x: SimpleName) :: Nil => x
      case parts => MixfixName(parts) pos parts
    }
  }

  type MixfixElement = Either[MixfixPartName, Param]

  def toParams(args: List[MixfixElement]): List[Param] = {
    args collect { case Right(a) => a }
  }

  def toParam(ret: MixfixElement): Param = ret match {
    case Right(p) => p
    case Left(x) => ???
  }

  def funDef = {
    positioned {
      eat(FUN)
      val elements = mixfixElements
      val g = optGuard
      la.kind match {
        case EQ =>
          eat(EQ)
          val ret = exp
          AmbMixfixDef(FlagFun(), mixfixName(elements), g, toParams(elements), Param(false, Output(), ret))
        case ARROW =>
          eat(ARROW)
          val ret = exp
          AmbMixfixDef(FlagFun(), mixfixName(elements), g, toParams(elements), Param(false, Output(), ret))
        case BACKARROW =>
          eat(BACKARROW)
          mixfixReturn
          val g2 = g match {
            case None => optGuard
            case Some(g) => Some(g)
          }
          AmbMixfixDef(FlagFun(), mixfixName(elements), g2, toParams(elements), toParam(mixfixReturn))
      }
    }
  }

  def traitDef = {
    positioned {
      eat(TRAIT)
      if (la.kind == LC) {
        eat(LC)
        if (la.kind == SEMI) {
          pushback(LC)
          block
          return
        }
        else {
          pushback(LC)
        }
      }
      mixfixElements
      g = optGuard
      la.kind match {
        case WITH =>
          withs
          if (la.kind == LC)
            block
        case EQ =>
          eat(EQ)
          exp
        case ARROW =>
          eat(ARROW)
          exp
        case BACKARROW =>
          eat(BACKARROW)
          mixfixReturn
          if (g == None)
            guard
      }
    }
  }

  def withs = {
    eat(WITH)
    select
    if (la.kind == WITH)
      withs
  }

  def mixfixElements = {
    la.kind match {
      case ID | OP =>
        mixfixElementName
        mixfixElementsWithoutName
      case _ =>
        mixfixElementNoName
        mixfixElements
    }
  }

  def mixfixElementsWithoutName = {
    la.kind match {
      case ID(_) | OP(_) =>
        mixfixElementName
        mixfixElementsWithoutName
      case _ =>
        mixfixElementNoName
        mixfixElementsWithoutName
    }
  }

  def mixfixElementName = {
    la.kind match {
      case ID =>
        eat(ID)
      case OP =>
        eat(OP)
      case _ =>
        error(s"expected identifier or operator")
    }
  }

  def mixfixElementNoName = {
    la.kind match {
      case LC =>
        eat(LC)
        la.kind match {
          case LC =>
            eat(LC)
            la.kind match {
              case QUESTION =>
                eat(QUESTION)
                val e = tuple
                eat(RC)
                eat(RC)
                Param(true, Input(), CallByName(e))
              case _ =>
                val e = tuple
                eat(RC)
                eat(RC)
                Param(true, Input(), CallByName(e))
            }
          case QUESTION =>
            eat(QUESTION)
            val e = tuple
            eat(RC)
            Param(false, Input(), CallByName(e))
            by name input
          case _ =>
            val e = tuple
            eat(RC)
            Param(false, Input(), CallByName(e))
        }
      case LP =>
        eat(LP)
        la.kind match {
          case LP =>
            eat(LP)
            la.kind match {
              case QUESTION =>
                eat(QUESTION)
                val e = tuple
                eat(RP)
                eat(RP)
                Param(true, Input(), e)
              case BANG =>
                eat(BANG)
                val e = tuple
                eat(RP)
                eat(RP)
                Param(true, Output(), e)
              case _ =>
                val e = tuple
                eat(RP)
                eat(RP)
                Param(true, Input(), e)
            }
          case QUESTION =>
            eat(QUESTION)
            val e = tuple
            eat(RP)
            Param(false, Input(), e)
          case BANG =>
            eat(BANG)
            val e = tuple
            eat(RP)
            Param(false, Output(), e)
          case _ =>
            val e = tuple
            eat(RP)
            Param(false, Input(), e)
        }
      case _ =>
        error(s"expected ( or {")
    }
  }

  def mixfixReturn = {
    la.kind match {
      case LC =>
        eat(LC)
        la.kind match {
          case QUESTION =>
            eat(QUESTION)
            val e = tuple
            eat(RC)
            Param(false, Input(), CallByName(e))
          case _ =>
            val e = tuple
            eat(RC)
            Param(false, Input(), CallByName(e))
        }
      case LP =>
        eat(LP)
        la.kind match {
          case QUESTION =>
            eat(QUESTION)
            val e = tuple
            eat(RP)
            Param(false, Input(), e)
          case _ =>
            val e = tuple
            eat(RP)
            Param(false, Input(), e)
        }
      case _ =>
        val e = exp0noArrow
        Param(false, Input(), e)
    }
  }

  def guard = {
    optSemis
    eat(FOR)
    exp
  }

  def kase = {
    exp0noArrow
    optGuard
    eat(ARROW)
    exp
  }

  def kaseLayout = {
    eat(LC)
    optSemis
    kasesInBlock
    eat(RC)
  }

  def kasesInBlock = {
    kase
    if (la.kind == SEMI) {
      semis
      if (la.kind != RC) {
        kasesInBlock
      }
    }
  }

  def cmdOrKaseBlock = {
    eat(LC)
    optSemis
    la.kind match {
      case FUN => cmdsInBlock
      case VAL => cmdsInBlock
      case VAR => cmdsInBlock
      case IMPORT => cmdsInBlock
      case TRAIT => cmdsInBlock
      case _ =>
        exp
        exp match {
          case Arrow(arg, ret) =>
            kasesInBlock
          case _ =>
            cmdsInBlock
        }
    }
    eat(RC)
  }

  def optSemis = {
    if (la.kind == SEMI) {
      semis
    }
  }

  def block = {
    eat(LC)
    optSemis
    if (la.kind != RC) {
      cmdsInBlock
    }
    eat(RC)
  }

  def cmdsInBlock = {
    cmd
    if (la.kind == SEMI) {
      semis
      if (la.kind != RC) {
        cmdsInBlock
      }
    }
  }

  def exp = {
    if (la.kind == NATIVE)
      eat(NATIVE)
    else {
      exp0
      la.kind match {
        case ASSIGN =>
          eat(ASSIGN)
          exp
        case EQ =>
          eat(EQ)
          exp
        case BACKARROW =>
          eat(BACKARROW)
          exp
        case WITH =>
          eat(WITH)
          exp
      }
    }
  }

  def exp1 = {
    la.kind match {
      case FUN =>
        eat(FUN)
        tupleExp
        optGuard
        eat(ARROW)
        exp
      case FOR =>
        eat(FOR)
        select
        exp
      case TRAIT =>
        eat(TRAIT)
        block
    }
  }

  def exp0 = {
    unparsedExp
    la.kind match {
      case ARROW =>
        eat(ARROW)
        exp
      case AT =>
        eat(AT)
        exp
      case COLON =>
        eat(COLON)
        exp
    }
  }

  def exp0noArrow = {
    unparsedExp
    la.kind match {
      case AT =>
        eat(AT)
        exp
      case COLON =>
        eat(COLON)
        exp
    }
  }

  def unparsedExp = {
    la.kind match {
      case FOR | FUN | TRAIT =>
        exp1
      case _ =>
        select
        la.kind match {
          case LC | LP | ID | TICK | OP | LIT =>
            unparsedExp
          case _ =>
            return e
        }
    }
  }

  def select = {
    primary
    if (ls.kind == DOT)
      selectors
  }

  def selectors = {
    eat(DOT)
    la.kind match {
      case ID =>
        eat(ID)
      case TICK =>
        mixfixName
      case LP =>
        tupleExp
      case LB =>
        listExp
      case INT | FLOAT | CHAR | STRING | UNDERSCORE =>
        literal
    }
    if (ls.kind == DOT)
      selectors
  }

  def primary = {
    la.kind match {
      case ID =>
        eat(ID)
      case OP =>
        op
      case TICK =>
        mixfixName
      case LP =>
        tupleExp
      case LB =>
        listExp
      case LC =>
        cmdOrKaseBlock
      case INT | FLOAT | CHAR | STRING | UNDERSCORE =>
        literal
    }
  }

  def tuple = {
    exp
    if (la.kind == COMMA) {
      eat(COMMA)
      tuple
    }
  }

  def listExp = {
    eat(LB)
    if (la.kind == RB)
      eat(RB)
    else {
      tuple
      eat(RB)
    }
  }

  def tupleExp = {
    eat(LP)
    if (la.kind == RP)
      eat(RP)
    else {
      tuple
      eat(RP)
    }
  }

  def literal = {
    la.kind match {
      case INT => eat(INT)
      case FLOAT => eat(FLOAT)
      case STRING => eat(STRING)
      case CHAR => eat(CHAR)
      case UNDERSCORE => eat(UNDERSCORE)
    }
  }

  def mixfixName = {
    eat(TICK)
    while (la.kind != TICK) {
      la.kind match {
        case UNDERSCORE => eat(UNDERSCORE)
        case OP => eat(OP)
        case ID => eat(ID)
      }
    }
    eat(TICK)
  }

  def op = {
    la.kind match {
      case OPERATOR => eat(OPERATOR)
      case LBRACKET => eat(LBRACKET)
      case RBRACKET => eat(RBRACKET)
      case QUESTION => eat(QUESTION)
      case BANG => eat(BANG)
    }
  }

  def semis = {
    eat(SEMI)
    while (la.kind == SEMI)
      eat(SEMI)
  }
}
*/
