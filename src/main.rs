use std::env;
use std::process;

fn error(s: String) {
  eprintln!("{}", s);
  process::exit(1);
}

//  //////////////////////////////////////////////////////////////////
//  
// pointer
//  
//  //////////////////////////////////////////////////////////////////
struct Pointer {
  code: Vec<char>,
  index: usize,
}
impl Pointer {
  const ACCEPT_TOKENS: [&'static str; 13] = [
    "+",
    "-",
    "*",
    "/",
    "(",
    "(",
    "=",
    "==",
    ">",
    ">=",
    "<",
    "<=",
    ";",
  ];
  fn code(&mut self) -> char {
    let tmp_code = self.code[self.index];
    self.index += 1;
    tmp_code
  }
  fn token_cmp(&mut self, token: &str) -> bool {
    let mut is_accept = true;
    let token_chars: Vec<char> = token.chars().collect();
    for i in 0..token_chars.len() {
      if self.code.len() != self.index + i && self.code[self.index + i] != token_chars[i] {
        is_accept = false;
      }
    }
    is_accept
  }
  fn check_token(&mut self) -> (bool, &str) {
    let mut is_accept = false;
    let mut tok = "";
    for token in Self::ACCEPT_TOKENS {
      if self.token_cmp(token) {
        is_accept = true;
        tok = token;
      }
    }
    (is_accept, tok)
  }
  fn c(&mut self) -> char {
    self.code[self.index]
  }
  /// # is out of range
  /// code[index]でエラーが起きないかをbool型で返してくれる
  fn is_out(&mut self) -> bool {
    self.index == self.code.len()
  }
}

struct LVar{
  name: String,
  offset: i64,
}

//  //////////////////////////////////////////////////////////////////
//  
//  token
//  
//  //////////////////////////////////////////////////////////////////
/// トークンの種類
enum TokenKind {
  RESERVED(String),
  IDENT(String),
  NUM(i64),
  EOF,
}
/// トークン
struct Token {
  kind: TokenKind,
}
struct TokenArray {
  tokens: Vec<Token>,
  index: usize,
}
impl TokenArray {
  fn consume(&mut self, op: &str) -> bool{
    match &self.tokens[self.index].kind {
      TokenKind::RESERVED (s) => {
        if s == op {
          self.index += 1;
          return true;
        }
      },
      _ => ()
    }
    false
  }
  fn consume_ident(&mut self) -> (bool, String) {
    match &self.tokens[self.index].kind {
      TokenKind::IDENT (s) => {
        self.index += 1;
        return (true, s.to_string())
      },
      _ => (),
    }
    (false, String::from("None"))
  }
  fn expect(&mut self, op: &str) {
    match &self.tokens[self.index].kind {
      TokenKind::RESERVED (s) => {
        if s == op {
          self.index += 1;
          return
        }
      },
      _ => ()
    }
    error(format!("{}ではありません", op));
  }
  fn expect_number(&mut self) -> i64 {
    match &self.tokens[self.index].kind {
      TokenKind::NUM (n) => {
        self.index += 1;
        *n
      },
      _ => {
        error(String::from("数ではありません"));
        0
      }
    }
  }
  fn find_lvar(&mut self, lvars: &mut Vec<LVar>) -> (bool, usize) {
    match &self.tokens[self.index].kind {
      TokenKind::IDENT(s) => {
        for i in 0..lvars.len() {
          if s.to_string() == lvars[i].name {
            self.index += 1;
            return (true, i);
          }
        }
      },
      _ => ()
    }
    self.index += 1;
    (false, 0)
  }
  fn at_eof(&mut self) -> bool {
    match &self.tokens[self.index].kind {
      TokenKind::EOF => {
        return true
      },
      _ => ()
    }
    false
  }
}

//  //////////////////////////////////////////////////////////////////
//  
//  node
//  
//  //////////////////////////////////////////////////////////////////
enum NodeKind {
  ADD, // +
  SUB, // -
  MUL, // *
  DIV, // /
  EQ, // ==
  NE, // !=
  LT, // <
  LE, // <=
  ASSIGN, // =
  LVAR(LVar), // local variables
  NUM(i64), // number
}
struct Node {
  kind: NodeKind,
  lhs: Option<usize>,
  rhs: Option<usize>,
}
/// # トークン解析
/// program    = stmt*
/// stmt       = expr ";"
/// expr       = assign
/// assign     = equality ("=" assign)?
/// equality   = relational ("==" relational | "!=" relational)*
/// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
/// add        = mul ("+" mul | "-" mul)*
/// mul        = unary ("*" unary | "/" unary)*
/// unary      = ("+" | "-")? primary
/// primary    = num | ident | "(" expr ")"
struct NodeArray {
  nodes: Vec<Node>
}
impl NodeArray {
  fn index(&mut self) -> usize {
    self.nodes.len() - 1
  }
  fn new_node(&mut self, kind: NodeKind, lhs: usize, rhs: usize) -> usize {
    self.nodes.push(Node {
      kind: kind,
      lhs: Some(lhs),
      rhs: Some(rhs),
    });
    return self.index();
  }
  fn new_node_lvar(&mut self, name: String, offset: i64) -> usize {
    self.nodes.push(Node {
      kind: NodeKind::LVAR(LVar {
        name: name,
        offset: offset,
      }),
      lhs: None,
      rhs: None,
    });
    return self.index();
  }
  fn new_node_num(&mut self, n: i64) -> usize{
    self.nodes.push(Node {
      kind: NodeKind::NUM(n),
      lhs: None,
      rhs: None,
    });
    self.index()
  } 
  fn gen_lval(&mut self, index: usize) {
    match &self.nodes[index].kind {
      NodeKind::LVAR(lvar) => {
        println!("  mov rax, rbp");
        println!("  sub rax, {}", lvar.offset);
        println!("  push rax");
      },
      _ => {
        error(String::from("代入の右辺値が変数ではありません。"));
      }
    }
  }
  fn gen(&mut self, index: usize) {
    match &self.nodes[index].kind {
      NodeKind::NUM (n) => {
        println!("  push {}", n);
        return;
      },
      NodeKind::LVAR (_) => {
        self.gen_lval(index);
        println!("  pop rax");
        println!("  mov rax, [rax]");
        println!("  push rax");
        return;
      },
      NodeKind::ASSIGN => {
        self.gen_lval(self.nodes[index].lhs.unwrap());
        self.gen(self.nodes[index].rhs.unwrap());

        println!("  pop rdi");
        println!("  pop rax");
        println!("  mov [rax], rdi");
        println!("  push rdi");
        return;
      },
      _ => ()
    }

    self.gen(self.nodes[index].lhs.unwrap());
    self.gen(self.nodes[index].rhs.unwrap());

    println!("  pop rdi");
    println!("  pop rax");

    match self.nodes[index].kind {
      NodeKind::ADD => println!("  add rax, rdi"),
      NodeKind::SUB => println!("  sub rax, rdi"),
      NodeKind::MUL => println!("  imul rax, rdi"),
      NodeKind::DIV => { println!("  cqo"); println!("  idiv rdi") },
      NodeKind::EQ => {
        println!("  cmp rax, rdi");
        println!("  sete al");
        println!("  movzb rax, al");
      },
      NodeKind::NE => {
        println!("  cmp rax, rdi");
        println!("  setne al");
        println!("  movzb rax, al");
      },
      NodeKind::LT => {
        println!("  cmp rax, rdi");
        println!("  setl al");
        println!("  movzb rax, al");
      },
      NodeKind::LE => {
        println!("  cmp rax, rdi");
        println!("  setle al");
        println!("  movzb rax, al");
      },
      _ => ()
    }

    println!("  push rax");
  }
}
/*
NodeArray トークン解析
*/
impl NodeArray {
  fn stmt(&mut self, toks: &mut TokenArray, lvars: &mut Vec<LVar>) -> usize {
    let index = self.expr(toks, lvars);
    toks.expect(";");
    index
  }
  fn expr(&mut self, toks: &mut TokenArray, lvars: &mut Vec<LVar>) -> usize {
    self.assign(toks, lvars)
  }
  fn assign(&mut self, toks: &mut TokenArray, lvars: &mut Vec<LVar>) -> usize {
    let mut index = self.equality(toks, lvars);
    if toks.consume("=") {
      let rhs = self.assign(toks, lvars);
      index = self.new_node(NodeKind::ASSIGN, index, rhs);
    }
    index
  }
  fn equality(&mut self, toks: &mut TokenArray, lvars: &mut Vec<LVar>) -> usize{
    let mut index = self.relational(toks, lvars);

    loop {
      if toks.consume("==") {
        let rhs = self.relational(toks, lvars);
        index = self.new_node(NodeKind::EQ, index, rhs);
      } else if toks.consume("!=") {
        let rhs = self.relational(toks, lvars);
        index = self.new_node(NodeKind::NE, index, rhs);
      } else {
        return index
      }
    }
  }
  fn relational(&mut self, toks: &mut TokenArray, lvars: &mut Vec<LVar>) -> usize {
    let mut index = self.add(toks, lvars);

    loop {
      if toks.consume("<") {
        let rhs = self.add(toks, lvars);
        index = self.new_node(NodeKind::LT, index, rhs);
      } else if toks.consume("<=") {
        let rhs = self.add(toks, lvars);
        index = self.new_node(NodeKind::LE, index, rhs);
      } else if toks.consume(">") {
        let rhs = self.add(toks, lvars);
        index = self.new_node(NodeKind::LT, rhs, index);
      } else if toks.consume(">=") {
        let rhs = self.add(toks, lvars);
        index = self.new_node(NodeKind::LE, rhs, index);
      } else {
        return index;
      }
    }
  }
  fn add(&mut self, toks: &mut TokenArray, lvars: &mut Vec<LVar>) -> usize {
    let mut index = self.mul(toks, lvars);

    loop {
      if toks.consume("+") {
        let rhs = self.mul(toks, lvars);
        index = self.new_node(NodeKind::ADD, index, rhs);
      } else if toks.consume("-") {
        let rhs = self.mul(toks, lvars);
        index = self.new_node(NodeKind::SUB, index, rhs);
      } else {
        return index;
      }
    }
  }
  fn mul(&mut self, toks: &mut TokenArray, lvars: &mut Vec<LVar>) -> usize {
    let mut index = self.unary(toks, lvars);

    loop {
      if toks.consume("*") {
        let rhs = self.unary(toks, lvars);
        index = self.new_node(NodeKind::MUL, index, rhs);
      } else if toks.consume("/") {
        let rhs = self.unary(toks, lvars);
        index = self.new_node(NodeKind::DIV, index, rhs);
      } else {
        return index;
      }
    }
  }
  fn unary(&mut self, toks: &mut TokenArray, lvars: &mut Vec<LVar>) -> usize {
    if toks.consume("+") {
      return self.primary(toks, lvars);
    }
    if toks.consume("-") {
      let lhs = self.new_node_num(0);
      let rhs = self.primary(toks, lvars);
      return self.new_node(NodeKind::SUB, lhs, rhs);
    }
    self.primary(toks, lvars)
  }
  fn primary(&mut self,  toks: &mut TokenArray, lvars: &mut Vec<LVar>) -> usize{
    let (is_ident, s) = toks.consume_ident();

    if is_ident {
      toks.index -= 1;
      let (is_exist, index) = toks.find_lvar(lvars);
      if is_exist {
        // すでに存在する
        self.new_node_lvar(s, lvars[index].offset);
      } else {
        // 存在しない
        let tmp = s.clone();
        if lvars.len() == 0 {
          self.new_node_lvar(s, 8);
          lvars.push(LVar {
            name: tmp,
            offset: 8,
          });
        } else {
          self.new_node_lvar(s, lvars[lvars.len() - 1].offset + 8);
          lvars.push(LVar {
            name: tmp,
            offset: lvars[lvars.len() - 1].offset + 8,
          });
        }
      }
      return self.index();
    }
    if toks.consume("(") {
      self.expr(toks, lvars);
      toks.expect(")");
      return self.index();
    }
    self.new_node_num(toks.expect_number());
    self.index()
  }
}

/// 数字まで区切って返してくれる
fn strtoi(p: &mut Pointer) -> i64 {
  let mut r = String::from("");
  while !p.is_out() && p.code[p.index].is_digit(10) {
    r.push(p.code());
  }
  r.to_string().parse::<i64>().unwrap()
}

fn tokenize(p: &mut Pointer) -> TokenArray {
  let mut toks = TokenArray {
    tokens: Vec::new(),
    index: 0,
  };
  while !p.is_out() {
    if p.c() == ' ' {
      p.index += 1;
      continue;
    }

    let (is_accept, tok) = p.check_token();
    if is_accept {
      toks.tokens.push(Token {
        kind: TokenKind::RESERVED(String::from(tok))
      });
      p.index += tok.len();
      continue;
    }

    if p.c() >= 'a' && p.c() <= 'z' {
      let mut r = String::from("");
      loop {
        if p.c() >= 'a' && p.c() <= 'z' {
          r.push(p.code());
        } else {
          break;
        }
      }
      toks.tokens.push(Token {
        kind: TokenKind::IDENT(r)
      });
      continue;
    }
    
    if p.c().is_digit(10) {
      toks.tokens.push(Token {
        kind: TokenKind::NUM(strtoi(p))
      });
      continue;
    }

    error(String::from("トークナイズできません"));
  }

  toks.tokens.push(Token {
    kind: TokenKind::EOF
  });
  toks
}

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() != 2 {
    eprintln!("引数の個数が正しくありません");
    process::exit(1);
  }

  //////////////////////////////////////////////////////////////////
  //
  // compile
  //
  //////////////////////////////////////////////////////////////////
  let mut p = Pointer {
    index: 0,
    code: args[1].chars().collect()
  };
  let mut tokens = tokenize(&mut p);
  let mut code: Vec<NodeArray> = Vec::new();
  let mut lvars: Vec<LVar> = Vec::new();
  while !tokens.at_eof() {
    code.push(NodeArray {
      nodes: Vec::new(),
    });
    let code_len = code.len() - 1;
    code[code_len].stmt(&mut tokens, &mut lvars);
  }
  println!(".intel_syntax noprefix");
  println!(".globl main");
  println!("main:");
  println!("  push rbp");
  println!("  mov rbp, rsp");
  println!("  sub rsp, 208");

  for mut line in code {
    let len = line.nodes.len() - 1;
    line.gen(len);

    println!("  pop rax");
  }

  println!("  mov rsp, rbp");
  println!("  pop rbp");
  println!("  ret");
}