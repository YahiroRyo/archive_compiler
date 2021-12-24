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
  const ACCEPT_TOKENS: [char; 6] = [
    '+',
    '-',
    '*',
    '/',
    '(',
    ')',
  ];
  fn code(&mut self) -> char {
    let tmp_code = self.code[self.index];
    self.index += 1;
    tmp_code
  }
  fn check_token(&mut self) -> bool{
    let mut is_accept = false;
    for token in Self::ACCEPT_TOKENS {
      if self.c() == token {
        is_accept = true;
      }
    }
    is_accept
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

//  //////////////////////////////////////////////////////////////////
//  
//  token
//  
//  //////////////////////////////////////////////////////////////////
/// トークンの種類
enum TokenKind {
  RESERVED(String),
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
  fn consume(&mut self, op: char) -> bool{
    match &self.tokens[self.index].kind {
      TokenKind::RESERVED (s) => {
        let chars: Vec<char> = s.chars().collect();
        if chars[0] == op {
          self.index += 1;
          return true;
        }
      },
      _ => ()
    }
    false
  }
  fn expect(&mut self, op: char) {
    match &self.tokens[self.index].kind {
      TokenKind::RESERVED (s) => {
        let chars: Vec<char> = s.chars().collect();
        if chars[0] == op {
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
  
  #[allow(dead_code)]
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
  ADD,
  SUB,
  MUL,
  DIV,
  NUM(i64),
}
struct Node {
  kind: NodeKind,
  lhs: Option<usize>,
  rhs: Option<usize>,
}
/// # トークン解析
/// expr    = mul ("+" mul | "-" mul)*  
/// mul     = unary ("*" unary | "/" unary)*  
/// unary   = ("+" | "-")? primary  
/// primary = num | "(" expr ")"  
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
  fn new_node_num(&mut self, n: i64) -> usize{
    self.nodes.push(Node {
      kind: NodeKind::NUM(n),
      lhs: None,
      rhs: None,
    });
    self.index()
  }  
  fn gen(&mut self, index: usize) {
    match &self.nodes[index].kind {
      NodeKind::NUM (n) => {
        println!("  push {}", n);
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
      _ => ()
    }

    println!("  push rax");
  }
}
impl NodeArray {
  fn expr(&mut self, toks: &mut TokenArray) -> usize {
    let mut index =  self.mul(toks);

    loop {
      if toks.consume('+') {
        let rhs = self.mul(toks);
        index = self.new_node(NodeKind::ADD, index, rhs);
      } else if toks.consume('-') {
        let rhs = self.mul(toks);
        index = self.new_node(NodeKind::SUB, index, rhs);
      } else {
        return index;
      }
    }
  }
  fn mul(&mut self, toks: &mut TokenArray) -> usize {
    let mut index = self.unary(toks);

    loop {
      if toks.consume('*') {
        let rhs = self.unary(toks);
        index = self.new_node(NodeKind::MUL, index, rhs);
      } else if toks.consume('/') {
        let rhs = self.unary(toks);
        index = self.new_node(NodeKind::DIV, index, rhs);
      } else {
        return index;
      }
    }
  }
  fn unary(&mut self, toks: &mut TokenArray) -> usize {
    if toks.consume('+') {
      return self.primary(toks);
    }
    if toks.consume('-') {
      let lhs = self.new_node_num(0);
      let rhs = self.primary(toks);
      return self.new_node(NodeKind::SUB, lhs, rhs);
    }
    self.primary(toks)
  }
  fn primary(&mut self,  toks: &mut TokenArray) -> usize{
    if toks.consume('(') {
      self.expr(toks);
      toks.expect(')');
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

    if p.check_token() {
      toks.tokens.push(Token {
        kind: TokenKind::RESERVED(String::from(p.code()))
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
  let mut nodes = NodeArray {
    nodes: vec![],
  };
  nodes.expr(&mut tokens);
  println!(".intel_syntax noprefix");
  println!(".globl main");
  println!("main:");

  let nodes_len = nodes.index();
  nodes.gen(nodes_len);

  println!("  pop rax");
  println!("  ret");
}