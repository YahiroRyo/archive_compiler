#!/bin/bash
assert() {
  echo "========================================="
  expected="$1"
  input="$2"
  is_write="$3"

  cargo run "$input" > tmp.s
  cc -no-pie -o tmp tmp.s
  result=$(./tmp)
  actual="$?"

  if [ "$is_write" == "true" ]; then
    if [ "$result" = "$expected" ]; then
      echo "$input => $result"
    else
      echo "$input => $expected expected, but got $result"
      exit 1
    fi
  elif [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 "0;"
assert 42 "42;"
assert 64 "42+22;"
assert 10 "30 + -20;"
assert 0 "1 > 2;"
assert 1 "3 > 2;"
assert 1 "1 < 2;"
assert 0 "3 < 2;"
assert 0 "1 >= 2;"
assert 1 "2 >= 2;"
assert 1 "3 >= 2;"
assert 1 "1 <= 2;"
assert 0 "3 <= 2;"
assert 1 "2 <= 2;"
assert 0 "1 == 2;"
assert 1 "3 == 3;"
assert 17 "a = 6 + 11; a;"
assert 17 "foo = 6 + 11; foo;"
assert 17 "bar = 6; foo = bar + 11; foo;"
assert 17 "bar = 1; foo = bar + 16; return foo;"
assert 2 "if (2 > 1) 2;"
assert 3 "if (2 < 1) 2; else 3;"
assert 5 "foo = 0; while (foo < 5) foo = foo + 1;"
assert 6 "for (i = 0; i < 6; i = i + 1) i;"
assert 10 "foo = 0; for (i = 0; i < 10; i = i + 1) foo = foo + 1; foo;"
assert 10 "a = 0; for (i = 0; i < 10; i = i + 1) { a = a + 1; } a;";
assert 81 "
foo = 0;
for (i = 0; i < 9; i = i + 1) {
  for (j = 0; j < 9; j = j + 1) {
    foo = foo + 1;
  }
}
foo;
"
assert 45 "
foo = 0;
bar = 0;
for (i = 0; i < 9; i = i + 1) {
  foo = foo + 1;
  bar = bar + foo;
}
bar;
"
assert 12 "foo = 12; print(foo);" true
assert 100100100100100100100100100 "
for (i = 0; i < 9; i = i + 1) {
  print(100);
}
" true
echo OK