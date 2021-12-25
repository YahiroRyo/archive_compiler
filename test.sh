#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  cargo run "$input" > tmp.s
  cc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 0
assert 42 42
assert 64 42+22
assert 10 "30 + -20"
assert 0 "1 > 2"
assert 1 "3 > 2"
assert 1 "1 < 2"
assert 0 "3 < 2"
assert 0 "1 >= 2"
assert 1 "3 >= 2"
assert 1 "1 <= 2"
assert 0 "3 <= 2"
assert 0 "1 == 2"
assert 1 "3 == 3"

echo OK