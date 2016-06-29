open OUnit2

let edn_parse msg s expected =
  assert_equal ~msg ~printer: Edn.to_string
    expected (Edn.from_string s)

let basic_parsing1 _ =
  edn_parse "nil" "nil" `Null;

  edn_parse "true" "true" (`Bool true);
  edn_parse "false" "false" (`Bool false);

  edn_parse "string" "\"hello\nworld\"" (`String "hello\nworld");

  edn_parse "symbol" "foo" (`Symbol (None, "foo"));
  edn_parse "symbol with namespace" "bar/foo:#" (`Symbol ((Some "bar"), "foo:#"));

  edn_parse "keyword" ":foo" (`Keyword (None, "foo"));
  edn_parse "keyword with namespace" ":bar/foo:#" (`Keyword ((Some "bar"), "foo:#"));

  edn_parse "standalone symbols" "(+ - .)" (`List [`Symbol (None, "+");
                                                   `Symbol (None, "-");
                                                   `Symbol (None, ".");]);

  edn_parse "int" "1" (`Int 1);
  edn_parse "int2" "123" (`Int 123);
  edn_parse "positive int" "+1" (`Int 1);
  edn_parse "positive int2" "+123" (`Int 123);
  edn_parse "negative int" "-1" (`Int (-1));
  edn_parse "negative int2" "-123" (`Int (-123));

  edn_parse "big int" "123N" (`BigInt "123");

  edn_parse "float" "1.2" (`Float 1.2);
  edn_parse "float2" "123.2" (`Float 123.2);
  edn_parse "positive float" "+1.2" (`Float 1.2);
  edn_parse "positive float2" "+123.2" (`Float 123.2);
  edn_parse "negative float" "-1.2" (`Float (-1.2));
  edn_parse "negative float2" "-123.2" (`Float (-123.2));
  edn_parse "exp float" "123.E33" (`Float 123.E33);

  edn_parse "decimal" "123.123M" (`Decimal "123.123");

  edn_parse "list" "(1 \"2\" :a)" (`List [`Int 1; `String "2"; `Keyword (None, "a")]);

  edn_parse "vector" "[1 \"2\" :a]" (`Vector [`Int 1; `String "2"; `Keyword (None, "a")]);

  edn_parse "map" "{:a 1, \"foo\" :bar, [1 2 3] four}"
    (`Assoc [(`Keyword (None, "a"), `Int 1);
             (`String "foo", `Keyword (None, "bar"));
             (`Vector [`Int 1; `Int 2; `Int 3], `Symbol (None, "four"))]);

  edn_parse "set" "#{1 \"2\" :a}" (`Set [`Int 1; `String "2"; `Keyword (None, "a")]);

  edn_parse "tag" "#myapp/Person \"Andrew\""
    (`Tag ((Some "myapp"), "Person", `String "Andrew"));

  edn_parse "comments" "[1\n;2\n3]" (`Vector [`Int 1; `Int 3]);

  edn_parse "discard" "[1 2 #_foo 42]" (`Vector [`Int 1; `Int 2; `Int 42]);

  edn_parse "nested"
    "[1 {:foo (1 2)} #{\"hello\"}]"
    (`Vector [`Int 1;
              `Assoc [(`Keyword (None, "foo"), `List [`Int 1; `Int 2])];
              `Set [`String "hello"]]);

  ()

let suite =
  "parsing" >:::
  ["basic_parsing1">:: basic_parsing1]

let () =
  run_test_tt_main suite
