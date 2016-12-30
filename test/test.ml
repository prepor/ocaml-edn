open OUnit2

let edn_parse msg s expected =
  assert_equal ~msg ~printer: Edn.to_string
               expected (Edn.from_string s)

let edn_write msg e s =
  assert_equal ~msg: ((Edn.to_string e) ^ " " ^ s)
               s (Edn.to_string e)

let edn_roundtrip msg s e =
  (edn_parse msg s e);
  (edn_write msg e s)

let basic_parsing1 _ =
  edn_roundtrip "nil" "nil" `Null;
  edn_roundtrip "true" "true" (`Bool true);
  edn_roundtrip "false" "false" (`Bool false);
  edn_roundtrip "string" "\"hello\\nworld\"" (`String "hello\nworld");
  edn_roundtrip "symbol" "foo" (`Symbol (None, "foo"));
  edn_roundtrip "symbol with namespace" "bar/foo:#" (`Symbol ((Some "bar"), "foo:#"));
  edn_roundtrip "keyword" ":foo" (`Keyword (None, "foo"));
  edn_roundtrip "keyword with namespace" ":bar/foo:#" (`Keyword ((Some "bar"), "foo:#"));
  edn_roundtrip "standalone symbols" "(+ - .)" (`List [`Symbol (None, "+");
                                                   `Symbol (None, "-");
                                                   `Symbol (None, ".");]);

  edn_roundtrip "int" "1" (`Int 1);
  edn_roundtrip "int2" "123" (`Int 123);
  edn_parse "positive int" "+1" (`Int 1);
  edn_parse "positive int2" "+123" (`Int 123);
  edn_roundtrip "negative int" "-1" (`Int (-1));
  edn_roundtrip "negative int2" "-123" (`Int (-123));

  edn_roundtrip "big int" "123N" (`BigInt "123");

  edn_roundtrip "float" "1.2" (`Float 1.2);
  edn_roundtrip "float2" "123.2" (`Float 123.2);
  edn_parse "positive float" "+1.2" (`Float 1.2);
  edn_parse "positive float2" "+123.2" (`Float 123.2);
  edn_roundtrip "negative float" "-1.2" (`Float (-1.2));
  edn_roundtrip "negative float2" "-123.2" (`Float (-123.2));
  edn_parse "exp float" "123.E33" (`Float 123.E33);

  edn_roundtrip "decimal" "123.123M" (`Decimal "123.123");

  edn_roundtrip "list" "(1 \"2\" :a)" (`List [`Int 1; `String "2"; `Keyword (None, "a")]);

  edn_roundtrip "vector" "[1 \"2\" :a]" (`Vector [`Int 1; `String "2"; `Keyword (None, "a")]);

  edn_roundtrip "map" "{:a 1 \"foo\" :bar [1 2 3] four}"
    (`Assoc [(`Keyword (None, "a"), `Int 1);
             (`String "foo", `Keyword (None, "bar"));
             (`Vector [`Int 1; `Int 2; `Int 3], `Symbol (None, "four"))]);

  edn_roundtrip "set" "#{1 \"2\" :a}" (`Set [`Int 1; `String "2"; `Keyword (None, "a")]);

  edn_roundtrip "tag" "#myapp/Person \"Andrew\""
    (`Tag ((Some "myapp"), "Person", `String "Andrew"));

  edn_parse "comments" "[1\n;2\n3]" (`Vector [`Int 1; `Int 3]);

  edn_parse "discard" "[1 2 #_foo 42]" (`Vector [`Int 1; `Int 2; `Int 42]);

  edn_parse "ignore commas" "{:a 1, \"foo\" :bar, [1 2 3] four}"
    (`Assoc [(`Keyword (None, "a"), `Int 1);
             (`String "foo", `Keyword (None, "bar"));
             (`Vector [`Int 1; `Int 2; `Int 3], `Symbol (None, "four"))]);


  edn_roundtrip "nested"
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
