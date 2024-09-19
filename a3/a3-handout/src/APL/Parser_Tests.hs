module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ],
      testGroup
        "Function application"
        [ parserTest "f x" $ Apply (Var "f") (Var "x"),
          parserTest "f x y" $ Apply (Apply (Var "f") (Var "x")) (Var "y"),
          parserTest "f (x y)" $ Apply (Var "f") (Apply (Var "x") (Var "y")),
          parserTest "f (x y) z" $ Apply (Apply (Var "f") (Apply (Var "x") (Var "y"))) (Var "z")
        ],
      testGroup
        "Power operator"
        [ parserTest "x**y" $ Pow (Var "x") (Var "y"),
          parserTest "x**y**z" $ Pow (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x**y*z" $ Mul (Pow (Var "x") (Var "y")) (Var "z"),
          parserTest "x*y**z" $ Mul (Var "x") (Pow (Var "y") (Var "z"))
        ],
      testGroup
        "Equality"
        [ parserTest "x==y" $ Eql (Var "x") (Var "y"),
          parserTest "x+y==y+x" $ Eql (Add (Var "x") (Var "y")) (Add (Var "y") (Var "x"))
        ],
      testGroup
        "Print"
        [ parserTest "print \"x\" y" $ Print "x" (Var "y"),
          parserTest "print \"foo\" bar" $ Print "foo" (Var "bar")
        ],
      testGroup
        "KvGet"
        [ parserTest "get x" $ KvGet (Var "x"),
          parserTest "get foo" $ KvGet (Var "foo"),
          parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y")
        ],
      testGroup
        "KvPut"
        [ parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          parserTest "put foo bar" $ KvPut (Var "foo") (Var "bar"),
          parserTest "put x y + z" $ Add (KvPut (Var "x") (Var "y")) (Var "z"),
          parserTest "x + put y z" $ Add (Var "x") (KvPut (Var "y") (Var "z"))
        ],
      testGroup
        "Lambda"
        [ parserTest "\\x -> x" $ Lambda "x" (Var "x"),
          parserTest "\\x -> x + y" $ Lambda "x" (Add (Var "x") (Var "y")),
          parserTest "\\x -> x + y + z" $ Lambda "x" (Add (Add (Var "x") (Var "y")) (Var "z")),
          parserTest "\\x -> x + (y + z)" $ Lambda "x" (Add (Var "x") (Add (Var "y") (Var "z"))),
          parserTest "\\x -> x**y*z" $ Lambda "x" (Mul (Pow (Var "x") (Var "y")) (Var "z"))
        ],
      testGroup
        "TryCatch"
        [ parserTest "try x catch y" $ TryCatch (Var "x") (Var "y"),
          parserTest "try x catch try y catch z" $ TryCatch (Var "x") (TryCatch (Var "y") (Var "z")),
          parserTest "try x catch y + z" $ TryCatch (Var "x") (Add (Var "y") (Var "z")),
          parserTest "try x + y catch z" $ TryCatch (Add (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "Let"
        [ parserTest "let x = y in z" $ Let "x" (Var "y") (Var "z"),
          parserTest "let x = y in let z = x in z" $ Let "x" (Var "y") (Let "z" (Var "x") (Var "z")),
          parserTest "let x = y in z + x" $ Let "x" (Var "y") (Add (Var "z") (Var "x")),
          parserTest "let x = y in x + z" $ Let "x" (Var "y") (Add (Var "x") (Var "z")),
          parserTest "let x = true in x + 5" $ Let "x" (CstBool True) (Add (Var "x") (CstInt 5))
        ]
    ]
