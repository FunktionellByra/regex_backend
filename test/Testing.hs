-- A stand-alone testing module for Regex parser. Call @Testing.main@ to execute on all
-- test cases.
-- Test suites generated with OpenAI's GPT 5.2 (checked manually by us afterwards).
-- TODO: offload much of this work to @QuickCheck@.

module Main where

import Parser      (parseReg,Regex(..))
import Regex       (match)
import Debug.Trace (trace)

import Test.QuickCheck
-- import Text.RE

{-
Idea:

1. Generate a random AST of our Tokens
  Positive Testing: 
    - From the AST, generate a String that definitely matches this AST
  Naive testing (will result mostly in negative testing):
    - From this AST, construct a Haskell-style regular expression and see if our algorithm and
      the Haskell-implementation result in the same 'match'-output
    - generate a random String and run both on it
-}

genAST :: Gen Regex
genAST = frequency [(24, sized helper), (1,return Epsilon)]
  where 
    helper :: Int -> Gen Regex
    helper 0 = genLiteral
    helper n = frequency [
              (4, genOr n)
            , (4, genConcat n)
            , (1, genLiteral)
            , (2, genPlus n)
            , (2, genKleene n)
            , (2, genOptional n)
          ]

    genLiteral :: Gen Regex
    genLiteral = Literal <$> elements ['a'..'z']

    genPlus :: Int -> Gen Regex
    genPlus k = let k' = k - 1 in do
      a <- helper k'
      return $ Plus a

    genKleene :: Int -> Gen Regex
    genKleene k = let k' = k - 1 in do
      a <- helper k'
      return $ Kleene a

    genOptional :: Int -> Gen Regex
    genOptional k = let k' = k - 1 in do
      a <- helper k'
      return $ Optional a

    genConcat :: Int -> Gen Regex
    genConcat k =  let k' = k `div` 2 in do
      a <- helper k'
      b <- helper k'
      return (Concat a b)

    genOr :: Int -> Gen Regex
    genOr k =  let k' = k `div` 2 in do
      a <- helper k'
      b <- helper k'
      return (Or a b)

createRegex :: Regex -> String
createRegex Epsilon      = ""
createRegex Dot          = "."
createRegex (Literal c)  = [c]
createRegex (Plus r)     = "(" createRegex r ++ ")+"
createRegex (Kleene r)   = "(" createRegex r ++ ")*"
createRegex (Optional r) = "(" createRegex r ++ ")?"
createRegex (Concat a b) = createRegex  ++ createRegex b
createRegex (Or a b)     = "(" createRegex a ++ "|" ++ createRegex b ")"

-- TODO: use QuickCheck to randomly populate constructors instead of using
-- minimal implementations.
createMatch :: Regex -> String
createMatch Epsilon      = ""
createMatch Dot          = "a"
createMatch (Literal c)  = [c]
createMatch (Plus r)     = createMatch r -- minimal r+
createMatch (Kleene r)   = ""            -- minimal r*
createMatch (Optional r) = ""            -- minimal r?
createMatch (Concat a b) = createRegex ++ createRegex b
createMatch (Or a _)     = createRegex a -- bias on `a`

prop_rgx :: Regex -> Bool
prop_rgx r = let 
  haskellRegex = createRegex r
  perfectMatch = createMatch r
  in
    undefined -- TODO: test that Haskell + our Matcher match 

main :: IO ()
main = do
  sample genAST
  -- mapM_ (\res -> putStrLn $ show res ++ "\n" ++ sep) tests
  -- where tests = [ testParser testSuiteParser       -- variety of regs
  --               , testParser matchingRegs          -- verify regs first
  --               , testMatching testSuiteMatching ] -- same regs as^ with match tests
  --       sep   = [ '=' | _ <- [1..80]] 

  --       matchingRegs = map (\(reg,_,_) -> reg) testSuiteMatching

testParser :: [String] -> Bool
testParser ps = process ps 1 True
    where
        process []     _ success = success
        process (s:ss) k success = case parseReg s of
            Just reg -> trace (prefix True  k ++ show reg) process ss (k+1) success
            Nothing  -> trace (prefix False k ++ s)        process ss (k+1) False

testMatching :: [(String, String, Bool)] -> Bool
testMatching matches = process matches 1 True
  where
    process [] _ success = success
    process ((pattern,input,expected):ss) k success =
      let res = match pattern input in
        trace (prefix (res == expected) k ++
          "Pattern:" ++ show pattern ++
          "\tinput: " ++ show input)
        $ process ss (k+1) (success && res == expected)

prefix :: Bool -> Int -> String
prefix b n = show n ++ ") " ++ if b then "[OK]: " else "[FAIL]: "

testSuiteParser :: [String]
testSuiteParser = testSuite1 ++ testSuite2 ++ complexRegexes

testSuiteMatching :: [(String, String, Bool)]
testSuiteMatching = [
    ("",                 "",           True)
  , ("",                 "a",          False)
  , ("a",                "a",           True)
  , ("a",                "b",          False)
  , (".",                "x",           True)
  , (".",                "",           False)
  , ("a*",               "",            True)
  , ("a*",               "aaa",          True)
  , ("a*",               "b",           False)
  , ("a+",               "a",            True)

  , ("a+",               "",           False)
  , ("ab",               "ab",           True)
  , ("ab",               "a",           False)
  , ("a.b",              "acb",          True)
  , ("a.b",              "ab",          False)
  , ("(ab)",             "ab",           True)
  , ("(ab)",             "a",           False)
  , ("(a)b",             "ab",           True)
  , ("a(b)",             "ab",           True)
  , ("(a)(b)",           "ab",           True)

  , ("(ab)*",            "",            True)
  , ("(ab)*",            "abab",         True)
  , ("(ab)*",            "aba",         False)
  , ("(ab)+",            "ab",           True)
  , ("(ab)+",            "",           False)
  , ("a(bc)",            "abc",          True)
  , ("a(bc)",            "ab",          False)
  , ("(a*)(b+)",         "aaabbb",       True)
  , ("(a*)(b+)",         "aaa",         False)
  , ("(a+)(b*)",         "a",            True)

  , ("(a+)(b*)",         "aaabbb",       True)
  , ("(a+)(b*)",         "bbb",         False)
  , ("(a.)",             "ab",           True)
  , ("(a.)",             "a",           False)
  , ("(a.)*",            "abab",         True)
  , ("(a.)*",            "aba",         False)
  , ("(a.)+",            "ab",           True)
  , ("(a.)+",            "",           False)
  , ("(a.b)",            "acb",          True)
  , ("(a.b)",            "ab",          False)

  , ("a(.*)b",           "ab",           True)
  , ("a(.*)b",           "axxxb",        True)
  , ("a(.*)b",           "axxx",        False)
  , ("(.*)",             "",            True)
  , ("(.*)",             "abc",          True)
  , ("(.+)",             "a",            True)
  , ("(.+)",             "",           False)
  , ("(a.*c)",           "ac",           True)
  , ("(a.*c)",           "abbbc",        True)
  , ("(a.*c)",           "abbb",        False)

  , ("(a+b)c",           "aaabc",        True)
  , ("(a+b)c",           "bc",          False)
  , ("a+(bc)",           "aaabc",        True)
  , ("a+(bc)",           "aabc",         True)
  , ("a+(bc)",           "aaab",        False)
  , ("(ab.c)",           "abxc",         True)
  , ("(ab.c)",           "abc",         False)
  , ("(a.*b.*c)",        "axbyc",        True)
  , ("(a.*b.*c)",        "ac",          False)
  , ("(a+b*)(c+)",       "aaabbbccc",    True)

  , ("(a+b*)(c+)",       "ccc",         False)
  , ("((ab)+c)",         "abc",          True)
  , ("((ab)+c)",         "ababc",        True)
  , ("((ab)+c)",         "ab",          False)
  , ("(a(bc)+)",         "abcbc",        True)
  , ("(a(bc)+)",         "abc",          True)
  , ("(a(bc)+)",         "ab",          False)
  , ("((a.)+b)",         "abababb",     True)
  , ("((a.)+b)",         "aabab",       False)
  , ("((a.)+b)",         "aba",         False)

  , ("(a(b(c)))",        "abc",          True)
  , ("(a(b(c)))",        "ab",          False)
  , ("((a*)(bc))",       "bc",           True)
  , ("((a*)(bc))",       "aaabc",        True)
  , ("((a*)(bc))",       "aab",         False)
  , ("(a(b*)c)",         "ac",           True)
  , ("(a(b*)c)",         "abbbc",        True)
  , ("(a(b*)c)",         "abcx",        False)
  , ("((ab)*(c+))",      "ababccc",      True)
  , ("((ab)*(c+))",      "abab",        False)

  , ("(a.(b+))",         "acbbb",        True)
  , ("(a.(b+))",         "ac",          False)
  , ("((a*)(b*)(c*))",   "",             True)
  , ("((a*)(b*)(c*))",   "aaabbbccc",    True)
  , ("((a*)(b*)(c*))",   "aaacccbbb",   False)
  , ("(a(b(c*)))",       "ab",           True)
  , ("(a(b(c*)))",       "abccc",        True)
  , ("(a(b(c*)))",       "ac",          False)
  , ("((a.)*(b+))",      "ababbb",       True)
  , ("((a.)*(b+))",      "aba",         False)

  , ("(((ab)+)*)",       "",             True)
  , ("(((ab)+)*)",       "ababab",       True)
  , ("(((ab)+)*)",       "aba",         False)
  , ("(a+(b(c+)))",      "aabccc",       True)
  , ("(a+(b(c+)))",      "ab",          False)
  , ("((a+)(b+)(c+))",   "aaabbbccc",    True)
  , ("((a+)(b+)(c+))",   "aaabbb",      False)
  , ("((a.*)(b+))",      "axxxbbb",      True)
  , ("((a.*)(b+))",      "axxx",        False)
  , ("(.(.)*)",          "abc",          True)

  , ("((a+b*)(c+))*",          "",               True)
  , ("((a+b*)(c+))*",          "aaabbbccc",       True)
  , ("((a+b*)(c+))*",          "cccaaabbb",      False)

  , ("(a*(bc)+d)",             "bcd",             True)
  , ("(a*(bc)+d)",             "aaabcbcd",        True)
  , ("(a*(bc)+d)",             "aaabd",           False)

  , ("((ab.)+(c*))",           "abxc",            True)
  , ("((ab.)+(c*))",           "abxabxc",         True)
  , ("((ab.)+(c*))",           "abx",             True)
  , ("((ab.)+(c*))",           "ab",              False)

  , ("((a.)*(b+)(c.))",        "abbbbcx",        True)
  , ("((a.)*(b+)(c.))",        "bbcx",             True)
  , ("((a.)*(b+)(c.))",        "ababbbcc",        True)

  , ("((a+b)(c+d))*",          "acbdac",          False)
  , ("((a+b)(c+d))*",          "",                True)
  , ("((a+b)(c+d))*",          "aaabcd",          True)

  , ("(a(bc+d)*)",             "abcbc",            False)
  , ("(a(bc+d)*)",             "ad",              False)
  , ("(a(bc+d)*)",             "a",                True)

  , ("((a*)(b(c+))*)",         "abcccbcc",        True)
  , ("((a*)(b(c+))*)",         "aaab",             False)

  , ("((a+b+).*)",             "aaabxxx",          True)
  , ("((a+b+).*)",             "bbb123",           False)
  , ("((a+b+).*)",             "xxx",             False)

  , ("((a.)+(b*)c)",           "ababbbc",          True)
  , ("((a.)+(b*)c)",           "abc",              True)
  , ("((a.)+(b*)c)",           "ababbb",           False)

  , ("((a(b*))+c)",            "abbbc",            True)
  , ("((a(b*))+c)",            "ac",               True)
  , ("((a(b*))+c)",            "abbb",             False)

  , ("(((ab)*c)+d)",           "abcd",             True)
  , ("(((ab)*c)+d)",           "ababcabcd",        True)
  , ("(((ab)*c)+d)",           "ab",              False)

  , ("((a*)(b*)(c+).*)",       "aaabbbcccxxx",     True)
  , ("((a*)(b*)(c+).*)",       "ccc",               True)
  , ("((a*)(b*)(c+).*)",       "bbb",              False)

  , ("(a.(b+c)+d*)",           "axbbbc",            True)
  , ("(a.(b+c)+d*)",           "axbcbbbcd",        True)
  , ("(a.(b+c)+d*)",           "axd",              False)

  , ("(((a.)*)+((b.)+))",      "abab",              False)
  , ("(((a.)*)+((b.)+))",      "bxbx",              True)
  , ("(((a.)*)+((b.)+))",      "",                 False)

  , ("((a+)(.(b+))*)",         "aaxbxb",            True)
  , ("((a+)(.(b+))*)",         "a",                 True)
  , ("((a+)(.(b+))*)",         "xb",               False)

  , ("((a*)(.(.)+))",          "xyz",               True)
  , ("((a*)(.(.)+))",          "a1b2",              True)
  , ("((a*)(.(.)+))",          "a",                False) ]

testSuite1 :: [String]
testSuite1 = 
            [ ""
            , "a"
            , "b"
            , "c"
            , "."
            , "ε"
            , "a*"
            , "b+"
            , "c*"
            , ".+"
            , "a+"
            , "(a)"
            , "(b*)"
            , "(c+)"
            , "(.)"
            , "()"
            , "aε"
            , "εa"
            , "a.b"
            , "(a.b)"
            , "(a)(b)"
            , "(a*)(b+)"
            , "(a+)(b*)"
            , "(a*)(b*)(c*)"
            , "a(bc)"
            , "(ab)c"
            , "(a)(bc)"
            , "((ab)c)"
            , "(a(b)c)"
            , "((a)(b)(c))"
            , "abc"
            , "a(bc)+"
            , "(ab)+"
            , "(ab)*"
            , "((ab)+)*"
            , "a(b+)"
            , "(a*)(bc*)"
            , "a.(b)"
            , "(a.)+"
            , ".a*"
            , "(..)"
            , "(..)*"
            , "(.a)*"
            , "a(.b)"
            , "(a.b+)"
            , "(.b)*"
            , "(.b+)"
            , "((a.b))"
            , "(a.(b))"
            , "a(b.)c"
            , "((a(b.)c))"
            , "a((bc)*)"
            , "(a((bc)*))"
            , "(a(bc)*)"
            , "(a(bc)+)"
            , "((a*)(b+))"
            , "a*(bc)"
            , "a*(bc)+"
            , "(ab)(cd)"
            , "(ab)(cd+)"
            , "((ab)(cd*))"
            , "((ab)*)(cd)"
            , "(a(bc)d)"
            , "(a(b(c)))"
            , "(a(b(c+)))"
            , "((a.(b.c)))"
            , "(a(b.(c)))"
            , "(((a.b).c))"
            , "(a*(b.c))"
            , "((a*b*)c)"
            , "(a(bc)*d)"
            , "(a(bc)+d)"
            , "(a.(b.(c)))"
            , "(a.(b.(c*)))"
            , "(a.(b.(c+)))"
            , "((a.b).(c*))"
            , "((a*)(b.c+))"
            , "a*b.c+"
            , "(((a*)b)c)"
            , "(a((b*)c))"
            , "(a(b(c)d))"
            , "((a)(bc))(d)"
            , "(a)((bc)(d+))"
            , "(ab)(c+)(d*)"
            , "(a.(b*c))"
            , "(a.(b+)c)"
            , "((a.(b+)(c)))"
            , "(a(b+)).(c)"
            , "((a(b+)).(c))"
            , "(a(bc.)*)"
            , "(a.(bc.)*)"
            , "((a.(bc.)*))"
            , "(a+)(b+)(c+)"
            , "(a+b+c+)"
            , "a+(bc)+"
            , "(a+(bc)+)"
            , "((a+)(bc)+)"
            , "(a.b.c)+"
            ]

testSuite2 :: [String]
testSuite2 =
  [ "a*a"
  , "b*b"
  , "c*c"
  , "d+d"
  , "e*e"
  , "f+f"
  , "g*g"
  , "h+h"
  , "i*i"
  , "j+j"
  , "k*k"
  , "l+l"
  , "m*m"
  , "n+n"
  , "o*o"
  , "p+p"
  , "q*q"
  , "r+r"
  , "s*s"
  , "t+t"
  , "u*u"
  , "v+v"
  , "w*w"
  , "x+x"
  , "y*y"
  , "z+z"
  , "a*b*c"
  , "a*b+c"
  , "a*bc+"
  , "(ab)*c"
  , "a(bc)*"
  , "a(bc)+"
  , "(ab)+(cd)"
  , "(ab)(cd+)"
  , "(ab*)(cd*)"
  , "(a*b)(c*d)"
  , "a(b)c(d)"
  , "a(bc)d+"
  , "(a)(b)(c)(d)"
  , "(a*)(b+)(c*)"
  , "(a*)(b*)(c*)(d*)"
  , "(a+)(b*)(c*)(d+)"
  , "a(bc)(de)"
  , "(ab)(cde)"
  , "a(b(cd))"
  , "(a(b(cd)))"
  , "((a)b)c+"
  , "((a*)(b+))c"
  , "(a(b(c)))"
  , "a(b(c(d)))"
  , "(a(b(c(d+))))"
  , "(a.(b.(c)))"
  , "(a.(b.(c*)))"
  , "(a.(b.(c+)))"
  , "((a*b)c)d"
  , "(a(b(c*)d))"
  , "(a(b(c+)d))"
  , "(ab)(c)(d)(e)"
  , "(a)(bc)(d*)(e+)"
  , "((a)(bc))(d*)(e*)"
  , "(a.(b*c))d"
  , "(a.(b+)c)d"
  , "(a.(b+)(c))d"
  , "((a.(b+)(c)))d"
  , "a(b+).(c)"
  , "(a(b+)).(c)"
  , "(a(b+)).(c+)"
  , "(a(bc.)*)d"
  , "(a.(bc.)*)e"
  , "((a.(bc.)*))f"
  , "(a+)(b+)(c+)(d+)"
  , "a+(bc)+d+"
  , "(a+(bc)+)d"
  , "((a+)(bc)+)d"
  , "(a.b.c)+d"
  , "(a.b)*(c.d)"
  , "((a.b)*)(c.d)"
  , "(a.(b*)).(c*)"
  , "(a.(b+)).(c+)"
  , "(a(b(c(d+)))).e"
  , "(a(b(c(d*))))f"
  , "a(b(c(d*)))"
  , "(ab)*(bc)*(cd)"
  , "(ab)+(bc)+(cd+)"
  , "(ab*)(bc+)(cd*)"
  , "(a*)(b*)(c*)(d*)(e*)"
  , "(a+)(b+)(c+)(d+)(e+)"
  , "a(bcdef)+"
  , "(abcdef)+"
  , "(abc*)(def+)"
  , "a(bc*)(de+)"
  , "a.(b.c.d.e)"
  , "(a.b.c.d)+"
  , "((a.b).(c.d))"
  , "(a.(b.(c.(d))))"
  , "(a*(b*(c*(d*))))"
  , "(a+(b+(c+(d+))))"
  , "εa+"
  , "aε*"
  , "()*"
  , "()+"
  , "a()b"
  , "(aε)b+"
  , "(a)()(b)(c*)"
  , "((a)ε(b))"
  , "(a(εc))"
  , "(a(bεc))"
  , "(a(b(εc)))"
  , "a(b(ε(c)))"
  , "(a.b.ε)+"
  , "(ε.*)+"
  , "(a*ε*)"
  , "(a+ε+)"
  , "a*()b*"
  , "(abεcd)+"
  , "(aεbεc)*"
  , "((aε)(bε))"
  , "(a()b(c*))"
  , "((((a))))"
  , "((((a*))))"
  , "((((a+))))"
  , "(a(b(c(d(e)))))"
  , "(a(b(c(d(e*)))))"
  , "(a(b(c(d(e+)))))"
  , "a(bc)(de)(fg)"
  , "(ab)(cd)(ef)(gh)"
  , "(ab)*(cd)*(ef*)"
  , "(ab)+(cd+)(ef)"
  , "a(bc+)(de*)"
  , "(ab)(bc+)(cd*)"
  , "((ab)(bc))(cd)"
  , "(a.(b.c))*"
  , "(a.(b.c))+"
  , "((a.(b.c)))+"
  , "a(b.(c.d))"
  , "(a.(b.(c.d)))"
  , "(a.(b.(c.(d))))"
  , "(a*b*c*d*)"
  , "(a+b+c+d+)"
  , "a*(b+(c*(d+)))"
  , "(a*(b+(c*(d+))))"
  , "(a+)(b*)(c+)(d*)"
  , "a.(b+).c*"
  , "(a.(b+).c*)"
  , "(a.(b+).c*)+"
  , "((a.(b+).c*))"
  , "((a.(b+).c*))+"
  , "(a.(b.c))*"
  , "(a.(b.c))+"
  , "(ab)(bc)*(cd)(de)+"
  , "a(bc)*(cd)*(de)+"
  , "((a(bc)*(cd)*(de)+))"
  , "(a(b(c(d(e(f))))))"
  , "(a(b(c(d(e(f*))))))"
  , "(a(b(c(d(e(f+))))))"
  , "(.*a.*)"
  , "(a.*b*)"
  , "(a.+b+)"
  , "(a*.b+)"
  , "(a+.b*)"
  , "(a+(bc*)d+)"
  , "(a*(bc+)d*)"
  , "(a*(b+(c*(d+(e)))))"
  , "a(bcdefg)*"
  , "a(bcdefg)+"
  , "(abcdefg)*"
  , "(abcdefg)+"
  , "(a*)(bc+)(d*)(ef*)"
  , "(a+)(bc*)(d+)(ef+)"
  , "(a.(b.c.d))"
  , "(a.(b.c.d))*"
  , "(a.(b.c.d))+"
  , "((a.(b.c.d)))"
  , "((a.(b.c.d)))+"
  , "a(b(c(d(e(f(g))))))"
  , "(a(b(c(d(e(f(g*)))))))"
  , "(a(b(c(d(e(f(g+)))))))"
  , "(ab)c*(de)f+"
  , "(ab+)c*(de*)f"
  , "(a(bc)*d(e+))"
  , "((a(bc)*d(e+)))"
  , "((a(bc)*(d(e+))))"
  , "(a+)(b*)(c*)(d+)(e*)(f*)"
  , "a(bc)(de)*f+"
  , "(ab)*(cd)+(ef*)"
  , "(ab)*(cd)*(ef*)"
  , "(ab+)(cd*)(ef+)"
  , "(ab*)(cd+)(ef*)"
  , "(a*(b*(c*(d*(e*)))))"
  , "(a+(b+(c+(d+(e+)))))"
  , "((a*b*c)d)e"
  , "a*((bc)+)d"
  , "(a*((bc)+)d)"
  , "((a*((bc)+)d))"
  , "(a.(b.(c.(d.(e.(f))))))"
  , "((a.(b.(c.(d.(e.(f)))))))"
  , "a*(b*(c*(d*(e*(f*(g*))))))"
  , "(a+(b+(c+(d+(e+(f+(g+)))))))"
  , "ε"
  , "ε*"
  , "ε+"
  , "()a(bc)"
  , "(aε)(bε)"
  , "((aε)b)+"
  , "(aε)*"
  , "(aε)+"
  , "aεbεcε"
  , "(aεbεcε)*"
  , "((aε)(bε)(cε))"
  , "(εa*)(εb+)"
  , "a*(b*ε)c+"
  , "(a*(b*ε)c+)"
  , "(a*(b*ε)c+)*"
  , "(abc*)(εde+)"
  , "((abc*)(εde+))"
  , "a.()b*(c+)"
  , "((a.()b*(c+)))"
  , "(a.b.c.d.e.f)"
  , "((a.b.c.d.e.f))"
  , "((a.b.c.d.e.f))*"
  , "((a.b.c.d.e.f))+"
  , "(a(bc)+d(e*))(f+)"
  , "(a*(bc)d*(ef))"
  , "(a+(bc)+d+(ef)*)"
  , "(ab*c*d+e*f+)"
  , "(abcd)*(efgh)+"
  , "(abc*d+)(ef*g+)"
  , "(a.(b.c))*"
  , "(a.(b.c.d))*"
  , "(a.(b.(c.(d.(e.(f))))))"
  , "(a*(b.(c*(d.(e*(f))))))"
  , "(a+(b.(c+(d.(e+(f))))))"
  , "a(bcdefgh)*"
  , "a(bcdefgh)+"
  , "(abcdefgh)*"
  , "(abcdefgh)+"
  , "(a*)(b+)(c*)(d+)(e*)(f+)"
  , "((a*)(b+)(c*)(d+)(e*)(f+))"
  , "((a*)(b+)(c*)(d+)(e*)(f+))*"
  ]

complexRegexes :: [String]
complexRegexes =
    [ "(a*(bc+(d.(e*f+))))(a*(bc+(d.(e*f+))))()(a+b*c*(d+))(a.(b.(c.(d.(e*(f+))))))"
    , "(a*(b*(c+(d*(e+(f*(g*(h+))))))))(bc*(de+))(a.(b.(c.(d.(e.(f.(g)))))))()(abc+)*"
    , "((a.b.c.d.e.f.g.h)+)(a*(b*(c*(d*(e*(f*(g*(h*(i+)))))))))(abc*(def+))()(a+)(b*)(c*)"
    , "(a(bc*(d+(e.(f*(g+))))))(a*(b.(c.(d.(e.(f.(g.(h))))))))(abc*(de+f*))(g+h*)*()a*b*c+"
    , "((a*(b+)(c*)(d*(e+))))(f.(g.(h.(i.(j.(k*(l+)))))))(a*b*c*d*)(e+f*)(g*(h+))()" ]
