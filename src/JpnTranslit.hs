module JpnTranslit where

import Data.List
import Data.Maybe

-- translit :: String -> String
-- translit ""       = ""
-- translit s@(x:xs) = case listToMaybe . match $ s of
--     Nothing     -> x : translit xs
--     Just (k, v) -> v ++ maybe "" translit (stripPrefix k s)

translit :: String -> String
translit ""       = ""
translit s@(x:xs) = maybe (x : translit xs) (\(k, v) -> v ++ maybe "" translit (stripPrefix k s)) . listToMaybe . match $ s

-- translit :: String -> String
-- translit s = foldr (\x xs -> maybe (x : translit xs) (\(k, v) -> v ++ maybe "" translit (stripPrefix k s)) . listToMaybe . match $ s) "" s

match :: String -> [(String, String)]
match s = filter (flip isPrefixOf s . fst) patterns

patterns :: [(String, String)]
patterns =
    [("a","あ")   ,("i","い")   ,("u","う")   ,("e","え")
    ,("o","お")   ,("ka","か")  ,("ki","き")  ,("ku","く")
    ,("ke","け")  ,("ko","こ")  ,("ga","が")  ,("gi","ぎ")
    ,("gu","ぐ")  ,("ge","げ")  ,("go","ご")  ,("sa","さ")
    ,("shi","し") ,("su","す")  ,("se","せ")  ,("so","そ")
    ,("za","ざ")  ,("ji","じ")  ,("zu","ず")  ,("ze","ぜ")
    ,("zo","ぞ")  ,("ta","た")  ,("chi","ち") ,("tsu","つ")
    ,("te","て")  ,("to","と")  ,("da","だ")  ,("de","で")
    ,("do","ど")  ,("na","な")  ,("ni","に")  ,("nu","ぬ")
    ,("ne","ね")  ,("no","の")  ,("ha","は")  ,("hi","ひ")
    ,("fu","ふ")  ,("he","へ")  ,("ho","ほ")  ,("ba","ば")
    ,("bi","び")  ,("bu","ぶ")  ,("be","べ")  ,("bo","ぼ")
    ,("pa","ぱ")  ,("pi","ぴ")  ,("pu","ぷ")  ,("pe","ぺ")
    ,("po","ぽ")  ,("ma","ま")  ,("mi","み")  ,("mu","む")
    ,("me","め")  ,("mo","も")  ,("ya","や")  ,("yu","ゆ")
    ,("yo","よ")  ,("ra","ら")  ,("ri","り")  ,("ru","る")
    ,("re","れ")  ,("ro","ろ")  ,("wa","わ")  ,("wo","を")
    ,("n","ん")   ,("m","ん")   ,(".","。")]

-- "ghoenmirutsukgo"
-- oldStk: "", newStk: "g", try match "g": [("ga","が"), ("gi","ぎ"), ("gu","ぐ"), ("ge","げ"), ("go","ご")], result: ""
-- oldStk: "g", newStk: "gh", try match "gh": [], result: "g"
-- oldStk: "h", newStk: "ho", try match "ho": [("ho","ほ")], result: "gほ"
-- oldStk "", newStk: "e", try match "e": [("e","え")], result:"gほえ"
-- oldStk "", newStk: "n", try match "n": [("na","な"), ("ni","に"), ("nu","ぬ"), ("ne","ね"), ("no","の"), ("n","ん")], result:"gほえ"
-- oldStk "n", newStk: "nm", newChar: 'm', if try match "nm" == [] then [("n","ん")], result: "gほえん"
-- oldStk "m", newStk: "mi", newChar: 'i', if try match "mi" == [] then ... else [("mi","み")], result:"gほえんみ"
-- oldStk "", newStk: "r", try match "r": [("ra","ら"), ("ri","り"), ("ru","る"), ("re","れ"), ("ro","ろ")], result:"gほえんみ"
-- oldStk "r", newStk: "ru", try match "ru": [("ru","る")], result:"gほえんみる"
-- oldStk "", newStk: "t", try match "t": [("ta","た"), ("chi","ち"), ("tsu","つ"), ("te","て"), ("to","と")], result:"gほえんみる"
-- oldStk "t", newStk: "ts", try match "ts": [("tsu","つ")], result:"gほえんみる"
-- oldStk "ts", newStk: "tsu", try match "tsu": [("tsu","つ")], result:"gほえんみるつ"
-- oldStk "", newStk: "k", try match "k": [("ka","か"), ("ki","き"), ("ku","く"), ("ke","け"), ("ko","こ")], result:"gほえんみるつ"
-- oldStk "k", newStk: "kg", try match "kg": [], result:"gほえんみるつk"
-- oldStk "g", newStk: "go", try match "go": [("go","ご")], result:"gほえんみるつkご"