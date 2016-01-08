import Data.List (concat, intersperse)

inputString :: String
inputString = "I started using Rdio in 2011, when I was in college. I hope that I can reclaim the four years I put into it. Rdio says their service will continue unchanged at least for the coming weeks. In that time, the company must offer an easy way for users to export their playlists and collection data into some platform-agnostic format like CSV: As of right now, I can’t find an online tool to do it that actually works."

fizzbuzz :: String -> String
fizzbuzz inputString =
  let nums = [1..]
      inputWords = words inputString
      inputTokens = zip inputWords nums
      f (w,n) =
        case (n `mod` 3 == 0, n `mod` 5 == 0) of
          (False, False) -> w
          (True, False) -> "fizz"
          (False, True) -> "buzz"
          (True, True) -> "fizzbuzz"
  in
    (concat . (intersperse " ") . (map f)) inputTokens
