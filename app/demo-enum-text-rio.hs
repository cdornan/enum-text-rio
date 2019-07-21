{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           RIO
import           Text.Enum.RIO.Fmt


data Foo = FOO_bar | FOO_bar_baz
  deriving (Bounded,Enum,EnumText,Eq,Ord,Show)
  deriving (Buildable,Display,TextParsable) via UsingEnumText  Foo


main :: IO ()
main = run $ test [minBound..maxBound]

test :: [Foo] -> RIO Env ()
test []         = return ()
test (foo:foos) = do
    logInfo $ fmt $ "hello "+|foo|+""
    test foos

--
-- RIO setup
--

data Env =
  Env
    { _env_lf :: LogFunc
    }

instance HasLogFunc Env where
  logFuncL = lens
    _env_lf
    (\x y -> x { _env_lf = y })

run :: RIO Env a -> IO a
run bdy = rio_log_func $ \lf -> runRIO (Env lf) bdy

rio_log_func :: (LogFunc -> IO a) -> IO a
rio_log_func cb = do
    lo <- opts <$> logOptionsHandle stdout is_logging
    withLogFunc lo cb
  where
    opts =
      setLogVerboseFormat   True
      . setLogUseTime       True
      . setLogUseLoc        False
      . setLogMinLevel      LevelDebug

    is_logging = True
