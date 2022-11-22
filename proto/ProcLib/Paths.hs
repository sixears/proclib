{-# LANGUAGE QuasiQuotes #-}

module ProcLib.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

cat :: AbsFile
cat = [absfile|__coreutils__/bin/cat|]

env :: AbsFile
env = [absfile|__coreutils__/bin/env|]

false :: AbsFile
false = [absfile|__coreutils__/bin/false|]

echo :: AbsFile
echo = [absfile|__coreutils__/bin/echo|]

grep :: AbsFile
grep = [absfile|__gnugrep__/bin/grep|]

hostname :: AbsFile
hostname = [absfile|__inetutils__/bin/hostname|]

ping :: AbsFile
-- | use the nix wrapper, we need setuid for raw socket creation
ping = [absfile|/run/wrappers/bin/ping|]

pwd :: AbsFile
pwd = [absfile|__coreutils__/bin/pwd|]

true :: AbsFile
true = [absfile|__coreutils__/bin/true|]

wc :: AbsFile
wc = [absfile|__coreutils__/bin/wc|]

