Run `stack ghci` to run with tasty and all other dependencies installed

In ghci:

import Test.Tasty (defaultMain)
:l test/Course/ListTest.hs
:browse Course.ListTest
defaultMain headOrTest
