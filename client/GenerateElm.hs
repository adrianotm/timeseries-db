{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Api
import           Data.List
import           Data.Text        hiding (intercalate, map)
import           Repository.Model
import           Servant.Elm      (DefineElm (DefineElm), Proxy (Proxy),
                                   defElmImports, defElmOptions, defaultOptions,
                                   deriveBoth, generateElmModuleWith)

main :: IO ()
main =
  generateElmModuleWith
    defElmOptions
    ["Api"]
    defElmImports
    "src"
    [ DefineElm (Proxy :: Proxy TS),
      DefineElm (Proxy :: Proxy QueryModel),
      DefineElm (Proxy :: Proxy Tag),
      DefineElm (Proxy :: Proxy Sort),
      DefineElm (Proxy :: Proxy GroupBy),
      DefineElm (Proxy :: Proxy Agg),
      DefineElm (Proxy :: Proxy QueryR),
      DefineElm (Proxy :: Proxy GroupAggR),
      DefineElm (Proxy :: Proxy AggR),
      DefineElm (Proxy :: Proxy DTS)
    ]
    (Proxy :: Proxy API)
