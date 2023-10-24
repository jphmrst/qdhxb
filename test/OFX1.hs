{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn this back on when more stable

module OFX1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

{-
import QDHXB.Internal.Debugln
qdhxb (
  -- setDebugging input 0 .
  -- setDebugging names 3 .
  -- setDebugging unique 3 .
  setDebugging flattening 1 .
  -- setDebugging generate 1 .
  id
  ) ["ofx/OFX_Common.xsd"] -- Standalone
-}

{-
qdhxb' ["ofx/OFX_Common.xsd"] -- Standalone
qdhxb' ["ofx/OFX_TypeDefinitions.xsd"] -- Uses AbstractTransactionRequest
                                       -- in ofx/OFX_Common.xsd
qdhxb' ["ofx/OFX2_Protocol.xsd"] -- Uses ofx:StatementTransactionRequest
                                 -- in ofx/OFX_Banking_Message_Wrappers.xsd
-}

testOFX1 :: TLT IO ()
testOFX1 = inGroup "OFX test 1" $ do
  return ()
  {-
  inGroup "Scalars" $ do
    "Correctly decode <orderperson> text in shiporder0a.xml" ~:
      "John Smith" @== (lift $ loadOrderperson "test/shiporder0a.xml")
    "Correctly decode <price> text in shiporder0b.xml" ~:
      10.9 @== (lift $ loadPrice "test/shiporder0b.xml")
-}
