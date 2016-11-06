module Handler.OneTimePad where

import Import
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import System.Random.MWC (uniformVector)
import Control.Monad.Reader (asks)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64 as B64
import Text.Blaze.Html (unsafeByteString)

getOneTimePadR :: Handler Html
getOneTimePadR = do
    mcleartext <- lookupGetParam "cleartext"
    let result =
            case mcleartext of
                Nothing -> mempty
                Just cleartext -> do
                    let cleartextBS = encodeUtf8 cleartext
                    gen <- asks appGen
                    random <- liftIO $ fromByteVector <$> uniformVector gen (length cleartextBS)
                    let pad = pack $ S.zipWith (+) random cleartextBS
                    $(widgetFile "one-time-pad-encrypt")

    defaultLayout $ do
        setTitle "One Time Pad"

        $(widgetFile "one-time-pad")

toBinary :: ByteString -> String
toBinary =
    foldr addWord ""
  where
    addWord w rest = pad (showIntAtBase 2 intToDigit w "") ++ (' ' : rest)
    pad x = replicate (8 - length x) '0' ++ x
