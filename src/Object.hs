module Object where

-- serialisable types from stream
-- ensure bytestring contains at least some minimum number of bytes
class Serialisable t where
    -- return remaining bytes after reading the type
    -- otherwise return the input bytestring
    readObj :: ByteString -> (ByteString, Maybe t)
