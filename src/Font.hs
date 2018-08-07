--
-- Font.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

-- module for reading true type fonts into the program.
module Font
  ( module Font
  ) where

import Data.Word
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

-- TrueType is big-endian
data TTTypeSpec
  = ShortFrac -- shortFrac is a signed 16 bit integer with a bias of 14,
  -- so the top two bits are sign and units and the next fourteen are fractional
  | Fixed -- 16.16 bit (i.e. 32 total) signed fixed-point number
  | FWord -- 16 bit signed integer describing a quantity in FUnits
  | UFWord -- 16 bit unsigned integer again describing FUnits
  | F2Dot14 -- 16-bit signed fixed number with the low 14 bits representing fraction
  -- pretty sure this is the same as ShortFrac...????
  | LongDateTime -- 64-bit signed integer representing a date by the number of seconds since 12:00 midnight,
  -- January 1, 1904. Pretty sure I don't care about dates for my application tho xD
  | SInt16 
  | SInt32
  | SInt64
  | UInt16 
  | UInt32
  | UInt64
  deriving (Show, Eq, Read, Ord, Enum)

-- these are the possible tables in a ttf file
data TTTableType
  -- required tables
  = Cmap
  | Glyph
  | Head
  | HorizontalHeader
  | HorizontalMetrics
  | Location
  | MaximumProfile
  | Name
  | PostScript
  -- optional tables
  | ControlValue
  | FontProgram
  | HorizontalDeviceMetrics
  | Kerning
  | OS2
  | ControlValueProgram
  deriving (Show, Eq, Read, Ord, Enum)

-- the table tags
tableCode :: TTTableType -> ByteString
tableCode Cmap = "cmap"
tableCode Glyph = "glyf"
tableCode Head = "head"
tableCode HorizontalHeader = "hhea"
tableCode HorizontalMetrics = "hmtx"
tableCode Location = "loca"
tableCode MaximumProfile = "maxp"
tableCode Name = "name"
tableCode PostScript = "post"
tableCode ControlValue = "cvt "
tableCode FontProgram = "fpgm"
tableCode HorizontalDeviceMetrics = "hdmx"
tableCode Kerning = "kern"
tableCode OS2 = "OS/2"
tableCode ControlValueProgram = "prep"

tableCodeWord :: TTTableType -> Word32
tableCodeWord = runGet (getWord32be) . tableCode

data OffsetSubtable = OffsetSubtable
  { scalerType :: Word32
  , numTables :: Word16
  , searchRange :: Word16
  , entrySelector :: Word16
  , rangeShift :: Word16
  }
  deriving (Read, Show, Eq, Ord)

instance Binary OffsetSubtable where
  put (OffsetSubtable{scalerType=st,numTables=nt,searchRange=sr,entrySelector=es,rangeShift=rs}) = do
    putWord32be st
    sequence_ $ map putWord16be [nt,sr,es,rs]
  get = OffsetSubtable <$> getWord32be <*> getWord16be <*> getWord16be <*> getWord16be <*> getWord16be

data TableDescriptor = TableDescriptor
  { tableTag :: Word32 -- tag for the table
  , tableCheckSum :: Word32 -- checksum for table
  , tableOffset :: Word32 -- offset from beginning of sfnt
  , tableLength :: Word32 -- length in bytes
  }
  deriving (Read,Show,Eq,Ord)

instance Binary TableDescriptor where
  put (TableDescriptor{tableTag=tt,tableCheckSum=cs,tableOffset=to,tableLength=tl}) =
    sequence_ $ map putWord32be [tt,cs,to,tl]
  get = TableDescriptor <$> getWord32be <*> getWord32be <*> getWord32be <*> getWord32be

data FontDirectory = FontDirectory
  { offsetSubtable :: OffsetSubtable
  , tableDirectory :: Map Word32 TableDescriptor
  }
  deriving (Read,Show,Eq,Ord)

instance Binary FontDirectory where
  put (FontDirectory{offsetSubtable=os,tableDirectory=td}) = do
    put os
    sequence_ $ map (put . snd) $ M.toAscList td
  get = do
    offsets <- get
    let nt = numTables offsets
    tableList <- sequence $ map (const get) [1..nt]
    let tables = M.fromList . map (\tdesc -> (tableTag tdesc,tdesc)) $ tableList
    return (FontDirectory offsets tables)


