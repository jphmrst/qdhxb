{-# LANGUAGE TemplateHaskell #-}

-- | Our internal representation of XSD elements.
module QDHXB.Internal.Types (
  -- * The representation types
  ItemRef(ElementItem, AttributeItem,  ComplexTypeItem),
  ItemDefn(SimpleRep, AttributeRep, SequenceRep)
  ) where

-- | A reference to an XSD element.
data ItemRef =
  ElementItem String (Maybe Int) (Maybe Int)
  -- ^ A named element type, possibly with numeric instance bounds.
  | AttributeItem String
  -- ^ The name of an attribute.
  | ComplexTypeItem String
  -- ^ The name of a complex type.
  deriving Show

-- | The actual definition of an XSD element.
data ItemDefn =
  SimpleRep String String
  -- ^ Defining one element to have the same structure as another.
  | AttributeRep String String
  -- ^ Defining the type of an attribute to be the same as another.
  | SequenceRep String [ItemRef]
  -- ^ Define an element to contain a sequence of subelements.
  deriving Show
