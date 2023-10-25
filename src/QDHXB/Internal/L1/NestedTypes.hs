{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn this back on when more stable

-- |The full XSD specification, bootstrapped here for the full
-- exported implementation.
module QDHXB.Internal.L1.NestedTypes(
  module QDHXB.Internal.L1.NestedTypes
  ) where

import QDHXB.Internal.L0
import QDHXB.Options
import qualified QDHXB.Expansions
import qualified QDHXB.Internal.L1.XML

-- Note that datatypes.xsd and xsd.xsd are mutually dependent, so both
-- must be loaded at the same call to `qdhxb`.  Recall (see ) that all
-- files are first parsed and flattened, and then all generated
-- together, so that all types etc. in the loaded files are known at
-- the final generation phase.

import QDHXB.Internal.Debugln
qdhxb (forNamespace "http://www.w3.org/XML/1998/namespace"
         (defaultModule "QDHXB.Internal.L1.XML")
        -- . setDebugging generate 2

        . renameTypeAndConstructor "Top152Elem2ComplexExt"
                                   "CompositionIncludeRecord"
        . renameTypeAndConstructor "Top158Elem2ComplexExt"
                                   "CompositionImportRecord"
        . renameTypeAndConstructor "Top154Elem2ComplexExt"
                                   "CompositionRedefineRecord"
        . renameTypeAndConstructor "Top156Elem2ComplexExt"
                                   "CompositionOverrideRecord"
        . renameTypeAndConstructor "Top182Elem2ComplexExt"
                                   "CompositionAnnotationRecord"

        . renameConstructor "SimpleDerivationSetTop8Union1SimpleRestrtoken"
                            "SimpleDerivationSetToken"
        . renameConstructor
          "SimpleDerivationSetTop8Union2ListSimpleRestrderivationControlElement"
          "SimpleDerivationSetControl"
        . renameType "SourceX37" "DocumentationSourceURI"
        . renameTypeAndConstructor "Top22Elem1ComplexExt" "Restriction"
        . renameConstructor "NamespaceListSpecialNamespaceList"
                            "NamespaceListSpecial"
        . renameConstructor "NamespaceListBasicNamespaceList"
                            "NamespaceListBasic"

        . renameTypeAndConstructor "SimpleTypeX1" "SimpleType"

        . renameType "Top20GroupSeq4Choice"
                              "SimpleRestrictionModelFacetOrAny"

        . renameTypeAndConstructor "Top24Elem1ComplexExt" "ListContent"
        . renameTypeAndConstructor "Top26Elem1ComplexExt" "UnionContent"

        {- ERRORS
        . renameTypeAndConstructor "Top24Elem1ComplexExtBSeq"
                                   "ListContentRecord"
        . renameTypeAndConstructor "Top26Elem1ComplexExtBSeq"
                                   "UnionContentRecord"
          -}

        . renameTypeAndConstructor "FacetX2" "Facet"

        . renameConstructor "Top20GroupSeq4ChoiceFacetX2"
                            "RestrictionByFacet"
        . renameConstructor "Top20GroupSeq4ChoiceRawXML"
                            "RestrictionByRawXML"

        -- IGNORED
        . renameConstructor "SimpleDerivationSetTop8Union1SimpleRestrtoken"
                            "SimpleDerivationSetByToken"
        -- IGNORED
        . renameConstructor
          "SimpleDerivationSetTop8Union2ListSimpleRestrderivationControlElement"
          "SimpleDerivationSetControl"

        -- IGNORED
        . renameType "Top8Union2ListSimpleRestrderivationControlElement"
                     "SDSDerivationControlList"

        . renameType "Top44Elem2ComplexRestrictednumFacet"
                     "TotalDigitsContentBase"
        . renameType "Top56Elem2ComplexRestrictedfacet" "WhiteSpaceFacet"
        . renameType "Top58Elem2ComplexRestrictednoFixedFacet"
                     "PatternNoFixedFacet"
        . renameTypeAndConstructor "AssertionX5" "Assertion"

        . renameType "Top62Elem2ComplexRestrictedfacet"
                     "ExplicitTimezoneFacet"

        {-
        . renameType "" ""
        . renameConstructor "" ""
        . renameTypeAndConstructor "" ""
        -}

        -- . setDebugging input 0
        -- . setDebugging names 3
        -- . setDebugging unique 4
        -- . setDebugging flattening 1
        -- . setDebugging generate 3
        -- . breakAfterAllInput
      ) [
  "src/QDHXB/datatypes.xsd", "src/QDHXB/xsd.xsd"
  ]
