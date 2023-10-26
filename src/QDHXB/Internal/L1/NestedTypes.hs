{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn this back on when more stable

-- |The full XSD specification, bootstrapped here for the full
-- exported implementation.
module QDHXB.Internal.L1.NestedTypes(
  module QDHXB.Internal.L1.NestedTypes
  ) where

import QDHXB.Internal.L0 (qdhxb)
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

        . renameTypeAndConstructor "Top24Elem1ComplexExtBSeq"
                                   "ListContentRecord"
        . renameTypeAndConstructor "Top26Elem1ComplexExtBSeq"
                                   "UnionContentRecord"

        . renameTypeAndConstructor "FacetX2" "Facet"

        . renameConstructor "Top20GroupSeq4ChoiceFacetX2"
                            "RestrictionByFacet"
        . renameConstructor "Top20GroupSeq4ChoiceRawXML"
                            "RestrictionByRawXML"

        . renameConstructor "SimpleDerivationSetTop8Union1SimpleRestrtoken"
                            "SimpleDerivationSetByToken"
        . renameConstructor
          "SimpleDerivationSetTop8Union2ListSimpleRestrderivationControlElement"
          "SimpleDerivationSetControl"

        . renameType "Top8Union2ListSimpleRestrderivationControlElement"
                     "SDSDerivationControlList"
        . renameType "Top8Union2ListSimpleRestrderivationControl"
                     "SDSDerivationControl"

        . renameType "Top44Elem2ComplexRestrictednumFacet"
                     "TotalDigitsContentBase"
        . renameType "Top56Elem2ComplexRestrictedfacet" "WhiteSpaceFacet"
        . renameType "Top58Elem2ComplexRestrictednoFixedFacet"
                     "PatternNoFixedFacet"
        . renameTypeAndConstructor "AssertionX5" "Assertion"

        . renameType "Top62Elem2ComplexRestrictedfacet"
                     "ExplicitTimezoneFacet"

        . renameType "MinOccursX15" "MinOccurs"
        . renameType "MaxOccursX16" "MaxOccurs"
        . renameType "OccursX3" "Occurs"
        . renameType "NameX17" "Name"
        . renameType "RefX18" "Ref"
        . renameType "DefRefX4" "DefRef"
        . renameType "AllX8" "All"
        . renameConstructor "NestedParticleElementX7" "NestedParticleElement"
        . renameConstructor "NestedParticleGroupX9X19" "NestedParticleGroupRef"
        . renameConstructor "ParticleElementX7X20" "ParticleElement"
        . renameConstructor "ParticleGroupX9X21" "ParticleGroup"
        . renameConstructor "ParticleAllX8" "ParticleAll"

        . renameTypeAndConstructor "GroupX9" "Group"
        . renameTypeAndConstructor "ElementX7" "Element"
        . renameTypeAndConstructor "AttributeX11" "Attribute"

        . renameTypeAndConstructor "Top30Elem2ComplexExt" "SchemaImpl"
        . renameTypeAndConstructor "Top30Elem2ComplexExtBSeq" "SchemaImplRecord"

        . renameConstructor "Top20GroupSeq4ChoiceFacetX2" "SRMFacet"
        . renameConstructor "Top20GroupSeq4ChoiceRawXML" "SRMAny"

        . renameType "Top8Union1SimpleRestrtoken" "SimpleDerivationSetTokenImpl"
        . renameTypeAndConstructor "Top12ComplexExtBSeq" "AnnotatedExtRecord"
        . renameConstructor "SchemaTopElementX7" "SchemaTopElement"
        . renameConstructor "SchemaTopAttributeX11" "SchemaTopAttribute"
        . renameConstructor "RedefinableSimpleTypeX1" "RedefinableSimpleType"
        . renameConstructor "RedefinableComplexTypeX6" "RedefinableComplexType"
        . renameConstructor "RedefinableGroupX9" "RedefinableGroup"
        . renameConstructor "RedefinableAttributeGroupX12"
                            "RedefinableAttributeGroup"
        . renameConstructor "DerivationSetTop24Union1SimpleRestrtoken"
                            "DerivationSetByToken"
        . renameConstructor "DerivationSetList_reducedDerivationControl"
                            "DerivationSetControlList"
        . renameType "List_typeDerivationControl" "TypeDerivationControlList"

        . renameConstructor "FullDerivationSetTop24Union1SimpleRestrtoken"
                            "FullDerivationSetByToken"
        . renameConstructor "FullDerivationSetList_reducedDerivationControl"
                            "FullDerivationSetControlList"
        . renameConstructor "AllNNITop32Union1SimpleRestrNMTOKEN"
                            "AllNNIUnbounded"
        . renameConstructor "AllNNINonNegativeInteger" "AllNNIPosInt"
        . renameConstructor "TypeDefParticleGroupX9" "TypeDefParticleGroup"
        . renameConstructor "TypeDefParticleAllX8" "TypeDefParticleAll"
        . renameConstructor "NestedParticleElementX7" "NestedParticleElement"
        . renameConstructor "NestedParticleGroupX9X19" "NestedParticleGroup"
        . renameConstructor "ParticleElementX7X20" "ParticleElement"
        . renameConstructor "ParticleGroupX9X21" "ParticleGroup"
        . renameConstructor "ParticleAllX8" "ParticleAll"
        . renameTypeAndConstructor "ComplexTypeX6" "ComplexType"
        . renameConstructor "BlockSetTop84Union1SimpleRestrtoken" "BlockSetAll"
        . renameConstructor
          "BlockSetTop84Union2ListSimpleRestrderivationControlElement"
          "BlockSetKeywordList"
        . renameConstructor "NamespaceX34" "NamespaceAttr"
        . renameType "NotNamespaceX35" "NotNamespace"
        . renameType "ProcessContentsX36" "ProcessContents"
        . renameTypeAndConstructor "AnyAttrGroupX10" "AnyAttrGroup"
        . renameConstructor "XpathDefaultNamespaceTop140Union1SimpleRestrtoken"
                            "XpathDefaultNamespaceToken"
        . renameConstructor "XpathDefaultNamespaceAnyURI"
                            "XpathDefaultNamespaceURI"
        . renameTypeAndConstructor "AttributeGroupX12" "AttributeGroup"
        . renameType "Top24Union1SimpleRestrtoken" "DerivationSetByTokenRecord"

        . renameConstructor "FullDerivationSetTop28Union1SimpleRestrtoken"
                            "FullDerivationSetAll"
        . renameConstructor "FullDerivationSetList_typeDerivationControl"
                            "FullDerivationSetControlList"
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
