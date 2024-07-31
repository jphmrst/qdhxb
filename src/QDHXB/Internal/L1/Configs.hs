
-- |Configuration options for XSD import calls.  Naming these very
-- long expressions takes them mercifully out of error messages; but
-- since we use them in a template call, they need to be in their own
-- module.

module QDHXB.Internal.L1.Configs (nestedTypesConfig) where

import QDHXB.Options
import QDHXB.Options.TranslationOptionSet

-- import QDHXB.Internal.Debugln

-- ^ Configuration for the NestedTypes module.
nestedTypesConfig :: QDHXBOptionSet -> QDHXBOptionSet
nestedTypesConfig =
  forNamespace "http://www.w3.org/XML/1998/namespace"
    (defaultModule "QDHXB.Internal.L1.XML")

  {- Renaming from debugging.xsd -}

  . renameConstructor
    "SimpleDerivationSetDatatypes8Union1SimpleRestrtoken"
    "SimpleDerivationSetToken"
  . renameConstructor
    "SimpleDerivationSetDatatypes8Union2ListSimpleRestrderivationControlElement"
    "SimpleDerivationSetControl"

  . renameConstructor
    "SimpleDerivationSetDatatypes8Union1SimpleRestrtoken"
    "SimpleDerivationSetByToken"
  . renameConstructor
    "SimpleDerivationSetDatatypes8Union2ListSimpleRestrderivationControlElement"
    "SimpleDerivationSetControl"

  . renameType "Datatypes8Union1SimpleRestrtoken"
               "SimpleDerivationSetTokenImpl"

  . renameType "Datatypes8Union2ListSimpleRestrderivationControlElement"
               "SDSDerivationControlList"
  . renameType "Datatypes8Union2ListSimpleRestrderivationControl"
               "SDSDerivationControl"

  . renameType "Datatypes8Union2ListSimpleRestrderivationControl"
               "SimpleDerivationSetControlImpl"

  . renameType "Datatypes20GroupSeq4Choice" "SimpleRestrictionModelFacets"
  . renameConstructor "Datatypes20GroupSeq4ChoiceFacetX2"
                      "SimpleRestrictionModelFacetAny"
  . renameConstructor "Datatypes20GroupSeq4ChoiceRawXML"
                      "SimpleRestrictionModelFacetRaw"

  . renameTypeAndConstructor "Datatypes22Elem1ComplexExt"
                             "RestrictionImpl"
  . renameTypeAndConstructor "Datatypes24Elem1ComplexExt" "ListImpl"
  . renameTypeAndConstructor "Datatypes24Elem1ComplexExtBSeq"
                             "ListImplTypeList"
  . renameTypeAndConstructor "Datatypes26Elem1ComplexExt" "UnionImpl"
  . renameTypeAndConstructor "Datatypes26Elem1ComplexExtBSeq"
                             "UnionImplTypeList"

  . renameType "Datatypes44Elem2ComplexRestrictednumFacet"
               "TotalDigitsImpl"

  . renameType "Datatypes56Elem2ComplexRestrictedfacet" "WhitespaceImpl"
  . renameType "Datatypes58Elem2ComplexRestrictednoFixedFacet"
               "PatternImpl"
  . renameType "Datatypes62Elem2ComplexRestrictedfacet"
               "ExplicitTimezoneImpl"

  {-
  . renameType "" ""
  . renameConstructor "" ""
  . renameTypeAndConstructor "" ""
  -}

  {- Renaming from xsd.xsd -}

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

  . attributeTypeHint (Just "http://www.w3.org/2001/XMLSchema") "value"
                      "String"

