{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn this back on when more stable

-- |The full XSD specification, bootstrapped here for the full
-- exported implementation.

module QDHXB.Internal.L1.NestedTypes(
  -- * Main XSD loading function
  loadSchema,
  -- * Representation types
  -- ** Attributes
  Attribute(..), AttributeGroup(..),
  -- ** Major XSD type declarations
  SimpleType(..),
  ComplexType(..), ComplexTypeModel(..),
  ListImpl(..), ListImplTypeList(..),
  UnionImpl(..), UnionImplTypeList(..),
  -- ** Facets
  Facet(..), NoFixedFacet, NumFacet, IntFacet,
  TotalDigitsImpl, WhitespaceImpl, PatternImpl, ExplicitTimezoneImpl,
  -- ** Derivations
  SimpleDerivation(..), SimpleDerivationSetTokenImpl, TypeDerivationControlList,
  -- ** Other respresentations
  All, Name, Composition(..), Element(..), GroupRef, Group(..),
  DocumentationSourceURI,
  Assertion(..), MinOccurs, MaxOccurs, Occurs(..),
  Ref, DefRef(..),
  NamespaceX34,
  NotNamespace, ProcessContents, AnyAttrGroup(..), DerivationControl,
  RestrictionImpl(..),
  SDSDerivationControl, SDSDerivationControlList,
  SimpleDerivationSet(..), Annotated(..), TopLevelSimpleType,
  LocalSimpleType, AnyType(..),
  SimpleRestrictionModel(..), SimpleRestrictionModelFacets(..),
  OpenAttrs, Xsd12ComplexExtBSeq(..), Xsd182Elem2ComplexExt(..),
  Xsd152Elem2ComplexExt(..), Xsd158Elem2ComplexExt(..),
  Xsd154Elem2ComplexExt(..), Xsd156Elem2ComplexExt(..), SchemaTop(..),
  Redefinable(..), TopLevelElement, TopLevelAttribute,
  Xsd174Elem2ComplexExt(..), LocalComplexType, NamedGroup, NamedAttributeGroup,
  FormChoice, ReducedDerivationControl, Xsd24Union1SimpleRestrtoken,
  List_reducedDerivationControl, DerivationSet(..), TypeDerivationControl,
  Xsd28Union1SimpleRestrtoken, FullDerivationSet(..),
  Xsd30Elem2ComplexExtBSeq4Seq(..), Xsd74Elem2ComplexExt(..),
  Xsd30Elem2ComplexExtBSeq6Seq(..), Xsd30Elem2ComplexExtBSeq(..),
  Xsd30Elem2ComplexExt(..), Xsd32Union1SimpleRestrNMTOKEN,
  AllNNI(..), TypeDefParticle(..), ExplicitGroup, LocalElement,
  NestedParticle(..), Particle(..), Xsd44ComplexExtBSeq(..),
  AttributeGroupRef, Xsd48GroupSeq2Choice(..), AttrDecls(..),
  Xsd50Elem2ComplexExt(..), Wildcard(..), Assertions(..), XSeq(..),
  Xsd72Elem2ComplexExt(..),
  Xsd80Elem2ComplexExt(..),
  Xsd70Elem2ComplexExt(..), TopLevelComplexType, XSeqX23(..),
  Xsd64ComplexExtBSeq2Choice(..), Xsd64ComplexExtBSeq(..), RestrictionType(..),
  ComplexRestrictionType, Xsd68ComplexExtBSeq(..), ExtensionType(..),
  Xsd70Elem2ComplexExtBChoice(..), Xsd72Elem2ComplexExtBSeq(..),
  Xsd74Elem2ComplexExtBSeq(..), SimpleRestrictionType, SimpleExtensionType,
  Xsd80Elem2ComplexExtBChoice(..), Xsd84Union1SimpleRestrtoken,
  Xsd84Union2ListSimpleRestrderivationControl,
  Xsd84Union2ListSimpleRestrderivationControlElement, BlockSet(..),
  Xsd86ComplexExtBSeq2Choice(..), AltType(..), Xsd86ComplexExtBSeq(..),
  IdentityConstraint(..), Xsd94ComplexExtBChoice(..), RealGroup,
  SimpleExplicitGroup, XElem1ComplexRestrictedgroupRef,
  Xsd108GroupSeq4Choice(..), AllModel(..), NamespaceList(..),
  Xsd132ListUnion(..), Xsd120AtrGrpGroupElemAttrSimpleRestrbasicNamespaceList,
  BasicNamespaceList, Xsd120AtrGrpGroupElemAttrSimpleRestrNMTOKEN,
  SpecialNamespaceList, Xsd132ListUnion1SimpleRestrtoken,
  Xsd136ListUnion1SimpleRestrtoken,
  Xsd136ListUnion(..), QnameList, Xsd138ListUnion1SimpleRestrtoken,
  Xsd138ListUnion(..), QnameListA, Xsd140Union1SimpleRestrtoken,
  XpathDefaultNamespace(..), Xsd154Elem2ComplexExtBChoice(..),
  Xsd156Elem2ComplexExtBSeq(..), Xsd160Elem2ComplexExt(..),
  Xsd162Elem2ComplexExt(..), Xsd164ComplexExtBSeq(..), Keybase(..),
  Xsd172Elem2ComplexExt(..), Public, Source, Appinfo(..), Documentation(..),
  Xsd182Elem2ComplexExtBChoice(..)
  -- , module QDHXB.Internal.L1.NestedTypes
  --
  -- * Translation notes
  --
  -- | The sections below order the Haskell types extracted from the
  -- XSD files in an order and with indentation reflecting the
  -- structure of the underlying XSD files.
  --
  -- == Contents of XSD file @datatypes.xsd@
  --
  --   [Type `DerivationControl`]: From simple type @derivationControl@
  --
  --   [Type `SimpleDerivation`]: From group @simpleDerivation@'s
  --   implementing @<choice>@.
  --
  --      [Constructor `SimpleDerivationRestriction`]:
  --
  --      [Constructor `SimpleDerivationList`]:
  --
  --      [Constructor `SimpleDerivationUnion`]:
  --
  --  [Type `SimpleDerivationSet`]: From simple type @simpleDerivationSet@
  --
  --      [Constructor `SimpleDerivationSetToken`]: Renamed from
  --      @SimpleDerivationSetDatatypes8Union1SimpleRestrtoken@
  --
  --          [Type/constructor `SimpleDerivationSetTokenImpl`]: Renamed
  --          from @Datatypes8Union1SimpleRestrtoken@
  --
  --      [Constructor `SimpleDerivationSetControl`]: Renamed from
  --      @SimpleDerivationSetDatatypes8Union2ListSimpleRestrderivationControlElement@
  --
  --          [Type `SimpleDerivationSetControlImpl`]: Renamed
  --          from @Datatypes8Union2ListSimpleRestrderivationControl@
  --
  --  [Type `simpleType`]: From complex type @simpleType@, renamed from
  --  @SimpleTypeX1@
  --
  --  [Type `TopLevelSimpleType`]: From simple type @topLevelSimpleType@
  --
  --  [Type `LocalSimpleType`]: From simple type @localSimpleType@
  --
  --  [Element @<simpleType>@]: Of type `TopLevelSimpleType`
  --
  --  [Element @<facet>@]: Abstract, no type
  --
  --  [Type `SimpleRestrictionModel`]: From group @simpleRestrictionModel@
  --
  --      [Type `SimpleRestrictionModelFacets`]: Renamed from
  --      @Datatypes20GroupSeq4Choice@
  --
  --          [Constructor `SimpleRestrictionModelFacetAny`]: Renamed
  --          from @Datatypes20GroupSeq4ChoiceFacetX2@
  --
  --          [Constructor `SimpleRestrictionModelFacetRaw`]: Renamed
  --          from @Datatypes20GroupSeq4ChoiceRawXML@
  --
  --  [Element @<restriction>@]:
  --
  --      [Type `RestrictionImpl`]: Renamed from
  --      @Datatypes22Elem1ComplexExt@
  --
  --  [Element @<list>@]:
  --
  --      [Type `ListImpl`]: Renamed from @Datatypes24Elem1ComplexExt@
  --
  --          [Type `ListImplTypeList`]: Renamed from
  --          @Datatypes24Elem1ComplexExtBSeq@
  --
  --  [Element @<union>@]:
  --
  --      [Type `UnionImpl`]: Renamed from @Datatypes26Elem1ComplexExt@
  --
  --          [Type `UnionImplTypeList`]: Renamed from
  --          @Datatypes26Elem1ComplexExtBSeq@
  --
  --  [Type `Facet`]: From complex type @facet@
  --
  --  [Type `NoFixedFacet`]: From complex type @noFixedFacet@
  --
  --  [Element @minExclusive@]: Of type `Facet`
  --
  --  [Element @minInclusive@]: Of type `Facet`
  --
  --  [Element @maxExclusive@]: Of type `Facet`
  --
  --  [Element @maxInclusive@]: Of type `Facet`
  --
  --  [Type `NumFacet`]: From complex type @numFacet@
  --
  --  [Type `IntFacet`]: From complex type @intFacet@
  --
  --  [Element @totalDigits@]:
  --
  --      [Type `TotalDigitsImpl`]: Renamed from
  --      @Datatypes44Elem2ComplexRestrictednumFacet@
  --
  --  [Element @fractionDigits@]: Of type `NumFacet`
  --
  --  [Element @length@]: Of type `NumFacet`
  --
  --  [Element @minLength@]: Of type `NumFacet`
  --
  --  [Element @maxLength@]: Of type `NumFacet`
  --
  --  [Element @enumeration@]: Of type `NoFixedFacet`
  --
  --  [Element @whitespace@]:
  --
  --      [Type `WhitespaceImpl`]: Renamed from
  --      @Datatypes56Elem2ComplexRestrictedfacet@
  --
  --  [Element @pattern@]:
  --
  --      [Type `PatternImpl`]: Renamed from
  --      @Datatypes58Elem2ComplexRestrictednoFixedFacet@
  --
  --  [Element @assertion@]: Of type `Assertion`
  --
  --  [Element @explicitTimezone@]:
  --
  --      [Type `ExplicitTimezoneImpl`]: Renamed from
  --      @Datatypes62Elem2ComplexRestrictedfacet@
  --
  --  [Type ``]: From complex type @@
  --
  --  [Type ``]: From complex type @@
  --
  --  [Type ``]: From simple type @@
  --
  --  [Type ``]: From simple type @@
  --
  --  [Type ``]: From simple type @@
  --
  --  [Type ``]: From simple type @@
  --
  -- == Contents of XSD file @xsd.xsd@
  --
  --  [Type `OpenAttrs`]: From complex type @openAttrs@
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

        -- . setDebugging input 0
        -- . setDebugging names 3
        -- . setDebugging unique 4
        -- . setDebugging flattening 1
        -- . setDebugging generate 3
        -- . breakAfterAllInput
      ) [
  "src/QDHXB/datatypes.xsd", "src/QDHXB/xsd.xsd"
  ]
