import Test.TLT
-- import Control.Monad.IO.Class
import Shiporder.Mini1 (testShiporderMini1)
import Shiporder.Mini2 (testShiporderMini2)
import Shiporder.Mini3 (testShiporderMini3)
import Shiporder.Shiporder0 (testShiporder0)
import Shiporder.Shiporder1 (testShiporder1)
import Shiporder.Shiporder2 (testShiporder2)
import Books.Books1 (testBooks1)
import Books.BookMod0 (testBookMod0)
import Books.BooksMod0 (testBooksMod0)
import Books.BooksMod1 (testBooksMod1)
import Books.BooksMod2 (testBooksMod2)
import CustomersOrders.CustomersOrders1 (testCustomersOrders1)
import Address.Address (testAddress)
import Age.Age (testAge)
import Age.Age0 (testAge0)
import Initials.Initials (testInitials)
import SizesNoList.SizesNoList (testSizesNoList)
import SizesNoList.Min1 (testSNLmin1)
import SizesAndList.SizesAndList (testSizesAndList)
import SizesAndList.SizesAndListInline (testSizesAndListInline)
import PersonAttr1.PersonAttr1 (testPersonAttr1)
import PersonAttr2.PersonAttr2 (testPersonAttr2)
import PersonAttr2.PersonAttr2Inline (testPersonAttr2Inline)
import PersonAttr2r.PersonAttr2r (testPersonAttr2r)
import GroupBasic.MinGroup0 (testMinGroup0)
import GroupBasic.MinGroup1 (testMinGroup1)
import GroupBasic.GroupBasic0 (testGB0)
import GroupBasic.GroupBasic1 (testGB1)
import GroupBasic.GroupRefs3 (testGB3)
import GroupBasic.GroupRefs4 (testGB4)
import ChoiceBasic.ChoiceBasic0 (testCB0)
import ChoiceBasic.ChoiceBasic1 (testCB1)
import ChoiceBasic.ChoiceBasic2 (testCB2)
import SequenceBasic.SequenceBasic0 (testSequencebasic0)
import SequenceBasic.SequenceBasic1 (testSequencebasic1)
import SequenceBasic.SequenceBasic2 (testSequencebasic2)
import Any.Any0 (testAny0)
import Multifile.MBooksA (testMBookA)
import RenameNested.Attrs (testNestedAttrs)
import RenameNested.Attrs2 (testNestedAttrs2)
import RenameNested.Elems (testNestedElems)
import Small.ElementType (testElementType)
import Small.ElementSimples (testElementSimples)
import Small.ElementTypeAttr (testElementTypeAttr)
import Small.ElementTypeAttr2 (testElementTypeAttr2)
import Assertions.Assertion01 (testAssertion01)

import OFX1 (testOFX1)
import XSD1 (testXSD1)

main :: IO ()
main = do
  tlt $ do
    testAge
    testAge0
    testInitials
    testSequencebasic0
    testSequencebasic1
    testSequencebasic2
    testShiporderMini1
    testShiporderMini2
    testShiporderMini3
    testShiporder0
    testShiporder1
    testShiporder2
    testBooks1
    testBookMod0
    testBooksMod0
    testBooksMod1
    testBooksMod2
    testCustomersOrders1
    testOFX1
    testXSD1
    testSizesNoList
    testSNLmin1
    testSizesAndList
    testSizesAndListInline
    testPersonAttr1
    testPersonAttr2
    testPersonAttr2Inline
    testPersonAttr2r
    testMinGroup0
    testMinGroup1
    testGB0
    testGB1
    testGB3
    testCB0
    testCB1
    testCB2
    testAddress
    testAny0
    testMBookA
    testNestedElems
    testNestedAttrs
    testNestedAttrs2
    testElementType
    testElementSimples
    testElementTypeAttr
    testAssertion01
