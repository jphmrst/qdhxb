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
import Age.Age (testAge)
import Age.Age0 (testAge0)
import Initials.Initials (testInitials)
import SizesNoList.SizesNoList (testSizesNoList)
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
import ChoiceBasic.ChoiceBasic0 (testCB0)
import ChoiceBasic.ChoiceBasic1 (testCB1)
import SequenceBasic.SequenceBasic0 (testSequencebasic0)
import SequenceBasic.SequenceBasic1 (testSequencebasic1)
import SequenceBasic.SequenceBasic2 (testSequencebasic2)

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
    testCB0
    testCB1
